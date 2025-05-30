pub(crate) mod ops;
pub(crate) mod strategies;

use strategies::{FastStrategy, FilterStrategy, StatTrackStrategy};
pub use strategies::{FilterPreProcess, GameStats, process_stats};

use crate::{
    LOG_ONLY, ResponseErr, STATUS_OK, Spinner, command_err,
    commands::handler::{CmdErr, ReplHandle},
    models::{
        cli::{Filters, Region, Source},
        json_data::{ContCode, GetInfo, LocationApiResponse, ServerInfo},
    },
    parse_hostname,
    utils::display::{DisplayCountOf, DisplayServerCount, SOURCE_HMW, SingularPlural},
};

use std::{
    borrow::Cow,
    collections::HashSet,
    fmt::Display,
    fs::File,
    io::{self, Write},
    net::{AddrParseError, IpAddr, SocketAddr, ToSocketAddrs},
    path::Path,
    time::Duration,
};

use repl_oxide::ansi_code::{CLEAR_LINE, GREEN, RESET, YELLOW};
use reqwest::Client;
use tracing::{error, info, instrument, trace, warn};

const MASTER_LOCATION_URL: &str = "http://ip-api.com/json/";
const REQUESTED_FIELDS: &str = "?fields=message,continentCode";

const FAVORITES_LOC: &str = "players2";
const FAVORITES: &str = "favourites.json";

pub(crate) const DEFAULT_SOURCES: [Source; 2] = [Source::Iw4Master, Source::HmwMaster];
const DEFAULT_H2M_SERVER_CAP: usize = 100;
const DEFAULT_INFO_RETRIES: u8 = 3;

const RETRY_TIME_SCALE: Duration = Duration::from_millis(800);

const LOCAL_HOST: &str = "localhost";

pub(crate) const GAME_ID: [&str; 2] = ["H2M", "HMW"];
pub(crate) const H2M_ID: &str = GAME_ID[0];
pub(crate) const HMW_ID: &str = GAME_ID[1];

const NA_CONT_CODE: [ContCode; 1] = [[b'N', b'A']];
const SA_CONT_CODE: [ContCode; 1] = [[b'S', b'A']];
const EU_CONT_CODES: [ContCode; 2] = [[b'E', b'U'], [b'A', b'F']];
const APAC_CONT_CODES: [ContCode; 2] = [[b'A', b'S'], [b'O', b'C']];

fn serialize_json(f: &mut std::fs::File, from: String) -> io::Result<()> {
    const COMMA: char = ',';
    let ips = if from.ends_with(COMMA) {
        &from[..from.len() - COMMA.len_utf8()]
    } else {
        from.as_str()
    };
    write!(f, "[{ips}]")
}

#[instrument(name = "filter", level = "trace", skip_all)]
pub(crate) async fn build_favorites(
    repl: &mut ReplHandle,
    curr_dir: &Path,
    args: Filters,
    version: f64,
) -> Result<bool, CmdErr> {
    let mut ip_collected = 0;
    let mut ips = String::new();

    let mut favorites_path = curr_dir.join(FAVORITES_LOC);
    if !favorites_path.exists() {
        std::fs::create_dir(&favorites_path).map_err(|err| {
            command_err!("Could not create missing {FAVORITES_LOC} directory, err: {err}")
        })?;
        info!("\"players2\" folder is missing, a new one was created");
    }

    favorites_path.push(FAVORITES);
    let mut favorites_json = File::create(&favorites_path)
        .map_err(|err| command_err!("Could not create {FAVORITES}, err: {err}"))?;

    let spinner = Spinner::new(String::new());

    let limit = args.limit.unwrap_or({
        if version < 1.0 {
            DEFAULT_H2M_SERVER_CAP
        } else {
            10000
        }
    });

    if version < 1.0 && limit >= DEFAULT_H2M_SERVER_CAP {
        println!(
            "{CLEAR_LINE}{YELLOW}NOTE: Currently the in game server browser breaks when you add more than 100 servers to favorites{RESET}"
        )
    }

    let mut filter = filter_server_list(repl, args, spinner).await?;

    if filter.duplicates != 0 {
        println!(
            "{CLEAR_LINE}{YELLOW}{}{RESET} duplicate {} not included",
            filter.duplicates,
            SingularPlural(filter.duplicates, "server was", "servers were")
        );
    }

    println!(
        "{CLEAR_LINE}{} match the parameters in the current query",
        DisplayServerCount(filter.servers.len(), GREEN)
    );

    if filter.servers.len() > limit {
        filter.servers.sort_unstable_by_key(|&(_, clients)| clients);
    }

    for (addr, _) in filter.servers.iter().rev() {
        ips.push_str(&format!("\"{}\",", addr));
        ip_collected += 1;
        if ip_collected == limit {
            break;
        }
    }

    serialize_json(&mut favorites_json, ips)
        .map_err(|err| command_err!("Could not write to {FAVORITES}, err: {err}"))?;

    println!(
        "{GREEN}{FAVORITES} updated with {}{RESET}",
        DisplayCountOf(ip_collected, "entry", "entries")
    );

    Ok(filter.cache_modified)
}

pub(crate) struct FilterData {
    /// (addr, clients)
    servers: Vec<(SocketAddr, u8)>,
    duplicates: usize,
    cache_modified: bool,
}

impl Region {
    fn to_bytes(self) -> &'static [ContCode] {
        match self {
            Region::Apac => &APAC_CONT_CODES,
            Region::EU => &EU_CONT_CODES,
            Region::NA => &NA_CONT_CODE,
            Region::SA => &SA_CONT_CODE,
        }
    }

    fn to_byte_set(regions: &[Region]) -> HashSet<ContCode> {
        regions
            .iter()
            .copied()
            .flat_map(Region::to_bytes)
            .copied()
            .collect()
    }
}

impl Filters {
    fn need_get_info_data(&self) -> bool {
        self.excludes.is_some()
            || self.includes.is_some()
            || self.player_min.is_some()
            || self.team_size_max.is_some()
            || self.with_bots
            || self.without_bots
            || !self.include_unresponsive
    }

    /// Possible to execute filters using data found from the iw4 master server endpoint
    /// see [`ServerInfo`] for included fields.
    fn can_use_iw4m_data(&self) -> bool {
        !self.with_bots && !self.without_bots && self.include_unresponsive
    }

    fn regions(&self) -> Option<HashSet<ContCode>> {
        self.region.as_deref().map(Region::to_byte_set)
    }
}

#[instrument(level = "trace", skip_all)]
async fn filter_server_list(
    repl: &mut ReplHandle,
    args: Filters,
    spinner: Spinner,
) -> Result<FilterData, CmdErr> {
    let sources = args
        .source
        .as_deref()
        .map(|user_sources| user_sources.iter().copied().collect::<HashSet<_>>());

    spinner.update_message(format!(
        "Retrieving master {}",
        SingularPlural(
            sources
                .as_ref()
                .map(HashSet::len)
                .unwrap_or(DEFAULT_SOURCES.len()),
            "server",
            "servers"
        )
    ));

    if args.stats {
        StatTrackStrategy::execute(repl, args, sources, spinner).await
    } else {
        FastStrategy::execute(repl, args, sources, spinner).await
    }
}

#[derive(Debug)]
pub struct Server {
    pub source: Sourced,
    pub info: GetInfo,
}

impl From<HostMeta> for Server {
    /// Real source is `Sourced::Iw4`  
    /// Source kind is modified to avoid cloning `ServerInfo` fields into the desired `GetInfo`
    fn from(value: HostMeta) -> Self {
        Server {
            info: GetInfo {
                clients: value.server.clients,
                max_clients: value.server.max_clients,
                // game_name: value.server.game,
                game_type: value.server.game_type.into(),
                host_name: value.server.host_name,
                map_name: value.server.map.into(),
                ..Default::default()
            },
            source: Sourced::Iw4Cached(value.resolved_addr),
        }
    }
}

pub(crate) struct GetInfoMetaData {
    msg: String,
    display_url: bool,
    display_socket_addr: bool,
    display_source: bool,
    retries: u8,
    pub(crate) url: String,
    pub(crate) meta: Sourced,
}

#[allow(dead_code)]
impl GetInfoMetaData {
    fn new(meta: Sourced, server_info_endpoint: &'static str) -> Self {
        GetInfoMetaData {
            msg: String::new(),
            display_url: false,
            display_source: false,
            display_socket_addr: false,
            retries: 0,
            url: format!("http://{}{server_info_endpoint}", meta.socket_addr()),
            meta,
        }
    }

    #[inline]
    pub(crate) fn set_err_msg(mut self, msg: String) -> Self {
        self.msg = msg;
        self
    }

    #[inline]
    pub(crate) fn with_url(&mut self) -> &mut Self {
        self.display_url = true;
        self
    }

    #[inline]
    pub(crate) fn with_socket_addr(&mut self) -> &mut Self {
        self.display_socket_addr = true;
        self
    }

    #[inline]
    pub(crate) fn with_source(&mut self) -> &mut Self {
        self.display_source = true;
        self
    }

    #[inline]
    /// `GetInfoErr` default display is without addr
    pub(crate) fn without_url(&mut self) -> &mut Self {
        self.display_url = false;
        self
    }

    #[inline]
    /// `GetInfoErr` default display is without ip
    pub(crate) fn without_ip(&mut self) -> &mut Self {
        self.display_url = false;
        self
    }

    #[inline]
    /// `GetInfoErr` default display is without source
    pub(crate) fn without_source(&mut self) -> &mut Self {
        self.display_source = false;
        self
    }
}

impl Display for GetInfoMetaData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)?;
        if self.display_url {
            write!(f, ", with addr: {}", self.url)?;
        }
        if self.display_socket_addr {
            write!(f, ", with ip: {}", self.meta.socket_addr())?;
        }
        if self.display_source {
            write!(f, ", with source: {}", self.meta)?;
        }
        Ok(())
    }
}

#[derive(Default)]
pub(crate) struct UnresponsiveCounter {
    pub(crate) hmw: usize,
    pub(crate) hmw_cached: usize,
    pub(crate) iw4: usize,
    pub(crate) iw4_cached: usize,
    pub(crate) used_cached_servers: usize,
}

impl UnresponsiveCounter {
    #[inline]
    pub(crate) fn total(&self) -> usize {
        self.hmw + self.hmw_cached + self.iw4 + self.iw4_cached
    }

    fn add(&mut self, from: &Sourced) {
        match from {
            Sourced::Hmw(_) => self.hmw += 1,
            Sourced::HmwCached(_) => self.hmw_cached += 1,
            Sourced::Iw4(_) => self.iw4 += 1,
            Sourced::Iw4Cached(_) => self.iw4_cached += 1,
        }
    }

    fn used_cached_server(&mut self) {
        self.used_cached_servers += 1
    }
}

pub(crate) enum Request {
    New(Sourced),
    Retry(GetInfoMetaData),
}

pub(crate) async fn try_get_info(
    from: Request,
    client: Client,
    server_info_endpoint: &'static str,
) -> Result<Server, GetInfoMetaData> {
    let meta_data = match from {
        Request::New(meta) => GetInfoMetaData::new(meta, server_info_endpoint),
        Request::Retry(mut err) => {
            err.retries += 1;
            err
        }
    };

    let server_response = match client.get(&meta_data.url).send().await {
        Ok(res) => res,
        Err(err) => return Err(meta_data.set_err_msg(err.without_url().to_string())),
    };

    if server_response.status() != STATUS_OK {
        return Err(meta_data.set_err_msg(format!("getInfo {}", server_response.status())));
    }

    match server_response.json::<GetInfo>().await {
        Ok(info) => Ok(Server {
            source: meta_data.meta,
            info,
        }),
        Err(err) => Err(meta_data.set_err_msg(err.without_url().to_string())),
    }
}

#[derive(Debug)]
pub(crate) struct HostMeta {
    pub(crate) resolved_addr: SocketAddr,
    pub(crate) server: ServerInfo,
}

impl HostMeta {
    fn from_ip(ip: IpAddr, server: ServerInfo) -> Self {
        Self {
            resolved_addr: SocketAddr::new(ip, server.port),
            server,
        }
    }

    fn try_from(host_ip: &str, webfront_url: &str, server: ServerInfo) -> Option<Self> {
        if let Some(set_resolved) = server
            .resolved_ip
            .as_deref()
            .and_then(|ip_str| ip_str.parse::<IpAddr>().ok())
        {
            trace!("used resolved from iw4: {set_resolved}");
            return Some(Self::from_ip(set_resolved, server));
        }

        resolve_address(&server.ip, host_ip, webfront_url)
            .map_err(|err| {
                error!(name: LOG_ONLY, "{err}, server_ip: {}, host_ip: {host_ip}, webfront_url: {webfront_url}", server.ip)
            })
            .map(|ip| Self::from_ip(ip, server))
            .ok()
    }
}

impl GetInfo {
    pub fn player_ct(&self) -> u8 {
        self.clients.checked_sub(self.bots).unwrap_or_else(|| {
            error!(name: LOG_ONLY, "Server: {}, found with invalid bot count. Bots: {} > Clients: {}",
                parse_hostname(&self.host_name),
                self.bots,
                self.clients
            );
            0
        })
    }

    pub(crate) fn max_public_slots(&self) -> u8 {
        self.max_clients
            .checked_sub(self.private_clients.max(0) as u8).unwrap_or_else(|| {
            error!(name: LOG_ONLY, "Server: {}, found with invalid private_client count. Private clients: {} > Clients: {}",
                parse_hostname(&self.host_name),
                self.private_clients,
                self.clients
            );
            0
        })
    }
}

#[derive(Debug)]
pub enum Sourced {
    Hmw(SocketAddr),
    HmwCached(SocketAddr),

    // Only pub so we can write tests for ensuring proper table formatting, `HostMeta` does not
    // need to be included, as it eventually gets converted to `Server` before it is displayed.
    #[allow(private_interfaces)]
    Iw4(HostMeta),

    Iw4Cached(SocketAddr),
}

impl Sourced {
    fn addr_copy(&self) -> Self {
        match self {
            &Sourced::Hmw(addr) => Sourced::Hmw(addr),
            &Sourced::HmwCached(addr) => Sourced::HmwCached(addr),
            &Sourced::Iw4Cached(addr) => Sourced::Iw4Cached(addr),
            Sourced::Iw4(host_meta) => Sourced::Iw4(HostMeta {
                resolved_addr: host_meta.resolved_addr,
                server: ServerInfo::default(),
            }),
        }
    }

    fn try_parse_hmw_master(ip_port: String) -> Option<Self> {
        macro_rules! hmw_fmt_err {
            ($($arg:tt)*) => {
                error!(name: LOG_ONLY, "Unexpected {SOURCE_HMW} formatting: {}", format_args!($($arg)*))
            }
        }

        match ip_port
            .rsplit_once(':')
            .map(|(ip, port)| (ip.parse().map_err(|err| (err, ip)), port.parse::<u16>()))
        {
            Some((Ok(ip), Ok(port))) => return Some(Self::Hmw(SocketAddr::new(ip, port))),
            Some((Err((err, ip_str)), Ok(port))) => {
                if let Some(ip) = try_resolve_from_str(ip_str) {
                    return Some(Self::Hmw(SocketAddr::new(ip, port)));
                }
                hmw_fmt_err!("failed to parse ip address in: {ip_port}, {err}")
            }
            Some((Ok(_), Err(err))) => {
                hmw_fmt_err!("failed to parse port in: {ip_port}, {err}")
            }
            Some((Err(_), Err(_))) => {
                hmw_fmt_err!("invalid string: {ip_port}")
            }
            None => {
                hmw_fmt_err!("address was not formatted with a port: {ip_port}")
            }
        }
        None
    }
}

pub(crate) trait Addressable {
    fn socket_addr(&self) -> SocketAddr;
}

impl Addressable for Sourced {
    fn socket_addr(&self) -> SocketAddr {
        match self {
            Sourced::Hmw(addr) | Sourced::HmwCached(addr) | Sourced::Iw4Cached(addr) => *addr,
            Sourced::Iw4(meta) => meta.resolved_addr,
        }
    }
}

impl Addressable for Server {
    fn socket_addr(&self) -> SocketAddr {
        self.source.socket_addr()
    }
}

impl Source {
    fn with_cached_ip(self, addr: SocketAddr) -> Sourced {
        match self {
            Source::Iw4Master => Sourced::Iw4Cached(addr),
            Source::HmwMaster => Sourced::HmwCached(addr),
        }
    }
}

#[instrument(level = "trace", skip_all)]
pub(crate) async fn try_location_lookup(
    ip: IpAddr,
    client: Client,
) -> Result<(IpAddr, ContCode), ResponseErr> {
    let location_api_url = format!("{MASTER_LOCATION_URL}{ip}{REQUESTED_FIELDS}");

    let api_response = client.get(location_api_url).send().await?;

    if api_response.status() != STATUS_OK {
        return Err(ResponseErr::bad_status("IP api", api_response));
    }

    let code = match api_response.json::<LocationApiResponse>().await? {
        LocationApiResponse {
            cont_code: Some(code),
            ..
        } => code,
        LocationApiResponse { message, .. } => {
            return Err(ResponseErr::Other(
                message
                    .map(Cow::Owned)
                    .unwrap_or(Cow::Borrowed("unknown error")),
            ));
        }
    };

    Ok((ip, code))
}

#[instrument(level = "trace", skip_all)]
fn resolve_address(
    server_ip: &str,
    host_ip: &str,
    webfront_url: &str,
) -> Result<IpAddr, AddrParseError> {
    let ip_trim = server_ip.trim_matches('/').trim_matches(':');
    if !ip_trim.is_empty() && ip_trim != LOCAL_HOST {
        if let Ok(ip) = ip_trim.parse::<IpAddr>() {
            return if ip.is_unspecified() {
                parse_possible_ipv6(host_ip, webfront_url)
            } else {
                Ok(ip)
            };
        }
        if let Some(ip) = try_resolve_from_str(ip_trim) {
            return Ok(ip);
        }
    }

    parse_possible_ipv6(host_ip, webfront_url)
}

fn try_resolve_from_str(ip_str: &str) -> Option<IpAddr> {
    if let Some(ip) = (ip_str, 80)
        .to_socket_addrs()
        .ok()
        .and_then(|mut socket_addr| socket_addr.next().map(|socket| socket.ip()))
    {
        trace!("Found socket address of: {ip}, from: {ip_str}");
        return Some(ip);
    }
    None
}

#[instrument(level = "trace", skip_all)]
fn parse_possible_ipv6(ip: &str, webfront_url: &str) -> Result<IpAddr, AddrParseError> {
    let ip_trim = ip.trim_matches('/').trim_matches(':');

    if let Some(ip) = try_resolve_from_str(ip_trim) {
        return Ok(ip);
    }

    match ip_trim.parse::<IpAddr>() {
        Ok(ip) => Ok(ip),
        Err(err) => {
            const HTTP_ENDING: &str = "//";
            if let Some(i) = webfront_url.find(HTTP_ENDING) {
                const PORT_SEPARATOR: char = ':';
                let ip_start = i + HTTP_ENDING.len();
                let ipv6_slice = if let Some(j) = webfront_url[ip_start..].rfind(PORT_SEPARATOR) {
                    let ip_end = j + ip_start;
                    if ip_end <= ip_start {
                        error!(name: LOG_ONLY, "Bad ipv6 slice op: {webfront_url}");
                        return Err(err);
                    }
                    &webfront_url[ip_start..ip_end]
                } else {
                    &webfront_url[ip_start..]
                };
                trace!("Parsed: {ipv6_slice}, from webfront_url");
                return ipv6_slice.parse::<IpAddr>();
            }
            Err(err)
        }
    }
}
