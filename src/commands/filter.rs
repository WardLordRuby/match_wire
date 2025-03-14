use crate::{
    location_api_key::FIND_IP_NET_PRIVATE_KEY,
    make_slice_ascii_lowercase,
    models::{
        cli::{Filters, Region, Source},
        json_data::*,
    },
    parse_hostname,
    utils::{
        caching::Cache,
        display::{
            DisplayCachedServerUse, DisplayCountOf, DisplayGetInfoCount, DisplayServerCount,
            SingularPlural, SOURCE_HMW,
        },
    },
    Spinner, LOG_ONLY, TERM_CLEAR_LINE,
};

use std::{
    borrow::Cow,
    collections::HashSet,
    fmt::Display,
    fs::File,
    io::{self, ErrorKind, Write},
    net::{AddrParseError, IpAddr, SocketAddr, ToSocketAddrs},
    path::Path,
    sync::Arc,
};

use constcat::concat;
use repl_oxide::ansi_code::{GREEN, RED, RESET, YELLOW};
use reqwest::Client;
use tokio::{sync::Mutex, task::JoinSet};
use tracing::{error, info, instrument, trace, warn};

const MASTER_LOCATION_URL: &str = "https://api.findip.net";

const IW4_MASTER_URL: &str = "https://master.iw4.zip";
const JSON_SERVER_ENDPOINT: &str = "/instance";
const SERVER_GET_INFO_ENDPOINT: &str = "/getInfo";

const HMW_MASTER_URL: &str = "https://ms.horizonmw.org/game-servers";
const FAVORITES_LOC: &str = "players2";
const FAVORITES: &str = "favourites.json";

const DEFAULT_H2M_SERVER_CAP: usize = 100;
const DEFUALT_INFO_RETRIES: u8 = 3;
pub(crate) const DEFUALT_SOURCES: [Source; 2] = [Source::Iw4Master, Source::HmwMaster];
const RETRY_TIME_SCALE: u64 = 800; // ms
const LOCAL_HOST: &str = "localhost";

pub(crate) const GAME_ID: &str = "H2M";
const NA_CONT_CODE: [[u8; 2]; 1] = [[b'N', b'A']];
const EU_CONT_CODE: [[u8; 2]; 1] = [[b'E', b'U']];
const APAC_CONT_CODES: [[u8; 2]; 3] = [[b'A', b'F'], [b'A', b'S'], [b'O', b'C']];

fn serialize_json(into: &mut std::fs::File, from: String) -> io::Result<()> {
    const COMMA: char = ',';
    let ips = if from.ends_with(COMMA) {
        &from[..from.len() - COMMA.len_utf8()]
    } else {
        from.as_str()
    };
    write!(into, "[{ips}]")
}

async fn get_iw4_master() -> reqwest::Result<Vec<HostData>> {
    trace!("retreiving iw4 master server list");
    const INSTANCE_URL: &str = concat!(IW4_MASTER_URL, JSON_SERVER_ENDPOINT);
    reqwest::get(INSTANCE_URL)
        .await?
        .json::<Vec<HostData>>()
        .await
}

async fn get_hmw_master() -> reqwest::Result<Vec<String>> {
    trace!("retreiving hmw master server list");
    reqwest::get(HMW_MASTER_URL)
        .await?
        .json::<Vec<String>>()
        .await
}

#[instrument(name = "filter", level = "trace", skip_all)]
pub(crate) async fn build_favorites(
    curr_dir: &Path,
    args: Filters,
    cache: Arc<Mutex<Cache>>,
    version: f64,
) -> io::Result<bool> {
    let mut ip_collected = 0;
    let mut ips = String::new();

    let favorites_path = curr_dir.join(format!("{FAVORITES_LOC}/{FAVORITES}"));
    let mut favorites_json = match File::create(&favorites_path) {
        Ok(file) => file,
        Err(err) if err.kind() == ErrorKind::NotFound => {
            std::fs::create_dir(curr_dir.join(FAVORITES_LOC))?;
            info!("\"players2\" folder is missing, a new one was created");
            File::create(favorites_path)?
        }
        Err(err) => return Err(err),
    };

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
            "{TERM_CLEAR_LINE}{YELLOW}NOTE: Currently the in game server browser breaks when you add more than 100 servers to favorites{RESET}"
        )
    }

    let (mut servers, update_cache) = filter_server_list(args, cache, limit, &spinner)
        .await
        .map_err(|err| io::Error::other(format!("{err:?}")))?;

    println!(
        "{TERM_CLEAR_LINE}{} match the prameters in the current query",
        DisplayServerCount(servers.len(), GREEN)
    );

    if servers.len() > limit {
        servers.sort_unstable_by_key(|server| server.info.as_ref().map_or(0, |info| info.clients));
    }

    for server in servers.iter().rev() {
        ips.push_str(&format!("\"{}\",", server.source.socket_addr()));
        ip_collected += 1;
        if ip_collected == limit {
            break;
        }
    }

    serialize_json(&mut favorites_json, ips)?;

    spinner.finish();

    println!(
        "{GREEN}{FAVORITES} updated with {}{RESET}",
        DisplayCountOf(ip_collected, "entry", "entries")
    );
    Ok(update_cache)
}

pub(crate) struct Server {
    pub(crate) source: Sourced,
    pub(crate) info: Option<GetInfo>,
}

impl From<HostMeta> for Server {
    /// Real source is Sourced::Iw4  
    /// Source kind is modifed to avoid cloning `ServerInfo` fields into the desired `GetInfo`
    fn from(value: HostMeta) -> Self {
        Server {
            info: Some(GetInfo {
                clients: value.server.clients,
                max_clients: value.server.max_clients,
                // private_clients: 0,
                bots: 0,
                // game_name: value.server.game,
                // game_type: value.server.game_type,
                host_name: value.server.host_name,
            }),
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
    fn new(meta: Sourced) -> Self {
        GetInfoMetaData {
            msg: String::new(),
            display_url: false,
            display_source: false,
            display_socket_addr: false,
            retries: 0,
            url: format!("http://{}{SERVER_GET_INFO_ENDPOINT}", meta.socket_addr()),
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
}

impl UnresponsiveCounter {
    #[inline]
    fn total(&self) -> usize {
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
}

pub(crate) enum Request {
    New(Sourced),
    Retry(GetInfoMetaData),
}

pub(crate) async fn try_get_info(
    from: Request,
    client: reqwest::Client,
) -> Result<Server, GetInfoMetaData> {
    let meta_data = match from {
        Request::New(meta) => GetInfoMetaData::new(meta),
        Request::Retry(mut err) => {
            err.retries += 1;
            err
        }
    };
    let server_response = match client.get(&meta_data.url).send().await {
        Ok(res) => res,
        Err(err) => return Err(meta_data.set_err_msg(err.without_url().to_string())),
    };
    match server_response.json::<GetInfo>().await {
        Ok(info) => Ok(Server {
            source: meta_data.meta,
            info: Some(info),
        }),
        Err(err) => Err(meta_data.set_err_msg(err.without_url().to_string())),
    }
}

pub(crate) struct HostMeta {
    pub(crate) resolved_addr: SocketAddr,
    pub(crate) server: ServerInfo,
}

impl HostMeta {
    fn try_from(host_ip: &str, webfront_url: &str, server: ServerInfo) -> Option<Self> {
        resolve_address(&server.ip, host_ip, webfront_url)
            .map_err(|err| {
                error!(name: LOG_ONLY, "{err}, server_ip: {}, host_ip: {host_ip}, webfront_url: {webfront_url}", server.ip)
            })
            .map(|ip| HostMeta {
                resolved_addr: SocketAddr::new(ip, server.port),
                server,
            })
            .ok()
    }
}

pub(crate) enum Sourced {
    Hmw(SocketAddr),
    HmwCached(SocketAddr),
    Iw4(HostMeta),
    Iw4Cached(SocketAddr),
}

impl Sourced {
    pub(crate) fn to_valid_source(&self) -> Option<Source> {
        match self {
            Self::Hmw(_) => Some(Source::HmwMaster),
            Self::Iw4(_) => Some(Source::Iw4Master),
            Self::HmwCached(_) | Self::Iw4Cached(_) => None,
        }
    }

    fn try_from_hmw_master(ip_port: String) -> Option<Self> {
        let (ip, port) = match ip_port
            .rsplit_once(':')
            .map(|(ip, port)| (ip.parse().map_err(|err| (err, ip)), port.parse::<u16>()))
        {
            Some((Ok(ip), Ok(port))) => (ip, port),
            Some((Ok(_), Err(err))) => {
                error!(name: LOG_ONLY, "Unexpected {SOURCE_HMW} formatting: failed to parse port in: {ip_port}, {err}");
                return None;
            }
            Some((Err((err, ip_str)), Ok(port))) => {
                let Some(ip) = try_resolve_from_str(ip_str) else {
                    error!(name: LOG_ONLY, "Unexpected {SOURCE_HMW} formatting: failed to parse ip address in: {ip_port}, {err}");
                    return None;
                };
                trace!("Found socket address of: {ip}, from: {ip_str}");
                (ip, port)
            }
            Some((Err(_), Err(_))) => {
                error!(name: LOG_ONLY, "Unexpected {SOURCE_HMW} formatting: invalid string: {ip_port}");
                return None;
            }
            None => {
                error!(name: LOG_ONLY, "Unexpected {SOURCE_HMW} formatting: address was not formatted with a port: {ip_port}");
                return None;
            }
        };
        Some(Sourced::Hmw(SocketAddr::new(ip, port)))
    }

    pub(crate) fn socket_addr(&self) -> SocketAddr {
        match self {
            Sourced::Hmw(addr) | Sourced::HmwCached(addr) | Sourced::Iw4Cached(addr) => *addr,
            Sourced::Iw4(meta) => meta.resolved_addr,
        }
    }
}

impl Source {
    fn with_cached_ip(self, addr: SocketAddr) -> Sourced {
        match self {
            Source::Iw4Master => Sourced::Iw4Cached(addr),
            Source::HmwMaster => Sourced::HmwCached(addr),
        }
    }

    async fn try_get_cached_servers(
        self,
        cache: Option<Arc<Mutex<Cache>>>,
        err: &reqwest::Error,
    ) -> Option<Vec<Sourced>> {
        let cache = cache?;
        let cached = {
            let lock = cache.lock().await;
            let backup = match self {
                Source::Iw4Master => &lock.iw4m,
                Source::HmwMaster => &lock.hmw,
            };

            backup
                .iter()
                .flat_map(|(&ip, ports)| {
                    ports
                        .iter()
                        .map(|&port| self.with_cached_ip(SocketAddr::new(ip, port)))
                        .collect::<Vec<_>>()
                })
                .collect::<Vec<_>>()
        };

        if cached.is_empty() {
            return None;
        }

        error!("Could not fetch {self} servers");
        error!(name: LOG_ONLY, "{err}");
        warn!("{}", DisplayCachedServerUse(self, cached.len()));
        Some(cached)
    }
}

async fn iw4_servers(cache: Option<Arc<Mutex<Cache>>>) -> reqwest::Result<Vec<Sourced>> {
    match get_iw4_master().await {
        Ok(hosts) => Ok(hosts
            .into_iter()
            .filter_map(|mut host| {
                host.servers.retain(|server| server.game == GAME_ID);
                (!host.servers.is_empty()).then_some(host)
            })
            .flat_map(|host| {
                host.servers
                    .into_iter()
                    .filter_map(|server| {
                        HostMeta::try_from(&host.ip_address, &host.webfront_url, server)
                            .map(Sourced::Iw4)
                    })
                    .collect::<Vec<_>>()
            })
            .collect()),
        Err(err) => {
            if let Some(backup) = Source::Iw4Master.try_get_cached_servers(cache, &err).await {
                return Ok(backup);
            }
            Err(err)
        }
    }
}

async fn hmw_servers(cache: Option<Arc<Mutex<Cache>>>) -> reqwest::Result<Vec<Sourced>> {
    match get_hmw_master().await {
        Ok(list) => Ok(list
            .into_iter()
            .filter_map(Sourced::try_from_hmw_master)
            .collect()),
        Err(err) => {
            if let Some(backup) = Source::HmwMaster.try_get_cached_servers(cache, &err).await {
                return Ok(backup);
            }
            Err(err)
        }
    }
}

pub(crate) async fn queue_info_requests(
    servers: Vec<Sourced>,
    remove_duplicates: bool,
    client: &Client,
) -> JoinSet<Result<Server, GetInfoMetaData>> {
    let mut dup = HashSet::new();

    JoinSet::from_iter(
        servers
            .into_iter()
            .filter(|server| !remove_duplicates || dup.insert(server.socket_addr()))
            .map(|server| try_get_info(Request::New(server), client.clone())),
    )
}

trait Conversion {
    fn to_server(self, limit: usize) -> Vec<Server>;
}

impl Conversion for Vec<Sourced> {
    fn to_server(self, limit: usize) -> Vec<Server> {
        let no_info = |source: Sourced| -> Server { Server { source, info: None } };
        let with_info = |source: Sourced| -> Server {
            if let Sourced::Iw4(meta) = source {
                Server::from(meta)
            } else {
                Server { source, info: None }
            }
        };

        let operation = if self.len() <= limit {
            no_info
        } else {
            with_info
        };
        self.into_iter().map(operation).collect()
    }
}

pub(crate) async fn get_sourced_servers<I>(
    sources: I,
    cache: Option<&Arc<Mutex<Cache>>>,
) -> Result<Vec<Sourced>, &'static str>
where
    I: IntoIterator<Item = Source>,
{
    let mut tasks = JoinSet::from_iter(sources.into_iter().map(|source| {
        let cache = cache.map(Arc::clone);
        async move {
            match source {
                Source::HmwMaster => hmw_servers(cache).await,
                Source::Iw4Master => iw4_servers(cache).await,
            }
        }
    }));

    let mut servers = Vec::new();

    while let Some(task_res) = tasks.join_next().await {
        match task_res {
            Ok(Ok(mut sourced_servers)) => {
                if servers.is_empty() {
                    servers = sourced_servers;
                } else {
                    servers.append(&mut sourced_servers);
                }
            }
            Ok(Err(err)) => error!("{err}"),
            Err(err) => error!(name: LOG_ONLY, "{err:?}"),
        }
    }

    if servers.is_empty() {
        return Err("Could not populate any servers from source(s)");
    }
    Ok(servers)
}

impl Region {
    fn to_chars(self) -> &'static [[u8; 2]] {
        match self {
            Region::Apac => &APAC_CONT_CODES,
            Region::EU => &EU_CONT_CODE,
            Region::NA => &NA_CONT_CODE,
        }
    }
}

fn to_region_set(regions: &[Region]) -> HashSet<[u8; 2]> {
    regions
        .iter()
        .copied()
        .flat_map(Region::to_chars)
        .copied()
        .collect()
}

#[instrument(level = "trace", skip_all)]
async fn filter_server_list(
    mut args: Filters,
    cache: Arc<Mutex<Cache>>,
    limit: usize,
    spinner: &Spinner,
) -> Result<(Vec<Server>, bool), &'static str> {
    let sources = args
        .source
        .as_deref()
        .map(|user_sources| user_sources.iter().copied().collect::<HashSet<_>>());

    spinner.update_message(format!(
        "Retrieving master {}",
        SingularPlural(
            sources
                .as_ref()
                .map(|s| s.len())
                .unwrap_or(DEFUALT_SOURCES.len()),
            "server",
            "servers"
        )
    ));

    let mut servers = match sources {
        Some(user_sources) => get_sourced_servers(user_sources, Some(&cache)).await,
        None => get_sourced_servers(DEFUALT_SOURCES, Some(&cache)).await,
    }?;

    let cache_modified = if let Some(regions) = args.region.as_deref().map(to_region_set) {
        spinner.update_message(format!(
            "Determining region of {}",
            DisplayServerCount(servers.len(), GREEN)
        ));

        let mut valid_regions = Vec::new();
        let mut tasks = JoinSet::new();
        let mut check_again = Vec::new();
        let mut new_lookups = HashSet::new();
        let client = reqwest::Client::new();

        let mut cache = cache.lock().await;

        for sourced_data in servers {
            let socket_addr = sourced_data.socket_addr();
            if let Some(cached_region) = cache.ip_to_region.get(&socket_addr.ip()) {
                if regions.contains(cached_region) {
                    valid_regions.push(sourced_data);
                }
                continue;
            }
            if new_lookups.insert(socket_addr.ip()) {
                let client = client.clone();
                trace!("Requsting location data for: {}", socket_addr.ip());
                tasks.spawn(async move {
                    try_location_lookup(&socket_addr.ip(), client)
                        .await
                        .map(|location| (sourced_data, location.code))
                });
            } else {
                check_again.push(sourced_data)
            }
        }

        let mut failure_count = 0_usize;

        while let Some(res) = tasks.join_next().await {
            match res {
                Ok(Ok((sourced_data, cont_code))) => {
                    cache
                        .ip_to_region
                        .insert(sourced_data.socket_addr().ip(), cont_code);
                    if regions.contains(&cont_code) {
                        valid_regions.push(sourced_data)
                    }
                }
                Ok(Err(err)) => {
                    error!(name: LOG_ONLY, "{err}");
                    failure_count += 1
                }
                Err(err) => {
                    error!(name: LOG_ONLY, "{err:?}");
                    failure_count += 1
                }
            }
        }

        if !new_lookups.is_empty() {
            info!(
                "Made {} new location {}",
                new_lookups.len(),
                SingularPlural(new_lookups.len(), "request", "requests")
            );
        }

        for sourced_data in check_again {
            if let Some(cached_region) = cache.ip_to_region.get(&sourced_data.socket_addr().ip()) {
                if regions.contains(cached_region) {
                    valid_regions.push(sourced_data)
                }
            }
        }

        drop(cache);

        if failure_count > 0 {
            println!(
                "{TERM_CLEAR_LINE}{RED}Failed to resolve location for {failure_count} server {}{RESET}",
                SingularPlural(failure_count, "hoster", "hosters")
            )
        }

        println!(
            "{TERM_CLEAR_LINE}{} match the input {}",
            DisplayServerCount(valid_regions.len(), GREEN),
            SingularPlural(
                args.region.as_ref().expect("outer if").len(),
                "region",
                "regions"
            ),
        );

        servers = valid_regions;
        !new_lookups.is_empty()
    } else {
        false
    };

    let servers = if args.excludes.is_some()
        || args.includes.is_some()
        || args.player_min.is_some()
        || args.team_size_max.is_some()
        || args.with_bots
        || args.without_bots
        || !args.include_unresponsive
    {
        let mut valid_servers = Vec::with_capacity(servers.len());

        let client = reqwest::Client::builder()
            .timeout(tokio::time::Duration::from_secs(3))
            .build()
            .unwrap();

        let mut tasks = queue_info_requests(servers, true, &client).await;

        let use_backup_server_info =
            !args.with_bots && !args.without_bots && args.include_unresponsive;
        let mut did_not_respond = UnresponsiveCounter::default();
        let mut used_backup_data = 0_usize;
        let mut sent_retires = false;
        let max_attempts = args.retry_max.unwrap_or(DEFUALT_INFO_RETRIES);

        let mut cache = cache.lock().await;

        while !tasks.is_empty() {
            spinner.update_message(format!(
                "{}",
                DisplayGetInfoCount(tasks.len(), sent_retires)
            ));

            let mut retries = JoinSet::new();
            while let Some(res) = tasks.join_next().await {
                match res {
                    Ok(Ok(server)) => {
                        cache.update_cache_with(&server, None);
                        valid_servers.push(server)
                    }
                    Ok(Err(mut err)) => {
                        if err.retries < max_attempts {
                            let client = client.clone();
                            retries.spawn(async move {
                                tokio::time::sleep(tokio::time::Duration::from_millis(
                                    RETRY_TIME_SCALE * (err.retries + 1) as u64,
                                ))
                                .await;
                                try_get_info(Request::Retry(err), client).await
                            });
                        } else {
                            did_not_respond.add(&err.meta);
                            error!(name: LOG_ONLY, "{}", err.with_socket_addr().with_source());
                            if use_backup_server_info {
                                if let Sourced::Iw4(meta) = err.meta {
                                    used_backup_data += 1;
                                    valid_servers.push(Server::from(meta));
                                }
                            }
                        }
                    }
                    Err(err) => error!(name: LOG_ONLY, "{err:?}"),
                }
            }
            sent_retires = true;
            tasks = retries;
        }

        drop(cache);

        if did_not_respond.total() > 0 {
            if use_backup_server_info {
                println!(
                    "{TERM_CLEAR_LINE}Included outdated server data for {YELLOW}{used_backup_data}{RESET} \
                    of {} that did not respond to 'getInfo' request",
                    DisplayServerCount(did_not_respond.total(), RED)
                )
            } else {
                println!("{TERM_CLEAR_LINE}{did_not_respond}");
            }
        }

        args.includes.as_deref_mut().map(make_slice_ascii_lowercase);
        args.excludes.as_deref_mut().map(make_slice_ascii_lowercase);

        for i in (0..valid_servers.len()).rev() {
            let server = &valid_servers[i];

            let Some(ref info) = server.info else {
                valid_servers.swap_remove(i);
                continue;
            };

            if let Some(team_size_max) = args.team_size_max {
                if info.max_clients > team_size_max * 2 {
                    valid_servers.swap_remove(i);
                    continue;
                }
            }

            if let Some(player_min) = args.player_min {
                if info.clients < player_min {
                    valid_servers.swap_remove(i);
                    continue;
                }
            }

            if args.with_bots && info.bots == 0 {
                valid_servers.swap_remove(i);
                continue;
            }

            if args.without_bots && info.bots != 0 {
                valid_servers.swap_remove(i);
                continue;
            }

            let mut hostname_l = None;
            if let Some(include_terms) = args.includes.as_deref() {
                hostname_l = Some(parse_hostname(&info.host_name));
                if !include_terms
                    .iter()
                    .any(|term| hostname_l.as_deref().unwrap().contains(term))
                {
                    valid_servers.swap_remove(i);
                    continue;
                }
            }
            if let Some(exclude_terms) = args.excludes.as_deref() {
                if hostname_l.is_none() {
                    hostname_l = Some(parse_hostname(&info.host_name));
                }
                if exclude_terms
                    .iter()
                    .any(|term| hostname_l.as_deref().unwrap().contains(term))
                {
                    valid_servers.swap_remove(i);
                }
            }
        }
        valid_servers
    } else {
        servers.to_server(limit)
    };

    Ok((servers, cache_modified))
}

#[instrument(level = "trace", skip_all)]
async fn try_location_lookup(
    ip: &IpAddr,
    client: reqwest::Client,
) -> Result<Continent, Cow<'static, str>> {
    let location_api_url = format!("{MASTER_LOCATION_URL}/{}{FIND_IP_NET_PRIVATE_KEY}", ip);

    let api_response = client
        .get(location_api_url.as_str())
        .send()
        .await
        .map_err(|err| format!("{}, ip: {ip}", err.without_url()))?;

    match api_response.json::<ServerLocation>().await {
        Ok(json) => {
            if let Some(code) = json.continent {
                return Ok(code);
            }
            Err(json
                .message
                .map(Cow::Owned)
                .unwrap_or(Cow::Borrowed("unknown error")))
        }
        Err(err) => Err(Cow::Owned(format!("{}, ip: {ip}", err.without_url()))),
    }
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
            trace!("Found socket address of: {ip}, from: {ip_trim}");
            return Ok(ip);
        }
    }

    parse_possible_ipv6(host_ip, webfront_url)
}

fn try_resolve_from_str(ip: &str) -> Option<IpAddr> {
    if let Ok(mut socket_addr) = (ip, 80).to_socket_addrs() {
        return socket_addr.next().map(|socket| socket.ip());
    }
    None
}

#[instrument(level = "trace", skip_all)]
fn parse_possible_ipv6(ip: &str, webfront_url: &str) -> Result<IpAddr, AddrParseError> {
    let ip_trim = ip.trim_matches('/').trim_matches(':');

    if let Some(ip) = try_resolve_from_str(ip_trim) {
        trace!("Found socket address of: {ip}, from: {ip_trim}");
        return Ok(ip);
    }

    match ip_trim.parse::<IpAddr>() {
        Ok(ip) => Ok(ip),
        Err(err) => {
            const HTTP_ENDING: &str = "//";
            if let Some(i) = webfront_url.find(HTTP_ENDING) {
                const PORT_SEPERATOR: char = ':';
                let ip_start = i + HTTP_ENDING.len();
                let ipv6_slice = if let Some(j) = webfront_url[ip_start..].rfind(PORT_SEPERATOR) {
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
