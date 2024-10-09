use crate::{
    cli::{Filters, Region, Source},
    location_api_key::FIND_IP_NET_PRIVATE_KEY,
    lowercase_vec, parse_hostname,
    utils::{
        caching::Cache,
        display::{DisplayCountOf, DisplayGetInfoCount, DisplayServerCount, SingularPlural},
        input::style::{GREEN, RED, WHITE, YELLOW},
        json_data::*,
    },
    LOG_ONLY,
};

use reqwest::Client;
use tokio::{sync::Mutex, task::JoinHandle};
use tracing::{error, info, instrument, trace};

use std::{
    collections::HashSet,
    fmt::Display,
    fs::File,
    io::{self, Write},
    net::{AddrParseError, IpAddr, SocketAddr, ToSocketAddrs},
    path::PathBuf,
    sync::Arc,
};

const MASTER_LOCATION_URL: &str = "https://api.findip.net";

const IW4_MASTER_URL: &str = "http://master.iw4.zip";
const HMW_MASTER_URL: &str = "http://ms.s2mod.to/game-servers";
const JSON_SERVER_ENDPOINT: &str = "/instance";
const SERVER_GET_INFO_ENDPOINT: &str = "/getInfo";
const FAVORITES_LOC: &str = "players2";
const FAVORITES: &str = "favourites.json";

const DEFAULT_H2M_SERVER_CAP: usize = 100;
const DEFUALT_INFO_RETRIES: u8 = 3;
const RETRY_TIME_SCALE: u64 = 800; // ms
const LOCAL_HOST: &str = "localhost";

pub const GAME_ID: &str = "H2M";
const CODE_NA: [char; 2] = ['N', 'A'];
const CODE_EU: [char; 2] = ['E', 'U'];
const APAC_CONT_CODES: [[char; 2]; 3] = [['A', 'F'], ['A', 'S'], ['O', 'C']];

fn serialize_json(into: &mut std::fs::File, from: String) -> io::Result<()> {
    const COMMA: char = ',';
    let ips = if from.ends_with(COMMA) {
        &from[..from.len() - COMMA.len_utf8()]
    } else {
        from.as_str()
    };
    write!(into, "[{ips}]")
}

impl Region {
    fn matches(&self, country_code: [char; 2]) -> bool {
        match self {
            Region::NA if country_code != CODE_NA => false,
            Region::EU if country_code != CODE_EU => false,
            Region::Apac if !APAC_CONT_CODES.contains(&country_code) => false,
            _ => true,
        }
    }
}

async fn get_iw4_master() -> reqwest::Result<Vec<HostData>> {
    trace!("retreiving iw4 master server list");
    let instance_url = format!("{IW4_MASTER_URL}{JSON_SERVER_ENDPOINT}");
    reqwest::get(instance_url.as_str())
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
pub async fn build_favorites(
    curr_dir: Arc<PathBuf>,
    args: &Filters,
    cache: Arc<Mutex<Cache>>,
    version: f64,
) -> io::Result<bool> {
    let mut ip_collected = 0;
    let mut ips = String::new();
    let mut favorites_json = File::create(curr_dir.join(format!("{FAVORITES_LOC}/{FAVORITES}")))?;
    let limit = args.limit.unwrap_or({
        if version < 1.0 {
            DEFAULT_H2M_SERVER_CAP
        } else {
            10000
        }
    });

    if version < 1.0 && limit >= DEFAULT_H2M_SERVER_CAP {
        println!("{YELLOW}NOTE: Currently the in game server browser breaks when you add more than 100 servers to favorites{WHITE}")
    }

    let (mut servers, update_cache) = filter_server_list(args, cache, limit)
        .await
        .map_err(|err| io::Error::other(format!("{err:?}")))?;

    println!(
        "{} match the prameters in the current query",
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

    println!(
        "{GREEN}{FAVORITES} updated with {}{WHITE}",
        DisplayCountOf(ip_collected, "entry", "entries")
    );
    Ok(update_cache)
}

pub struct Server {
    pub source: Sourced,
    pub info: Option<GetInfo>,
}

impl From<HostMeta> for Server {
    /// Real source is Sourced::Iw4  
    /// Source kind is modifed to avoid cloning `ServerInfo` fields into the desired `GetInfo`
    fn from(value: HostMeta) -> Self {
        Server {
            info: Some(GetInfo {
                clients: value.server.clients,
                max_clients: value.server.max_clients,
                private_clients: 0,
                bots: 0,
                game_name: value.server.game,
                game_type: value.server.game_type,
                host_name: value.server.host_name,
            }),
            source: Sourced::Iw4Cached(value.resolved_addr),
        }
    }
}

pub struct GetInfoMetaData {
    msg: String,
    display_url: bool,
    display_socket_addr: bool,
    display_source: bool,
    retries: u8,
    pub url: String,
    pub meta: Sourced,
}

impl GetInfoMetaData {
    pub fn new(meta: Sourced) -> Self {
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
    pub fn set_err_msg(mut self, msg: String) -> Self {
        self.msg = msg;
        self
    }

    #[inline]
    pub fn with_url(&mut self) -> &mut Self {
        self.display_url = true;
        self
    }

    #[inline]
    pub fn with_socket_addr(&mut self) -> &mut Self {
        self.display_socket_addr = true;
        self
    }

    #[inline]
    pub fn with_source(&mut self) -> &mut Self {
        self.display_source = true;
        self
    }

    #[inline]
    /// `GetInfoErr` default display is without addr
    pub fn without_url(&mut self) -> &mut Self {
        self.display_url = false;
        self
    }

    #[inline]
    /// `GetInfoErr` default display is without ip
    pub fn without_ip(&mut self) -> &mut Self {
        self.display_url = false;
        self
    }

    #[inline]
    /// `GetInfoErr` default display is without source
    pub fn without_source(&mut self) -> &mut Self {
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
pub struct UnresponsiveCounter {
    pub hmw: usize,
    pub hmw_cached: usize,
    pub iw4: usize,
    pub iw4_cached: usize,
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

pub enum Request {
    New(Sourced),
    Retry(GetInfoMetaData),
}

pub async fn try_get_info(
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
    let server_responce = match client.get(&meta_data.url).send().await {
        Ok(res) => res,
        Err(err) => return Err(meta_data.set_err_msg(err.without_url().to_string())),
    };
    match server_responce.json::<GetInfo>().await {
        Ok(info) => Ok(Server {
            source: meta_data.meta,
            info: Some(info),
        }),
        Err(err) => Err(meta_data.set_err_msg(err.without_url().to_string())),
    }
}

pub struct HostMeta {
    pub resolved_addr: SocketAddr,
    pub server: ServerInfo,
}

impl HostMeta {
    fn try_from(host_ip: &str, webfront_url: &str, server: ServerInfo) -> Option<Self> {
        resolve_address(&server.ip, host_ip, webfront_url).map_or_else(
            |err| {
                error!(name: LOG_ONLY, "{err}");
                None
            },
            |ip| {
                Some(HostMeta {
                    resolved_addr: SocketAddr::new(ip, server.port),
                    server,
                })
            },
        )
    }
}

pub enum Sourced {
    Hmw(SocketAddr),
    HmwCached(SocketAddr),
    Iw4(HostMeta),
    Iw4Cached(SocketAddr),
}

impl Sourced {
    pub fn to_valid_source(&self) -> Option<Source> {
        match self {
            Self::Hmw(_) => Some(Source::HmwMaster),
            Self::Iw4(_) => Some(Source::Iw4Master),
            Self::HmwCached(_) | Self::Iw4Cached(_) => None,
        }
    }

    pub fn try_from_hmw_master(ip_port: String) -> Option<Self> {
        let (ip, port) = match ip_port
            .rsplit_once(':')
            .map(|(ip, port)| (ip.parse().map_err(|err| (err, ip)), port.parse::<u16>()))
        {
            Some((Ok(ip), Ok(port))) => (ip, port),
            Some((Ok(_), Err(err))) => {
                error!(name: LOG_ONLY, "Unexpected hmw master server formatting: failed to parse port in: {ip_port}, {err}");
                return None;
            }
            Some((Err((err, ip_str)), Ok(port))) => {
                let Some(ip) = try_resolve_from_str(ip_str) else {
                    error!(name: LOG_ONLY, "Unexpected hmw master server formatting: failed to parse ip address in: {ip_port}, {err}");
                    return None;
                };
                trace!("Found socket address of: {ip}, from: {ip_str}");
                (ip, port)
            }
            Some((Err(_), Err(_))) => {
                error!(name: LOG_ONLY, "Unexpected hmw master server formatting: invalid string: {ip_port}");
                return None;
            }
            None => {
                error!(name: LOG_ONLY, "Unexpected hmw master server formatting: address was not formatted with a port: {ip_port}");
                return None;
            }
        };
        Some(Sourced::Hmw(SocketAddr::new(ip, port)))
    }

    pub fn socket_addr(&self) -> SocketAddr {
        match self {
            Sourced::Hmw(addr) | Sourced::HmwCached(addr) | Sourced::Iw4Cached(addr) => *addr,
            Sourced::Iw4(meta) => meta.resolved_addr,
        }
    }
}

pub async fn iw4_servers(cache: Option<&Mutex<Cache>>) -> reqwest::Result<Vec<Sourced>> {
    match get_iw4_master().await {
        Ok(mut hosts) => {
            hosts
                .iter_mut()
                .for_each(|host| host.servers.retain(|server| server.game == GAME_ID));
            hosts.retain(|host| !host.servers.is_empty());
            Ok(hosts
                .into_iter()
                .flat_map(|host| {
                    host.servers
                        .into_iter()
                        .filter_map(|server| {
                            HostMeta::try_from(&host.ip_address, &host.webfront_url, server)
                                .map(Sourced::Iw4)
                        })
                        .collect::<Vec<_>>()
                })
                .collect())
        }
        Err(err) => {
            if let Some(cache) = cache {
                error!(name: LOG_ONLY, "{err}");
                let cache = cache.lock().await;
                return Ok(cache
                    .iw4m
                    .iter()
                    .flat_map(|(&ip, ports)| {
                        ports
                            .iter()
                            .map(|&port| Sourced::Iw4Cached(SocketAddr::new(ip, port)))
                            .collect::<Vec<_>>()
                    })
                    .collect());
            }
            Err(err)
        }
    }
}

pub async fn hmw_servers(cache: Option<&Mutex<Cache>>) -> reqwest::Result<Vec<Sourced>> {
    match get_hmw_master().await {
        Ok(list) => Ok(list
            .into_iter()
            .filter_map(Sourced::try_from_hmw_master)
            .collect()),
        Err(err) => {
            if let Some(cache) = cache {
                error!(name: LOG_ONLY, "{err}");
                let cache = cache.lock().await;
                return Ok(cache
                    .hmw
                    .iter()
                    .flat_map(|(&ip, ports)| {
                        ports
                            .iter()
                            .map(|&port| Sourced::HmwCached(SocketAddr::new(ip, port)))
                            .collect::<Vec<_>>()
                    })
                    .collect());
            }
            Err(err)
        }
    }
}

pub async fn queue_info_requests(
    servers: Vec<Sourced>,
    tasks: &mut Vec<JoinHandle<Result<Server, GetInfoMetaData>>>,
    remove_duplicates: bool,
    client: &Client,
) {
    let mut dup = HashSet::new();
    for server in servers.into_iter() {
        if remove_duplicates && !dup.insert(server.socket_addr()) {
            continue;
        }

        let client = client.clone();
        tasks.push(tokio::spawn(async move {
            try_get_info(Request::New(server), client).await
        }));
    }
}

fn to_server(disregard_meta: bool, vec: Vec<Sourced>) -> Vec<Server> {
    let no_info = |source: Sourced| -> Server { Server { source, info: None } };
    let with_info = |source: Sourced| -> Server {
        if let Sourced::Iw4(meta) = source {
            Server::from(meta)
        } else {
            Server { source, info: None }
        }
    };

    let operation = if disregard_meta { no_info } else { with_info };
    vec.into_iter().map(operation).collect()
}

#[instrument(level = "trace", skip_all)]
async fn filter_server_list(
    args: &Filters,
    cache: Arc<Mutex<Cache>>,
    limit: usize,
) -> reqwest::Result<(Vec<Server>, bool)> {
    let mut servers = Vec::new();

    if let Some(ref list) = args.source {
        if list.contains(&Source::Iw4Master) {
            match iw4_servers(Some(&cache)).await {
                Ok(iw4) => servers = iw4,
                Err(err) => error!("{err}"),
            }
        }
        if list.contains(&Source::HmwMaster) {
            match hmw_servers(Some(&cache)).await {
                Ok(ref mut hmw) => servers.append(hmw),
                Err(err) => error!("{err}"),
            }
        }
    } else {
        servers = iw4_servers(Some(&cache)).await.unwrap_or_else(|err| {
            error!("{err}");
            Vec::new()
        });
        match hmw_servers(Some(&cache)).await {
            Ok(ref mut hmw) => servers.append(hmw),
            Err(err) => error!("{err}"),
        }
    };

    let cache_modified = if let Some(ref regions) = args.region {
        println!(
            "Determining region of {}...",
            DisplayServerCount(servers.len(), GREEN)
        );

        let mut server_list = Vec::new();
        let mut tasks = Vec::new();
        let mut check_again = Vec::new();
        let mut new_lookups = HashSet::new();
        let client = reqwest::Client::new();

        let mut cache = cache.lock().await;

        for sourced_data in servers {
            let socket_addr = sourced_data.socket_addr();
            if let Some(cached_region) = cache.ip_to_region.get(&socket_addr.ip()) {
                if regions.iter().any(|region| region.matches(*cached_region)) {
                    server_list.push(sourced_data);
                }
                continue;
            }
            if new_lookups.insert(socket_addr.ip()) {
                let client = client.clone();
                trace!("Requsting location data for: {}", socket_addr.ip());
                tasks.push(tokio::spawn(async move {
                    try_location_lookup(&socket_addr.ip(), client)
                        .await
                        .map(|location| (sourced_data, location.code))
                }))
            } else {
                check_again.push(sourced_data)
            }
        }

        let mut failure_count = 0_usize;

        for task in tasks {
            match task.await {
                Ok(Ok((sourced_data, cont_code))) => {
                    cache
                        .ip_to_region
                        .insert(sourced_data.socket_addr().ip(), cont_code);
                    if regions.iter().any(|region| region.matches(cont_code)) {
                        server_list.push(sourced_data)
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
                if regions.iter().any(|region| region.matches(*cached_region)) {
                    server_list.push(sourced_data)
                }
            }
        }

        if failure_count > 0 {
            eprintln!(
                "{RED}Failed to resolve location for {failure_count} server {}{WHITE}",
                SingularPlural(failure_count, "hoster", "hosters")
            )
        }

        servers = server_list;
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
        let mut tasks = Vec::with_capacity(servers.len());
        let mut host_list = Vec::with_capacity(servers.len());

        let client = reqwest::Client::builder()
            .timeout(tokio::time::Duration::from_secs(3))
            .build()
            .unwrap();

        queue_info_requests(servers, &mut tasks, true, &client).await;

        let use_backup_server_info =
            !args.with_bots && !args.without_bots && args.include_unresponsive;
        let mut did_not_respond = UnresponsiveCounter::default();
        let mut used_backup_data = 0_usize;
        let mut sent_retires = false;
        let max_attempts = args.retry_max.unwrap_or(DEFUALT_INFO_RETRIES);

        while !tasks.is_empty() {
            println!("{}", DisplayGetInfoCount(tasks.len(), sent_retires));
            let mut retries = Vec::new();
            for task in tasks {
                match task.await {
                    Ok(Ok(server)) => host_list.push(server),
                    Ok(Err(mut err)) => {
                        if err.retries < max_attempts {
                            let client = client.clone();
                            retries.push(tokio::task::spawn(async move {
                                tokio::time::sleep(tokio::time::Duration::from_millis(
                                    RETRY_TIME_SCALE * (err.retries + 1) as u64,
                                ))
                                .await;
                                try_get_info(Request::Retry(err), client).await
                            }));
                        } else {
                            did_not_respond.add(&err.meta);
                            error!(name: LOG_ONLY, "{}", err.with_socket_addr().with_source());
                            if use_backup_server_info {
                                if let Sourced::Iw4(meta) = err.meta {
                                    used_backup_data += 1;
                                    host_list.push(Server::from(meta));
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

        if did_not_respond.total() > 0 {
            if use_backup_server_info {
                println!(
                    "Included outdated server data for {YELLOW}{used_backup_data}{WHITE} \
                    of {} that did not respond to 'getInfo' request",
                    DisplayServerCount(did_not_respond.total(), RED)
                )
            } else {
                eprintln!("{did_not_respond}");
            }
        }

        let include = args.includes.as_ref().map(|s| lowercase_vec(s));
        let exclude = args.excludes.as_ref().map(|s| lowercase_vec(s));

        for i in (0..host_list.len()).rev() {
            let server = &host_list[i];

            let Some(ref info) = server.info else {
                host_list.swap_remove(i);
                continue;
            };

            if let Some(team_size_max) = args.team_size_max {
                if info.max_clients > team_size_max * 2 {
                    host_list.swap_remove(i);
                    continue;
                }
            }

            if let Some(player_min) = args.player_min {
                if info.clients < player_min {
                    host_list.swap_remove(i);
                    continue;
                }
            }

            if args.with_bots && info.bots == 0 {
                host_list.swap_remove(i);
                continue;
            }

            if args.without_bots && info.bots != 0 {
                host_list.swap_remove(i);
                continue;
            }

            let mut hostname_l = None;
            if let Some(ref strings) = include {
                hostname_l = Some(parse_hostname(&info.host_name));
                if !strings
                    .iter()
                    .any(|string| hostname_l.as_ref().unwrap().contains(string))
                {
                    host_list.swap_remove(i);
                    continue;
                }
            }
            if let Some(ref strings) = exclude {
                if hostname_l.is_none() {
                    hostname_l = Some(parse_hostname(&info.host_name));
                }
                if strings
                    .iter()
                    .any(|string| hostname_l.as_ref().unwrap().contains(string))
                {
                    host_list.swap_remove(i);
                }
            }
        }
        host_list
    } else {
        to_server(servers.len() <= limit, servers)
    };

    Ok((servers, cache_modified))
}

#[instrument(level = "trace", skip_all)]
pub async fn try_location_lookup(
    ip: &IpAddr,
    client: reqwest::Client,
) -> Result<Continent, String> {
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
                .unwrap_or_else(|| String::from("unknown error")))
        }
        Err(err) => Err(format!("{}, ip: {ip}", err.without_url())),
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
    match ip.parse::<IpAddr>() {
        Ok(ip) => Ok(ip),
        Err(err) => {
            const HTTP_ENDING: &str = "//";
            if let Some(i) = webfront_url.find(HTTP_ENDING) {
                const PORT_SEPERATOR: char = ':';
                let ip_start = i + HTTP_ENDING.len();
                let ipv6_slice = if let Some(j) = webfront_url[ip_start..].rfind(PORT_SEPERATOR) {
                    let ip_end = j + ip_start;
                    if ip_end <= ip_start {
                        error!(name: LOG_ONLY, "Bad ipv6 string slice");
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
