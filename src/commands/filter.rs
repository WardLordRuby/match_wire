use crate::{
    cli::{Filters, Region, Source},
    lowercase_vec,
    not_your_private_keys::LOCATION_PRIVATE_KEY,
    parse_hostname,
    utils::{caching::Cache, json_data::*},
    LOG_ONLY,
};

use reqwest::Client;
use tokio::{sync::Mutex, task::JoinHandle};
use tracing::{error, info, instrument, trace};

use std::{
    collections::HashSet,
    fs::File,
    io::{self, Write},
    net::{IpAddr, SocketAddr, ToSocketAddrs},
    path::PathBuf,
    sync::Arc,
};

const MASTER_LOCATION_URL: &str = "https://api.findip.net/";

const MASTER_URL: &str = "http://master.iw4.zip/";
const HMW_MASTER_URL: &str = "http://ms.s2mod.to/game-servers";
const JSON_SERVER_ENDPOINT: &str = "instance";
const SERVER_GET_INFO_ENDPOINT: &str = "/getInfo";
const FAVORITES_LOC: &str = "players2";
const FAVORITES: &str = "favourites.json";

const DEFAULT_SERVER_CAP: usize = 100;
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
    let instance_url = format!("{MASTER_URL}{JSON_SERVER_ENDPOINT}");
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
            DEFAULT_SERVER_CAP
        } else {
            1000
        }
    });

    if version < 1.0 && limit >= DEFAULT_SERVER_CAP {
        println!("NOTE: Currently the in game server browser breaks when you add more than 100 servers to favorites")
    }

    let (mut servers, update_cache) = filter_server_list(args, cache)
        .await
        .map_err(|err| io::Error::other(format!("{err:?}")))?;

    println!(
        "{} servers match the prameters in the current query",
        servers.len()
    );

    if servers.len() > limit {
        // MARK: FIXME
        // console now gets overwhelmed with messages if servers are added that do not reply to GetInfo requests
        // aka Server.info == None
        servers.sort_unstable_by_key(|server| server.info.as_ref().map_or(0, |info| info.clients));
    }

    for server in servers.iter().rev() {
        // MARK: DEBUG
        // make sure that this formats ipv6 how the favorites.json expects
        ips.push_str(&format!("\"{}\",", server.socket_addr));
        ip_collected += 1;
        if ip_collected == limit {
            break;
        }
    }

    serialize_json(&mut favorites_json, ips)?;

    println!("{FAVORITES} updated with {ip_collected} entries");
    Ok(update_cache)
}

enum Task {
    Allowed((Server, [char; 2])),
    Filtered((Server, [char; 2])),
    Err(io::Error),
}

pub struct Server {
    pub source: Sourced,
    pub socket_addr: SocketAddr,
    pub info: Option<GetInfo>,
}

impl Server {
    /// Real source is Sourced::Iw4
    fn from(value: HostMeta) -> Option<Self> {
        value.server.ip.parse().ok().map(|ip| Server {
            socket_addr: SocketAddr::new(ip, value.server.port),
            info: Some(GetInfo {
                clients: value.server.clients,
                max_clients: value.server.max_clients,
                private_clients: 0,
                bots: 0,
                game_name: value.server.game,
                game_type: value.server.game_type,
                host_name: value.server.host_name,
            }),
            source: Sourced::Iw4Cached(SocketAddr::new(ip, value.server.port)),
        })
    }
}

pub struct GetInfoErr {
    pub err: String,
    pub meta: Sourced,
}

pub async fn try_get_info(
    socket_addr: SocketAddr,
    meta: Sourced,
    client: reqwest::Client,
) -> Result<Server, GetInfoErr> {
    let addr = format!("http://{}{SERVER_GET_INFO_ENDPOINT}", socket_addr);
    let server_responce = match client.get(&addr).send().await {
        Ok(res) => res,
        Err(err) => {
            return Err(GetInfoErr {
                err: format!("{err}, with ip: {socket_addr}"),
                meta,
            })
        }
    };
    match server_responce.json::<GetInfo>().await {
        Ok(info) => Ok(Server {
            source: meta,
            socket_addr,
            info: Some(info),
        }),
        Err(err) => Err(GetInfoErr {
            err: err.to_string(),
            meta,
        }),
    }
}

pub struct HostMeta {
    pub host_ip: String,
    pub webfront_url: String,
    pub server: ServerInfo,
    pub server_resolved_addr: Option<IpAddr>,
}

impl HostMeta {
    fn from(host_ip: &str, webfront_url: &str, server: ServerInfo) -> Self {
        HostMeta {
            host_ip: host_ip.to_string(),
            webfront_url: webfront_url.to_string(),
            server,
            server_resolved_addr: None,
        }
    }
}

pub enum Sourced {
    Hmw(String),
    HmwCached(SocketAddr),
    Iw4(HostMeta),
    Iw4Cached(SocketAddr),
    Failed,
}

impl Sourced {
    pub fn to_valid_source(&self) -> Option<Source> {
        match self {
            Self::Hmw(_) => Some(Source::HmwMaster),
            Self::Iw4(_) => Some(Source::Iw4Master),
            Self::HmwCached(_) | Self::Iw4Cached(_) | Self::Failed => None,
        }
    }

    pub fn try_parse_socket_addr(&mut self) -> Result<SocketAddr, String> {
        match self {
            Sourced::Iw4(ref mut meta) => {
                let ip = match resolve_address(meta) {
                    Ok(ip) => ip,
                    Err(err) => {
                        return Err(format!(
                            "could not resolve address from: {} or {}, {err}",
                            meta.server.ip, meta.host_ip
                        ))
                    }
                };
                Ok(SocketAddr::new(ip, meta.server.port))
            }
            Sourced::Iw4Cached(ref cached) | Sourced::HmwCached(ref cached) => Ok(*cached),
            Sourced::Hmw(ref ip_port) => {
                // we assume hmw master always have resolved ips
                let (ip, port) = match ip_port
                    .rsplit_once(':')
                    .map(|(ip, port)| (ip.parse(), port.parse::<u16>()))
                {
                    Some((Ok(ip), Ok(port))) => (ip, port),
                    Some((_, Err(err))) => {
                        return Err(format!("failed to parse port in: {ip_port}, {err}"))
                    }
                    Some((Err(err), _)) => {
                        return Err(format!("failed to parse ip address in: {ip_port}, {err}"))
                    }
                    None => {
                        return Err(format!("address was not formatted with a port: {ip_port}"))
                    }
                };
                Ok(SocketAddr::new(ip, port))
            }
            Sourced::Failed => Err(String::from("Nothing to parse in failed attempt")),
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
                        .map(|server| {
                            Sourced::Iw4(HostMeta::from(
                                &host.ip_address,
                                &host.webfront_url,
                                server,
                            ))
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
        Ok(list) => Ok(list.into_iter().map(Sourced::Hmw).collect()),
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
    tasks: &mut Vec<JoinHandle<Result<Server, GetInfoErr>>>,
    remove_duplicates: bool,
    client: &Client,
) {
    let mut dup = HashSet::new();
    for mut server in servers.into_iter() {
        let socket_addr = match server.try_parse_socket_addr() {
            Ok(addr) => addr,
            Err(err) => {
                error!(name: LOG_ONLY, "{err}");
                continue;
            }
        };
        if remove_duplicates && !dup.insert(socket_addr) {
            continue;
        }

        let client = client.clone();
        tasks.push(tokio::spawn(async move {
            try_get_info(socket_addr, server, client).await
        }));
    }
}

fn to_server(vec: Vec<Sourced>) -> Vec<Server> {
    let mut output = Vec::with_capacity(vec.len());
    for mut server in vec {
        let socket_addr = match server.try_parse_socket_addr() {
            Ok(ip) => ip,
            Err(err) => {
                error!(name: LOG_ONLY, "{err}");
                continue;
            }
        };
        let server = match server {
            Sourced::Iw4(meta) => match Server::from(meta) {
                Some(server) => server,
                None => continue,
            },
            Sourced::Iw4Cached(_) | Sourced::HmwCached(_) | Sourced::Hmw(_) => Server {
                source: server,
                socket_addr,
                info: None,
            },
            Sourced::Failed => unreachable!("by try parse addr err above"),
        };
        output.push(server);
    }
    output
}

#[instrument(level = "trace", skip_all)]
async fn filter_server_list(
    args: &Filters,
    cache: Arc<Mutex<Cache>>,
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

    let servers = if args.excludes.is_some()
        || args.includes.is_some()
        || args.player_min.is_some()
        || args.team_size_max.is_some()
    {
        let mut tasks = Vec::with_capacity(servers.len());
        let mut host_list = Vec::with_capacity(servers.len());

        let client = reqwest::Client::new();
        queue_info_requests(servers, &mut tasks, true, &client).await;

        for task in tasks {
            match task.await {
                Ok(result) => match result {
                    Ok(server) => host_list.push(server),
                    Err(info) => {
                        error!(name: LOG_ONLY, "{}", info.err);
                        // if args.bots.is_none {} // wrap in this once done
                        if let Sourced::Iw4(meta) = info.meta {
                            if let Some(server) = Server::from(meta) {
                                host_list.push(server);
                            }
                        }
                    }
                },
                Err(err) => error!(name: LOG_ONLY, "{err:?}"),
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
        to_server(servers)
    };

    if let Some(region) = args.region {
        println!("Determining region of {} servers...", servers.len());

        let mut server_list = Vec::new();
        let mut tasks = Vec::new();
        let mut check_again = Vec::new();
        let mut new_lookups = HashSet::new();
        let client = reqwest::Client::new();

        let mut cache = cache.lock().await;

        for server in servers {
            if let Some(cached_region) = cache.ip_to_region.get(&server.socket_addr.ip()) {
                if region.matches(*cached_region) {
                    server_list.push(server);
                }
                continue;
            }
            if new_lookups.insert(server.socket_addr.ip()) {
                let client = client.clone();
                trace!("Requsting location data for: {}", server.socket_addr.ip());
                tasks.push(tokio::spawn(async move {
                    let location = match try_location_lookup(&server.socket_addr.ip(), client).await
                    {
                        Ok(loc) => loc,
                        Err(err) => return Task::Err(err),
                    };
                    if region.matches(location.code) {
                        return Task::Allowed((server, location.code));
                    }
                    Task::Filtered((server, location.code))
                }))
            } else {
                check_again.push(server)
            }
        }

        let mut failure_count = 0_usize;

        for task in tasks {
            match task.await {
                Ok(result) => match result {
                    Task::Allowed((server, region)) => {
                        cache.ip_to_region.insert(server.socket_addr.ip(), region);
                        server_list.push(server)
                    }
                    Task::Filtered((server, region)) => {
                        cache.ip_to_region.insert(server.socket_addr.ip(), region);
                    }
                    Task::Err(err) => {
                        error!(name: LOG_ONLY, "{err}");
                        failure_count += 1
                    }
                },
                Err(err) => {
                    error!(name: LOG_ONLY, "{err:?}");
                    failure_count += 1
                }
            }
        }

        match new_lookups.len() {
            0 => (),
            len => info!("Made {len} new location requests"),
        }

        for server in check_again {
            if let Some(cached_region) = cache.ip_to_region.get(&server.socket_addr.ip()) {
                if region.matches(*cached_region) {
                    server_list.push(server)
                }
            }
        }

        if failure_count > 0 {
            eprintln!("Failed to resolve location for {failure_count} server hoster(s)")
        }

        return Ok((server_list, !new_lookups.is_empty()));
    }
    Ok((servers, false))
}

#[instrument(level = "trace", skip_all)]
pub async fn try_location_lookup(ip: &IpAddr, client: reqwest::Client) -> io::Result<Continent> {
    let location_api_url = format!("{MASTER_LOCATION_URL}{}{LOCATION_PRIVATE_KEY}", ip);

    let api_response = client
        .get(location_api_url.as_str())
        .send()
        .await
        .map_err(|err| io::Error::other(format!("{err:?}, outbound url: {location_api_url}",)))?;

    match api_response.json::<ServerLocation>().await {
        Ok(json) => {
            if let Some(code) = json.continent {
                return Ok(code);
            }
            Err(io::Error::other(
                json.message
                    .unwrap_or_else(|| String::from("unknown error")),
            ))
        }
        Err(err) => Err(io::Error::other(format!(
            "{err:?}, outbound url: {location_api_url}",
        ))),
    }
}

#[instrument(level = "trace", skip_all)]
fn resolve_address(host_meta: &mut HostMeta) -> io::Result<IpAddr> {
    let server_ip = host_meta.server.ip.as_str();
    let ip_trim = server_ip.trim_matches('/').trim_matches(':');
    if ip_trim.is_empty() || ip_trim == LOCAL_HOST {
        let ip = parse_possible_ipv6(&host_meta.host_ip, &host_meta.webfront_url)?;
        host_meta.server.ip = ip.to_string();
        return Ok(ip);
    }
    if let Ok(ip) = ip_trim.parse::<IpAddr>() {
        if ip.is_unspecified() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("Addr: {ip}, is unspecified"),
            ));
        }
        if server_ip == ip.to_string() {
            return Ok(ip);
        }
        trace!("{server_ip} trimmed and parsed to: {ip}");
        host_meta.server.ip = ip.to_string();
        return Ok(ip);
    }

    if let Ok(mut socket_addr) = (ip_trim, 80).to_socket_addrs() {
        if let Some(ip) = socket_addr.next().map(|socket| socket.ip()) {
            trace!("Found socket address of: {ip}, from: {ip_trim}");
            host_meta.server.ip = ip.to_string();
            return Ok(ip);
        }
    }

    let ip = parse_possible_ipv6(&host_meta.host_ip, &host_meta.webfront_url)?;
    host_meta.server.ip = ip.to_string();
    Ok(ip)
}

#[instrument(level = "trace", skip_all)]
fn parse_possible_ipv6(ip: &str, webfront_url: &str) -> io::Result<IpAddr> {
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
                        return Err(io::Error::other("Failed to parse ip"));
                    }
                    &webfront_url[ip_start..ip_end]
                } else {
                    &webfront_url[ip_start..]
                };
                trace!("Parsed: {ipv6_slice}, from webfront_url");
                return ipv6_slice.parse::<IpAddr>().map_err(io::Error::other);
            }
            Err(io::Error::other(err))
        }
    }
}
