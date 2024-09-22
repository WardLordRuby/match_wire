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
    collections::{HashMap, HashSet},
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

pub async fn get_iw4_master() -> reqwest::Result<Vec<HostData>> {
    trace!("retreiving iw4 master server list");
    let instance_url = format!("{MASTER_URL}{JSON_SERVER_ENDPOINT}");
    reqwest::get(instance_url.as_str())
        .await?
        .json::<Vec<HostData>>()
        .await
}

pub async fn get_hmw_master() -> reqwest::Result<Vec<String>> {
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
) -> io::Result<bool> {
    let mut ip_collected = 0;
    let mut ips = String::new();
    let mut favorites_json = File::create(curr_dir.join(format!("{FAVORITES_LOC}/{FAVORITES}")))?;
    let limit = args.limit.unwrap_or(DEFAULT_SERVER_CAP);

    // MARK: TODO
    // Use version num of h2m-mod to determine if to display this
    if limit >= DEFAULT_SERVER_CAP {
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
        servers.sort_unstable_by_key(|server| server.info.clients);
    }

    for server in servers.iter().rev() {
        ips.push_str(&format!("\"{}:{}\",", server.ip, server.port));
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

#[derive(Clone)]
pub struct Server {
    pub ip: IpAddr,
    pub port: u16,
    pub info: GetInfo,
}

impl Server {
    pub fn socket_addr(&self) -> SocketAddr {
        SocketAddr::new(self.ip, self.port)
    }
}

pub struct GetInfoErr {
    pub err: String,
    pub meta: Option<HostMeta>,
}

pub async fn try_get_info(
    ip: IpAddr,
    port: u16,
    mut meta: Option<HostMeta>,
    client: reqwest::Client,
) -> Result<Server, GetInfoErr> {
    let addr = format!("http://{}:{}{SERVER_GET_INFO_ENDPOINT}", ip, port);
    let server_responce = match client.get(&addr).send().await {
        Ok(res) => res,
        Err(err) => {
            return Err(GetInfoErr {
                err: format!("{err}, with ip: {ip}:{port}"),
                meta,
            })
        }
    };
    server_responce
        .json::<GetInfo>()
        .await
        .map(|info| Server { ip, port, info })
        .map_err(|err| {
            if let Some(ref mut data) = meta {
                data.server_resolved_addr = Some(ip);
            }
            GetInfoErr {
                err: err.to_string(),
                meta,
            }
        })
}

pub struct HostMeta {
    pub host_ip: String,
    pub webfront_url: String,
    pub server: ServerInfo,
    pub server_resolved_addr: Option<IpAddr>,
}

pub async fn insert_iw4_servers(
    cache: Option<&Mutex<Cache>>,
    map: &mut HashMap<String, Option<HostMeta>>,
) {
    match get_iw4_master().await {
        Ok(mut hosts) => {
            hosts
                .iter_mut()
                .for_each(|host| host.servers.retain(|server| server.game == GAME_ID));
            hosts.retain(|host| !host.servers.is_empty());
            for host in hosts {
                for server in host.servers {
                    map.insert(
                        format!("{}:{}", server.ip, server.port),
                        Some(HostMeta {
                            host_ip: host.ip_address.clone(),
                            webfront_url: host.webfront_url.clone(),
                            server,
                            server_resolved_addr: None,
                        }),
                    );
                }
            }
        }
        Err(err) => {
            error!(name: LOG_ONLY, "{err}");
            if let Some(cache) = cache {
                let cache = cache.lock().await;
                cache.iw4m.iter().for_each(|(ip, ports)| {
                    for port in ports {
                        map.insert(format!("{}:{}", ip, port), None);
                    }
                });
            }
        }
    }
}

pub async fn insert_hmw_servers(
    cache: Option<&Mutex<Cache>>,
    map: &mut HashMap<String, Option<HostMeta>>,
) {
    match get_hmw_master().await {
        Ok(list) => {
            list.into_iter().for_each(|ip_port| {
                map.insert(ip_port, None);
            });
        }
        Err(err) => {
            error!(name: LOG_ONLY, "{err}");
            if let Some(cache) = cache {
                let cache = cache.lock().await;
                cache.hmw.iter().for_each(|(ip, ports)| {
                    for port in ports {
                        map.insert(format!("{}:{}", ip, port), None);
                    }
                });
            }
        }
    }
}

pub async fn queue_info_requests(
    map: HashMap<String, Option<HostMeta>>,
    tasks: &mut Vec<JoinHandle<Result<Server, GetInfoErr>>>,
    client: &Client,
) {
    for (ip_port, host_meta) in map.into_iter() {
        let (ip, port) = match ip_port
            .rsplit_once(':')
            .map(|(ip, port)| (ip, port.parse::<u16>()))
        {
            Some((ip, Ok(port))) => (ip, port),
            Some((_, Err(err))) => {
                error!(name: LOG_ONLY, "failed to parse port in: {ip_port}, {err}");
                continue;
            }
            None => {
                error!(name: LOG_ONLY, "address was not formatted with a port: {ip_port}");
                continue;
            }
        };
        let ip = match resolve_address(ip, host_meta.as_ref()) {
            IP::Unchanged(ip) => ip,
            IP::Modified(new) => new,
            IP::Err(err) => {
                error!(name: LOG_ONLY, "could not resolve address: {ip_port}, {err}");
                continue;
            }
        };
        let client = client.clone();
        tasks.push(tokio::spawn(async move {
            try_get_info(ip, port, host_meta, client).await
        }));
    }
}

#[instrument(level = "trace", skip_all)]
async fn filter_server_list(
    args: &Filters,
    cache: Arc<Mutex<Cache>>,
) -> reqwest::Result<(Vec<Server>, bool)> {
    let mut servers = HashMap::new();

    // MARK: OPTIMIZE
    // we don't need to do this if we never filter against any of the retrieved data
    if let Some(ref list) = args.source {
        if list.contains(&Source::Iw4Master) {
            insert_iw4_servers(Some(&cache), &mut servers).await;
        }
        if list.contains(&Source::HmwMaster) {
            insert_hmw_servers(Some(&cache), &mut servers).await;
        }
    } else {
        insert_iw4_servers(Some(&cache), &mut servers).await;
        insert_hmw_servers(Some(&cache), &mut servers).await;
    };

    let mut tasks = Vec::with_capacity(servers.len());
    let mut host_list = Vec::with_capacity(servers.len());

    let client = reqwest::Client::new();
    queue_info_requests(servers, &mut tasks, &client).await;

    for task in tasks {
        match task.await {
            Ok(result) => match result {
                Ok(server) => host_list.push(server),
                Err(info) => {
                    error!(name: LOG_ONLY, "{}", info.err);
                    // Do we want to do something with metadata?
                    // if let Some(data) = err.meta {}
                }
            },
            Err(err) => error!(name: LOG_ONLY, "{err:?}"),
        }
    }

    let include = args.includes.as_ref().map(|s| lowercase_vec(s));
    let exclude = args.excludes.as_ref().map(|s| lowercase_vec(s));

    for i in (0..host_list.len()).rev() {
        let server = &host_list[i];
        if let Some(team_size_max) = args.team_size_max {
            if server.info.max_clients > team_size_max * 2 {
                host_list.swap_remove(i);
                continue;
            }
        }

        if let Some(player_min) = args.player_min {
            if server.info.clients < player_min {
                host_list.swap_remove(i);
                continue;
            }
        }

        let mut hostname_l = None;
        if let Some(ref strings) = include {
            hostname_l = Some(parse_hostname(&server.info.host_name));
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
                hostname_l = Some(parse_hostname(&server.info.host_name));
            }
            if strings
                .iter()
                .any(|string| hostname_l.as_ref().unwrap().contains(string))
            {
                host_list.swap_remove(i);
            }
        }
    }

    if let Some(region) = args.region {
        println!("Determining region of {} servers...", host_list.len());

        let mut server_list = Vec::new();
        let mut tasks = Vec::new();
        let mut check_again = Vec::new();
        let mut new_lookups = HashSet::new();

        let mut cache = cache.lock().await;

        for server in host_list {
            // we assume hmw master always have resolved ips
            if let Some(cached_region) = cache.ip_to_region.get(&server.ip) {
                if region.matches(*cached_region) {
                    server_list.push(server);
                    continue;
                }
                continue;
            }
            if new_lookups.insert(server.ip) {
                let client = client.clone();
                trace!("Requsting location data for: {}", server.ip);
                tasks.push(tokio::spawn(async move {
                    let location = match try_location_lookup(&server.ip, client).await {
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

        let source = args
            .source
            .as_ref()
            .and_then(|sources| (sources.len() == 1).then(|| sources.first()))
            .flatten();
        for task in tasks {
            match task.await {
                Ok(result) => match result {
                    Task::Allowed((server, region)) => {
                        cache.update_cache_with(&server, Some(region), source);
                        server_list.push(server)
                    }
                    Task::Filtered((server, region)) => {
                        cache.push(server, Some(region), source);
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

        let update = check_again
            .into_iter()
            .fold(Vec::new(), |mut update, server| {
                if let Some(cached_region) = cache.ip_to_region.get(&server.ip) {
                    update.push((server.clone(), *cached_region));
                    if region.matches(*cached_region) {
                        server_list.push(server)
                    }
                }
                update
            });

        update
            .into_iter()
            .for_each(|(server, region)| cache.push(server, Some(region), source));

        if failure_count > 0 {
            eprintln!("Failed to resolve location for {failure_count} server hoster(s)")
        }

        return Ok((server_list, !new_lookups.is_empty()));
    }
    Ok((host_list, false))
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

enum IP {
    Unchanged(IpAddr),
    Modified(IpAddr),
    Err(io::Error),
}

#[instrument(level = "trace", skip_all)]
fn resolve_address(server_ip: &str, host_meta: Option<&HostMeta>) -> IP {
    let ip_trim = server_ip.trim_matches('/').trim_matches(':');
    if ip_trim.is_empty() || ip_trim == LOCAL_HOST {
        if let Some(meta) = host_meta {
            return match parse_possible_ipv6(&meta.host_ip, &meta.webfront_url) {
                Ok(ip) => IP::Modified(ip),
                Err(err) => IP::Err(err),
            };
        }
        return IP::Err(io::Error::other(format!("ip: {server_ip}, is invalid")));
    }
    if let Ok(ip) = ip_trim.parse::<IpAddr>() {
        if ip.is_unspecified() {
            return IP::Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("Addr: {ip}, is unspecified"),
            ));
        }
        if server_ip == ip.to_string() {
            return IP::Unchanged(ip);
        }
        trace!("{server_ip} trimmed and parsed to: {ip}");
        return IP::Modified(ip);
    }

    if let Ok(mut socket_addr) = (ip_trim, 80).to_socket_addrs() {
        if let Some(ip) = socket_addr.next().map(|socket| socket.ip()) {
            trace!("Found socket address of: {ip}, from: {ip_trim}");
            return IP::Modified(ip);
        }
    }
    if let Some(meta) = host_meta {
        return match parse_possible_ipv6(&meta.host_ip, &meta.webfront_url) {
            Ok(ip) => IP::Modified(ip),
            Err(err) => IP::Err(err),
        };
    }
    IP::Err(io::Error::other(format!("ip: {server_ip}, is invalid")))
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
