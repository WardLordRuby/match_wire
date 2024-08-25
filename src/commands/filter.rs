use crate::{
    cli::{Cli, Region},
    lowercase_vec,
    not_your_private_keys::LOCATION_PRIVATE_KEY,
    parse_hostname,
    utils::{caching::Cache, json_data::*},
};

use tracing::{error, instrument, trace};

use std::{
    collections::HashSet,
    fs::File,
    io::{self, Write},
    net::{IpAddr, ToSocketAddrs},
    path::Path,
    sync::LazyLock,
};

const MASTER_LOCATION_URL: &str = "https://api.findip.net/";

pub const MASTER_URL: &str = "https://master.iw4.zip/";
const JSON_SERVER_ENDPOINT: &str = "instance";
const FAVORITES_LOC: &str = "players2";
const FAVORITES: &str = "favourites.json";

const DEFAULT_SERVER_CAP: usize = 100;
const LOCAL_HOST: &str = "localhost";

pub const GAME_ID: &str = "H2M";
const CODE_NA: &str = "NA";
const CODE_EU: &str = "EU";

static APAC_CONT_CODES: LazyLock<HashSet<&str>> = LazyLock::new(populate_apac_cont_codes);

fn populate_apac_cont_codes() -> HashSet<&'static str> {
    const APAC_CONT_CODES_ARR: [&str; 3] = ["AS", "OC", "AF"];
    HashSet::from(APAC_CONT_CODES_ARR)
}

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
    fn matches(&self, country_code: &str) -> bool {
        match self {
            Region::NA if country_code != CODE_NA => false,
            Region::EU if country_code != CODE_EU => false,
            Region::Apac if !APAC_CONT_CODES.contains(country_code) => false,
            _ => true,
        }
    }
}

pub async fn get_server_master() -> reqwest::Result<Vec<HostData>> {
    trace!("retreiving master server list");
    let instance_url = format!("{MASTER_URL}{JSON_SERVER_ENDPOINT}");
    reqwest::get(instance_url.as_str())
        .await?
        .json::<Vec<HostData>>()
        .await
}

#[instrument(name = "filter", skip_all)]
pub async fn build_favorites(curr_dir: &Path, args: &Cli, cache: &mut Cache) -> io::Result<bool> {
    let mut ip_collected = 0;
    let mut ips = String::new();
    let mut favorites_json = File::create(curr_dir.join(format!("{FAVORITES_LOC}/{FAVORITES}")))?;
    let limit = args.limit.unwrap_or(DEFAULT_SERVER_CAP);

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
        servers.sort_unstable_by_key(|server| server.clientnum);
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
    Allowed((ServerInfo, String)),
    Filtered((ServerInfo, String)),
    Err(io::Error),
}

#[instrument(level = "trace", skip_all)]
async fn filter_server_list(
    args: &Cli,
    cache: &mut Cache,
) -> reqwest::Result<(Vec<ServerInfo>, bool)> {
    let mut host_list = get_server_master().await?;

    let include = args.includes.as_ref().map(|s| lowercase_vec(s));
    let exclude = args.excludes.as_ref().map(|s| lowercase_vec(s));

    for i in (0..host_list.len()).rev() {
        for j in (0..host_list[i].servers.len()).rev() {
            if host_list[i].servers[j].game != GAME_ID {
                host_list[i].servers.swap_remove(j);
                continue;
            }

            if let Some(team_size_max) = args.team_size_max {
                if host_list[i].servers[j].maxclientnum > team_size_max * 2 {
                    host_list[i].servers.swap_remove(j);
                    continue;
                }
            }

            if let Some(player_min) = args.player_min {
                if host_list[i].servers[j].clientnum < player_min {
                    host_list[i].servers.swap_remove(j);
                    continue;
                }
            }

            let mut hostname_l = None;
            if let Some(ref strings) = include {
                hostname_l = Some(parse_hostname(&host_list[i].servers[j].hostname));
                if !strings
                    .iter()
                    .any(|string| hostname_l.as_ref().unwrap().contains(string))
                {
                    host_list[i].servers.swap_remove(j);
                    continue;
                }
            }
            if let Some(ref strings) = exclude {
                if hostname_l.is_none() {
                    hostname_l = Some(parse_hostname(&host_list[i].servers[j].hostname));
                }
                if strings
                    .iter()
                    .any(|string| hostname_l.as_ref().unwrap().contains(string))
                {
                    host_list[i].servers.swap_remove(j);
                }
            }
        }
        if host_list[i].servers.is_empty() {
            host_list.swap_remove(i);
        }
    }

    if let Some(region) = args.region {
        println!(
            "Determining region of {} servers...",
            host_list.iter().fold(0_usize, |mut count, host| {
                count += host.servers.len();
                count
            })
        );

        let client = reqwest::Client::new();
        let mut server_list = Vec::new();
        let mut tasks = Vec::new();
        let mut check_again = Vec::new();
        let mut new_lookups = HashSet::new();

        for host in host_list {
            for mut server in host.servers {
                match resolve_address(&server.ip, &host.ip_address, &host.webfront_url) {
                    IP::Unchanged => (),
                    IP::Modified(ip) => server.ip = ip.to_string(),
                    IP::Err(err) => {
                        error!("{err}");
                        continue;
                    }
                }
                if let Some(cached_region) = cache.ip_to_region.get(&server.ip) {
                    if region.matches(cached_region) {
                        server_list.push(server);
                        continue;
                    }
                    continue;
                }
                if new_lookups.insert(server.ip.clone()) {
                    let client = client.clone();
                    trace!("Requsting location data for: {}", server.ip);
                    tasks.push(tokio::spawn(async move {
                        let location = match try_location_lookup(&server, client).await {
                            Ok(loc) => loc,
                            Err(err) => return Task::Err(err),
                        };
                        if region.matches(&location.code) {
                            return Task::Allowed((server, location.code));
                        }
                        Task::Filtered((server, location.code))
                    }))
                } else {
                    check_again.push(server)
                }
            }
        }

        let mut failure_count = 0_usize;

        for task in tasks {
            match task.await {
                Ok(result) => match result {
                    Task::Allowed((server, region)) => {
                        cache.update_cache_with(&server, region);
                        server_list.push(server)
                    }
                    Task::Filtered((server, region)) => {
                        cache.push(ServerCache::consume(server, region));
                    }
                    Task::Err(err) => {
                        error!("{err}");
                        failure_count += 1
                    }
                },
                Err(err) => {
                    error!("{err:?}");
                    failure_count += 1
                }
            }
        }

        let update = check_again
            .into_iter()
            .fold(Vec::new(), |mut update, server| {
                if let Some(cached_region) = cache.ip_to_region.get(&server.ip) {
                    update.push(ServerCache::from(&server, cached_region.clone()));
                    if region.matches(cached_region) {
                        server_list.push(server)
                    }
                }
                update
            });

        update.into_iter().for_each(|server| cache.push(server));

        if failure_count > 0 {
            eprintln!("Failed to resolve location for {failure_count} server hoster(s)")
        }

        return Ok((server_list, !new_lookups.is_empty()));
    }
    Ok((
        host_list
            .into_iter()
            .flat_map(|host| host.servers)
            .collect(),
        false,
    ))
}

#[instrument(level = "trace", skip_all)]
pub async fn try_location_lookup(
    server: &ServerInfo,
    client: reqwest::Client,
) -> io::Result<Continent> {
    let location_api_url = format!("{MASTER_LOCATION_URL}{}{LOCATION_PRIVATE_KEY}", server.ip);

    let api_response = client
        .get(location_api_url.as_str())
        .send()
        .await
        .map_err(|err| {
            io::Error::other(format!(
                "{err:?}, outbound url: {location_api_url}, server id: {}",
                server.id
            ))
        })?;

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
            "{err:?}, outbound url: {location_api_url}, server id: {}",
            server.id
        ))),
    }
}

pub enum IP {
    Unchanged,
    Modified(IpAddr),
    Err(io::Error),
}

#[instrument(level = "trace", skip_all)]
pub fn resolve_address(server_ip: &str, host_ip: &str, webfront_url: &str) -> IP {
    let ip_trim = server_ip.trim_matches('/').trim_matches(':');
    if ip_trim.is_empty() || ip_trim == LOCAL_HOST {
        match parse_possible_ipv6(host_ip, webfront_url) {
            Ok(ip) => return IP::Modified(ip),
            Err(err) => return IP::Err(err),
        }
    }
    if let Ok(ip) = ip_trim.parse::<IpAddr>() {
        if ip.is_unspecified() {
            return IP::Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("Addr: {ip}, is unspecified"),
            ));
        }
        if server_ip == ip.to_string() {
            return IP::Unchanged;
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
    match parse_possible_ipv6(host_ip, webfront_url) {
        Ok(ip) => IP::Modified(ip),
        Err(err) => IP::Err(err),
    }
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
