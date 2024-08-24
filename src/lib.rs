pub mod cli;
pub mod json_data;
pub mod not_your_private_keys;

use cli::{Cli, Region};
use json_data::*;
use not_your_private_keys::LOCATION_PRIVATE_KEY;
use std::{
    collections::HashSet,
    fs::File,
    io,
    io::Write,
    net::{IpAddr, ToSocketAddrs},
    path::Path,
    sync::LazyLock,
    time::Duration,
};

pub const VERSION_URL: &str =
    "https://gist.githubusercontent.com/WardLordRuby/324d7c1fb454aed5f5155a790bd028f0/raw/";

pub const DEFAULT_SERVER_CAP: usize = 100;
pub const H2M_MAX_CLIENT_NUM: i64 = 18;
pub const H2M_MAX_TEAM_SIZE: i64 = 9;

pub const REQUIRED_FILES: [&str; 3] = ["h1_mp64_ship.exe", "h2m-mod", "players2"];

const GAME_ID: &str = "H2M";
const CODE_NA: &str = "NA";
const CODE_EU: &str = "EU";

const MASTER_LOCATION_URL: &str = "https://api.findip.net/";

const MASTER_URL: &str = "https://master.iw4.zip/";
const JSON_SERVER_ENDPOINT: &str = "instance";
const FAVORITES_LOC: &str = "players2";
const FAVORITES: &str = "favourites.json";

static APAC_CONT_CODES: LazyLock<HashSet<&str>> = LazyLock::new(populate_apac_cont_codes);

fn populate_apac_cont_codes() -> HashSet<&'static str> {
    const APAC_CONT_CODES_ARR: [&str; 3] = ["AS", "OC", "AF"];
    HashSet::from(APAC_CONT_CODES_ARR)
}

pub async fn get_latest_version() -> reqwest::Result<()> {
    let current_version = env!("CARGO_PKG_VERSION");
    let client = reqwest::Client::new();
    let version = client
        .get(VERSION_URL)
        .timeout(Duration::from_secs(6))
        .send()
        .await?
        .json::<Version>()
        .await?;
    if current_version != version.latest {
        println!(
            "New version available for download at: \n\
            https://github.com/WardLordRuby/H2M_favorites/releases/download/v{}/h2m_favorites.exe",
            version.latest
        )
    }
    Ok(())
}

#[derive(Debug)]
pub enum Operation {
    All,
    Any,
    Count,
}

pub enum OperationResult<'a> {
    Bool(bool),
    Count((usize, HashSet<&'a str>)),
}

/// `Operation::All` and `Operation::Any` map to `OperationResult::bool(_result_)`  
/// `Operation::Count` maps to `OperationResult::Count((_num_found_, _HashSet<_&input_list_>))`  
/// when matching you will always have to `_ => unreachable()` for the return type you will never get
pub fn does_dir_contain<'a, T>(
    dir: &Path,
    operation: Operation,
    list: &'a [T],
) -> io::Result<OperationResult<'a>>
where
    T: std::borrow::Borrow<str> + std::cmp::Eq + std::hash::Hash,
{
    let entries = std::fs::read_dir(dir)?;
    let file_names = entries
        .filter_map(|entry| Some(entry.ok()?.file_name()))
        .collect::<Vec<_>>();
    let str_names = file_names
        .iter()
        .filter_map(|f| f.to_str())
        .collect::<HashSet<_>>();

    match operation {
        Operation::All => Ok(OperationResult::Bool({
            let result = list
                .iter()
                .all(|check_file| str_names.contains(check_file.borrow()));
            result
        })),
        Operation::Any => Ok(OperationResult::Bool({
            let result = list
                .iter()
                .any(|check_file| str_names.contains(check_file.borrow()));
            result
        })),
        Operation::Count => Ok(OperationResult::Count({
            let collection = list
                .iter()
                .filter(|&check_file| str_names.contains(check_file.borrow()))
                .map(|t| t.borrow())
                .collect::<HashSet<_>>();
            let num_found = collection.len();
            (num_found, collection)
        })),
    }
}

pub fn await_user_for_end() {
    println!("Press enter to exit...");
    let stdin = io::stdin();
    let _ = stdin.read_line(&mut String::new());
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

pub async fn build_favorites(curr_dir: &Path, args: Cli) -> io::Result<()> {
    let mut ip_collected = 0;
    let mut ips = String::new();
    let mut favorites_json = File::create(curr_dir.join(format!("{FAVORITES_LOC}/{FAVORITES}")))?;
    let limit = args.limit.unwrap_or(DEFAULT_SERVER_CAP);

    if limit >= DEFAULT_SERVER_CAP {
        println!("NOTE: Currently the in game server browser breaks when you add more than 100 servers to favorites")
    }

    let mut servers = filter_server_list(&args)
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
    Ok(())
}

enum Task {
    Allowed(Vec<ServerInfo>),
    Filtered,
    #[allow(dead_code)]
    Error(io::Error),
}

fn lowercase_vec(vec: &[String]) -> Vec<String> {
    vec.iter().map(|s| s.trim().to_lowercase()).collect()
}

fn parse_hostname(name: &str) -> String {
    const COLOR_ESCAPE_CODE: char = '^';
    let mut host_name = String::new();
    let mut chars = name.chars().peekable();
    while let Some(c) = chars.next() {
        if c == COLOR_ESCAPE_CODE {
            if chars.peek().is_some() {
                chars.next();
            }
        } else {
            host_name.push(c.to_ascii_lowercase());
        }
    }
    host_name
}

async fn filter_server_list(args: &Cli) -> reqwest::Result<Vec<ServerInfo>> {
    let instance_url = format!("{MASTER_URL}{JSON_SERVER_ENDPOINT}");
    let mut host_list = reqwest::get(instance_url.as_str())
        .await?
        .json::<Vec<HostData>>()
        .await?;

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
            "Determining region of {} server hosters...",
            host_list.len()
        );
        let mut failure_count = 0_usize;
        let mut server_list = Vec::new();

        let client = reqwest::Client::builder()
            .timeout(Duration::from_secs(20))
            .build()?;

        let tasks = host_list
            .into_iter()
            .map(|host| {
                let client = client.clone();
                tokio::spawn(async move {
                    let location = match try_location_lookup(&host, client).await {
                        Ok(loc) => loc,
                        Err(err) => return Task::Error(err),
                    };
                    match region {
                        Region::NA if location.code != CODE_NA => Task::Filtered,
                        Region::EU if location.code != CODE_EU => Task::Filtered,
                        Region::Apac if !APAC_CONT_CODES.contains(location.code.as_str()) => {
                            Task::Filtered
                        }
                        _ => Task::Allowed(host.servers),
                    }
                })
            })
            .collect::<Vec<_>>();

        for task in tasks {
            match task.await {
                Ok(result) => match result {
                    Task::Allowed(mut servers) => server_list.append(&mut servers),
                    Task::Filtered => (),
                    Task::Error(_) => failure_count += 1,
                },
                Err(_) => failure_count += 1,
            }
        }

        if failure_count > 0 {
            eprintln!("Failed to resolve location for {failure_count} server hoster(s)")
        }

        return Ok(server_list);
    }
    Ok(host_list.drain(..).flat_map(|host| host.servers).collect())
}

async fn try_location_lookup(host: &HostData, client: reqwest::Client) -> io::Result<Continent> {
    let format_url =
        |ip: IpAddr| -> String { format!("{MASTER_LOCATION_URL}{ip}{LOCATION_PRIVATE_KEY}") };
    let location_api_url = match resolve_address(&host.ip_address) {
        Ok(ip) => format_url(ip),
        Err(_) => {
            const HTTP_ENDING: &str = "//";
            if let Some(i) = host.webfront_url.find(HTTP_ENDING) {
                const PORT_SEPERATOR: char = ':';
                let ip_start = i + HTTP_ENDING.len();
                let ipv6_slice =
                    if let Some(j) = host.webfront_url[ip_start..].rfind(PORT_SEPERATOR) {
                        &host.webfront_url[ip_start..j + ip_start]
                    } else {
                        &host.webfront_url[ip_start..]
                    };
                match resolve_address(ipv6_slice) {
                    Ok(ip) => format_url(ip),
                    Err(_) => format_url(resolve_address(&host.servers[0].ip)?),
                }
            } else {
                format_url(resolve_address(&host.servers[0].ip)?)
            }
        }
    };

    let api_response = client
        .get(location_api_url.as_str())
        .send()
        .await
        .map_err(|err| io::Error::other(format!("{err:?}, outbound url: {location_api_url}")))?;

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
        Err(err) => Err(io::Error::other(format!("{err:?}"))),
    }
}

fn resolve_address(input: &str) -> io::Result<IpAddr> {
    let ip_trim = input.trim_matches('/').trim_matches(':');
    if ip_trim.is_empty() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "Ip can not be empty",
        ));
    }
    if let Ok(ip) = ip_trim.parse::<IpAddr>() {
        if ip.is_unspecified() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!("Addr: {ip}, is not valid"),
            ));
        }
        return Ok(ip);
    }

    let addr = (ip_trim, 80)
        .to_socket_addrs()?
        .next()
        .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "Hostname could not be resolved"))?;

    Ok(addr.ip())
}
