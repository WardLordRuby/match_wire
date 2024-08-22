pub mod cli;
pub mod not_your_private_keys;
pub mod server_data;

use cli::{Filters, Region};
use futures::stream::{FuturesUnordered, StreamExt};
use not_your_private_keys::LOCATION_PRIVATE_KEY;
use server_data::*;
use std::{collections::HashSet, error::Error, fs::File, io::Write, path::Path, sync::LazyLock};

pub const DEFAULT_MAX_PING: u128 = 200;
pub const DEFAULT_MIN_PLAYERS: u16 = 0;
pub const DEFAULT_SERVER_CAP: usize = 100;

pub const REQUIRED_FILES: [&str; 3] = ["h1_mp64_ship.exe", "h2m-mod", "players2"];

const MASTER_LOCATION_URL: &str = "https://api.findip.net/";

const MASTER_URL: &str = "https://master.iw4.zip/";
const FAVORITES_LOC: &str = "players2";
const FAVORITES: &str = "favourites.json";

static APAC_CONT_CODES: LazyLock<HashSet<&str>> = LazyLock::new(populate_apac_cont_codes);

fn populate_apac_cont_codes() -> HashSet<&'static str> {
    const APAC_CONT_CODES_ARR: [&str; 3] = ["AS", "OC", "AF"];
    HashSet::from(APAC_CONT_CODES_ARR)
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
) -> std::io::Result<OperationResult<'a>>
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
    println!("Press any key to exit...");
    let stdin = std::io::stdin();
    let _ = stdin.read_line(&mut String::new());
}

fn serialize_json(into: &mut std::fs::File, from: String) -> std::io::Result<()> {
    const COMMA: char = ',';
    let ips = if from.ends_with(COMMA) {
        &from[..from.len() - COMMA.len_utf8()]
    } else {
        from.as_str()
    };
    write!(into, "[{ips}]")
}

pub async fn build_favorites(curr_dir: &Path, args: Filters) -> Result<(), Box<dyn Error>> {
    let mut ip_collected = 0;
    let mut ips = String::new();
    let mut favorites_json = File::create(curr_dir.join(format!("{FAVORITES_LOC}/{FAVORITES}")))?;

    if args.limit >= DEFAULT_SERVER_CAP {
        println!("NOTE: Currently the in game server browser breaks when you add more than 100 servers to favorites")
    }

    let mut servers = filter_server_list(&args).await?;

    println!(
        "{} servers match the prameters in the current query",
        servers.len()
    );

    if servers.len() > args.limit {
        servers.sort_unstable_by_key(|server| server.clientnum);
    }

    for server in servers.iter().rev() {
        ips.push_str(&format!("\"{}:{}\",", server.ip, server.port));
        ip_collected += 1;
        if ip_collected == args.limit {
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
    Error(Box<dyn Error>),
}

async fn filter_server_list(args: &Filters) -> Result<Vec<ServerInfo>, Box<dyn Error>> {
    let instance_url = format!("{MASTER_URL}instance");
    let mut host_list = reqwest::get(instance_url.as_str())
        .await?
        .json::<Vec<HostData>>()
        .await?;

    let include = args.includes.as_ref().map(|s| s.trim().to_lowercase());
    let exclude = args.excludes.as_ref().map(|s| s.trim().to_lowercase());

    for i in (0..host_list.len()).rev() {
        for j in (0..host_list[i].servers.len()).rev() {
            if host_list[i].servers[j].game != "H2M" {
                host_list[i].servers.swap_remove(j);
                continue;
            }

            let mut hostname_l = None;
            if let Some(ref string) = include {
                hostname_l = Some(host_list[i].servers[j].hostname.to_lowercase());
                if !hostname_l.as_ref().unwrap().contains(string) {
                    host_list[i].servers.swap_remove(j);
                    continue;
                }
            }
            if let Some(ref string) = exclude {
                if hostname_l
                    .unwrap_or_else(|| host_list[i].servers[j].hostname.to_lowercase())
                    .contains(string)
                {
                    host_list[i].servers.swap_remove(j);
                    continue;
                }
            }

            if host_list[i].servers[j].clientnum < args.player_min {
                host_list[i].servers.swap_remove(j);
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

        let mut tasks = FuturesUnordered::new();

        for host in host_list {
            let task = async move {
                let location = match try_location_lookup(&host).await {
                    Ok(loc) => loc,
                    Err(err) => return Task::Error(err),
                };
                match region {
                    Region::NA if location.code != "NA" => return Task::Filtered,
                    Region::EU if location.code != "EU" => return Task::Filtered,
                    Region::Apac if !APAC_CONT_CODES.contains(location.code.as_str()) => {
                        return Task::Filtered
                    }
                    _ => (),
                }
                Task::Allowed(host.servers)
            };
            tasks.push(task);
        }

        while let Some(result) = tasks.next().await {
            match result {
                Task::Allowed(mut servers) => server_list.append(&mut servers),
                Task::Filtered => (),
                Task::Error(_) => failure_count += 1,
            }
        }

        if failure_count > 0 {
            println!("Failed to resolve location for {failure_count} server hoster(s)")
        }

        return Ok(server_list);
    }
    Ok(host_list.drain(..).flat_map(|host| host.servers).collect())
}

async fn try_location_lookup(host: &HostData) -> Result<Continent, Box<dyn Error>> {
    let location_api_url = format!(
        "{MASTER_LOCATION_URL}{}{LOCATION_PRIVATE_KEY}",
        host.ip_address
    );

    let mut recovery_ip = false;
    let mut get_backup_url = || -> Option<String> {
        if recovery_ip {
            return None;
        }
        recovery_ip = true;
        Some(format!(
            "{MASTER_LOCATION_URL}{}{LOCATION_PRIVATE_KEY}",
            host.servers[0].ip
        ))
    };

    let api_attempt = match reqwest::get(location_api_url.as_str()).await {
        Ok(response) => response,
        Err(_) => reqwest::get(get_backup_url().unwrap()).await?,
    };

    match api_attempt.json::<ServerLocation>().await {
        Ok(json) => {
            if let Some(code) = json.continent {
                return Ok(code);
            }
            if let Some(backup_ip) = get_backup_url() {
                return process_secondary_ip_location(backup_ip).await;
            }
            Err(json_msg_err(json.message))
        }
        Err(err) => {
            if let Some(backup_ip) = get_backup_url() {
                return process_secondary_ip_location(backup_ip).await;
            }
            Err(Box::new(err))
        }
    }
}

async fn process_secondary_ip_location(ip: String) -> Result<Continent, Box<dyn Error>> {
    let backup_json = reqwest::get(ip).await?.json::<ServerLocation>().await?;
    if let Some(code) = backup_json.continent {
        return Ok(code);
    }
    Err(json_msg_err(backup_json.message))
}

fn json_msg_err(msg: Option<String>) -> Box<dyn Error> {
    Box::new(std::io::Error::other(
        msg.unwrap_or_else(|| String::from("unknown error")),
    ))
}
