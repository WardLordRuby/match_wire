pub mod cli;
pub mod json_data;
pub mod not_your_private_keys;
pub mod subscriber;
pub mod commands {
    pub mod filter;
}

use commands::filter::build_cache;
use json_data::{CacheFile, ServerCache, ServerInfo, Version};
use std::{
    collections::{HashMap, HashSet},
    io,
    path::{Path, PathBuf},
    time::Duration,
};
use tracing::{error, info, instrument};

pub const VERSION_URL: &str =
    "https://gist.githubusercontent.com/WardLordRuby/324d7c1fb454aed5f5155a790bd028f0/raw/";

pub const H2M_MAX_CLIENT_NUM: i64 = 18;
pub const H2M_MAX_TEAM_SIZE: i64 = 9;

pub const REQUIRED_FILES: [&str; 3] = ["h1_mp64_ship.exe", "h2m-mod", "players2"];

const LOCAL_DATA: &str = "LOCALAPPDATA";
const CACHED_DATA: &str = "region_cache.json";

pub const APP_NAME: &str = "h2m_favorites";
pub const LOG_NAME: &str = "h2m_favorties.log";

pub struct Cache {
    pub host_to_connect: HashMap<String, String>,
    pub ip_to_region: HashMap<String, String>,
    pub servers: Vec<ServerCache>,
    pub created: std::time::SystemTime,
}

impl Cache {
    fn from(servers: Vec<ServerCache>, created: std::time::SystemTime) -> Self {
        let len = servers.len();
        let (host_to_connect, ip_to_region) = servers.iter().fold(
            (HashMap::with_capacity(len), HashMap::new()),
            |(mut host_map, mut ip_map), server| {
                host_map.insert(server.hostname.clone(), server.get_id());
                ip_map.insert(server.ip.clone(), server.region.clone());
                (host_map, ip_map)
            },
        );
        Cache {
            host_to_connect,
            ip_to_region,
            servers,
            created,
        }
    }

    fn update_cache_with(&mut self, server: &ServerInfo, region: String) {
        self.host_to_connect
            .insert(server.hostname.clone(), server.get_id());
        self.ip_to_region.insert(server.ip.clone(), region.clone());
        self.servers.push(ServerCache::from(server, region))
    }

    fn update_cache_with_consume(&mut self, server: ServerCache) {
        self.host_to_connect
            .insert(server.hostname.clone(), server.get_id());
        self.ip_to_region
            .insert(server.ip.clone(), server.region.clone());
        self.servers.push(server)
    }
}

impl ServerCache {
    fn from(value: &ServerInfo, region: String) -> Self {
        ServerCache {
            hostname: value.hostname.clone(),
            ip: value.ip.clone(),
            port: value.port,
            region,
        }
    }

    fn get_id(&self) -> String {
        format!("{}:{}", self.ip, self.port)
    }
}

impl ServerInfo {
    fn get_id(&self) -> String {
        format!("{}:{}", self.ip, self.port)
    }
}

// MARK: TODOS
// 1. need to rework the program to stay open so we can store a map in memory
// 2. grab tracing and set up tracing as well as our map save to file - DONE
// 3. create map of host-name -> (ip:port, region) - DONE
//    - need a map when user filters favorites
//    - need a map when starts the app
// 4. app will have additional command so all current args need to get moved into a new filter command
// 5. app needs to display a '>' when waiting for a user to enter the next command
// 6. app needs to be able to interact with h2m-mod.exe or h2m-revivied.exe
//    - listen to the stdout
//    - inject a connect command if it is not busy

#[macro_export]
macro_rules! new_io_error {
    ($kind:expr, $msg:expr) => {
        Err(io::Error::new($kind, $msg))
    };
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

/// Validates local/app_dir exists and modifies input if valid
pub fn check_app_dir_exists(local: &mut PathBuf) -> io::Result<()> {
    use crate::{does_dir_contain, Operation, OperationResult, APP_NAME};
    match does_dir_contain(local, Operation::All, &[APP_NAME]) {
        Ok(OperationResult::Bool(true)) => {
            local.push(APP_NAME);
            Ok(())
        }
        Ok(OperationResult::Bool(false)) => {
            local.push(APP_NAME);
            std::fs::create_dir(local)
        }
        Err(err) => Err(err),
        _ => unreachable!(),
    }
}

pub fn format_panic_info(info: &std::panic::PanicInfo) -> String {
    let payload_str = if let Some(location) = info.location() {
        format!(
            "PANIC {}:{}:{}:",
            location.file(),
            location.line(),
            location.column(),
        )
    } else {
        String::from("PANIC:")
    };
    if let Some(msg) = info.payload().downcast_ref::<&str>() {
        format!("{payload_str} {msg}")
    } else if let Some(msg) = info.payload().downcast_ref::<String>() {
        format!("{payload_str} {msg}")
    } else {
        format!("{payload_str} no attached message")
    }
}

pub fn await_user_for_end() {
    println!("Press enter to exit...");
    let stdin = io::stdin();
    let _ = stdin.read_line(&mut String::new());
}

pub fn lowercase_vec(vec: &[String]) -> Vec<String> {
    vec.iter().map(|s| s.trim().to_lowercase()).collect()
}

pub fn parse_hostname(name: &str) -> String {
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

#[instrument(skip_all)]
pub async fn app_startup() -> io::Result<(Cache, Option<PathBuf>)> {
    let mut local_env_dir = None;
    if let Some(path) = std::env::var_os(LOCAL_DATA) {
        let mut dir = PathBuf::from(path);

        if let Err(err) = check_app_dir_exists(&mut dir) {
            error!("{err:?}");
        } else {
            subscriber::init_subscriber(&dir).unwrap_or_else(|err| eprintln!("{err}"));
            local_env_dir = Some(dir);
            match read_map(local_env_dir.as_ref().unwrap()) {
                Ok(cache) => return Ok((cache, local_env_dir)),
                Err(err) => info!("{err}"),
            }
        }
    } else {
        error!("Could not find %appdata%/local");
        if cfg!(debug_assertions) {
            subscriber::init_subscriber(Path::new("")).unwrap();
        }
    }
    let server_cache = build_cache().await.map_err(io::Error::other)?;
    if let Some(ref dir) = local_env_dir {
        match std::fs::File::create(dir.join(CACHED_DATA)) {
            Ok(file) => {
                let data = CacheFile {
                    version: env!("CARGO_PKG_VERSION").to_string(),
                    created: std::time::SystemTime::now(),
                    cache: server_cache,
                };
                if let Err(err) = serde_json::to_writer_pretty(file, &data) {
                    error!("{err}")
                }
                return Ok((Cache::from(data.cache, data.created), local_env_dir));
            }
            Err(err) => error!("{err}"),
        }
    }
    Ok((
        Cache::from(server_cache, std::time::SystemTime::now()),
        local_env_dir,
    ))
}

fn read_map(local_env_dir: &Path) -> io::Result<Cache> {
    match does_dir_contain(local_env_dir, Operation::All, &[CACHED_DATA]) {
        Ok(OperationResult::Bool(true)) => {
            let file = std::fs::File::open(local_env_dir.join(CACHED_DATA))?;
            let reader = io::BufReader::new(file);
            let data = serde_json::from_reader::<_, CacheFile>(reader)?;
            if data.version != env!("CARGO_PKG_VERSION") {
                return new_io_error!(io::ErrorKind::InvalidData, "version mismatch");
            }
            let curr_time = std::time::SystemTime::now();
            match curr_time.duration_since(data.created) {
                Ok(time) if time > Duration::new(60 * 60 * 24, 0) => {
                    return new_io_error!(io::ErrorKind::InvalidData, "cache is too old")
                }
                Err(err) => return new_io_error!(io::ErrorKind::Other, err),
                _ => (),
            }
            Ok(Cache::from(data.cache, data.created))
        }
        Ok(OperationResult::Bool(false)) => {
            new_io_error!(io::ErrorKind::NotFound, format!("{CACHED_DATA} not found"))
        }
        Err(err) => Err(err),
        _ => unreachable!(),
    }
}

pub fn update_cache(server_cache: Cache, local_env_dir: &Path) -> io::Result<()> {
    let file = std::fs::File::create(local_env_dir.join(CACHED_DATA))?;
    let data = CacheFile {
        version: env!("CARGO_PKG_VERSION").to_string(),
        created: server_cache.created,
        cache: server_cache.servers,
    };
    serde_json::to_writer_pretty(file, &data).map_err(io::Error::other)
}
