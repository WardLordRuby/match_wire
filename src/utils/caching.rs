use crate::{
    commands::filter::{get_server_master, resolve_address, try_location_lookup, GAME_ID, IP},
    does_dir_contain, new_io_error,
    utils::json_data::{CacheFile, ServerCache, ServerInfo},
    Operation, OperationResult, CACHED_DATA, LOG_ONLY,
};
use std::{
    collections::HashMap,
    io,
    path::{Path, PathBuf},
    sync::Arc,
    time::{Duration, SystemTime},
};
use tokio::sync::Mutex;
use tracing::{error, info, instrument, trace};

#[derive(Debug)]
pub struct Cache {
    pub host_to_connect: HashMap<String, String>,
    pub ip_to_region: HashMap<String, String>,
    pub servers: Vec<ServerCache>,
    pub created: SystemTime,
}

impl Default for Cache {
    fn default() -> Self {
        Cache {
            host_to_connect: HashMap::new(),
            ip_to_region: HashMap::new(),
            servers: Vec::new(),
            created: SystemTime::now(),
        }
    }
}

impl Cache {
    pub fn from(servers: Vec<ServerCache>, created: SystemTime) -> Self {
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

    pub fn update_cache_with(&mut self, server: &ServerInfo, region: String) {
        self.host_to_connect
            .insert(server.hostname.clone(), server.get_id());
        self.ip_to_region.insert(server.ip.clone(), region.clone());
        self.servers.push(ServerCache::from(server, region))
    }

    pub fn push(&mut self, server: ServerCache) {
        self.host_to_connect
            .insert(server.hostname.clone(), server.get_id());
        self.ip_to_region
            .insert(server.ip.clone(), server.region.clone());
        self.servers.push(server)
    }
}

impl ServerCache {
    pub fn from(value: &ServerInfo, region: String) -> Self {
        ServerCache {
            hostname: value.hostname.clone(),
            ip: value.ip.clone(),
            port: value.port,
            region,
        }
    }
    pub fn consume(value: ServerInfo, region: String) -> Self {
        ServerCache {
            hostname: value.hostname,
            ip: value.ip,
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

#[instrument(level = "trace", skip_all)]
pub async fn build_cache() -> reqwest::Result<Vec<ServerCache>> {
    let host_list = get_server_master().await?;
    let client = reqwest::Client::new();
    let mut tasks = Vec::new();

    println!("Updating server location cache...");

    for host in host_list {
        for mut server in host.servers {
            if server.game != GAME_ID {
                continue;
            }
            match resolve_address(&server.ip, &host.ip_address, &host.webfront_url) {
                IP::Unchanged => (),
                IP::Modified(ip) => server.ip = ip.to_string(),
                IP::Err(err) => {
                    error!(name: LOG_ONLY, "{err}");
                    continue;
                }
            }
            let client = client.clone();
            tasks.push(tokio::spawn(async move {
                let location = match try_location_lookup(&server, client).await {
                    Ok(loc) => loc,
                    Err(err) => return Err(err),
                };
                Ok(ServerCache {
                    hostname: server.hostname,
                    ip: server.ip,
                    port: server.port,
                    region: location.code,
                })
            }))
        }
    }

    let mut collection = Vec::new();
    for task in tasks {
        match task.await {
            Ok(result) => match result {
                Ok(cache) => collection.push(cache),
                Err(err) => error!(name: LOG_ONLY, "{err:?}"),
            },
            Err(err) => error!(name: LOG_ONLY, "{err:?}"),
        }
    }
    trace!("Fetched regions for all H2M servers");
    Ok(collection)
}

#[instrument(level = "trace", skip_all)]
pub fn read_cache(local_env_dir: &Path) -> io::Result<Cache> {
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
            trace!("Cache read from file");
            Ok(Cache::from(data.cache, data.created))
        }
        Ok(OperationResult::Bool(false)) => {
            new_io_error!(io::ErrorKind::NotFound, format!("{CACHED_DATA} not found"))
        }
        Err(err) => Err(err),
        _ => unreachable!(),
    }
}

#[instrument(skip_all)]
pub async fn update_cache(
    server_cache: Arc<Mutex<Cache>>,
    local_env_dir: Arc<Option<PathBuf>>,
) -> io::Result<()> {
    let Some(ref local_path) = *local_env_dir else {
        return new_io_error!(io::ErrorKind::Other, "No valid location to save cache to");
    };
    let cache = server_cache.lock().await;
    let file = std::fs::File::create(local_path.join(CACHED_DATA))?;
    let data = CacheFile {
        version: env!("CARGO_PKG_VERSION").to_string(),
        created: cache.created,
        cache: cache.servers.clone(),
    };
    drop(cache);
    serde_json::to_writer_pretty(file, &data).map_err(io::Error::other)?;
    info!("Cache updated locally");
    Ok(())
}
