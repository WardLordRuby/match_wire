use crate::{
    commands::{
        filter::{get_server_master, resolve_address, try_location_lookup, GAME_ID, IP},
        launch_h2m::HostName,
        reconnect::HISTORY_MAX,
    },
    does_dir_contain, new_io_error,
    utils::{
        input_line::LineReader,
        json_data::{CacheFile, ServerCache, ServerInfo},
    },
    Operation, OperationResult, CACHED_DATA, LOG_ONLY,
};
use std::{
    collections::HashMap,
    fmt::Display,
    io,
    path::{Path, PathBuf},
    sync::Arc,
    time::{Duration, SystemTime},
};
use tokio::sync::Mutex;
use tracing::{error, info, instrument, trace};

pub struct ReadCache {
    pub cache: Cache,
    pub connection_history: Vec<HostName>,
}

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
pub async fn build_cache(connection_history: Option<&[HostName]>) -> reqwest::Result<CacheFile> {
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

    let mut cache = Vec::new();
    for task in tasks {
        match task.await {
            Ok(result) => match result {
                Ok(server) => cache.push(server),
                Err(err) => error!(name: LOG_ONLY, "{err:?}"),
            },
            Err(err) => error!(name: LOG_ONLY, "{err:?}"),
        }
    }
    trace!("Fetched regions for all H2M servers");
    Ok(CacheFile {
        version: env!("CARGO_PKG_VERSION").to_string(),
        created: std::time::SystemTime::now(),
        connection_history: connection_history.map(|v| v.to_vec()).unwrap_or_default(),
        cache,
    })
}

pub struct ReadCacheErr {
    pub err: io::Error,
    pub connection_history: Option<Vec<HostName>>,
}

impl From<io::Error> for ReadCacheErr {
    fn from(value: io::Error) -> Self {
        ReadCacheErr {
            err: value,
            connection_history: None,
        }
    }
}

impl From<serde_json::Error> for ReadCacheErr {
    fn from(value: serde_json::Error) -> Self {
        ReadCacheErr {
            err: io::Error::other(value.to_string()),
            connection_history: None,
        }
    }
}

impl Display for ReadCacheErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", self.err)
    }
}

#[instrument(level = "trace", skip_all)]
pub fn read_cache(local_env_dir: &Path) -> Result<ReadCache, ReadCacheErr> {
    match does_dir_contain(local_env_dir, Operation::All, &[CACHED_DATA]) {
        Ok(OperationResult::Bool(true)) => {
            let file = std::fs::File::open(local_env_dir.join(CACHED_DATA))?;
            let reader = io::BufReader::new(file);
            let data = serde_json::from_reader::<_, CacheFile>(reader)?;
            if data.version != env!("CARGO_PKG_VERSION") {
                return Err(ReadCacheErr {
                    err: io::Error::new(io::ErrorKind::InvalidData, "version mismatch"),
                    connection_history: Some(data.connection_history),
                });
            }
            let curr_time = std::time::SystemTime::now();
            match curr_time.duration_since(data.created) {
                Ok(time) if time > Duration::new(60 * 60 * 24, 0) => {
                    return Err(ReadCacheErr {
                        err: io::Error::new(io::ErrorKind::InvalidData, "cache is too old"),
                        connection_history: Some(data.connection_history),
                    })
                }
                Err(err) => {
                    return Err(ReadCacheErr {
                        err: io::Error::other(err),
                        connection_history: Some(data.connection_history),
                    })
                }
                _ => (),
            }
            trace!("Cache read from file");
            Ok(ReadCache {
                cache: Cache::from(data.cache, data.created),
                connection_history: data.connection_history,
            })
        }
        Ok(OperationResult::Bool(false)) => {
            Err(io::Error::new(io::ErrorKind::NotFound, format!("{CACHED_DATA} not found")).into())
        }
        Err(err) => Err(err.into()),
        _ => unreachable!(),
    }
}

#[instrument(level = "trace", skip_all)]
pub async fn update_cache<'a>(
    connection_history: Arc<Mutex<Vec<HostName>>>,
    server_cache: Arc<Mutex<Cache>>,
    local_env_dir: Option<Arc<PathBuf>>,
    line_handle: &mut LineReader<'a>,
) -> io::Result<()> {
    let Some(ref local_path) = local_env_dir else {
        return new_io_error!(io::ErrorKind::Other, "No valid location to save cache to");
    };
    let file = std::fs::File::create(local_path.join(CACHED_DATA))?;
    let data = {
        let cache = server_cache.lock().await;
        let connection_history = connection_history.lock().await;
        CacheFile {
            version: env!("CARGO_PKG_VERSION").to_string(),
            created: cache.created,
            cache: cache.servers.clone(),
            connection_history: connection_history
                .iter()
                .rev()
                .take(HISTORY_MAX as usize)
                .cloned()
                .collect(),
        }
    };
    serde_json::to_writer_pretty(file, &data).map_err(io::Error::other)?;
    // MARK: FIXME
    // find better way to get background tasks to print on their own line
    // passing in the linehandle just for this is sad
    line_handle
        .move_to_beginning(line_handle.get_curr_len())
        .unwrap();
    info!("Cache updated locally");
    Ok(())
}