use crate::{
    commands::{
        filter::{get_server_master, resolve_address, try_location_lookup, GAME_ID, IP},
        handler::CommandContext,
        launch_h2m::HostName,
        reconnect::HISTORY_MAX,
    },
    does_dir_contain, new_io_error,
    utils::{
        input::line::LineReader,
        json_data::{CacheFile, ServerCache, ServerInfo},
    },
    Operation, OperationResult, CACHED_DATA, LOG_ONLY,
};
use std::{
    collections::HashMap,
    fmt::Display,
    io::{self, ErrorKind},
    path::Path,
    time::{Duration, SystemTime},
};

use tracing::{error, info, instrument, trace};

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

pub struct ReadCache {
    pub cache: Cache,
    pub connection_history: Vec<HostName>,
}

impl From<CacheFile> for ReadCache {
    fn from(value: CacheFile) -> Self {
        ReadCache {
            cache: Cache::from(value.cache, value.created),
            connection_history: value.connection_history,
        }
    }
}

pub struct ReadCacheErr {
    pub err: io::Error,
    pub connection_history: Option<Vec<HostName>>,
}

impl ReadCacheErr {
    fn new(kind: ErrorKind, err: String) -> Self {
        ReadCacheErr {
            err: io::Error::new(kind, err),
            connection_history: None,
        }
    }

    fn with_history<E>(kind: ErrorKind, err: E, history: Vec<HostName>) -> Self
    where
        E: Into<Box<dyn std::error::Error + Send + Sync>>,
    {
        ReadCacheErr {
            err: io::Error::new(kind, err),
            connection_history: Some(history),
        }
    }
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
                    return Err(ReadCacheErr::with_history(
                        ErrorKind::InvalidData,
                        "cache is too old",
                        data.connection_history,
                    ))
                }
                Err(err) => {
                    return Err(ReadCacheErr::with_history(
                        ErrorKind::Other,
                        err,
                        data.connection_history,
                    ))
                }
                _ => (),
            }
            trace!("Cache read from file");
            Ok(ReadCache::from(data))
        }
        Ok(OperationResult::Bool(false)) => Err(ReadCacheErr::new(
            ErrorKind::NotFound,
            format!("{CACHED_DATA} not found"),
        )),
        Err(err) => Err(err.into()),
        _ => unreachable!(),
    }
}

#[instrument(level = "trace", skip_all)]
pub async fn update_cache<'a>(
    context: &CommandContext,
    line_handle: &mut LineReader<'a>,
) -> io::Result<()> {
    let local_env_dir = context.local_dir();
    let Some(ref local_path) = local_env_dir else {
        return new_io_error!(io::ErrorKind::Other, "No valid location to save cache to");
    };
    let file = std::fs::File::create(local_path.join(CACHED_DATA))?;
    let data = {
        let cache_lock = context.cache();
        let history_lock = context.h2m_server_connection_history();
        let cache = cache_lock.lock().await;
        let connection_history = history_lock.lock().await;
        CacheFile {
            version: env!("CARGO_PKG_VERSION").to_string(),
            created: cache.created,
            cache: cache.servers.clone(),
            connection_history: if connection_history.len() > HISTORY_MAX as usize {
                connection_history[connection_history.len() - HISTORY_MAX as usize..].to_vec()
            } else {
                connection_history.clone()
            },
        }
    };
    serde_json::to_writer_pretty(file, &data).map_err(io::Error::other)?;
    // MARK: FIXME
    // find better way to get background tasks to print on their own line
    // passing in the linehandle just for this is sad
    line_handle
        .move_to_beginning(line_handle.line_len())
        .unwrap();
    info!("Cache updated locally");
    Ok(())
}
