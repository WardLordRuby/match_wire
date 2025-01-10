use crate::{
    cli::Source,
    commands::{
        filter::{get_sourced_servers, queue_info_requests, Server, Sourced, DEFUALT_SOURCES},
        handler::CommandContext,
        launch_h2m::HostName,
        reconnect::HISTORY_MAX,
    },
    does_dir_contain, new_io_error,
    utils::{
        input::style::{GREEN, WHITE},
        json_data::{CacheFile, ServerCache},
    },
    Operation, OperationResult, CACHED_DATA, LOG_ONLY,
};
use std::{
    collections::HashMap,
    io,
    net::{IpAddr, SocketAddr},
    path::Path,
    time::{Duration, SystemTime},
};
use tracing::{error, info, instrument, trace};

pub struct Cache {
    /// Key: host name with cod color codes
    pub host_to_connect: HashMap<String, SocketAddr>,
    pub ip_to_region: HashMap<IpAddr, [char; 2]>,
    pub connection_history: Vec<HostName>,
    pub iw4m: HashMap<IpAddr, Vec<u16>>,
    pub hmw: HashMap<IpAddr, Vec<u16>>,
    pub created: SystemTime,
}

impl From<CacheFile> for Cache {
    fn from(value: CacheFile) -> Self {
        Cache {
            host_to_connect: value.cache.host_names,
            ip_to_region: value.cache.regions,
            connection_history: value.connection_history,
            iw4m: value.cache.iw4m,
            hmw: value.cache.hmw,
            created: value.created,
        }
    }
}

impl Cache {
    fn new() -> Self {
        Cache {
            host_to_connect: HashMap::new(),
            ip_to_region: HashMap::new(),
            connection_history: Vec::new(),
            iw4m: HashMap::new(),
            hmw: HashMap::new(),
            created: SystemTime::now(),
        }
    }

    pub fn insert_ports(&mut self, ip: IpAddr, ports: &[u16], source: Source) {
        let map = match source {
            Source::HmwMaster => &mut self.hmw,
            Source::Iw4Master => &mut self.iw4m,
        };
        map.entry(ip)
            .and_modify(|cached| {
                for port in ports {
                    if !cached.contains(port) {
                        cached.push(*port);
                    }
                }
            })
            .or_insert(ports.to_vec());
    }

    pub fn update_cache_with(&mut self, server: &Server, region: Option<[char; 2]>) {
        let socket_addr = server.source.socket_addr();
        if let Some(ref info) = server.info {
            self.host_to_connect
                .insert(info.host_name.clone(), socket_addr);
        }
        if let Some(region) = region {
            self.ip_to_region.insert(socket_addr.ip(), region);
        }
        if let Some(source) = server.source.to_valid_source() {
            self.insert_ports(socket_addr.ip(), &[socket_addr.port()], source);
        }
    }

    pub fn push(&mut self, server: Server, region: Option<[char; 2]>) {
        let socket_addr = server.source.socket_addr();
        if let Some(info) = server.info {
            self.host_to_connect.insert(info.host_name, socket_addr);
        }
        if let Some(region) = region {
            self.ip_to_region.insert(socket_addr.ip(), region);
        }
        if let Some(source) = server.source.to_valid_source() {
            self.insert_ports(socket_addr.ip(), &[socket_addr.port()], source);
        }
    }
}

impl CacheFile {
    fn from_backups(
        connection_history: Option<Vec<HostName>>,
        regions: Option<HashMap<IpAddr, [char; 2]>>,
    ) -> Self {
        CacheFile {
            version: env!("CARGO_PKG_VERSION").to_string(),
            created: std::time::SystemTime::now(),
            connection_history: connection_history.unwrap_or_default(),
            cache: ServerCache {
                iw4m: HashMap::new(),
                hmw: HashMap::new(),
                regions: regions.unwrap_or_default(),
                host_names: HashMap::new(),
            },
        }
    }
}

#[instrument(level = "trace", skip_all)]
pub async fn build_cache(
    connection_history: Option<&[HostName]>,
    regions: Option<&HashMap<IpAddr, [char; 2]>>,
) -> Result<CacheFile, (&'static str, CacheFile)> {
    println!("{GREEN}Updating cache...{WHITE}");

    let servers = get_sourced_servers(DEFUALT_SOURCES, None)
        .await
        .map_err(|err| {
            (
                err,
                CacheFile::from_backups(connection_history.map(|v| v.to_vec()), regions.cloned()),
            )
        })?;

    let mut cache = Cache::new();

    let client = reqwest::Client::builder()
        .timeout(tokio::time::Duration::from_secs(3))
        .build()
        .unwrap();

    let mut tasks = queue_info_requests(servers, false, &client).await;

    while let Some(res) = tasks.join_next().await {
        match res {
            Ok(result) => match result {
                Ok(server) => {
                    let region = regions
                        .as_ref()
                        .and_then(|cache| cache.get(&server.source.socket_addr().ip()).copied());
                    cache.push(server, region)
                }
                Err(mut err) => {
                    error!(name: LOG_ONLY, "{}", err.with_socket_addr().with_source());
                    let source = err.meta.to_valid_source();
                    if let Sourced::Iw4(data) = err.meta {
                        if let Ok(ip) = data.server.ip.parse() {
                            if let Some(source) = source {
                                cache.insert_ports(ip, &[data.server.port], source);
                            }
                            cache.host_to_connect.insert(
                                data.server.host_name,
                                SocketAddr::new(ip, data.server.port),
                            );
                        }
                    }
                }
            },
            Err(err) => error!(name: LOG_ONLY, "{err}"),
        }
    }

    Ok(CacheFile {
        version: env!("CARGO_PKG_VERSION").to_string(),
        created: std::time::SystemTime::now(),
        connection_history: connection_history.map(|v| v.to_vec()).unwrap_or_default(),
        cache: ServerCache {
            iw4m: cache.iw4m,
            hmw: cache.hmw,
            regions: cache.ip_to_region,
            host_names: cache.host_to_connect,
        },
    })
}

pub struct ReadCacheErr {
    pub err: String,
    pub connection_history: Option<Vec<HostName>>,
    pub region_cache: Option<HashMap<IpAddr, [char; 2]>>,
}

impl ReadCacheErr {
    fn new(err: String) -> Self {
        ReadCacheErr {
            err,
            connection_history: None,
            region_cache: None,
        }
    }

    fn with_old(err: String, history: Vec<HostName>, region: HashMap<IpAddr, [char; 2]>) -> Self {
        ReadCacheErr {
            err,
            connection_history: Some(history),
            region_cache: Some(region),
        }
    }
}

impl From<io::Error> for ReadCacheErr {
    fn from(value: io::Error) -> Self {
        ReadCacheErr {
            err: format!("{value}, Starting new cache file"),
            connection_history: None,
            region_cache: None,
        }
    }
}

impl From<serde_json::Error> for ReadCacheErr {
    fn from(value: serde_json::Error) -> Self {
        ReadCacheErr {
            err: format!("{value}, Starting new cache file"),
            connection_history: None,
            region_cache: None,
        }
    }
}

#[instrument(level = "trace", skip_all)]
pub async fn read_cache(local_env_dir: &Path) -> Result<Cache, ReadCacheErr> {
    match does_dir_contain(local_env_dir, Operation::All, &[CACHED_DATA]) {
        Ok(OperationResult::Bool(true)) => {
            let file = std::fs::File::open(local_env_dir.join(CACHED_DATA))?;
            let reader = io::BufReader::new(file);
            let data = serde_json::from_reader::<_, CacheFile>(reader)?;
            let curr_time = std::time::SystemTime::now();
            match curr_time.duration_since(data.created) {
                Ok(time) if time > Duration::new(60 * 60 * 24, 0) => {
                    return Err(ReadCacheErr::with_old(
                        "cache is too old".to_string(),
                        data.connection_history,
                        data.cache.regions,
                    ))
                }
                Err(err) => {
                    return Err(ReadCacheErr::with_old(
                        err.to_string(),
                        data.connection_history,
                        data.cache.regions,
                    ))
                }
                _ => (),
            }
            trace!("Cache read from file");
            Ok(Cache::from(data))
        }
        Ok(OperationResult::Bool(false)) => {
            Err(ReadCacheErr::new(format!("{CACHED_DATA} not found")))
        }
        Err(err) => Err(err.into()),
        _ => unreachable!(),
    }
}

#[instrument(level = "trace", skip_all)]
pub async fn write_cache<'a>(context: &CommandContext) -> io::Result<()> {
    let local_env_dir = context.local_dir();
    let Some(local_path) = local_env_dir else {
        return new_io_error!(io::ErrorKind::Other, "No valid location to save cache to");
    };
    let file = std::fs::File::create(local_path.join(CACHED_DATA))?;
    let data = {
        let cache_lock = context.cache();
        let cache = cache_lock.lock().await;
        CacheFile {
            version: env!("CARGO_PKG_VERSION").to_string(),
            created: cache.created,
            cache: ServerCache {
                iw4m: cache.iw4m.clone(),
                hmw: cache.hmw.clone(),
                regions: cache.ip_to_region.clone(),
                host_names: cache.host_to_connect.clone(),
            },
            connection_history: if cache.connection_history.len() > HISTORY_MAX {
                cache.connection_history[cache.connection_history.len() - HISTORY_MAX..].to_vec()
            } else {
                cache.connection_history.clone()
            },
        }
    };
    serde_json::to_writer_pretty(file, &data).map_err(io::Error::other)?;
    info!(name: LOG_ONLY, "Cache saved locally");
    Ok(())
}
