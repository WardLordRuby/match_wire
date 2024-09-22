use crate::{
    cli::Source,
    commands::{
        filter::{
            insert_hmw_servers, insert_iw4_servers, queue_info_requests, try_get_info, Server,
        },
        handler::{CommandContext, Message},
        launch_h2m::HostName,
        reconnect::HISTORY_MAX,
    },
    does_dir_contain, new_io_error,
    utils::json_data::{CacheFile, ServerCache},
    Operation, OperationResult, CACHED_DATA, LOG_ONLY,
};
use std::{
    collections::HashMap,
    fmt::Display,
    io::{self, ErrorKind},
    net::{IpAddr, SocketAddr},
    path::Path,
    time::{Duration, SystemTime},
};

use tracing::{error, instrument, trace};

pub struct Cache {
    pub host_to_connect: HashMap<String, SocketAddr>,
    pub ip_to_region: HashMap<IpAddr, [char; 2]>,
    pub connection_history: Vec<HostName>,
    pub iw4m: HashMap<IpAddr, Vec<u16>>,
    pub hmw: HashMap<IpAddr, Vec<u16>>,
    pub created: SystemTime,
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

    pub async fn from(value: CacheFile) -> Self {
        let mut servers = value.cache.iw4m.clone();
        let mut len = 0_usize;
        value.cache.hmw.iter().for_each(|(ip, insert)| {
            servers
                .entry(*ip)
                .and_modify(|ports| {
                    for port in insert.iter() {
                        if !ports.contains(port) {
                            ports.push(*port);
                        }
                    }
                    len += ports.len();
                })
                .or_insert({
                    len += insert.len();
                    insert.clone()
                });
        });
        let mut tasks = Vec::with_capacity(len);
        let client = reqwest::Client::new();
        for (ip, ports) in servers {
            for port in ports {
                len += 1;
                let client = client.clone();
                tasks.push(tokio::spawn(async move {
                    try_get_info(ip, port, None, client).await
                }));
            }
        }
        let mut host_to_connect = HashMap::with_capacity(len);
        for task in tasks {
            match task.await {
                Ok(result) => match result {
                    Ok(server) => {
                        let id = server.socket_addr();
                        host_to_connect.insert(server.info.host_name, id);
                    }
                    Err(info) => error!("{}", info.err),
                },
                Err(err) => error!("{err}"),
            }
        }
        Cache {
            host_to_connect,
            ip_to_region: value.cache.regions,
            connection_history: value.connection_history,
            iw4m: value.cache.iw4m,
            hmw: value.cache.hmw,
            created: value.created,
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

    pub fn update_cache_with(
        &mut self,
        server: &Server,
        region: Option<[char; 2]>,
        source: Option<&Source>,
    ) {
        self.host_to_connect
            .insert(server.info.host_name.clone(), server.socket_addr());
        if let Some(region) = region {
            self.ip_to_region.insert(server.ip, region);
        }
        if let Some(&source) = source {
            self.insert_ports(server.ip, &[server.port], source);
        }
    }

    pub fn push(&mut self, server: Server, region: Option<[char; 2]>, source: Option<&Source>) {
        let id = server.socket_addr();
        self.host_to_connect.insert(server.info.host_name, id);
        if let Some(region) = region {
            self.ip_to_region.insert(server.ip, region);
        }
        if let Some(&source) = source {
            self.insert_ports(server.ip, &[server.port], source);
        }
    }
}

#[instrument(level = "trace", skip_all)]
pub async fn build_cache(
    connection_history: Option<&[HostName]>,
    regions: Option<&HashMap<IpAddr, [char; 2]>>,
) -> reqwest::Result<CacheFile> {
    let mut cache = Cache::new();
    let client = reqwest::Client::new();
    let mut iw4_tasks = Vec::new();
    let mut hmw_tasks = Vec::new();

    let mut iw4_servers = HashMap::new();
    let mut hmw_servers = HashMap::new();
    insert_iw4_servers(None, &mut iw4_servers).await;
    insert_hmw_servers(None, &mut hmw_servers).await;
    queue_info_requests(iw4_servers, &mut iw4_tasks, &client).await;
    queue_info_requests(hmw_servers, &mut hmw_tasks, &client).await;

    for task in iw4_tasks {
        match task.await {
            Ok(result) => match result {
                Ok(server) => {
                    let region = regions.and_then(|cache| cache.get(&server.ip).cloned());
                    cache.push(server, region, Some(&Source::Iw4Master))
                }
                Err(info) => {
                    error!(name: LOG_ONLY, "{}", info.err);
                    if let Some(data) = info.meta {
                        let ip = data
                            .server_resolved_addr
                            .expect("is always some when returned as an error");
                        cache
                            .host_to_connect
                            .insert(data.server.host_name, SocketAddr::new(ip, data.server.port));
                        cache.insert_ports(ip, &[data.server.port], Source::Iw4Master);
                    }
                }
            },
            Err(err) => error!(name: LOG_ONLY, "{err}"),
        }
    }

    for task in hmw_tasks {
        match task.await {
            Ok(result) => match result {
                Ok(server) => {
                    let region = regions.and_then(|cache| cache.get(&server.ip).cloned());
                    cache.push(server, region, Some(&Source::HmwMaster));
                }
                Err(info) => error!(name: LOG_ONLY, "{}", info.err),
            },
            Err(err) => error!(name: LOG_ONLY, "{err}"),
        }
    }

    println!("Updating server location cache...");

    trace!("Fetched regions for all H2M servers");
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
    pub err: io::Error,
    pub connection_history: Option<Vec<HostName>>,
    pub region_cache: Option<HashMap<IpAddr, [char; 2]>>,
}

impl ReadCacheErr {
    fn new(kind: ErrorKind, err: String) -> Self {
        ReadCacheErr {
            err: io::Error::new(kind, err),
            connection_history: None,
            region_cache: None,
        }
    }

    fn with_old<E>(
        kind: ErrorKind,
        err: E,
        history: Vec<HostName>,
        region: HashMap<IpAddr, [char; 2]>,
    ) -> Self
    where
        E: Into<Box<dyn std::error::Error + Send + Sync>>,
    {
        ReadCacheErr {
            err: io::Error::new(kind, err),
            connection_history: Some(history),
            region_cache: Some(region),
        }
    }
}

impl From<io::Error> for ReadCacheErr {
    fn from(value: io::Error) -> Self {
        ReadCacheErr {
            err: value,
            connection_history: None,
            region_cache: None,
        }
    }
}

impl From<serde_json::Error> for ReadCacheErr {
    fn from(value: serde_json::Error) -> Self {
        ReadCacheErr {
            err: io::Error::other(value.to_string()),
            connection_history: None,
            region_cache: None,
        }
    }
}

impl Display for ReadCacheErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", self.err)
    }
}

#[instrument(level = "trace", skip_all)]
pub async fn read_cache(local_env_dir: &Path) -> Result<Cache, ReadCacheErr> {
    match does_dir_contain(local_env_dir, Operation::All, &[CACHED_DATA]) {
        Ok(OperationResult::Bool(true)) => {
            let file = std::fs::File::open(local_env_dir.join(CACHED_DATA))?;
            let reader = io::BufReader::new(file);
            let data = serde_json::from_reader::<_, CacheFile>(reader)?;
            if data.version != env!("CARGO_PKG_VERSION") {
                return Err(ReadCacheErr::new(
                    io::ErrorKind::InvalidData,
                    "version mismatch".to_string(),
                ));
            }
            let curr_time = std::time::SystemTime::now();
            match curr_time.duration_since(data.created) {
                Ok(time) if time > Duration::new(60 * 60 * 24, 0) => {
                    return Err(ReadCacheErr::with_old(
                        ErrorKind::InvalidData,
                        "cache is too old",
                        data.connection_history,
                        data.cache.regions,
                    ))
                }
                Err(err) => {
                    return Err(ReadCacheErr::with_old(
                        ErrorKind::Other,
                        err,
                        data.connection_history,
                        data.cache.regions,
                    ))
                }
                _ => (),
            }
            trace!("Cache read from file");
            Ok(Cache::from(data).await)
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
pub async fn update_cache<'a>(context: &CommandContext) -> io::Result<()> {
    let local_env_dir = context.local_dir();
    let Some(ref local_path) = local_env_dir else {
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
            connection_history: if cache.connection_history.len() > HISTORY_MAX as usize {
                cache.connection_history[cache.connection_history.len() - HISTORY_MAX as usize..]
                    .to_vec()
            } else {
                cache.connection_history.clone()
            },
        }
    };
    serde_json::to_writer_pretty(file, &data).map_err(io::Error::other)?;
    context
        .msg_sender()
        .send(Message::Info(String::from("Cache updated locally")))
        .await
        .unwrap_or_else(|err| error!("{err}"));
    Ok(())
}
