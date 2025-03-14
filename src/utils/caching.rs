use crate::{
    cli::Source,
    commands::{
        filter::{get_sourced_servers, queue_info_requests, Server, Sourced, DEFUALT_SOURCES},
        handler::CommandContext,
        launch_h2m::HostName,
        reconnect::HISTORY_MAX,
    },
    does_dir_contain, new_io_error,
    utils::json_data::{CacheFile, ContCodeMap},
    Operation, OperationResult, Spinner, CACHED_DATA, LOG_ONLY, SPLASH_SCREEN_VIS,
};
use constcat::concat;
use std::{
    borrow::Cow,
    collections::HashMap,
    io,
    net::{IpAddr, SocketAddr},
    path::Path,
    sync::{atomic::Ordering, Arc},
    time::{Duration, SystemTime},
};
use tokio::sync::Mutex;
use tracing::{error, info, instrument, trace};

pub struct Cache {
    /// Key: host name with cod color codes
    pub host_to_connect: HashMap<String, SocketAddr>,
    pub ip_to_region: HashMap<IpAddr, [u8; 2]>,
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

impl Default for Cache {
    fn default() -> Self {
        Cache {
            host_to_connect: HashMap::new(),
            ip_to_region: HashMap::new(),
            connection_history: Vec::new(),
            iw4m: HashMap::new(),
            hmw: HashMap::new(),
            created: SystemTime::now(),
        }
    }
}

impl Cache {
    fn insert_ports(&mut self, ip: IpAddr, ports: &[u16], source: Source) {
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

    fn try_insert_region_and_ports(
        &mut self,
        addr: SocketAddr,
        region: Option<[u8; 2]>,
        valid_source: Option<Source>,
    ) {
        if let Some(region) = region {
            self.ip_to_region.insert(addr.ip(), region);
        }
        if let Some(source) = valid_source {
            self.insert_ports(addr.ip(), &[addr.port()], source);
        }
    }

    pub(crate) fn update_cache_with(&mut self, server: &Server, region: Option<[u8; 2]>) {
        let socket_addr = server.source.socket_addr();
        if let Some(ref info) = server.info {
            self.host_to_connect
                .insert(info.host_name.clone(), socket_addr);
        }
        self.try_insert_region_and_ports(socket_addr, region, server.source.to_valid_source());
    }

    pub(crate) fn push(&mut self, server: Server, region: Option<[u8; 2]>) {
        let socket_addr = server.source.socket_addr();
        if let Some(info) = server.info {
            self.host_to_connect.insert(info.host_name, socket_addr);
        }
        self.try_insert_region_and_ports(socket_addr, region, server.source.to_valid_source());
    }
}

#[instrument(level = "trace", skip_all)]
pub async fn build_cache(prev: Option<&Arc<Mutex<Cache>>>) -> Result<Cache, &'static str> {
    let spinner = if !SPLASH_SCREEN_VIS.load(Ordering::SeqCst) {
        Some(Spinner::new(String::from("Updating cache")))
    } else {
        info!("Updating cache...");
        None
    };

    let finish_spinner = || {
        if let Some(spinner) = spinner {
            spinner.finish();
        }
    };

    let servers = match get_sourced_servers(DEFUALT_SOURCES, prev).await {
        Ok(servers) => servers,
        Err(err) => {
            finish_spinner();
            return Err(err);
        }
    };

    let mut cache = Cache::default();

    let regions = match prev {
        Some(p) => {
            let mut lock = p.lock().await;
            std::mem::swap(&mut cache.connection_history, &mut lock.connection_history);
            Some(std::mem::take(&mut lock.ip_to_region))
        }
        None => None,
    };

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
                    let Sourced::Iw4(data) = err.meta else {
                        continue;
                    };
                    let Ok(ip) = data.server.ip.parse() else {
                        continue;
                    };
                    if let Some(source) = source {
                        cache.insert_ports(ip, &[data.server.port], source);
                    }
                    cache
                        .host_to_connect
                        .insert(data.server.host_name, SocketAddr::new(ip, data.server.port));
                }
            },
            Err(err) => error!(name: LOG_ONLY, "{err:?}"),
        }
    }

    finish_spinner();
    info!("Cache updated!");

    Ok(cache)
}

pub struct ReadCacheErr {
    pub err: Cow<'static, str>,
    pub cache: Option<CacheFile>,
}

impl ReadCacheErr {
    fn new<C: Into<Cow<'static, str>>>(err: C) -> Self {
        ReadCacheErr {
            err: err.into(),
            cache: None,
        }
    }

    fn with_old<C: Into<Cow<'static, str>>>(err: C, cache: CacheFile) -> Self {
        ReadCacheErr {
            err: err.into(),
            cache: Some(cache),
        }
    }
}

impl From<io::Error> for ReadCacheErr {
    fn from(value: io::Error) -> Self {
        ReadCacheErr {
            err: format!("{value}, Starting new cache file").into(),
            cache: None,
        }
    }
}

impl From<serde_json::Error> for ReadCacheErr {
    fn from(value: serde_json::Error) -> Self {
        ReadCacheErr {
            err: format!("{value}, Starting new cache file").into(),
            cache: None,
        }
    }
}

#[instrument(level = "trace", skip_all)]
pub fn read_cache(local_env_dir: &Path) -> Result<CacheFile, ReadCacheErr> {
    match does_dir_contain(local_env_dir, Operation::All, &[CACHED_DATA]) {
        Ok(OperationResult::Bool(true)) => {
            let file = std::fs::File::open(local_env_dir.join(CACHED_DATA))?;
            let reader = io::BufReader::new(file);
            let file_contents = serde_json::from_reader::<_, CacheFile>(reader)?;
            trace!("Cache read from file");
            let curr_time = std::time::SystemTime::now();
            match curr_time.duration_since(file_contents.created) {
                Ok(time) if time > Duration::new(60 * 60 * 24, 0) => {
                    Err(ReadCacheErr::with_old("cache is too old", file_contents))
                }
                Err(err) => Err(ReadCacheErr::with_old(err.to_string(), file_contents)),
                _ => Ok(file_contents),
            }
        }
        Ok(OperationResult::Bool(false)) => {
            Err(ReadCacheErr::new(concat!(CACHED_DATA, " not found")))
        }
        Err(err) => Err(err.into()),
        _ => unreachable!(),
    }
}

#[instrument(level = "trace", skip_all)]
pub async fn write_cache(context: &CommandContext, cmd_history: &[String]) -> io::Result<()> {
    let Some(local_path) = context.local_dir() else {
        return new_io_error!(io::ErrorKind::Other, "No valid location to save cache to");
    };
    let file = std::fs::File::create(local_path.join(CACHED_DATA))?;
    {
        let cache_lock = context.cache();
        let cache = cache_lock.lock().await;

        serde_json::to_writer_pretty(
            file,
            &serde_json::json!({
                "version": env!("CARGO_PKG_VERSION"),
                "created": cache.created,
                "connection_history": if cache.connection_history.len() > HISTORY_MAX {
                    &cache.connection_history[cache.connection_history.len() - HISTORY_MAX..]
                } else {
                    &cache.connection_history
                },
                "cmd_history": cmd_history,
                "cache": {
                    "iw4m": cache.iw4m,
                    "hmw": cache.hmw,
                    "regions": ContCodeMap(&cache.ip_to_region),
                    "host_names": cache.host_to_connect,
                },
            }),
        )
        .map_err(io::Error::other)?;
    }
    info!(name: LOG_ONLY, "Cache saved locally");
    Ok(())
}
