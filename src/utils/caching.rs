use crate::{
    client_with_timeout,
    commands::{
        filter::{get_sourced_servers, queue_info_requests, Server, Sourced, DEFAULT_SOURCES},
        handler::CommandContext,
        launch_h2m::HostName,
        reconnect::HISTORY_MAX,
    },
    does_dir_contain,
    models::{
        cli::Source,
        json_data::{CacheFile, ContCodeMap},
    },
    new_io_error, Operation, OperationResult, Spinner, CACHED_DATA, CRATE_VER, LOG_ONLY,
    SPLASH_SCREEN_VIS,
};

use std::{
    borrow::Cow,
    collections::HashMap,
    io,
    net::{IpAddr, SocketAddr},
    path::Path,
    sync::{atomic::Ordering, Arc},
    time::{Duration, SystemTime},
};

use constcat::concat;
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

impl Sourced {
    pub(crate) fn to_flattened_source(&self) -> Source {
        match self {
            Self::Hmw(_) | Self::HmwCached(_) => Source::HmwMaster,
            Self::Iw4(_) | Self::Iw4Cached(_) => Source::Iw4Master,
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

    fn internal_update_cache_call(
        &mut self,
        addr: SocketAddr,
        source: Source,
        host_name: Option<String>,
    ) {
        if let Some(host_name) = host_name {
            self.host_to_connect.insert(host_name, addr);
        }
        self.insert_ports(addr.ip(), &[addr.port()], source);
    }

    pub(crate) fn update_cache_with(&mut self, server: &Server) {
        self.internal_update_cache_call(
            server.source.socket_addr(),
            server.source.to_flattened_source(),
            server.info.as_ref().map(|info| info.host_name.clone()),
        );
    }

    pub(crate) fn push(&mut self, server: Server) {
        self.internal_update_cache_call(
            server.source.socket_addr(),
            server.source.to_flattened_source(),
            server.info.map(|info| info.host_name),
        );
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

    let client = client_with_timeout(5);

    let servers = match get_sourced_servers(DEFAULT_SOURCES, prev, &client).await {
        Ok(servers) => servers,
        Err(err) => {
            finish_spinner();
            return Err(err);
        }
    };

    let mut cache = Cache::default();

    if let Some(prev_cache) = prev {
        let mut lock = prev_cache.lock().await;
        std::mem::swap(&mut cache.connection_history, &mut lock.connection_history);

        for server in servers.iter() {
            let server_ip = server.socket_addr().ip();
            let Some(cached_region) = lock.ip_to_region.remove(&server_ip) else {
                continue;
            };
            cache.ip_to_region.insert(server_ip, cached_region);
        }
    }

    let mut tasks = queue_info_requests(servers, false, &client).await;

    while let Some(res) = tasks.join_next().await {
        match res {
            Ok(result) => match result {
                Ok(server) => cache.push(server),
                Err(mut err) => {
                    error!(name: LOG_ONLY, "{}", err.with_socket_addr().with_source());
                    let Sourced::Iw4(data) = err.meta else {
                        continue;
                    };
                    let Ok(ip) = data.server.ip.parse() else {
                        continue;
                    };
                    cache.insert_ports(ip, &[data.server.port], Source::Iw4Master);
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
                "version": CRATE_VER,
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
