use crate::{
    CACHED_DATA, CRATE_VER, LOG_ONLY, Operation, OperationResult, Spinner, client_with_timeout,
    commands::{
        filter::{
            Addressable, DEFAULT_SOURCES, Server, Sourced,
            ops::{get_sourced_servers, spawn_info_requests},
            strategies::FastStrategy,
        },
        handler::CommandContext,
        reconnect::HISTORY_MAX,
    },
    does_dir_contain,
    models::{
        cli::Source,
        json_data::{CacheFile, ContCodeMap},
    },
    new_io_error,
    utils::global_state::{self, ThreadCopyState},
};

use std::{
    borrow::Cow,
    collections::HashMap,
    io,
    net::{IpAddr, SocketAddr},
    path::Path,
    time::{Duration, SystemTime},
};

use constcat::concat;
use tracing::{error, info, instrument, trace};

pub(crate) type AddrMap = HashMap<IpAddr, Vec<u16>>;

impl Sourced {
    pub(crate) fn to_flattened_source(&self) -> Source {
        match self {
            Self::Hmw(_) | Self::HmwCached(_) => Source::HmwMaster,
            Self::Iw4(_) | Self::Iw4Cached(_) => Source::Iw4Master,
        }
    }
}

impl global_state::Cache {
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

    fn internal_update_cache_call(&mut self, addr: SocketAddr, source: Source, host_name: String) {
        self.host_to_connect.insert(host_name, addr);
        self.insert_ports(addr.ip(), &[addr.port()], source);
    }

    pub(crate) fn update_cache_with(&mut self, server: &Server) {
        self.internal_update_cache_call(
            server.source.socket_addr(),
            server.source.to_flattened_source(),
            server.info.host_name.clone(),
        );
    }

    pub(crate) fn push(&mut self, server: Server) {
        self.internal_update_cache_call(
            server.source.socket_addr(),
            server.source.to_flattened_source(),
            server.info.host_name,
        );
    }
}

#[instrument(level = "trace", skip_all)]
pub async fn build_cache() -> Result<(), &'static str> {
    let splash_screen_visible = {
        #[cfg(debug_assertions)]
        {
            false
        }

        #[cfg(not(debug_assertions))]
        {
            crate::splash_screen::is_visible()
        }
    };

    let spinner = if !splash_screen_visible {
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
    let servers = match get_sourced_servers::<_, FastStrategy>(DEFAULT_SOURCES, &client).await {
        Ok(servers) => servers,
        Err(err) => {
            finish_spinner();
            return Err(err);
        }
    };

    global_state::Cache::with_borrow_mut(|cache| {
        let mut ip_to_region = servers
            .iter()
            .filter_map(|server| {
                let server_ip = server.socket_addr().ip();
                cache
                    .ip_to_region
                    .get(&server_ip)
                    .map(|&region| (server_ip, region))
            })
            .collect::<HashMap<_, _>>();

        std::mem::swap(&mut cache.ip_to_region, &mut ip_to_region);
    });
    let (_, mut requests) = spawn_info_requests(servers.into_iter(), false, &client);
    let mut servers = Vec::with_capacity(requests.len());
    while let Some(res) = requests.join_next().await {
        match res {
            Ok(Ok(server)) => servers.push(server),
            Ok(Err(mut err)) => {
                error!(name: LOG_ONLY, "{}", err.with_socket_addr().with_source());
                if let Sourced::Iw4(meta) = err.meta {
                    servers.push(Server::from(meta));
                };
            }
            Err(err) => error!(name: LOG_ONLY, "{err:?}"),
        }
    }

    global_state::Cache::with_borrow_mut(|cache| {
        for server in servers {
            cache.push(server);
        }

        cache.created = SystemTime::now();
    });

    global_state::UpdateCache::set(true);

    finish_spinner();
    info!("Cache updated!");

    Ok(())
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
pub fn write_cache(context: &CommandContext, cmd_history: &[String]) -> io::Result<()> {
    let Some(local_path) = context.local_dir() else {
        return new_io_error!(io::ErrorKind::Other, "No valid location to save cache to");
    };
    let file = std::fs::File::create(local_path.join(CACHED_DATA))?;

    global_state::Cache::with_borrow(|cache| {
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
        .map_err(io::Error::other)
    })?;

    info!(name: LOG_ONLY, "Cache saved locally");
    Ok(())
}
