use super::{
    Addressable, GetInfoMetaData, LocationBatchRes, RETRY_TIME_SCALE, RegionContainer, Request,
    Server, Sourced, UnresponsiveCounter, strategies::FilterStrategy, try_batched_location_lookup,
    try_get_info,
};
use crate::{
    LOG_ONLY, ResponseErr, STATUS_OK,
    commands::settings::Settings,
    make_slice_ascii_lowercase,
    models::{
        cli::{Filters, Source},
        json_data::HostData,
    },
    parse_hostname,
    utils::{
        display::{
            DISP_NAME_HMW, DISP_NAME_IW4, DisplayCachedServerUse, DisplayGetInfoCount,
            DisplayServerCount, SingularPlural, indicator::Spinner,
        },
        main_thread_state,
    },
};

use std::{collections::HashSet, net::SocketAddr};

use repl_oxide::ansi_code::{CLEAR_LINE, GREEN, RED, RESET};
use reqwest::Client;
use serde::de::DeserializeOwned;
use tokio::task::JoinSet;
use tracing::{error, info, trace, warn};

pub(super) type HmwJsonFmt = String;
pub(super) type Iw4JsonFmt = HostData;

pub(crate) async fn get_sourced_servers<I, S>(
    sources: I,
    client: &Client,
) -> Result<S, &'static str>
where
    I: IntoIterator<Item = Source>,
    S: FilterStrategy,
{
    let responses = JoinSet::from_iter(
        sources
            .into_iter()
            .map(|source| source.try_get_sourced_servers(client.clone())),
    )
    .join_all()
    .await;

    let mut servers = S::default();

    for res in responses {
        match res {
            Ok(response) => response.push_all::<S>(&mut servers),
            Err((source, err)) => {
                error!("Could not fetch {source} servers");

                if source
                    .try_extend_with_cached_servers::<S>(&mut servers)
                    .is_ok()
                {
                    error!(name: LOG_ONLY, "{err}");
                } else {
                    error!("{err}")
                }
            }
        }
    }

    if servers.is_empty() {
        return Err("Could not populate any servers from source(s)");
    }
    Ok(servers)
}

pub(super) enum SourceResponse {
    Iw4Master(Vec<Iw4JsonFmt>),
    HmwMaster(Vec<HmwJsonFmt>),
}

impl SourceResponse {
    #[inline]
    fn push_all<S: FilterStrategy>(self, collection: &mut S) {
        match self {
            SourceResponse::HmwMaster(data) => collection.extend(data),
            SourceResponse::Iw4Master(data) => collection.extend(data),
        }
    }
}

impl From<Vec<HmwJsonFmt>> for SourceResponse {
    fn from(value: Vec<HmwJsonFmt>) -> Self {
        Self::HmwMaster(value)
    }
}

impl From<Vec<Iw4JsonFmt>> for SourceResponse {
    fn from(value: Vec<Iw4JsonFmt>) -> Self {
        Self::Iw4Master(value)
    }
}

struct HmwServers;
struct Iw4Servers;

trait MasterServer {
    type Response: DeserializeOwned + Into<SourceResponse>;
    const NAME: &str;

    fn url() -> &'static str;
}

impl MasterServer for HmwServers {
    type Response = Vec<HmwJsonFmt>;
    const NAME: &str = DISP_NAME_HMW;

    #[inline]
    fn url() -> &'static str {
        main_thread_state::Endpoints::hmw_master_server()
    }
}

impl MasterServer for Iw4Servers {
    type Response = Vec<Iw4JsonFmt>;
    const NAME: &str = DISP_NAME_IW4;

    #[inline]
    fn url() -> &'static str {
        main_thread_state::Endpoints::iw4_master_server()
    }
}

impl Source {
    async fn fetch<T: MasterServer>(client: Client) -> Result<SourceResponse, ResponseErr> {
        trace!("retrieving {} master server list", T::NAME);

        let response = client.get(T::url()).send().await?;

        if response.status() != STATUS_OK {
            return Err(ResponseErr::bad_status(
                format!("{} master", T::NAME),
                response,
            ));
        }

        response
            .json::<T::Response>()
            .await
            .map(Into::into)
            .map_err(Into::into)
    }

    async fn try_get_sourced_servers(
        self,
        client: Client,
    ) -> Result<SourceResponse, (Source, ResponseErr)> {
        match self {
            Source::HmwMaster => Self::fetch::<HmwServers>(client).await,
            Source::Iw4Master => Self::fetch::<Iw4Servers>(client).await,
        }
        .map_err(|err| (self, err))
    }

    fn try_extend_with_cached_servers<S: FilterStrategy>(self, servers: &mut S) -> Result<(), ()> {
        main_thread_state::Cache::with_borrow(|cache| {
            let backup = match self {
                Source::Iw4Master => &cache.iw4m,
                Source::HmwMaster => &cache.hmw,
            };

            let cached_server_ct = backup.values().fold(0, |acc, servers| acc + servers.len());

            if cached_server_ct == 0 {
                return Err(());
            }

            servers.extend(backup.iter().flat_map(|(&ip, ports)| {
                ports
                    .iter()
                    .map(move |&port| self.with_cached_ip(SocketAddr::new(ip, port)))
            }));

            warn!("{}", DisplayCachedServerUse(self, cached_server_ct));

            Ok(())
        })
    }
}

pub(crate) fn spawn_info_requests(
    servers: impl Iterator<Item = Sourced>,
    remove_duplicates: bool,
    client: &Client,
) -> (usize, JoinSet<Result<Server, GetInfoMetaData>>) {
    let server_info_endpoint = main_thread_state::Endpoints::server_info_endpoint();

    let mut dup = HashSet::new();
    let (tasks, server_ct) = servers.fold((JoinSet::new(), 0), |(mut set, ct), server| {
        if !remove_duplicates || dup.insert(server.socket_addr()) {
            set.spawn(try_get_info(
                Request::New(server),
                client.clone(),
                server_info_endpoint,
            ));
        }
        (set, ct + 1)
    });

    (server_ct - tasks.len(), tasks)
}

/// Returns if [`main_thread_state::Cache`] was modified along with the joined result
pub(super) async fn join_info_requests<R>(
    mut requests: JoinSet<Result<Server, GetInfoMetaData>>,
    args: &Filters,
    settings: Settings,
    client: &Client,
    spinner: &Spinner,
    init: impl FnOnce(usize) -> R,
    insert: impl Fn(&mut R, Server),
) -> (R, bool) {
    let mut out = init(requests.len());

    let can_use_backup = args.can_use_iw4m_data();
    let mut responses = Vec::with_capacity(requests.len());
    let mut did_not_respond = UnresponsiveCounter::default();
    let mut sent_retires = false;
    let max_attempts = args.retry_max.unwrap_or(settings.server_retires);
    let server_info_endpoint = main_thread_state::Endpoints::server_info_endpoint();

    while !requests.is_empty() {
        spinner.update_message(DisplayGetInfoCount(requests.len(), sent_retires).to_string());

        let mut retries = JoinSet::new();
        while let Some(res) = requests.join_next().await {
            match res {
                Ok(Ok(server)) => {
                    // Server isn't running but management software is
                    if server.info.max_clients == 0 {
                        did_not_respond.add(&server.source)
                    } else {
                        responses.push(server)
                    }
                }
                Ok(Err(mut err)) => {
                    if err.retries < max_attempts {
                        let client = client.clone();
                        retries.spawn(async move {
                            tokio::time::sleep(RETRY_TIME_SCALE * (err.retries + 1) as u32).await;
                            try_get_info(Request::Retry(err), client, server_info_endpoint).await
                        });
                    } else {
                        did_not_respond.add(&err.meta);
                        error!(name: LOG_ONLY, "{}", err.with_socket_addr().with_source());
                        if can_use_backup && let Sourced::Iw4(meta) = err.meta {
                            did_not_respond.used_cached_server();
                            insert(&mut out, Server::from(meta));
                        }
                    }
                }
                Err(err) => error!(name: LOG_ONLY, "{err:?}"),
            }
        }

        sent_retires = true;
        requests = retries;
    }

    let modified = main_thread_state::Cache::with_borrow_mut(|cache| {
        responses.into_iter().fold(false, |prev, server| {
            let inserted = cache.update_cache_with(&server);
            insert(&mut out, server);
            inserted || prev
        })
    });

    if did_not_respond.total() > 0 {
        println!("{CLEAR_LINE}{did_not_respond}");
    }

    (out, modified)
}

/// Returns if [`main_thread_state::Cache`] was modified
pub(crate) fn process_region_requests((results, ips_requested): LocationBatchRes) -> bool {
    if results.is_empty() {
        return false;
    }

    let mut found_ct = 0;
    let mut cache_modified = false;
    main_thread_state::Cache::with_borrow_mut(|cache| {
        for (ips, results) in results {
            for (ip, location_res) in ips.iter().zip(results) {
                if let Some(code) = location_res.cont_code {
                    found_ct += 1;
                    cache_modified |= cache.ip_to_region.insert(*ip, code).is_none();
                } else if let Some(err) = location_res.message {
                    error!(name: LOG_ONLY, "API err: {err}, for Ip: {ip}")
                } else {
                    error!(name: LOG_ONLY, "Unknown API error, for Ip: {ip}")
                }
            }
        }
    });

    if found_ct > 0 {
        info!(
            "Cached region data for {} new {}",
            found_ct,
            SingularPlural(found_ct, "hoster", "hosters")
        );
    }

    let failure_ct = ips_requested - found_ct;
    if failure_ct > 0 {
        println!(
            "{CLEAR_LINE}{RED}Failed to resolve location for {failure_ct} server {}{RESET}",
            SingularPlural(failure_ct, "hoster", "hosters")
        )
    }

    cache_modified
}

/// Returns if [`main_thread_state::Cache`] was modified\
/// Does not retain the ordering of `servers`, internally appends valid new lookups to the end
pub(super) async fn filter_via_region<S>(
    sourced_servers: &mut Vec<S>,
    regions: &RegionContainer,
    client: &Client,
    spinner: &Spinner,
) -> bool
where
    S: Addressable + Send + 'static,
{
    spinner.update_message(format!(
        "Determining region of {}",
        DisplayServerCount(sourced_servers.len(), GREEN)
    ));

    let servers = std::mem::replace(
        sourced_servers,
        Vec::with_capacity((sourced_servers.len() as f32 * 0.6).round() as usize),
    );
    let mut check_again = Vec::new();
    let mut new_lookups = HashSet::new();
    let mut new_lookups_vec = Vec::new();

    main_thread_state::Cache::with_borrow(|cache| {
        for sourced_data in servers {
            let socket_addr = sourced_data.socket_addr();
            if let Some(cached_region) = cache.ip_to_region.get(&socket_addr.ip()) {
                if regions.contains(cached_region) {
                    sourced_servers.push(sourced_data);
                }
                continue;
            }
            if new_lookups.insert(socket_addr.ip()) {
                new_lookups_vec.push(socket_addr.ip());
            }
            check_again.push(sourced_data)
        }
    });

    let cache_modified =
        process_region_requests(try_batched_location_lookup(&new_lookups_vec, client).await);

    if cache_modified {
        main_thread_state::Cache::with_borrow(|cache| {
            for sourced_data in check_again {
                if cache
                    .ip_to_region
                    .get(&sourced_data.socket_addr().ip())
                    .is_some_and(|cached_region| regions.contains(cached_region))
                {
                    sourced_servers.push(sourced_data)
                }
            }
        });
    }

    println!(
        "{CLEAR_LINE}{} match the input {}",
        DisplayServerCount(sourced_servers.len(), GREEN),
        SingularPlural(regions.len(), "region", "regions"),
    );

    cache_modified
}

/// Does not retain the ordering of `servers`, internally uses `swap_remove`
pub(super) fn filter_via_get_info(servers: &mut Vec<Server>, args: &mut Filters) {
    args.includes.as_deref_mut().map(make_slice_ascii_lowercase);
    args.excludes.as_deref_mut().map(make_slice_ascii_lowercase);

    for i in (0..servers.len()).rev() {
        let info = &servers[i].info;

        if args
            .team_size_max
            .is_some_and(|max| info.max_clients > max * 2)
        {
            servers.swap_remove(i);
            continue;
        }

        if args.player_min.is_some_and(|min| info.clients < min) {
            servers.swap_remove(i);
            continue;
        }

        if args.with_bots && info.bots == 0 {
            servers.swap_remove(i);
            continue;
        }

        if args.without_bots && info.bots != 0 {
            servers.swap_remove(i);
            continue;
        }

        let mut normalized_hostname = None;
        if let Some(include_terms) = args.includes.as_deref() {
            normalized_hostname = Some(parse_hostname(&info.host_name));
            let hostname = normalized_hostname.as_deref().unwrap();
            if !include_terms.iter().any(|term| hostname.contains(term)) {
                servers.swap_remove(i);
                continue;
            }
        }
        if let Some(exclude_terms) = args.excludes.as_deref() {
            if normalized_hostname.is_none() {
                normalized_hostname = Some(parse_hostname(&info.host_name));
            }
            let hostname = normalized_hostname.as_deref().unwrap();
            if exclude_terms.iter().any(|term| hostname.contains(term)) {
                servers.swap_remove(i);
            }
        }
    }
}
