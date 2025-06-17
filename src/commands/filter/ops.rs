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
            DisplayCachedServerUse, DisplayGetInfoCount, DisplayServerCount, SingularPlural,
            indicator::Spinner,
        },
        global_state,
    },
};

use std::{collections::HashSet, net::SocketAddr};

use repl_oxide::ansi_code::{CLEAR_LINE, GREEN, RED, RESET};
use reqwest::Client;
use tokio::task::JoinSet;
use tracing::{error, info, trace, warn};

pub(crate) async fn get_sourced_servers<I, S>(
    sources: I,
    client: &Client,
) -> Result<S, &'static str>
where
    I: IntoIterator<Item = Source>,
    S: FilterStrategy,
{
    let mut tasks = JoinSet::from_iter(
        sources
            .into_iter()
            .map(|source| source.try_get_sourced_servers::<S>(client.clone())),
    );

    let mut servers = S::default();

    while let Some(task_res) = tasks.join_next().await {
        match task_res {
            Ok(Ok(mut sourced_servers)) => {
                if servers.is_empty() {
                    servers = sourced_servers;
                } else {
                    servers.append(&mut sourced_servers);
                }
            }
            Ok(Err(err)) => error!("{err}"),
            Err(err) => error!(name: LOG_ONLY, "{err:?}"),
        }
    }

    if servers.is_empty() {
        return Err("Could not populate any servers from source(s)");
    }
    Ok(servers)
}

impl Source {
    async fn iw4_servers<S: FilterStrategy>(client: Client) -> Result<S, ResponseErr> {
        trace!("retrieving iw4 master server list");

        let response = client
            .get(global_state::Endpoints::iw4_master_server())
            .send()
            .await?;

        if response.status() != STATUS_OK {
            return Err(ResponseErr::bad_status("Iw4 master", response));
        }

        Ok(response
            .json::<Vec<HostData>>()
            .await
            .map(S::iw4_master_map)?)
    }

    async fn hmw_servers<S: FilterStrategy>(client: Client) -> Result<S, ResponseErr> {
        trace!("retrieving hmw master server list");

        let response = client
            .get(global_state::Endpoints::hmw_master_server())
            .send()
            .await?;

        if response.status() != STATUS_OK {
            return Err(ResponseErr::bad_status("HMW master", response));
        }

        Ok(response
            .json::<Vec<String>>()
            .await
            .map(S::hmw_master_map)?)
    }

    async fn try_get_sourced_servers<S: FilterStrategy>(
        self,
        client: Client,
    ) -> Result<S, ResponseErr> {
        let sourced_res = match self {
            Source::HmwMaster => Self::hmw_servers::<S>(client).await,
            Source::Iw4Master => Self::iw4_servers::<S>(client).await,
        };

        let Err(err) = sourced_res else {
            return sourced_res;
        };

        if let Some(backup) = self.try_get_cached_servers::<S>().await {
            error!("Could not fetch {self} servers");
            error!(name: LOG_ONLY, "{err}");
            warn!("{}", DisplayCachedServerUse(self, backup.server_ct()));
            return Ok(backup);
        }
        Err(err)
    }

    async fn try_get_cached_servers<S: FilterStrategy>(self) -> Option<S> {
        let cached = global_state::Cache::with_borrow(|cache| {
            let backup = match self {
                Source::Iw4Master => &cache.iw4m,
                Source::HmwMaster => &cache.hmw,
            };

            backup
                .iter()
                .flat_map(|(&ip, ports)| {
                    ports
                        .iter()
                        .map(move |&port| self.with_cached_ip(SocketAddr::new(ip, port)))
                })
                .collect::<Vec<_>>()
        });

        if cached.is_empty() {
            return None;
        }

        Some(S::from_cached(cached))
    }
}

pub(crate) fn spawn_info_requests(
    servers: impl Iterator<Item = Sourced>,
    remove_duplicates: bool,
    client: &Client,
) -> (usize, JoinSet<Result<Server, GetInfoMetaData>>) {
    let server_info_endpoint = global_state::Endpoints::server_info_endpoint();

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

/// Returns if [`global_state::Cache`] was modified along with the joined result
pub(super) async fn join_info_requests<R>(
    mut requests: JoinSet<Result<Server, GetInfoMetaData>>,
    args: &Filters,
    settings: &Settings,
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
    let server_info_endpoint = global_state::Endpoints::server_info_endpoint();

    while !requests.is_empty() {
        spinner.update_message(format!(
            "{}",
            DisplayGetInfoCount(requests.len(), sent_retires)
        ));

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
                        if can_use_backup {
                            if let Sourced::Iw4(meta) = err.meta {
                                did_not_respond.used_cached_server();
                                insert(&mut out, Server::from(meta));
                            }
                        }
                    }
                }
                Err(err) => error!(name: LOG_ONLY, "{err:?}"),
            }
        }

        sent_retires = true;
        requests = retries;
    }

    let modified = global_state::Cache::with_borrow_mut(|cache| {
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

/// Returns if [`global_state::Cache`] was modified
pub(crate) fn process_region_requests((results, ips_requested): LocationBatchRes) -> bool {
    if results.is_empty() {
        return false;
    }

    let mut found_ct = 0;
    let mut cache_modified = false;
    global_state::Cache::with_borrow_mut(|cache| {
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

/// Returns if [`global_state::Cache`] was modified\
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

    global_state::Cache::with_borrow(|cache| {
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
        global_state::Cache::with_borrow(|cache| {
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
