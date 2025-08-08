use super::{
    Addressable, DEFAULT_SOURCES, FilterData, GAME_ID, GetInfoMetaData, H2M_ID, HMW_ID, HostMeta,
    Request, Server, Sourced, ops::*, try_batched_location_lookup, try_get_info,
};
use crate::{
    command_err,
    commands::{
        handler::{CmdErr, ReplHandle},
        settings::Settings,
    },
    display::{indicator::Spinner, table::DisplaySourceStatsInner},
    elide,
    models::{
        cli::{Filters, Source},
        json_data::{GetInfo, HostData, ServerInfo},
    },
    utils::main_thread_state,
};

use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    net::{IpAddr, SocketAddr},
};

use reqwest::Client;
use tokio::task::JoinSet;

pub(crate) trait FilterStrategy:
    Default + Sized + Send + IntoIterator<Item = Sourced> + 'static
{
    async fn new(sources: Option<HashSet<Source>>, client: &Client) -> Result<Self, CmdErr> {
        match sources {
            Some(user_sources) => get_sourced_servers::<_, Self>(user_sources, client).await,
            None => get_sourced_servers::<_, Self>(DEFAULT_SOURCES, client).await,
        }
        .map_err(|err| command_err!("{err}"))
    }

    fn append(&mut self, other: &mut Self);
    fn is_empty(&self) -> bool;
    fn server_ct(&self) -> usize;
    fn iw4_master_map(hosts: Vec<HostData>) -> Self;
    fn hmw_master_map(servers: Vec<String>) -> Self;
    fn from_cached(cached: Vec<Sourced>) -> Self;
    async fn execute(
        repl: &mut ReplHandle,
        client: Client,
        args: Filters,
        settings: Settings,
        sources: Option<HashSet<Source>>,
        spinner: Spinner,
    ) -> Result<FilterData, CmdErr>;
}

#[derive(Default)]
pub(crate) struct FastStrategy(Vec<Sourced>);

impl IntoIterator for FastStrategy {
    type Item = Sourced;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl FastStrategy {
    pub(crate) fn iter(&self) -> std::slice::Iter<'_, Sourced> {
        self.0.iter()
    }
}

impl FilterStrategy for FastStrategy {
    fn append(&mut self, other: &mut Self) {
        self.0.append(&mut other.0)
    }

    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    fn server_ct(&self) -> usize {
        self.0.len()
    }

    fn iw4_master_map(hosts: Vec<HostData>) -> Self {
        let servers = hosts
            .into_iter()
            .filter_map(|mut host| {
                host.servers
                    .retain(|server| GAME_ID.contains(&server.game.as_str()));
                (!host.servers.is_empty()).then_some(host)
            })
            .flat_map(|host| {
                host.servers.into_iter().filter_map(move |server| {
                    HostMeta::try_from(&host.ip_address, &host.webfront_url, server)
                        .map(Sourced::Iw4)
                })
            })
            .collect();

        Self(servers)
    }

    fn hmw_master_map(servers: Vec<String>) -> Self {
        let servers = servers
            .into_iter()
            .filter_map(Sourced::try_parse_hmw_master)
            .collect();

        Self(servers)
    }

    fn from_cached(cached: Vec<Sourced>) -> Self {
        Self(cached)
    }

    async fn execute(
        _repl: &mut ReplHandle,
        client: Client,
        mut args: Filters,
        settings: Settings,
        sources: Option<HashSet<Source>>,
        spinner: Spinner,
    ) -> Result<FilterData, CmdErr> {
        let mut sourced_servers = Self::new(sources, &client).await?;
        let mut cache_modified = false;

        if let Some(regions) = args.regions(settings) {
            cache_modified =
                filter_via_region(&mut sourced_servers.0, &regions, &client, &spinner).await
        }

        let (duplicates, servers) = if args.need_get_info_data() {
            let (duplicates, requests) =
                spawn_info_requests(sourced_servers.into_iter(), true, &client);

            let (mut servers, mod_info) = join_info_requests(
                requests,
                &args,
                settings,
                &client,
                &spinner,
                Vec::with_capacity,
                |vec, server| vec.push(server),
            )
            .await;
            cache_modified |= mod_info;

            filter_via_get_info(&mut servers, &mut args);

            let out = servers
                .iter()
                .map(|server| (server.source.socket_addr(), server.info.player_ct()))
                .collect();

            (duplicates, out)
        } else {
            let start_len = sourced_servers.0.len();
            let mut ip_set = HashSet::with_capacity(start_len);
            let out = sourced_servers
                .0
                .into_iter()
                .filter(|source| ip_set.insert(source.socket_addr()))
                .map(|source| {
                    if let Sourced::Iw4(meta) = &source {
                        (source.socket_addr(), meta.server.clients)
                    } else {
                        (source.socket_addr(), 0)
                    }
                })
                .collect::<Vec<_>>();

            (start_len - out.len(), out)
        };

        spinner.finish();

        Ok(FilterData {
            servers,
            duplicates,
            cache_modified,
        })
    }
}

#[derive(Debug, Default, Clone)]
pub struct GameStats {
    pub game: Cow<'static, str>,
    pub servers: usize,
    pub unresponsive: usize,
    pub players: usize,
}

impl GameStats {
    pub fn new(games: &[String]) -> Vec<Self> {
        main_thread_state::GameDisplayMap::with_borrow(|map| {
            games
                .iter()
                .map(|game| Self {
                    game: map
                        .get(game.as_str())
                        .copied()
                        .map(Cow::Borrowed)
                        .unwrap_or_else(|| {
                            elide(game, 32).unwrap_or_else(|| game.to_owned()).into()
                        }),
                    ..Default::default()
                })
                .collect()
        })
    }

    fn update(
        &mut self,
        addr: &SocketAddr,
        map: &HashMap<SocketAddr, GetInfo>,
        backup: Option<&ServerInfo>,
    ) -> (usize, usize, usize) {
        if let Some((player_ct, _)) = map
            .get(addr)
            .map(|i| (i.player_ct() as usize, i.max_clients))
            .or_else(|| backup.map(|b| (b.clients as usize, b.max_clients)))
        {
            self.servers += 1;
            self.players += player_ct;
            (1, 0, player_ct)
        } else {
            self.unresponsive += 1;
            (0, 1, 0)
        }
    }

    pub fn add(&mut self, (server, backup, player_ct): (usize, usize, usize)) {
        self.servers += server;
        self.unresponsive += backup;
        self.players += player_ct;
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.servers + self.unresponsive == 0
    }
}

#[derive(Default)]
struct Requests {
    info: JoinSet<Result<Server, GetInfoMetaData>>,
    unseen_ips: Vec<IpAddr>,
    duplicates: usize,
}

#[derive(Default)]
pub(super) struct StatTrackStrategy {
    game: Vec<String>,

    /// `Game<Severs>`
    servers: Vec<Vec<Sourced>>,

    /// Only tracked iw4 master as total
    host_ct: usize,
}

impl IntoIterator for StatTrackStrategy {
    type Item = Sourced;
    type IntoIter = std::iter::Flatten<std::vec::IntoIter<Vec<Sourced>>>;

    fn into_iter(self) -> Self::IntoIter {
        self.servers.into_iter().flatten()
    }
}

impl FilterStrategy for StatTrackStrategy {
    fn append(&mut self, other: &mut Self) {
        self.host_ct += other.host_ct;

        for (game, mut servers) in other.drain(..) {
            let self_i = self.game_i(&game);
            self.servers[self_i].append(&mut servers);
        }
    }

    fn is_empty(&self) -> bool {
        self.servers.iter().all(|server| server.is_empty())
    }

    fn server_ct(&self) -> usize {
        self.servers
            .iter()
            .fold(0, |acc, servers| acc + servers.len())
    }

    fn iw4_master_map(hosts: Vec<HostData>) -> Self {
        let mut out = Self::default();

        for host in hosts {
            for server in host.servers {
                if let Some(sourced) =
                    HostMeta::try_from(&host.ip_address, &host.webfront_url, server)
                {
                    let i = out.game_i(&sourced.server.game);
                    out.servers[i].push(Sourced::Iw4(sourced));
                }
            }

            out.host_ct += 1;
        }

        out
    }

    fn hmw_master_map(servers: Vec<String>) -> Self {
        Self::from_sourced(
            HMW_ID,
            servers
                .into_iter()
                .filter_map(Sourced::try_parse_hmw_master)
                .collect(),
        )
    }

    fn from_cached(cached: Vec<Sourced>) -> Self {
        Self::from_sourced(HMW_ID, cached)
    }

    async fn execute(
        repl: &mut ReplHandle,
        client: Client,
        mut args: Filters,
        settings: Settings,
        sources: Option<HashSet<Source>>,
        spinner: Spinner,
    ) -> Result<FilterData, CmdErr> {
        let mut stat_track = Self::new(sources, &client).await?;

        let requests = Self::queue_requests(
            &client,
            // MARK: TODO
            // 'getInfo' endpoint is only correct for H2M/HMW servers, find a way request the same data from _all_
            // game servers. Meantime iw4m servers are **not** marked as unresponsive in `GameStats::update`. If
            // 'getInfo' requests are received from games other than hmw, we need to ensure _only_ hmw servers are cached
            stat_track
                .filter_sourced(&[HMW_ID])
                .flatten()
                .map(Sourced::addr_copy),
        );

        let ((info_map, mod_info), regions) = tokio::join!(
            join_info_requests(
                requests.info,
                &args,
                settings,
                &client,
                &spinner,
                HashMap::with_capacity,
                |map, server| {
                    map.insert(server.socket_addr(), server.info);
                },
            ),
            try_batched_location_lookup(&requests.unseen_ips, &client)
        );

        let mut cache_modified = process_region_requests(regions) || mod_info;

        let source_stats = stat_track.get_source_stats(&info_map);
        let mut servers = stat_track.collect_to_servers(&[HMW_ID], info_map);

        if let Some(regions) = args.regions(settings) {
            cache_modified |= filter_via_region(&mut servers, &regions, &client, &spinner).await
        }

        filter_via_get_info(&mut servers, &mut args);
        servers.sort_unstable_by(|a, b| b.info.player_ct().cmp(&a.info.player_ct()));

        let out = servers
            .iter()
            .map(|server| (server.socket_addr(), server.info.player_ct()))
            .collect();

        spinner.finish();

        main_thread_state::LastServerStats::set(source_stats, servers);
        main_thread_state::LastServerStats::display(repl)?;

        Ok(FilterData {
            servers: out,
            duplicates: requests.duplicates,
            cache_modified,
        })
    }
}

impl StatTrackStrategy {
    /// For sources where it is not possible to determine the number of hosters
    fn from_sourced<S: AsRef<str>>(game: S, servers: Vec<Sourced>) -> Self {
        Self {
            game: vec![String::from(game.as_ref())],
            servers: vec![servers],
            host_ct: 0,
        }
    }

    fn filter_sourced(&self, ids: &'static [&str]) -> impl Iterator<Item = &[Sourced]> {
        self.game
            .iter()
            .zip(self.servers.iter())
            .filter(|(id, _)| ids.contains(&id.as_str()))
            .map(|(_, servers)| servers.as_slice())
    }

    /// `(game, servers)`
    fn drain<R>(&mut self, r: R) -> impl Iterator<Item = (String, Vec<Sourced>)> + use<'_, R>
    where
        R: std::ops::RangeBounds<usize> + Copy,
    {
        self.game.drain(r).zip(self.servers.drain(r))
    }

    fn game_i(&mut self, mut game: &str) -> usize {
        // Servers found from the iw4 master server still have the game id set as h2m
        if game == H2M_ID {
            game = HMW_ID
        }

        self.game
            .iter()
            .position(|stored| stored == game)
            .unwrap_or_else(|| {
                let i = self.game.len();
                self.game.push(game.to_string());
                self.servers.push(Vec::new());
                i
            })
    }

    fn queue_requests(client: &Client, servers: impl Iterator<Item = Sourced>) -> Requests {
        let server_info_endpoint = main_thread_state::Endpoints::server_info_endpoint();

        let mut seen = HashSet::new();
        let mut res = Requests::default();

        main_thread_state::Cache::with_borrow(|cache| {
            for server in servers {
                let socket_addr = server.socket_addr();
                if seen.insert(socket_addr) {
                    res.info.spawn(try_get_info(
                        Request::New(server),
                        client.clone(),
                        server_info_endpoint,
                    ));
                } else {
                    res.duplicates += 1;
                }

                let ip = socket_addr.ip();
                if !cache.ip_to_region.contains_key(&ip) {
                    res.unseen_ips.push(ip);
                }
            }
        });

        res
    }

    fn collect_to_servers(
        &mut self,
        ids: &'static [&str],
        mut map: HashMap<SocketAddr, GetInfo>,
    ) -> Vec<Server> {
        let mut out = Vec::with_capacity(self.server_ct_by_id(ids));

        for servers in self
            .game
            .iter()
            .enumerate()
            .filter(|(_, id)| ids.contains(&id.as_str()))
            .map(|(i, _)| std::mem::take(&mut self.servers[i]))
        {
            for server in servers {
                if let Some(info) = map.remove(&server.socket_addr()) {
                    out.push(Server {
                        source: server,
                        info,
                    });
                }
            }
        }

        out
    }

    /// [`Self::collect_to_servers`] will move the data required to compute this total
    fn server_ct_by_id(&self, ids: &'static [&str]) -> usize {
        self.filter_sourced(ids).map(<[Sourced]>::len).sum()
    }

    fn get_source_stats(&self, map: &HashMap<SocketAddr, GetInfo>) -> Vec<DisplaySourceStatsInner> {
        let mut hmw = GameStats::new(&self.game);
        let mut hmw_total = GameStats::default();

        let mut iw4 = hmw.clone();
        let mut iw4_total = GameStats::default();

        for (i, servers) in self.servers.iter().map(Vec::as_slice).enumerate() {
            for sourced_server in servers {
                match sourced_server {
                    Sourced::Hmw(socket_addr) => {
                        hmw_total.add(hmw[i].update(socket_addr, map, None));
                    }
                    Sourced::Iw4(server) => iw4_total.add(iw4[i].update(
                        &server.resolved_addr,
                        map,
                        // trust the iw4 instance while we don't have getInfo setup for other games
                        (self.game[i] != HMW_ID).then_some(&server.server),
                    )),
                    _ => (),
                }
            }
        }

        iw4.sort_unstable_by(|a, b| b.players.cmp(&a.players));

        [
            (Source::HmwMaster, hmw_total, hmw, None),
            (
                Source::Iw4Master,
                iw4_total,
                iw4,
                (self.host_ct > 0).then_some(self.host_ct),
            ),
        ]
        .into_iter()
        .filter(|(_, total, _, _)| !total.is_empty())
        .collect()
    }
}

pub struct FilterPreProcess {
    pub addr_lens: Vec<usize>,
    pub max_addr_len: usize,
}

impl FilterPreProcess {
    pub const fn default() -> Self {
        Self {
            addr_lens: Vec::new(),
            max_addr_len: 0,
        }
    }

    fn new(addr_lens: Vec<usize>, max_addr_len: usize) -> Self {
        Self {
            addr_lens,
            max_addr_len,
        }
    }
}

pub fn process_stats(filter: &mut [Server]) -> FilterPreProcess {
    main_thread_state::IDMaps::with_borrow(|map_ids, game_type_ids| {
        let (mut addr_lens, mut max_addr_len) = (Vec::with_capacity(filter.len()), 0);

        for server in filter.iter_mut() {
            let pairs = [
                (&mut server.info.map_name, map_ids, 17),
                (&mut server.info.game_type, game_type_ids, 13),
            ];

            for (field, map, max_len) in pairs {
                if let Some(&display_name) = map.get(field.as_ref()) {
                    *field = Cow::Borrowed(display_name);
                } else if let Some(elided) = elide(field, max_len) {
                    *field = Cow::Owned(elided);
                }
            }

            let ip = server.socket_addr().to_string();
            addr_lens.push(ip.len());
            max_addr_len = max_addr_len.max(ip.len());
        }

        FilterPreProcess::new(addr_lens, max_addr_len)
    })
}
