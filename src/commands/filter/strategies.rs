use super::{
    Addressable, DEFAULT_SOURCES, FilterData, GAME_ID, GetInfoMetaData, H2M_ID, HMW_ID, HostMeta,
    Request, Server, Sourced, ops::*, try_get_info,
};
use crate::{
    ResponseErr, Spinner, client_with_timeout, command_err,
    commands::{
        filter::try_location_lookup,
        handler::{CmdErr, ReplHandle},
    },
    display::{self, BoxBottom, BoxTop, Line, Space},
    models::{
        cli::{Filters, Source},
        json_data::{ContCode, GetInfo, HostData, ServerInfo},
    },
    parse_hostname,
    utils::{caching::AddrMap, global_state},
};

use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    fmt::Display,
    io,
    net::{IpAddr, SocketAddr},
};

use reqwest::Client;
use tokio::task::JoinSet;

const MIN_HOST_NAME_LEN: usize = 18;
const FILTER_HEADER_LEN: usize = 107;
pub(crate) const MIN_FILTER_COLS: usize = FILTER_HEADER_LEN + MIN_HOST_NAME_LEN;

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
        args: Filters,
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
        mut args: Filters,
        sources: Option<HashSet<Source>>,
        spinner: Spinner,
    ) -> Result<FilterData, CmdErr> {
        let client = client_with_timeout(5);

        let mut sourced_servers = Self::new(sources, &client).await?;
        let mut cache_modified = false;

        if let Some(regions) = args.regions() {
            cache_modified =
                filter_via_region(&mut sourced_servers.0, &regions, &client, &spinner).await
        }

        let (duplicates, servers) = if args.need_get_info_data() {
            let (duplicates, requests) =
                spawn_info_requests(sourced_servers.into_iter(), true, &client);

            let (mut servers, mod_info) = join_info_requests(
                requests,
                &args,
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
pub(crate) struct GameStats {
    game: Cow<'static, str>,

    servers: usize,
    unresponsive: usize,
    players: usize,
}

impl GameStats {
    fn new(games: &[String]) -> Vec<Self> {
        games
            .iter()
            .map(|game| Self {
                game: game_name_str(game),
                ..Default::default()
            })
            .collect()
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
            // Server isn't running but management software is
            .filter(|(_, m)| *m != 0)
        {
            self.servers += 1;
            self.players += player_ct;
            (1, 0, player_ct)
        } else {
            self.unresponsive += 1;
            (0, 1, 0)
        }
    }

    fn add(&mut self, (server, backup, player_ct): (usize, usize, usize)) {
        self.servers += server;
        self.unresponsive += backup;
        self.players += player_ct;
    }

    fn is_empty(&self) -> bool {
        self.servers + self.unresponsive == 0
    }
}

struct Requests {
    info: JoinSet<Result<Server, GetInfoMetaData>>,
    region: JoinSet<Result<(IpAddr, ContCode), ResponseErr>>,
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
        mut args: Filters,
        sources: Option<HashSet<Source>>,
        spinner: Spinner,
    ) -> Result<FilterData, CmdErr> {
        let client = client_with_timeout(5);

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

        args.include_unresponsive = false;

        let ((info_map, mod_info), mod_region) = tokio::join!(
            join_info_requests(
                requests.info,
                &args,
                &client,
                &spinner,
                HashMap::with_capacity,
                |map, server| {
                    map.insert(server.socket_addr(), server.info);
                },
            ),
            join_region_requests(requests.region)
        );

        let mut cache_modified = mod_info || mod_region;

        let source_stats = stat_track.get_source_stats(&info_map);
        let mut servers = stat_track.collect_to_servers(&[HMW_ID], info_map);

        if let Some(regions) = args.regions().as_ref() {
            cache_modified |= filter_via_region(&mut servers, regions, &client, &spinner).await
        }

        filter_via_get_info(&mut servers, &mut args);
        servers.sort_unstable_by(|a, b| b.info.player_ct().cmp(&a.info.player_ct()));

        let out = servers
            .iter()
            .map(|server| (server.socket_addr(), server.info.player_ct()))
            .collect();

        spinner.finish();

        process_stats(repl, source_stats, servers)?;

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
        fn insert(map: &mut AddrMap, ip: IpAddr, port: u16) -> (bool, bool) {
            let (mut unique_ip, mut unique_socket) = (false, false);
            map.entry(ip)
                .and_modify(|ports| {
                    if !ports.contains(&port) {
                        unique_socket = true;
                        ports.push(port);
                    }
                })
                .or_insert_with(|| {
                    (unique_ip, unique_socket) = (true, true);
                    vec![port]
                });
            (unique_ip, unique_socket)
        }

        let server_info_endpoint = global_state::Endpoints::server_info_endpoint();

        let mut seen = HashMap::new();

        let (info, region, server_ct) = global_state::Cache::with_borrow(|cache| {
            servers.fold(
                (JoinSet::new(), JoinSet::new(), 0),
                |(mut info_set, mut region_set, ct), server| {
                    let socket_addr = server.socket_addr();
                    let (unique_ip, unique_socket) =
                        insert(&mut seen, socket_addr.ip(), socket_addr.port());

                    if unique_socket {
                        info_set.spawn(try_get_info(
                            Request::New(server),
                            client.clone(),
                            server_info_endpoint,
                        ));
                    }
                    if unique_ip {
                        let ip = socket_addr.ip();
                        if !cache.ip_to_region.contains_key(&ip) {
                            region_set.spawn(try_location_lookup(ip, client.clone()));
                        }
                    }

                    (info_set, region_set, ct + 1)
                },
            )
        });

        Requests {
            duplicates: server_ct - info.len(),
            info,
            region,
        }
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

fn process_stats(
    repl: &mut ReplHandle,
    source: Vec<DisplaySourceStatsInner>,
    mut filter: Vec<Server>,
) -> io::Result<()> {
    global_state::IDMaps::with_borrow(|map_ids, game_type_ids| {
        for server in filter.iter_mut() {
            if let Some(&display_name) = map_ids.get(server.info.map_name.as_ref()) {
                server.info.map_name = Cow::Borrowed(display_name);
            }
            if let Some(&display_name) = game_type_ids.get(server.info.game_type.as_ref()) {
                server.info.game_type = Cow::Borrowed(display_name);
            }
        }
    });

    display::stats(repl, &source, &filter)?;

    global_state::LastServerStats::set(source, filter);

    Ok(())
}

fn count_digits(mut n: usize) -> usize {
    if n == 0 {
        return 1;
    }

    let mut digits = 0;
    while n > 0 {
        digits += 1;
        n /= 10;
    }
    digits
}

fn server_disp_len(servers: usize, unresponsive: usize) -> usize {
    count_digits(servers) + count_digits(unresponsive) + 2
}

pub(crate) type DisplaySourceStatsInner = (Source, GameStats, Vec<GameStats>, Option<usize>);

pub(crate) struct DisplaySourceStats<'a>(pub(crate) &'a [DisplaySourceStatsInner]);

impl Display for DisplaySourceStats<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        const SOURCE_STAT_WIDTH: usize = 73;
        const GAME_HOST_SERVERS_WIDTH: usize = 62;
        const GAME_HOST_WIDTH: usize = 34;
        const HOST_SERVERS_WIDTH: usize = 23;
        const PLAYERS_WIDTH: usize = 9;

        for (source, total, stats, host_ct) in self
            .0
            .iter()
            .map(|(s, t, stats, h)| (s, t, stats.as_slice(), h))
        {
            writeln!(f, " {}", BoxTop(Some(source.to_str()), SOURCE_STAT_WIDTH))?;
            writeln!(
                f,
                " │ Game                        Id    {}  Servers(unresponsive)  Players │",
                if host_ct.is_some() { "Hosts" } else { "     " }
            )?;
            writeln!(f, " │{}│", Space(SOURCE_STAT_WIDTH))?;

            for game_stats in stats.iter() {
                if game_stats.is_empty() {
                    continue;
                }

                let col_1_spacing = GAME_HOST_SERVERS_WIDTH
                    - game_stats.game.len()
                    - server_disp_len(game_stats.servers, game_stats.unresponsive);
                let col_2_spacing = PLAYERS_WIDTH - count_digits(game_stats.players);

                writeln!(
                    f,
                    " │ {}{}{}({}){}{} │",
                    game_stats.game,
                    Space(col_1_spacing),
                    game_stats.servers,
                    game_stats.unresponsive,
                    Space(col_2_spacing),
                    game_stats.players
                )?;
            }
            writeln!(f, " │ {} │", Line(SOURCE_STAT_WIDTH - 2))?;

            let col_1_spacing = GAME_HOST_WIDTH - host_ct.map(count_digits).unwrap_or_default();
            write!(f, " │ Total{}", Space(col_1_spacing))?;

            if let Some(host_total) = *host_ct {
                write!(f, "{host_total}")?;
            }

            let col_2_spacing =
                HOST_SERVERS_WIDTH - server_disp_len(total.servers, total.unresponsive);
            let col_3_spacing = PLAYERS_WIDTH - count_digits(total.players);

            writeln!(
                f,
                "{}{}({}){}{} │",
                Space(col_2_spacing),
                total.servers,
                total.unresponsive,
                Space(col_3_spacing),
                total.players
            )?;

            writeln!(f, " {}", BoxBottom(SOURCE_STAT_WIDTH))?;
        }
        Ok(())
    }
}

fn player_disp_len(players: u8, bots: u8, max: u8) -> usize {
    count_digits(players as usize) + count_digits(bots as usize) + count_digits(max as usize) + 3
}

/// `(Filtered data, width)`
pub(crate) struct DisplayFilterStats<'a>(pub(crate) &'a [Server], pub(crate) usize);

impl Display for DisplayFilterStats<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        const MODE_WIDTH: usize = 16;
        const MAP_PLAYERS_WIDTH: usize = 28;
        const PASS_WIDTH: usize = 6;
        const VERSION_WIDTH: usize = 9;
        const REGION_WIDTH: usize = 37;

        let width = self.1;
        let (ips, max_addr_len) = self.0.iter().fold(
            (Vec::with_capacity(self.0.len()), 0),
            |(mut lens, acc), server| {
                let ip = server.socket_addr().to_string();
                lens.push(ip.len());
                (lens, acc.max(ip.len()))
            },
        );
        let max_host_len = width - FILTER_HEADER_LEN + 2 + (REGION_WIDTH - (max_addr_len + 13));

        writeln!(f, " {}", BoxTop(Some("Servers"), width))?;
        writeln!(
            f,
            " │ Name{}Game Mode       Map  Players(bots)/Max Slots  Pass  Version  Region{}IP │",
            Space(max_host_len - 2),
            Space(max_addr_len + 8),
        )?;
        writeln!(f, " │{}│", Space(width))?;

        let (total_servers, mut total_players) = (self.0.len(), 0);

        global_state::Cache::with_borrow(|cache| {
            for (server, addr_len) in self.0.iter().zip(ips) {
                let mut name = parse_hostname(&server.info.host_name);

                let mut name_chars = name.char_indices();
                if let Some(i) = name_chars
                    .nth(max_host_len - 1)
                    .and_then(|(i, _)| name_chars.next().is_some().then_some(i))
                {
                    let mut elided = String::from(name[..i].trim_end());
                    elided.push('…');
                    name = elided
                }

                let game_type = server.info.game_type.as_ref();
                let map_name = server.info.map_name.as_ref();
                let player_ct = server.info.player_ct();
                let bots = server.info.bots;
                let max_players = server.info.max_public_slots();
                let private = server.info.private.then_some("X").unwrap_or_default();
                let version = server.info.game_version.trim_start_matches('v');
                let addr = server.socket_addr();
                writeln!(
                    f,
                    " │ {name}{}{game_type}{}{map_name}{}{player_ct}({bots})/{max_players}{}{private}{}{version}    {}    {}connect {addr} │",
                    Space(max_host_len + 2 - name.chars().count()),
                    Space(MODE_WIDTH - game_type.len()),
                    Space(
                        MAP_PLAYERS_WIDTH
                            - player_disp_len(player_ct, bots, max_players)
                            - map_name.len()
                    ),
                    Space(PASS_WIDTH - private.len()),
                    Space(VERSION_WIDTH - version.len()),
                    cache
                        .ip_to_region
                        .get(&addr.ip())
                        .map(|code| code.iter().map(|&i| i as char).collect::<String>())
                        .unwrap_or_else(|| String::from("??")),
                    Space(max_addr_len - addr_len),
                )?;

                total_players += player_ct as usize
            }
            Ok(())
        })?;
        let digit_ct = count_digits(total_servers) + count_digits(total_players);

        writeln!(f, " │ {} │", Line(width - 2))?;
        writeln!(
            f,
            " │ {}Total servers: {}  Total players: {} │",
            Space(width - 34 - digit_ct),
            total_servers,
            total_players
        )?;
        writeln!(f, " {}", BoxBottom(width))?;

        Ok(())
    }
}

fn game_name_str(name: &str) -> Cow<'static, str> {
    Cow::Borrowed(match name {
        "COD" => "Modern Warfare             (COD)",
        "H1" => "Modern Warfare Remastered  (H1)",
        "HMW" => "Horizon Modern Warfare     (HMW)",
        "IW3" => "Modern Warfare             (IW3)",
        "IW4" => "Modern Warfare II          (IW4)",
        "IW5" => "Modern Warfare III         (IW3)",
        "IW6" => "Ghosts                     (IW6)",
        "IW7" => "Infinite Warfare           (IW7)",
        "T4" => "World at War               (T4)",
        "T5" => "Black Ops I                (T5)",
        "T6" => "Black Ops II               (T6)",
        "T7" => "Black Ops III              (T7)",
        "SHG1" => "Advanced Warfare           (SHG1)",
        "L4D2" => "Left for Dead II           (L4D2)",
        rest => return Cow::Owned(rest.to_owned()),
    })
}

pub(crate) const MAP_IDS: [(&str, &str); 62] = [
    // MWR
    ("mp_convoy", "Ambush"),
    ("mp_backlot", "Backlot"),
    ("mp_bloc", "Bloc"),
    ("mp_bog", "Bog"),
    ("mp_bog_summer", "Beach Bog"),
    ("mp_broadcast", "Broadcast"),
    ("mp_carentan", "Chinatown"),
    ("mp_countdown", "Countdown"),
    ("mp_crash", "Crash"),
    ("mp_crash_snow", "Winter Crash"),
    ("mp_creek", "Creek"),
    ("mp_crossfire", "Crossfire"),
    ("mp_citystreets", "District"),
    ("mp_farm", "Downpour"),
    ("mp_farm_spring", "Daybreak"),
    ("mp_killhouse", "Killhouse"),
    ("mp_overgrown", "Overgrown"),
    ("mp_pipeline", "Pipeline"),
    ("mp_shipment", "Shipment"),
    ("mp_showdown", "Showdown"),
    ("mp_strike", "Strike"),
    ("mp_vacant", "Vacant"),
    ("mp_cargoship", "Wet Work"),
    // MW2
    ("mp_afghan", "Afghan"),
    ("mp_complex", "Bailout"),
    ("mp_abandon", "Carnival"),
    ("mp_derail", "Derail"),
    ("mp_estate", "Estate"),
    ("mp_favela", "Favela"),
    ("mp_fuel2", "Fuel"),
    ("mp_highrise", "Highrise"),
    ("mp_invasion", "Invasion"),
    ("mp_checkpoint", "Karachi"),
    ("mp_quarry", "Quarry"),
    ("mp_rundown", "Rundown"),
    ("mp_rust", "Rust"),
    ("mp_compact", "Salvage"),
    ("mp_boneyard", "Scrapyard"),
    ("mp_nightshift", "Skidrow"),
    ("mp_storm", "Storm"),
    ("mp_subbase", "Sub Base"),
    ("mp_terminal", "Terminal"),
    ("mp_trailerpark", "Trailer Park"),
    ("mp_underpass", "Underpass"),
    ("mp_brecourt", "Wasteland"),
    // MW3
    ("mp_bootleg", "Bootleg"),
    ("mp_dome", "Dome"),
    ("mp_courtyard_ss", "Erosion"),
    ("mp_lambeth", "Fallen"),
    ("mp_hardhat", "Hardhat"),
    ("mp_alpha", "Lockdown"),
    ("mp_bravo", "Mission"),
    ("mp_paris", "Resistance"),
    ("mp_underground", "Underground"),
    //MW2CR
    ("airport", "Airport"),
    ("boneyard", "Dumpsite"),
    ("cliffhanger", "Blizzard"),
    ("contingency", "Contingency"),
    ("dc_whitehouse", "Whiskey Hotel"),
    ("dcburning", "DC Burning"),
    ("estate", "Safehouse"),
    ("gulag", "Gulag"),
];

pub(crate) const GAME_TYPE_IDS: [(&str, &str); 10] = [
    ("war", "Ground War"),
    ("dom", "Domination"),
    ("conf", "Kill Confirmed"),
    ("sd", "S&D"),
    ("dm", "TDM"),
    ("hp", "Hard Point"),
    ("gun", "Gun Game"),
    ("koth", "Headquarters"),
    ("sab", "Sabotage"),
    ("infect", "Infection"),
];
