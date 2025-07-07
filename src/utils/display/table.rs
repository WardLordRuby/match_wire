use super::{BoxBottom, BoxTop, DisplayContCode, Line, Space};
use crate::{
    commands::filter::{Addressable, FilterPreProcess, Server, strategies::GameStats},
    elide,
    models::{cli::Source, json_data::ContCodeMap},
    parse_hostname,
};

use std::{borrow::Cow, fmt::Display};

pub(crate) const INNER_TABLE_PADDING: usize = 2;
pub(crate) const TABLE_PADDING: u16 = INNER_TABLE_PADDING as u16 + 3;
const MIN_HOST_NAME_LEN: usize = 18;
const MIN_CONNECT_LEN: usize = 44;
const FILTER_HEADER_LEN: usize = 75;
pub const MIN_FILTER_COLS: usize = MIN_HOST_NAME_LEN + FILTER_HEADER_LEN + MIN_CONNECT_LEN;

pub fn count_digits(mut n: usize) -> usize {
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

pub(crate) struct DisplayHistory<'a>(
    pub(crate) &'a [(&'a str, usize, usize, &'a str)],
    pub(crate) usize,
);

impl Display for DisplayHistory<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let width = self.1;
        let interior_width = width - 4;

        writeln!(f)?;
        writeln!(f, " {}", BoxTop(Some("History"), width))?;
        writeln!(f, " │ Server Name{}Connection Command │", Space(width - 31))?;
        writeln!(f, " │ {} │", Line(width - INNER_TABLE_PADDING))?;
        for (i, (host_name, host_len, ip_len, ip)) in self.0.iter().copied().enumerate() {
            let spacing = interior_width - host_len - ip_len;
            writeln!(f, " │ {}.{host_name}{}{ip} │", i + 1, Space(spacing))?;
        }
        writeln!(f, " {}", BoxBottom(width))
    }
}

pub type DisplaySourceStatsInner = (Source, GameStats, Vec<GameStats>, Option<usize>);

pub struct DisplaySourceStats<'a>(pub &'a [DisplaySourceStatsInner]);

impl Display for DisplaySourceStats<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn server_disp_len(servers: usize, unresponsive: usize) -> usize {
            count_digits(servers) + count_digits(unresponsive) + 2
        }

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
                    - game_stats.game.chars().count()
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
            writeln!(f, " │ {} │", Line(SOURCE_STAT_WIDTH - INNER_TABLE_PADDING))?;

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

fn parse_ver(ver: &str) -> Cow<'_, str> {
    let trim = ver.trim_start_matches('v');

    if trim.len() <= 5 {
        return Cow::Borrowed(trim);
    }

    let mut chars = trim.char_indices();

    if let Some(pre) = chars
        .nth(5)
        .and_then(|(i, c)| (c == '-').then_some(&trim[..i]))
    {
        return Cow::Owned(format!(
            "{pre}{}",
            chars
                .next()
                .map(|(_, c)| c.to_ascii_lowercase())
                .unwrap_or_default()
        ));
    }

    if let Some(elided) = elide(trim, 6) {
        return Cow::Owned(elided);
    }

    Cow::Borrowed(trim)
}

/// `(Filtered data, Pre processed data, cont code map, width)`
pub struct DisplayFilterStats<'a>(
    pub &'a [Server],
    pub &'a FilterPreProcess,
    pub &'a ContCodeMap,
    pub usize,
);

impl Display for DisplayFilterStats<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn player_disp_len(players: u8, bots: u8, max: u8) -> usize {
            count_digits(players as usize)
                + count_digits(bots as usize)
                + count_digits(max as usize)
                + 3
        }

        const MODE_WIDTH: usize = 16;
        const MAP_PLAYERS_WIDTH: usize = 28;
        const PASS_WIDTH: usize = 5;
        const VERSION_WIDTH: usize = 10;

        let width = self.3;
        let max_addr_len = self.1.max_addr_len;
        let region_width = max_addr_len + 8;
        let max_host_len = width - (FILTER_HEADER_LEN - INNER_TABLE_PADDING + region_width);

        writeln!(f, " {}", BoxTop(Some("Servers"), width))?;
        writeln!(
            f,
            " │ Name{}Game Mode       Map  Players(bots)/Max Slots  Pass  Version  Region{}IP │",
            Space(max_host_len - 2),
            Space(region_width),
        )?;
        writeln!(f, " │{}│", Space(width))?;

        let (total_servers, mut total_players) = (self.0.len(), 0);

        for (server, &addr_len) in self.0.iter().zip(self.1.addr_lens.iter()) {
            let mut name = parse_hostname(&server.info.host_name);

            if let Some(elided) = elide(&name, max_host_len - 1) {
                name = elided
            }

            let game_type = server.info.game_type.as_ref();
            let map_name = server.info.map_name.as_ref();
            let player_ct = server.info.player_ct();
            let bots = server.info.bots;
            let max_players = server.info.max_public_slots();
            let private = if server.info.private { "X" } else { "" };
            let version = parse_ver(&server.info.game_version);
            let addr = server.socket_addr();
            writeln!(
                f,
                " │ {name}{}{game_type}{}{map_name}{}{player_ct}({bots})/{max_players}{}{private}{}{version}    {}    {}connect {addr} │",
                Space(max_host_len + 2 - name.chars().count()),
                Space(MODE_WIDTH - game_type.chars().count()),
                Space(
                    MAP_PLAYERS_WIDTH
                        - player_disp_len(player_ct, bots, max_players)
                        - map_name.chars().count()
                ),
                Space(PASS_WIDTH - private.len()),
                Space(VERSION_WIDTH - version.chars().count()),
                DisplayContCode(self.2.get(&addr.ip()).copied()),
                Space(max_addr_len - addr_len),
            )?;

            total_players += player_ct as usize
        }

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
