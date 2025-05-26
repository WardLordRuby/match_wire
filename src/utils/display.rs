use crate::{
    CRATE_NAME, CRATE_VER, LOG_ONLY, ResponseErr,
    commands::{
        filter::{
            Addressable, FilterPreProcess, Server, Sourced, UnresponsiveCounter,
            strategies::GameStats,
        },
        handler::{AppDetails, GameDetails},
        launch_h2m::{LaunchError, WinApiErr, game_open},
    },
    elide, global_state,
    models::cli::Source,
    parse_hostname,
    utils::caching::ReadCacheErr,
};

use std::{borrow::Cow, fmt::Display};

use constcat::concat;
use repl_oxide::ansi_code::{GREEN, RED, RESET, YELLOW};

pub(crate) const DISP_NAME_HMW: &str = "HMW";

#[cfg(not(debug_assertions))]
pub(crate) const DISP_NAME_H2M: &str = "H2M";

const DISP_NAME_IW4: &str = "Iw4";

macro_rules! server_type {
    (master, $name:expr) => {
        concat!($name, " master server")
    };
    (cached, $name:expr) => {
        concat!("Cached ", $name, " server")
    };
}

pub(crate) const SOURCE_HMW: &str = server_type!(master, DISP_NAME_HMW);
const SOURCE_HMW_CACHED: &str = server_type!(cached, DISP_NAME_HMW);
const SOURCE_IW4: &str = server_type!(master, DISP_NAME_IW4);
const SOURCE_IW4_CACHED: &str = server_type!(cached, DISP_NAME_IW4);

pub(crate) const INNER_TABLE_PADDING: usize = 2;
pub(crate) const TABLE_PADDING: u16 = INNER_TABLE_PADDING as u16 + 3;
const MIN_HOST_NAME_LEN: usize = 18;
const MIN_CONNECT_LEN: usize = 44;
const FILTER_HEADER_LEN: usize = 75;
pub const MIN_FILTER_COLS: usize = MIN_HOST_NAME_LEN + FILTER_HEADER_LEN + MIN_CONNECT_LEN;

const PRIME: [usize; 4] = [2, 3, 5, 7];
const MAX_PRODUCT: usize = 42;

macro_rules! write_div_str {
    ($f:expr, $c:literal, $width:expr) => {
        if let Some(max_div) = calc_max_div($width) {
            #[cfg(debug_assertions)]
            const _C: char = $c;

            const TWO: &str = concat!($c, $c);
            const THREE: &str = concat!(TWO, $c);
            const FIVE: &str = concat!(THREE, TWO);
            const SEVEN: &str = concat!(FIVE, TWO);
            const FOURTEEN: &str = concat!(SEVEN, SEVEN);
            const FIFTEEN: &str = concat!(FOURTEEN, $c);
            const THIRTY: &str = concat!(FIFTEEN, FIFTEEN);
            const THIRTY_FIVE: &str = concat!(THIRTY, FIVE);

            let draw = match max_div {
                2 => TWO,
                3 => THREE,
                5 => FIVE,
                6 => concat!(THREE, THREE),
                7 => SEVEN,
                10 => concat!(FIVE, FIVE),
                14 => FOURTEEN,
                15 => FIFTEEN,
                21 => concat!(FOURTEEN, SEVEN),
                30 => THIRTY,
                35 => THIRTY_FIVE,
                42 => concat!(THIRTY_FIVE, SEVEN),
                _ => unreachable!("capped by `MAX_PRODUCT`"),
            };

            write!($f, "{}", Repeat(draw, $width / max_div))
        } else {
            write!($f, "{}", Repeat($c, $width))
        }
    };
}

fn calc_max_div(width: usize) -> Option<usize> {
    if width < *PRIME.last().unwrap() {
        return None;
    }

    let mut max_div = 1;
    for prime in PRIME {
        if width % prime == 0 {
            let acc = max_div * prime;

            if acc > MAX_PRODUCT {
                break;
            }

            max_div = acc
        }
    }

    (max_div > 1).then_some(max_div)
}

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

/// Logs and print's the given `err`
pub fn error<E: Display>(err: E) {
    tracing::error!("{err}")
}

/// Logs and print's the given `warning`
pub fn warning<E: Display>(warning: E) {
    tracing::warn!("{warning}")
}

/// _Only_ logs the given `err`
pub fn log_error<E: Display>(err: E) {
    tracing::error!(name: LOG_ONLY, "{err}")
}

impl Display for ResponseErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResponseErr::Reqwest(err) => write!(f, "{err}"),
            ResponseErr::Status(ctx, status) => write!(f, "{ctx}: {status}"),
            ResponseErr::Other(msg) => write!(f, "{msg}"),
        }
    }
}

pub(crate) struct ConnectionHelp;

fn connection_help() -> Cow<'static, str> {
    if let Some(game_name) = game_open().unwrap_or_else(WinApiErr::resolve_to_closed) {
        Cow::Owned(format!(
            "Close {game_name} and use command `{YELLOW}launch{RESET}` to establish valid connection"
        ))
    } else {
        Cow::Borrowed(concat!(
            "Use command `",
            YELLOW,
            "launch",
            RESET,
            "` to re-launch game"
        ))
    }
}

impl Display for ConnectionHelp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", connection_help())
    }
}

impl From<ConnectionHelp> for Cow<'static, str> {
    fn from(_value: ConnectionHelp) -> Self {
        connection_help()
    }
}

#[cfg(not(debug_assertions))]
pub(crate) struct HmwDownloadHint;

#[cfg(not(debug_assertions))]
impl Display for HmwDownloadHint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "HMW mod files are available to download for free through the Horizon MW launcher\n{}",
            global_state::Endpoints::hmw_download()
        )
    }
}

pub(crate) struct DisplayLogs;

impl Display for DisplayLogs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        global_state::ConsoleHistory::with_borrow_mut(|history| {
            for line in history.new_entries() {
                writeln!(f, "{line}")?;
            }
            history.displayed();
            Ok(())
        })
    }
}

/// `(count, COLOR)`
pub(crate) struct DisplayServerCount(pub(crate) usize, pub(crate) &'static str);

impl Display for DisplayServerCount {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}{RESET} {}",
            self.1,
            self.0,
            SingularPlural(self.0, "server", "servers")
        )
    }
}

///`(source, count)`
pub(crate) struct DisplayCachedServerUse(pub(crate) Source, pub(crate) usize);

impl Display for DisplayCachedServerUse {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Using {} cached {} {}",
            self.1,
            self.0,
            SingularPlural(self.1, "server", "servers")
        )
    }
}

/// `(count, sent_retires)`
pub(crate) struct DisplayGetInfoCount(pub(crate) usize, pub(crate) bool);

impl Display for DisplayGetInfoCount {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} 'getInfo' for {}",
            if !self.1 { "Requesting" } else { "Retrying" },
            if !self.1 {
                DisplayServerCount(self.0, GREEN)
            } else {
                DisplayServerCount(self.0, YELLOW)
            },
        )
    }
}

/// `(count, "singular", "plural")`
pub(crate) struct DisplayCountOf(
    pub(crate) usize,
    pub(crate) &'static str,
    pub(crate) &'static str,
);

impl Display for DisplayCountOf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.0, SingularPlural(self.0, self.1, self.2))
    }
}

/// `(count, "singular", "plural")`
pub(crate) struct SingularPlural(
    pub(crate) usize,
    pub(crate) &'static str,
    pub(crate) &'static str,
);

impl Display for SingularPlural {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", if self.0 == 1 { self.1 } else { self.2 })
    }
}

/// `history.len()`
pub(crate) struct DisplayHistoryErr(pub(crate) usize);

impl Display for DisplayHistoryErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "History only contains {}",
            DisplayCountOf(self.0, "entry", "entries")
        )
    }
}

pub struct DisplayPanic<'a>(pub &'a std::panic::PanicHookInfo<'a>);

impl Display for DisplayPanic<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(location) = self.0.location() {
            write!(
                f,
                "PANIC {}:{}:{}: ",
                location.file(),
                location.line(),
                location.column(),
            )?;
        } else {
            write!(f, "PANIC: ")?;
        }
        if let Some(msg) = self.0.payload().downcast_ref::<&str>() {
            write!(f, "{msg}")
        } else if let Some(msg) = self.0.payload().downcast_ref::<String>() {
            write!(f, "{msg}")
        } else {
            write!(f, "no attached message")
        }
    }
}

impl Display for Sourced {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Hmw(_) => SOURCE_HMW,
                Self::HmwCached(_) => SOURCE_HMW_CACHED,
                Self::Iw4(_) => SOURCE_IW4,
                Self::Iw4Cached(_) => SOURCE_IW4_CACHED,
            }
        )
    }
}

impl Source {
    pub(crate) fn to_str(self) -> &'static str {
        match self {
            Source::Iw4Master => DISP_NAME_IW4,
            Source::HmwMaster => DISP_NAME_HMW,
        }
    }
}

impl Display for Source {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_str())
    }
}

impl UnresponsiveCounter {
    fn flatten(&self) -> [(usize, &'static str); 4] {
        const _: () = assert!(
            // offset by one since this struct also tracks the use of cached servers
            std::mem::size_of::<UnresponsiveCounter>() == std::mem::size_of::<usize>() * 5,
            "`flatten()` needs to be updated if additional server sources are added"
        );

        [
            (self.hmw, SOURCE_HMW),
            (self.hmw_cached, SOURCE_HMW_CACHED),
            (self.iw4, SOURCE_IW4),
            (self.iw4_cached, SOURCE_IW4_CACHED),
        ]
    }
}

impl Display for UnresponsiveCounter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut line_entered = false;
        let mut display_count_of = |count: usize, source: &'static str| -> std::fmt::Result {
            write!(
                f,
                "{}{} from: {source}, did not respond to a 'getInfo' request",
                if line_entered { "\n" } else { "" },
                DisplayServerCount(count, RED),
            )?;
            line_entered = true;
            Ok(())
        };

        for (count, source) in self.flatten() {
            if count > 0 {
                display_count_of(count, source)?
            }
        }

        if self.used_cached_servers > 0 {
            write!(
                f,
                "\nIncluded outdated server data for {YELLOW}{}{RESET} \
                    of {} that did not respond to 'getInfo' request",
                self.used_cached_servers,
                DisplayServerCount(self.total(), RED)
            )?;
        }

        Ok(())
    }
}

impl Display for WinApiErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} code: {}", self.msg, self.code)
    }
}

impl Display for LaunchError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::GameRunning(found_game) => write!(f, "{found_game} is already running"),
            Self::SpawnErr(err) => write!(f, "{}", err.to_string_lossy()),
            Self::WinApiErr(err) => write!(f, "{err}"),
        }
    }
}

impl Display for ReadCacheErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.err)
    }
}

pub(crate) struct HmwUpdateHelp;

impl Display for HmwUpdateHelp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "A new version of HMW is available for download. Use 'HMW Launcher.exe' to update game files"
        )
    }
}

impl Display for GameDetails {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.version.is_none() && self.hash_curr.is_none() {
            return Ok(());
        }

        write!(f, "{} ", self.game_file_name())?;
        let color = match (&self.hash_curr, &self.hash_latest) {
            (Some(curr), Some(latest)) => {
                if curr == latest {
                    GREEN
                } else {
                    YELLOW
                }
            }
            _ => RESET,
        };
        if let Some(version) = self.version {
            write!(f, "{color}v{version}{RESET}")?;
        }
        if let Some(ref hash) = self.hash_curr {
            write!(
                f,
                "{}hash: {color}{hash}{RESET}",
                if self.version.is_some() { ", " } else { "" }
            )?;
        }
        if color == YELLOW {
            write!(f, "\n{GREEN}{HmwUpdateHelp}{RESET}")?;
        }
        Ok(())
    }
}

impl Display for AppDetails {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let color = if let Some(ref latest) = self.ver_latest {
            if CRATE_VER == latest { GREEN } else { YELLOW }
        } else {
            RESET
        };
        write!(f, "{CRATE_NAME}.exe {color}v{CRATE_VER}{RESET}")?;
        if let Some(ref msg) = self.update_msg {
            if color == YELLOW {
                write!(f, "\n{msg}")?;
            }
        }
        Ok(())
    }
}

/// `(repeat, count)`
struct Repeat<T>(T, usize);

impl<T: Display> Display for Repeat<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for _ in 0..self.1 {
            write!(f, "{}", self.0)?;
        }
        Ok(())
    }
}

/// `(Option<"title">, total_width)`
pub(crate) struct BoxTop(pub(crate) Option<&'static str>, pub(crate) usize);

impl Display for BoxTop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let title = self.0.unwrap_or_default();
        write!(f, "┌{title}{}┐", Line(self.1 - title.chars().count()))
    }
}

/// `(total_width)`
pub(crate) struct BoxBottom(pub(crate) usize);

impl Display for BoxBottom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "└{}┘", Line(self.0))
    }
}

/// `(total_width)`
pub struct Line(pub usize);

impl Display for Line {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write_div_str!(f, '─', self.0)
    }
}

/// `(total_width)`
pub(crate) struct Space(pub(crate) usize);

impl Display for Space {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write_div_str!(f, ' ', self.0)
    }
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

/// `(Filtered data, Pre processed data, width)`
pub struct DisplayFilterStats<'a>(pub &'a [Server], pub &'a FilterPreProcess, pub usize);

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

        let width = self.2;
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

        global_state::Cache::with_borrow(|cache| {
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
                let private = server.info.private.then_some("X").unwrap_or_default();
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

pub const GAME_DISPLAY_NAMES: [(&str, &str); 14] = [
    ("COD", "Modern Warfare             (COD)"),
    ("H1", "Modern Warfare Remastered  (H1)"),
    ("HMW", "Horizon Modern Warfare     (HMW)"),
    ("IW3", "Modern Warfare             (IW3)"),
    ("IW4", "Modern Warfare II          (IW4)"),
    ("IW5", "Modern Warfare III         (IW3)"),
    ("IW6", "Ghosts                     (IW6)"),
    ("IW7", "Infinite Warfare           (IW7)"),
    ("T4", "World at War               (T4)"),
    ("T5", "Black Ops I                (T5)"),
    ("T6", "Black Ops II               (T6)"),
    ("T7", "Black Ops III              (T7)"),
    ("SHG1", "Advanced Warfare           (SHG1)"),
    ("L4D2", "Left for Dead II           (L4D2)"),
];

pub const MAP_IDS: [(&str, &str); 62] = [
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

pub const GAME_TYPE_IDS: [(&str, &str); 12] = [
    ("war", "TDM"),
    ("dom", "Domination"),
    ("dd", "Demolition"),
    ("ctf", "CTF"),
    ("conf", "Kill Confirmed"),
    ("sd", "S&D"),
    ("dm", "Free for All"),
    ("hp", "Hard Point"),
    ("gun", "Gun Game"),
    ("koth", "Headquarters"),
    ("sab", "Sabotage"),
    ("infect", "Infection"),
];
