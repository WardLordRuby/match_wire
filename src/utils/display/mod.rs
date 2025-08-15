pub(crate) mod indicator;
pub mod table;

use crate::{
    CRATE_NAME, CRATE_VER, LOG_ONLY, RateLimitConfig, RateLimiter, ResponseErr,
    commands::{
        filter::{Sourced, UnresponsiveCounter},
        handler::{AppDetails, GameDetails, ModFileStatus},
        launch::{LaunchError, WinApiErr, game_open},
    },
    main_thread_state,
    models::cli::Source,
    utils::caching::{ContCode, ReadCacheErr},
};

use std::{borrow::Cow, fmt::Display};

use constcat::concat;
use repl_oxide::ansi_code::{GREEN, RED, RESET, YELLOW};

pub(crate) const DISP_NAME_HMW: &str = "HMW";
const HMW_LAUNCHER: &str = "HMW Launcher.exe";

pub(crate) const VERIFY_HELP: &str = concat!(
    GREEN,
    "Run ",
    YELLOW,
    "version --verify-all",
    GREEN,
    " to check hashes",
    RESET,
);

#[cfg(not(debug_assertions))]
pub(crate) const DISP_NAME_H2M: &str = "H2M";

pub(crate) const DISP_NAME_IW4: &str = "Iw4";

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
                // We know `max_div` must be greater than 1 since
                // `initial max_div * PRIME.last() < MAX_PRODUCT`. Thus it is okay to return
                return Some(max_div);
            }

            max_div = acc
        }
    }

    (max_div > 1).then_some(max_div)
}

/// Logs and print's the given `err`
#[inline]
pub fn error<E: Display>(err: E) {
    tracing::error!("{err}")
}

/// Logs and print's the given `warning`
#[inline]
pub fn warning<E: Display>(warning: E) {
    tracing::warn!("{warning}")
}

/// _Only_ logs the given `err`
#[inline]
pub fn log_error<E: Display>(err: E) {
    tracing::error!(name: LOG_ONLY, "{err}")
}

impl Display for ResponseErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResponseErr::Reqwest(err) => write!(f, "{err}"),
            ResponseErr::Status(ctx, status) => write!(f, "{ctx}: {status}"),
            ResponseErr::Other(msg) => write!(f, "{msg}"),
            ResponseErr::Pgp(err) => write!(f, "PGP verification failed: {err}"),
            ResponseErr::Serialize(ctx, err) => write!(f, "{ctx} formatting has changed: {err}"),
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
            main_thread_state::Endpoints::hmw_download()
        )
    }
}

pub(crate) struct DisplayContCode(Option<ContCode>);

impl Display for DisplayContCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Some(cont_code) = self.0 else {
            return write!(f, "??");
        };

        write!(f, "{}{}", cont_code[0] as char, cont_code[1] as char)
    }
}

pub(crate) struct DisplayLogs;

impl Display for DisplayLogs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        main_thread_state::ConsoleHistory::with_borrow_mut(|history| {
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
    fn flatten(&self) -> Vec<(usize, &'static str)> {
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
        .into_iter()
        .filter(|&(ct, _)| ct > 0)
        .collect()
    }
}

impl Display for UnresponsiveCounter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let total = self.total();
        let counts = self.flatten();
        debug_assert!(!counts.is_empty(), "Nothing to display");

        if counts.len() == 1 {
            write!(
                f,
                "{} from: {}, did not respond to a 'getInfo' request",
                DisplayServerCount(counts[0].0, RED),
                counts[0].1
            )?;
        } else {
            write!(
                f,
                "{} did not respond to a 'getInfo' request (",
                DisplayServerCount(total, RED),
            )?;

            for (i, (count, server)) in counts.into_iter().enumerate() {
                write!(
                    f,
                    "{}{RED}{count}{RESET}-{server}",
                    if i != 0 { ", " } else { "" }
                )?;
            }
            write!(f, ")")?;
        }

        if self.used_cached_servers > 0 {
            write!(
                f,
                "\nIncluded outdated server data for {YELLOW}{}{RESET} \
                    of {} that did not respond to 'getInfo' request",
                self.used_cached_servers,
                DisplayServerCount(total, RED)
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

impl Display for ModFileStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (outdated, files) = match self {
            ModFileStatus::Initial => {
                unreachable!("Startup will always advance state past `Initial`")
            }
            ModFileStatus::MissingFiles(items) => (false, items.as_slice()),
            ModFileStatus::VerifyReady => {
                return write!(f, "{YELLOW}HMW files not verified{RESET}\n{VERIFY_HELP}");
            }
            ModFileStatus::Outdated(items) => (true, items.as_slice()),
            ModFileStatus::UpToDate => {
                return write!(f, "{GREEN}All HMW files verified! GLHF{RESET}");
            }
        };

        write!(
            f,
            "{RED}{} {}",
            if outdated { "Outdated" } else { "Missing" },
            files[0]
        )?;
        if files.len() > 1 {
            writeln!(
                f,
                ", and {}{RESET}",
                DisplayCountOf(files.len() - 1, "other file", "other files")
            )
        } else {
            writeln!(f, "{RESET}")
        }?;

        write!(
            f,
            "{GREEN}Use '{HMW_LAUNCHER}' to {} game files{RESET}",
            if outdated { "update" } else { "get missing" }
        )
    }
}

impl ModFileStatus {
    fn color(&self) -> &'static str {
        match self {
            ModFileStatus::Initial | ModFileStatus::VerifyReady => "",
            ModFileStatus::MissingFiles(_) | ModFileStatus::Outdated(_) => YELLOW,
            ModFileStatus::UpToDate => GREEN,
        }
    }
}

pub(crate) struct HmwUpdateHelp;

impl Display for HmwUpdateHelp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "A new version of HMW is available for download. Use '{HMW_LAUNCHER}' to update game files"
        )
    }
}

impl Display for GameDetails {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.version.is_none() && self.hash_curr.is_none() {
            return Ok(());
        }

        let color = self.mod_verification.color();
        writeln!(f, "{}", self.mod_verification)?;

        write!(f, "{} ", self.game_file_name())?;
        if let Some(version) = self.version {
            write!(f, "{color}v{version}{RESET}")?;
        }
        if let Some(hash) = self.hash_curr.as_deref() {
            write!(
                f,
                "{}hash: {color}{hash}{RESET}",
                if self.version.is_some() { ", " } else { "" }
            )?;
        }
        if color == YELLOW
            && !matches!(
                self.mod_verification,
                ModFileStatus::MissingFiles(_) | ModFileStatus::Outdated(_)
            )
        {
            write!(f, "\n{GREEN}{HmwUpdateHelp}{RESET}")?;
        }
        Ok(())
    }
}

impl Display for AppDetails {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let color = if let (Some(curr), Some(latest)) =
            (self.hash_curr.as_deref(), self.hash_latest.as_deref())
        {
            if curr == latest { GREEN } else { YELLOW }
        } else {
            ""
        };
        write!(f, "{CRATE_NAME}.exe {color}v{CRATE_VER}{RESET}")?;
        if color == YELLOW
            && let Some(msg) = self.update_msg.as_deref()
        {
            write!(f, "\n{GREEN}{msg}{RESET}")?;
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

impl<T: RateLimitConfig> Display for RateLimiter<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.limited() {
            write!(
                f,
                "{} service rate limited for another {} seconds",
                T::DISP_NAME,
                (self.interval - self.elapsed().unwrap_or_default()).as_secs()
            )
        } else {
            write!(f, "{} service not currently rate limited", T::DISP_NAME)
        }
    }
}

pub const GAME_DISPLAY_NAMES: [(&str, &str); 15] = [
    ("COD", "Call of Duty 2             (COD)"),
    ("H1", "Modern Warfare Remastered  (H1)"),
    ("HMW", "Horizon Modern Warfare     (HMW)"),
    ("IW3", "Modern Warfare             (IW3)"),
    ("IW4", "Modern Warfare II          (IW4)"),
    ("IW5", "Modern Warfare III         (IW5)"),
    ("IW6", "Ghosts                     (IW6)"),
    ("IW7", "Infinite Warfare           (IW7)"),
    ("T4", "World at War               (T4)"),
    ("T5", "Black Ops I                (T5)"),
    ("T6", "Black Ops II               (T6)"),
    ("T7", "Black Ops III              (T7)"),
    ("SHG1", "Advanced Warfare           (SHG1)"),
    ("L4D2", "Left for Dead II           (L4D2)"),
    ("UKN", "Unknown"),
];

pub const MAP_IDS: [(&str, &str); 65] = [
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
    ("mp_plaza2", "Arkaden"),
    ("mp_mogadishu", "Bakaara"),
    ("mp_bootleg", "Bootleg"),
    ("mp_dome", "Dome"),
    ("mp_courtyard_ss", "Erosion"),
    ("mp_lambeth", "Fallen"),
    ("mp_hardhat", "Hardhat"),
    ("mp_alpha", "Lockdown"),
    ("mp_bravo", "Mission"),
    ("mp_paris", "Resistance"),
    ("mp_seatown", "Seatown"),
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

pub const GAME_TYPE_IDS: [(&str, &str); 13] = [
    ("war", "TDM"),
    ("dom", "Domination"),
    ("dd", "Demolition"),
    ("dz", "Drop Zone"),
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
