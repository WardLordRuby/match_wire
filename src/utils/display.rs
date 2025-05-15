use crate::{
    commands::{
        filter::{
            strategies::{
                DisplayFilterStats, DisplaySourceStats, DisplaySourceStatsInner, MIN_FILTER_COLS,
            },
            Server, Sourced, UnresponsiveCounter,
        },
        handler::{AppDetails, GameDetails, ReplHandle},
        launch_h2m::{game_open, LaunchError, WinApiErr},
    },
    global_state,
    models::cli::Source,
    try_fit_table,
    utils::caching::ReadCacheErr,
    CRATE_NAME, CRATE_VER, LOG_ONLY,
};

use std::{borrow::Cow, fmt::Display, io};

use constcat::concat;
use repl_oxide::ansi_code::{GREEN, RED, RESET, YELLOW};

pub(crate) const DISP_NAME_HMW: &str = "HMW";

#[cfg(not(debug_assertions))]
pub(crate) const DISP_NAME_H2M: &str = "H2M";

const DISP_NAME_IW4: &str = "Iw4";

pub(crate) const TABLE_PADDING: u16 = 5;

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
                break;
            }

            max_div = acc
        }
    }

    (max_div > 1).then_some(max_div)
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

pub(crate) fn stats(
    repl: &mut ReplHandle,
    source: &[DisplaySourceStatsInner],
    filter: &[Server],
) -> io::Result<()> {
    let (cols, rows) = repl.terminal_size();
    let width = (cols.saturating_sub(TABLE_PADDING) as usize).max(MIN_FILTER_COLS);
    try_fit_table(repl, (cols, rows), width)?;

    println!();
    println!("{}", DisplaySourceStats(source));
    println!("{}", DisplayFilterStats(filter, width));

    Ok(())
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
            if CRATE_VER == latest {
                GREEN
            } else {
                YELLOW
            }
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

        write!(f, "┌{title}")?;
        write!(f, "{}", Line(self.1 - title.chars().count()))?;
        write!(f, "┐")
    }
}

/// `(total_width)`
pub(crate) struct BoxBottom(pub(crate) usize);

impl Display for BoxBottom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "└")?;
        write!(f, "{}", Line(self.0))?;
        write!(f, "┘")
    }
}

/// `(total_width)`
pub(crate) struct Line(pub(crate) usize);

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
