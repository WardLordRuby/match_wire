use crate::{
    commands::{
        filter::{Sourced, UnresponsiveCounter},
        handler::{AppDetails, ConsoleHistory, GameDetails},
        launch_h2m::{game_open, LaunchError, WinApiErr},
    },
    models::cli::Source,
    utils::caching::ReadCacheErr,
    CRATE_NAME, CRATE_VER,
};

use std::{borrow::Cow, fmt::Display, sync::atomic::Ordering};

use constcat::concat;
use repl_oxide::ansi_code::{GREEN, RED, RESET, YELLOW};

pub(crate) const DISP_NAME_HMW: &str = "HMW";
pub(crate) const DISP_NAME_H2M: &str = "H2M";
const DISP_NAME_IW4: &str = "Iw4m";

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

pub(crate) struct DisplayLogs<'a>(pub(crate) &'a ConsoleHistory);

impl Display for DisplayLogs<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let last = self.0.last.load(Ordering::Relaxed);
        for line in &self.0.history[last..] {
            writeln!(f, "{line}")?;
        }
        self.0.last.store(self.0.history.len(), Ordering::SeqCst);
        Ok(())
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

impl Display for Source {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Source::Iw4Master => DISP_NAME_IW4,
                Source::HmwMaster => DISP_NAME_HMW,
            }
        )
    }
}

impl UnresponsiveCounter {
    fn flatten(&self) -> [(usize, &'static str); 4] {
        const _: () = assert!(
            std::mem::size_of::<UnresponsiveCounter>() == std::mem::size_of::<usize>() * 4,
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
        let mut wrote_details = false;
        if let Some(version) = self.version {
            write!(f, "{color}v{version}{RESET}")?;
            wrote_details = true;
        }
        if let Some(ref hash) = self.hash_curr {
            write!(
                f,
                "{}hash: {color}{hash}{RESET}",
                if wrote_details { ", " } else { "" }
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
