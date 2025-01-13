use crate::{
    commands::{
        filter::{Sourced, UnresponsiveCounter},
        handler::{AppDetails, GameDetails},
        launch_h2m::{h2m_running, LaunchError},
    },
    utils::{
        caching::ReadCacheErr,
        input::style::{GREEN, RED, WHITE, YELLOW},
    },
};
use constcat::concat;
use std::{borrow::Cow, fmt::Display};

const SOURCE_HMW: &str = "HMW master server";
const SOURCE_HMW_CACHED: &str = "Cached HMW server";
const SOURCE_IW4: &str = "Iw4m master server";
const SOURCE_IW4_CACHED: &str = "Cached Iw4m server";

pub struct ConnectionHelp;

fn connection_help() -> &'static str {
    if h2m_running() {
        concat!(
            "Close MW2 Remastered and use command `",
            YELLOW,
            "launch",
            WHITE,
            "` to establish valid connection"
        )
    } else {
        concat!(
            "Use command `",
            YELLOW,
            "launch",
            WHITE,
            "` to re-launch game"
        )
    }
}

impl Display for ConnectionHelp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", connection_help())
    }
}

impl From<ConnectionHelp> for Cow<'static, str> {
    fn from(_value: ConnectionHelp) -> Self {
        Cow::Borrowed(connection_help())
    }
}

pub struct DisplayLogs<'a>(pub &'a [String]);

impl Display for DisplayLogs<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for line in self.0 {
            writeln!(f, "{line}")?;
        }
        Ok(())
    }
}

/// `(count, COLOR)`
pub struct DisplayServerCount(pub usize, pub &'static str);

impl Display for DisplayServerCount {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}{WHITE} {}",
            self.1,
            self.0,
            SingularPlural(self.0, "server", "servers")
        )
    }
}

///`(source, count)`
pub struct DisplayCachedServerUse(pub &'static str, pub usize);

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
pub struct DisplayGetInfoCount(pub usize, pub bool);

impl Display for DisplayGetInfoCount {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} 'getInfo' for {}...",
            if !self.1 { "Requesting" } else { "Retring" },
            if !self.1 {
                DisplayServerCount(self.0, GREEN)
            } else {
                DisplayServerCount(self.0, YELLOW)
            },
        )
    }
}

/// `(count, "singular", "plural")`
pub struct DisplayCountOf(pub usize, pub &'static str, pub &'static str);

impl Display for DisplayCountOf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.0, SingularPlural(self.0, self.1, self.2))
    }
}

/// `(count, "singular", "plural")`
pub struct SingularPlural(pub usize, pub &'static str, pub &'static str);

impl Display for SingularPlural {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", if self.0 == 1 { self.1 } else { self.2 })
    }
}

/// `history.len()`
pub struct DisplayHistoryErr(pub usize);

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
        let display = match self {
            Sourced::Hmw(_) => SOURCE_HMW,
            Sourced::HmwCached(_) => SOURCE_HMW_CACHED,
            Sourced::Iw4(_) => SOURCE_IW4,
            Sourced::Iw4Cached(_) => SOURCE_IW4_CACHED,
        };
        write!(f, "{display}")
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

impl Display for LaunchError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let display = match self {
            LaunchError::Running(msg) => *msg,
            LaunchError::SpawnErr(err) => &err.to_string_lossy(),
        };
        write!(f, "{display}")
    }
}

impl Display for ReadCacheErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.err)
    }
}

pub struct HmwUpdateHelp;

impl Display for HmwUpdateHelp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "A new version of HMW is available for download. Use 'HMW Launcher.exe' to update game files")
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
            _ => WHITE,
        };
        let mut wrote_details = false;
        if let Some(version) = self.version {
            write!(f, "{color}v{version}{WHITE}")?;
            wrote_details = true;
        }
        if let Some(ref hash) = self.hash_curr {
            write!(
                f,
                "{}hash: {color}{hash}{WHITE}",
                if wrote_details { ", " } else { "" }
            )?;
        }
        if color == YELLOW {
            write!(f, "\n{GREEN}{HmwUpdateHelp}{WHITE}")?;
        }
        Ok(())
    }
}

impl Display for AppDetails {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let color = if let Some(ref latest) = self.ver_latest {
            if self.ver_curr == latest {
                GREEN
            } else {
                YELLOW
            }
        } else {
            WHITE
        };
        write!(
            f,
            "{}.exe {color}v{}{WHITE}",
            env!("CARGO_PKG_NAME"),
            self.ver_curr
        )?;
        if let Some(ref msg) = self.update_msg {
            if color == YELLOW {
                write!(f, "\n{msg}")?;
            }
        }
        Ok(())
    }
}
