use crate::{
    cli::Source,
    commands::{
        filter::{Sourced, UnresponsiveCounter},
        launch_h2m::LaunchError,
    },
    utils::input::style::{GREEN, RED, WHITE, YELLOW},
};
use std::fmt::Display;

use super::caching::ReadCacheErr;

pub struct ConnectionHelp;

impl Display for ConnectionHelp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Close H2M and use command `{YELLOW}launch{WHITE}` to establish valid connection"
        )
    }
}

pub struct DisplayServerCount(pub usize, pub &'static str);

impl Display for DisplayServerCount {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}{WHITE} {}",
            self.1,
            self.0,
            if self.0 == 1 { "server" } else { "servers" }
        )
    }
}

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

pub struct DisplayCountOf<'a>(pub usize, pub &'a str, pub &'a str);

impl Display for DisplayCountOf<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {}",
            self.0,
            if self.0 == 1 { self.1 } else { self.2 }
        )
    }
}

pub struct DisplayHistoryErr(pub usize);

impl Display for DisplayHistoryErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0 == 1 {
            write!(f, "History only contains 1 entry")
        } else {
            write!(f, "History only contains {} entries", self.0)
        }
    }
}

pub struct DisplayPanic<'a>(pub &'a std::panic::PanicInfo<'a>);

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
            Sourced::Hmw(_) => "HMW master server",
            Sourced::HmwCached(_) => "Cached HMW server",
            Sourced::Iw4(_) => "Iw4m master server",
            Sourced::Iw4Cached(_) => "Cached Iw4m server",
        };
        write!(f, "{display}")
    }
}

impl Display for Source {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let display = match self {
            Source::HmwMaster => "HMW master server",
            Source::Iw4Master => "Iw4m master server",
        };
        write!(f, "{display}")
    }
}

impl Display for UnresponsiveCounter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut line_entered = false;
        if self.iw4 > 0 {
            write!(
                f,
                "{} from: {}, did not respond to a 'getInfo' request",
                DisplayServerCount(self.iw4, RED),
                Source::Iw4Master
            )?;
            line_entered = true;
        }
        if self.hmw > 0 {
            write!(
                f,
                "{}{} from: {}, servers did not respond to a 'getInfo' request",
                if line_entered { "\n" } else { "" },
                DisplayServerCount(self.hmw, RED),
                Source::HmwMaster
            )?;
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
