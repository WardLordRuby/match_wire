use crate::{
    cli::HistoryArgs,
    commands::{
        handler::{CommandContext, CommandHandle},
        launch_h2m::HostName,
    },
    utils::caching::Cache,
};
use std::{ffi::OsString, fmt::Display};
use tokio::sync::{Mutex, RwLock};
use tracing::error;
use winptyrs::PTY;

pub const HISTORY_MAX: i64 = 6;

struct DisplayHistory<'a>(&'a [HostName], &'a [&'a str]);

impl<'a> Display for DisplayHistory<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        debug_assert!(self.0.len() >= self.1.len());
        let longest_host_len = self
            .0
            .iter()
            .rev()
            .take(HISTORY_MAX as usize)
            .map(|entry| entry.parsed.chars().count())
            .max()
            .unwrap_or_default();
        let longest_connect_len = self
            .1
            .iter()
            .rev()
            .take(HISTORY_MAX as usize)
            .map(|entry| entry.chars().count())
            .max()
            .unwrap_or_default();
        let width = longest_connect_len + longest_host_len + 15;
        let connect_len = self.1.len() - 1;
        writeln!(f, "{}", "-".repeat(width))?;
        for (i, host_name) in self.0.iter().rev().enumerate() {
            let curr_ip = self.1[connect_len - i];
            let spacing = width - 15 - host_name.parsed.chars().count() - curr_ip.chars().count();
            writeln!(
                f,
                "| {}. {}{}connect {curr_ip} |",
                i + 1,
                host_name.parsed,
                " ".repeat(spacing)
            )?;
            if i == HISTORY_MAX as usize - 1 {
                break;
            }
        }
        writeln!(f, "{}", "-".repeat(width))?;
        Ok(())
    }
}

async fn display_history<'a>(history: &'a [HostName], cache: &'a Mutex<Cache>) {
    let cache = cache.lock().await;
    let ips = history.iter().rev().fold(
        Vec::with_capacity(HISTORY_MAX as usize),
        |mut connections, entry| {
            connections.push(
                cache
                    .host_to_connect
                    .get(&entry.raw)
                    .map(|ip| ip.as_str())
                    .unwrap_or("Server not found in cache"),
            );
            connections
        },
    );
    println!("{}", DisplayHistory(history, &ips));
}

struct DisplayHistoryErr(usize);

impl Display for DisplayHistoryErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0 == 1 {
            writeln!(f, "History only contains 1 entry")?;
        } else {
            writeln!(f, "History only contains {} entries", self.0)?;
        }
        Ok(())
    }
}

pub async fn reconnect<'a>(args: HistoryArgs, context: &mut CommandContext<'a>) -> CommandHandle {
    let server_history_arc = context.h2m_server_connection_history();
    let mut server_history = server_history_arc.lock().await;
    if server_history.is_empty() {
        error!("No joined servers in history, connect to a server to add it to history");
        return CommandHandle::default();
    }
    let cache_arc = context.cache();
    if args.history {
        display_history(&server_history, &cache_arc).await;
        return CommandHandle::default();
    }
    if let Err(err) = context.check_h2m_connection().await {
        error!("{err}");
        return CommandHandle::default();
    }
    let history_len = server_history.len();
    let cache = cache_arc.lock().await;
    if let Some(num) = args.connect {
        if num > 1 {
            if num as usize > history_len {
                error!("{}", DisplayHistoryErr(history_len));
                return CommandHandle::default();
            }
            let entry = server_history.remove(history_len - num as usize);
            server_history.push(entry);
        }
    }
    let connect = cache
        .host_to_connect
        .get(&server_history.last().unwrap().raw);
    if let Some(ip_port) = connect {
        let lock = context.pty_handle().unwrap();
        connect_to(ip_port, &lock)
            .await
            .unwrap_or_else(|err| error!("{err}"));
    } else {
        error!("Could not find server in cache")
    }
    CommandHandle::default()
}

/// Before calling be sure to guard against invalid handles by checking `.check_h2m_connection().is_ok()`
async fn connect_to(ip_port: &str, lock: &RwLock<PTY>) -> Result<(), String> {
    let handle = lock.read().await;
    let send_command = |command: &str| match handle.write(OsString::from(command)) {
        Ok(chars) => {
            if chars == 0 {
                Err(String::from("Failed to send command to h2m console"))
            } else {
                Ok(())
            }
        }
        Err(err) => Err(err.to_string_lossy().to_string()),
    };

    send_command("disconnect\r\n")?;
    std::thread::sleep(std::time::Duration::from_millis(10));
    send_command(&format!("connect {ip_port}\r\n"))
}
