use crate::{
    cli::HistoryArgs,
    commands::{
        handler::{CommandContext, CommandHandle},
        launch_h2m::HostName,
    },
};
use std::{ffi::OsString, fmt::Display};
use tokio::sync::RwLock;
use tracing::error;
use winptyrs::PTY;

pub const HISTORY_MAX: i64 = 6;

struct DisplayHistory<'a>(&'a [HostName]);

impl<'a> Display for DisplayHistory<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, host_name) in self.0.iter().rev().enumerate() {
            writeln!(f, "{}. {}", i + 1, host_name.parsed)?;
            if i == HISTORY_MAX as usize - 1 {
                break;
            }
        }
        Ok(())
    }
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
    if let Err(err) = context.check_h2m_connection().await {
        error!("{err}");
        return CommandHandle::default();
    }
    let lock = context.pty_handle().unwrap();
    let server_history_arc = context.h2m_server_connection_history();
    let server_history = server_history_arc.lock().await;
    if server_history.is_empty() {
        error!("No joined servers in history, connect to a server to add it to history");
        return CommandHandle::default();
    }
    if args.history {
        println!("{}", DisplayHistory(&server_history));
        return CommandHandle::default();
    }
    let history_len = server_history.len();
    let cache_arc = context.cache();
    let cache = cache_arc.lock().await;
    let connect = if let Some(num) = args.connect {
        if num as usize > history_len {
            error!("{}", DisplayHistoryErr(history_len));
            return CommandHandle::default();
        }
        cache
            .host_to_connect
            .get(&server_history[history_len - num as usize].raw)
    } else {
        cache
            .host_to_connect
            .get(&server_history.last().unwrap().raw)
    };
    if let Some(ip_port) = connect {
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
