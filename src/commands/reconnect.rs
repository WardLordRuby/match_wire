use crate::{
    cli::HistoryArgs,
    commands::{
        handler::{CommandContext, CommandHandle},
        launch_h2m::HostName,
    },
    utils::{
        display::{ConnectionHelp, DisplayHistoryErr},
        input::style::{WHITE, YELLOW},
    },
};
use std::{borrow::Cow, collections::HashMap, ffi::OsString, fmt::Display, net::SocketAddr};
use tokio::sync::RwLock;
use tracing::{error, info};
use winptyrs::PTY;

pub const HISTORY_MAX: usize = 6;

struct DisplayHistory<'a>(&'a [HostName], &'a [Cow<'static, str>]);

impl<'a> Display for DisplayHistory<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut longest_host_len = 0;
        let mut longest_connect_len = 0;
        let set = self
            .0
            .iter()
            .rev()
            .take(HISTORY_MAX)
            .enumerate()
            .map(|(i, host)| {
                let host_ip = self.1[i].as_ref();
                let name_len = host.parsed.chars().count();
                let ip_len = host_ip.chars().count();
                longest_host_len = longest_host_len.max(name_len);
                longest_connect_len = longest_connect_len.max(ip_len);
                (i + 1, host.parsed.as_str(), name_len, ip_len, host_ip)
            })
            .collect::<Vec<_>>();
        let width = longest_connect_len + longest_host_len + 8;
        writeln!(f)?;
        writeln!(f, "{}", "-".repeat(width))?;
        for (num, host_name, host_len, ip_len, ip) in set {
            let spacing = width - 7 - host_len - ip_len;
            writeln!(f, "| {num}.{host_name}{} {ip} |", " ".repeat(spacing))?;
        }
        writeln!(f, "{}", "-".repeat(width))?;
        Ok(())
    }
}

fn display_history<'a>(history: &'a [HostName], host_to_connect: &'a HashMap<String, SocketAddr>) {
    let ips = history
        .iter()
        .rev()
        .take(HISTORY_MAX)
        .map(|entry| {
            host_to_connect
                .get(&entry.raw)
                .map(|ip| Cow::Owned(format!("connect {ip}")))
                .unwrap_or(Cow::Borrowed("Server not found in cache"))
        })
        .collect::<Vec<_>>();
    println!("{}", DisplayHistory(history, &ips));
}

pub async fn reconnect(args: HistoryArgs, context: &mut CommandContext) -> CommandHandle {
    let cache_arc = context.cache();
    let mut cache = cache_arc.lock().await;
    if cache.connection_history.is_empty() {
        info!("No joined servers in history, connect to a server to add it to history");
        return CommandHandle::Processed;
    }
    if args.history {
        display_history(&cache.connection_history, &cache.host_to_connect);
        return CommandHandle::Processed;
    }
    if let Err(err) = context.check_h2m_connection().await {
        error!("{err}");
        println!("{ConnectionHelp}");
        return CommandHandle::Processed;
    }
    let history_len = cache.connection_history.len();
    if let Some(num) = args.connect {
        if num > 1 {
            if num as usize > history_len {
                error!("{}", DisplayHistoryErr(history_len));
                return CommandHandle::Processed;
            }
            let entry = cache.connection_history.remove(history_len - num as usize);
            cache.connection_history.push(entry);
        }
    }
    let connect = cache
        .host_to_connect
        .get(&cache.connection_history.last().unwrap().raw)
        .copied();

    drop(cache);

    if let Some(ip_port) = connect {
        let lock = context.pty_handle().unwrap();
        connect_to(ip_port, &lock)
            .await
            .unwrap_or_else(|err| error!("{err}"));
    } else {
        error!("Could not find server in cache");
        println!("use command '{YELLOW}cache{WHITE} update' to attempt to locate missing server");
    }
    CommandHandle::Processed
}

/// Before calling be sure to guard against invalid handles by checking `.check_h2m_connection().is_ok()`
async fn connect_to(ip_port: SocketAddr, lock: &RwLock<PTY>) -> Result<(), String> {
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
