use crate::{
    commands::{
        handler::{CommandContext, CommandHandle, CommandSender},
        launch_h2m::HostName,
    },
    models::cli::HistoryArgs,
    utils::display::{ConnectionHelp, DisplayHistoryErr},
    LOG_ONLY,
};

use std::{
    borrow::Cow, collections::HashMap, fmt::Display, net::SocketAddr, sync::atomic::Ordering,
};

use repl_oxide::ansi_code::{RESET, YELLOW};
use tokio::sync::RwLock;
use tracing::{error, info};
use winptyrs::PTY;

pub const HISTORY_MAX: usize = 6;

struct DisplayHistory<'a>(&'a [HostName], &'a [Cow<'static, str>]);

impl Display for DisplayHistory<'_> {
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

impl CommandContext {
    pub(crate) async fn reconnect(&mut self, args: HistoryArgs) -> std::io::Result<CommandHandle> {
        let cache_arc = self.cache();
        let mut cache = cache_arc.lock().await;
        if cache.connection_history.is_empty() {
            info!("No joined servers in history, connect to a server to add it to history");
            return Ok(CommandHandle::Processed);
        }
        if args.history {
            display_history(&cache.connection_history, &cache.host_to_connect);
            return Ok(CommandHandle::Processed);
        }
        let lock = match self.check_h2m_connection().await {
            Ok(lock) => lock,
            Err(err) => {
                error!("{err}");
                println!("{ConnectionHelp}");
                return Ok(CommandHandle::Processed);
            }
        };

        let mut target = cache.connection_history.len() - 1;
        let mut modify_cache = false;
        if let Some(num) = args.connect.map(|n| n as usize) {
            if num > 1 {
                if num > cache.connection_history.len() {
                    error!("{}", DisplayHistoryErr(cache.connection_history.len()));
                    return Ok(CommandHandle::Processed);
                }
                (target, modify_cache) = (cache.connection_history.len() - num, true);
            }
        }
        let Some(&ip_port) = cache
            .host_to_connect
            .get(&cache.connection_history[target].raw)
        else {
            error!("Could not find server in cache");
            println!(
                "use command '{YELLOW}cache{RESET} update' to attempt to locate missing server"
            );
            return Ok(CommandHandle::Processed);
        };

        if let Err(err) = connect_to(ip_port, &lock).await {
            error!("{err}");
            return Ok(CommandHandle::Processed);
        }

        info!(name: LOG_ONLY, "Connected to {ip_port}");

        if modify_cache {
            let entry = cache.connection_history.remove(target);
            cache.connection_history.push(entry);
            self.cache_needs_update().store(true, Ordering::SeqCst);
        }
        Ok(CommandHandle::Processed)
    }
}

/// Before calling be sure to guard against invalid handles by checking `.check_h2m_connection().is_ok()`
async fn connect_to(ip_port: SocketAddr, lock: &RwLock<PTY>) -> Result<(), Cow<'_, str>> {
    let game_console = lock.read().await;
    game_console.send_cmd("disconnect")?;
    tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;
    game_console.send_cmd(format!("connect {ip_port}"))
}
