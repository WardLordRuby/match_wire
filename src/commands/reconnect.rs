use crate::{
    commands::{
        handler::{CmdErr, CommandContext, CommandHandle, CommandSender, ReplHandle},
        launch_h2m::HostName,
    },
    models::cli::HistoryArgs,
    try_fit_table,
    utils::{
        display::{BoxBottom, BoxTop, ConnectionHelp, DisplayHistoryErr, Line, Space},
        global_state::{self, PtyAccessErr},
    },
};

use std::{borrow::Cow, collections::HashMap, fmt::Display, io, net::SocketAddr};

use repl_oxide::ansi_code::{RESET, YELLOW};
use tracing::{error, info};

pub const HISTORY_MAX: usize = 6;

struct DisplayHistory<'a>(&'a [(&'a str, usize, usize, &'a str)], usize);

impl Display for DisplayHistory<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let width = self.1;
        let interior_width = width - 4;

        writeln!(f)?;
        writeln!(f, " {}", BoxTop(Some("History"), width))?;
        writeln!(f, " │ Server Name{}Connection Command │", Space(width - 31))?;
        writeln!(f, " │ {} │", Line(width - 2))?;
        for (i, (host_name, host_len, ip_len, ip)) in self.0.iter().copied().enumerate() {
            let spacing = interior_width - host_len - ip_len;
            writeln!(f, " │ {}.{host_name}{}{ip} │", i + 1, Space(spacing))?;
        }
        writeln!(f, " {}", BoxBottom(width))
    }
}

fn display_history(
    repl: &mut ReplHandle,
    history: &[HostName],
    host_to_connect: &HashMap<String, SocketAddr>,
) -> io::Result<()> {
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

    let (set, max_host_len, max_connect_len) =
        history.iter().rev().zip(ips.iter().map(Cow::as_ref)).fold(
            (Vec::with_capacity(HISTORY_MAX), 0, 0),
            |(mut out, max_host_len, max_ip_len), (host, connect_ip)| {
                let ip_len = connect_ip.chars().count();
                let host_len = host.parsed.chars().count();
                out.push((host.parsed.as_str(), host_len, ip_len, connect_ip));
                (out, max_host_len.max(host_len), max_ip_len.max(ip_len))
            },
        );

    let width = (max_host_len + max_connect_len + 6).max(32);

    try_fit_table(repl, repl.terminal_size(), width)?;

    println!("{}", DisplayHistory(&set, width));
    Ok(())
}

impl CommandContext {
    pub(crate) fn reconnect(
        &mut self,
        repl: &mut ReplHandle,
        args: HistoryArgs,
    ) -> io::Result<CommandHandle> {
        let ip_port = match global_state::Cache::with_borrow(|cache| {
            if cache.connection_history.is_empty() {
                info!("No joined servers in history, connect to a server to add it to history");
                return Err(CmdErr::Command);
            }

            if args.history {
                display_history(repl, &cache.connection_history, &cache.host_to_connect)?;
                return Ok(None);
            }

            let target_i = if let Some(num) = args.connect.filter(|&i| i > 1).map(usize::from) {
                if num > cache.connection_history.len() {
                    error!("{}", DisplayHistoryErr(cache.connection_history.len()));
                    return Err(CmdErr::Command);
                }
                cache.connection_history.len() - num
            } else {
                cache.connection_history.len() - 1
            };

            match cache
                .host_to_connect
                .get(&cache.connection_history[target_i].raw)
            {
                Some(&addr) => Ok(Some(addr)),
                None => {
                    error!("Could not find server in cache");
                    println!(
                        "use command '{YELLOW}cache{RESET} update' to attempt to locate missing server"
                    );
                    Err(CmdErr::Command)
                }
            }
        }) {
            Ok(Some(addr)) => addr,
            Ok(None) | Err(CmdErr::Command) => return Ok(CommandHandle::Processed),
            Err(CmdErr::Critical(err)) => return Err(err),
        };

        if let Err(err) =
            global_state::PtyHandle::try_if_alive(|game_console| game_console.send_connect(ip_port))
        {
            error!("{err}");
            if let PtyAccessErr::ConnectionErr(_) = err {
                println!("{ConnectionHelp}");
            }
        }

        // Success notification and cache modification taken care of by `HostName::from_request` in launch_h2m.rs.
        // Since other parts of the process is always scanning the game console when active looking for direct
        // connection attempts.

        Ok(CommandHandle::Processed)
    }
}
