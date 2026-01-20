use crate::{
    LOG_ONLY,
    commands::{
        CommandContext, CommandErr, CommandReturn, CommandSender, HistoryTag, Message, ReplHandle,
        filter::{Request, Sourced, try_get_info},
        launch::connection::HostName,
    },
    models::cli::HistoryArgs,
    parse_hostname, send_msg_over, try_fit_table,
    utils::{
        caching::HostNameMap,
        display::{
            ConnectionHelp, DisplayHistoryErr,
            table::{DisplayHistory, TABLE_PADDING},
        },
        main_thread_state,
        request::client_with_timeout,
    },
};

use std::{borrow::Cow, io, net::SocketAddr};

use repl_oxide::ansi_code::{GREEN, RED, RESET, YELLOW};
use tokio::task::JoinHandle;
use tracing::{error, info};

pub const HISTORY_MAX: usize = 6;

/// The returned `io::Error` should be propagated as [`CmdErr::Critical`]
///
/// [`CmdErr::Critical`]: crate::commands::handler::CmdErr::Critical
fn display_history(
    repl: &mut ReplHandle,
    history: &[HostName],
    host_to_connect: &HostNameMap,
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

    let width = (max_host_len + max_connect_len + TABLE_PADDING as usize).max(32);

    try_fit_table(repl, repl.terminal_size(), width)?;

    println!("{}", DisplayHistory(&set, width));
    Ok(())
}

impl CommandContext {
    pub(super) fn reconnect(&mut self, repl: &mut ReplHandle, args: HistoryArgs) -> CommandReturn {
        if args.abort {
            return if self.try_abort_queued_con() {
                CommandReturn::processed()
            } else {
                println!("{RED}No queued connection attempt to abort{RESET}");
                CommandReturn::command_err()
            };
        }

        let ip_port = match main_thread_state::Cache::with_borrow(|cache| {
            if cache.connection_history.is_empty() {
                info!("No joined servers in history, connect to a server to add it to history");
                return Err(CommandErr::Command);
            }

            if args.history {
                display_history(repl, &cache.connection_history, &cache.host_to_connect)?;
                return Ok(None);
            }

            let target_i = if let Some(num) = args.connect.filter(|&i| i > 1).map(usize::from) {
                if num > cache.connection_history.len() {
                    error!("{}", DisplayHistoryErr(cache.connection_history.len()));
                    return Err(CommandErr::Command);
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
                    Err(CommandErr::NonCritical)
                }
            }
        }) {
            Ok(Some(addr)) => addr,
            Ok(None) => return CommandReturn::processed(),
            Err(err) => return CommandReturn::err(err),
        };

        self.try_abort_queued_con();

        // Since the history stack is always changing as players connect to servers, reconnect commands are tagged
        // non-deterministic since running the same command again will likely not connect you to the same server
        const HISTORY_OUTPUT: CommandReturn = CommandReturn::tagged(HistoryTag::Nondeterministic);

        if args.queue {
            self.init_queued_connection(ip_port);
            return HISTORY_OUTPUT;
        }

        if let Err(err) = main_thread_state::pty_handle::try_if_alive(|game_console| {
            game_console.send_connect(ip_port)
        }) {
            error!("{err}");
            if err.is_connection_err() {
                println!("{ConnectionHelp}");
            }
        }

        // Success notification and cache modification taken care of by `HostName::from_request` in launch.rs.
        // Since other parts of the process are always scanning the game console looking for direct connection
        // attempts when it is alive.

        HISTORY_OUTPUT
    }

    fn try_abort_queued_con(&mut self) -> bool {
        let Some(prev) = self
            .queued_con_task
            .take()
            .filter(|task| !task.is_finished())
        else {
            return false;
        };

        prev.abort();
        println!("{YELLOW}Previously queued connection attempt aborted{RESET}");

        true
    }

    /// [`Self::try_abort_queued_con`] must be called prior to calling this method
    fn set_queued_con(&mut self, task: JoinHandle<()>) {
        assert!(
            self.queued_con_task.replace(task).is_none(),
            "Task must be aborted prior to a new queued connection attempt is spawned"
        )
    }

    fn init_queued_connection(&mut self, addr: SocketAddr) {
        let msg_sender = self.msg_sender();

        let task = tokio::spawn(async move {
            let client = client_with_timeout(4);
            let info_endpoint = main_thread_state::Endpoints::server_info_endpoint();

            let mut hostname = None;
            let mut attempts = 1_usize;

            loop {
                let server = match try_get_info(
                    Request::New(Sourced::Hmw(addr)),
                    client.clone(),
                    info_endpoint,
                )
                .await
                {
                    Ok(server) => server.info,
                    Err(err) => {
                        error!(name: LOG_ONLY, "{err}");
                        send_msg_over(&msg_sender, Message::error("Queued server did not respond"))
                            .await;
                        break;
                    }
                };

                let player_ct = server.player_ct();
                let max_public_slots = server.max_public_slots();

                if player_ct < max_public_slots {
                    if let Err(err) = main_thread_state::pty_handle::try_if_alive(|game_console| {
                        game_console.send_connect(addr)
                    }) {
                        send_msg_over(&msg_sender, Message::error(err.to_string())).await;
                        if err.is_connection_err() {
                            send_msg_over(&msg_sender, Message::str(ConnectionHelp)).await;
                        }
                    } else if let Some(parsed) = hostname {
                        let info_msg = format!(
                            "{GREEN}Connecting to '{parsed}'! - {player_ct}/{max_public_slots} players{RESET}",
                        );
                        send_msg_over(&msg_sender, Message::str(info_msg)).await;
                    }
                    break;
                }

                if hostname.is_none() {
                    let parsed = parse_hostname(&server.host_name);
                    let info_msg = format!("{GREEN}Connection to '{parsed}' queued!{RESET}");
                    send_msg_over(&msg_sender, Message::str(info_msg)).await;
                    hostname = Some(parsed);
                }

                if attempts.is_multiple_of(5) {
                    let info_msg = format!(
                        "{YELLOW}'{}' at capacity - {player_ct}/{max_public_slots} players",
                        hostname.as_deref().unwrap()
                    );
                    send_msg_over(&msg_sender, Message::str(info_msg)).await;
                }

                tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;
                attempts += 1;
            }
        });

        self.set_queued_con(task);
    }
}
