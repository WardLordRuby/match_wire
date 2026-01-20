pub mod connection;
pub(crate) mod ffi;

use connection::{Connection, add_to_history};
use ffi::{WinApiErr, game_open};

use crate::{
    commands::{CommandContext, Message},
    send_msg_over, strip_unwanted_ansi_sequences,
    utils::{
        display::ConnectionHelp,
        main_thread_state::{self, ThreadCopyState, pty_handle::PseudoConStatus},
    },
};

use std::{
    ffi::OsString,
    os::windows::ffi::{OsStrExt, OsStringExt},
    path::{Path, PathBuf},
};

use core::str;
use repl_oxide::ansi_code::{RED, RESET};
use tracing::error;

use winptyrs::{AgentConfig, MouseMode, PTY, PTYArgs, PTYBackend};

macro_rules! uppercase_pairs {
    ($($c:literal),* $(,)?) => {
        [$(($c, $c.to_ascii_uppercase())),*]
    };
}

const CMP_LEN: usize = 8;

const JOINING_STR: &str = "Joining ";
const CONNECTING_STR: &str = "Connecti";
const CONNECT_STR: [(char, char); CMP_LEN] =
    uppercase_pairs!['c', 'o', 'n', 'n', 'e', 'c', 't', ' '];
const ESCAPE_CHAR: char = '\x1b';
const COLOR_CMD: char = 'm';
const CARRIAGE_RETURN: u16 = '\r' as u16;
const NEW_LINE: u16 = '\n' as u16;

const _: () = assert!(JOINING_STR.len() == CMP_LEN);
const _: () = assert!(CONNECTING_STR.len() == CMP_LEN);
pub enum LaunchError {
    SpawnErr(OsString),
    WinApiErr(WinApiErr),
}

pub async fn try_init_launch(
    exe_path: PathBuf,
    no_launch: bool,
) -> Option<Result<PTY, LaunchError>> {
    if no_launch {
        return None;
    }

    let game_open = match game_open() {
        Ok(open_status) => open_status,
        Err(err) => {
            error!("{err}, could not determine if it is okay to launch");
            return None;
        }
    };

    if let Some(window_name) = game_open {
        main_thread_state::alt_screen::push_message(Message::str(format!(
            "{RED}{window_name} is already running{RESET}\n{ConnectionHelp}"
        )));
    }

    // delay h2m doesn't block splash screen
    tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
    Some(spawn_pseudo(&exe_path))
}

/// **IMPORTANT**: caller **MUST** ensure game is not open prior to this call
pub(crate) fn spawn_pseudo(game_path: &Path) -> Result<PTY, LaunchError> {
    let pty_args = PTYArgs {
        cols: 250,
        rows: 50,
        mouse_mode: MouseMode::WINPTY_MOUSE_MODE_NONE,
        timeout: 25000,
        agent_config: AgentConfig::WINPTY_FLAG_PLAIN_OUTPUT,
    };

    let mut conpty =
        PTY::new_with_backend(&pty_args, PTYBackend::ConPTY).map_err(LaunchError::SpawnErr)?;

    conpty
        .spawn(game_path.into(), None, None, None)
        .map_err(LaunchError::SpawnErr)?;

    Ok(conpty)
}

#[inline]
fn case_insensitive_cmp_direct(line: &str) -> Option<Connection> {
    let mut chars = line
        .char_indices()
        .skip_while(|(_, ch)| !ch.is_alphabetic());

    for ((lower, upper), (_, ch)) in CONNECT_STR.into_iter().zip(chars.by_ref()) {
        if ch != lower && ch != upper {
            return None;
        }
    }

    chars.next().map(|(i, _)| Connection::Direct(i))
}

impl CommandContext {
    pub fn init_listener(&self) -> Result<(), String> {
        let game_name = self.game_name_owned();

        let PseudoConStatus::Attached =
            main_thread_state::pty_handle::check_connection().map_err(|err| err.to_string())?
        else {
            return Err(format!("No connection to {game_name} console"));
        };

        let msg_sender = self.msg_sender();
        let game_state_change = self.game_state_change();
        let version = self.game_version().unwrap_or(1.0);

        tokio::spawn(async move {
            let mut buffer = OsString::new();
            let connecting_str = if version < 1.0 { JOINING_STR } else { CONNECTING_STR };

            /// 16 KB
            const BUFFER_SIZE: u32 = 16384;
            const PROCESS_INTERVAL: std::time::Duration = std::time::Duration::from_secs(3);

            tokio::time::sleep(tokio::time::Duration::from_secs(10)).await;
            'task: loop {
                tokio::time::sleep(PROCESS_INTERVAL).await;

                let start_time = tokio::time::Instant::now();

                while start_time.elapsed() < PROCESS_INTERVAL {
                    match main_thread_state::pty_handle::try_if_alive(|console_handle| {
                        console_handle
                            .read(BUFFER_SIZE, false)
                            .map(|os_string| {
                                if os_string.is_empty() {
                                    return true;
                                }
                                buffer.push(os_string);
                                false
                            })
                            .map_err(|err| err.to_string_lossy().to_string())
                    }) {
                        Ok(true) => break,
                        Ok(false) => tokio::task::yield_now().await,
                        Err(err) => {
                            send_msg_over(&msg_sender, Message::error(err)).await;
                            break 'task;
                        }
                    }
                }

                if buffer.is_empty() {
                    continue;
                }

                let mut wide_encode_buf = Vec::new();

                'byte_iter: for byte in buffer.encode_wide() {
                    if byte != CARRIAGE_RETURN && byte != NEW_LINE {
                        wide_encode_buf.push(byte);
                        continue;
                    }

                    if wide_encode_buf.is_empty() {
                        continue;
                    }

                    let cur = String::from_utf16_lossy(&wide_encode_buf);
                    let line = strip_unwanted_ansi_sequences(&cur);

                    if !line.is_empty() {
                        // don't store lines that that _only_ contain ansi escape commands,
                        // unless a color command is found then append it to the next line
                        let mut chars = line.char_indices().peekable();
                        let mut color_cmd = None;
                        while let Some((_, ESCAPE_CHAR)) = chars.peek() {
                            let (i, _) = chars.next().expect("Escape char found");
                            if let Some((j, c)) = chars.find(|(_, c)| c.is_alphabetic())
                                && c == COLOR_CMD
                            {
                                color_cmd = Some(&line[i..=j]);
                            }
                            if chars.peek().is_none() {
                                if let Some(cmd) = color_cmd {
                                    if line != cmd {
                                        // line must contain multiple ansi escape commands, only add the color cmd to the next line
                                        wide_encode_buf = cmd.encode_utf16().collect();
                                    }
                                } else {
                                    wide_encode_buf.clear();
                                }
                                continue 'byte_iter;
                            }
                        }

                        let mut trimmed = chars.next().map(|(i, _)| &line[i..]).expect(
                            "must be `Some` otherwise we would have continued `'byte_iter` & `line` must not be empty",
                        );

                        let connection_ln = if let Some(rest) = trimmed.strip_prefix(']') {
                            trimmed = rest;
                            case_insensitive_cmp_direct(trimmed)
                        } else {
                            trimmed
                                .contains(connecting_str)
                                .then_some(Connection::Browser)
                        };

                        if let Some(connect_kind) = connection_ln {
                            add_to_history(&msg_sender, trimmed, connect_kind, version).await;
                        }

                        main_thread_state::ConsoleHistory::push(line.into_owned());
                    }

                    wide_encode_buf.clear();
                }

                if let Some(msg_concat) = main_thread_state::ConsoleHistory::concat_new() {
                    if msg_sender.send(Message::str(msg_concat)).await.is_err() {
                        main_thread_state::ForwardLogs::set(false);
                    } else {
                        main_thread_state::ConsoleHistory::with_borrow_mut(|history| {
                            history.displayed()
                        });
                    }
                }

                buffer = OsString::from_wide(&wide_encode_buf);
            }
            send_msg_over(
                &msg_sender,
                Message::warn(format!("No longer reading {game_name} console output")),
            )
            .await;
            game_state_change.notify_one();
        });

        Ok(())
    }
}
