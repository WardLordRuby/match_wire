pub mod commands;
pub mod models {
    pub mod cli;
    pub mod command_scheme;
    pub mod json_data;
}
pub mod utils {
    pub mod caching;
    pub mod details;
    pub mod display;
    pub mod limiter;
    pub mod request;

    /// All state is stored in TLS, it is crucial everything accessed within this file is done through
    /// the main thread. Failing to do so may result in runtime panics.
    pub mod main_thread_state;

    pub mod subscriber;
}

use crate::{
    commands::{Message, ReplHandle, StartupCacheContents},
    models::{
        cli::Command,
        json_data::{CacheFile, StartupInfo},
    },
    utils::{
        caching::{ReadCacheErr, build_cache},
        display::{self, table::TABLE_PADDING},
        main_thread_state,
        subscriber::init_subscriber,
    },
};

use std::{
    borrow::Cow,
    fmt::Display,
    io::{self, BufRead, BufReader},
    path::{Path, PathBuf},
};

use clap::CommandFactory;
use constcat::concat;
use crossterm::cursor;
use regex::Regex;
use repl_oxide::ansi_code::{RED, RESET};
use sha2::{Digest, Sha256};
use tracing::{error, info};

pub const CRATE_NAME: &str = env!("CARGO_PKG_NAME");
pub const CRATE_VER: &str = env!("CARGO_PKG_VERSION");

const MAIN_PROMPT: &str = concat!(CRATE_NAME, ".exe");
pub const LOG_ONLY: &str = "log_only";

pub const MAX_H2M_CLIENT_NUM: u8 = 18;
const MAX_H2M_TEAM_SIZE: u8 = 9;

mod files {
    pub(crate) const GAME_ENTRIES: [&str; 5] = [
        "h1_mp64_ship.exe",
        "hmw-mod.exe",
        "players2",
        "h2m-mod.exe",
        "h2m-revived.exe",
    ];

    pub(crate) const FNAME_HMW: &str = GAME_ENTRIES[1];

    #[cfg(not(debug_assertions))]
    pub(crate) use release::*;

    #[cfg(not(debug_assertions))]
    mod release {
        use super::*;

        pub(crate) const FNAME_MWR: &str = GAME_ENTRIES[0];
        pub(crate) const FNAME_H2M_1: &str = GAME_ENTRIES[3];
        pub(crate) const FNAME_H2M_2: &str = GAME_ENTRIES[4];
    }
}

#[cfg(not(debug_assertions))]
use files::*;

const LOCAL_DATA: &str = "LOCALAPPDATA";
const CACHED_DATA: &str = "cache.json";

pub mod splash_screen {
    use std::{io, thread::JoinHandle};

    #[cfg(debug_assertions)]
    pub use debug::*;
    #[cfg(not(debug_assertions))]
    pub use release::*;

    // Skip splash screen on debug builds
    #[cfg(debug_assertions)]
    mod debug {
        use super::*;

        pub fn enter() -> JoinHandle<io::Result<()>> {
            std::thread::spawn(|| Ok(()))
        }

        pub(crate) fn leave(task: JoinHandle<io::Result<()>>) {
            task.join().unwrap().unwrap();
        }
    }

    #[cfg(not(debug_assertions))]
    mod release {
        use super::*;
        use crate::main_thread_state;

        use std::time::Duration;

        use crossterm::{
            cursor, execute, queue,
            terminal::{self, BeginSynchronizedUpdate, EndSynchronizedUpdate},
        };

        pub fn enter() -> JoinHandle<io::Result<()>> {
            // font: 4Max - patorjk.com
            const SPLASH_TEXT: [&str; 4] = [
                r#"8b    d8    db    888888  dP""b8 88  88     Yb        dP 88 88""Yb 888888"#,
                r#"88b  d88   dPYb     88   dP   `" 88  88      Yb  db  dP  88 88__dP 88__  "#,
                r#"88YbdP88  dP__Yb    88   Yb      888888       YbdPYbdP   88 88"Yb  88""  "#,
                r#"88 YY 88 dP""""Yb   88    YboodP 88  88        YP  YP    88 88  Yb 888888"#,
            ];

            main_thread_state::alt_screen::enter();

            std::thread::spawn(|| {
                let mut stdout = io::stdout();

                execute!(stdout, terminal::EnterAlternateScreen)?;

                let (columns, rows) = terminal::size()?;
                let start_y = rows.saturating_sub(SPLASH_TEXT.len() as u16) / 2;
                let start_x = columns.saturating_sub(SPLASH_TEXT[0].len() as u16) / 2;

                for (i, &line) in SPLASH_TEXT.iter().enumerate() {
                    execute!(stdout, BeginSynchronizedUpdate)?;

                    queue!(
                        stdout,
                        cursor::MoveTo(start_x, start_y + i as u16),
                        crossterm::style::Print(line)
                    )?;

                    std::thread::sleep(Duration::from_millis(160));
                    execute!(stdout, EndSynchronizedUpdate)?;
                }

                std::thread::sleep(Duration::from_secs(2));

                Ok(())
            })
        }

        pub(crate) fn leave(task: JoinHandle<io::Result<()>>) {
            task.join().unwrap().unwrap();

            execute!(io::stdout(), terminal::LeaveAlternateScreen).unwrap();
            main_thread_state::alt_screen::leave();
        }
    }
}

/// Returns the working `%localappdata%` for this program
pub fn try_init_logger() -> Option<PathBuf> {
    let local_dir = std::env::var_os(LOCAL_DATA);
    let local_dir = local_dir.as_ref().map(Path::new);

    if let Some(local) = local_dir {
        match check_app_dir_exists(local) {
            Ok(app_dir_local) => {
                init_subscriber(&app_dir_local).unwrap_or_else(|err| {
                    main_thread_state::alt_screen::push_message(Message::error(err.to_string()))
                });
                info!(name: LOG_ONLY, "App startup");
                Some(app_dir_local)
            }
            Err(err) => {
                main_thread_state::alt_screen::push_message(Message::error(err.to_string()));
                None
            }
        }
    } else {
        main_thread_state::alt_screen::push_message(Message::error(
            "Could not find %appdata%/local",
        ));

        #[cfg(debug_assertions)]
        init_subscriber(Path::new("")).unwrap();

        None
    }
}

fn open_dir(path: &Path) -> io::Result<std::process::Child> {
    std::process::Command::new("explorer").arg(path).spawn()
}

#[cfg(not(debug_assertions))]
fn contains_required_files(exe_dir: &Path) -> Result<PathBuf, Cow<'static, str>> {
    use crate::utils::display::HmwDownloadHint;

    fn exists(exe_dir: &Path, file: &'static str) -> Result<bool, String> {
        exe_dir
            .join(file)
            .try_exists()
            .map_err(|err| err.to_string())
    }

    if !exists(exe_dir, FNAME_MWR)? {
        return Err(Cow::Borrowed(concat!(
            "Move ",
            CRATE_NAME,
            ".exe into your 'Call of Duty Modern Warfare Remastered' directory",
        )));
    }

    let found_game = if exists(exe_dir, FNAME_HMW)? {
        FNAME_HMW
    } else if exists(exe_dir, FNAME_H2M_1)? {
        FNAME_H2M_1
    } else if exists(exe_dir, FNAME_H2M_2)? {
        FNAME_H2M_2
    } else {
        return Err(Cow::Owned(format!("Mod exe not found, {HmwDownloadHint}")));
    };

    Ok(exe_dir.join(found_game))
}

fn hash_file_hex(path: &Path) -> io::Result<String> {
    let file = std::fs::read(path)?;
    let mut hasher = Sha256::new();
    hasher.update(&file);
    Ok(format!("{:x}", hasher.finalize()))
}

pub fn await_user_for_end<D: Display>(err: D) {
    println!("{RED}{err}{RESET}\nPress enter to exit...");
    let stdin = std::io::stdin();
    let mut reader = BufReader::new(stdin);
    let _ = reader.read_line(&mut String::new());
}

/// Validates `%localappdata%` structure and returns an owned path to this programs own directory
fn check_app_dir_exists(local: &Path) -> io::Result<PathBuf> {
    const PREV_NAME: &str = "h2m_favorites";
    let prev_local_dir = local.join(PREV_NAME);
    let curr_local_dir = local.join(CRATE_NAME);

    if !curr_local_dir.try_exists()? {
        std::fs::create_dir(&curr_local_dir)?;
    }

    if prev_local_dir
        .try_exists()
        .map_err(|err| {
            main_thread_state::alt_screen::push_message(Message::error(format!(
                "{err}, looking for {}",
                prev_local_dir.display()
            )))
        })
        .unwrap_or_default()
    {
        std::fs::remove_dir_all(prev_local_dir)?;
    }

    Ok(curr_local_dir)
}

fn make_slice_ascii_lowercase(slice: &mut [String]) {
    slice.iter_mut().for_each(|s| s.make_ascii_lowercase());
}

fn elide(str: &str, at: usize) -> Option<String> {
    let mut chars = str.char_indices();
    let i = chars
        .nth(at)
        .and_then(|(i, _)| chars.next().is_some().then_some(i))?;
    let mut elided = String::from(str[..i].trim_end());
    elided.push('…');
    Some(elided)
}

fn parse_hostname(name: &str) -> String {
    let trimmed_name = name.trim_start();
    if trimmed_name.is_empty() {
        return String::new();
    }

    fn step(c: char, chars: &mut impl Iterator<Item = char>) -> Option<char> {
        if c == COLOR_ESCAPE_CODE {
            chars.next();
            None
        } else {
            Some(c.to_ascii_lowercase())
        }
    }

    const COLOR_ESCAPE_CODE: char = '^';
    let mut chars = trimmed_name.chars();
    let mut host_name = String::new();

    if let Some(c) = step(chars.next().expect("early return"), &mut chars) {
        host_name.push(c);
    }

    let mut chars = chars.skip_while(|c| c.is_whitespace());

    while let Some(c) = chars.next() {
        if let Some(c) = step(c, &mut chars) {
            host_name.push(c);
        }
    }

    host_name
}

const ANSI_REGEX_PATTERN: &str = r"(?x)
    \x1b\[\?(?:25[hl]|47[hl]|1049[hl])      # Private modes
    |
    \x1b\][^\x07\x1b]*(?:\x07|\x1b\\)       # OSC sequences
    |
    \x1b\[[\d;]*t                           # Window manipulation
";

/// Uses a regex to strip ansi OSC sequences, window manipulation sequences, and private modes
pub fn strip_unwanted_ansi_sequences(input: &str) -> Cow<'_, str> {
    thread_local! { static ANSI_RE: Regex = Regex::new(ANSI_REGEX_PATTERN).expect("valid pattern") }
    ANSI_RE.with(|re| re.replace_all(input, ""))
}

pub fn print_help() {
    #[cfg(windows)]
    if cursor::position().unwrap() != (0, 0) {
        println!()
    }
    Command::command()
        .print_help()
        .expect("Failed to print help");
    println!();
}

async fn send_msg_over(sender: &tokio::sync::mpsc::Sender<Message>, message: Message) {
    if main_thread_state::alt_screen::is_visible() {
        main_thread_state::alt_screen::push_message(message);
    } else {
        sender
            .send(message)
            .await
            .unwrap_or_else(|returned| returned.0.log());
    }
}

pub async fn startup_cache_task(
    cache_res: Option<Result<CacheFile, ReadCacheErr>>,
) -> StartupCacheContents {
    let (cmd_history, prev_cache) = match cache_res {
        Some(Ok(mut file_contents)) => {
            let startup_contents = StartupCacheContents {
                command_history: std::mem::take(&mut file_contents.cmd_history),
                modified: false,
            };
            main_thread_state::Cache::set(file_contents.into());
            return startup_contents;
        }
        Some(Err(err)) => {
            error!("{err}");
            err.cache
                .map(|mut file_contents| {
                    (
                        Some(std::mem::take(&mut file_contents.cmd_history)),
                        Some(file_contents.into()),
                    )
                })
                .unwrap_or_default()
        }
        None => (None, None),
    };

    main_thread_state::Cache::set(prev_cache.unwrap_or_default());

    StartupCacheContents {
        command_history: cmd_history.unwrap_or_default(),
        modified: build_cache().await.map_err(display::error).is_ok(),
    }
}

/// The returned `io::Error` should be propagated as [`CmdErr::Critical`]
///
/// [`CmdErr::Critical`]: crate::commands::handler::CmdErr::Critical
fn try_fit_table(
    repl: &mut ReplHandle,
    (cols, rows): (u16, u16),
    desired: usize,
) -> io::Result<()> {
    let min_terminal_cols = desired as u16 + TABLE_PADDING;

    if cols < min_terminal_cols {
        repl.set_terminal_size((min_terminal_cols, rows))?;
    }

    Ok(())
}
