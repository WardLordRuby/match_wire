pub mod commands {
    pub mod filter;
    pub mod handler;
    pub mod launch_h2m;
    pub mod reconnect;
}
pub mod models {
    pub mod cli;
    pub mod command_scheme;
    pub mod json_data;
}
pub mod utils {
    pub mod caching;
    pub mod display;
    pub mod global_state;
    pub mod subscriber;
}

use crate::{
    commands::{
        handler::{Message, ReplHandle, StartupCacheContents},
        launch_h2m::get_exe_version,
    },
    models::{
        cli::Command,
        json_data::{CacheFile, HmwManifest, StartupInfo},
    },
    utils::{
        caching::{ReadCacheErr, build_cache, read_cache},
        display::{self, table::TABLE_PADDING},
        global_state,
        subscriber::init_subscriber,
    },
};

use std::{
    borrow::Cow,
    io::{self, BufRead, BufReader},
    path::{Path, PathBuf},
    time::Duration,
};

use clap::CommandFactory;
use constcat::concat;
use crossterm::cursor;
use repl_oxide::ansi_code::{RED, RESET};
use reqwest::Client;
use sha2::{Digest, Sha256};
use tracing::{error, info};

pub(crate) const STATUS_OK: reqwest::StatusCode = reqwest::StatusCode::OK;

pub(crate) const CONSEC_CMD_DELAY: Duration = Duration::from_millis(15);

pub const CRATE_NAME: &str = env!("CARGO_PKG_NAME");
pub const CRATE_VER: &str = env!("CARGO_PKG_VERSION");

pub(crate) const MAIN_PROMPT: &str = concat!(CRATE_NAME, ".exe");
pub const LOG_ONLY: &str = "log_only";

const MOD_FILES_MODULE_NAME: &str = "mod";
// const MOD_FILES_LATEST_VER: &str = "1.1";

pub const H2M_MAX_CLIENT_NUM: i64 = 18;
pub(crate) const H2M_MAX_TEAM_SIZE: i64 = 9;

pub const SAVED_HISTORY_CAP: usize = 20;

pub(crate) mod files {
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

pub const LOCAL_DATA: &str = "LOCALAPPDATA";
pub(crate) const CACHED_DATA: &str = "cache.json";

pub mod splash_screen {
    use crate::commands::handler::Message;
    use std::io;

    #[cfg(not(debug_assertions))]
    use crossterm::{
        cursor, execute, queue,
        terminal::{self, BeginSynchronizedUpdate, EndSynchronizedUpdate},
    };

    #[cfg(not(debug_assertions))]
    pub(crate) use release::*;

    #[cfg(not(debug_assertions))]
    mod release {

        use super::*;

        use std::cell::{Cell, RefCell};

        use tracing_subscriber::fmt::format::Writer;

        thread_local! {
            pub(super) static SPLASH_SCREEN_VIS: Cell<bool> = const { Cell::new(false) };
            pub(super) static SPLASH_SCREEN_EVENT_BUFFER:
                RefCell<SplashScreenEvents> = RefCell::new(SplashScreenEvents::default());
        }

        #[derive(Default)]
        pub(super) struct SplashScreenEvents {
            pub(super) pre_subscriber: Vec<Message>,
            pub(super) from_subscriber: String,
        }

        pub(crate) fn is_visible() -> bool {
            SPLASH_SCREEN_VIS.get()
        }

        pub(crate) fn push_formatter<F>(print: F) -> std::fmt::Result
        where
            F: FnOnce(Writer<'_>) -> std::fmt::Result,
        {
            SPLASH_SCREEN_EVENT_BUFFER
                .with_borrow_mut(|buf| print(Writer::new(&mut buf.from_subscriber)))
        }
    }

    /// **Only** use for errors encountered before tracing subscriber has been initialized otherwise prefer a tracing
    /// event as our subscriber format layer takes care of fn call behind the scenes
    pub fn push_message(message: Message) {
        #[cfg(debug_assertions)]
        message.record();

        #[cfg(not(debug_assertions))]
        {
            if !SPLASH_SCREEN_VIS.get() {
                return message.record();
            }

            SPLASH_SCREEN_EVENT_BUFFER.with_borrow_mut(|buf| buf.pre_subscriber.push(message))
        }
    }

    pub async fn enter() -> io::Result<()> {
        #[cfg(not(debug_assertions))]
        {
            // font: 4Max - patorjk.com
            const SPLASH_TEXT: [&str; 4] = [
                r#"8b    d8    db    888888  dP""b8 88  88     Yb        dP 88 88""Yb 888888"#,
                r#"88b  d88   dPYb     88   dP   `" 88  88      Yb  db  dP  88 88__dP 88__  "#,
                r#"88YbdP88  dP__Yb    88   Yb      888888       YbdPYbdP   88 88"Yb  88""  "#,
                r#"88 YY 88 dP""""Yb   88    YboodP 88  88        YP  YP    88 88  Yb 888888"#,
            ];

            let mut stdout = std::io::stdout();

            SPLASH_SCREEN_VIS.set(true);
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

                tokio::time::sleep(std::time::Duration::from_millis(160)).await;
                execute!(stdout, EndSynchronizedUpdate)?;
            }

            tokio::time::sleep(std::time::Duration::from_secs(2)).await;
        }

        Ok(())
    }

    pub(crate) async fn leave(task: tokio::task::JoinHandle<io::Result<()>>) {
        task.await.unwrap().unwrap();

        #[cfg(not(debug_assertions))]
        {
            execute!(std::io::stdout(), terminal::LeaveAlternateScreen).unwrap();
            SPLASH_SCREEN_VIS.set(false);

            let events = SPLASH_SCREEN_EVENT_BUFFER.take();

            events
                .pre_subscriber
                .into_iter()
                .for_each(|message| message.record());

            print!("{}", events.from_subscriber)
        }
    }
}

#[derive(Debug)]
pub enum ResponseErr {
    Reqwest(reqwest::Error),
    Status(&'static str, reqwest::StatusCode),
    Other(Cow<'static, str>),
}

impl ResponseErr {
    fn bad_status(ctx: &'static str, response: reqwest::Response) -> Self {
        Self::Status(ctx, response.status())
    }
    #[allow(dead_code)]
    fn other<T: Into<Cow<'static, str>>>(msg: T) -> Self {
        Self::Other(msg.into())
    }
}

impl From<reqwest::Error> for ResponseErr {
    fn from(err: reqwest::Error) -> Self {
        Self::Reqwest(err)
    }
}

pub type LoggerRes = (Option<PathBuf>, Option<Result<CacheFile, ReadCacheErr>>);

pub fn try_init_logger() -> LoggerRes {
    let mut local_dir = std::env::var_os(LOCAL_DATA).map(PathBuf::from);
    let mut cache_res = None;
    if let Some(ref mut dir) = local_dir {
        if let Err(err) = check_app_dir_exists(dir) {
            splash_screen::push_message(Message::error(err.to_string()));
        } else {
            init_subscriber(dir)
                .unwrap_or_else(|err| splash_screen::push_message(Message::error(err.to_string())));
            info!(name: LOG_ONLY, "App startup");
            cache_res = Some(read_cache(dir));
        }
    } else {
        splash_screen::push_message(Message::error("Could not find %appdata%/local"));

        #[cfg(debug_assertions)]
        init_subscriber(std::path::Path::new("")).unwrap();
    }

    (local_dir, cache_res)
}

pub(crate) fn client_with_timeout(secs: u64) -> Client {
    Client::builder().timeout(Duration::from_secs(secs)).build().expect(
        "TLS backend cannot be initialized, or the resolver cannot load the system configuration",
    )
}

pub async fn get_latest_hmw_manifest() -> Result<HmwManifest, ResponseErr> {
    let client = client_with_timeout(6);
    let response = client
        .get(global_state::Endpoints::hmw_manifest())
        .send()
        .await?;

    if response.status() != STATUS_OK {
        return Err(ResponseErr::bad_status("HMW manifest", response));
    }

    response.json::<HmwManifest>().await.map_err(Into::into)
}

pub(crate) fn open_dir(path: &Path) {
    if let Err(err) = std::process::Command::new("explorer").arg(path).spawn() {
        error!("{err}")
    }
}

#[cfg(not(debug_assertions))]
fn contains_required_files(exe_dir: &Path) -> Result<PathBuf, Cow<'static, str>> {
    use crate::utils::display::HmwDownloadHint;

    if !exe_dir.join(FNAME_MWR).exists() {
        return Err(Cow::Borrowed(concat!(
            "Move ",
            CRATE_NAME,
            ".exe into your 'Call of Duty Modern Warfare Remastered' directory",
        )));
    }

    let found_game = if exe_dir.join(FNAME_HMW).exists() {
        FNAME_HMW
    } else if exe_dir.join(FNAME_H2M_1).exists() {
        FNAME_H2M_1
    } else if exe_dir.join(FNAME_H2M_2).exists() {
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

fn exe_details(game_exe_path: &Path) -> (Option<f64>, Option<String>) {
    let version = get_exe_version(game_exe_path).or_else(|| {
        println!(
            "{RED}Failed to get version of {}{RESET}",
            game_exe_path
                .file_name()
                .expect("input was not modified")
                .to_string_lossy()
        );
        None
    });
    let hash = hash_file_hex(game_exe_path)
        .map_err(|err| {
            println!(
                "{RED}{err}, input file_name: {}{RESET}",
                game_exe_path
                    .file_name()
                    .expect("input was not modified")
                    .to_string_lossy()
            )
        })
        .ok();
    (version, hash)
}

pub fn await_user_for_end() {
    println!("Press enter to exit...");
    let stdin = std::io::stdin();
    let mut reader = BufReader::new(stdin);
    let _ = reader.read_line(&mut String::new());
}

/// Validates local/app_dir exists and modifies input if valid
fn check_app_dir_exists(local: &mut PathBuf) -> io::Result<()> {
    const PREV_NAME: &str = "h2m_favorites";
    let prev_local_dir = local.join(PREV_NAME);

    local.push(CRATE_NAME);
    if !local.exists() {
        std::fs::create_dir(local)?;
    }

    if prev_local_dir.exists() {
        std::fs::remove_dir_all(prev_local_dir)?;
    }
    Ok(())
}

pub(crate) fn make_slice_ascii_lowercase(vec: &mut [String]) {
    vec.iter_mut().for_each(|s| s.make_ascii_lowercase());
}

pub(crate) fn elide(str: &str, at: usize) -> Option<String> {
    let mut chars = str.char_indices();
    let i = chars
        .nth(at)
        .and_then(|(i, _)| chars.next().is_some().then_some(i))?;
    let mut elided = String::from(str[..i].trim_end());
    elided.push('…');
    Some(elided)
}

pub(crate) fn parse_hostname(name: &str) -> String {
    let trimmed_name = name.trim_start();
    if trimmed_name.is_empty() {
        return String::new();
    }

    fn step(c: char, chars: &mut std::iter::Peekable<std::str::Chars<'_>>) -> Option<char> {
        if c == COLOR_ESCAPE_CODE {
            chars.next();
            None
        } else {
            Some(c.to_ascii_lowercase())
        }
    }

    const COLOR_ESCAPE_CODE: char = '^';
    let mut chars = trimmed_name.chars().peekable();
    let mut host_name = String::new();

    if let Some(c) = step(chars.next().expect("early return"), &mut chars) {
        host_name.push(c);
    }

    while chars.peek().copied().is_some_and(char::is_whitespace) {
        chars.next();
    }

    while let Some(c) = chars.next() {
        if let Some(c) = step(c, &mut chars) {
            host_name.push(c);
        }
    }

    host_name
}

pub fn strip_ansi_private_modes(input: &str) -> Cow<'_, str> {
    let re = regex::Regex::new(r"\x1b\[\?(?:25[hl]|47[hl]|1049[hl])").unwrap();
    re.replace_all(input, "")
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

pub(crate) async fn send_msg_over(sender: &tokio::sync::mpsc::Sender<Message>, message: Message) {
    sender
        .send(message)
        .await
        .unwrap_or_else(|returned| returned.0.log());
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
            global_state::Cache::set(file_contents.into());
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
                .unwrap_or((None, None))
        }
        None => (None, None),
    };

    global_state::Cache::set(prev_cache.unwrap_or_default());

    StartupCacheContents {
        command_history: cmd_history.unwrap_or_default(),
        modified: build_cache().await.map_err(display::error).is_ok(),
    }
}

/// The returned `io::Error` should be propagated as [`CmdErr::Critical`]
///
/// [`CmdErr::Critical`]: crate::commands::handler::CmdErr::Critical
pub(crate) fn try_fit_table(
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
