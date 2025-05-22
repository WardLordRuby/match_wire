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
        display::{self, TABLE_PADDING},
        global_state,
        subscriber::init_subscriber,
    },
};

use std::{
    borrow::Cow,
    collections::HashSet,
    io::{self, BufRead, BufReader, Read, Write},
    path::{Path, PathBuf},
    sync::mpsc::{self, TryRecvError},
    time::Duration,
};

use clap::CommandFactory;
use constcat::concat;
use crossterm::cursor;
use repl_oxide::ansi_code::{CLEAR_LINE, RED, RESET};
use reqwest::Client;
use serde_json::{Value, from_value};
use serde_json_path::JsonPath;
use sha2::{Digest, Sha256};
use tracing::{error, info};

pub(crate) const STATUS_OK: reqwest::StatusCode = reqwest::StatusCode::OK;

pub(crate) const CONSEC_CMD_DELAY: Duration = Duration::from_millis(15);

pub const CRATE_NAME: &str = env!("CARGO_PKG_NAME");
pub const CRATE_VER: &str = env!("CARGO_PKG_VERSION");

pub(crate) const MAIN_PROMPT: &str = concat!(CRATE_NAME, ".exe");
pub const LOG_ONLY: &str = "log_only";

const MOD_FILES_MODULE_NAME: &str = "mod";
const MOD_FILES_LATEST_VER: &str = "1.1";

pub(crate) const H2M_MAX_CLIENT_NUM: i64 = 18;
pub(crate) const H2M_MAX_TEAM_SIZE: i64 = 9;

pub const SAVED_HISTORY_CAP: usize = 20;

pub(crate) mod files {
    pub(crate) const GAME_ENTRIES: [&str; 7] = [
        "h1_mp64_ship.exe",
        "h2m-mod",
        "players2",
        "h2m-mod.exe",
        "h2m-revived.exe",
        "hmw-mod",
        "hmw-mod.exe",
    ];

    pub(crate) const FNAME_HMW: &str = GAME_ENTRIES[6];

    #[cfg(not(debug_assertions))]
    pub(crate) use release::*;

    #[cfg(not(debug_assertions))]
    mod release {
        use super::*;

        pub(crate) const FNAME_MWR: &str = GAME_ENTRIES[0];
        pub(crate) const DIR_NAME_HMW: &str = GAME_ENTRIES[5];
        pub(crate) const FNAME_H2M_1: &str = GAME_ENTRIES[3];
        pub(crate) const FNAME_H2M_2: &str = GAME_ENTRIES[4];
        pub(crate) const DIR_NAME_H2M: &str = GAME_ENTRIES[1];
    }
}

use files::*;

pub const LOCAL_DATA: &str = "LOCALAPPDATA";
pub(crate) const CACHED_DATA: &str = "cache.json";

#[macro_export]
macro_rules! new_io_error {
    ($kind:expr, $msg:expr) => {
        Err(std::io::Error::new($kind, $msg))
    };
}

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
            pub(super) static SPLASH_SCREEN_VIS: Cell<bool> = const { Cell::new(true) };
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
    Status(reqwest::StatusCode),
    Other(Cow<'static, str>),
}

impl ResponseErr {
    fn bad_status(response: reqwest::Response) -> Self {
        Self::Status(response.status())
    }
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

fn try_query_json_path(value: &Value, path: &str) -> Result<String, String> {
    let json_query = JsonPath::parse(path)
        .map_err(|err| format!("Failed to parse JSONPath string: {path}, {err}"))?;
    let value = json_query
        .query(value)
        .exactly_one()
        .map_err(|err| format!("Failed to query hmw_manifest: {err}"))?;
    match value {
        Value::String(hash) => Ok(hash.clone()),
        v => Err(format!(
            "Incorrect JSONPath: {path}, expected `String` got, {v:?}"
        )),
    }
}

pub async fn get_latest_hmw_hash() -> Result<String, ResponseErr> {
    let client = client_with_timeout(6);
    let response = client
        .get(global_state::Endpoints::hmw_manifest())
        .send()
        .await?;

    if response.status() != STATUS_OK {
        return Err(ResponseErr::bad_status(response));
    }

    let latest = response.json::<Value>().await?;

    if let Some(json_path) = global_state::Endpoints::manifest_hash_path() {
        match try_query_json_path(&latest, json_path) {
            Ok(hash) => return Ok(hash),
            Err(err) => splash_screen::push_message(Message::error(err)),
        }
    }

    let mut hmw_manifest = match from_value::<HmwManifest>(latest) {
        Ok(manifest) => manifest,
        Err(_) => {
            return Err(ResponseErr::other(
                "hmw manifest.json formatting has changed",
            ));
        }
    };

    hmw_manifest
        .modules
        .iter_mut()
        .find(|module| {
            module.name == MOD_FILES_MODULE_NAME && module.version == MOD_FILES_LATEST_VER
        })
        .and_then(|module| module.files_with_hashes.remove(FNAME_HMW))
        .ok_or(ResponseErr::other(
            "hmw manifest.json formatting has changed",
        ))
}

pub(crate) fn open_dir(path: &Path) {
    if let Err(err) = std::process::Command::new("explorer").arg(path).spawn() {
        error!("{err}")
    }
}

#[allow(dead_code)]
pub(crate) enum Operation {
    All,
    Any,
    Count,
}

pub(crate) enum OperationResult<'a> {
    Bool(bool),
    Count((usize, HashSet<&'a str>)),
}

/// `Operation::All` and `Operation::Any` map to `OperationResult::bool(_result_)`  
/// `Operation::Count` maps to `OperationResult::Count((_num_found_, _HashSet<_&input_list_>))`  
/// when matching you will always have to `_ => unreachable()` for the return type you will never get
pub(crate) fn does_dir_contain<'a, T>(
    dir: &Path,
    operation: Operation,
    list: &'a [T],
) -> io::Result<OperationResult<'a>>
where
    T: std::borrow::Borrow<str> + std::cmp::Eq + std::hash::Hash,
{
    let entries = std::fs::read_dir(dir)?;
    let file_names = entries
        .filter_map(|entry| Some(entry.ok()?.file_name()))
        .collect::<Vec<_>>();
    let str_names = file_names
        .iter()
        .filter_map(|f| f.to_str())
        .collect::<HashSet<_>>();

    match operation {
        Operation::All => Ok(OperationResult::Bool({
            list.iter()
                .all(|check_file| str_names.contains(check_file.borrow()))
        })),
        Operation::Any => Ok(OperationResult::Bool({
            list.iter()
                .any(|check_file| str_names.contains(check_file.borrow()))
        })),
        Operation::Count => Ok(OperationResult::Count({
            let collection = list
                .iter()
                .filter(|&check_file| str_names.contains(check_file.borrow()))
                .map(|t| t.borrow())
                .collect::<HashSet<_>>();
            let num_found = collection.len();
            (num_found, collection)
        })),
    }
}

#[cfg(not(debug_assertions))]
fn contains_required_files(exe_dir: &Path) -> Result<PathBuf, Cow<'static, str>> {
    use crate::utils::display::HmwDownloadHint;

    match does_dir_contain(exe_dir, Operation::Count, &GAME_ENTRIES)
        .expect("Failed to read contents of current dir")
    {
        OperationResult::Count((_, files)) => {
            if !files.contains(FNAME_MWR) {
                return Err(Cow::Borrowed(concat!(
                    "Move ",
                    CRATE_NAME,
                    ".exe into your 'Call of Duty Modern Warfare Remastered' directory",
                )));
            }
            if !files.contains(DIR_NAME_HMW) && !files.contains(DIR_NAME_H2M) {
                return Err(Cow::Owned(format!(
                    "Mw2 Remastered mod files not found, {HmwDownloadHint}",
                )));
            }
            let found_game = if files.contains(FNAME_HMW) {
                FNAME_HMW
            } else if files.contains(FNAME_H2M_1) {
                FNAME_H2M_1
            } else if files.contains(FNAME_H2M_2) {
                FNAME_H2M_2
            } else {
                return Err(Cow::Owned(format!("Mod exe not found, {HmwDownloadHint}",)));
            };
            Ok(exe_dir.join(found_game))
        }
        _ => unreachable!(),
    }
}

fn hash_file_hex(path: &Path) -> io::Result<String> {
    let file = std::fs::File::open(path)?;
    let mut reader = BufReader::new(file);
    let mut hasher = Sha256::new();
    let mut buffer = [0; 8192];

    loop {
        let bytes_read = reader.read(&mut buffer)?;
        if bytes_read == 0 {
            break;
        }
        hasher.update(&buffer[..bytes_read]);
    }

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
    let local_dir = local.clone();

    match does_dir_contain(local, Operation::Count, &[CRATE_NAME, PREV_NAME]) {
        Ok(OperationResult::Count((_, files))) => {
            local.push(CRATE_NAME);

            if !files.contains(CRATE_NAME) {
                std::fs::create_dir(&local)?;
            }

            if files.contains(PREV_NAME) {
                std::fs::remove_dir_all(local_dir.join(PREV_NAME))?;
            }
            Ok(())
        }
        Err(err) => Err(err),
        _ => unreachable!(),
    }
}

pub(crate) fn make_slice_ascii_lowercase(vec: &mut [String]) {
    vec.iter_mut().for_each(|s| s.make_ascii_lowercase());
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

pub(crate) struct Spinner {
    task: std::thread::JoinHandle<io::Result<()>>,
    sender: mpsc::Sender<String>,
}

impl Spinner {
    pub(crate) fn new(mut message: String) -> Self {
        let (tx, rx) = mpsc::channel();

        Self {
            task: std::thread::spawn(move || {
                const SPINNER_CHARS: &str = "-\\|/";
                let mut stdout = std::io::stdout();
                for ch in SPINNER_CHARS.chars().cycle() {
                    match rx.try_recv() {
                        Ok(new) => message = new,
                        Err(TryRecvError::Empty) => (),
                        Err(TryRecvError::Disconnected) => {
                            write!(stdout, "{CLEAR_LINE}")?;
                            break;
                        }
                    }
                    write!(stdout, "{CLEAR_LINE}{ch} {message}")?;
                    stdout.flush()?;
                    std::thread::sleep(Duration::from_millis(60));
                }
                Ok(())
            }),
            sender: tx,
        }
    }

    pub(crate) fn update_message(&self, message: String) {
        self.sender
            .send(message)
            .unwrap_or_else(|err| println!("{CLEAR_LINE}{}...", err.0))
    }

    pub(crate) fn finish(self) {
        drop(self.sender);
        match self.task.join() {
            Ok(Ok(())) => (),
            Ok(Err(err)) => error!(name: LOG_ONLY, "{err}"),
            Err(err) => error!(name: LOG_ONLY, "{err:?}"),
        }
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
