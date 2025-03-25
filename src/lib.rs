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
    pub mod subscriber;
}

use crate::{
    commands::{
        handler::{AppDetails, Message, StartupCacheContents},
        launch_h2m::get_exe_version,
    },
    models::{
        cli::Command,
        json_data::{CacheFile, Endpoints, HmwManifest, StartupInfo},
    },
    utils::caching::{build_cache, Cache, ReadCacheErr},
};

use std::{
    borrow::Cow,
    collections::HashSet,
    io::{self, BufRead, BufReader, Read, Write},
    path::{Path, PathBuf},
    sync::{
        atomic::AtomicBool,
        mpsc::{self, TryRecvError},
        Arc, OnceLock,
    },
    time::Duration,
};

use clap::CommandFactory;
use constcat::concat;
use crossterm::cursor;
use repl_oxide::ansi_code::{RED, RESET};
use serde_json::{from_value, Value};
use serde_json_path::JsonPath;
use sha2::{Digest, Sha256};
use tracing::error;

#[cfg(not(debug_assertions))]
use crossterm::{execute, terminal};

pub(crate) const MAIN_PROMPT: &str = concat!(env!("CARGO_PKG_NAME"), ".exe");
pub const LOG_ONLY: &str = "log_only";

const STARTUP_INFO_URL: &str =
    "https://gist.githubusercontent.com/WardLordRuby/15920ff68ae348933636a5c18bc51709/raw";

const MOD_FILES_MODULE_NAME: &str = "mod";
const HMW_DOWNLOAD_HINT: &str =
    "HMW mod files are available to download for free through the Horizon MW launcher";

pub(crate) const H2M_MAX_CLIENT_NUM: i64 = 18;
pub(crate) const H2M_MAX_TEAM_SIZE: i64 = 9;

pub const SAVED_HISTORY_CAP: usize = 20;

pub(crate) const REQUIRED_FILES: [&str; 7] = [
    "h1_mp64_ship.exe",
    "h2m-mod",
    "players2",
    "h2m-mod.exe",
    "h2m-revived.exe",
    "hmw-mod",
    "hmw-mod.exe",
];

pub const LOCAL_DATA: &str = "LOCALAPPDATA";
pub(crate) const CACHED_DATA: &str = "cache.json";

pub(crate) const TERM_CLEAR_LINE: &str = "\r\x1B[J";

pub(crate) static SPLASH_SCREEN_VIS: AtomicBool = AtomicBool::new(false);

#[cfg(not(debug_assertions))]
pub(crate) static SPLASH_SCREEN_EVENT_BUFFER: std::sync::LazyLock<
    std::sync::Mutex<SplashScreenEvents>,
> = std::sync::LazyLock::new(|| std::sync::Mutex::new(SplashScreenEvents::default()));

pub(crate) static ENDPOINTS: OnceLock<Endpoints> = OnceLock::new();

#[macro_export]
macro_rules! new_io_error {
    ($kind:expr, $msg:expr) => {
        Err(std::io::Error::new($kind, $msg))
    };
}

#[cfg(not(debug_assertions))]
#[derive(Default)]
pub(crate) struct SplashScreenEvents {
    pub(crate) pre_subscriber: Vec<Message>,
    pub(crate) from_subscriber: String,
}

fn set_fallback_endpoints() -> AppDetails {
    ENDPOINTS
        .set(Endpoints::default())
        .expect("only called if failed to reach match_wire_remote_info.json");
    AppDetails::default()
}

pub async fn set_endpoints() -> AppDetails {
    let client = reqwest::Client::new();
    let response = match client
        .get(STARTUP_INFO_URL)
        .timeout(Duration::from_secs(3))
        .send()
        .await
    {
        Ok(data) => data,
        Err(err) => {
            print_during_splash(Message::error(format!(
                "Could not reach MatchWire startup json: {err}"
            )));
            return set_fallback_endpoints();
        }
    };

    match response.json::<StartupInfo>().await {
        Ok(startup) => {
            ENDPOINTS.set(startup.endpoints).expect("only valid set");
            AppDetails::from(startup.version)
        }
        Err(err) => {
            print_during_splash(Message::error(format!(
                "Failed to parse MatchWire startup json: {err}"
            )));
            set_fallback_endpoints()
        }
    }
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

pub async fn get_latest_hmw_hash() -> reqwest::Result<Result<String, &'static str>> {
    let client = reqwest::Client::new();
    let latest = client
        .get(Endpoints::hmw_manifest())
        .timeout(Duration::from_secs(6))
        .send()
        .await?
        .json::<Value>()
        .await?;

    if let Some(json_path) = Endpoints::manifest_hash_path() {
        match try_query_json_path(&latest, json_path) {
            Ok(hash) => return Ok(Ok(hash)),
            Err(err) => print_during_splash(Message::error(err)),
        }
    }

    let mut hmw_manifest = match from_value::<HmwManifest>(latest) {
        Ok(manifest) => manifest,
        Err(_) => return Ok(Err("hmw manifest.json formatting has changed")),
    };

    Ok(hmw_manifest
        .modules
        .iter_mut()
        .find(|module| module.name == MOD_FILES_MODULE_NAME)
        .and_then(|module| module.files_with_hashes.remove(REQUIRED_FILES[6]))
        .ok_or("hmw manifest.json formatting has changed"))
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
            let result = list
                .iter()
                .all(|check_file| str_names.contains(check_file.borrow()));
            result
        })),
        Operation::Any => Ok(OperationResult::Bool({
            let result = list
                .iter()
                .any(|check_file| str_names.contains(check_file.borrow()));
            result
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

fn hmw_download_hint() -> String {
    format!("{HMW_DOWNLOAD_HINT}\n{}", Endpoints::hmw_download())
}

pub fn contains_required_files(exe_dir: &Path) -> Result<PathBuf, Cow<'static, str>> {
    match does_dir_contain(exe_dir, Operation::Count, &REQUIRED_FILES)
        .expect("Failed to read contents of current dir")
    {
        OperationResult::Count((_, files)) => {
            if !files.contains(REQUIRED_FILES[0]) {
                return Err(Cow::Borrowed(concat!(
                    "Move ",
                    env!("CARGO_PKG_NAME"),
                    ".exe into your 'Call of Duty Modern Warfare Remastered' directory",
                )));
            }
            if !files.contains(REQUIRED_FILES[5]) && !files.contains(REQUIRED_FILES[1]) {
                return Err(Cow::Owned(format!(
                    "Mw2 Remastered mod files not found, {}",
                    hmw_download_hint()
                )));
            }
            let found_game = if files.contains(REQUIRED_FILES[6]) {
                REQUIRED_FILES[6]
            } else if files.contains(REQUIRED_FILES[3]) {
                REQUIRED_FILES[3]
            } else if files.contains(REQUIRED_FILES[4]) {
                REQUIRED_FILES[4]
            } else {
                return Err(Cow::Owned(format!(
                    "Mod exe not found, {}",
                    hmw_download_hint()
                )));
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

pub fn exe_details(game_exe_path: &Path) -> (Option<f64>, Option<String>) {
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
pub fn check_app_dir_exists(local: &mut PathBuf) -> io::Result<()> {
    const PREV_NAME: &str = "h2m_favorites";
    let app_name = env!("CARGO_PKG_NAME");
    let local_dir = local.clone();

    match does_dir_contain(local, Operation::Count, &[app_name, PREV_NAME]) {
        Ok(OperationResult::Count((_, files))) => {
            local.push(app_name);

            if !files.contains(app_name) {
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
    const COLOR_ESCAPE_CODE: char = '^';
    let mut host_name = String::new();
    let mut chars = name.chars().peekable();
    while let Some(c) = chars.next() {
        if c == COLOR_ESCAPE_CODE {
            if chars.peek().is_some() {
                chars.next();
            }
        } else {
            host_name.push(c.to_ascii_lowercase());
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
                            write!(stdout, "{TERM_CLEAR_LINE}")?;
                            break;
                        }
                    }
                    write!(stdout, "{TERM_CLEAR_LINE}{ch} {message}")?;
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
            .unwrap_or_else(|err| println!("{}...", err.0))
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

pub async fn splash_screen() -> io::Result<()> {
    #[cfg(not(debug_assertions))]
    {
        use crossterm::{
            queue,
            terminal::{self, BeginSynchronizedUpdate, EndSynchronizedUpdate},
        };

        // font: 4Max - patorjk.com
        const SPLASH_TEXT: [&str; 4] = [
            r#"8b    d8    db    888888  dP""b8 88  88     Yb        dP 88 88""Yb 888888"#,
            r#"88b  d88   dPYb     88   dP   `" 88  88      Yb  db  dP  88 88__dP 88__  "#,
            r#"88YbdP88  dP__Yb    88   Yb      888888       YbdPYbdP   88 88"Yb  88""  "#,
            r#"88 YY 88 dP""""Yb   88    YboodP 88  88        YP  YP    88 88  Yb 888888"#,
        ];

        let mut stdout = std::io::stdout();

        SPLASH_SCREEN_VIS.store(true, std::sync::atomic::Ordering::SeqCst);
        execute!(stdout, terminal::EnterAlternateScreen)?;

        let (width, height) = terminal::size()?;

        let start_y = height.saturating_sub(SPLASH_TEXT.len() as u16) / 2;

        for (i, &line) in SPLASH_TEXT.iter().enumerate() {
            let start_x = width.saturating_sub(line.chars().count() as u16) / 2;
            execute!(stdout, BeginSynchronizedUpdate)?;

            queue!(
                stdout,
                cursor::MoveTo(start_x, start_y + i as u16),
                crossterm::style::Print(line)
            )?;

            tokio::time::sleep(Duration::from_millis(160)).await;
            execute!(stdout, EndSynchronizedUpdate)?;
        }

        tokio::time::sleep(Duration::from_secs(2)).await;
    }

    Ok(())
}

pub(crate) async fn leave_splash_screen(task: tokio::task::JoinHandle<io::Result<()>>) {
    task.await.unwrap().unwrap();

    #[cfg(not(debug_assertions))]
    {
        execute!(std::io::stdout(), terminal::LeaveAlternateScreen).unwrap();
        SPLASH_SCREEN_VIS.store(false, std::sync::atomic::Ordering::SeqCst);

        let events = loop {
            match SPLASH_SCREEN_EVENT_BUFFER.try_lock() {
                Ok(mut buffer) => break std::mem::take(&mut *buffer),
                Err(_) => tokio::time::sleep(Duration::from_millis(20)).await,
            }
        };

        events
            .pre_subscriber
            .into_iter()
            .for_each(|message| message.record());

        print!("{}", events.from_subscriber)
    }
}

/// **Only** use for errors encountered before tracing subscriber has been initialized otherwise prefer a tracing
/// event as our subscriber format layer takes care of fn call behind the scenes
pub fn print_during_splash(message: Message) {
    #[cfg(debug_assertions)]
    {
        assert!(!SPLASH_SCREEN_VIS.load(std::sync::atomic::Ordering::SeqCst));
        println!("{message}");
    }

    #[cfg(not(debug_assertions))]
    {
        let mut event_buffer = SPLASH_SCREEN_EVENT_BUFFER.lock().expect("lock uncontested");
        event_buffer.pre_subscriber.push(message);
    }
}

pub async fn startup_cache_task(
    cache_res: Option<Result<CacheFile, ReadCacheErr>>,
) -> StartupCacheContents {
    use tokio::sync::Mutex;

    let (cmd_history, prev_cache) = match cache_res {
        Some(Ok(mut file_contents)) => {
            return StartupCacheContents {
                command_history: std::mem::take(&mut file_contents.cmd_history),
                cache: Arc::new(Mutex::new(Cache::from(file_contents))),
                modified: false,
            }
        }
        Some(Err(err)) => {
            error!("{err}");
            err.cache
                .map(|mut file_contents| {
                    (
                        Some(std::mem::take(&mut file_contents.cmd_history)),
                        Some(Arc::new(Mutex::new(Cache::from(file_contents)))),
                    )
                })
                .unwrap_or((None, None))
        }
        None => (None, None),
    };

    let build_res = build_cache(prev_cache.as_ref()).await;
    let modified = build_res.is_ok();

    let cache = match build_res {
        Ok(updated_cache) => {
            if let Some(prev) = prev_cache {
                {
                    let mut lock = prev.lock().await;
                    *lock = updated_cache;
                }
                prev
            } else {
                Arc::new(Mutex::new(updated_cache))
            }
        }
        Err(err) => {
            error!("{err}");
            prev_cache.unwrap_or_else(|| Arc::new(Mutex::new(Cache::default())))
        }
    };

    StartupCacheContents {
        command_history: cmd_history.unwrap_or_default(),
        cache,
        modified,
    }
}
