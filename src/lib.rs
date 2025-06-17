pub mod commands {
    pub mod filter;
    pub mod handler;
    pub mod launch;
    pub mod reconnect;
    pub mod settings;
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
        launch::get_exe_version,
    },
    models::{
        cli::Command,
        json_data::{CacheFile, HmwManifest, StartupInfo},
    },
    utils::{
        caching::{ReadCacheErr, build_cache},
        display::{self, table::TABLE_PADDING},
        global_state::{self, AltScreen},
        subscriber::init_subscriber,
    },
};

use std::{
    borrow::Cow,
    fmt::Display,
    io::{self, BufRead, BufReader},
    marker::PhantomData,
    num::NonZero,
    path::{Path, PathBuf},
    time::{Duration, Instant},
};

use clap::CommandFactory;
use constcat::concat;
use crossterm::cursor;
use pgp::composed::{CleartextSignedMessage, Deserializable, SignedPublicKey};
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

pub const H2M_MAX_CLIENT_NUM: u8 = 18;
pub(crate) const H2M_MAX_TEAM_SIZE: u8 = 9;

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
    use std::io;

    #[cfg(not(debug_assertions))]
    use crossterm::{
        cursor, execute, queue,
        terminal::{self, BeginSynchronizedUpdate, EndSynchronizedUpdate},
    };

    #[cfg(not(debug_assertions))]
    use crate::global_state::AltScreen;

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

            AltScreen::enter();
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
            AltScreen::leave();
        }
    }
}

#[derive(Debug)]
pub enum ResponseErr {
    Reqwest(reqwest::Error),
    Status(&'static str, reqwest::StatusCode),
    Pgp(pgp::errors::Error),
    Serialize(&'static str, serde_json::Error),
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

impl From<pgp::errors::Error> for ResponseErr {
    fn from(err: pgp::errors::Error) -> Self {
        Self::Pgp(err)
    }
}

/// Returns the working `%localappdata%` for this program
pub fn try_init_logger() -> Option<PathBuf> {
    let local_dir = std::env::var_os(LOCAL_DATA);
    let local_dir = local_dir.as_ref().map(Path::new);

    if let Some(local) = local_dir {
        match check_app_dir_exists(local) {
            Ok(app_dir_local) => {
                init_subscriber(&app_dir_local)
                    .unwrap_or_else(|err| AltScreen::push_message(Message::error(err.to_string())));
                info!(name: LOG_ONLY, "App startup");
                Some(app_dir_local)
            }
            Err(err) => {
                AltScreen::push_message(Message::error(err.to_string()));
                None
            }
        }
    } else {
        AltScreen::push_message(Message::error("Could not find %appdata%/local"));

        #[cfg(debug_assertions)]
        init_subscriber(std::path::Path::new("")).unwrap();

        None
    }
}

pub(crate) fn client_with_timeout(secs: u64) -> Client {
    Client::builder().timeout(Duration::from_secs(secs)).build().expect(
        "TLS backend cannot be initialized, or the resolver cannot load the system configuration",
    )
}

pub async fn get_latest_hmw_manifest() -> Result<HmwManifest, ResponseErr> {
    let (Some(hmw_manifest), Some(hmw_pgp_public_key)) = (
        global_state::Endpoints::hmw_signed_manifest(),
        global_state::Endpoints::hmw_pgp_public_key(),
    ) else {
        return Err(ResponseErr::other(
            "Could not verify encryption of HMW manifest location\n\
                Restart MatchWire to try again",
        ));
    };

    try_parse_signed_json::<HmwManifest>(
        hmw_manifest,
        "HMW manifest",
        hmw_pgp_public_key,
        "HMW PGP public key",
    )
    .await
}

pub(crate) async fn try_parse_signed_json<T: serde::de::DeserializeOwned>(
    cleartext_json_url: &str,
    cleartext_ctx: &'static str,
    public_key_url: &str,
    public_key_ctx: &'static str,
) -> Result<T, ResponseErr> {
    let client = client_with_timeout(6);

    let (cleartext_response, public_key_response) = tokio::try_join!(
        client.get(cleartext_json_url).send(),
        client.get(public_key_url).send()
    )?;

    if cleartext_response.status() != STATUS_OK {
        return Err(ResponseErr::bad_status(cleartext_ctx, cleartext_response));
    }

    if public_key_response.status() != STATUS_OK {
        return Err(ResponseErr::bad_status(public_key_ctx, public_key_response));
    }

    let (cleartext_string, public_key_string) =
        tokio::try_join!(cleartext_response.text(), public_key_response.text())?;

    let signed_string = pgp_verify_cleartext(cleartext_string, &public_key_string)?;
    info!(name: LOG_ONLY, "{cleartext_ctx} PGP signature verified!");

    serde_json::from_str::<T>(&signed_string)
        .map_err(|err| ResponseErr::Serialize(cleartext_ctx, err))
}

pub fn pgp_verify_cleartext(
    mut cleartext_string: String,
    public_key_str: &str,
) -> Result<String, ResponseErr> {
    // MARK: XXX
    // Remove this manual addition of the rfc9580 7.1 Cleartext header once HMW manifest is compliant
    const CLEARTEXT_HEADER: &str = "-----BEGIN PGP SIGNED MESSAGE-----";
    const PGP_SIG_HEADER: &str = "-----BEGIN PGP SIGNATURE-----";
    if !cleartext_string.starts_with(CLEARTEXT_HEADER) {
        let split_i = cleartext_string
            .find(PGP_SIG_HEADER)
            .ok_or_else(|| ResponseErr::other("Manifest did not contain PGP signature"))?;

        let (signed_msg, signature) = cleartext_string.split_at(split_i);
        cleartext_string = format!("{CLEARTEXT_HEADER}\nHash: SHA256\n\n{signed_msg}\n{signature}");
    }

    let (public_key, _headers_public) = SignedPublicKey::from_string(public_key_str)?;
    let (msg, _headers_msg) = CleartextSignedMessage::from_string(&cleartext_string)?;

    msg.verify(&public_key)?;
    Ok(msg.signed_text())
}

pub(crate) fn open_dir(path: &Path) {
    if let Err(err) = std::process::Command::new("explorer").arg(path).spawn() {
        error!("{err}")
    }
}

#[cfg(not(debug_assertions))]
fn contains_required_files(exe_dir: &Path) -> Result<PathBuf, Cow<'static, str>> {
    use crate::utils::display::HmwDownloadHint;

    macro_rules! exists {
        ($file:expr) => {
            $file.try_exists().map_err(|err| err.to_string())?
        };
    }

    if !exists!(exe_dir.join(FNAME_MWR)) {
        return Err(Cow::Borrowed(concat!(
            "Move ",
            CRATE_NAME,
            ".exe into your 'Call of Duty Modern Warfare Remastered' directory",
        )));
    }

    let found_game = if exists!(exe_dir.join(FNAME_HMW)) {
        FNAME_HMW
    } else if exists!(exe_dir.join(FNAME_H2M_1)) {
        FNAME_H2M_1
    } else if exists!(exe_dir.join(FNAME_H2M_2)) {
        FNAME_H2M_2
    } else {
        return Err(Cow::Owned(format!("Mod exe not found, {HmwDownloadHint}")));
    };

    Ok(exe_dir.join(found_game))
}

pub(crate) fn hash_file_hex(path: &Path) -> io::Result<String> {
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

pub fn await_user_for_end<D: Display>(err: D) {
    println!("{RED}{err}{RESET}");
    println!("Press enter to exit...");
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
        std::fs::create_dir(local)?;
    }

    if prev_local_dir
        .try_exists()
        .map_err(|err| {
            AltScreen::push_message(Message::error(format!(
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
    if AltScreen::is_visible() {
        AltScreen::push_message(message);
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

#[derive(Clone, Copy)]
pub struct RateLimiter<T> {
    ct: usize,
    start: Option<Instant>,
    limit: usize,
    interval: Duration,
    _marker: PhantomData<T>,
}

pub trait RateLimitConfig {
    const LIMIT: NonZero<usize>;
    const INTERVAL: NonZeroDuration;
    const DISP_NAME: &str;
}

#[derive(Clone, Copy)]
pub struct NonZeroDuration(Duration);

impl NonZeroDuration {
    pub const fn new(duration: Duration) -> Option<Self> {
        if duration.is_zero() {
            None
        } else {
            Some(Self(duration))
        }
    }

    pub const fn get(&self) -> Duration {
        self.0
    }
}

/// `$ident` is the struct and display name of the generated `RateLimitConfig`\
/// `($ident:ident, $limit:usize, $interval:Duration)`
///
/// ```compile_fail
/// # use match_wire::impl_rate_limit_config;
/// # use std::time::Duration;
/// // Limit must be non-zero
/// impl_rate_limit_config!(LimitZero, 0, Duration::from_secs(60)); // doesn't compile!
/// ```
///
/// ```compile_fail
/// # use match_wire::impl_rate_limit_config;
/// # use std::time::Duration;
/// // Interval must be non-zero
/// impl_rate_limit_config!(IntervalZero, 60, Duration::from_secs(0)); // doesn't compile!
/// ```
#[macro_export]
macro_rules! impl_rate_limit_config {
    ($ident:ident, $limit:expr, $interval:expr) => {
        #[derive(Clone, Copy)]
        pub(crate) struct $ident;

        impl $crate::RateLimitConfig for $ident {
            const LIMIT: std::num::NonZero<usize> = std::num::NonZero::new($limit).unwrap();
            const INTERVAL: $crate::NonZeroDuration =
                $crate::NonZeroDuration::new($interval).unwrap();
            const DISP_NAME: &str = stringify!($ident);
        }

        const _: () = assert!(!$interval.is_zero(), "Interval must be non-zero");
        const _: () = assert!($limit != 0, "Limit must be non-zero");
    };
}

impl<T: RateLimitConfig> RateLimiter<T> {
    #[allow(clippy::new_without_default)] // prefer new since it can be const
    pub const fn new() -> Self {
        Self {
            ct: 0,
            start: None,
            limit: T::LIMIT.get(),
            interval: T::INTERVAL.get(),
            _marker: PhantomData,
        }
    }

    #[inline]
    fn elapsed(&self) -> Option<Duration> {
        self.start.map(|start| start.elapsed())
    }

    pub fn remainder(&self) -> Option<Duration> {
        self.start
            .and_then(|start| self.interval.checked_sub(start.elapsed()))
    }

    /// This method is used to advance the internal state and return if it is ok to send an API request. Do
    /// **NOT** use to find if the limit has been reached when not also attempting to send a request.
    /// Limited status can be found with [`Self::limited`].
    pub fn within_limit(&mut self) -> bool {
        if self.ct == 0 {
            self.start = Some(Instant::now())
        } else if self.ct >= self.limit {
            return if self.elapsed().unwrap() > self.interval {
                self.ct = 0;
                self.within_limit()
            } else {
                false
            };
        }

        self.ct += 1;
        true
    }

    pub fn limited(&self) -> bool {
        !(self.ct < self.limit || self.elapsed().expect("limit can not be 0") > self.interval)
    }
}
