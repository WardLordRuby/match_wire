use super::{
    caching::{AddrMap, ContCodeMap, DnsResolutionMap, HostNameMap},
    display::{
        self, GAME_DISPLAY_NAMES, GAME_MODE_IDS, MAP_IDS,
        table::{
            DisplayFilterStats, DisplaySourceStats, DisplaySourceStatsInner, MIN_FILTER_COLS,
            TABLE_PADDING,
        },
    },
};
use crate::{
    LOG_ONLY, StartupInfo,
    commands::{
        filter::{FilterPreProcess, Server},
        handler::{Message, ReplHandle},
        launch::{HostName, WinApiErr},
    },
    models::json_data::{CacheFile, CondManifest, Version},
    try_fit_table, try_parse_signed_json,
};

use std::{
    borrow::Cow,
    cell::{Cell, OnceCell, RefCell},
    collections::HashMap,
    ffi::OsString,
    io,
    path::{Path, PathBuf},
    thread::LocalKey,
    time::SystemTime,
};

use repl_oxide::ansi_code::{RESET, YELLOW};
use tracing::{error, info};
use windows_sys::Win32::Foundation::HWND;
use winptyrs::PTY;

const MATCH_WIRE_PUBLIC_PGP_KEY: &str =
    "https://gist.githubusercontent.com/WardLordRuby/ca630bae78429d56a9484a099ddbefde/raw";
const SIGNED_STARTUP_INFO: &str =
    "https://gist.githubusercontent.com/WardLordRuby/795d840e208df2de08735c152889b2e4/raw";

const ENV_FILE: &str = ".env";
const ENV_OVERRIDE_PATH_KEY: &str = "HMW_ENV";

thread_local! {
    static GAME_CONSOLE_HISTORY: RefCell<ConsoleHistory> = const { RefCell::new(ConsoleHistory::new()) };
    static CACHE: RefCell<Cache> = panic!("Attempted to access cache prior to set");
    static UPDATE_CACHE: Cell<bool> = const { Cell::new(false) };
    static FORWARD_LOGS: Cell<bool> = const { Cell::new(false) };
    static ENDPOINTS: OnceCell<Endpoints> = const { OnceCell::new() };
    static PTY_HANDLE: RefCell<Option<PTY>> = panic!("Attempted to access PTY prior to set");
    static SELF_HWND: Option<HWND> = crate::commands::launch::get_console_hwnd()
        .map_err(display::error)
        .unwrap_or_default();
    static LAST_SERVER_STATS: RefCell<LastServerStats> = const { RefCell::new(LastServerStats::default()) };
    static ID_MAPS: IDMaps = IDMaps::new();
    static ALT_SCREEN_VIS: Cell<bool> = const { Cell::new(false) };
    static ALT_SCREEN_EVENT_BUFFER: RefCell<AltScreenEvents> = const { RefCell::new(AltScreenEvents::new()) };
    static EXE_PATH: io::Result<PathBuf> = std::env::current_exe();
    static ENV_PATH: io::Result<local_path::Env> = local_path::Env::new();
}

pub mod local_path {
    use super::{ENV_FILE, ENV_OVERRIDE_PATH_KEY, ENV_PATH, PathBuf, error, io};
    use crate::CRATE_NAME;

    pub mod exe {
        use super::{
            super::{EXE_PATH, Path, io},
            CRATE_NAME,
        };

        pub const GET_DIR_ERR: &str =
            constcat::concat!("Failed to find parent directory of ", CRATE_NAME, ".exe");

        pub fn with_borrow_path<R>(f: impl FnOnce(Result<&Path, &io::Error>) -> R) -> R {
            EXE_PATH.with(|path_res| f(path_res.as_deref()))
        }

        pub(crate) fn with_borrow_dir<R>(f: impl FnOnce(io::Result<&Path>) -> R) -> R {
            EXE_PATH.with(|path_res| {
                let exe_path = match path_res.as_deref() {
                    Ok(path) => path,
                    Err(err) => return f(Err(io::Error::new(err.kind(), err.to_string()))),
                };

                f(exe_path
                    .parent()
                    .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, GET_DIR_ERR)))
            })
        }
    }

    pub(crate) struct Env {
        pub(crate) path: PathBuf,
        pub(crate) exists: io::Result<bool>,
        pub(crate) user_modified: bool,
    }

    impl Env {
        pub(super) fn new() -> io::Result<Self> {
            super::local_path::exe::with_borrow_dir(|dir_res| {
                let exe_dir = dir_res?;

                let path_res = std::env::var(ENV_OVERRIDE_PATH_KEY);
                let user_modified = path_res.is_ok();
                let path = path_res
                    .map(|val| {
                        let mut user_path = PathBuf::from(val);

                        if user_path.is_dir() {
                            user_path.push(ENV_FILE);
                        }

                        if user_path.is_absolute() {
                            user_path
                        } else {
                            exe_dir.join(user_path)
                        }
                    })
                    .unwrap_or_else(|err| {
                        if let std::env::VarError::NotUnicode(_) = err {
                            error!("Non-Unicode value set for environment variable: {ENV_OVERRIDE_PATH_KEY}")
                        }
                        exe_dir.join(ENV_FILE)
                    });

                let exists = path.try_exists();

                Ok(Self {
                    path,
                    exists,
                    user_modified,
                })
            })
        }

        pub(crate) fn with_borrow<R>(f: impl FnOnce(Result<&Self, &io::Error>) -> R) -> R {
            ENV_PATH.with(|cell| f(cell.as_ref()))
        }
    }
}

#[derive(Default)]
pub(crate) struct AltScreenEvents {
    pub(crate) pre_subscriber: Vec<Message>,
    pub(crate) from_subscriber: String,
}

impl AltScreenEvents {
    const fn new() -> Self {
        Self {
            pre_subscriber: Vec::new(),
            from_subscriber: String::new(),
        }
    }
}

pub(crate) mod alt_screen {
    use super::{ALT_SCREEN_EVENT_BUFFER, ALT_SCREEN_VIS, Message};

    pub(crate) fn enter() {
        ALT_SCREEN_VIS.set(true);
    }

    pub(crate) fn leave() {
        ALT_SCREEN_VIS.set(false);
        let events = ALT_SCREEN_EVENT_BUFFER.take();

        events
            .pre_subscriber
            .into_iter()
            .for_each(|message| message.record());

        print!("{}", events.from_subscriber);
    }

    pub(crate) fn is_visible() -> bool {
        ALT_SCREEN_VIS.get()
    }

    /// Prefer a tracing event when possible as our subscriber format layer takes care of fn call behind the scenes
    pub fn push_message(message: Message) {
        if !ALT_SCREEN_VIS.get() {
            return message.record();
        }

        ALT_SCREEN_EVENT_BUFFER.with_borrow_mut(|buf| buf.pre_subscriber.push(message))
    }

    #[cfg(not(debug_assertions))]
    use tracing_subscriber::fmt::format::Writer;

    #[cfg(not(debug_assertions))]
    pub(crate) fn push_formatter<F>(print: F) -> std::fmt::Result
    where
        F: FnOnce(Writer<'_>) -> std::fmt::Result,
    {
        ALT_SCREEN_EVENT_BUFFER.with_borrow_mut(|buf| print(Writer::new(&mut buf.from_subscriber)))
    }
}

pub struct Cache {
    /// Key: host name with cod color codes
    pub host_to_connect: HostNameMap,
    pub ip_to_region: ContCodeMap,
    pub dns_resolution: DnsResolutionMap,
    pub connection_history: Vec<HostName>,
    pub iw4m: AddrMap,
    pub hmw: AddrMap,
    pub hmw_manifest: CondManifest,
    pub created: SystemTime,
}

impl From<CacheFile> for Cache {
    fn from(value: CacheFile) -> Self {
        Self {
            host_to_connect: value.cache.host_names,
            ip_to_region: value.cache.regions,
            dns_resolution: value.cache.dns_resolution,
            connection_history: value.connection_history,
            iw4m: value.cache.iw4m,
            hmw: value.cache.hmw,
            hmw_manifest: value.hmw_manifest,
            created: value.created,
        }
    }
}

impl Default for Cache {
    fn default() -> Self {
        Self {
            host_to_connect: HashMap::new(),
            ip_to_region: HashMap::new(),
            dns_resolution: HashMap::new(),
            connection_history: Vec::new(),
            iw4m: HashMap::new(),
            hmw: HashMap::new(),
            hmw_manifest: CondManifest::default(),
            created: SystemTime::now(),
        }
    }
}

impl Cache {
    pub fn set(cache: Self) {
        CACHE.set(cache);
    }

    pub(crate) fn clear() {
        CACHE.set(Self::default());
        UpdateCache::set(true);
    }

    pub(crate) fn with_borrow<R>(f: impl FnOnce(&Self) -> R) -> R {
        CACHE.with_borrow(f)
    }

    pub(crate) fn with_borrow_mut<R>(f: impl FnOnce(&mut Self) -> R) -> R {
        CACHE.with_borrow_mut(f)
    }
}

#[derive(serde::Deserialize, Debug)]
pub struct Endpoints {
    iw4_master_server: Cow<'static, str>,
    hmw_master_server: Cow<'static, str>,
    hmw_manifest_signed: Option<String>,
    hmw_pgp_public_key: Option<String>,

    #[cfg(not(debug_assertions))]
    hmw_download: Cow<'static, str>,

    server_info_endpoint: Cow<'static, str>,

    #[serde(skip)]
    skip_pgp: bool,
}

impl Endpoints {
    const fn default() -> Self {
        Self {
            iw4_master_server: Cow::Borrowed("https://master.iw4.zip/instance"),
            hmw_master_server: Cow::Borrowed("https://ms.horizonmw.org/game-servers"),
            hmw_manifest_signed: None,
            hmw_pgp_public_key: None,

            #[cfg(not(debug_assertions))]
            hmw_download: Cow::Borrowed("https://docs.horizonmw.org/download"),

            server_info_endpoint: Cow::Borrowed("/getInfo"),
            skip_pgp: false,
        }
    }

    #[expect(clippy::result_large_err)]
    fn set(endpoints: Self) -> Result<(), Self> {
        ENDPOINTS.with(|cell| cell.set(endpoints))
    }

    fn search_env() -> Option<String> {
        local_path::Env::with_borrow(|env| {
            let env = env.map_err(display::error).ok()?;

            let Ok(true) = env.exists else {
                if env.user_modified {
                    match &env.exists {
                        Ok(true) => unreachable!("by outer let"),
                        Ok(false) => error!("{}, doesn't exist", env.path.display()),
                        Err(err) => error!("Env var {ENV_OVERRIDE_PATH_KEY}: {err}"),
                    }
                }
                return None;
            };

            Self::parse_env_file(&env.path).map_err(display::error).ok()
        })
    }

    fn parse_env_file(path: &Path) -> io::Result<String> {
        const HOST_KEY: &str = "MASTER_SERVER_HOST";
        const PROTOCOL_KEY: &str = "MASTER_SERVER_PROTOCOL";

        let file = std::fs::read_to_string(path)?;

        let parse_field = |field| {
            file.split_once(field)
                .map(|(_, rhs)| {
                    let rhs = rhs.trim_start_matches([' ', '=']);
                    rhs.split_once(char::is_whitespace)
                        .map(|(val, _)| val)
                        .unwrap_or(rhs)
                })
                .ok_or_else(|| {
                    io::Error::new(
                        io::ErrorKind::InvalidData,
                        format!("env did not contain {field}"),
                    )
                })
        };

        let host = parse_field(HOST_KEY)?;
        let protocol = parse_field(PROTOCOL_KEY)?;

        Ok(format!("{protocol}://{host}"))
    }

    fn set_manifest_override(&mut self, mut host: String) {
        fn extract_endpoint(url: &str) -> Option<&str> {
            let mut ct = 0;
            for (i, ch) in url.char_indices() {
                if ch == '/' {
                    ct += 1
                }

                if ct == 3 {
                    return Some(&url[i..]);
                }
            }

            None
        }

        if let Some(prev_manifest) = self.hmw_manifest_signed.as_deref() {
            if let Some(endpoint) = extract_endpoint(prev_manifest) {
                host.push_str(endpoint)
            } else {
                error!("Failed to find endpoint in prev manifest url: {prev_manifest}")
            }
        } else {
            error!("Failed to append endpoint to env manifest url: no known endpoint available")
        }

        self.skip_pgp = true;
        self.hmw_manifest_signed = Some(host);
    }

    pub async fn init() -> Option<Version> {
        let parse_task = tokio::spawn(try_parse_signed_json::<StartupInfo>(
            SIGNED_STARTUP_INFO,
            "MatchWire startup json",
            MATCH_WIRE_PUBLIC_PGP_KEY,
            "MatchWire PGP public key",
        ));

        let env_override = Self::search_env();

        match parse_task.await.expect("task can not panic") {
            Ok(mut startup) => {
                if let Some(env_manifest) = env_override {
                    startup.endpoints.set_manifest_override(env_manifest);
                }
                Self::set(startup.endpoints).expect("only valid set");
                Some(startup.version)
            }
            Err(err) => {
                alt_screen::push_message(Message::error(format!(
                    "Failed to verify startup data: {err}"
                )));
                Self::set(Self::default()).expect("only valid set");
                None
            }
        }
    }

    fn get<R>(f: impl FnOnce(&'static Endpoints) -> R) -> R {
        ENDPOINTS.with(|cell| {
            let endpoints = cell.get().expect(
                "tried to access endpoints before startup process or outside of the main thread",
            );

            // Safety:
            // - Initialization of `OnceCell<Endpoints>` and all subsequent access to containing fields
            //   happen on the same thread set by tokio "Current thread" runtime.
            // - The use of `OnceCell` ensures we will panic on the above 'expect' before we allow undefined behavior
            let endpoints =
                unsafe { std::mem::transmute::<&Endpoints, &'static Endpoints>(endpoints) };

            f(endpoints)
        })
    }

    pub(crate) fn skip_verification() -> bool {
        Self::get(|endpoints| endpoints.skip_pgp)
    }

    #[cfg(not(debug_assertions))]
    #[inline]
    pub(crate) fn hmw_download() -> &'static str {
        Self::get(|endpoints| &endpoints.hmw_download)
    }
    #[inline]
    pub(crate) fn hmw_master_server() -> &'static str {
        Self::get(|endpoints| &endpoints.hmw_master_server)
    }
    #[inline]
    pub(crate) fn hmw_signed_manifest() -> Option<&'static str> {
        Self::get(|endpoints| endpoints.hmw_manifest_signed.as_deref())
    }
    #[inline]
    pub(crate) fn hmw_pgp_public_key() -> Option<&'static str> {
        Self::get(|endpoints| endpoints.hmw_pgp_public_key.as_deref())
    }
    #[inline]
    pub(crate) fn iw4_master_server() -> &'static str {
        Self::get(|endpoints| &endpoints.iw4_master_server)
    }
    #[inline]
    pub(crate) fn server_info_endpoint() -> &'static str {
        Self::get(|endpoints| &endpoints.server_info_endpoint)
    }
}

trait LocalKeyInternal<T: Copy + Default + 'static> {
    fn local_key() -> &'static LocalKey<Cell<T>>;
}

#[expect(private_bounds)]
pub(crate) trait ThreadCopyState<T: Copy + Default + 'static>: LocalKeyInternal<T> {
    fn get() -> T {
        Self::local_key().get()
    }

    fn set(value: T) {
        Self::local_key().set(value)
    }

    fn take() -> T {
        Self::local_key().take()
    }

    fn and_modify(f: impl FnOnce(T) -> T) {
        let local_key = Self::local_key();
        local_key.set(f(local_key.get()))
    }
}

pub(crate) struct UpdateCache;

impl LocalKeyInternal<bool> for UpdateCache {
    fn local_key() -> &'static LocalKey<Cell<bool>> {
        &UPDATE_CACHE
    }
}

impl ThreadCopyState<bool> for UpdateCache {}

pub(crate) struct ForwardLogs;

impl LocalKeyInternal<bool> for ForwardLogs {
    fn local_key() -> &'static LocalKey<Cell<bool>> {
        &FORWARD_LOGS
    }
}

impl ThreadCopyState<bool> for ForwardLogs {}

pub(crate) struct ConsoleHistory {
    pub(crate) entries: Vec<String>,
    last: usize,
}

impl ConsoleHistory {
    const fn new() -> Self {
        Self {
            entries: Vec::new(),
            last: 0,
        }
    }

    pub(crate) fn push(value: String) {
        GAME_CONSOLE_HISTORY.with_borrow_mut(|history| history.entries.push(value))
    }

    pub(crate) fn concat_new() -> Option<String> {
        GAME_CONSOLE_HISTORY.with_borrow(|history| {
            (ForwardLogs::get() && history.last() < history.entries.len())
                .then(|| history.new_entries().join("\n"))
        })
    }

    pub(crate) fn with_borrow_mut<R>(f: impl FnOnce(&mut Self) -> R) -> R {
        GAME_CONSOLE_HISTORY.with_borrow_mut(f)
    }

    pub(crate) fn reset_i(&mut self) {
        self.last = 0
    }

    pub(crate) fn last(&self) -> usize {
        self.last
    }

    pub(crate) fn new_entries(&self) -> &[String] {
        &self.entries[self.last..]
    }

    pub(crate) fn displayed(&mut self) {
        self.last = self.entries.len()
    }
}

pub(crate) mod app_hwnd {
    use super::{HWND, SELF_HWND};

    pub(crate) fn get() -> Option<HWND> {
        SELF_HWND.with(|&option| option)
    }
}

pub(crate) mod pty_handle {
    use super::{Cow, LOG_ONLY, OsString, PTY, PTY_HANDLE, WinApiErr, display, error, info};
    use crate::{
        commands::{
            handler::CommandSender,
            launch::{game_open, terminate_process_by_id},
        },
        utils::display::indicator::Spinner,
    };

    use std::{fmt::Display, time::Duration};

    pub(crate) enum PtyAccessErr {
        PtyErr(OsString),
        UserErr(Cow<'static, str>),
        NotAlive,
    }

    impl PtyAccessErr {
        pub(crate) fn is_connection_err(&self) -> bool {
            matches!(self, PtyAccessErr::PtyErr(_) | PtyAccessErr::NotAlive)
        }
    }

    impl From<OsString> for PtyAccessErr {
        fn from(err: OsString) -> Self {
            Self::PtyErr(err)
        }
    }

    impl Display for PtyAccessErr {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                PtyAccessErr::PtyErr(err) => write!(f, "{}", err.to_string_lossy()),
                PtyAccessErr::UserErr(err) => write!(f, "{err}"),
                PtyAccessErr::NotAlive => write!(f, "Game console not currently connected"),
            }
        }
    }

    impl From<PtyAccessErr> for Cow<'static, str> {
        fn from(err: PtyAccessErr) -> Self {
            match err {
                PtyAccessErr::UserErr(cow) => cow,
                err => err.to_string().into(),
            }
        }
    }

    pub(crate) enum ConnectionErr {
        PtyErr(OsString),
        WinApiErr(WinApiErr),
    }

    impl Display for ConnectionErr {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::PtyErr(err) => write!(f, "{}", err.to_string_lossy()),
                Self::WinApiErr(err) => write!(f, "{err}"),
            }
        }
    }

    impl From<OsString> for ConnectionErr {
        fn from(err: OsString) -> Self {
            Self::PtyErr(err)
        }
    }

    impl From<WinApiErr> for ConnectionErr {
        fn from(err: WinApiErr) -> Self {
            Self::WinApiErr(err)
        }
    }

    pub(crate) enum PseudoConStatus {
        Attached,
        Unattached,
        Closed,
    }

    pub(crate) fn set(pty: Option<PTY>) {
        PTY_HANDLE.set(pty)
    }

    fn is_alive(handle: &mut Option<PTY>) -> Result<bool, OsString> {
        let Some(game_console) = handle else {
            return Ok(false);
        };

        let alive_res = game_console.is_alive();
        if matches!(alive_res, Ok(false) | Err(_)) {
            *handle = None
        }
        alive_res
    }

    /// Only checks if the PTY console is alive, if you also need more information about the games current open
    /// state use [`Self::check_connection`]
    pub(crate) fn game_connected() -> Result<bool, OsString> {
        PTY_HANDLE.with_borrow_mut(is_alive)
    }

    pub(crate) fn check_connection() -> Result<PseudoConStatus, ConnectionErr> {
        if game_connected()? {
            return Ok(PseudoConStatus::Attached);
        }

        Ok(match game_open()? {
            Some(_) => PseudoConStatus::Unattached,
            None => PseudoConStatus::Closed,
        })
    }

    pub(crate) fn try_if_alive<R, E: Into<Cow<'static, str>>>(
        f: impl FnOnce(&PTY) -> Result<R, E>,
    ) -> Result<R, PtyAccessErr> {
        PTY_HANDLE.with_borrow_mut(|handle| {
            if !is_alive(handle)? {
                return Err(PtyAccessErr::NotAlive);
            }

            f(handle
                .as_ref()
                .expect("early return ensures handle is some"))
            .map_err(|err| PtyAccessErr::UserErr(err.into()))
        })
    }

    pub(crate) fn try_drop_pty(game_name: &str) {
        let sent_quit = PTY_HANDLE.with_borrow_mut(|handle| {
            let Some(game_console) = handle else {
                return false;
            };

            let sent_quit = matches!(game_console.is_alive(), Ok(true))
                && game_open()
                    .unwrap_or_else(WinApiErr::resolve_to_closed)
                    .is_some()
                && game_console
                    .send_cmd("quit")
                    .map_err(display::log_error)
                    .is_ok();

            if !sent_quit {
                *handle = None;
            }

            sent_quit
        });

        if sent_quit {
            info!(name: LOG_ONLY, "{game_name}'s console accepted quit command");
            wait_for_exit(game_name);
        }
    }

    pub(crate) fn wait_for_exit(game_name: &str) {
        const TIMEOUT: Duration = Duration::from_secs(60);

        let spinner = Spinner::new(format!("Waiting for {game_name} to close"));
        let start_time = std::time::Instant::now();

        PTY_HANDLE.with_borrow_mut(|handle| {
            let Some(game_console) = handle else { return };

            while let Ok(true) = game_console.is_alive() {
                if start_time.elapsed() > TIMEOUT {
                    let pid = game_console.get_pid();
                    if terminate_process_by_id(pid)
                        .map_err(display::log_error)
                        .is_ok()
                    {
                        error!(name: LOG_ONLY, "Hang detected, {game_name} terminated by windows api");
                    }

                    break;
                }

                std::thread::sleep(Duration::from_millis(600));
            }

            *handle = None;
        });

        spinner.finish();
    }
}

pub(crate) struct LastServerStats {
    pub(crate) source: Vec<DisplaySourceStatsInner>,
    pub(crate) filter: Vec<Server>,
    pub(crate) pre_process: FilterPreProcess,
}

impl LastServerStats {
    const fn default() -> Self {
        Self {
            source: Vec::new(),
            filter: Vec::new(),
            pre_process: FilterPreProcess::default(),
        }
    }

    /// The returned `io::Error` should be propagated as [`CmdErr::Critical`]
    ///
    /// [`CmdErr::Critical`]: crate::commands::handler::CmdErr::Critical
    pub(crate) fn display(repl: &mut ReplHandle) -> io::Result<()> {
        LAST_SERVER_STATS.with_borrow(|stats| {
            if stats.source.is_empty() {
                println!(
                    "{YELLOW}No previous filter data in memory, \
                    run a filter command using '--stats' to store{RESET}"
                );
                return Ok(());
            }

            let (cols, rows) = repl.terminal_size();
            let width = (cols.saturating_sub(TABLE_PADDING) as usize).max(MIN_FILTER_COLS);
            try_fit_table(repl, (cols, rows), width)?;

            println!("\n{}", DisplaySourceStats(&stats.source));

            Cache::with_borrow(|cache| {
                println!(
                    "{}",
                    DisplayFilterStats(
                        &stats.filter,
                        &stats.pre_process,
                        &cache.ip_to_region,
                        width
                    )
                );
            });

            Ok(())
        })
    }

    pub(crate) fn set(source: Vec<DisplaySourceStatsInner>, mut filter: Vec<Server>) {
        LAST_SERVER_STATS.set(Self {
            pre_process: FilterPreProcess::compute(&mut filter),
            filter,
            source,
        });
    }
}

pub(crate) struct IDMaps {
    pub(crate) game_ids: HashMap<&'static str, &'static str>,
    pub(crate) game_mode_ids: HashMap<&'static str, &'static str>,
    pub(crate) map_ids: HashMap<&'static str, &'static str>,
}

impl IDMaps {
    fn new() -> Self {
        Self {
            game_ids: HashMap::from(GAME_DISPLAY_NAMES),
            game_mode_ids: HashMap::from(GAME_MODE_IDS),
            map_ids: HashMap::from(MAP_IDS),
        }
    }

    pub(crate) fn with_borrow<R>(f: impl FnOnce(&Self) -> R) -> R {
        ID_MAPS.with(|maps| f(maps))
    }
}

#[cfg(test)]
mod test {
    use super::{MATCH_WIRE_PUBLIC_PGP_KEY, SIGNED_STARTUP_INFO};
    use crate::{ResponseErr, models::json_data::StartupInfo, pgp_verify_cleartext};
    use reqwest::{StatusCode, blocking::get};

    fn blocking_verify(
        cleartext_json_url: &str,
        cleartext_ctx: &'static str,
        public_key_url: &str,
        public_key_ctx: &'static str,
    ) -> Result<String, ResponseErr> {
        let cleartext_response = get(cleartext_json_url).unwrap();
        let public_key_response = get(public_key_url).unwrap();

        if cleartext_response.status() != StatusCode::OK {
            panic!(
                "{cleartext_ctx} returned status: {}",
                cleartext_response.status()
            )
        }

        if public_key_response.status() != StatusCode::OK {
            panic!(
                "{public_key_ctx} returned status: {}",
                public_key_response.status()
            )
        }

        let cleartext_string = cleartext_response.text().unwrap();
        let public_key_string = public_key_response.text().unwrap();

        pgp_verify_cleartext(&cleartext_string, &public_key_string)
    }

    #[test]
    fn pgp_verify_manifest() {
        let remote_endpoints = blocking_verify(
            SIGNED_STARTUP_INFO,
            "MatchWire startup data",
            MATCH_WIRE_PUBLIC_PGP_KEY,
            "MatchWire PGP public key",
        )
        .map(|data| {
            serde_json::from_str::<StartupInfo>(&data)
                .unwrap()
                .endpoints
        })
        .unwrap();

        let hmw_manifest_res = blocking_verify(
            remote_endpoints.hmw_manifest_signed.as_deref().unwrap(),
            "HMW manifest",
            remote_endpoints.hmw_pgp_public_key.as_deref().unwrap(),
            "HMW PGP public key",
        );

        assert!(hmw_manifest_res.is_ok())
    }
}
