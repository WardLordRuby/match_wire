use super::{
    caching::AddrMap,
    display::{
        self, GAME_DISPLAY_NAMES, GAME_TYPE_IDS, MAP_IDS,
        indicator::Spinner,
        table::{
            DisplayFilterStats, DisplaySourceStats, DisplaySourceStatsInner, MIN_FILTER_COLS,
            TABLE_PADDING,
        },
    },
};
use crate::{
    LOG_ONLY, StartupInfo,
    commands::{
        filter::{FilterPreProcess, Server, process_stats},
        handler::{CommandSender, Message, ReplHandle},
        launch::{HostName, WinApiErr, game_open, terminate_process_by_id},
    },
    models::json_data::{CacheFile, CondManifest, Version},
    try_fit_table, try_parse_signed_json,
};

use std::{
    borrow::Cow,
    cell::{Cell, LazyCell, OnceCell, RefCell},
    collections::HashMap,
    fmt::Display,
    io,
    net::{IpAddr, SocketAddr},
    path::{Path, PathBuf},
    thread::LocalKey,
    time::{Duration, SystemTime},
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
const ENV_REL_KEY: &str = "HMW_ENV";

type IDMapsInner = (
    HashMap<&'static str, &'static str>,
    HashMap<&'static str, &'static str>,
);
type ServerStatsInner = (Vec<DisplaySourceStatsInner>, Vec<Server>, FilterPreProcess);

thread_local! {
    static GAME_CONSOLE_HISTORY: RefCell<ConsoleHistory> = const { RefCell::new(ConsoleHistory::new()) };
    static CACHE: RefCell<Cache> = panic!("Attempted to access cache prior to set");
    static UPDATE_CACHE: Cell<bool> = const { Cell::new(false) };
    static FORWARD_LOGS: Cell<bool> = const { Cell::new(false) };
    static ENDPOINTS: OnceCell<Endpoints> = const { OnceCell::new() };
    static PTY_HANDLE: RefCell<Option<PTY>> = panic!("Attempted to access PTY prior to set");
    static SELF_HWND: Option<HWND> = crate::commands::launch::get_console_hwnd()
        .map_err(crate::utils::display::error)
        .unwrap_or_default();
    static LAST_SERVER_STATS: RefCell<ServerStatsInner> =
        const { RefCell::new((Vec::new(), Vec::new(), FilterPreProcess::default())) };
    static ID_MAPS: OnceCell<IDMapsInner> = const { OnceCell::new() };
    static GAME_ID_MAP: OnceCell<HashMap<&'static str, &'static str>> = const { OnceCell::new() };
    static ALT_SCREEN_VIS: Cell<bool> = const { Cell::new(false) };
    static ALT_SCREEN_EVENT_BUFFER: RefCell<AltScreenEvents> = RefCell::new(AltScreenEvents::default());
    static EXE_PATH: LazyCell<io::Result<PathBuf>> = LazyCell::new(std::env::current_exe);
}

pub struct ExePath;

impl ExePath {
    /// **Warning**: Calling this method outside of the main thread may result in the ability to obtain dangling references
    pub fn get() -> Result<&'static Path, &'static io::Error> {
        EXE_PATH.with(|cell|
            // Safety: Initialization of the `LazyCell` and all subsequent access happen on the same thread set by
            // tokio "Current thread" runtime. Thus making the thread local and contained `Endpoints` actually 'static.
            unsafe {
                std::mem::transmute::<
                    Result<&Path, &io::Error>,
                    Result<&'static Path, &'static io::Error>,
                >(cell.as_deref())
            })
    }
}

#[derive(Default)]
pub(crate) struct AltScreenEvents {
    #[allow(dead_code)] // currently only used on release
    pub(crate) pre_subscriber: Vec<Message>,
    pub(crate) from_subscriber: String,
}

#[cfg(not(debug_assertions))]
use tracing_subscriber::fmt::format::Writer;

pub(crate) struct AltScreen;

impl AltScreen {
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
    pub(crate) fn push_formatter<F>(print: F) -> std::fmt::Result
    where
        F: FnOnce(Writer<'_>) -> std::fmt::Result,
    {
        ALT_SCREEN_EVENT_BUFFER.with_borrow_mut(|buf| print(Writer::new(&mut buf.from_subscriber)))
    }
}

pub struct Cache {
    /// Key: host name with cod color codes
    pub host_to_connect: HashMap<String, SocketAddr>,
    pub ip_to_region: HashMap<IpAddr, [u8; 2]>,
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

    pub(crate) fn with_borrow<R>(f: impl FnOnce(&Cache) -> R) -> R {
        CACHE.with_borrow(f)
    }

    pub(crate) fn with_borrow_mut<R>(f: impl FnOnce(&mut Cache) -> R) -> R {
        CACHE.with_borrow_mut(f)
    }
}

#[derive(serde::Deserialize, Debug)]
pub struct Endpoints {
    iw4_master_server: Cow<'static, str>,
    hmw_master_server: Cow<'static, str>,
    hmw_manifest_signed: Option<String>,
    hmw_pgp_public_key: Option<String>,

    // `hmw_download` only used on release builds
    #[allow(dead_code)]
    hmw_download: Cow<'static, str>,

    server_info_endpoint: Cow<'static, str>,

    #[serde(skip)]
    skip_pgp: bool,
}

impl Endpoints {
    #[allow(clippy::result_large_err)]
    fn set(endpoints: Endpoints) -> Result<(), Endpoints> {
        ENDPOINTS.with(|cell| cell.set(endpoints))
    }

    fn set_default() {
        Self::set(Self {
            iw4_master_server: Cow::Borrowed("https://master.iw4.zip/instance"),
            hmw_master_server: Cow::Borrowed("https://ms.horizonmw.org/game-servers"),
            hmw_manifest_signed: None,
            hmw_pgp_public_key: None,
            hmw_download: Cow::Borrowed("https://docs.horizonmw.org/download"),
            server_info_endpoint: Cow::Borrowed("/getInfo"),
            skip_pgp: false,
        })
        .expect("only called if failed to reach `STARTUP_INFO_URL`")
    }

    fn search_env() -> Option<String> {
        let Ok(Some(exe_dir)) = ExePath::get().map(Path::parent) else {
            return None;
        };

        let mut env_var_found = false;
        let env_path = std::env::var(ENV_REL_KEY)
            .map(|val| {
                env_var_found = true;
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
            .unwrap_or_else(|_| exe_dir.join(ENV_FILE));

        let exists = env_path.try_exists();

        let Ok(true) = exists else {
            if env_var_found {
                match exists {
                    Ok(true) => unreachable!("by outer let"),
                    Ok(false) => error!("{}, doesn't exist", env_path.display()),
                    Err(err) => error!("Env var {ENV_REL_KEY}: {err}"),
                }
            }
            return None;
        };

        Self::parse_env_file(&env_path).map_err(display::error).ok()
    }

    fn parse_env_file(path: &Path) -> io::Result<String> {
        const HOST_KEY: &str = "MASTER_SERVER_HOST";
        const PROTOCOL_KEY: &str = "MASTER_SERVER_PROTOCOL";

        let file = std::fs::read_to_string(path)?;

        let parse_field = |field| {
            file.split_once(field)
                .map(|(_, rhs)| {
                    rhs.trim_start_matches([' ', '='])
                        .split_once(char::is_whitespace)
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

        Ok(format!(
            // MARK: TODO
            // Need to fully implement a system for handling query pram
            "{protocol}://{host}/manifest?launcherVersion=1.6.1"
        ))
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
                if env_override.is_some() {
                    startup.endpoints.skip_pgp = true;
                    startup.endpoints.hmw_manifest_signed = env_override;
                }
                Self::set(startup.endpoints).expect("only valid set");
                Some(startup.version)
            }
            Err(err) => {
                AltScreen::push_message(Message::error(format!(
                    "Failed to verify startup data: {err}"
                )));
                Self::set_default();
                None
            }
        }
    }

    fn get<R>(f: impl FnOnce(&'static Endpoints) -> R) -> R {
        ENDPOINTS.with(|cell| {
            // Safety: Initialization of `OnceCell<Endpoints>` and all subsequent access to containing fields
            // happen on the same thread set by tokio "Current thread" runtime. Thus making the thread local
            // and contained `Endpoints` actually 'static.
            unsafe {
                let endpoints = cell.get().expect(
                    "tried to access endpoints before startup process or outside of the main thread",
                );

                f(std::mem::transmute::<&Endpoints, &'static Endpoints>(
                    endpoints,
                ))
            }
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

#[allow(private_bounds)]
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

pub(crate) struct AppHWND;

impl AppHWND {
    pub(crate) fn get() -> Option<HWND> {
        SELF_HWND.with(|&option| option)
    }
}

pub(crate) enum PtyAccessErr {
    ConnectionErr(Cow<'static, str>),
    UserErr(Cow<'static, str>),
}

macro_rules! inner {
    ($struct:expr) => {
        match $struct {
            PtyAccessErr::ConnectionErr(cow) => cow,
            PtyAccessErr::UserErr(cow) => cow,
        }
    };
}

impl Display for PtyAccessErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", inner!(self))
    }
}

impl From<PtyAccessErr> for Cow<'static, str> {
    fn from(value: PtyAccessErr) -> Self {
        inner!(value)
    }
}

pub(crate) struct PtyHandle;

impl PtyHandle {
    pub(crate) fn set(pty: Option<PTY>) {
        PTY_HANDLE.set(pty)
    }

    /// Prefer to use [`Self::check_connection`] where possible as it also will drop no longer alive `PTY`s.
    pub(crate) fn is_alive() -> Result<bool, Cow<'static, str>> {
        PTY_HANDLE.with_borrow(|handle| {
            let Some(handle) = handle.as_ref() else {
                return Err(Cow::Borrowed("No connection to game client is active"));
            };

            handle
                .is_alive()
                .map_err(|err| Cow::Owned(err.to_string_lossy().to_string()))
        })
    }

    pub(crate) fn check_connection() -> Result<(), Cow<'static, str>> {
        if let Err(err) = {
            match Self::is_alive() {
                Ok(true) => Ok(()),
                Ok(false) => Err(Cow::Borrowed("No connection to game client is active")),
                Err(err) => Err(err),
            }
        } {
            PTY_HANDLE.with_borrow_mut(|handle| *handle = None);
            return Err(err);
        };

        Ok(())
    }

    pub(crate) fn try_if_alive<R>(
        f: impl FnOnce(&PTY) -> Result<R, Cow<'static, str>>,
    ) -> Result<R, PtyAccessErr> {
        Self::check_connection().map_err(PtyAccessErr::ConnectionErr)?;

        PTY_HANDLE
            .with_borrow(|handle| handle.as_ref().map(f))
            .expect("early return ensures handle is some")
            .map_err(PtyAccessErr::UserErr)
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
            Self::wait_for_exit(game_name);
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

pub(crate) struct LastServerStats;

impl LastServerStats {
    /// The returned `io::Error` should be propagated as [`CmdErr::Critical`]
    ///
    /// [`CmdErr::Critical`]: crate::commands::handler::CmdErr::Critical
    pub(crate) fn display(repl: &mut ReplHandle) -> io::Result<()> {
        LAST_SERVER_STATS.with_borrow(|(source, filter, pre_process)| {
            if source.is_empty() {
                println!(
                    "{YELLOW}No previous filter data in memory, \
                    run a filter command using '--stats' to store{RESET}"
                );
                return Ok(());
            }

            let (cols, rows) = repl.terminal_size();
            let width = (cols.saturating_sub(TABLE_PADDING) as usize).max(MIN_FILTER_COLS);
            try_fit_table(repl, (cols, rows), width)?;

            println!();
            println!("{}", DisplaySourceStats(source));

            Cache::with_borrow(|cache| {
                println!(
                    "{}",
                    DisplayFilterStats(filter, pre_process, &cache.ip_to_region, width)
                );
            });

            Ok(())
        })
    }

    pub(crate) fn set(source: Vec<DisplaySourceStatsInner>, mut filter: Vec<Server>) {
        let pre_process = process_stats(&mut filter);
        LAST_SERVER_STATS.set((source, filter, pre_process));
    }
}

pub(crate) struct IDMaps;

impl IDMaps {
    /// `map_ids, game_type_ids`
    pub(crate) fn with_borrow<R>(
        f: impl FnOnce(&HashMap<&str, &'static str>, &HashMap<&str, &'static str>) -> R,
    ) -> R {
        ID_MAPS.with(|cell| {
            let (map_ids, game_type_ids) =
                cell.get_or_init(|| (HashMap::from(MAP_IDS), HashMap::from(GAME_TYPE_IDS)));
            f(map_ids, game_type_ids)
        })
    }
}

pub(crate) struct GameDisplayMap;

impl GameDisplayMap {
    pub(crate) fn with_borrow<R>(f: impl FnOnce(&HashMap<&str, &'static str>) -> R) -> R {
        GAME_ID_MAP.with(|cell| f(cell.get_or_init(|| HashMap::from(GAME_DISPLAY_NAMES))))
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
