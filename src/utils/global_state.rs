use super::{caching::AddrMap, display};
use crate::{
    LOG_ONLY, STATUS_OK, Spinner, StartupInfo, client_with_timeout,
    commands::{
        filter::{
            Server,
            strategies::{DisplaySourceStatsInner, GAME_TYPE_IDS, MAP_IDS},
        },
        handler::{AppDetails, CommandSender, Message, ReplHandle},
        launch_h2m::{HostName, WinApiErr, game_open},
    },
    models::json_data::CacheFile,
    splash_screen,
};

use std::{
    borrow::Cow,
    cell::{Cell, OnceCell, RefCell},
    collections::HashMap,
    fmt::Display,
    io,
    net::{IpAddr, SocketAddr},
    thread::LocalKey,
    time::SystemTime,
};

use repl_oxide::ansi_code::{RESET, YELLOW};
use tracing::{error, info};
use windows_sys::Win32::Foundation::HWND;
use winptyrs::PTY;

type IDMapsInner = (
    HashMap<&'static str, &'static str>,
    HashMap<&'static str, &'static str>,
);

thread_local! {
    static GAME_CONSOLE_HISTORY: RefCell<ConsoleHistory> = const { RefCell::new(ConsoleHistory::new()) };
    static CACHE: RefCell<Cache> = panic!("Attempted to access cache prior to set");
    static UPDATE_CACHE: Cell<bool> = const { Cell::new(false) };
    static FORWARD_LOGS: Cell<bool> = const { Cell::new(false) };
    static ENDPOINTS: OnceCell<Endpoints> = const { OnceCell::new() };
    static PTY_HANDLE: RefCell<Option<PTY>> = panic!("Attempted to access PTY prior to set");
    static SELF_HWND: Option<HWND> = crate::commands::launch_h2m::get_console_hwnd()
        .map_err(crate::utils::display::error)
        .unwrap_or_default();
    static LAST_SERVER_STATS: RefCell<(Vec<DisplaySourceStatsInner>, Vec<Server>)> = const { RefCell::new((Vec::new(), Vec::new())) };
    static ID_MAPS: OnceCell<IDMapsInner> = const { OnceCell::new() };

}

pub struct Cache {
    /// Key: host name with cod color codes
    pub host_to_connect: HashMap<String, SocketAddr>,
    pub ip_to_region: HashMap<IpAddr, [u8; 2]>,
    pub connection_history: Vec<HostName>,
    pub iw4m: AddrMap,
    pub hmw: AddrMap,
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
            created: SystemTime::now(),
        }
    }
}

impl Cache {
    pub(crate) fn set(cache: Self) {
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

#[allow(dead_code)] // `hmw_download` only used on release builds
#[derive(serde::Deserialize, Debug)]
pub struct Endpoints {
    iw4_master_server: Cow<'static, str>,
    hmw_master_server: Cow<'static, str>,
    hmw_manifest: Cow<'static, str>,
    hmw_download: Cow<'static, str>,
    manifest_hash_path: Option<String>,
    server_info_endpoint: Cow<'static, str>,
}

impl Endpoints {
    #[allow(clippy::result_large_err)]
    fn set(endpoints: Endpoints) -> Result<(), Endpoints> {
        ENDPOINTS.with(|cell| cell.set(endpoints))
    }

    fn set_default<D: Display>(ctx: &'static str, err: D) -> AppDetails {
        splash_screen::push_message(Message::error(format!("{ctx}: {err}")));

        Self::set(Self {
            iw4_master_server: Cow::Borrowed("https://master.iw4.zip/instance"),
            hmw_master_server: Cow::Borrowed("https://ms.horizonmw.org/game-servers"),
            hmw_manifest: Cow::Borrowed("https://price.horizonmw.org/manifest.json"),
            hmw_download: Cow::Borrowed("https://docs.horizonmw.org/download"),
            manifest_hash_path: None,
            server_info_endpoint: Cow::Borrowed("/getInfo"),
        })
        .expect("only called if failed to reach `STARTUP_INFO_URL`");
        AppDetails::default()
    }

    pub async fn init() -> AppDetails {
        const STARTUP_INFO_URL: &str =
            "https://gist.githubusercontent.com/WardLordRuby/15920ff68ae348933636a5c18bc51709/raw";

        let client = client_with_timeout(6);
        let response = match client.get(STARTUP_INFO_URL).send().await {
            Ok(data) => data,
            Err(err) => {
                return Self::set_default("Could not reach MatchWire startup json", err);
            }
        };

        if response.status() != STATUS_OK {
            return Self::set_default("Received bad response", response.status());
        }

        match response.json::<StartupInfo>().await {
            Ok(startup) => {
                Self::set(startup.endpoints).expect("only valid set");
                AppDetails::from(startup.version)
            }
            Err(err) => Self::set_default("Failed to parse MatchWire startup json", err),
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
    pub(crate) fn hmw_manifest() -> &'static str {
        Self::get(|endpoints| &endpoints.hmw_manifest)
    }
    #[inline]
    pub(crate) fn manifest_hash_path() -> Option<&'static str> {
        Self::get(|endpoints| endpoints.manifest_hash_path.as_deref())
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
                return Err(Cow::Borrowed("No connection to H2M is active"));
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
                Ok(false) => Err(Cow::Borrowed("No connection to H2M is active")),
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
        PTY_HANDLE.with_borrow_mut(|handle| {
            let Some(game_console) = handle else {
                return;
            };

            if matches!(game_console.is_alive(), Ok(true))
                && game_open()
                    .unwrap_or_else(WinApiErr::resolve_to_closed)
                    .is_some()
                && game_console
                    .send_cmd("quit")
                    .map_err(display::log_error)
                    .is_ok()
            {
                let spinner = Spinner::new(format!("Waiting for {game_name} to close"));

                info!(name: LOG_ONLY, "{game_name}'s console accepted quit command");
                if let Err(err) = game_console.wait_for_exit() {
                    error!(name: LOG_ONLY, "{}", err.to_string_lossy())
                }

                spinner.finish();
            }

            *handle = None;
        });
    }
}

pub(crate) struct LastServerStats;

impl LastServerStats {
    pub(crate) fn display(repl: &mut ReplHandle) -> io::Result<()> {
        LAST_SERVER_STATS.with_borrow(|(source, filter)| {
            if source.is_empty() {
                println!(
                    "{YELLOW}No previous filter data in memory, \
                    run a filter command using '--stats' to store{RESET}"
                );
                return Ok(());
            }

            display::stats(repl, source, filter)
        })
    }

    pub(crate) fn set(source: Vec<DisplaySourceStatsInner>, filter: Vec<Server>) {
        LAST_SERVER_STATS.set((source, filter));
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
