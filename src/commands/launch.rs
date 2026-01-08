use crate::{
    CRATE_NAME, LOG_ONLY, client_with_timeout,
    commands::{
        filter::{Addressable, GetInfoMetaData, Request, Sourced, try_get_info},
        handler::{CommandContext, Message},
    },
    parse_hostname, send_msg_over, strip_ansi_private_modes,
    utils::{
        display,
        main_thread_state::{self, ThreadCopyState, pty_handle::PseudoConStatus},
    },
};

use std::{
    ffi::{OsStr, OsString, c_void},
    net::{AddrParseError, SocketAddr},
    os::windows::ffi::{OsStrExt, OsStringExt},
    path::Path,
};

use core::str;
use repl_oxide::strip_ansi;
use serde::{Deserialize, Serialize};
use tokio::sync::mpsc::Sender;
use tracing::{error, info, warn};
use windows_sys::Win32::{
    Foundation::{CloseHandle, GetLastError, HWND},
    Storage::FileSystem::{
        GetFileVersionInfoSizeW, GetFileVersionInfoW, VS_FIXEDFILEINFO, VerQueryValueW,
    },
    System::Threading::{OpenProcess, PROCESS_TERMINATE, TerminateProcess},
    UI::WindowsAndMessaging::{
        DrawMenuBar, EnableMenuItem, EnumWindows, GetClassNameA, GetSystemMenu, GetWindowTextW,
        IsWindowVisible, MF_BYCOMMAND, MF_ENABLED, MF_GRAYED, SC_CLOSE, SW_HIDE, ShowWindow,
        WNDENUMPROC,
    },
};
use winptyrs::{AgentConfig, MouseMode, PTY, PTYArgs, PTYBackend};

#[expect(non_camel_case_types, reason = "match C type name")]
type wchar_t = u16;

macro_rules! utf16_array {
    ($($c:literal),* $(,)?) => {
        [$(($c as u16)),*]
    };

    (pairs: $(($a:literal, $b:literal)),* $(,)?) => {
        [$(($a as u16, $b as u16)),*]
    };
}

macro_rules! uppercase_pairs {
    ($($c:literal),* $(,)?) => {
        [$(($c, $c.to_ascii_uppercase())),*]
    };
}

const CMP_LEN: usize = 8;

const WINDOW_NAMES_GAME: [&str; 3] = ["h2m", "hmw", "horizonmw"];
const WINDOW_CLASS_NAME_WIN11_TERMINAL: &str = "CASCADIA_HOSTING_WINDOW_CLASS";
const WINDOW_CLASS_NAME_WIN_CONSOLE_HOST: &str = "ConsoleWindowClass";
// WINDOW_CLASS_NAME_GAME_CLIENT = "H1" | WINDOW_CLASS_NAME_GAME_SPLASH_SCREEN = "H2M Splash Screen"
const WINDOW_CLASS_NAMES_GAME: [&str; 3] = ["H1", "H2M Splash Screen", "HMW Splash Screen"];
const WINDOW_CLASS_NAME_PSEUDO_CONSOLE: &str = "PseudoConsoleWindow";
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

#[inline]
fn case_insensitive_cmp_direct(line: &str) -> Option<Connection> {
    let mut chars = line.char_indices();
    let (_, first) = chars.find(|(_, ch)| ch.is_alphabetic())?;

    let (lower_fist, upper_first) = CONNECT_STR[0];
    if first != lower_fist && first != upper_first {
        return None;
    }

    for (&(lower, upper), (_, ch)) in CONNECT_STR[1..].iter().zip(chars.by_ref()) {
        if ch != lower && ch != upper {
            return None;
        }
    }

    chars.next().map(|(i, _)| Connection::Direct(i))
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct HostName {
    pub parsed: String,
    pub raw: String,
}

pub struct HostNameRequestMeta {
    pub host_name: HostName,
    pub socket_addr: Option<Result<SocketAddr, String>>,
}

impl HostNameRequestMeta {
    fn new(host_name_raw: String, socket_addr: Option<Result<SocketAddr, String>>) -> Self {
        HostNameRequestMeta {
            host_name: HostName {
                parsed: parse_hostname(&host_name_raw),
                raw: host_name_raw,
            },
            socket_addr,
        }
    }
}

enum HostRequestErr {
    AddrParseErr(AddrParseError),
    RequestErr(String),
}

impl From<AddrParseError> for HostRequestErr {
    fn from(value: AddrParseError) -> Self {
        HostRequestErr::AddrParseErr(value)
    }
}

impl From<GetInfoMetaData> for HostRequestErr {
    fn from(value: GetInfoMetaData) -> Self {
        // meta data discarded since the caller doesn't use it / avoids triggering large enum variant size diff
        HostRequestErr::RequestErr(format!(
            "Server at: {}, did not respond to a 'getInfo' request",
            value.meta.socket_addr()
        ))
    }
}

impl HostName {
    pub fn from_browser(stripped: &str, version: f64) -> Result<HostNameRequestMeta, String> {
        let (host_name, socket_addr) = if version < 1.0 {
            let host_name = stripped
                .split_once(JOINING_STR)
                .expect("`Connection::Browser` is found and client is 'original h2m', meaning `JOINING_STR` was found in the `value` array")
                .1
                .strip_suffix("...")
                .ok_or_else(|| {
                    format!("Unexpected H2M console output found. ansi_stripped_input: '{stripped}' does not end in: '...'")
                })?;

            (host_name, None)
        } else {
            let (pre, host_name) = stripped.split_once("} ").ok_or_else(|| {
                format!("Unexpected HMW console output found. ansi_stripped_input: '{stripped}', does not contain: '}} '")
            })?;
            let ip = pre
                .rsplit_once('{')
                .ok_or_else(|| format!("Unexpected HMW console output found. left_stripped_split: '{pre}', does not contain '{{'"))
                .and_then(|(_, ip_str)| {
                    ip_str.parse::<SocketAddr>()
                        .map_err(|err| format!("Failed to parse: {ip_str}, {err}"))
                });

            (host_name, Some(ip))
        };

        Ok(HostNameRequestMeta::new(host_name.to_string(), socket_addr))
    }

    async fn from_request(
        stripped: &str,
        offset: usize,
    ) -> Result<HostNameRequestMeta, HostRequestErr> {
        let socket_addr = stripped[offset..].trim().parse::<SocketAddr>()?;
        let server_info = try_get_info(
            Request::New(Sourced::Hmw(socket_addr)),
            client_with_timeout(5),
            main_thread_state::Endpoints::server_info_endpoint(),
        )
        .await?;

        Ok(HostNameRequestMeta::new(
            server_info.info.host_name,
            Some(Ok(socket_addr)),
        ))
    }
}

enum Connection {
    Browser,
    Direct(usize),
}

/// Caller must guarantee that the given `Connection` kind is valid within `line`
async fn add_to_history(msg_sender: &Sender<Message>, line: &str, kind: Connection, version: f64) {
    fn cache_insert(host_name_meta: HostNameRequestMeta) {
        main_thread_state::Cache::with_borrow_mut(|cache| {
            let mut modified = true;
            if let Some(Ok(ip)) = host_name_meta.socket_addr {
                cache
                    .host_to_connect
                    .entry(host_name_meta.host_name.raw.clone())
                    .and_modify(|cache_ip| {
                        if *cache_ip == ip {
                            modified = false;
                        } else {
                            *cache_ip = ip
                        }
                    })
                    .or_insert(ip);
            }
            if let Some(index) = cache
                .connection_history
                .iter()
                .position(|prev| prev.raw == host_name_meta.host_name.raw)
            {
                let history_last = cache.connection_history.len() - 1;
                if index != history_last {
                    let entry = cache.connection_history.remove(index);
                    cache.connection_history.push(entry);
                    modified = true;
                }
            } else {
                cache.connection_history.push(host_name_meta.host_name);
                modified = true
            };
            main_thread_state::UpdateCache::and_modify(|curr| curr || modified);
        });
    }

    debug_assert_eq!(strip_ansi(line), line);
    let res = match kind {
        Connection::Browser => match HostName::from_browser(line, version) {
            Ok(mut data) => {
                if let Some(Err(ref mut err)) = data.socket_addr {
                    send_msg_over(msg_sender, Message::error(std::mem::take(err))).await;
                }
                Ok(data)
            }
            Err(err) => Err(err),
        },
        Connection::Direct(offset) => match HostName::from_request(line, offset).await {
            Ok(data) => Ok(data),
            Err(HostRequestErr::AddrParseErr(err)) => Err(err.to_string()),
            Err(HostRequestErr::RequestErr(err)) => Err(err),
        },
    };
    match res {
        Ok(meta) => {
            info!(name: LOG_ONLY, "Connected to {}", meta.host_name.parsed);
            cache_insert(meta);
        }
        Err(err) => send_msg_over(msg_sender, Message::error(err)).await,
    }
}

impl CommandContext {
    pub fn init_listener(&self) -> Result<(), String> {
        let game_name = self.game_name();

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
                    let line = strip_ansi_private_modes(&cur);

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

    /// Caller must guarantee: `hwnd` is a valid `HWND` pointer
    pub(crate) unsafe fn toggle_close_state(&mut self, hwnd: HWND) -> Result<(), WinApiErr> {
        let state = if self.can_close_console { MF_GRAYED } else { MF_ENABLED };

        // Safety:
        // Caller provided a valid `hwnd`
        // - `brevert` set to 0 always returns a valid menu pointer
        let menu = unsafe { GetSystemMenu(hwnd, 0) };

        assert_ne!(
            // Safety:
            // - `menu` is a valid pointer see above
            // - The rest of the args are all found in the `windows_sys` crate
            unsafe { EnableMenuItem(menu, SC_CLOSE, MF_BYCOMMAND | state) },
            -1,
            "Menu item does not exist"
        );

        // Safety: Caller provided a valid `hwnd`
        if unsafe { DrawMenuBar(hwnd) } == 0 {
            // Safety: `DrawMenuBar` returned an error(0)
            return Err(unsafe { WinApiErr::get_last("DrawMenuBar") });
        }

        self.can_close_console = !self.can_close_console;
        Ok(())
    }
}

pub enum LaunchError {
    SpawnErr(OsString),
    WinApiErr(WinApiErr),
}

pub struct WinApiErr {
    pub(crate) msg: String,
    pub(crate) code: u32,
}

impl From<WinApiErr> for LaunchError {
    fn from(err: WinApiErr) -> Self {
        LaunchError::WinApiErr(err)
    }
}

impl WinApiErr {
    /// Caller must guarantee that this is only ever called if the windows api returned an error
    unsafe fn get_last(from_api_call: &'static str) -> Self {
        let (code, validity) = match unsafe { GetLastError() } {
            0 => (0, "an unknown error"),
            err => (err, "error"),
        };
        Self {
            msg: format!("Windows api {from_api_call} returned {validity}"),
            code,
        }
    }

    fn with_generic_msg(msg: String) -> Self {
        Self { msg, code: 0 }
    }

    pub(crate) fn resolve_to_closed(self) -> Option<&'static str> {
        error!("{self}");
        None
    }
    /// Return holds a placeholder string that is not to be used
    pub(crate) fn resolve_to_open(self) -> Option<&'static str> {
        error!("{self}");
        Some("don't use this value")
    }
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

/// Caller must guarantee: `hwnd` is a valid pointer
unsafe fn class_name_a_matches<F>(hwnd: HWND, f: F) -> Result<bool, String>
where
    F: FnOnce(&str) -> bool,
{
    const BUFF_A_LEN: usize = 256;
    let mut class_name = [0; BUFF_A_LEN];

    // Safety:
    // - `lpClassName`: Unicode is not defined so C type `LPSTR` is used, Windows type def for `CHAR` rust equivalent `u8`
    // - `nMaxCount` is a `c_int` that is expected to be `i32`
    let c_len = unsafe { GetClassNameA(hwnd, class_name.as_mut_ptr(), BUFF_A_LEN as i32) };

    if c_len == 0 {
        // Safety: `GetClassNameA` always returns an error when it's result is 0
        return Err(unsafe { WinApiErr::get_last("GetClassNameA") }.to_string());
    }

    Ok(f(
        str::from_utf8(&class_name[..c_len as usize]).map_err(|err| err.to_string())?
    ))
}

/// Caller must guarantee: `hwnd` is a valid pointer
unsafe fn get_window_text_w(hwnd: HWND) -> String {
    const BUFF_W_LEN: usize = 512;
    let mut title: [wchar_t; BUFF_W_LEN] = [0; BUFF_W_LEN];

    // Safety:
    // - `title` is the expected `u16` (wchar_t) byte buffer
    // - `nMaxCount` is a `c_int` that is expected to be `i32`
    let t_len = unsafe { GetWindowTextW(hwnd, title.as_mut_ptr(), BUFF_W_LEN as i32) };

    if t_len <= 0 {
        return String::new();
    }

    String::from_utf16_lossy(&title[..t_len as usize])
}

/// Caller must guarantee:
/// - If provided the EnumWindowsCallback contains no unchecked safety conditions
/// - The type of `lParam` is correctly referenced within the provided EnumWindowsCallback if provided
unsafe fn enum_windows<T>(f: WNDENUMPROC, lparam: &mut T) -> Result<(), WinApiErr> {
    // Safety: The caller guarantees that `f` is a valid callback and `lparam` is correctly typed and valid.
    // We cast `lparam` to a raw pointer for the Windows API, which is safe given these guarantees.
    if unsafe { EnumWindows(f, lparam as *mut _ as isize) } == 0 {
        // Safety: Enum windows returned error (0), it must have set an Error code
        return Err(unsafe { WinApiErr::get_last("EnumWindows") });
    }
    Ok(())
}

/// Caller must guarantee: The terminal window title is set to the crate name
pub(crate) fn get_console_hwnd() -> Result<Option<HWND>, WinApiErr> {
    let mut result = None;

    // Safety:
    // - Safety guarantees in `pseudo_window_search` hold true
    // - `lParam` is correctly referenced within `pseudo_window_search`
    unsafe { enum_windows(Some(self_window_search), &mut result) }?;

    Ok(result)
}

unsafe extern "system" fn self_window_search(hwnd: HWND, lparam: isize) -> i32 {
    // Safety: `hwnd` is a valid pointer given by the `EnumWindows` windows api
    if unsafe { IsWindowVisible(hwnd) } == 0 {
        return 1;
    }

    // Safety: `hwnd` is a valid pointer
    let window_title = unsafe { get_window_text_w(hwnd) };

    if window_title == CRATE_NAME {
        // Safety
        // - we input `lparam` as a `*mut Option<HWND>` making it safe to cast back to
        // - memory is aligned since raw pointers will always be the same size as a `isize`
        let result = unsafe { &mut *(lparam as *mut Option<HWND>) };

        if result.is_some() {
            return 1;
        }

        // Safety: `hwnd` is a valid pointer
        if unsafe {
            class_name_a_matches(hwnd, |class| {
                match class {
                    WINDOW_CLASS_NAME_WIN_CONSOLE_HOST => return true,
                    WINDOW_CLASS_NAME_WIN11_TERMINAL => {
                        info!(name: LOG_ONLY,
                            "Can not disable menu items on the windows 11 terminal during gameplay,\
                            to enable this feature set windows default terminal to 'windows console\
                            host' in developer settings."
                        )
                    }
                    class => {
                        warn!(name: LOG_ONLY, "Found unexpected windows terminal class: {class}")
                    }
                }
                false
            })
        }
        .map_err(display::log_error)
        .unwrap_or_default()
        {
            *result = Some(hwnd)
        };
    }

    1
}

pub(crate) fn hide_pseudo_console() -> Result<bool, WinApiErr> {
    let mut result = false;

    // Safety:
    // - Safety guarantees in `pseudo_window_search` hold true
    // - `lParam` is correctly referenced within `pseudo_window_search`
    unsafe { enum_windows(Some(pseudo_window_search), &mut result) }?;

    Ok(result)
}

unsafe extern "system" fn pseudo_window_search(hwnd: HWND, lparam: isize) -> i32 {
    // Safety: `hwnd` is a valid pointer given by the `EnumWindows` windows api
    if unsafe { IsWindowVisible(hwnd) } == 0 {
        return 1;
    }

    // Safety: `hwnd` is a valid pointer
    if unsafe {
        class_name_a_matches(hwnd, |class| {
            class == WINDOW_CLASS_NAME_PSEUDO_CONSOLE && get_window_text_w(hwnd).is_empty()
        })
    }
    .map_err(display::log_error)
    .unwrap_or_default()
    {
        // Safety
        // - `hwnd` is a valid pointer given by the `EnumWindows` windows api
        // - `SW_HIDE` is a flag provided by the windows api
        unsafe { ShowWindow(hwnd, SW_HIDE) };

        // Safety
        // - we input `lparam` as a `*mut bool` making it safe to cast back to
        // - memory is aligned since raw pointers will always be the same size as a `isize`
        unsafe { *(lparam as *mut bool) = true }
    }

    1
}

pub(crate) fn game_open() -> Result<Option<&'static str>, WinApiErr> {
    let mut result = None;

    // Safety:
    // - Safety guarantees in `game_window_search` hold true
    // - `lParam` is correctly referenced within `game_window_search`
    unsafe { enum_windows(Some(game_window_search), &mut result) }?;

    Ok(result)
}

unsafe extern "system" fn game_window_search(hwnd: HWND, lparam: isize) -> i32 {
    // Safety: `hwnd` is a valid pointer given by the `EnumWindows` windows api
    if unsafe { IsWindowVisible(hwnd) } == 0 {
        return 1;
    }

    // Safety: `hwnd` is a valid pointer
    let mut window_title = unsafe { get_window_text_w(hwnd) };
    window_title.make_ascii_lowercase();

    let Some(associated_game) = WINDOW_NAMES_GAME
        .into_iter()
        .find(|&game_name| window_title.contains(game_name))
    else {
        return 1;
    };

    // Safety: `hwnd` is a valid pointer
    if unsafe { class_name_a_matches(hwnd, |class| WINDOW_CLASS_NAMES_GAME.contains(&class)) }
        .map_err(display::log_error)
        .unwrap_or_default()
    {
        // Safety:
        // - We input `lparam` as a `*mut Option<&'static str>` casted to `isize`
        // - We can assign `associated_game` since we know it is a static value
        // - memory `lparam` points to lives for longer than `EnumWindows`
        // - memory is aligned since raw pointers will always be the same size as a `isize`
        let res = unsafe { &mut *(lparam as *mut Option<&str>) };

        if res.is_some() {
            return 1;
        }

        *res = Some(associated_game);
    }

    1
}

pub(crate) fn terminate_process_by_id(pid: u32) -> Result<(), WinApiErr> {
    // Safety: The correct parameters are supplied to `OpenProcess` for our use case
    let process_handle = unsafe { OpenProcess(PROCESS_TERMINATE, 0, pid) };

    if process_handle.is_null() {
        return Err(WinApiErr::with_generic_msg(format!(
            "Failed to open process with ID: {pid}",
        )));
    }

    // Safety: Early return ensures that the raw pointer given by `OpenProcess` is not null
    let result = unsafe { TerminateProcess(process_handle, 1) };

    // Safety: Early return ensures that the raw pointer given by `OpenProcess` is not null
    unsafe { CloseHandle(process_handle) };

    if result == 0 {
        // Safety: `TerminateProcess` returned an error(0)
        return Err(unsafe { WinApiErr::get_last("TerminateProcess") });
    }

    Ok(())
}

pub(crate) fn get_exe_version(path: &Path) -> Option<f64> {
    let wide_path = OsStr::new(path)
        .encode_wide()
        .chain(std::iter::once(0))
        .collect::<Vec<wchar_t>>();

    // Safety:
    // - `lpstrFilename`: Unicode is defined so C type `LPWSTR` is used, Windows type def for `wchar_t` is a unsigned short rust equivalent `u16`
    // - `GetFileVersionInfoSizeW` always sets `lpdwHandle` to 0
    let size = unsafe { GetFileVersionInfoSizeW(wide_path.as_ptr(), std::ptr::null_mut()) };
    if size == 0 {
        return None;
    }

    let mut buffer = vec![0_u8; size as usize];

    // Safety:
    // - `lpstrFilename`: Unicode is defined so C type `LPWSTR` is used, Windows type def for `wchar_t` is a unsigned short rust equivalent `u16`
    // - `GetFileVersionInfoW` ignores `dwHandle`
    // - `size` is correctly acquired from `GetFileVersionInfoSizeW` and is not empty
    // - `lpData` file version will always fit within `u8`s
    if unsafe { GetFileVersionInfoW(wide_path.as_ptr(), 0, size, buffer.as_mut_ptr() as *mut _) }
        == 0
    {
        return None;
    }

    const VER_PATH: [wchar_t; 2] = utf16_array!['\\', 0];
    let mut version_info: *mut c_void = std::ptr::null_mut();
    let mut len = 0;

    // Safety:
    // - `pBlock` accepts any and `GetFileVersionInfoW` correctly writes the version to `buffer`
    // - `ipSubBlock` correctly points to the `u16` encoded version-information path `VER_INFO`
    // - `lplpBuffer` takes a `c_void` mut pointer
    // - `puLen` is defined as a `PUINT` rust equivalent `*mut u32`
    if unsafe {
        VerQueryValueW(
            buffer.as_ptr() as *const _,
            VER_PATH.as_ptr(),
            &mut version_info,
            &mut len,
        )
    } == 0
    {
        return None;
    }

    // Safety: `VerQueryValueW` did not error so it is okay to cast do the supplied struct
    let info = unsafe { &*(version_info as *const VS_FIXEDFILEINFO) };

    parse_fixed_file_info(info)
}

#[expect(clippy::identity_op)]
fn parse_fixed_file_info(info: &VS_FIXEDFILEINFO) -> Option<f64> {
    fn trim_u16(num: u16) -> String {
        if num == 0 {
            return num.to_string();
        }
        num.to_string().trim_start_matches('0').to_string()
    }

    let major = (info.dwFileVersionMS >> 16) & 0xffff;
    let minor = (info.dwFileVersionMS >> 0) & 0xffff;
    let build = (info.dwFileVersionLS >> 16) & 0xffff;
    let revision = (info.dwFileVersionLS >> 0) & 0xffff;

    let version = format!(
        "{}.{}{}{}",
        major,
        trim_u16(minor as u16),
        trim_u16(build as u16),
        trim_u16(revision as u16)
    );
    version.parse().ok()
}
