use crate::{
    commands::{
        filter::{try_get_info, GetInfoMetaData, Request, Sourced},
        handler::{CommandContext, Message},
    },
    parse_hostname, strip_ansi_private_modes,
    utils::caching::Cache,
};

use std::{
    ffi::{c_void, OsStr, OsString},
    net::{AddrParseError, SocketAddr},
    os::windows::ffi::{OsStrExt, OsStringExt},
    path::Path,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
};

use core::str;
use repl_oxide::strip_ansi;
use serde::{Deserialize, Serialize};
use tokio::sync::{mpsc::Sender, Mutex};
use tracing::{error, trace};
use windows_sys::Win32::{
    Foundation::{GetLastError, HWND},
    Storage::FileSystem::{
        GetFileVersionInfoSizeW, GetFileVersionInfoW, VerQueryValueW, VS_FIXEDFILEINFO,
    },
    UI::WindowsAndMessaging::{EnumWindows, GetClassNameA, GetWindowTextW},
};
use winptyrs::{AgentConfig, MouseMode, PTYArgs, PTYBackend, PTY};

#[allow(non_camel_case_types)]
type wchar_t = u16;

macro_rules! utf16_array {
    ($($c:literal),* $(,)?) => {
        [$(($c as u16)),*]
    };

    (pairs: $(($a:literal, $b:literal)),* $(,)?) => {
        [$(($a as u16, $b as u16)),*]
    };
}

const GAME_WINDOW_NAMES: [&str; 3] = ["h2m", "hmw", "horizonmw"];
// console class = "ConsoleWindowClass" || "CASCADIA_HOSTING_WINDOW_CLASS"
// game class = "H1" || splash screen class = "H2M Splash Screen"
const GAME_WINDOW_CLASS_NAMES: [&str; 3] = ["H1", "H2M Splash Screen", "HMW Splash Screen"];
const JOIN_STR: &str = "Joining ";
const JOIN_BYTES: [u16; 8] = utf16_array!['J', 'o', 'i', 'n', 'i', 'n', 'g', ' '];
const CONNECTING_BYTES: [u16; 8] = utf16_array!['C', 'o', 'n', 'n', 'e', 'c', 't', 'i'];
const CONNECT_STR: &str = "connect ";
const CONNECT_BYTES: [(u16, u16); 8] = utf16_array![pairs:
    ('c', 'C'),
    ('o', 'O'),
    ('n', 'N'),
    ('n', 'N'),
    ('e', 'E'),
    ('c', 'C'),
    ('t', 'T'),
    (' ', ' '),
];
const ERROR_BYTES: [u16; 9] = utf16_array!['\x1b', '[', '3', '8', ';', '5', ';', '1', 'm'];
const ESCAPE_CHAR: char = '\x1b';
const COLOR_CMD: char = 'm';
const CARRIAGE_RETURN: u16 = '\r' as u16;
const NEW_LINE: u16 = '\n' as u16;

#[inline]
fn case_insensitve_cmp_direct(window: &[u16], kind: &mut Connection) -> bool {
    debug_assert_eq!(window.len(), CONNECT_BYTES.len());
    if window
        .iter()
        .zip(CONNECT_BYTES)
        .any(|(&byte, (lower, upper))| byte != lower && byte != upper)
    {
        return false;
    }
    *kind = Connection::Direct;
    true
}

async fn send_msg_over(sender: &Sender<Message>, message: Message) {
    sender
        .send(message)
        .await
        .unwrap_or_else(|returned| returned.0.log());
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
    fn from(mut value: GetInfoMetaData) -> Self {
        // meta data discarded since the caller doesn't use it / avoids triggering large enum variant size diff
        HostRequestErr::RequestErr(value.with_socket_addr().to_string())
    }
}

impl HostName {
    pub fn from_browser(value: &[u16], version: f64) -> Result<HostNameRequestMeta, String> {
        let stripped = strip_ansi(&String::from_utf16_lossy(value));

        let (host_name, socket_addr) = if version < 1.0 {
            let host_name = stripped
                .split_once(JOIN_STR)
                .expect("`Connection::Browser` is found and client is 'origional h2m', meaning `JOIN_BYTES` were found in the `value` array")
                .1
                .strip_suffix("...")
                .ok_or_else(|| {
                    format!("Unexpected H2M console output found. ansi_stripped_input: '{stripped}' does not end in: '...'")
                })?;

            (host_name.to_string(), None)
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

            (host_name.to_string(), Some(ip))
        };

        Ok(HostNameRequestMeta::new(host_name, socket_addr))
    }

    async fn from_request(value: &[u16]) -> Result<HostNameRequestMeta, HostRequestErr> {
        let mut input = String::from_utf16_lossy(value);
        input.make_ascii_lowercase();

        let ip_str = input
            .split_once(CONNECT_STR)
            .expect("`Connection::Direct` is found, meaning `CONNECT_BYTES` were found in the `value` array")
            .1
            .trim();
        let socket_addr = ip_str.parse::<SocketAddr>()?;
        let server_info = try_get_info(
            Request::New(Sourced::Hmw(socket_addr)),
            reqwest::Client::new(),
        )
        .await?;
        let host_name = server_info
            .info
            .expect("always `Some` when `try_get_info` is `Ok`")
            .host_name;
        Ok(HostNameRequestMeta::new(host_name, Some(Ok(socket_addr))))
    }
}

enum Connection {
    Browser,
    Direct,
}

async fn add_to_history(
    cache_arc: &Arc<Mutex<Cache>>,
    update_cache: &Arc<AtomicBool>,
    background_msg: &Sender<Message>,
    wide_encode: &[u16],
    kind: Connection,
    version: f64,
) {
    async fn cache_insert(
        cache_arc: &Arc<Mutex<Cache>>,
        update_cache: &Arc<AtomicBool>,
        host_name_meta: HostNameRequestMeta,
    ) {
        let mut cache = cache_arc.lock().await;
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
        if modified {
            update_cache.store(true, Ordering::Relaxed);
        }
    }

    match kind {
        Connection::Browser => {
            let meta = match HostName::from_browser(wide_encode, version) {
                Ok(mut data) => {
                    if let Some(Err(ref mut err)) = data.socket_addr {
                        send_msg_over(background_msg, Message::error(std::mem::take(err))).await;
                    }
                    data
                }
                Err(err) => {
                    send_msg_over(background_msg, Message::error(err)).await;
                    return;
                }
            };
            cache_insert(cache_arc, update_cache, meta).await;
        }
        Connection::Direct => {
            let cache_arc = Arc::clone(cache_arc);
            let update_cache = Arc::clone(update_cache);
            let background_msg = background_msg.clone();
            let wide_encode = wide_encode.to_vec();
            tokio::spawn(async move {
                let meta = match HostName::from_request(&wide_encode).await {
                    Ok(data) => data,
                    Err(request_err) => {
                        match request_err {
                            // NOTE: disregard `AddrParseErr` because of partial read of pseudo console input bug
                            HostRequestErr::AddrParseErr(err) => trace!("{err}"),
                            HostRequestErr::RequestErr(err) => {
                                send_msg_over(&background_msg, Message::error(err)).await;
                            }
                        }
                        return;
                    }
                };
                cache_insert(&cache_arc, &update_cache, meta).await;
            });
        }
    }
}

pub async fn initalize_listener(context: &mut CommandContext) -> Result<(), String> {
    let pty = context.check_h2m_connection().await?;

    let console_history_arc = context.h2m_console_history();
    let cache_arc = context.cache();
    let cache_needs_update = context.cache_needs_update();
    let forward_logs_arc = context.forward_logs();
    let msg_sender_arc = context.msg_sender();
    let version = context.game_version().unwrap_or(1.0);
    let game_name = context.game_name();

    tokio::spawn(async move {
        let mut buffer = OsString::new();

        let connecting_bytes = if version < 1.0 {
            JOIN_BYTES
        } else {
            CONNECTING_BYTES
        };

        const BUFFER_SIZE: u32 = 16384; // 16 KB
        const PROCESS_INTERVAL: std::time::Duration = std::time::Duration::from_secs(3);

        tokio::time::sleep(tokio::time::Duration::from_secs(10)).await;
        'task: loop {
            tokio::time::sleep(PROCESS_INTERVAL).await;
            let handle = pty.read().await;
            if !matches!(handle.is_alive(), Ok(true)) {
                break;
            }

            let start_time = tokio::time::Instant::now();

            while start_time.elapsed() < PROCESS_INTERVAL {
                match handle.read(BUFFER_SIZE, false) {
                    Ok(os_string) => {
                        if os_string.is_empty() {
                            break;
                        }
                        buffer.push(os_string);
                    }
                    Err(err) => {
                        send_msg_over(
                            &msg_sender_arc,
                            Message::error(err.to_string_lossy().to_string()),
                        )
                        .await;
                        break 'task;
                    }
                }

                tokio::task::yield_now().await;
            }

            if buffer.is_empty() {
                continue;
            }

            let mut wide_encode_buf = Vec::new();
            let mut console = console_history_arc.lock().await;

            'byte_iter: for byte in buffer.encode_wide() {
                if byte != CARRIAGE_RETURN && byte != NEW_LINE {
                    wide_encode_buf.push(byte);
                    continue;
                }

                let mut connect_kind = Connection::Browser;
                if !wide_encode_buf.starts_with(&ERROR_BYTES)
                    && wide_encode_buf
                        .windows(connecting_bytes.len())
                        .any(|window| {
                            window == connecting_bytes
                                || case_insensitve_cmp_direct(window, &mut connect_kind)
                        })
                {
                    add_to_history(
                        &cache_arc,
                        &cache_needs_update,
                        &msg_sender_arc,
                        &wide_encode_buf,
                        connect_kind,
                        version,
                    )
                    .await;
                }

                let cur = String::from_utf16_lossy(&wide_encode_buf);
                let line = strip_ansi_private_modes(&cur);
                if !line.is_empty() {
                    // don't store lines that that _only_ contain ansi escape commands,
                    // unless a color command is found then append it to the next line
                    let mut chars = line.char_indices().peekable();
                    let mut color_cmd = None;
                    while let Some((i, ESCAPE_CHAR)) = chars.next() {
                        if let Some((j, c)) = chars.find(|(_, c)| c.is_alphabetic()) {
                            if c == COLOR_CMD {
                                color_cmd = Some(&line[i..=j]);
                            }
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
                    console.history.push(line.into_owned());
                }

                wide_encode_buf.clear();
            }

            let last = console.last.load(Ordering::Relaxed);
            if forward_logs_arc.load(Ordering::Acquire) && last < console.history.len() {
                let msg = console.history[last..].join("\n");
                if msg_sender_arc.send(Message::str(msg)).await.is_err() {
                    forward_logs_arc.store(false, Ordering::SeqCst);
                } else {
                    console.last.store(console.history.len(), Ordering::SeqCst);
                }
            }

            buffer = OsString::from_wide(&wide_encode_buf);
        }
        send_msg_over(
            &msg_sender_arc,
            Message::warn(format!("No longer reading {game_name} console ouput")),
        )
        .await;
    });
    Ok(())
}

pub enum LaunchError {
    GameRunning(&'static str),
    SpawnErr(OsString),
    WinApiErr((&'static str, u32)),
}

impl LaunchError {
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

pub fn launch_h2m_pseudo(game_path: &Path) -> Result<PTY, LaunchError> {
    // MARK: FIXME
    // can we figure out a way to never inherit pseudo process name
    if let Some(game_name) = game_open()? {
        return Err(LaunchError::GameRunning(game_name));
    }

    let pty_args = PTYArgs {
        cols: 250,
        rows: 50,
        mouse_mode: MouseMode::WINPTY_MOUSE_MODE_NONE,
        timeout: 20000,
        agent_config: AgentConfig::WINPTY_FLAG_PLAIN_OUTPUT,
    };

    // MARK: FIXME
    // why does the pseudo terminal spawn with no cols or rows

    let mut conpty =
        PTY::new_with_backend(&pty_args, PTYBackend::ConPTY).map_err(LaunchError::SpawnErr)?;

    conpty
        .spawn(game_path.into(), None, None, None)
        .map_err(LaunchError::SpawnErr)?;

    Ok(conpty)
}

pub(crate) fn game_open() -> Result<Option<&'static str>, LaunchError> {
    let mut result = "";

    // Saftey:
    // - saftey guarantees in `enum_windows_callback` hold true
    // - `lParam` is a controlled type by us
    if unsafe { EnumWindows(Some(enum_windows_callback), &mut result as *mut _ as isize) } == 0 {
        // Saftey: Enum windows returned error (0), it must have set an Error code
        match unsafe { GetLastError() } {
            0 => (),
            err => {
                return Err(LaunchError::WinApiErr((
                    "Windows api EnumWindows returned error",
                    err,
                )));
            }
        }
    }
    Ok((!result.is_empty()).then_some(result))
}

#[allow(clippy::identity_op)]
pub(crate) fn get_exe_version(path: &Path) -> Option<f64> {
    let wide_path = OsStr::new(path)
        .encode_wide()
        .chain(std::iter::once(0))
        .collect::<Vec<wchar_t>>();

    // Saftey:
    // - `lpstrFilename`: Unicode is defined so C type `LPWSTR` is used, Windows type def for `wchar_t` is a unsigned short rust equivalent `u16`
    // - `GetFileVersionInfoSizeW` always sets `lpdwHandle` to 0
    let size = unsafe { GetFileVersionInfoSizeW(wide_path.as_ptr(), std::ptr::null_mut()) };
    if size == 0 {
        return None;
    }

    let mut buffer = vec![0_u8; size as usize];

    // Saftey:
    // - `lpstrFilename`: Unicode is defined so C type `LPWSTR` is used, Windows type def for `wchar_t` is a unsigned short rust equivalent `u16`
    // - `GetFileVersionInfoW` ignores `dwHandle`
    // - `size` is correctly aquired from `GetFileVersionInfoSizeW` and is not empty
    // - `lpData` file version will always fit within `u8`s
    if unsafe { GetFileVersionInfoW(wide_path.as_ptr(), 0, size, buffer.as_mut_ptr() as *mut _) }
        == 0
    {
        return None;
    }

    const VER_PATH: [wchar_t; 2] = ['\\' as u16, 0];
    let mut version_info: *mut c_void = std::ptr::null_mut();
    let mut len = 0;

    // Saftey:
    // - `pBlock` accepts any and `GetFileVersionInfoW` correctly writes the verson to `buffer`
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

    // Saftey: `VerQueryValueW` did not error so it is okay to cast do the supplied struct
    let info = unsafe { &*(version_info as *const VS_FIXEDFILEINFO) };

    let major = (info.dwFileVersionMS >> 16) & 0xffff;
    let minor = (info.dwFileVersionMS >> 0) & 0xffff;
    let build = (info.dwFileVersionLS >> 16) & 0xffff;
    let revision = (info.dwFileVersionLS >> 0) & 0xffff;

    let trim_u16 = |num: u16| -> String {
        if num == 0 {
            "0".to_string()
        } else {
            num.to_string().trim_start_matches('0').to_string()
        }
    };

    let version = format!(
        "{}.{}{}{}",
        major,
        trim_u16(minor as u16),
        trim_u16(build as u16),
        trim_u16(revision as u16)
    );
    version.parse().ok()
}

unsafe extern "system" fn enum_windows_callback(hwnd: HWND, lparam: isize) -> i32 {
    let mut title: [wchar_t; 512] = [0; 512];

    // Saftey:
    // - `title` is the expected `u16` byte buffer
    // - `nMaxCount` is a `c_int` that is expected to be `i32`
    let t_len = unsafe { GetWindowTextW(hwnd, title.as_mut_ptr(), title.len() as i32) };

    if t_len <= 0 {
        return 1;
    }

    let mut window_title = String::from_utf16_lossy(&title[..t_len as usize]);
    window_title.make_ascii_lowercase();

    let Some(associated_game) = GAME_WINDOW_NAMES
        .into_iter()
        .find(|&game_name| window_title.contains(game_name))
    else {
        return 1;
    };

    let mut class_name = [0; 256];

    // Saftey:
    // - `lpClassName`: Unicode is not defined so C type `LPSTR` is used, Windows type def for `CHAR` rust equivalent `u8`
    // - `nMaxCount` is a `c_int` that is expected to be `i32`
    let c_len = unsafe { GetClassNameA(hwnd, class_name.as_mut_ptr(), class_name.len() as i32) };

    if c_len <= 0 {
        return 1;
    }

    let class_name_str = str::from_utf8(&class_name[..c_len as usize]).unwrap_or_default();

    // Check if the window class name indicates it is the game window or the game's splash screen
    if GAME_WINDOW_CLASS_NAMES
        .iter()
        .any(|&h2m_class| class_name_str == h2m_class)
    {
        // Saftey:
        // - We input `lparam` as a `*mut &'static str` casted to `isize`
        // - We can assign `associated_game` since we know it is a static value
        // - memory `lparam` points to lives for longer than `EnumWindows`
        // - memory is aligned since raw pointers will always be the same size as a `isize`
        let res = unsafe { &mut *(lparam as *mut &str) };

        if !res.is_empty() {
            return 1;
        }

        *res = associated_game;
    }

    1
}
