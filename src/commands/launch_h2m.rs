use crate::{
    commands::{
        filter::{try_get_info, Sourced},
        handler::{CommandContext, Message},
    },
    parse_hostname,
    utils::caching::Cache,
    LOG_ONLY,
};
use serde::{Deserialize, Serialize};
use std::{
    borrow::Cow,
    ffi::{CStr, OsStr, OsString},
    fmt::Display,
    net::SocketAddr,
    os::windows::ffi::{OsStrExt, OsStringExt},
    path::Path,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
};
use tokio::sync::Mutex;
use tracing::error;
use winapi::{
    shared::{minwindef::DWORD, windef::HWND},
    um::{
        winnt::WCHAR,
        winuser::{EnumWindows, GetClassNameA, GetWindowTextW, IsWindowVisible},
        winver::{GetFileVersionInfoSizeW, GetFileVersionInfoW, VerQueryValueW},
    },
};
use winptyrs::{AgentConfig, MouseMode, PTYArgs, PTYBackend, PTY};

#[repr(C)]
#[allow(non_snake_case, non_camel_case_types)]
struct VS_FIXEDFILEINFO {
    dwSignature: DWORD,
    dwStrucVersion: DWORD,
    dwFileVersionMS: DWORD,
    dwFileVersionLS: DWORD,
    dwProductVersionMS: DWORD,
    dwProductVersionLS: DWORD,
    dwFileFlagsMask: DWORD,
    dwFileFlags: DWORD,
    dwFileOS: DWORD,
    dwFileType: DWORD,
    dwFileSubtype: DWORD,
    dwFileDateMS: DWORD,
    dwFileDateLS: DWORD,
}

const H2M_NAMES: [&str; 2] = ["h2m-mod.exe", "h2m-revived.exe"];
const H2M_WINDOW_NAME: &str = "h2m";
// console class = "ConsoleWindowClass" || "CASCADIA_HOSTING_WINDOW_CLASS"
// game class = "H1" || splash screen class = "H2M Splash Screen"
const H2M_WINDOW_CLASS_NAMES: [&str; 2] = ["H1", "H2M Splash Screen"];
// const JOIN_STR: &str = "Joining";
const JOIN_BYTES: [u16; 7] = [74, 111, 105, 110, 105, 110, 103];
// const CONNECTING_STR: &str = "Connect";
const CONNECTING_BYTES: [u16; 7] = [67, 111, 110, 110, 101, 99, 116];
const CONNECT_STR: &str = "onnect ";
const CONNECT_BYTES: [u16; 7] = [111, 110, 110, 101, 99, 116, 32];
const ERROR_BYTES: [u16; 9] = [27, 91, 51, 56, 59, 53, 59, 49, 109];
const ESCAPE_CHAR: char = '\x1b';
const COLOR_CMD: char = 'm';
const CARRIAGE_RETURN: u16 = 13;
const NEW_LINE: u16 = 10;
// const RESET_COLOR: [u16; 3] = [27, 91, 109];
// const ESCAPE: u16 = 27;
// const COLOR_CMD_BYTE: u16 = 109;

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct HostName {
    pub parsed: String,
    pub raw: String,
}

pub struct HostNameRequestMeta {
    pub host_name: HostName,
    pub socket_addr: Option<SocketAddr>,
}

impl HostNameRequestMeta {
    fn new(host_name_raw: String, socket_addr: Option<SocketAddr>) -> Self {
        HostNameRequestMeta {
            host_name: HostName {
                parsed: parse_hostname(&host_name_raw),
                raw: host_name_raw,
            },
            socket_addr,
        }
    }
}

impl HostName {
    pub fn from_browser(value: &[u16], version: f64) -> HostNameRequestMeta {
        let input_string = String::from_utf16_lossy(value);
        let stripped = strip_ansi_codes(&input_string);

        let (host_name, socket_addr) = if version < 1.0 {
            (
                stripped
                    .split_once(' ')
                    .map_or(stripped.as_ref(), |(_, suf)| suf)
                    .trim_end_matches('.')
                    .to_string(),
                None,
            )
        } else {
            let (ip_str, host_name) = match stripped.split_once("} ") {
                Some((pre, host_name)) => pre
                    .rsplit_once('{')
                    .map_or((None, host_name), |(_, ip_str)| (Some(ip_str), host_name)),
                None => return HostNameRequestMeta::new(stripped.into_owned(), None),
            };
            (host_name.to_string(), ip_str.and_then(|ip| ip.parse().ok()))
        };

        HostNameRequestMeta::new(host_name, socket_addr)
    }

    async fn from_request(value: &[u16]) -> Result<HostNameRequestMeta, String> {
        let input = String::from_utf16_lossy(value);
        let ip_str = input
            .split_once(CONNECT_STR)
            .map(|(_, suf)| suf)
            .expect("`from_request` only called when `Connection::Direct` is found, meaning `CONNECT_BYTES` were found in the `value` array")
            .trim();
        let socket_addr = ip_str
            .parse::<SocketAddr>()
            .map_err(|err| err.to_string())?;
        // `Sourced::Failed` since additional features of `try_get_info` are not used here
        let server_info = try_get_info(socket_addr, Sourced::Failed, reqwest::Client::new())
            .await
            .map_err(|meta| meta.err)?;
        let host_name = server_info.info.expect("request returned `Ok`").host_name;
        Ok(HostNameRequestMeta::new(host_name, Some(socket_addr)))
    }
}

fn strip_ansi_codes(input: &str) -> Cow<'_, str> {
    let re = regex::Regex::new(r"\x1b\[[0-9;]*[a-zA-Z]|\[(\?25[hl])(\])?").unwrap();
    re.replace_all(input, "")
}

fn strip_ansi_private_modes(input: &str) -> Cow<'_, str> {
    let re = regex::Regex::new(r"\x1b\[(\?25[hl])(\])?").unwrap();
    re.replace_all(input, "")
}

enum Connection {
    Browser,
    Direct,
}

async fn add_to_history(
    cache_arc: &Arc<Mutex<Cache>>,
    update_cache: &Arc<AtomicBool>,
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
        if let Some(ip) = host_name_meta.socket_addr {
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
            let meta = HostName::from_browser(wide_encode, version);
            cache_insert(cache_arc, update_cache, meta).await;
        }
        Connection::Direct => {
            let cache_arc = cache_arc.clone();
            let update_cache = update_cache.clone();
            let wide_encode = wide_encode.to_vec();
            tokio::task::spawn(async move {
                let meta = match HostName::from_request(&wide_encode).await {
                    Ok(data) => data,
                    Err(err) => {
                        error!(name: LOG_ONLY, "{err}");
                        return;
                    }
                };
                cache_insert(&cache_arc, &update_cache, meta).await;
            });
        }
    }
}

pub async fn initalize_listener(context: &mut CommandContext) -> Result<(), String> {
    context.check_h2m_connection().await?;

    let console_history_arc = context.h2m_console_history();
    let cache_arc = context.cache();
    let cache_needs_update = context.cache_needs_update();
    let forward_logs_arc = context.forward_logs();
    let msg_sender_arc = context.msg_sender();
    let pty = context.pty_handle().unwrap();
    let version = context.h2m_version();

    tokio::spawn(async move {
        let mut buffer = OsString::new();

        let connecting_bytes = if version < 1.0 {
            JOIN_BYTES
        } else {
            CONNECTING_BYTES
        };

        const BUFFER_SIZE: usize = 16384; // 16 KB
        const PROCESS_INTERVAL: std::time::Duration = std::time::Duration::from_millis(1500);

        tokio::time::sleep(tokio::time::Duration::from_secs(10)).await;
        'task: loop {
            tokio::time::sleep(PROCESS_INTERVAL).await;
            let handle = pty.read().await;
            if !matches!(handle.is_alive(), Ok(true)) {
                break;
            }

            let start_time = tokio::time::Instant::now();

            while start_time.elapsed() < PROCESS_INTERVAL {
                match handle.read(BUFFER_SIZE as u32, false) {
                    Ok(os_string) => {
                        if os_string.is_empty() {
                            break;
                        }
                        buffer.push(os_string);
                    }
                    Err(err) => {
                        let _ = msg_sender_arc.send(Message::Err(format!("{err:?}"))).await;
                        break 'task;
                    }
                }

                tokio::task::yield_now().await;
            }

            if buffer.is_empty() {
                continue;
            }

            let mut wide_encode_buf = Vec::new();
            let mut console_history = console_history_arc.lock().await;
            let start = console_history.len();

            'byte_iter: for byte in buffer.encode_wide() {
                if byte != CARRIAGE_RETURN && byte != NEW_LINE {
                    wide_encode_buf.push(byte);
                    continue;
                }

                let mut connect_kind = Connection::Browser;
                if wide_encode_buf
                    .windows(connecting_bytes.len())
                    .any(|window| {
                        window == connecting_bytes || {
                            let direct = window == CONNECT_BYTES;
                            if direct {
                                connect_kind = Connection::Direct;
                            }
                            direct
                        }
                    })
                    && !wide_encode_buf.starts_with(&ERROR_BYTES)
                {
                    add_to_history(
                        &cache_arc,
                        &cache_needs_update,
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
                        chars.find(|&(j, c)| {
                            c.is_alphabetic() && {
                                if c == COLOR_CMD {
                                    color_cmd = Some(&line[i..=j]);
                                }
                                true
                            }
                        });
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
                    console_history.push(line.into_owned());
                }

                wide_encode_buf.clear();
            }

            if forward_logs_arc.load(Ordering::Acquire) && start < console_history.len() {
                let msg = console_history[start..].join("\n");
                if msg_sender_arc.send(Message::Str(msg)).await.is_err() {
                    forward_logs_arc.store(false, Ordering::SeqCst);
                }
            }

            buffer = OsString::from_wide(&wide_encode_buf);
        }
        let _ = msg_sender_arc
            .send(Message::Warn(String::from(
                "No longer reading H2M console ouput",
            )))
            .await;
    });
    Ok(())
}

pub enum LaunchError {
    Running(&'static str),
    SpawnErr(OsString),
}

impl Display for LaunchError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let display = match self {
            LaunchError::Running(msg) => *msg,
            LaunchError::SpawnErr(err) => &err.to_string_lossy(),
        };
        write!(f, "{display}")
    }
}

pub fn launch_h2m_pseudo(exe_dir: &Path) -> Result<(PTY, f64), LaunchError> {
    // MARK: FIXME
    // can we figure out a way to never inherit pseudo process name
    if h2m_running() {
        return Err(LaunchError::Running("H2M is already running"));
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

    let mut conpty = PTY::new_with_backend(&pty_args, PTYBackend::ConPTY).unwrap();

    let spawned = match conpty.spawn(exe_dir.join(H2M_NAMES[0]).into(), None, None, None) {
        Ok(_) => H2M_NAMES[0],
        Err(err) => {
            conpty
                .spawn(exe_dir.join(H2M_NAMES[1]).into(), None, None, None)
                .map_err(|_| LaunchError::SpawnErr(err))?;
            H2M_NAMES[1]
        }
    };
    let spawned_path = exe_dir.join(spawned);
    let version = get_exe_version(&spawned_path).unwrap_or_else(|| {
        error!("Failed to get versoin of {spawned}");
        0.0
    });

    Ok((conpty, version))
}

pub fn h2m_running() -> bool {
    let mut result: bool = false;
    unsafe {
        EnumWindows(Some(enum_windows_callback), &mut result as *mut _ as isize);
    }
    result
}

#[allow(clippy::identity_op)]
fn get_exe_version(path: &Path) -> Option<f64> {
    let wide_path: Vec<u16> = OsStr::new(path)
        .encode_wide()
        .chain(std::iter::once(0))
        .collect();

    unsafe {
        let size = GetFileVersionInfoSizeW(wide_path.as_ptr(), std::ptr::null_mut());
        if size == 0 {
            return None;
        }

        let mut buffer: Vec<u8> = vec![0; size as usize];
        if GetFileVersionInfoW(wide_path.as_ptr(), 0, size, buffer.as_mut_ptr() as *mut _) == 0 {
            return None;
        }

        let mut version_info: *mut winapi::ctypes::c_void = std::ptr::null_mut();
        let mut len: u32 = 0;
        if VerQueryValueW(
            buffer.as_ptr() as *const _,
            "\\".encode_utf16()
                .chain(std::iter::once(0))
                .collect::<Vec<WCHAR>>()
                .as_ptr(),
            &mut version_info,
            &mut len,
        ) == 0
        {
            return None;
        }

        let info = &*(version_info as *const VS_FIXEDFILEINFO);
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
}

unsafe extern "system" fn enum_windows_callback(hwnd: HWND, lparam: isize) -> i32 {
    let mut title: [u16; 512] = [0; 512];
    let length = GetWindowTextW(hwnd, title.as_mut_ptr(), title.len() as i32);

    if length <= 0 && IsWindowVisible(hwnd) == 0 {
        return 1;
    }

    let window_title = OsString::from_wide(&title[..length as usize])
        .to_string_lossy()
        .to_ascii_lowercase();

    if !window_title.contains(H2M_WINDOW_NAME) {
        return 1;
    }

    let mut class_name: [i8; 256] = [0; 256];
    let length = GetClassNameA(hwnd, class_name.as_mut_ptr(), class_name.len() as i32);

    if length <= 0 {
        return 1;
    }

    let class_name_str = CStr::from_ptr(class_name.as_ptr()).to_str().unwrap_or("");

    // Check if the window class name indicates it is the game window or the game's splash screen
    if H2M_WINDOW_CLASS_NAMES
        .iter()
        .any(|&h2m_class| class_name_str == h2m_class)
    {
        let result = &mut *(lparam as *mut bool);
        *result = true;
        return 0; // Break
    }

    1 // Continue
}
