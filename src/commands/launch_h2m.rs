use crate::{
    commands::handler::{CommandContext, Message},
    parse_hostname,
};
use serde::{Deserialize, Serialize};
use std::{
    ffi::{CStr, OsStr, OsString},
    fmt::Display,
    os::windows::ffi::{OsStrExt, OsStringExt},
    path::Path,
    sync::atomic::Ordering,
};
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
const JOIN_CHARS: &str = "Joining ";
const JOIN_BYTES: [u16; 8] = [74, 111, 105, 110, 105, 110, 103, 32];
// "Connecti"
const CONNECT_BYTES: [u16; 8] = [67, 111, 110, 110, 101, 99, 116, 105];
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

impl HostName {
    pub fn from(value: &[u16], version: f64) -> Self {
        let host_name = String::from_utf16_lossy(value);
        let host_name = strip_ansi_codes(&host_name, version);
        HostName {
            parsed: parse_hostname(&host_name),
            raw: host_name,
        }
    }
}

fn strip_ansi_codes(input: &str, version: f64) -> String {
    let re = regex::Regex::new(r"\x1b\[[0-9;]*[a-zA-Z]|\[(\?25[hl])(\])?").unwrap();
    let input = re.replace_all(input, "");

    if version < 1.0 {
        input.trim_start_matches(JOIN_CHARS).trim_end_matches('.')
    } else {
        input
            .split_once("} ")
            .map_or(input.as_ref(), |(_, suf)| suf)
    }
    .to_string()
}

fn strip_ansi_private_modes(input: &[u16]) -> String {
    let re = regex::Regex::new(r"\x1b\[(\?25[hl])(\])?").unwrap();
    re.replace_all(&String::from_utf16_lossy(input), "")
        .to_string()
}

fn add_to_history(history: &mut Vec<HostName>, wide_encode: &[u16], version: f64) {
    let host_name = HostName::from(wide_encode, version);
    if let Some(index) = history.iter().position(|prev| prev.raw == host_name.raw) {
        let history_last = history.len() - 1;
        if index != history_last {
            let entry = history.remove(index);
            history.push(entry);
        }
    } else {
        history.push(host_name);
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
            CONNECT_BYTES
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
                if byte == CARRIAGE_RETURN || byte == NEW_LINE {
                    // MARK: TODO
                    // support for direct connect strings
                    if wide_encode_buf
                        .windows(JOIN_BYTES.len())
                        .any(|window| window == connecting_bytes)
                        && !wide_encode_buf.starts_with(&ERROR_BYTES)
                    {
                        let mut cache = cache_arc.lock().await;
                        add_to_history(&mut cache.connection_history, &wide_encode_buf, version);
                        cache_needs_update.store(true, Ordering::Relaxed);
                    }
                    let line = strip_ansi_private_modes(&wide_encode_buf);
                    if !line.is_empty() {
                        // don't store lines that that _only_ contain an ansi escape command
                        // unless it is a color command then we append to next line
                        let mut chars = line.chars().peekable();
                        let mut color_cmd = None;
                        while let Some(ESCAPE_CHAR) = chars.next() {
                            let mut curr = ESCAPE_CHAR.to_string();
                            chars.find(|&c| {
                                curr.push(c);
                                c.is_alphabetic() && {
                                    if c == COLOR_CMD {
                                        color_cmd = Some(std::mem::take(&mut curr));
                                    }
                                    true
                                }
                            });
                            if chars.peek().is_none() {
                                if let Some(cmd) = color_cmd {
                                    if line != cmd {
                                        // line contains no text but multiple ansi escape commands, only add the color cmd to the next line
                                        let mut char_buf = [0; 2];
                                        let start_byte = line
                                            .chars()
                                            .next()
                                            .expect("outer if")
                                            .encode_utf16(&mut char_buf);
                                        let i = wide_encode_buf.windows(start_byte.len())
                                            .position(|window| window == start_byte)
                                            .expect("the `strip_ansi_private_modes` regex left this cmd in the buf");
                                        wide_encode_buf.truncate(i);
                                        wide_encode_buf.extend(cmd.encode_utf16());
                                    }
                                } else {
                                    wide_encode_buf.clear();
                                }
                                continue 'byte_iter;
                            }
                        }
                        console_history.push(line);
                    }
                    wide_encode_buf.clear();
                    continue;
                }
                wide_encode_buf.push(byte);
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
