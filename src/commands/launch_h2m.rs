use crate::{commands::handler::CommandContext, parse_hostname};
use serde::{Deserialize, Serialize};
use std::{
    ffi::{CStr, OsString},
    os::windows::ffi::{OsStrExt, OsStringExt},
    sync::atomic::Ordering,
};
use tracing::error;
use winapi::{
    shared::windef::HWND,
    um::winuser::{EnumWindows, GetClassNameA, GetWindowTextW, IsWindowVisible},
};
use winptyrs::{AgentConfig, MouseMode, PTYArgs, PTYBackend, PTY};

const H2M_NAMES: [&str; 2] = ["h2m-mod.exe", "h2m-revived.exe"];
const H2M_WINDOW_NAMES: [&str; 2] = ["h2m-mod", "h2m-revived"];
const JOIN_CHARS: &str = "Joining ";
const JOIN_BYTES: [u16; 8] = [74, 111, 105, 110, 105, 110, 103, 32];
const CARRIAGE_RETURN: u16 = 13;
const NEW_LINE: u16 = 10;

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct HostName {
    pub parsed: String,
    pub raw: String,
}

impl From<&[u16]> for HostName {
    fn from(value: &[u16]) -> Self {
        let host_name = String::from_utf16_lossy(value);
        let host_name = strip_ansi_codes(&host_name);
        HostName {
            parsed: parse_hostname(&host_name),
            raw: host_name,
        }
    }
}

fn strip_ansi_codes(input: &str) -> String {
    let re = regex::Regex::new(r"\x1b\[[0-9;]*[a-zA-Z]").unwrap();
    re.replace_all(input, "")
        .trim_start()
        .trim_start_matches(JOIN_CHARS)
        .trim_end_matches('.')
        .to_string()
}

fn add_to_history(history: &mut Vec<HostName>, wide_encode: &[u16]) {
    let host_name = HostName::from(wide_encode);
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

pub async fn initalize_listener<'a>(context: &mut CommandContext<'a>) {
    if let Err(err) = context.check_h2m_connection().await {
        error!("{err}");
        return;
    }

    let h2m_console_history = context.h2m_console_history();
    let h2m_server_connection_history = context.h2m_server_connection_history();
    let cache_needs_update = context.cache_needs_update();
    let lock = context.pty_handle().unwrap();

    // MARK: IMPROVE
    // speed up the time it takes to read in commands | maybe use channels?
    tokio::spawn(async move {
        let mut buffer = OsString::new();

        let handle = lock.read().await;
        tokio::time::sleep(tokio::time::Duration::from_secs(10)).await;
        while let Ok(true) = handle.is_alive() {
            tokio::time::sleep(tokio::time::Duration::from_millis(1500)).await;
            match handle.read(1024, false) {
                Ok(os_string) => {
                    if os_string.is_empty() {
                        continue;
                    }
                    buffer.push(os_string);
                    let mut wide_encode_buf = Vec::new();
                    let mut wide_encode = buffer.encode_wide().peekable();
                    while let Some(byte) = wide_encode.next() {
                        if byte == NEW_LINE {
                            continue;
                        }
                        if byte == CARRIAGE_RETURN {
                            if wide_encode.peek().is_some() {
                                wide_encode.next();
                            }
                            if !wide_encode_buf.is_empty() {
                                let mut console_history = h2m_console_history.lock().await;
                                if wide_encode_buf
                                    .windows(JOIN_BYTES.len())
                                    .any(|window| window == JOIN_BYTES)
                                {
                                    let mut connection_history =
                                        h2m_server_connection_history.lock().await;
                                    add_to_history(&mut connection_history, &wide_encode_buf[..]);
                                    cache_needs_update.store(true, Ordering::Relaxed);
                                }
                                console_history.push(String::from_utf16_lossy(&wide_encode_buf));
                                wide_encode_buf.clear();
                            }
                            continue;
                        }
                        wide_encode_buf.push(byte);
                    }
                    buffer = OsString::from_wide(&wide_encode_buf);
                }
                Err(err) => error!("{err:?}"),
            }
        }
    });
}

pub async fn launch_h2m_pseudo<'a>(context: &mut CommandContext<'a>) -> Result<(), String> {
    // MARK: FIXME
    // can we figure out a way to never inherit pseudo process name
    if h2m_running() {
        return Err(String::from(
            "Close H2M and relaunch using 'launch' command",
        ));
    }

    let pty_args = PTYArgs {
        cols: 100,
        rows: 50,
        mouse_mode: MouseMode::WINPTY_MOUSE_MODE_NONE,
        timeout: 10000,
        agent_config: AgentConfig::WINPTY_FLAG_PLAIN_OUTPUT,
    };

    // MARK: FIXME
    // why does the pseudo terminal spawn with no cols or rows

    context.init_pty(PTY::new_with_backend(&pty_args, PTYBackend::ConPTY).unwrap());

    let exe_dir = context.exe_dir();
    let lock = context.pty_handle().unwrap();
    let mut conpty = lock.write().await;

    if conpty
        .spawn(exe_dir.join(H2M_NAMES[0]).into(), None, None, None)
        .is_err()
    {
        conpty
            .spawn(exe_dir.join(H2M_NAMES[1]).into(), None, None, None)
            .map_err(|err| err.to_string_lossy().to_string())?;
    }

    Ok(())
}

fn h2m_running() -> bool {
    let mut result: bool = false;
    unsafe {
        EnumWindows(Some(enum_windows_callback), &mut result as *mut _ as isize);
    }
    result
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

    if !H2M_WINDOW_NAMES
        .iter()
        .any(|h2m_name| window_title.contains(h2m_name))
    {
        return 1;
    }

    let mut class_name: [i8; 256] = [0; 256];
    let length = GetClassNameA(hwnd, class_name.as_mut_ptr(), class_name.len() as i32);

    if length <= 0 {
        return 1;
    }

    // Convert the C string to a Rust &str
    let class_name_str = CStr::from_ptr(class_name.as_ptr()).to_str().unwrap_or("");

    // Check if the window class name indicates it is the console window or game window
    // game class = "H1"
    // console class = "ConsoleWindowClass" || "CASCADIA_HOSTING_WINDOW_CLASS"
    // MARK: NOTE
    // temp fix, to get around our process being picked up as the game process
    // would like to add the console window types back in
    if class_name_str == "H1" {
        let result = &mut *(lparam as *mut bool);
        *result = true;
        return 0; // Break
    }

    1 // Continue
}
