use crate::{commands::handler::CommandContext, parse_hostname};
use std::{
    ffi::{CStr, OsString},
    os::windows::ffi::{OsStrExt, OsStringExt},
    path::Path,
    sync::{atomic::Ordering, Arc},
};
use tracing::error;
use winapi::{
    shared::windef::HWND,
    um::winuser::{EnumWindows, GetClassNameA, GetWindowTextW, IsWindowVisible},
};
use winptyrs::{AgentConfig, MouseMode, PTYArgs, PTYBackend, PTY};

const H2M_NAMES: [&str; 2] = ["h2m-mod.exe", "h2m-revived.exe"];
const H2M_WINDOW_NAMES: [&str; 2] = ["h2m-mod", "h2m-revived"];
const JOIN_BYTES: [u16; 8] = [74, 111, 105, 110, 105, 110, 103, 32];
const CARRIAGE_RETURN: u16 = 13;
const NEW_LINE: u16 = 10;

#[derive(Debug)]
pub struct HostName {
    pub parsed: String,
    pub raw: String,
}

impl From<&[u16]> for HostName {
    fn from(value: &[u16]) -> Self {
        let host_name = String::from_utf16_lossy(&value[JOIN_BYTES.len()..]);
        HostName {
            parsed: parse_hostname(&host_name),
            raw: host_name,
        }
    }
}

pub fn initalize_listener(handle: Arc<PTY>, command_context: &CommandContext) {
    let listening = command_context.connected_to_pseudoterminal();
    if listening.load(Ordering::Relaxed) {
        error!("Still listening to H2M console events");
        return;
    }
    let h2m_console_history = command_context.h2m_console_history();
    let h2m_server_connection_history = command_context.h2m_server_connection_history();
    tokio::spawn(async move {
        let mut buffer = OsString::new();

        while let Ok(true) = handle.is_alive() {
            tokio::time::sleep(tokio::time::Duration::from_secs(4)).await;
            match handle.read(4000, false) {
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
                        // MARK: DEBUG
                        // make sure we properly parse each line and remove color code
                        // create tests for this
                        if byte == CARRIAGE_RETURN {
                            if wide_encode.peek().is_some() {
                                wide_encode.next();
                            }
                            if !wide_encode_buf.is_empty() {
                                let mut console_history = h2m_console_history.blocking_lock();
                                let mut connection_history =
                                    h2m_server_connection_history.blocking_lock();
                                if wide_encode_buf.starts_with(&JOIN_BYTES) {
                                    connection_history.push(HostName::from(&wide_encode_buf[..]));
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
        // ideally we should make our handle no longer accessable here
        listening.store(false, Ordering::SeqCst)
    });
}

pub fn launch_h2m_pseudo(exe_dir: &Path) -> Result<PTY, String> {
    let pty_args = PTYArgs {
        cols: 100,
        rows: 50,
        mouse_mode: MouseMode::WINPTY_MOUSE_MODE_NONE,
        timeout: 10000,
        agent_config: AgentConfig::WINPTY_FLAG_PLAIN_OUTPUT,
    };

    // MARK: FIX
    // why does the pseudo terminal spawn with no cols or rows
    let mut conpty = PTY::new_with_backend(&pty_args, PTYBackend::ConPTY).unwrap();
    if conpty
        .spawn(exe_dir.join(H2M_NAMES[0]).into(), None, None, None)
        .is_err()
    {
        conpty
            .spawn(exe_dir.join(H2M_NAMES[1]).into(), None, None, None)
            .map_err(|err| err.to_string_lossy().to_string())?;
    }

    Ok(conpty)
}

pub fn h2m_running() -> bool {
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
    if class_name_str == "ConsoleWindowClass"
        || class_name_str == "CASCADIA_HOSTING_WINDOW_CLASS"
        || class_name_str == "H1"
    {
        let result = &mut *(lparam as *mut bool);
        *result = true;
        return 0; // Break
    }

    1 // Continue
}
