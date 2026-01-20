use super::LaunchError;
use crate::{CRATE_NAME, LOG_ONLY, commands::CommandContext, utils::display};

use core::ffi::c_void;
use std::{ffi::OsStr, os::windows::ffi::OsStrExt, path::Path};

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

const WINDOW_NAMES_GAME: [&str; 3] = ["h2m", "hmw", "horizonmw"];
const WINDOW_CLASS_NAME_WIN11_TERMINAL: &str = "CASCADIA_HOSTING_WINDOW_CLASS";
const WINDOW_CLASS_NAME_WIN_CONSOLE_HOST: &str = "ConsoleWindowClass";
// WINDOW_CLASS_NAME_GAME_CLIENT = "H1" | WINDOW_CLASS_NAME_GAME_SPLASH_SCREEN = "H2M Splash Screen"
const WINDOW_CLASS_NAMES_GAME: [&str; 3] = ["H1", "H2M Splash Screen", "HMW Splash Screen"];
const WINDOW_CLASS_NAME_PSEUDO_CONSOLE: &str = "PseudoConsoleWindow";

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

impl CommandContext {
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
