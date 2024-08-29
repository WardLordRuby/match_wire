use crate::commands::handler::CommandHandle;
use std::ffi::{CStr, OsString};
use std::os::windows::ffi::OsStringExt;
use winapi::{
    shared::windef::HWND,
    um::{
        handleapi::{CloseHandle, DuplicateHandle, INVALID_HANDLE_VALUE},
        processenv::GetStdHandle,
        processthreadsapi::OpenProcess,
        winbase::{STD_INPUT_HANDLE, STD_OUTPUT_HANDLE},
        // wincon::AttachConsole,
        winnt::{HANDLE, PROCESS_DUP_HANDLE},
        winuser::{
            EnumWindows, GetClassNameA, GetWindowTextW, GetWindowThreadProcessId, IsWindowVisible,
        },
    },
};

const H2M_CONSOLE_NAMES: [&str; 2] = ["h2m-mod", "h2m-revived"];

// MARK: TODO
// 1. find correct threadProcessId - Done
// 2. get rawHandles to stdout and stdin of that threadProcessId - Not confimred working yet
// 3. filter stdout reads
// 4. create a stack of (color_coded_hostname, parsed_hostname)
// 5. create a fn to display list of parsed_hostname -> connect ip:port
// 6. add feat to connect to specify what number in the stack to connect to
// 7. send commands into stdin of the threadProcessId

pub fn try_locate_h2m_console() -> Result<(HANDLE, HANDLE), &'static str> {
    let mut result = None;
    for app_name in H2M_CONSOLE_NAMES {
        let search_title = app_name.to_string();
        unsafe {
            EnumWindows(
                Some(enum_windows_callback),
                &mut (&search_title, &mut result) as *mut _ as isize,
            );
        }
        if result.is_some() {
            break;
        }
    }
    if let Some(thread_process_id) = result {
        return get_console_handles(thread_process_id);
    }
    Err("Could not find h2m instance open")
}

pub fn reconnect(show_history: bool) -> CommandHandle {
    if show_history {
        todo!();
    }
    todo!()
}

unsafe extern "system" fn enum_windows_callback(hwnd: HWND, lparam: isize) -> i32 {
    let mut title: [u16; 512] = [0; 512];
    let length = GetWindowTextW(hwnd, title.as_mut_ptr(), title.len() as i32);

    if length <= 0 && IsWindowVisible(hwnd) == 0 {
        return 1;
    }

    let window_title = OsString::from_wide(&title[..length as usize])
        .to_string_lossy()
        .to_ascii_lowercase()
        .to_string();
    let (search_title, result) = &mut *(lparam as *mut (&String, &mut Option<u32>));

    if !window_title.contains(*search_title) {
        return 1;
    }

    let mut class_name: [i8; 256] = [0; 256];
    let length = GetClassNameA(hwnd, class_name.as_mut_ptr(), class_name.len() as i32);

    if length <= 0 {
        return 1;
    }

    // Convert the C string to a Rust &str
    let class_name_str = CStr::from_ptr(class_name.as_ptr()).to_str().unwrap_or("");

    // Check if the window class name indicates a console window
    if class_name_str == "ConsoleWindowClass" || class_name_str == "CASCADIA_HOSTING_WINDOW_CLASS" {
        let mut process_id = 0;
        GetWindowThreadProcessId(hwnd, &mut process_id);
        **result = Some(process_id);
        return 0; // Break
    }

    1 // Continue
}

fn get_console_handles(process_id: u32) -> Result<(HANDLE, HANDLE), &'static str> {
    unsafe {
        // Open the target process
        let process_handle = OpenProcess(PROCESS_DUP_HANDLE, 0, process_id);
        if process_handle.is_null() {
            return Err("h2m instance was terminated");
        }

        // Get the standard handles for the current process
        let current_stdin = GetStdHandle(STD_INPUT_HANDLE);
        let current_stdout = GetStdHandle(STD_OUTPUT_HANDLE);

        let mut target_stdin: HANDLE = INVALID_HANDLE_VALUE;
        let mut target_stdout: HANDLE = INVALID_HANDLE_VALUE;

        if DuplicateHandle(
            process_handle,
            current_stdin,
            process_handle,
            &mut target_stdin,
            0,
            1,
            winapi::um::winnt::DUPLICATE_SAME_ACCESS,
        ) == 0
        {
            return Err("Error duplicating handle");
        }

        if DuplicateHandle(
            process_handle,
            current_stdout,
            process_handle,
            &mut target_stdout,
            0,
            1,
            winapi::um::winnt::DUPLICATE_SAME_ACCESS,
        ) == 0
        {
            return Err("Error duplicating handle");
        }

        // Close the handle to the process itself
        CloseHandle(process_handle);

        Ok((target_stdin, target_stdout))
    }
}

// use std::ffi::OsStr;
// use std::os::windows::ffi::OsStrExt;
// use std::ptr::null_mut;
// use winapi::um::fileapi::{CreateFileW, OPEN_EXISTING};
// use winapi::um::handleapi::INVALID_HANDLE_VALUE;
// use winapi::um::winnt::{FILE_SHARE_READ, FILE_SHARE_WRITE, GENERIC_READ, GENERIC_WRITE, HANDLE};

// fn wide_string(s: &str) -> Vec<u16> {
//     OsStr::new(s).encode_wide().chain(Some(0)).collect()
// }

// fn get_files() -> () {
//     unsafe {
//         let conin_name = wide_string(r"\\.\conin$");
//         let conout_name = wide_string(r"\\.\conout$");

//         let stdin_handle = CreateFileW(
//             conin_name.as_ptr(),
//             GENERIC_READ | GENERIC_WRITE,
//             FILE_SHARE_READ | FILE_SHARE_WRITE,
//             null_mut(),
//             OPEN_EXISTING,
//             0,
//             null_mut(),
//         );

//         let stdout_handle = CreateFileW(
//             conout_name.as_ptr(),
//             GENERIC_READ | GENERIC_WRITE,
//             FILE_SHARE_READ | FILE_SHARE_WRITE,
//             null_mut(),
//             OPEN_EXISTING,
//             0,
//             null_mut(),
//         );

//         if stdin_handle == INVALID_HANDLE_VALUE || stdout_handle == INVALID_HANDLE_VALUE {
//             // Handle error
//         }

//         // Use stdin_handle and stdout_handle as needed
//     }
// }
