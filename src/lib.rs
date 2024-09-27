pub mod cli;
pub mod not_your_private_keys;
pub mod commands {
    pub mod filter;
    pub mod handler;
    pub mod launch_h2m;
    pub mod reconnect;
}
pub mod utils {
    pub mod input {
        pub mod completion;
        pub mod line;
        pub mod style;
    }
    pub mod caching;
    pub mod json_data;
    pub mod subscriber;
}

use clap::CommandFactory;
use cli::UserCommand;
use commands::handler::{end_forward, CommandContext};
use crossterm::{cursor, execute, terminal};
use std::{
    collections::HashSet,
    io::{self, BufRead, BufReader, Write},
    path::{Path, PathBuf},
    time::Duration,
};
use tracing::info;
use utils::{
    input::{
        line::{LineData, LineReader},
        style::{GREEN, WHITE},
    },
    json_data::Version,
};

pub const LOG_ONLY: &str = "log_only";

pub const VERSION_URL: &str =
    "https://gist.githubusercontent.com/WardLordRuby/a7b22837f3e9561f087a4b8a7ac2a905/raw/";

pub const H2M_MAX_CLIENT_NUM: i64 = 18;
pub const H2M_MAX_TEAM_SIZE: i64 = 9;

pub const REQUIRED_FILES: [&str; 3] = ["h1_mp64_ship.exe", "h2m-mod", "players2"];

pub const LOCAL_DATA: &str = "LOCALAPPDATA";
pub const CACHED_DATA: &str = "cache.json";

#[macro_export]
macro_rules! new_io_error {
    ($kind:expr, $msg:expr) => {
        Err(std::io::Error::new($kind, $msg))
    };
}

pub async fn get_latest_version() -> reqwest::Result<()> {
    let current_version = env!("CARGO_PKG_VERSION");
    let client = reqwest::Client::new();
    let version = client
        .get(VERSION_URL)
        .timeout(Duration::from_secs(6))
        .send()
        .await?
        .json::<Version>()
        .await?;
    if current_version != version.latest {
        info!("{}", version.message)
    }
    Ok(())
}

#[derive(Debug)]
pub enum Operation {
    All,
    Any,
    Count,
}

pub enum OperationResult<'a> {
    Bool(bool),
    Count((usize, HashSet<&'a str>)),
}

/// `Operation::All` and `Operation::Any` map to `OperationResult::bool(_result_)`  
/// `Operation::Count` maps to `OperationResult::Count((_num_found_, _HashSet<_&input_list_>))`  
/// when matching you will always have to `_ => unreachable()` for the return type you will never get
pub fn does_dir_contain<'a, T>(
    dir: &Path,
    operation: Operation,
    list: &'a [T],
) -> io::Result<OperationResult<'a>>
where
    T: std::borrow::Borrow<str> + std::cmp::Eq + std::hash::Hash,
{
    let entries = std::fs::read_dir(dir)?;
    let file_names = entries
        .filter_map(|entry| Some(entry.ok()?.file_name()))
        .collect::<Vec<_>>();
    let str_names = file_names
        .iter()
        .filter_map(|f| f.to_str())
        .collect::<HashSet<_>>();

    match operation {
        Operation::All => Ok(OperationResult::Bool({
            let result = list
                .iter()
                .all(|check_file| str_names.contains(check_file.borrow()));
            result
        })),
        Operation::Any => Ok(OperationResult::Bool({
            let result = list
                .iter()
                .any(|check_file| str_names.contains(check_file.borrow()));
            result
        })),
        Operation::Count => Ok(OperationResult::Count({
            let collection = list
                .iter()
                .filter(|&check_file| str_names.contains(check_file.borrow()))
                .map(|t| t.borrow())
                .collect::<HashSet<_>>();
            let num_found = collection.len();
            (num_found, collection)
        })),
    }
}

pub fn contains_required_files(exe_dir: &Path) -> io::Result<()> {
    match does_dir_contain(exe_dir, Operation::Count, &REQUIRED_FILES)
        .expect("Failed to read contents of current dir")
    {
        OperationResult::Count((count, _)) if count == REQUIRED_FILES.len() => Ok(()),
        OperationResult::Count((_, files)) => {
            if !files.contains(REQUIRED_FILES[0]) {
                return new_io_error!(
                    std::io::ErrorKind::Other,
                    format!(
                        "Move {}.exe into your 'Call of Duty Modern Warfare Remastered' directory",
                        env!("CARGO_PKG_NAME")
                    )
                );
            } else if !files.contains(REQUIRED_FILES[1]) {
                return new_io_error!(
                    std::io::ErrorKind::Other,
                    "H2M mod files not found, H2M mod files are available to download for free through the Horizon MW launcher\n\
                    https://discord.com/invite/HorizonMW"
                );
            }
            if !files.contains(REQUIRED_FILES[2]) {
                std::fs::create_dir(exe_dir.join(REQUIRED_FILES[2]))
                    .expect("Failed to create players2 folder");
                println!("{GREEN}players2 folder is missing, a new one was created{WHITE}");
            }
            Ok(())
        }
        _ => unreachable!(),
    }
}

pub async fn await_user_for_end() {
    println!("Press enter to exit...");
    let stdin = std::io::stdin();
    let mut reader = BufReader::new(stdin);
    let _ = reader.read_line(&mut String::new());
}

/// Validates local/app_dir exists and modifies input if valid
pub fn check_app_dir_exists(local: &mut PathBuf) -> io::Result<()> {
    const PREV_NAME: &str = "h2m_favorites";
    let app_name = env!("CARGO_PKG_NAME");
    let local_dir = local.clone();

    match does_dir_contain(local, Operation::Count, &[app_name, PREV_NAME]) {
        Ok(OperationResult::Count((_, files))) => {
            local.push(app_name);

            if !files.contains(app_name) {
                std::fs::create_dir(&local)?;
            }

            if files.contains(PREV_NAME) {
                std::fs::remove_dir_all(local_dir.join(PREV_NAME))?;
            }
            Ok(())
        }
        Err(err) => Err(err),
        _ => unreachable!(),
    }
}

pub fn format_panic_info(info: &std::panic::PanicInfo) -> String {
    let payload_str = if let Some(location) = info.location() {
        format!(
            "PANIC {}:{}:{}:",
            location.file(),
            location.line(),
            location.column(),
        )
    } else {
        String::from("PANIC:")
    };
    if let Some(msg) = info.payload().downcast_ref::<&str>() {
        format!("{payload_str} {msg}")
    } else if let Some(msg) = info.payload().downcast_ref::<String>() {
        format!("{payload_str} {msg}")
    } else {
        format!("{payload_str} no attached message")
    }
}

pub fn lowercase_vec(vec: &[String]) -> Vec<String> {
    vec.iter().map(|s| s.trim().to_lowercase()).collect()
}

pub fn parse_hostname(name: &str) -> String {
    const COLOR_ESCAPE_CODE: char = '^';
    let mut host_name = String::new();
    let mut chars = name.chars().peekable();
    while let Some(c) = chars.next() {
        if c == COLOR_ESCAPE_CODE {
            if chars.peek().is_some() {
                chars.next();
            }
        } else {
            host_name.push(c.to_ascii_lowercase());
        }
    }
    host_name
}

pub fn conditionally_remove_hook(ctx: &mut CommandContext, line: &mut LineReader, uid: usize) {
    line.set_prompt(LineData::default_prompt());
    line.set_completion(true);
    if let Some(callback) = line.next_callback() {
        if callback.uid() == uid {
            line.pop_callback();
        }
    }
    end_forward(ctx);
}

pub fn print_help() {
    if cursor::position().unwrap() != (0, 0) {
        println!()
    }
    UserCommand::command()
        .print_help()
        .expect("Failed to print help");
    println!();
}

pub async fn splash_screen() -> io::Result<()> {
    // font: 4Max - patorjk.com
    let text = r#"
        8b    d8    db    888888  dP""b8 88  88     Yb        dP 88 88""Yb 888888       
        88b  d88   dPYb     88   dP   `" 88  88      Yb  db  dP  88 88__dP 88__         
        88YbdP88  dP__Yb    88   Yb      888888       YbdPYbdP   88 88"Yb  88""         
        88 YY 88 dP""""Yb   88    YboodP 88  88        YP  YP    88 88  Yb 888888       "#;

    let mut stdout = std::io::stdout();

    execute!(stdout, terminal::EnterAlternateScreen)?;

    let (width, height) = terminal::size()?;

    let lines = text.lines().collect::<Vec<_>>();

    let start_y = height.saturating_sub(lines.len() as u16) / 2;

    for (i, line) in lines.iter().enumerate() {
        let start_x = width.saturating_sub(line.len() as u16) / 2;
        execute!(
            stdout,
            cursor::MoveTo(start_x, start_y + i as u16),
            crossterm::style::Print(line)
        )?;
        tokio::time::sleep(tokio::time::Duration::from_millis(160)).await;
    }

    stdout.flush()?;

    tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;
    execute!(stdout, terminal::LeaveAlternateScreen)?;

    Ok(())
}
