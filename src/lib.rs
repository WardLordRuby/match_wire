pub mod cli;
pub mod command_scheme;
pub mod location_api_key;
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
    pub mod display;
    pub mod json_data;
    pub mod subscriber;
}

use clap::CommandFactory;
use cli::UserCommand;
use commands::{handler::AppDetails, launch_h2m::get_exe_version};
use crossterm::{cursor, execute, terminal};
use sha2::{Digest, Sha256};
use std::{
    borrow::Cow,
    collections::HashSet,
    io::{self, BufRead, BufReader, Read, Write},
    path::{Path, PathBuf},
    time::Duration,
};
use utils::{
    input::style::{GREEN, RED, WHITE},
    json_data::Version,
};

pub const LOG_ONLY: &str = "log_only";

pub const VERSION_URL: &str =
    "https://gist.githubusercontent.com/WardLordRuby/a7b22837f3e9561f087a4b8a7ac2a905/raw/";
const HMW_LATEST_URL: &str = "https://price.horizonmw.org/manifest.json";

pub const H2M_MAX_CLIENT_NUM: i64 = 18;
pub const H2M_MAX_TEAM_SIZE: i64 = 9;

pub const REQUIRED_FILES: [&str; 5] = [
    "h1_mp64_ship.exe",
    "h2m-mod",
    "players2",
    "h2m-mod.exe",
    "h2m-revived.exe",
];

pub const LOCAL_DATA: &str = "LOCALAPPDATA";
pub const CACHED_DATA: &str = "cache.json";

#[macro_export]
macro_rules! new_io_error {
    ($kind:expr, $msg:expr) => {
        Err(std::io::Error::new($kind, $msg))
    };
}

#[macro_export]
macro_rules! break_if {
    ($expr:expr, is_some_err) => {
        if let Some(Err(err)) = $expr {
            error!("{err}");
            break;
        }
    };

    ($expr:expr, is_err) => {
        if let Err(err) = $expr {
            error!("{err}");
            break;
        }
    };
}

pub async fn get_latest_version() -> reqwest::Result<AppDetails> {
    let client = reqwest::Client::new();
    let version = client
        .get(VERSION_URL)
        .timeout(Duration::from_secs(6))
        .send()
        .await?
        .json::<Version>()
        .await?;
    Ok(AppDetails::from(version))
}

pub async fn get_latest_hmw_hash() -> reqwest::Result<String> {
    let client = reqwest::Client::new();
    let latest = client
        .get(HMW_LATEST_URL)
        .timeout(Duration::from_secs(6))
        .send()
        .await?;
    // MARK: TODO
    // .json::<todo!("need to model the manifest.json")>()
    // .await?;
    todo!("return desired hash")
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

pub fn contains_required_files(exe_dir: &Path) -> Result<PathBuf, &'static str> {
    match does_dir_contain(exe_dir, Operation::Count, &REQUIRED_FILES)
        .expect("Failed to read contents of current dir")
    {
        OperationResult::Count((_, files)) => {
            if !files.contains(REQUIRED_FILES[0]) {
                return Err(concat!(
                    "Move ",
                    env!("CARGO_PKG_NAME"),
                    ".exe into your 'Call of Duty Modern Warfare Remastered' directory",
                ));
            }
            if !files.contains(REQUIRED_FILES[1]) {
                return Err(
                    "H2M mod files not found, H2M mod files are available to download for free through the Horizon MW launcher\n\
                    https://discord.com/invite/HorizonMW"
                );
            }
            let found_game = if files.contains(REQUIRED_FILES[3]) {
                REQUIRED_FILES[3]
            } else if files.contains(REQUIRED_FILES[4]) {
                REQUIRED_FILES[4]
            } else {
                return Err(
                    "h2m-mod.exe not found, H2M mod files are available to download for free through the Horizon MW launcher\n\
                    https://discord.com/invite/HorizonMW"
                );
            };
            if !files.contains(REQUIRED_FILES[2]) {
                std::fs::create_dir(exe_dir.join(REQUIRED_FILES[2]))
                    .expect("Failed to create players2 folder");
                println!("{GREEN}players2 folder is missing, a new one was created{WHITE}");
            }
            Ok(exe_dir.join(found_game))
        }
        _ => unreachable!(),
    }
}

fn hash_file_hex(path: &Path) -> io::Result<String> {
    let file = std::fs::File::open(path)?;
    let mut reader = BufReader::new(file);
    let mut hasher = Sha256::new();
    let mut buffer = [0; 8192];

    loop {
        let bytes_read = reader.read(&mut buffer)?;
        if bytes_read == 0 {
            break;
        }
        hasher.update(&buffer[..bytes_read]);
    }

    Ok(format!("{:x}", hasher.finalize()))
}

pub fn exe_details(game_exe_path: &Path) -> (Option<f64>, Option<String>) {
    let version = get_exe_version(game_exe_path).map(Some).unwrap_or_else(|| {
        eprintln!(
            "{RED}Failed to get version of {}{WHITE}",
            game_exe_path
                .file_name()
                .expect("input was not modified")
                .to_string_lossy()
        );
        None
    });
    let hash = hash_file_hex(game_exe_path)
        .map(Some)
        .unwrap_or_else(|err| {
            eprintln!(
                "{RED}{err}, input file_name: {}{WHITE}",
                game_exe_path
                    .file_name()
                    .expect("input was not modified")
                    .to_string_lossy()
            );
            None
        });
    (version, hash)
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

pub fn strip_ansi_sequences(input: &str) -> Cow<'_, str> {
    let re =
        regex::Regex::new(r"\x1b\[[0-9;]*[a-zA-Z]|\x1b\[\?(?:25[hl]|47[hl]|1049[hl])").unwrap();
    re.replace_all(input, "")
}

pub fn strip_ansi_private_modes(input: &str) -> Cow<'_, str> {
    let re = regex::Regex::new(r"\x1b\[\?(?:25[hl]|47[hl]|1049[hl])").unwrap();
    re.replace_all(input, "")
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
