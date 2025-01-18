use crate::{
    cli::{CacheCmd, Command, Filters, UserCommand},
    commands::{
        filter::build_favorites,
        launch_h2m::{h2m_running, initalize_listener, launch_h2m_pseudo, LaunchError},
    },
    exe_details,
    utils::{
        caching::{build_cache, write_cache, Cache},
        display::{ConnectionHelp, DisplayLogs, HmwUpdateHelp},
        input::{
            line::{
                AsyncCallback, EventLoop, HookUID, InitLineCallback, InputEventHook, InputHook,
                InputHookErr, LineData, Print,
            },
            style::{RED, WHITE, YELLOW},
        },
        json_data::Version,
    },
    CACHED_DATA, LOG_ONLY, REQUIRED_FILES,
};
use clap::Parser;
use crossterm::{
    event::{Event, KeyCode, KeyEvent, KeyModifiers},
    terminal,
};
use std::{
    borrow::Cow,
    ffi::OsString,
    fmt::Display,
    io::Stdout,
    path::{Path, PathBuf},
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
};
use tokio::{
    sync::{
        mpsc::{channel, Receiver, Sender},
        Mutex, RwLock, RwLockReadGuard,
    },
    task::JoinError,
};
use tracing::{error, info, warn};
use winptyrs::PTY;

pub enum Message {
    Str(Cow<'static, str>),
    Info(Cow<'static, str>),
    Err(Cow<'static, str>),
    Warn(Cow<'static, str>),
}

impl Print for Message {
    fn print(&self) {
        match self {
            Self::Str(msg) => println!("{msg}"),
            Self::Info(msg) => info!("{msg}"),
            Self::Warn(msg) => warn!("{msg}"),
            Self::Err(msg) => error!("{msg}"),
        }
    }
}

impl Message {
    #[inline]
    pub fn str<T: Into<Cow<'static, str>>>(value: T) -> Self {
        Self::Str(value.into())
    }
    #[inline]
    pub fn info<T: Into<Cow<'static, str>>>(value: T) -> Self {
        Self::Info(value.into())
    }
    #[inline]
    pub fn error<T: Into<Cow<'static, str>>>(value: T) -> Self {
        Self::Err(value.into())
    }
    #[inline]
    pub fn warn<T: Into<Cow<'static, str>>>(value: T) -> Self {
        Self::Warn(value.into())
    }
}

impl Display for Message {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use crate::utils::input::style::{GREEN, RED, WHITE, YELLOW};

        let (line_color, msg, reset_color): (&str, &str, &str) = match self {
            Self::Str(msg) => ("", msg, ""),
            Self::Info(msg) => (GREEN, msg, WHITE),
            Self::Err(msg) => (RED, msg, WHITE),
            Self::Warn(msg) => (YELLOW, msg, WHITE),
        };

        writeln!(f, "{line_color}{msg}{reset_color}")
    }
}

pub struct GameDetails {
    pub path: PathBuf,
    pub game_name: Cow<'static, str>,
    pub version: Option<f64>,
    pub hash_curr: Option<String>,
    pub hash_latest: Option<String>,
}

impl GameDetails {
    fn set_game_name(path: &Path) -> Cow<'static, str> {
        let file_name = path
            .file_name()
            .expect("path points to exe")
            .to_string_lossy();

        match file_name.as_ref() {
            n if n == REQUIRED_FILES[6] || n == REQUIRED_FILES[5] => Cow::Borrowed("HMW"),
            n if n == REQUIRED_FILES[3] || n == REQUIRED_FILES[1] || n == REQUIRED_FILES[4] => {
                Cow::Borrowed("H2M")
            }
            _ => Cow::Owned(file_name.into_owned()),
        }
    }

    pub fn default(exe_dir: &Path) -> Self {
        GameDetails {
            path: exe_dir.join(REQUIRED_FILES[6]),
            game_name: Cow::Borrowed("HMW"),
            version: None,
            hash_curr: None,
            hash_latest: None,
        }
    }

    pub fn new(path: PathBuf, version: Option<f64>, hash_curr: Option<String>) -> Self {
        GameDetails {
            game_name: GameDetails::set_game_name(&path),
            path,
            version,
            hash_curr,
            hash_latest: None,
        }
    }

    pub fn game_file_name(&self) -> Cow<'_, str> {
        self.path
            .file_name()
            .expect("was not modified since it was set")
            .to_string_lossy()
    }

    fn update(&mut self, from: (Option<f64>, Option<String>)) {
        if from.0.is_some() {
            self.version = from.0;
        }
        if from.1.is_some() {
            self.hash_curr = from.1;
        }
    }
}

pub struct AppDetails {
    pub ver_curr: &'static str,
    pub ver_latest: Option<String>,
    pub update_msg: Option<String>,
}

impl Default for AppDetails {
    fn default() -> Self {
        AppDetails {
            ver_curr: env!("CARGO_PKG_VERSION"),
            ver_latest: None,
            update_msg: None,
        }
    }
}

impl From<Version> for AppDetails {
    fn from(value: Version) -> Self {
        AppDetails {
            ver_curr: env!("CARGO_PKG_VERSION"),
            ver_latest: Some(value.latest),
            update_msg: Some(value.message),
        }
    }
}

type LaunchResult = Result<Result<PTY, LaunchError>, JoinError>;
type AppVersionResult = Result<reqwest::Result<AppDetails>, JoinError>;
type HmwHashResult = Result<reqwest::Result<Result<String, &'static str>>, JoinError>;

pub enum CommandHandle {
    Processed,
    InsertHook(InputHook<CommandContext, Stdout>),
    Exit,
}

pub struct CommandContext {
    cache: Arc<Mutex<Cache>>,
    cache_needs_update: Arc<AtomicBool>,
    forward_logs: Arc<AtomicBool>,
    h2m_console_history: Arc<Mutex<Vec<String>>>,
    pty_handle: Option<Arc<RwLock<PTY>>>,
    local_dir: Option<PathBuf>,
    msg_sender: Arc<Sender<Message>>,
    game: GameDetails,
    app: AppDetails,
}

impl CommandContext {
    pub fn new(
        game_launch: LaunchResult,
        app_ver: AppVersionResult,
        hmw_hash: HmwHashResult,
        cache: Result<Cache, JoinError>,
        mut game: GameDetails,
        local_dir: Option<PathBuf>,
    ) -> (Self, Receiver<Message>, Receiver<bool>, bool) {
        let (handle, try_start_listener) = if let Ok(Ok(handle)) = game_launch {
            (Some(handle), true)
        } else {
            let err = match game_launch {
                Err(join_err) => join_err.to_string(),
                Ok(Err(launch_err)) => launch_err.to_string(),
                Ok(Ok(_)) => unreachable!("by happy path"),
            };
            error!("Could not launch H2M as child process: {err}");
            println!("{ConnectionHelp}");
            (None, false)
        };

        let app = if let Ok(Ok(app)) = app_ver {
            if let (Some(latest), Some(msg)) = (&app.ver_latest, &app.update_msg) {
                if app.ver_curr != latest {
                    info!("{msg}")
                }
            }
            app
        } else {
            let err = match app_ver {
                Err(join_err) => join_err.to_string(),
                Ok(Err(reqwest_err)) => reqwest_err.to_string(),
                Ok(Ok(_)) => unreachable!("by happy path"),
            };
            error!("Could not get latest MatchWire version: {err}");
            AppDetails::default()
        };

        if let Ok(Ok(Ok(hash_latest))) = hmw_hash {
            if let Some(ref hash_curr) = game.hash_curr {
                if hash_curr != &hash_latest {
                    info!("{HmwUpdateHelp}")
                }
            }
            game.hash_latest = Some(hash_latest);
        } else {
            let err = match hmw_hash {
                Ok(Ok(Err(err))) => Cow::Borrowed(err),
                Ok(Err(err)) => Cow::Owned(err.to_string()),
                Err(err) => Cow::Owned(err.to_string()),
                Ok(Ok(Ok(_))) => unreachable!("by happy path"),
            };
            error!("Could not get latest HMW version: {err}");
        };

        let cache_needs_update = Arc::new(AtomicBool::new(false));
        let (update_cache_tx, update_cache_rx) = channel(20);

        tokio::spawn({
            let cache_needs_update = Arc::clone(&cache_needs_update);
            async move {
                loop {
                    tokio::time::sleep(tokio::time::Duration::from_secs(240)).await;
                    if cache_needs_update
                        .compare_exchange(true, false, Ordering::Acquire, Ordering::SeqCst)
                        .is_ok()
                        && update_cache_tx.send(true).await.is_err()
                    {
                        break;
                    }
                }
            }
        });

        let (message_tx, message_rx) = channel(50);

        (
            CommandContext {
                cache: Arc::new(Mutex::new(cache.unwrap_or_else(|err| {
                    error!("Critical error building cache, could not populate cache");
                    error!(name: LOG_ONLY, "{err:?}");
                    Cache::default()
                }))),
                msg_sender: Arc::new(message_tx),
                app,
                game,
                local_dir,
                pty_handle: handle.map(|pty| Arc::new(RwLock::new(pty))),
                cache_needs_update,
                forward_logs: Arc::new(AtomicBool::new(false)),
                h2m_console_history: Arc::new(Mutex::new(Vec::<String>::new())),
            },
            message_rx,
            update_cache_rx,
            try_start_listener,
        )
    }
    #[inline]
    pub fn cache(&self) -> Arc<Mutex<Cache>> {
        Arc::clone(&self.cache)
    }
    #[inline]
    pub fn cache_needs_update(&self) -> Arc<AtomicBool> {
        Arc::clone(&self.cache_needs_update)
    }
    #[inline]
    pub fn forward_logs(&self) -> Arc<AtomicBool> {
        Arc::clone(&self.forward_logs)
    }
    pub async fn check_h2m_connection(&mut self) -> Result<Arc<RwLock<PTY>>, Cow<'static, str>> {
        let Some(ref lock) = self.pty_handle else {
            return Err(Cow::Borrowed("No Pseudoconsole set"));
        };
        let handle = lock.read().await;
        match handle.is_alive() {
            Ok(true) => Ok(Arc::clone(lock)),
            Ok(false) => Err(Cow::Borrowed("No connection to H2M is active")),
            Err(err) => {
                drop(handle);
                self.pty_handle = None;
                Err(Cow::Owned(err.to_string_lossy().to_string()))
            }
        }
    }
    #[inline]
    pub fn local_dir(&self) -> Option<&Path> {
        self.local_dir.as_deref()
    }
    #[inline]
    pub fn update_local_dir(&mut self, local_dir: PathBuf) {
        self.local_dir = Some(local_dir)
    }
    #[inline]
    pub fn h2m_console_history(&self) -> Arc<Mutex<Vec<String>>> {
        Arc::clone(&self.h2m_console_history)
    }
    #[inline]
    pub fn pty_handle(&self) -> Option<Arc<RwLock<PTY>>> {
        self.pty_handle.as_ref().map(Arc::clone)
    }
    #[inline]
    pub fn msg_sender(&self) -> Arc<Sender<Message>> {
        Arc::clone(&self.msg_sender)
    }
    #[inline]
    pub fn game_version(&self) -> Option<f64> {
        self.game.version
    }
    pub fn game_name(&self) -> Cow<'static, str> {
        Cow::clone(&self.game.game_name)
    }
    #[inline]
    fn init_pty(&mut self, pty: PTY) {
        self.pty_handle = Some(Arc::new(RwLock::new(pty)))
    }

    pub async fn graceful_shutdown(&mut self) {
        if self.cache_needs_update().load(Ordering::SeqCst) {
            write_cache(self)
                .await
                .unwrap_or_else(|err| error!(name: LOG_ONLY, "{err}"))
        }
        self.try_send_quit_cmd().await;
        terminal::disable_raw_mode().unwrap();
        info!(name: LOG_ONLY, "graceful app shutdown");
    }

    async fn try_send_quit_cmd(&mut self) {
        if !h2m_running() {
            return;
        }
        let Ok(lock) = self.check_h2m_connection().await else {
            return;
        };
        let game_console = lock.read().await;
        match game_console.send_cmd("quit") {
            Ok(()) => {
                info!(name: LOG_ONLY, "{}'s console accepted quit command", self.game_name());
                tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;
            }
            Err(err) => error!(name: LOG_ONLY, "{err}"),
        }
    }

    async fn try_send_cmd_from_hook<S: AsRef<str>>(
        &mut self,
        command: S,
        input_hook_uid: HookUID,
    ) -> Result<Result<(), Cow<'static, str>>, InputHookErr> {
        let lock = self.check_h2m_connection().await.map_err(|err| {
            InputHookErr::new(input_hook_uid, format!("Could not send command: {err}"))
        })?;

        let game_console = lock.read().await;

        Ok(game_console.send_cmd(command))
    }

    pub async fn try_execute_command(&mut self, mut user_tokens: Vec<String>) -> CommandHandle {
        let mut input_tokens = Vec::with_capacity(user_tokens.len() + 1);
        input_tokens.push(String::new());
        input_tokens.append(&mut user_tokens);
        match UserCommand::try_parse_from(input_tokens) {
            Ok(cli) => match cli.command {
                Command::Filter { args } => self.new_favorites_with(args).await,
                Command::Reconnect { args } => self.reconnect(args).await,
                Command::Launch => self.launch_handler().await,
                Command::Cache { option } => self.modify_cache(option).await,
                Command::Console => self.open_game_console().await,
                Command::GameDir => open_dir(self.game.path.parent()),
                Command::LocalEnv => open_dir(self.local_dir.as_deref()),
                Command::Version => self.print_version(),
                Command::Quit => self.quit().await,
            },
            Err(err) => {
                if let Err(prt_err) = err.print() {
                    error!("{err} {prt_err}");
                }
                CommandHandle::Processed
            }
        }
    }

    pub async fn launch_handler(&mut self) -> CommandHandle {
        match launch_h2m_pseudo(&self.game.path) {
            Ok(conpty) => {
                info!("Launching {}...", self.game_name());
                self.game.update(exe_details(&self.game.path));
                self.init_pty(conpty);
                if let Err(err) = self.listener_routine().await {
                    error!("{err}")
                }
            }
            Err(err) => match err {
                LaunchError::Running(msg) => {
                    if self.check_h2m_connection().await.is_ok() {
                        info!("Connection already active")
                    } else {
                        error!("{msg}");
                        println!("{ConnectionHelp}");
                    }
                }
                LaunchError::SpawnErr(err) => error!("{}", err.to_string_lossy()),
            },
        };
        CommandHandle::Processed
    }

    /// if calling manually you are responsible for setting pty inside of context
    pub async fn listener_routine(&mut self) -> Result<(), String> {
        initalize_listener(self).await?;
        let handle = self
            .pty_handle()
            .expect("initalize_listener returned early if this is `None`");
        let msg_sender = self.msg_sender();
        let game_name = self.game_name();
        tokio::spawn(async move {
            const SLEEP: tokio::time::Duration = tokio::time::Duration::from_secs(4);
            let mut attempt = 1;
            let messages = loop {
                tokio::time::sleep(SLEEP * attempt).await;
                match handle.read().await.is_alive() {
                    Ok(true) => {
                        if attempt == 3 {
                            break vec![Message::info(format!("Connected to {game_name} console"))];
                        }
                    }
                    Ok(false) => {
                        break vec![
                            Message::error(format!(
                                "Could not establish connection to {game_name}"
                            )),
                            Message::str(ConnectionHelp),
                        ];
                    }
                    Err(err) => break vec![Message::error(err.to_string_lossy().to_string())],
                }
                attempt += 1;
            };

            for msg in messages {
                msg_sender
                    .send(msg)
                    .await
                    .unwrap_or_else(|err| error!("{err}"));
            }
        });
        Ok(())
    }

    async fn new_favorites_with(&self, args: Option<Filters>) -> CommandHandle {
        let cache = self.cache();
        let exe_dir = self.game.path.parent().expect("has parent");

        let new_entries_found = build_favorites(
            exe_dir,
            &args.unwrap_or_default(),
            cache,
            self.game.version.unwrap_or(1.0),
        )
        .await
        .unwrap_or_else(|err| {
            error!("{err}");
            false
        });
        if new_entries_found {
            self.cache_needs_update().store(true, Ordering::Release);
        }

        CommandHandle::Processed
    }

    fn print_version(&self) -> CommandHandle {
        println!("{}", self.app);
        if self.game.version.is_some() || self.game.hash_curr.is_some() {
            println!("{}", self.game)
        }
        CommandHandle::Processed
    }

    async fn modify_cache(&self, arg: CacheCmd) -> CommandHandle {
        let Some(ref local_dir) = self.local_dir else {
            error!("Can not create cache with out a valid save directory");
            return CommandHandle::Processed;
        };

        let cache_file = match arg {
            CacheCmd::Update => {
                let cache_arc = self.cache();
                let mut cache = cache_arc.lock().await;

                match build_cache(
                    Some(std::mem::take(&mut cache.connection_history)),
                    Some(std::mem::take(&mut cache.ip_to_region)),
                )
                .await
                {
                    Ok(data) => data,
                    Err((err, backup)) => {
                        cache.connection_history = backup.connection_history;
                        cache.ip_to_region = backup.cache.regions;
                        error!("{err}, cache remains unchanged");
                        return CommandHandle::Processed;
                    }
                }
            }
            CacheCmd::Reset => match build_cache(None, None).await {
                Ok(data) => data,
                Err((err, _)) => {
                    error!("{err}, cache remains unchanged");
                    return CommandHandle::Processed;
                }
            },
        };

        match std::fs::File::create(local_dir.join(CACHED_DATA)) {
            Ok(file) => {
                if let Err(err) = serde_json::to_writer_pretty(file, &cache_file) {
                    error!("{err}")
                }
            }
            Err(err) => error!("{err}"),
        }
        let cache = self.cache();
        let mut cache = cache.lock().await;
        *cache = Cache::from(cache_file);
        CommandHandle::Processed
    }

    async fn open_game_console(&mut self) -> CommandHandle {
        if self.check_h2m_connection().await.is_err() || !h2m_running() {
            let history = self.h2m_console_history.lock().await;
            if !history.is_empty() {
                println!("{YELLOW}No active connection to H2M, displaying old logs{WHITE}");
                tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;
                print!("{}", DisplayLogs(&history));
            } else {
                println!("{YELLOW}No active connection to H2M{WHITE}");
            }
            return CommandHandle::Processed;
        }

        {
            let history = self.h2m_console_history.lock().await;
            self.forward_logs.store(true, Ordering::SeqCst);
            print!("{}", DisplayLogs(&history));
        }

        let uid = HookUID::new();
        let game_exe_name = self.game.game_file_name().into_owned();

        let init: Box<InitLineCallback<CommandContext, Stdout>> = Box::new(|handle| {
            handle.set_prompt(game_exe_name);
            handle.disable_completion();
            Ok(())
        });

        let input_hook: Box<InputEventHook<CommandContext, Stdout>> =
            Box::new(move |handle, event| match event {
                Event::Key(KeyEvent {
                    code: KeyCode::Char('c'),
                    modifiers: KeyModifiers::CONTROL,
                    ..
                }) => {
                    if !handle.line.input().is_empty() {
                        handle.ctrl_c_line()?;
                        return Ok((EventLoop::Continue, false));
                    }
                    Ok((EventLoop::Callback(Box::new(end_forward_logs)), true))
                }
                Event::Key(KeyEvent {
                    code: KeyCode::Char(c),
                    ..
                }) => {
                    handle.insert_char(c);
                    Ok((EventLoop::Continue, false))
                }
                Event::Key(KeyEvent {
                    code: KeyCode::Backspace,
                    ..
                }) => {
                    if handle.line.input().is_empty() {
                        return Ok((EventLoop::Callback(Box::new(end_forward_logs)), true));
                    }
                    handle.remove_char()?;
                    Ok((EventLoop::Continue, false))
                }
                Event::Key(KeyEvent {
                    code: KeyCode::Enter,
                    ..
                }) => {
                    if handle.line.input().is_empty() {
                        handle.new_line()?;
                        return Ok((EventLoop::Continue, false));
                    }
                    let cmd = handle.line.take_input();
                    handle.new_line()?;

                    let send_user_cmd: Box<AsyncCallback<CommandContext>> =
                        Box::new(move |context| {
                            Box::pin(async move {
                                context
                                    .try_send_cmd_from_hook(cmd, uid)
                                    .await?
                                    .unwrap_or_else(|err| error!("{err}"));
                                Ok(())
                            })
                        });

                    Ok((EventLoop::AsyncCallback(send_user_cmd), false))
                }
                _ => Ok((EventLoop::Continue, false)),
            });

        CommandHandle::InsertHook(InputHook::new(
            uid,
            Some(init),
            Some(Box::new(end_forward_logs)),
            input_hook,
        ))
    }

    async fn quit(&mut self) -> CommandHandle {
        if self.check_h2m_connection().await.is_err() || !h2m_running() {
            return CommandHandle::Exit;
        }

        println!(
            "{RED}Quitting {} will also close {}\n{YELLOW}Are you sure you want to quit?{WHITE}",
            env!("CARGO_PKG_NAME"),
            self.game_name()
        );

        let init: Box<InitLineCallback<CommandContext, Stdout>> = Box::new(|handle| {
            handle.set_prompt(format!(
                "Press ({YELLOW}y{WHITE}) or ({YELLOW}ctrl_c{WHITE}) to close"
            ));
            Ok(())
        });

        let input_hook: Box<InputEventHook<CommandContext, Stdout>> =
            Box::new(|handle, event| match event {
                Event::Key(
                    KeyEvent {
                        code: KeyCode::Char('c'),
                        modifiers: KeyModifiers::CONTROL,
                        ..
                    }
                    | KeyEvent {
                        code: KeyCode::Char('y'),
                        ..
                    },
                ) => Ok((EventLoop::Break, true)),
                _ => {
                    handle.set_prompt(LineData::default_prompt());
                    Ok((EventLoop::Continue, true))
                }
            });

        CommandHandle::InsertHook(InputHook::with_new_uid(Some(init), None, input_hook))
    }
}

#[inline]
fn end_forward_logs(context: &mut CommandContext) {
    context.forward_logs().store(false, Ordering::SeqCst);
}

pub trait CommandSender {
    fn send_cmd<S: AsRef<str>>(&self, command: S) -> Result<(), Cow<'static, str>>;
}

impl CommandSender for RwLockReadGuard<'_, PTY> {
    /// Before calling be sure to guard against invalid handles by checking `.check_h2m_connection().is_ok()`
    fn send_cmd<S: AsRef<str>>(&self, command: S) -> Result<(), Cow<'static, str>> {
        let cmd_str = command.as_ref();
        let mut os_command = OsString::from(cmd_str);
        os_command.push("\r\n");

        match self.write(os_command) {
            Ok(chars) => {
                if chars != cmd_str.chars().count() as u32 + 2 {
                    return Err(Cow::Borrowed("Failed to send command to h2m console"));
                }
                Ok(())
            }
            Err(err) => Err(Cow::Owned(err.to_string_lossy().to_string())),
        }
    }
}

fn open_dir(path: Option<&Path>) -> CommandHandle {
    if let Some(dir) = path {
        if let Err(err) = std::process::Command::new("explorer").arg(dir).spawn() {
            error!("{err}")
        };
    } else {
        error!("Could not find local dir");
    }
    CommandHandle::Processed
}
