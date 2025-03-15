use crate::{
    commands::{
        filter::build_favorites,
        launch_h2m::{game_open, initalize_listener, launch_h2m_pseudo, LaunchError},
    },
    exe_details, get_latest_hmw_hash, leave_splash_screen,
    models::{
        cli::{CacheCmd, Command, Filters},
        json_data::Version,
    },
    print_during_splash,
    utils::{
        caching::{build_cache, write_cache, Cache},
        display::{ConnectionHelp, DisplayLogs, HmwUpdateHelp, DISP_NAME_H2M, DISP_NAME_HMW},
    },
    LOG_ONLY, MAIN_PROMPT, REQUIRED_FILES,
};

use std::{
    borrow::Cow,
    ffi::OsString,
    fmt::Display,
    io::{self, Stdout},
    path::{Path, PathBuf},
    sync::{
        atomic::{AtomicBool, AtomicUsize, Ordering},
        Arc,
    },
};

use clap::Parser;
use crossterm::event::{Event, KeyCode, KeyEvent, KeyModifiers};
use repl_oxide::{
    ansi_code::{GREEN, RED, RESET, YELLOW},
    executor::{format_for_clap, CommandHandle as CmdHandle, Executor},
    input_hook::{CallbackErr, HookControl, HookStates, HookUID, HookedEvent, InputHook},
    repl_builder, EventLoop, Repl,
};
use tokio::{
    sync::{
        mpsc::{channel, Receiver, Sender},
        Mutex, RwLock, RwLockReadGuard,
    },
    task::JoinHandle,
};
use tracing::{error, info, warn};
use winptyrs::PTY;

pub enum Message {
    Str(Cow<'static, str>),
    Info(Cow<'static, str>),
    Err(Cow<'static, str>),
    Warn(Cow<'static, str>),
}

impl From<std::io::Error> for Message {
    fn from(value: std::io::Error) -> Self {
        Self::Err(Cow::Owned(value.to_string()))
    }
}

impl Message {
    pub fn log(&self) {
        match self {
            Self::Str(_) => (),
            Self::Info(msg) => info!(name: LOG_ONLY, "{msg}"),
            Self::Warn(msg) => warn!(name: LOG_ONLY, "{msg}"),
            Self::Err(msg) => error!(name: LOG_ONLY, "{msg}"),
        }
    }

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
        let (line_color, msg, reset_color): (&str, &str, &str) = match self {
            Self::Str(msg) => ("", msg, ""),
            Self::Info(msg) => (GREEN, msg, RESET),
            Self::Err(msg) => (RED, msg, RESET),
            Self::Warn(msg) => (YELLOW, msg, RESET),
        };

        write!(f, "{line_color}{msg}{reset_color}")
    }
}

#[derive(Debug)]
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
            n if n == REQUIRED_FILES[6] || n == REQUIRED_FILES[5] => Cow::Borrowed(DISP_NAME_HMW),
            n if n == REQUIRED_FILES[3] || n == REQUIRED_FILES[1] || n == REQUIRED_FILES[4] => {
                Cow::Borrowed(DISP_NAME_H2M)
            }
            _ => Cow::Owned(file_name.into_owned()),
        }
    }

    pub fn default(exe_dir: &Path) -> Self {
        GameDetails {
            path: exe_dir.join(REQUIRED_FILES[6]),
            game_name: Cow::Borrowed(DISP_NAME_HMW),
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

pub(crate) type CommandHandle = CmdHandle<CommandContext, Stdout>;
pub type ReplHandle = Repl<CommandContext, Stdout>;

#[derive(Default)]
pub(crate) struct ConsoleHistory {
    pub(crate) history: Vec<String>,
    pub(crate) last: AtomicUsize,
}

pub struct CommandContext {
    cache: Arc<Mutex<Cache>>,
    cache_needs_update: Arc<AtomicBool>,
    forward_logs: Arc<AtomicBool>,
    h2m_console_history: Arc<Mutex<ConsoleHistory>>,
    pty_handle: Option<Arc<RwLock<PTY>>>,
    local_dir: Option<PathBuf>,
    msg_sender: Sender<Message>,
    game: GameDetails,
    app: AppDetails,
}

impl Executor<Stdout> for CommandContext {
    async fn try_execute_command(
        &mut self,
        _line_handle: &mut ReplHandle,
        user_tokens: Vec<String>,
    ) -> io::Result<CommandHandle> {
        let command = match Command::try_parse_from(format_for_clap(&user_tokens)) {
            Ok(c) => c,
            Err(err) => return err.print().map(|_| CommandHandle::Processed),
        };

        self.cache_needs_update.store(true, Ordering::Relaxed);

        match command {
            Command::Filter { args } => self.new_favorites_with(args).await,
            Command::Reconnect { args } => self.reconnect(args).await,
            Command::Launch => self.launch_handler().await,
            Command::Cache { option } => self.modify_cache(option).await,
            Command::Console { all } => self.open_game_console(all).await,
            Command::GameDir => open_dir(self.game.path.parent()),
            Command::LocalEnv => open_dir(self.local_dir.as_deref()),
            Command::Version => self.print_version().await,
            Command::Quit => self.quit().await,
        }
    }
}

pub struct StartupData {
    pub local_dir: Option<PathBuf>,
    pub game: GameDetails,
    pub cache_task: JoinHandle<StartupCacheContents>,
    pub splash_task: JoinHandle<io::Result<()>>,
    pub launch_task: JoinHandle<Option<Result<PTY, LaunchError>>>,
    pub version_task: JoinHandle<reqwest::Result<AppDetails>>,
    pub hmw_hash_task: JoinHandle<reqwest::Result<Result<String, &'static str>>>,
}

#[derive(Default)]
pub struct StartupCacheContents {
    pub command_history: Vec<String>,
    pub cache: Arc<Mutex<Cache>>,
    pub modified: bool,
}

impl CommandContext {
    pub async fn from(
        mut startup_data: StartupData,
        term: Stdout,
    ) -> (ReplHandle, Self, (Receiver<Message>, Receiver<()>)) {
        let (launch_res, app_ver_res, hmw_hash_res, cache_res) = tokio::join!(
            startup_data.launch_task,
            startup_data.version_task,
            startup_data.hmw_hash_task,
            startup_data.cache_task
        );

        let (handle, try_start_listener) = match launch_res {
            Ok(Some(Ok(handle))) => (Some(handle), true),
            Ok(None) => (None, false),
            rest => {
                let err = match rest {
                    Err(join_err) => join_err.to_string(),
                    Ok(Some(Err(launch_err))) => launch_err.to_string(),
                    Ok(Some(Ok(_))) | Ok(None) => unreachable!("by covered paths"),
                };
                error!("Could not launch H2M as child process: {err}");
                print_during_splash(Message::Str(ConnectionHelp.into()));
                (None, false)
            }
        };

        let app = if let Ok(Ok(app)) = app_ver_res {
            if let (Some(latest), Some(msg)) = (&app.ver_latest, &app.update_msg) {
                if app.ver_curr != latest {
                    info!("{msg}")
                }
            }
            app
        } else {
            let err = match app_ver_res {
                Err(join_err) => join_err.to_string(),
                Ok(Err(reqwest_err)) => reqwest_err.to_string(),
                Ok(Ok(_)) => unreachable!("by happy path"),
            };
            error!("Could not get latest MatchWire version: {err}");
            AppDetails::default()
        };

        if let Ok(Ok(Ok(hash_latest))) = hmw_hash_res {
            if let Some(ref hash_curr) = startup_data.game.hash_curr {
                if hash_curr != &hash_latest {
                    info!("{HmwUpdateHelp}")
                }
            }
            startup_data.game.hash_latest = Some(hash_latest);
        } else {
            let err = match hmw_hash_res {
                Ok(Ok(Err(err))) => Cow::Borrowed(err),
                Ok(Err(err)) => Cow::Owned(err.to_string()),
                Err(err) => Cow::Owned(err.to_string()),
                Ok(Ok(Ok(_))) => unreachable!("by happy path"),
            };
            error!("Could not get latest HMW version: {err}");
        };
        let startup_contents = cache_res.unwrap_or_else(|err| {
            error!("Critical error building cache, could not populate cache");
            error!(name: LOG_ONLY, "{err:?}");
            StartupCacheContents::default()
        });

        let cache_needs_update = Arc::new(AtomicBool::new(startup_contents.modified));
        let (update_cache_tx, update_cache_rx) = channel(20);

        tokio::spawn({
            let cache_needs_update = Arc::clone(&cache_needs_update);
            async move {
                loop {
                    if cache_needs_update
                        .compare_exchange(true, false, Ordering::Acquire, Ordering::SeqCst)
                        .is_ok()
                        && update_cache_tx.send(()).await.is_err()
                    {
                        break;
                    }
                    tokio::time::sleep(tokio::time::Duration::from_secs(240)).await;
                }
            }
        });

        let (message_tx, message_rx) = channel(50);

        let mut ctx = CommandContext {
            cache: startup_contents.cache,
            msg_sender: message_tx,
            app,
            game: startup_data.game,
            local_dir: startup_data.local_dir,
            pty_handle: handle.map(|pty| Arc::new(RwLock::new(pty))),
            cache_needs_update,
            forward_logs: Arc::new(AtomicBool::new(false)),
            h2m_console_history: Arc::new(Mutex::new(ConsoleHistory::default())),
        };

        if try_start_listener {
            ctx.listener_routine()
                .await
                .unwrap_or_else(|err| warn!("{err}"));
        }

        leave_splash_screen(startup_data.splash_task).await;

        (
            repl_builder(term)
                .with_prompt(MAIN_PROMPT)
                .with_completion(&crate::models::command_scheme::COMPLETION)
                .with_custom_quit_command("quit")
                .with_history_entries(&startup_contents.command_history)
                .build()
                .expect("`Stdout` accepts crossterm commands"),
            ctx,
            (message_rx, update_cache_rx),
        )
    }
    #[inline]
    pub(crate) fn cache(&self) -> Arc<Mutex<Cache>> {
        Arc::clone(&self.cache)
    }
    #[inline]
    pub(crate) fn cache_needs_update(&self) -> Arc<AtomicBool> {
        Arc::clone(&self.cache_needs_update)
    }
    #[inline]
    pub(crate) fn forward_logs(&self) -> Arc<AtomicBool> {
        Arc::clone(&self.forward_logs)
    }
    pub(crate) async fn check_h2m_connection(
        &mut self,
    ) -> Result<Arc<RwLock<PTY>>, Cow<'static, str>> {
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
    pub(crate) fn local_dir(&self) -> Option<&Path> {
        self.local_dir.as_deref()
    }
    #[inline]
    pub(crate) fn h2m_console_history(&self) -> Arc<Mutex<ConsoleHistory>> {
        Arc::clone(&self.h2m_console_history)
    }
    #[inline]
    pub(crate) fn pty_handle(&self) -> Option<Arc<RwLock<PTY>>> {
        self.pty_handle.as_ref().map(Arc::clone)
    }
    #[inline]
    pub(crate) fn msg_sender(&self) -> Sender<Message> {
        self.msg_sender.clone()
    }
    #[inline]
    pub(crate) fn game_version(&self) -> Option<f64> {
        self.game.version
    }
    #[inline]
    pub(crate) fn game_name(&self) -> Cow<'static, str> {
        Cow::clone(&self.game.game_name)
    }
    #[inline]
    fn init_pty(&mut self, pty: PTY) {
        self.pty_handle = Some(Arc::new(RwLock::new(pty)))
    }

    pub async fn graceful_shutdown(&mut self, cmd_history: &[String]) {
        if self.cache_needs_update().load(Ordering::SeqCst) {
            write_cache(self, cmd_history)
                .await
                .unwrap_or_else(|err| error!(name: LOG_ONLY, "{err}"))
        }
        self.try_send_quit_cmd().await;
        info!(name: LOG_ONLY, "graceful app shutdown");
    }

    async fn try_send_quit_cmd(&mut self) {
        if game_open()
            .unwrap_or_else(LaunchError::resolve_to_closed)
            .is_none()
        {
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
    ) -> Result<Result<(), Cow<'static, str>>, CallbackErr> {
        let lock = self.check_h2m_connection().await.map_err(|err| {
            CallbackErr::new(input_hook_uid, format!("Could not send command: {err}"))
        })?;

        let game_console = lock.read().await;

        Ok(game_console.send_cmd(command))
    }

    pub(crate) async fn launch_handler(&mut self) -> io::Result<CommandHandle> {
        if self.check_h2m_connection().await.is_ok() {
            println!(
                "{GREEN}Connection to {} already active{RESET}",
                self.game_name()
            );
            return Ok(CommandHandle::Processed);
        }

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
                already_open_err @ LaunchError::GameRunning(_) => {
                    println!("{RED}{already_open_err}{RESET}");
                    println!("{ConnectionHelp}");
                }
                other_err => error!("{other_err}"),
            },
        };
        Ok(CommandHandle::Processed)
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
            const SLEEP: tokio::time::Duration = tokio::time::Duration::from_millis(4500);
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
                msg_sender.send(msg).await.unwrap_or_else(|err| err.0.log());
            }
        });
        Ok(())
    }

    async fn new_favorites_with(&self, args: Option<Filters>) -> io::Result<CommandHandle> {
        let exe_dir = self.game.path.parent().expect("has parent");

        let new_entries_found = build_favorites(
            exe_dir,
            args.unwrap_or_default(),
            self.cache(),
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

        Ok(CommandHandle::Processed)
    }

    async fn print_version(&mut self) -> io::Result<CommandHandle> {
        if self.game.hash_latest.is_none() {
            println!("{GREEN}Trying to get lastest HMW version..{RESET}");

            self.game.hash_latest = get_latest_hmw_hash()
                .await
                .map_err(|err| error!("{err}"))
                .ok()
                .and_then(|res| {
                    match res {
                        Ok(_) => info!("Found latest HMW version"),
                        Err(err) => error!("{err}"),
                    }
                    res.ok()
                })
        }

        println!("{}", self.app);
        if self.game.version.is_some() || self.game.hash_curr.is_some() {
            println!("{}", self.game)
        }
        Ok(CommandHandle::Processed)
    }

    async fn modify_cache(&self, arg: CacheCmd) -> io::Result<CommandHandle> {
        if self.local_dir.is_none() {
            error!("Can not create cache with out a valid save directory");
            return Ok(CommandHandle::Processed);
        }

        match build_cache(matches!(arg, CacheCmd::Update).then_some(&self.cache)).await {
            Ok(cache) => {
                let mut lock = self.cache.lock().await;
                *lock = cache
            }
            Err(err) => {
                error!("{err}, cache remains unchanged");
                return Ok(CommandHandle::Processed);
            }
        };

        self.cache_needs_update.store(true, Ordering::SeqCst);
        Ok(CommandHandle::Processed)
    }

    async fn open_game_console(&mut self, all: bool) -> io::Result<CommandHandle> {
        let h2m_connection_err = self.check_h2m_connection().await.is_err();
        let console = self.h2m_console_history.lock().await;

        if all {
            console.last.store(0, Ordering::SeqCst);
        }

        if h2m_connection_err
            || game_open()
                .unwrap_or_else(LaunchError::resolve_to_closed)
                .is_none()
        {
            if !console.history.is_empty() {
                println!(
                    "{YELLOW}No active connection to {}, displaying old logs{RESET}",
                    self.game_name()
                );
                tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;
                print!("{}", DisplayLogs(&console));
            } else {
                println!(
                    "{YELLOW}No active connection to {}{RESET}",
                    self.game_name()
                );
            }
            return Ok(CommandHandle::Processed);
        }

        print!("{}", DisplayLogs(&console));
        drop(console);

        let uid = HookUID::new();

        let line_changes = HookStates::<CommandContext, _>::new(
            |handle, context| {
                context.forward_logs.store(true, Ordering::SeqCst);
                handle.set_prompt(&context.game.game_file_name());
                handle.disable_completion();
                handle.disable_line_stylization();
                Ok(())
            },
            |handle, context| {
                context.forward_logs.store(false, Ordering::SeqCst);
                handle.set_prompt(MAIN_PROMPT);
                handle.enable_completion();
                handle.enable_line_stylization();
                Ok(())
            },
        );

        let input_hook = InputHook::new(uid, line_changes, move |handle, _context, event| {
            match event {
                Event::Key(KeyEvent {
                    code: KeyCode::Char('d'),
                    modifiers: KeyModifiers::CONTROL,
                    ..
                }) => {
                    handle.clear_line()?;
                    return HookedEvent::new(handle.process_close_signal()?, HookControl::Release);
                }
                Event::Key(KeyEvent {
                    code: KeyCode::Char('c'),
                    modifiers: KeyModifiers::CONTROL,
                    ..
                }) => {
                    if handle.input().is_empty() {
                        return HookedEvent::release_hook();
                    }
                    handle.ctrl_c_line()?;
                }
                Event::Key(KeyEvent {
                    code: KeyCode::Char(c),
                    ..
                }) => handle.insert_char(c),
                Event::Key(KeyEvent {
                    code: KeyCode::Backspace,
                    ..
                }) => {
                    if handle.input().is_empty() {
                        return HookedEvent::release_hook();
                    }
                    handle.remove_char()?;
                }
                Event::Key(KeyEvent {
                    code: KeyCode::Enter,
                    ..
                }) => {
                    if !handle.input().is_empty() {
                        let cmd = handle.new_line()?;

                        let send_user_cmd = EventLoop::<CommandContext, _>::new_async_callback(
                            move |_handle, context| {
                                Box::pin(async move {
                                    context
                                        .try_send_cmd_from_hook(cmd, uid)
                                        .await?
                                        .unwrap_or_else(|err| error!("{err}"));
                                    Ok(())
                                })
                            },
                        );

                        return HookedEvent::new(send_user_cmd, HookControl::Continue);
                    }
                    handle.new_line()?;
                }
                _ => handle.set_uneventful(),
            }
            HookedEvent::continue_hook()
        });

        Ok(CommandHandle::InsertHook(input_hook))
    }

    async fn quit(&mut self) -> io::Result<CommandHandle> {
        if game_open()
            .unwrap_or_else(LaunchError::resolve_to_open)
            .is_none()
        {
            while self.check_h2m_connection().await.is_ok() {
                tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
            }
            return Ok(CommandHandle::Exit);
        } else if self.check_h2m_connection().await.is_err() {
            return Ok(CommandHandle::Exit);
        }

        println!(
            "{RED}Quitting {} will also close {}\n{YELLOW}Are you sure you want to quit?{RESET}",
            env!("CARGO_PKG_NAME"),
            self.game_name()
        );

        let line_changes = HookStates::new(
            |handle, _context| {
                handle.set_prompt(&format!(
                    "Press ({YELLOW}y{RESET}) or ({YELLOW}ctrl_c{RESET}) to close"
                ));
                Ok(())
            },
            |handle, _context| {
                handle.set_prompt(MAIN_PROMPT);
                Ok(())
            },
        );

        let input_hook =
            InputHook::with_new_uid(line_changes, |_handle, _context, event| match event {
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
                ) => HookedEvent::break_repl(),
                _ => HookedEvent::release_hook(),
            });

        Ok(CommandHandle::InsertHook(input_hook))
    }
}

pub(crate) trait CommandSender {
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

fn open_dir(path: Option<&Path>) -> io::Result<CommandHandle> {
    if let Some(dir) = path {
        if let Err(err) = std::process::Command::new("explorer").arg(dir).spawn() {
            error!("{err}")
        };
    } else {
        error!("Could not find local dir");
    }
    Ok(CommandHandle::Processed)
}
