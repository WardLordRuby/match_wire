use crate::{
    commands::{
        filter::build_favorites,
        launch_h2m::{
            game_open, hide_pseudo_console, initialize_listener, launch_h2m_pseudo,
            toggle_close_state, LaunchError, WinApiErr,
        },
    },
    exe_details,
    files::*,
    get_latest_hmw_hash,
    models::{
        cli::{CacheCmd, Command, Filters},
        json_data::Version,
    },
    open_dir, splash_screen,
    utils::{
        caching::{build_cache, write_cache},
        display::{self, ConnectionHelp, DisplayLogs, HmwUpdateHelp, DISP_NAME_HMW},
        global_state::{self, ThreadCopyState},
    },
    Spinner, CRATE_NAME, CRATE_VER, LOG_ONLY, MAIN_PROMPT, SAVED_HISTORY_CAP,
};

use std::{
    borrow::Cow,
    ffi::OsString,
    fmt::Display,
    io::{self, Stdout},
    net::SocketAddr,
    path::{Path, PathBuf},
    sync::Arc,
};

use crossterm::{
    event::{Event, KeyCode, KeyEvent, KeyModifiers},
    terminal::SetTitle,
    QueueableCommand,
};
use repl_oxide::{
    ansi_code::{GREEN, RED, RESET, YELLOW},
    clap::try_parse_from,
    executor::{CommandHandle as CmdHandle, Executor},
    input_hook::{HookControl, HookID, HookStates, HookedEvent, InputHook},
    repl_builder, Repl,
};
use tokio::{
    sync::{
        mpsc::{channel, Receiver, Sender},
        Notify,
    },
    task::JoinHandle,
    time::Duration,
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
    pub(crate) fn log(&self) {
        match self {
            Self::Str(_) => (),
            Self::Info(msg) => info!(name: LOG_ONLY, "{msg}"),
            Self::Warn(msg) => warn!(name: LOG_ONLY, "{msg}"),
            Self::Err(msg) => error!(name: LOG_ONLY, "{msg}"),
        }
    }

    pub(crate) fn record(&self) {
        match self {
            Self::Str(msg) => println!("{msg}"),
            Self::Info(msg) => info!("{msg}"),
            Self::Warn(msg) => warn!("{msg}"),
            Self::Err(msg) => error!("{msg}"),
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
    pub fn get() -> Result<(Self, bool), Cow<'static, str>> {
        let exe_dir =
            std::env::current_dir().map_err(|err| format!("Failed to get current dir, {err:?}"))?;

        let no_launch = {
            #[cfg(not(debug_assertions))]
            {
                use clap::Parser;
                crate::models::cli::Cli::parse().no_launch
            }

            #[cfg(debug_assertions)]
            true
        };

        #[cfg(not(debug_assertions))]
        {
            let game = match crate::contains_required_files(&exe_dir) {
                Ok(game_exe_path) => {
                    let (version, hash) = crate::exe_details(&game_exe_path);
                    GameDetails::new(game_exe_path, version, hash)
                }
                Err(err) if no_launch => {
                    splash_screen::push_message(Message::error(err));
                    GameDetails::default(&exe_dir)
                }
                Err(err) => return Err(err),
            };

            Ok((game, no_launch))
        }

        #[cfg(debug_assertions)]
        Ok((GameDetails::default(&exe_dir), no_launch))
    }

    fn default(exe_dir: &Path) -> Self {
        GameDetails {
            path: exe_dir.join(FNAME_HMW),
            game_name: Cow::Borrowed(DISP_NAME_HMW),
            version: None,
            hash_curr: None,
            hash_latest: None,
        }
    }

    #[cfg(not(debug_assertions))]
    fn new(exe_path: PathBuf, version: Option<f64>, hash_curr: Option<String>) -> Self {
        fn set_game_name(exe_path: &Path) -> Cow<'static, str> {
            let file_name = exe_path
                .file_name()
                .expect("path points to exe")
                .to_string_lossy();

            match file_name.as_ref() {
                FNAME_HMW => Cow::Borrowed(DISP_NAME_HMW),
                FNAME_H2M_1 | FNAME_H2M_2 => Cow::Borrowed(crate::utils::display::DISP_NAME_H2M),
                _ => Cow::Owned(file_name.into_owned()),
            }
        }

        GameDetails {
            game_name: set_game_name(&exe_path),
            path: exe_path,
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

#[derive(Default)]
pub struct AppDetails {
    pub ver_latest: Option<String>,
    pub update_msg: Option<String>,
}

impl From<Version> for AppDetails {
    fn from(value: Version) -> Self {
        AppDetails {
            ver_latest: Some(value.latest),
            update_msg: Some(value.message),
        }
    }
}

pub(crate) type CommandHandle = CmdHandle<CommandContext, Stdout>;
pub type ReplHandle = Repl<CommandContext, Stdout>;

pub struct CommandContext {
    can_close_console: bool,
    local_dir: Option<PathBuf>,
    msg_sender: Sender<Message>,
    pub game_state_change: Arc<Notify>,
    game: GameDetails,
    app: AppDetails,
}

impl Executor<Stdout> for CommandContext {
    async fn try_execute_command(
        &mut self,
        line_handle: &mut ReplHandle,
        user_tokens: Vec<String>,
    ) -> io::Result<CommandHandle> {
        let command = match try_parse_from(&user_tokens) {
            Ok(c) => c,
            Err(err) => return err.print().map(|_| CommandHandle::Processed),
        };

        global_state::UpdateCache::set(true);

        match command {
            Command::Filter { args } => self.new_favorites_with(line_handle, args).await,
            Command::Last => Self::display_servers(line_handle),
            Command::Reconnect { args } => self.reconnect(line_handle, args),
            Command::Launch => self.launch_handler(),
            Command::Cache { option } => self.modify_cache(option).await,
            Command::Console { all } => self.open_game_console(all),
            Command::GameDir => self.try_open_game_dir(),
            Command::LocalEnv => self.try_open_local_dir(),
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
    pub hmw_hash_task: JoinHandle<reqwest::Result<Result<String, &'static str>>>,
}

#[derive(Default)]
pub struct StartupCacheContents {
    pub command_history: Vec<String>,
    pub modified: bool,
}

enum HookTag {
    GameConsole,
}

impl From<HookTag> for i32 {
    fn from(tag: HookTag) -> Self {
        tag as i32
    }
}

impl CommandContext {
    pub async fn from(
        mut startup_data: StartupData,
        app: AppDetails,
        term: Stdout,
    ) -> (ReplHandle, Self, Receiver<Message>) {
        let (launch_res, hmw_hash_res, cache_res) = tokio::join!(
            startup_data.launch_task,
            startup_data.hmw_hash_task,
            startup_data.cache_task
        );

        let game_console = match launch_res {
            Ok(Some(Ok(handle))) => Some(handle),
            Ok(None) => None,
            rest => {
                let err = match rest {
                    Err(join_err) => join_err.to_string(),
                    Ok(Some(Err(launch_err))) => launch_err.to_string(),
                    Ok(Some(Ok(_))) | Ok(None) => unreachable!("by covered paths"),
                };
                error!("Could not launch H2M as child process: {err}");
                splash_screen::push_message(Message::Str(ConnectionHelp.into()));
                None
            }
        };
        let console_set = game_console.is_some();
        global_state::PtyHandle::set(game_console);

        if let (Some(latest), Some(msg)) = (&app.ver_latest, &app.update_msg) {
            if CRATE_VER != latest {
                info!("{msg}")
            }
        }

        if let Ok(Ok(Ok(hash_latest))) = hmw_hash_res {
            if let Some(hash_curr) = startup_data.game.hash_curr.as_deref() {
                if hash_curr != hash_latest {
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

        let (message_tx, message_rx) = channel(50);
        global_state::UpdateCache::set(startup_contents.modified);

        let mut ctx = CommandContext {
            msg_sender: message_tx,
            game_state_change: Arc::new(Notify::const_new()),
            app,
            game: startup_data.game,
            local_dir: startup_data.local_dir,
            can_close_console: true,
        };

        if console_set {
            ctx.listener_routine().unwrap_or_else(display::warning);
        }

        splash_screen::leave(startup_data.splash_task).await;

        (
            repl_builder(term)
                .with_prompt(MAIN_PROMPT)
                .with_completion(&crate::models::command_scheme::COMPLETION)
                .with_custom_quit_command("quit")
                .with_history_entries(&startup_contents.command_history)
                .build()
                .expect("`Stdout` accepts crossterm commands"),
            ctx,
            message_rx,
        )
    }
    #[inline]
    pub(crate) fn local_dir(&self) -> Option<&Path> {
        self.local_dir.as_deref()
    }
    #[inline]
    pub(crate) fn msg_sender(&self) -> Sender<Message> {
        self.msg_sender.clone()
    }
    #[inline]
    pub(crate) fn game_state_change(&self) -> Arc<Notify> {
        Arc::clone(&self.game_state_change)
    }
    #[inline]
    pub(crate) fn game_version(&self) -> Option<f64> {
        self.game.version
    }
    #[inline]
    pub(crate) fn game_name(&self) -> Cow<'static, str> {
        Cow::clone(&self.game.game_name)
    }

    fn try_open_game_dir(&self) -> io::Result<CommandHandle> {
        self.game.path.parent().map(open_dir).unwrap_or_else(|| {
            error!(
                "Game path: {}, has no valid parent",
                self.game.path.display()
            );
        });
        Ok(CommandHandle::Processed)
    }

    fn try_open_local_dir(&self) -> io::Result<CommandHandle> {
        self.local_dir.as_deref().map(open_dir).unwrap_or_else(|| {
            error!("Failed to find local environment directory on startup");
        });
        Ok(CommandHandle::Processed)
    }

    pub fn graceful_shutdown(&mut self, cmd_history: &[String]) {
        if global_state::UpdateCache::get() {
            write_cache(self, cmd_history).unwrap_or_else(display::log_error)
        }

        global_state::PtyHandle::try_drop_pty(&self.game_name());

        if let Some(hwnd) = global_state::AppHWND::get().filter(|_| !self.can_close_console) {
            // Safety: `hwnd` only ever refers to the current process, making it so it _must_ always be a valid pointer
            unsafe { toggle_close_state(&mut self.can_close_console, hwnd) }
                .unwrap_or_else(display::log_error);
        }

        info!(name: LOG_ONLY, "graceful app shutdown");
    }

    fn try_send_cmd_from_hook(&mut self, command: String) -> Result<(), Cow<'static, str>> {
        global_state::PtyHandle::try_if_alive(|game_console| game_console.send_cmd(command))
            .map_err(Into::into)
    }

    pub(crate) fn launch_handler(&mut self) -> io::Result<CommandHandle> {
        if global_state::PtyHandle::check_connection().is_ok() {
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
                global_state::PtyHandle::set(Some(conpty));
                if let Err(err) = self.listener_routine() {
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

    pub fn save_cache_if_needed(&self, line_handle: &Repl<Self, Stdout>) -> io::Result<()> {
        if global_state::UpdateCache::take() {
            return write_cache(self, &line_handle.export_history(Some(SAVED_HISTORY_CAP)));
        }

        Ok(())
    }

    pub fn handle_game_state_change(
        &mut self,
        line_handle: &mut Repl<Self, Stdout>,
    ) -> io::Result<Result<(), WinApiErr>> {
        let game_open = global_state::PtyHandle::check_connection().is_ok();

        line_handle.writer().queue(SetTitle(CRATE_NAME))?;

        if !game_open && line_handle.input_hooked() {
            line_handle.remove_all_hooks_with_tag(self, HookTag::GameConsole)?;
        }

        if let Some(hwnd) =
            global_state::AppHWND::get().filter(|_| game_open == self.can_close_console)
        {
            // Safety: `hwnd` only ever refers to the current process, making it so it _must_ always be a valid pointer
            return Ok(unsafe { toggle_close_state(&mut self.can_close_console, hwnd) });
        }

        Ok(Ok(()))
    }

    /// if calling manually you are responsible for setting pty inside of context
    pub fn listener_routine(&mut self) -> Result<(), String> {
        initialize_listener(self)?;

        let msg_sender = self.msg_sender();
        let game_name = self.game_name();
        let game_state_change = self.game_state_change();
        tokio::spawn(async move {
            const SLEEP: Duration = Duration::from_millis(4500);
            let mut attempt = 1;
            let messages = loop {
                tokio::time::sleep(SLEEP * attempt).await;
                match global_state::PtyHandle::is_alive() {
                    Ok(true) if attempt == 3 => {
                        match hide_pseudo_console() {
                            Ok(true) => info!(name: LOG_ONLY, "Pseudo console window hidden"),
                            Ok(false) => {
                                info!(name: LOG_ONLY, "Could not find Pseudo console window to hide")
                            }
                            Err(win_api_err) => error!(name: LOG_ONLY, "{win_api_err}"),
                        }
                        game_state_change.notify_one();
                        break vec![Message::info(format!("Connected to {game_name} console"))];
                    }
                    Ok(true) => attempt += 1,
                    Ok(false) => {
                        break vec![
                            Message::error(format!(
                                "Could not establish connection to {game_name}"
                            )),
                            Message::str(ConnectionHelp),
                        ];
                    }
                    Err(err) => break vec![Message::error(err)],
                }
            };

            for msg in messages {
                msg_sender.send(msg).await.unwrap_or_else(|err| err.0.log());
            }
        });
        Ok(())
    }

    async fn new_favorites_with(
        &self,
        repl: &mut Repl<CommandContext, Stdout>,
        args: Option<Filters>,
    ) -> io::Result<CommandHandle> {
        let exe_dir = self.game.path.parent().expect("has parent");

        let new_entries_found = build_favorites(
            repl,
            exe_dir,
            args.unwrap_or_default(),
            self.game.version.unwrap_or(1.0),
        )
        .await
        .map_err(display::error)
        .unwrap_or_default();

        global_state::UpdateCache::and_modify(|curr| curr || new_entries_found);

        Ok(CommandHandle::Processed)
    }

    fn display_servers(repl: &mut Repl<CommandContext, Stdout>) -> io::Result<CommandHandle> {
        global_state::LastServerStats::display(repl.terminal_size());
        Ok(CommandHandle::Processed)
    }

    async fn print_version(&mut self) -> io::Result<CommandHandle> {
        if self.game.hash_latest.is_none() {
            println!("{GREEN}Trying to get latest HMW version..{RESET}");

            self.game.hash_latest = get_latest_hmw_hash()
                .await
                .map_err(display::error)
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
        println!("{}", self.game);
        Ok(CommandHandle::Processed)
    }

    async fn modify_cache(&self, arg: CacheCmd) -> io::Result<CommandHandle> {
        if self.local_dir.is_none() {
            error!("Can not create cache with out a valid save directory");
            return Ok(CommandHandle::Processed);
        }

        if let CacheCmd::Reset = arg {
            global_state::Cache::clear();
        }

        let cache_modified = build_cache()
            .await
            .map_err(|err| error!("{err}, cache remains unchanged"))
            .is_ok();

        global_state::UpdateCache::and_modify(|curr| curr || cache_modified);

        Ok(CommandHandle::Processed)
    }

    fn open_game_console(&mut self, all: bool) -> io::Result<CommandHandle> {
        if all {
            global_state::ConsoleHistory::with_borrow_mut(|history| history.reset_i());
        }

        if global_state::PtyHandle::check_connection().is_err()
            || game_open()
                .unwrap_or_else(WinApiErr::resolve_to_closed)
                .is_none()
        {
            print!("{YELLOW}No active connection to {}", self.game_name());

            if global_state::ConsoleHistory::with_borrow_mut(|history| {
                let not_empty = !history.entries.is_empty();
                if not_empty {
                    history.reset_i();
                }
                not_empty
            }) {
                println!(", displaying old logs{RESET}");

                std::thread::sleep(Duration::from_secs(2));
                print!("{DisplayLogs}");
            } else {
                println!()
            }
            return Ok(CommandHandle::Processed);
        }

        print!("{DisplayLogs}");

        let line_changes = HookStates::<CommandContext, _>::new(
            |handle, context| {
                global_state::ForwardLogs::set(true);
                handle.set_prompt(&context.game.game_file_name());
                handle.disable_completion();
                handle.disable_line_stylization();
                Ok(())
            },
            |handle, _context| {
                global_state::ForwardLogs::set(false);
                handle.set_prompt(MAIN_PROMPT);
                handle.enable_completion();
                handle.enable_line_stylization();
                Ok(())
            },
        );

        let input_hook = InputHook::new(
            HookID::tagged(HookTag::GameConsole),
            line_changes,
            move |handle, context, event| {
                match event {
                    Event::Key(KeyEvent {
                        code: KeyCode::Char('d'),
                        modifiers: KeyModifiers::CONTROL,
                        ..
                    }) => {
                        handle.clear_line()?;
                        return HookedEvent::new(
                            handle.process_close_signal()?,
                            HookControl::Release,
                        );
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
                        let cmd = handle.new_line()?;
                        if !cmd.trim().is_empty() {
                            if let Err(err) = context.try_send_cmd_from_hook(cmd) {
                                error!("{err}");
                                return HookedEvent::release_hook();
                            }
                        }
                    }
                    _ => handle.set_uneventful(),
                }
                HookedEvent::continue_hook()
            },
        );

        Ok(CommandHandle::InsertHook(input_hook))
    }

    async fn quit(&mut self) -> io::Result<CommandHandle> {
        if game_open()
            .unwrap_or_else(WinApiErr::resolve_to_open)
            .is_none()
        {
            let spinner = Spinner::new(format!("Waiting for {} to close", self.game_name()));

            while global_state::PtyHandle::check_connection().is_ok() {
                tokio::task::yield_now().await;
            }

            spinner.finish();
            return Ok(CommandHandle::Exit);
        } else if global_state::PtyHandle::check_connection().is_err() {
            return Ok(CommandHandle::Exit);
        }

        println!(
            "{RED}Quitting {CRATE_NAME} will also close {}\n{YELLOW}Are you sure you want to quit?{RESET}",
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
    fn send_connect(&self, ip: SocketAddr) -> Result<(), Cow<'static, str>>;
}

impl CommandSender for PTY {
    /// Before calling be sure to guard against invalid handles by checking pty connection is alive
    fn send_cmd<S: AsRef<str>>(&self, command: S) -> Result<(), Cow<'static, str>> {
        const NEW_LINE: &str = "\r\n";
        let cmd_str = command.as_ref();

        let mut os_command = OsString::from(cmd_str);
        os_command.push(NEW_LINE);

        match self.write(os_command) {
            Ok(n_chars) => {
                if n_chars != (cmd_str.chars().count() + NEW_LINE.len()) as u32 {
                    return Err(Cow::Borrowed("Failed to send command to h2m console"));
                }
                Ok(())
            }
            Err(err) => Err(Cow::Owned(err.to_string_lossy().to_string())),
        }
    }

    /// Before calling be sure to guard against invalid handles by checking pty connection is alive
    fn send_connect(&self, ip_port: SocketAddr) -> Result<(), Cow<'static, str>> {
        self.send_cmd("disconnect")?;
        std::thread::sleep(crate::CONSEC_CMD_DELAY);
        self.send_cmd(format!("connect {ip_port}"))
    }
}
