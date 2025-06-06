use crate::{
    CRATE_NAME, CRATE_VER, LOG_ONLY, MAIN_PROMPT, MOD_FILES_MODULE_NAME, ResponseErr,
    SAVED_HISTORY_CAP,
    commands::{
        filter::build_favorites,
        launch_h2m::{
            LaunchError, WinApiErr, game_open, hide_pseudo_console, init_listener,
            launch_h2m_pseudo, terminate_process_by_id, toggle_close_state,
        },
    },
    exe_details,
    files::*,
    get_latest_hmw_manifest, hash_file_hex,
    models::{
        cli::{CacheCmd, Command, Filters},
        json_data::{CondManifest, HmwManifest, Version},
    },
    open_dir, splash_screen,
    utils::{
        caching::{build_cache, write_cache},
        display::{
            self, ConnectionHelp, DISP_NAME_HMW, DisplayLogs, HmwUpdateHelp,
            indicator::{ProgressBar, Spinner},
        },
        global_state::{self, Cache, ThreadCopyState},
    },
};

use std::{
    borrow::Cow,
    ffi::OsString,
    fmt::Display,
    io::{self, ErrorKind, Stdout},
    net::SocketAddr,
    num::NonZero,
    path::{Path, PathBuf},
    sync::Arc,
};

use crossterm::{
    QueueableCommand,
    event::{Event, KeyCode, KeyEvent, KeyModifiers},
    terminal::SetTitle,
};
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use repl_oxide::{
    Repl,
    ansi_code::{GREEN, RED, RESET, YELLOW},
    clap::try_parse_from,
    executor::{CommandHandle as CmdHandle, Executor},
    input_hook::{HookControl, HookID, HookStates, HookedEvent, InputHook},
    repl_builder,
};
use tokio::{
    sync::{
        Notify,
        mpsc::{Receiver, Sender, channel},
    },
    task::JoinHandle,
    time::Duration,
};
use tracing::{error, info, warn};
use winptyrs::PTY;

macro_rules! hmw_hash_err {
    ($($arg:tt)*) => {
        error!("Could not get latest HMW version: {}", format_args!($($arg)*))
    }
}

pub(crate) enum CmdErr {
    Critical(io::Error),
    Command,
}

impl From<io::Error> for CmdErr {
    /// This implementation of `from` should be used carefully, not _all_ `io::Error`s are critical,
    /// but **all** `io::Error`s that originate from the repl_oxide crate are.
    fn from(err: io::Error) -> Self {
        Self::Critical(err)
    }
}

impl CmdErr {
    pub(crate) fn transpose<T>(res: Result<T, Self>) -> io::Result<Option<T>> {
        match res {
            Ok(t) => Ok(Some(t)),
            Err(CmdErr::Command) => Ok(None),
            Err(CmdErr::Critical(err)) => Err(err),
        }
    }
}

#[macro_export]
macro_rules! command_err {
    ($($arg:tt)*) => {{
        tracing::error!($($arg)*);
        CmdErr::Command
    }};
}

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
pub enum ModFileStatus {
    Initial,
    MissingFiles(Vec<String>),
    VerifyReady,
    Outdated(Vec<String>),
    UpToDate,
}

#[derive(Debug)]
pub struct GameDetails {
    pub path: PathBuf,
    pub game_name: Cow<'static, str>,
    pub version: Option<f64>,
    pub hash_curr: Option<String>,
    pub hash_latest: Option<String>,
    pub mod_verification: ModFileStatus,
}

impl GameDetails {
    pub fn get() -> Result<(Self, bool), Cow<'static, str>> {
        let exe_dir = std::env::current_exe()
            .map_err(|err| format!("Failed to get exe directory, {err:?}"))?;
        let exe_dir = exe_dir
            .parent()
            .ok_or("Failed to get exe parent directory")?;

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
        Ok((GameDetails::default(exe_dir), no_launch))
    }

    fn default(exe_dir: &Path) -> Self {
        GameDetails {
            path: exe_dir.join(FNAME_HMW),
            game_name: Cow::Borrowed(DISP_NAME_HMW),
            version: None,
            hash_curr: None,
            hash_latest: None,
            mod_verification: ModFileStatus::Initial,
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
            mod_verification: ModFileStatus::Initial,
        }
    }

    /// Returns `true` if `Cache` was modified / manifest was condensed
    pub(crate) fn conditional_condense_manifest(&mut self, mut man: HmwManifest) -> bool {
        fn try_get_exe_hash(cache: &Cache) -> Option<String> {
            cache.hmw_manifest.files_with_hashes.get(FNAME_HMW).cloned()
        }

        global_state::Cache::with_borrow_mut(|cache| {
            if man.modules.is_empty() {
                error!("HMW manifest formatting has changed, failed to verify version");
                self.hash_latest = None;
                return false;
            }

            if cache.hmw_manifest.guid == man.manifest_guid {
                if cache.hmw_manifest.verified {
                    self.mod_verification = ModFileStatus::UpToDate;
                }
                self.hash_latest = try_get_exe_hash(cache);
                return false;
            }

            self.mod_verification = ModFileStatus::Initial;

            man.modules.retain(|m| m.name == MOD_FILES_MODULE_NAME);
            man.modules.sort_unstable_by_key(|m| m.version.clone());

            let mut files_with_hashes = std::mem::take(&mut man.modules[0].files_with_hashes);

            man.modules
                .into_iter()
                .skip(1)
                .for_each(|m| files_with_hashes.extend(m.files_with_hashes));

            cache.hmw_manifest = CondManifest {
                guid: man.manifest_guid,
                files_with_hashes,
                verified: false,
            };

            self.hash_latest = try_get_exe_hash(cache);
            true
        })
    }

    pub(crate) fn get_exe_dir(&self) -> &Path {
        self.path
            .parent()
            .expect("self can not be created without a file added to path")
    }

    fn prep_verification_state(&mut self) -> Result<(), ()> {
        let exe_dir = self.get_exe_dir();

        let missing = global_state::Cache::with_borrow_mut(|cache| {
            let mut missing = Vec::new();

            for mod_file in cache.hmw_manifest.files_with_hashes.keys() {
                let file_path = exe_dir.join(mod_file);
                if !file_path.exists() {
                    missing.push(mod_file.clone())
                }
            }

            if !missing.is_empty() {
                cache.hmw_manifest.verified = false;
            }

            missing
        });

        if !missing.is_empty() {
            error!(name: LOG_ONLY, "Missing HMW files: {}", missing.join(", "));
            global_state::UpdateCache::set(true);
            self.mod_verification = ModFileStatus::MissingFiles(missing);
            return Err(());
        }

        self.mod_verification = ModFileStatus::VerifyReady;
        Ok(())
    }

    fn verify_hmw_files(&mut self) -> Result<(), ()> {
        if matches!(
            self.mod_verification,
            ModFileStatus::Initial | ModFileStatus::MissingFiles(_)
        ) && self.prep_verification_state().is_err()
        {
            return Ok(());
        }

        let exe_dir = self.get_exe_dir();
        let start = std::time::Instant::now();

        let outdated = global_state::Cache::with_borrow_mut(|cache| {
            if cache.hmw_manifest.files_with_hashes.is_empty() {
                error!("No manifest data available to verify files");
                return Vec::new();
            }

            let file_ct =
                NonZero::new(cache.hmw_manifest.files_with_hashes.len()).expect("early return");

            let progress = ProgressBar::new("Verifying", "Files", file_ct);

            let outdated = cache
                .hmw_manifest
                .files_with_hashes
                .par_iter()
                .filter_map(|(file, expected)| {
                    let hash = hash_file_hex(&exe_dir.join(file)).unwrap_or_else(|err| {
                        if err.kind() == ErrorKind::NotFound {
                            error!("file: {file}, not found",);
                        } else {
                            error!("file: {file}, {err}",);
                        }
                        String::new()
                    });
                    progress.tick();
                    (hash != *expected).then(|| file.clone())
                })
                .collect::<Vec<_>>();

            cache.hmw_manifest.verified = outdated.is_empty();

            progress.finish();
            outdated
        });

        if !outdated.is_empty() {
            error!(name: LOG_ONLY, "Outdated HMW files: {}", outdated.join(", "));
            self.mod_verification = ModFileStatus::Outdated(outdated);
            return Err(());
        }

        self.mod_verification = ModFileStatus::UpToDate;
        info!(name: LOG_ONLY, "Verified files in {:#?}", start.elapsed());

        global_state::UpdateCache::set(true);
        Ok(())
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
    queued_con_task: Option<JoinHandle<()>>,
}

pub enum HistoryTag {
    Invalid,
}

impl HistoryTag {
    pub fn filter(tag: Option<u32>) -> bool {
        tag != Some(Self::Invalid as u32)
    }
}

impl Executor<Stdout> for CommandContext {
    async fn try_execute_command(
        &mut self,
        line_handle: &mut ReplHandle,
        user_tokens: Vec<String>,
    ) -> io::Result<CommandHandle> {
        let command = match try_parse_from(&user_tokens) {
            Ok(c) => c,
            Err(err) => {
                line_handle
                    .tag_last_history(|tag| *tag = Some(HistoryTag::Invalid as u32))
                    .expect("command was added");
                return err.print().map(|_| CommandHandle::Processed);
            }
        };

        global_state::UpdateCache::set(true);

        match command {
            Command::Filter { args } => self.new_favorites_with(line_handle, args).await,
            Command::Last => global_state::LastServerStats::display(line_handle)
                .map(|_| CommandHandle::Processed),
            Command::Reconnect { args } => self.reconnect(line_handle, args),
            Command::Launch => self.launch_handler(),
            Command::Cache { option } => self.modify_cache(option).await,
            Command::Console { all } => self.open_game_console(all),
            Command::GameDir => self.try_open_game_dir(),
            Command::LocalEnv => self.try_open_local_dir(),
            Command::Version { verify_all } => self.print_version(verify_all).await,
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
    pub hmw_manifest_task: JoinHandle<Result<HmwManifest, ResponseErr>>,
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
            startup_data.hmw_manifest_task,
            startup_data.cache_task
        );

        macro_rules! hmw_launch_err {
            ($($arg:tt)*) => {{
                error!("Could not launch H2M as child process: {}", format_args!($($arg)*));
                splash_screen::push_message(Message::Str(ConnectionHelp.into()));
                None
            }}
        }

        let game_console = match launch_res {
            Ok(Some(Ok(handle))) => Some(handle),
            Ok(None) => None,
            Err(join_err) => hmw_launch_err!("{join_err}"),
            Ok(Some(Err(launch_err))) => hmw_launch_err!("{launch_err}"),
        };

        let console_set = game_console.is_some();
        global_state::PtyHandle::set(game_console);

        if let (Some(latest), Some(msg)) = (&app.ver_latest, &app.update_msg) {
            if CRATE_VER != latest {
                info!("{msg}")
            }
        }

        match hmw_hash_res {
            Ok(Ok(latest_manifest)) => {
                startup_data
                    .game
                    .conditional_condense_manifest(latest_manifest);

                if startup_data.game.prep_verification_state().is_err() {
                    splash_screen::push_message(Message::str(
                        startup_data.game.mod_verification.to_string(),
                    ));
                } else if let (Some(hash_curr), Some(hash_latest)) = (
                    startup_data.game.hash_curr.as_deref(),
                    startup_data.game.hash_latest.as_deref(),
                ) {
                    if hash_curr != hash_latest {
                        info!("{HmwUpdateHelp}")
                    }
                }
            }
            Ok(Err(err)) => hmw_hash_err!("{err}"),
            Err(err) => hmw_hash_err!("{err}"),
        }

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
            queued_con_task: None,
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
    pub(crate) fn try_abort_queued_con(&mut self) -> bool {
        let Some(prev) = self
            .queued_con_task
            .take()
            .filter(|task| !task.is_finished())
        else {
            return false;
        };

        prev.abort();
        println!("{YELLOW}Previously queued connection attempt aborted{RESET}");

        true
    }
    pub(crate) fn set_queued_con(&mut self, task: JoinHandle<()>) {
        assert!(
            self.queued_con_task.replace(task).is_none(),
            "Task must be aborted prior to a new queued connection attempt is spawned"
        )
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
            return write_cache(
                self,
                &line_handle.export_filtered_history(HistoryTag::filter, Some(SAVED_HISTORY_CAP)),
            );
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

        if let Some(hwnd) = (game_open == self.can_close_console)
            .then(global_state::AppHWND::get)
            .flatten()
        {
            // Safety: `hwnd` only ever refers to the current process, making it so it _must_ always be a valid pointer
            return Ok(unsafe { toggle_close_state(&mut self.can_close_console, hwnd) });
        }

        Ok(Ok(()))
    }

    /// if calling manually you are responsible for setting pty inside of context
    pub fn listener_routine(&mut self) -> Result<(), String> {
        init_listener(self)?;

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
        repl: &mut ReplHandle,
        args: Option<Filters>,
    ) -> io::Result<CommandHandle> {
        let exe_dir = self.game.path.parent().expect("has parent");

        let build_res = build_favorites(
            repl,
            exe_dir,
            args.unwrap_or_default(),
            self.game.version.unwrap_or(1.0),
        )
        .await;

        if let Some(cache_modified) = CmdErr::transpose(build_res)? {
            global_state::UpdateCache::and_modify(|curr| curr || cache_modified);
        }

        Ok(CommandHandle::Processed)
    }

    async fn print_version(&mut self, verify_all: bool) -> io::Result<CommandHandle> {
        if self.game.hash_latest.is_none() {
            println!("{GREEN}Trying to get latest HMW version..{RESET}");

            if let Ok(latest_manifest) = get_latest_hmw_manifest().await.map_err(display::error) {
                self.game.conditional_condense_manifest(latest_manifest);
            }

            if self.game.hash_latest.is_some() {
                info!("Found latest HMW version")
            }

            let _ = self.game.prep_verification_state();
        }

        if verify_all {
            let _ = self.game.verify_hmw_files();
        }

        println!("{}", self.app);
        println!();
        println!("{}", self.game);
        Ok(CommandHandle::Processed)
    }

    async fn modify_cache(&mut self, arg: CacheCmd) -> io::Result<CommandHandle> {
        if self.local_dir.is_none() {
            error!("Can not create cache with out a valid save directory");
            return Ok(CommandHandle::Processed);
        }

        if let CacheCmd::Reset = arg {
            global_state::Cache::clear();
        }

        let hmw_manifest_task = tokio::spawn(get_latest_hmw_manifest());

        let mut cache_modified = build_cache()
            .await
            .map_err(|err| error!("{err}, cache remains unchanged"))
            .is_ok();

        match hmw_manifest_task.await {
            Ok(Ok(latest_manifest)) => {
                cache_modified |= self.game.conditional_condense_manifest(latest_manifest);
            }
            Ok(Err(err)) => hmw_hash_err!("{err}"),
            Err(err) => hmw_hash_err!("{err}"),
        };

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
            const TIMEOUT: Duration = Duration::from_secs(60);

            let spinner = Spinner::new(format!("Waiting for {} to close", self.game_name()));
            let start_time = std::time::Instant::now();
            while global_state::PtyHandle::check_connection().is_ok() {
                if start_time.elapsed() > TIMEOUT {
                    if let Ok(pid) = global_state::PtyHandle::try_if_alive(|pty| Ok(pty.get_pid()))
                        .map_err(display::log_error)
                    {
                        if terminate_process_by_id(pid)
                            .map_err(display::log_error)
                            .is_ok()
                        {
                            error!(name: LOG_ONLY, "Hang detected, {} terminated by windows api", self.game_name());
                        }
                    }

                    break;
                }

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
