pub(crate) mod status;

use status::ModFileStatus;

use crate::{
    CRATE_NAME, LOG_ONLY, MAIN_PROMPT,
    commands::{
        CommandContext, CommandErr, CommandHandle, CommandReturn, CommandSender, HistoryTag,
        HookTag, Message, ReplHandle,
        filter::build_favorites,
        launch::{WinApiErr, game_open, hide_pseudo_console, spawn_pseudo},
        tag_last_cmd,
    },
    exe_details, get_latest_hmw_manifest,
    models::cli::{CacheCmd, Command, Filters},
    open_dir, send_msg_over,
    utils::{
        caching::{build_cache, write_cache},
        display::{self, ConnectionHelp, DisplayLogs},
        main_thread_state::{self, ThreadCopyState, pty_handle::PseudoConStatus},
    },
};

use std::{
    borrow::Cow,
    io::{self, Stdout},
    path::Path,
};

use constcat::concat;
use crossterm::{
    QueueableCommand,
    event::{Event, KeyCode, KeyEvent, KeyModifiers},
    terminal::SetTitle,
};
use repl_oxide::{
    ansi_code::{GREEN, RED, RESET, YELLOW},
    clap::try_parse_from,
    executor::Executor,
    input_hook::{HookControl, HookID, HookStates, HookedEvent, InputHook},
};
use tokio::{task::JoinHandle, time::Duration};
use tracing::{error, info};

#[macro_export]
macro_rules! hmw_hash_err {
    ($($arg:tt)*) => {
        error!("Could not get latest HMW version - {}", format_args!($($arg)*))
    }
}

const QUIT_PROMPT: &str = concat!(
    "Press (",
    YELLOW,
    "y",
    RESET,
    ") or (",
    YELLOW,
    "ctrl_c",
    RESET,
    ") to close"
);

// MARK: TODO
// Make error messages part of `CommandReturn` / `CommandErr` then remove these macros

#[macro_export]
macro_rules! command_err {
    ($($arg:tt)*) => {{
        tracing::error!($($arg)*);
        CommandErr::Command
    }};
}

#[macro_export]
macro_rules! non_critical_err {
    ($($arg:tt)*) => {{
        tracing::error!($($arg)*);
        CommandErr::NonCritical
    }};
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
                tag_last_cmd(line_handle, HistoryTag::Invalid);
                return err.print().map(|_| CommandHandle::Processed);
            }
        };

        let command_res = match command {
            Command::Filter { args } => self.new_favorites_with(line_handle, args).await,
            Command::Last => main_thread_state::LastServerStats::display(line_handle)
                .map(|_| CommandReturn::processed())
                .unwrap_or_else(CommandReturn::critical_err),
            Command::Reconnect { args } => self.reconnect(line_handle, args),
            Command::Launch => self.launch_handler(),
            Command::Cache { option } => self.modify_cache(option).await,
            Command::Console { all } => self.open_game_console(all),
            Command::GameDir => self.try_open_game_dir(),
            Command::LocalEnv => self.try_open_local_dir(),
            Command::Version { verify_all } => self.print_version(verify_all).await,
            Command::Settings { use_default } => self.settings(line_handle, use_default),
            Command::Quit => self.quit(),
        };

        if let Some(CommandErr::Critical(err)) = command_res.err {
            return Err(err);
        }

        if HistoryTag::is_valid(command_res.tag.map(Into::into)) {
            main_thread_state::UpdateCache::set(true);
        }

        if let Some(tag) = command_res.tag {
            tag_last_cmd(line_handle, tag);
        }

        Ok(command_res.outcome)
    }
}

impl CommandContext {
    pub(super) fn try_abort_queued_con(&mut self) -> bool {
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
    pub(super) fn set_queued_con(&mut self, task: JoinHandle<()>) {
        assert!(
            self.queued_con_task.replace(task).is_none(),
            "Task must be aborted prior to a new queued connection attempt is spawned"
        )
    }

    fn try_open_explorer(dir: &Path) -> CommandReturn {
        if let Err(err) = open_dir(dir) {
            error!("{err}");
            return CommandReturn::non_critical_err();
        }
        CommandReturn::processed()
    }

    fn try_open_game_dir(&self) -> CommandReturn {
        let Some(dir) = self.game.path().parent() else {
            error!(
                "Game path: {}, has no valid parent",
                self.game.path().display()
            );
            return CommandReturn::non_critical_err();
        };
        Self::try_open_explorer(dir)
    }

    fn try_open_local_dir(&self) -> CommandReturn {
        let Some(dir) = self.local_dir.as_deref() else {
            error!("Failed to find local environment directory on startup");
            return CommandReturn::non_critical_err();
        };
        Self::try_open_explorer(dir)
    }

    pub fn graceful_shutdown(&mut self, line_handle: &ReplHandle) {
        self.save_cache_if_needed(line_handle)
            .unwrap_or_else(display::log_error);

        main_thread_state::pty_handle::try_drop_pty(self.game_name());

        if let Some(hwnd) = main_thread_state::app_hwnd::get().filter(|_| !self.can_close_console) {
            // Safety: `hwnd` only ever refers to the current process, making it so it _must_ always be a valid pointer
            unsafe { self.toggle_close_state(hwnd) }.unwrap_or_else(display::log_error);
        }

        info!(name: LOG_ONLY, "graceful app shutdown");
    }

    fn try_send_cmd_from_hook(&mut self, command: String) -> Result<(), Cow<'static, str>> {
        main_thread_state::pty_handle::try_if_alive(|game_console| game_console.send_cmd(command))
            .map_err(Into::into)
    }

    pub(super) fn launch_handler(&mut self) -> CommandReturn {
        let game_open = match game_open() {
            Ok(open_status) => open_status,
            Err(err) => {
                error!("{err}, could not determine if it is okay to launch");
                return CommandReturn::non_critical_err();
            }
        };

        let pty_active = match main_thread_state::pty_handle::game_connected() {
            Ok(status) => status,
            Err(err) => {
                error!(
                    "{}, could not determine if it is okay to launch",
                    err.to_string_lossy()
                );
                return CommandReturn::non_critical_err();
            }
        };

        if let Some(window_name) = game_open {
            if pty_active {
                println!(
                    "{GREEN}Connection to {} already active{RESET}",
                    self.game_name()
                );
            } else {
                println!("{RED}{window_name} is already running{RESET}\n{ConnectionHelp}");
            }
            return CommandReturn::non_critical_err();
        } else if pty_active {
            // Game window is not present, it must be in the process of closing
            main_thread_state::pty_handle::wait_for_exit(self.game_name());
        }

        match spawn_pseudo(self.game.path()) {
            Ok(conpty) => {
                info!("Launching {}...", self.game_name());
                self.game.update(exe_details(self.game.path()));
                main_thread_state::pty_handle::set(Some(conpty));
                self.listener_routine().unwrap_or_else(display::error);
                CommandReturn::processed()
            }
            Err(err) => {
                error!("{err}");
                CommandReturn::non_critical_err()
            }
        }
    }

    pub fn save_cache_if_needed(&self, line_handle: &ReplHandle) -> io::Result<()> {
        if !main_thread_state::UpdateCache::take() {
            return Ok(());
        }
        write_cache(self, line_handle)
    }

    pub fn handle_game_state_change(
        &mut self,
        line_handle: &mut ReplHandle,
    ) -> io::Result<Result<(), WinApiErr>> {
        let game_open = main_thread_state::pty_handle::game_connected()
            .map_err(|err| display::log_error(err.to_string_lossy()))
            .unwrap_or_default();

        line_handle.writer().queue(SetTitle(CRATE_NAME))?;

        if !game_open && line_handle.input_hooked() {
            line_handle.remove_all_hooks_with_tag(self, HookTag::GameConsole)?;
        }

        if let Some(hwnd) = (game_open == self.can_close_console)
            .then(main_thread_state::app_hwnd::get)
            .flatten()
        {
            // Safety: `hwnd` only ever refers to the current process, making it so it _must_ always be a valid pointer
            return Ok(unsafe { self.toggle_close_state(hwnd) });
        }

        Ok(Ok(()))
    }

    /// if calling manually you are responsible for setting pty inside of context
    pub fn listener_routine(&mut self) -> Result<(), String> {
        self.init_listener()?;

        let msg_sender = self.msg_sender();
        let game_name = self.game_name_owned();
        let game_state_change = self.game_state_change();
        tokio::spawn(async move {
            const SLEEP: Duration = Duration::from_millis(4500);
            let mut attempt = 1;
            let messages = loop {
                tokio::time::sleep(SLEEP * attempt).await;
                match main_thread_state::pty_handle::check_connection() {
                    Ok(PseudoConStatus::Attached) if attempt == 3 => {
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
                    Ok(PseudoConStatus::Attached) => attempt += 1,
                    Ok(PseudoConStatus::Closed) | Ok(PseudoConStatus::Unattached) => {
                        break vec![
                            Message::error(format!(
                                "Could not establish connection to {game_name}"
                            )),
                            Message::str(ConnectionHelp),
                        ];
                    }
                    Err(err) => break vec![Message::error(err.to_string())],
                }
            };

            for msg in messages {
                send_msg_over(&msg_sender, msg).await;
            }
        });
        Ok(())
    }

    async fn new_favorites_with(
        &self,
        repl: &mut ReplHandle,
        args: Option<Filters>,
    ) -> CommandReturn {
        match build_favorites(self, repl, args).await {
            Ok(cache_modified) => {
                main_thread_state::UpdateCache::and_modify(|curr| curr | cache_modified);
                CommandReturn::processed()
            }
            Err(err) => CommandReturn::err(err),
        }
    }

    async fn print_version(&mut self, verify_all: bool) -> CommandReturn {
        if let ModFileStatus::Initial = self.game.mod_verification {
            let _ = self.game.prep_verification_state();
        }

        if verify_all {
            self.game.update(exe_details(self.game.path()));

            if !self.game.manifest_verified() {
                println!("{RED}Failed to verify integrity of HMW manifest{RESET}");
                return CommandReturn::non_critical_err();
            }

            let _ = self.game.verify_hmw_files();
        }

        println!("{}\n\n{}", self.appdata, self.game);
        CommandReturn::processed()
    }

    async fn modify_cache(&mut self, arg: CacheCmd) -> CommandReturn {
        if self.local_dir.is_none() {
            error!("Can not create cache with out a valid save directory");
            return CommandReturn::non_critical_err();
        }

        if let CacheCmd::Reset = arg {
            main_thread_state::Cache::clear();
        }

        let hmw_manifest_task = tokio::spawn(get_latest_hmw_manifest());

        let build_res = build_cache()
            .await
            .map_err(|err| error!("{err}, cache remains unchanged"));

        let mut cache_modified = build_res.is_ok();
        let mut encountered_err = !cache_modified;

        let manifest_res = hmw_manifest_task.await;
        encountered_err |= matches!(manifest_res, Ok(Err(_)) | Err(_));

        match manifest_res {
            Ok(Ok(latest_manifest)) => {
                cache_modified |= self
                    .game
                    .conditional_condense_manifest(latest_manifest)
                    .is_some();
            }
            Ok(Err(err)) => hmw_hash_err!("{err}"),
            Err(err) => hmw_hash_err!("{err}"),
        };

        main_thread_state::UpdateCache::and_modify(|curr| curr | cache_modified);

        if encountered_err {
            return CommandReturn::non_critical_err();
        }

        CommandReturn::processed()
    }

    fn open_game_console(&mut self, all: bool) -> CommandReturn {
        if all {
            main_thread_state::ConsoleHistory::with_borrow_mut(|history| history.reset_i());
        }

        if main_thread_state::pty_handle::check_connection().is_err()
            || game_open()
                .unwrap_or_else(WinApiErr::resolve_to_closed)
                .is_none()
        {
            print!("{YELLOW}No active connection to {}", self.game_name());

            if main_thread_state::ConsoleHistory::with_borrow_mut(|history| {
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
            return CommandReturn::non_critical_err();
        }

        print!("{DisplayLogs}");

        let line_changes = HookStates::<CommandContext, _>::new(
            |handle, context| {
                main_thread_state::ForwardLogs::set(true);
                handle.set_prompt(&context.game.game_file_name());
                handle.disable_completion();
                handle.disable_line_stylization();
                Ok(())
            },
            |handle, _context| {
                main_thread_state::ForwardLogs::set(false);
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
                        if !cmd.trim().is_empty()
                            && let Err(err) = context.try_send_cmd_from_hook(cmd)
                        {
                            error!("{err}");
                            return HookedEvent::release_hook();
                        }
                    }
                    _ => handle.set_uneventful(),
                }
                HookedEvent::continue_hook()
            },
        );

        CommandReturn::insert_hook(input_hook)
    }

    fn quit(&mut self) -> CommandReturn {
        if game_open()
            .unwrap_or_else(WinApiErr::resolve_to_open)
            .is_none()
        {
            main_thread_state::pty_handle::wait_for_exit(self.game_name());
            return CommandReturn::exit();
        } else if main_thread_state::pty_handle::check_connection().is_err() {
            return CommandReturn::exit();
        }

        println!(
            "{RED}Quitting {CRATE_NAME} will also close {}\n{YELLOW}Are you sure you want to quit?{RESET}",
            self.game_name()
        );

        let line_changes = HookStates::new(
            |handle, _context| {
                handle.set_prompt(QUIT_PROMPT);
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

        CommandReturn::insert_hook(input_hook)
    }
}
