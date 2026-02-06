pub mod filter;
pub mod handler;
pub mod launch;
pub mod reconnect;
pub mod settings;

use crate::{
    LOG_ONLY, MAIN_PROMPT,
    display::{self, ConnectionHelp, UPDATE_MSG_HMW},
    hmw_hash_err,
    models::json_data::HmwManifest,
    splash_screen,
    utils::{
        details::{AppDetails, GameDetails},
        main_thread_state::{self, ThreadCopyState},
        request::ResponseErr,
    },
};
use handler::status::ModFileStatus;
use launch::LaunchError;
use settings::{Settings, SettingsRenderer};

use std::{
    borrow::Cow,
    fmt::Display,
    io::{self, Stdout},
    net::SocketAddr,
    path::{Path, PathBuf},
    sync::Arc,
    time::Duration,
};

use repl_oxide::{
    Repl,
    ansi_code::{GREEN, RED, RESET, YELLOW},
    executor::CommandHandle as CmdHandle,
    input_hook::InputHook,
};
use tokio::{
    sync::{
        Notify,
        mpsc::{Receiver, Sender, channel},
    },
    task::JoinHandle,
};
use tracing::{error, info, warn};
use winptyrs::PTY;

const CONSEC_CMD_DELAY: Duration = Duration::from_millis(15);

#[macro_export]
macro_rules! from_static_cow_fn {
    ($ident:ident, $fn_name:ident) => {
        #[inline]
        pub fn $fn_name<T: Into<Cow<'static, str>>>(msg: T) -> Self {
            Self::$ident(msg.into())
        }
    };
}

macro_rules! unitOnlyDiscriminant {
    ($self:ty) => {
        impl $self {
            #[inline]
            const fn discriminant(self) -> u32 {
                self as u32
            }
        }

        impl From<$self> for u32 {
            fn from(tag: $self) -> Self {
                tag.discriminant()
            }
        }
    };
}

type CommandHandle = CmdHandle<CommandContext, Stdout>;
pub type ReplHandle = Repl<CommandContext, Stdout>;

/// Notes:
/// - `discriminant` impl will break if `HistoryTag` contains a non unit variant
/// - [`Self::is_valid`] will need to be updated if any _valid_ variant is added
#[derive(Clone, Copy)]
pub(crate) enum HistoryTag {
    Invalid,
    Nondeterministic,
}

unitOnlyDiscriminant!(HistoryTag);

impl HistoryTag {
    // Command: 'last --refresh' depends on the default behavior of valid entries being none.
    // If this changes when previous history are loaded on startup, all entries will have to be
    // marked as valid.
    pub(crate) fn is_valid<T: Into<u32>>(tag: Option<T>) -> bool {
        tag.is_none()
    }
}

#[derive(Default)]
pub struct StartupCacheContents {
    pub command_history: Vec<String>,
    pub modified: bool,
}

// Note: `discriminant` impl will break if `HookTag` contains a non unit variant
enum HookTag {
    GameConsole,
}

unitOnlyDiscriminant!(HookTag);

pub struct StartupData {
    pub local_dir: Option<PathBuf>,
    pub game_data: GameDetails,
    pub app_data: AppDetails,
    pub settings: Settings,
    pub cache_task: JoinHandle<StartupCacheContents>,
    pub launch_task: JoinHandle<Option<Result<PTY, LaunchError>>>,
    pub hmw_manifest_task: JoinHandle<Result<HmwManifest, ResponseErr>>,
}

pub struct CommandContext {
    can_close_console: bool,
    local_dir: Option<PathBuf>,
    msg_sender: Sender<Message>,
    pub game_state_change: Arc<Notify>,
    game: GameDetails,
    appdata: AppDetails,
    pub settings: Settings,
    settings_disp: SettingsRenderer,
    queued_con_task: Option<JoinHandle<()>>,
}

impl CommandContext {
    pub async fn from(
        mut startup_data: StartupData,
        splash_task: std::thread::JoinHandle<io::Result<()>>,
        term: Stdout,
    ) -> (ReplHandle, Self, Receiver<Message>) {
        let (launch_res, hmw_hash_res, cache_res) = tokio::join!(
            startup_data.launch_task,
            startup_data.hmw_manifest_task,
            startup_data.cache_task
        );

        macro_rules! hmw_launch_err {
            ($($arg:tt)*) => {{
                error!("Could not launch game as child process: {}", format_args!($($arg)*));
                main_thread_state::alt_screen::push_message(Message::Str(ConnectionHelp.into()));
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
        main_thread_state::pty_handle::set(game_console);

        if let (Some(curr), Some(latest)) = (
            startup_data.app_data.hash_curr.as_deref(),
            startup_data.app_data.hash_latest.as_deref(),
        ) && curr != latest
        {
            let msg = startup_data
                .app_data
                .update_msg
                .as_deref()
                .expect("Must be `Some` if `hash_latest` is `Some`");
            info!("{msg}");
        }

        match hmw_hash_res {
            Ok(Ok(latest_manifest)) => {
                let update_msg = startup_data
                    .game_data
                    .conditional_condense_manifest(latest_manifest)
                    .filter(|m| !m.is_empty())
                    .map(|_| UPDATE_MSG_HMW);

                let _ = startup_data.game_data.prep_verification_state();

                if let (Some(hash_curr), Some(hash_latest)) = (
                    startup_data.game_data.hash_curr.as_deref(),
                    startup_data.game_data.hash_latest.as_deref(),
                ) && hash_curr != hash_latest
                    && startup_data.game_data.mod_verification == ModFileStatus::UpToDate
                {
                    startup_data.game_data.mod_verification = ModFileStatus::VerifyReady;
                }

                if startup_data.game_data.mod_verification != ModFileStatus::UpToDate {
                    main_thread_state::alt_screen::push_message(if let Some(msg) = update_msg {
                        Message::str(msg)
                    } else {
                        Message::str(startup_data.game_data.mod_verification.to_string())
                    });
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
        main_thread_state::UpdateCache::set(startup_contents.modified);

        let mut ctx = CommandContext {
            msg_sender: message_tx,
            game_state_change: Arc::new(Notify::const_new()),
            appdata: startup_data.app_data,
            game: startup_data.game_data,
            settings: startup_data.settings,
            settings_disp: SettingsRenderer::new(),
            local_dir: startup_data.local_dir,
            can_close_console: true,
            queued_con_task: None,
        };

        if console_set {
            ctx.listener_routine().unwrap_or_else(display::warning);
        }

        let repl = Repl::new(term)
            .with_prompt(MAIN_PROMPT)
            .with_completion(crate::models::command_scheme::COMPLETION)
            .with_custom_quit_command("quit")
            .with_history_entries(&startup_contents.command_history)
            .with_custom_parse_err_hook(|repl, err| {
                tag_last_cmd(repl, HistoryTag::Invalid);
                println!("{RED}{err}{RESET}");
                Ok(())
            })
            .build()
            .expect("`Stdout` accepts crossterm commands");

        splash_screen::leave(splash_task);

        (repl, ctx, message_rx)
    }

    #[inline]
    pub(crate) fn local_dir(&self) -> Option<&Path> {
        self.local_dir.as_deref()
    }

    #[inline]
    pub(crate) fn get_exe_dir(&self) -> &Path {
        self.game.get_exe_dir()
    }

    #[inline]
    fn msg_sender(&self) -> Sender<Message> {
        self.msg_sender.clone()
    }

    #[inline]
    fn game_state_change(&self) -> Arc<Notify> {
        Arc::clone(&self.game_state_change)
    }

    #[inline]
    pub(crate) fn game_version(&self) -> Option<f64> {
        self.game.version
    }

    #[inline]
    pub(crate) fn mod_files_verified(&self) -> bool {
        self.game.mod_verification == ModFileStatus::UpToDate
    }

    #[inline]
    pub(crate) fn game_name_owned(&self) -> Cow<'static, str> {
        self.game.game_name_owned()
    }

    #[inline]
    pub(crate) fn game_name(&self) -> &str {
        self.game.game_name()
    }
}

pub(crate) enum CommandErr {
    /// An `io::Error` originating from the REPL's writer
    Critical(io::Error),
    /// An error that is **not** stemming from an ill formed command input
    NonCritical,
    /// An error from an attempt to process an invalid command
    Command,
}

impl From<io::Error> for CommandErr {
    /// This implementation of `from` should be used carefully, not _all_ `io::Error`s are critical,
    /// but **all** `io::Error`s that originate from the repl_oxide crate are.
    fn from(err: io::Error) -> Self {
        Self::Critical(err)
    }
}

pub(crate) struct CommandReturn {
    outcome: CommandHandle,
    tag: Option<HistoryTag>,
    err: Option<CommandErr>,
}

impl CommandReturn {
    const fn processed() -> Self {
        Self {
            outcome: CommandHandle::Processed,
            tag: None,
            err: None,
        }
    }

    fn insert_hook(hook: InputHook<CommandContext, Stdout>) -> Self {
        Self {
            outcome: CommandHandle::InsertHook(hook),
            tag: None,
            err: None,
        }
    }

    const fn tagged(tag: HistoryTag) -> Self {
        Self {
            outcome: CommandHandle::Processed,
            tag: Some(tag),
            err: None,
        }
    }

    const fn exit() -> Self {
        Self {
            outcome: CommandHandle::Exit,
            tag: None,
            err: None,
        }
    }

    const fn command_err() -> Self {
        Self {
            outcome: CommandHandle::Processed,
            tag: Some(HistoryTag::Invalid),
            err: Some(CommandErr::Command),
        }
    }

    const fn non_critical_err() -> Self {
        Self {
            outcome: CommandHandle::Processed,
            tag: None,
            err: Some(CommandErr::NonCritical),
        }
    }

    fn critical_err(err: io::Error) -> Self {
        Self {
            outcome: CmdHandle::Exit,
            tag: None,
            err: Some(CommandErr::Critical(err)),
        }
    }

    fn err(err: CommandErr) -> Self {
        let outcome = match err {
            CommandErr::Critical(_) => CommandHandle::Exit,
            CommandErr::Command | CommandErr::NonCritical => CommandHandle::Processed,
        };

        let tag = match err {
            CommandErr::Critical(_) | CommandErr::NonCritical => None,
            CommandErr::Command => Some(HistoryTag::Invalid),
        };

        Self {
            outcome,
            tag,
            err: Some(err),
        }
    }
}

/// Caller must guarantee an entry exists in history
fn tag_last_cmd(repl: &mut ReplHandle, tag: HistoryTag) {
    repl.tag_last_history(|entry_tag| *entry_tag = Some(tag.discriminant()))
        .expect("command was added")
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

    from_static_cow_fn!(Str, str);
    from_static_cow_fn!(Info, info);
    from_static_cow_fn!(Warn, warn);
    from_static_cow_fn!(Err, error);
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

pub(crate) trait CommandSender {
    fn send_cmd<S: AsRef<str>>(&self, command: S) -> Result<(), Cow<'static, str>>;
    fn send_connect(&self, ip: SocketAddr) -> Result<(), Cow<'static, str>>;
}

impl CommandSender for PTY {
    /// Before calling be sure to guard against invalid handles by checking pty connection is alive
    fn send_cmd<S: AsRef<str>>(&self, command: S) -> Result<(), Cow<'static, str>> {
        const NEW_LINE: &str = "\r\n";
        let cmd_str = command.as_ref();

        let mut os_command = std::ffi::OsString::from(cmd_str);
        os_command.push(NEW_LINE);

        match self.write(os_command) {
            Ok(n_chars) => {
                if n_chars != (cmd_str.chars().count() + NEW_LINE.len()) as u32 {
                    return Err(Cow::Borrowed("Failed to send command to game console"));
                }
                Ok(())
            }
            Err(err) => Err(Cow::Owned(err.to_string_lossy().to_string())),
        }
    }

    /// Before calling be sure to guard against invalid handles by checking pty connection is alive
    fn send_connect(&self, ip_port: SocketAddr) -> Result<(), Cow<'static, str>> {
        self.send_cmd("disconnect")?;
        std::thread::sleep(CONSEC_CMD_DELAY);
        self.send_cmd(format!("connect {ip_port}"))
    }
}
