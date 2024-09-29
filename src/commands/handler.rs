use crate::{
    cli::{CacheCmd, Command, Filters, UserCommand},
    commands::{
        filter::build_favorites,
        launch_h2m::{h2m_running, initalize_listener, launch_h2m_pseudo},
        reconnect::reconnect,
    },
    utils::{
        caching::{build_cache, Cache},
        input::{
            line::{
                AsyncCtxCallback, EventLoop, InputHook, InputHookErr, LineCallback, LineData,
                LineReader,
            },
            style::{GREEN, RED, WHITE, YELLOW},
        },
    },
    CACHED_DATA,
};
use clap::Parser;
use crossterm::event::{Event, KeyCode, KeyEvent, KeyModifiers};
use std::{
    ffi::OsString,
    fmt::Display,
    path::{Path, PathBuf},
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
};
use tokio::sync::{mpsc::Sender, Mutex, RwLock};
use tracing::{error, info};
use winptyrs::PTY;

pub enum Message {
    Str(String),
    Info(String),
    Err(String),
    Warn(String),
}

pub struct CommandContext {
    cache: Arc<Mutex<Cache>>,
    exe_dir: Arc<PathBuf>,
    cache_needs_update: Arc<AtomicBool>,
    forward_logs: Arc<AtomicBool>,
    h2m_console_history: Arc<Mutex<Vec<String>>>,
    pty_handle: Option<Arc<RwLock<PTY>>>,
    local_dir: Option<Arc<PathBuf>>,
    msg_sender: Arc<Sender<Message>>,
    h2m_version: f64,
}

impl CommandContext {
    #[inline]
    pub fn cache(&self) -> Arc<Mutex<Cache>> {
        self.cache.clone()
    }
    #[inline]
    pub fn exe_dir(&self) -> Arc<PathBuf> {
        self.exe_dir.clone()
    }
    #[inline]
    pub fn cache_needs_update(&self) -> Arc<AtomicBool> {
        self.cache_needs_update.clone()
    }
    #[inline]
    pub fn forward_logs(&self) -> Arc<AtomicBool> {
        self.forward_logs.clone()
    }
    pub async fn check_h2m_connection(&mut self) -> Result<(), String> {
        let mut error = String::from("No Pseudoconsole set");
        if let Some(ref lock) = self.pty_handle {
            let handle = lock.read().await;
            match handle.is_alive() {
                Ok(true) => return Ok(()),
                Ok(false) => {
                    return Err(String::from(
                        "No connection to H2M is active, use 'launch' command to start H2M",
                    ))
                }
                Err(err) => error = err.to_string_lossy().to_string(),
            }
        }
        self.pty_handle = None;
        Err(error)
    }
    #[inline]
    pub fn local_dir(&self) -> Option<Arc<PathBuf>> {
        self.local_dir.as_ref().map(Arc::clone)
    }
    #[inline]
    pub fn update_local_dir(&mut self, local_dir: PathBuf) {
        self.local_dir = Some(Arc::new(local_dir))
    }
    #[inline]
    pub fn h2m_console_history(&self) -> Arc<Mutex<Vec<String>>> {
        self.h2m_console_history.clone()
    }
    #[inline]
    pub fn pty_handle(&self) -> Option<Arc<RwLock<PTY>>> {
        self.pty_handle.clone()
    }
    #[inline]
    pub fn msg_sender(&self) -> Arc<Sender<Message>> {
        self.msg_sender.clone()
    }
    #[inline]
    pub fn h2m_version(&self) -> f64 {
        self.h2m_version
    }
    #[inline]
    fn init_pty(&mut self, pty: PTY) {
        self.pty_handle = Some(Arc::new(RwLock::new(pty)))
    }
}

#[derive(Default)]
pub struct CommandContextBuilder {
    cache: Option<Cache>,
    launch_res: Option<Result<(PTY, f64), String>>,
    exe_dir: Option<PathBuf>,
    msg_sender: Option<Sender<Message>>,
    local_dir: Option<PathBuf>,
}

impl CommandContextBuilder {
    pub fn new() -> Self {
        CommandContextBuilder::default()
    }
    pub fn cache(mut self, cache: Cache) -> Self {
        self.cache = Some(cache);
        self
    }
    pub fn exe_dir(mut self, exe_dir: PathBuf) -> Self {
        self.exe_dir = Some(exe_dir);
        self
    }
    pub fn msg_sender(mut self, sender: Sender<Message>) -> Self {
        self.msg_sender = Some(sender);
        self
    }
    pub fn local_dir(mut self, local_dir: Option<PathBuf>) -> Self {
        self.local_dir = local_dir;
        self
    }
    pub fn launch_res(mut self, res: Result<(PTY, f64), String>) -> Self {
        self.launch_res = Some(res);
        self
    }

    pub fn build(self) -> Result<CommandContext, &'static str> {
        let (handle, version) = match self.launch_res {
            Some(Ok((handle, version))) => (Some(handle), Some(version)),
            Some(Err(err)) => {
                error!("{err}");
                (None, None)
            }
            None => (None, None),
        };
        Ok(CommandContext {
            cache: self
                .cache
                .map(|cache| Arc::new(Mutex::new(cache)))
                .ok_or("cache is required")?,
            exe_dir: self.exe_dir.map(Arc::new).ok_or("exe_dir is required")?,
            msg_sender: self
                .msg_sender
                .map(Arc::new)
                .ok_or("msg_sender is required")?,
            pty_handle: handle.map(|pty| Arc::new(RwLock::new(pty))),
            local_dir: self.local_dir.map(Arc::new),
            cache_needs_update: Arc::new(AtomicBool::new(false)),
            forward_logs: Arc::new(AtomicBool::new(false)),
            h2m_console_history: Arc::new(Mutex::new(Vec::<String>::new())),
            h2m_version: version.unwrap_or(1.0),
        })
    }
}

pub enum CommandHandle {
    Processed,
    Callback((Box<LineCallback>, InputHook)),
    Exit,
}

pub async fn try_execute_command(
    mut user_args: Vec<String>,
    context: &mut CommandContext,
) -> CommandHandle {
    let mut input_tokens = vec![String::new()];
    input_tokens.append(&mut user_args);
    match UserCommand::try_parse_from(input_tokens) {
        Ok(cli) => match cli.command {
            Command::Filter { args } => new_favorites_with(args, context).await,
            Command::Reconnect { args } => reconnect(args, context).await,
            Command::Launch => launch_handler(context).await,
            Command::Cache { option } => modify_cache(context, option).await,
            Command::Console => open_h2m_console(context).await,
            Command::GameDir => open_dir(Some(context.exe_dir.as_path())),
            Command::LocalEnv => open_dir(context.local_dir.as_ref().map(|i| i.as_path())),
            Command::Version => print_version(context.h2m_version),
            Command::Quit => quit(context).await,
        },
        Err(err) => {
            if let Err(prt_err) = err.print() {
                error!("{err} {prt_err}");
            }
            CommandHandle::Processed
        }
    }
}

async fn new_favorites_with(args: Option<Filters>, context: &CommandContext) -> CommandHandle {
    let cache = context.cache();
    let exe_dir = context.exe_dir();

    let new_entries_found = build_favorites(
        exe_dir,
        &args.unwrap_or_default(),
        cache,
        context.h2m_version,
    )
    .await
    .unwrap_or_else(|err| {
        error!("{err}");
        false
    });
    if new_entries_found {
        context.cache_needs_update().store(true, Ordering::Release);
    }

    CommandHandle::Processed
}

async fn modify_cache(context: &CommandContext, arg: CacheCmd) -> CommandHandle {
    let Some(ref local_dir) = context.local_dir else {
        error!("Can not create cache with out a valid save directory");
        return CommandHandle::Processed;
    };

    let cache_file = match arg {
        CacheCmd::Update => {
            let cache_arc = context.cache();
            let cache = cache_arc.lock().await;

            match build_cache(Some(&cache.connection_history), Some(&cache.ip_to_region)).await {
                Ok(data) => data,
                Err(err) => {
                    error!("{err}, cache remains unchanged");
                    return CommandHandle::Processed;
                }
            }
        }
        CacheCmd::Reset => match build_cache(None, None).await {
            Ok(data) => data,
            Err(err) => {
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
    let cache = context.cache();
    let mut cache = cache.lock().await;
    *cache = Cache::from(cache_file);
    CommandHandle::Processed
}

pub async fn launch_handler(context: &mut CommandContext) -> CommandHandle {
    match launch_h2m_pseudo(&context.exe_dir) {
        Ok((conpty, version)) => {
            info!("Launching H2M-mod...");
            context.init_pty(conpty);
            context.h2m_version = version;
            if let Err(err) = listener_routine(context).await {
                error!("{err}")
            }
        }
        Err(err) => error!("{err}"),
    };
    CommandHandle::Processed
}

/// if calling manually you are responsible for setting pty and version inside of context
pub async fn listener_routine(context: &mut CommandContext) -> Result<(), String> {
    initalize_listener(context).await?;
    let pty = context.pty_handle();
    let msg_sender = context.msg_sender();
    tokio::task::spawn(async move {
        tokio::time::sleep(tokio::time::Duration::from_secs(15)).await;
        if let Some(lock) = pty {
            let handle = lock.read().await;
            let msg = match handle.is_alive() {
                Ok(true) => Message::Info(String::from("Connected to H2M-mod console")),
                Ok(false) => {
                    Message::Err(String::from("Could not establish connection to H2M-mod, use command `launch` to re-launch game"))
                }
                Err(err) => Message::Err(err.to_string_lossy().to_string()),
            };
            msg_sender
                .send(msg)
                .await
                .unwrap_or_else(|err| error!("{err}"));
        }
    });
    Ok(())
}

struct DisplayLogs<'a>(&'a [String]);

impl<'a> Display for DisplayLogs<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for line in self.0 {
            writeln!(f, "{line}")?;
        }
        Ok(())
    }
}

#[inline]
pub fn end_forward(context: &mut CommandContext) {
    context.forward_logs().store(false, Ordering::SeqCst);
}

async fn open_h2m_console(context: &mut CommandContext) -> CommandHandle {
    if context.check_h2m_connection().await.is_ok() && h2m_running() {
        {
            let history = context.h2m_console_history.lock().await;
            context.forward_logs.store(true, Ordering::SeqCst);
            print!("{}", DisplayLogs(&history));
        }

        let uid = InputHook::new_uid();

        let init = |handle: &mut LineReader<'_>| {
            handle.set_prompt(String::from("h2m-mod.exe"));
            handle.set_completion(false);
            Ok(())
        };

        let input_hook = move |handle: &mut LineReader<'_>, event: Event| match event {
            Event::Key(KeyEvent {
                code: KeyCode::Char('c'),
                modifiers: KeyModifiers::CONTROL,
                ..
            }) => {
                if !handle.line.input().is_empty() {
                    handle.ctrl_c_line()?;
                    return Ok((EventLoop::Continue, false));
                }
                handle.set_prompt(LineData::default_prompt());
                handle.set_completion(true);
                Ok((EventLoop::Callback(Box::new(end_forward)), true))
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
                    handle.set_prompt(LineData::default_prompt());
                    handle.set_completion(true);
                    return Ok((EventLoop::Callback(Box::new(end_forward)), true));
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

                let send_cmd: Box<AsyncCtxCallback> =
                    Box::new(move |context: &mut CommandContext| {
                        Box::pin(async move {
                            context.check_h2m_connection().await.map_err(|err| {
                                InputHookErr::new(uid, format!("Could not send command: {err}"))
                            })?;

                            let pty_handle = context.pty_handle().expect("above guard");
                            let h2m_console = pty_handle.write().await;

                            if h2m_console.write(OsString::from(cmd + "\r\n")).is_err() {
                                error!("failed to write command to h2m console");
                            }
                            Ok(())
                        })
                    });

                Ok((EventLoop::AsyncCallback(send_cmd), false))
            }
            _ => Ok((EventLoop::Continue, false)),
        };

        return CommandHandle::Callback((
            Box::new(init),
            InputHook::new(uid, Box::new(input_hook)),
        ));
    }

    let history = context.h2m_console_history.lock().await;
    if !history.is_empty() {
        println!("{YELLOW}No active connection to H2M, displaying old logs{WHITE}");
        std::thread::sleep(std::time::Duration::from_secs(2));
        print!("{}", DisplayLogs(&history));
    } else {
        println!("{YELLOW}No active connection to H2M{WHITE}");
    }
    CommandHandle::Processed
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

fn print_version(h2m_v: f64) -> CommandHandle {
    println!(
        "{}.exe {GREEN}v{}{WHITE}",
        env!("CARGO_PKG_NAME"),
        env!("CARGO_PKG_VERSION")
    );
    if h2m_v != 1.0 {
        println!("h2m-mod.exe {GREEN}v{h2m_v}{WHITE}")
    }
    CommandHandle::Processed
}

async fn quit(context: &mut CommandContext) -> CommandHandle {
    if context.check_h2m_connection().await.is_ok() && h2m_running() {
        println!(
            "{RED}Quitting {} will also close H2M-mod\n{YELLOW}Are you sure you want to quit?{WHITE}",
            env!("CARGO_PKG_NAME")
        );

        let init = |handle: &mut LineReader<'_>| {
            handle.set_prompt(String::from("Press (y) or (ctrl_c) to close"));
            Ok(())
        };

        let input_hook = |handle: &mut LineReader<'_>, event: Event| match event {
            Event::Key(KeyEvent {
                code: KeyCode::Char('c'),
                modifiers: KeyModifiers::CONTROL,
                ..
            }) => Ok((EventLoop::Break, true)),
            Event::Key(KeyEvent {
                code: KeyCode::Char('y'),
                ..
            }) => Ok((EventLoop::Break, true)),
            _ => {
                handle.set_prompt(LineData::default_prompt());
                Ok((EventLoop::Continue, true))
            }
        };

        return CommandHandle::Callback((
            Box::new(init),
            InputHook::new(InputHook::new_uid(), Box::new(input_hook)),
        ));
    }
    CommandHandle::Exit
}
