use crate::{
    cli::{Command, Filters, UserCommand},
    commands::{filter::build_favorites, launch_h2m::HostName, reconnect::reconnect},
    utils::caching::Cache,
};
use clap::Parser;
use std::{
    path::{Path, PathBuf},
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
};
use tokio::{runtime, sync::Mutex, task::JoinHandle};
use tracing::error;
use winptyrs::PTY;

pub struct CommandContext<'a> {
    cache: &'a Arc<Mutex<Cache>>,
    exe_dir: &'a Arc<PathBuf>,
    cache_needs_update: &'a Arc<AtomicBool>,
    command_runtime: &'a runtime::Handle,
    connected_to_pseudoterminal: &'a Arc<AtomicBool>,
    h2m_console_history: &'a Arc<Mutex<Vec<String>>>,
    h2m_server_connection_history: &'a Arc<Mutex<Vec<HostName>>>,
    h2m_handle: Option<Arc<PTY>>,
    command_entered: bool,
    local_dir: Option<&'a Arc<PathBuf>>,
}

impl<'a> CommandContext<'a> {
    pub fn cache(&self) -> Arc<Mutex<Cache>> {
        Arc::clone(self.cache)
    }
    pub fn exe_dir(&self) -> Arc<PathBuf> {
        Arc::clone(self.exe_dir)
    }
    pub fn cache_needs_update(&self) -> Arc<AtomicBool> {
        Arc::clone(self.cache_needs_update)
    }
    pub fn command_runtime(&self) -> &'a runtime::Handle {
        self.command_runtime
    }
    pub fn connected_to_pseudoterminal(&self) -> Arc<AtomicBool> {
        Arc::clone(self.connected_to_pseudoterminal)
    }
    pub fn local_dir(&self) -> Result<Arc<PathBuf>, &'static str> {
        if let Some(arc_ref) = self.local_dir {
            return Ok(Arc::clone(arc_ref));
        }
        Err("Local dir was not set")
    }
    pub fn update_local_dir(&mut self, local_dir: &'a Arc<PathBuf>) {
        self.local_dir = Some(local_dir)
    }
    pub fn h2m_console_history(&self) -> Arc<Mutex<Vec<String>>> {
        Arc::clone(self.h2m_console_history)
    }
    pub fn h2m_server_connection_history(&self) -> Arc<Mutex<Vec<HostName>>> {
        Arc::clone(self.h2m_server_connection_history)
    }
    pub fn h2m_handle(&self) -> Option<Arc<PTY>> {
        self.h2m_handle.as_ref().map(Arc::clone)
    }
    pub fn update_h2m_handle(&mut self, handle: PTY) {
        self.h2m_handle = Some(Arc::new(handle))
    }
    pub fn was_command_entered(&self) -> bool {
        self.command_entered
    }
    fn command_entered(&mut self) {
        self.command_entered = true
    }
    pub fn command_handled(&mut self) {
        self.command_entered = false
    }
}

#[derive(Default)]
pub struct CommandContextBuilder<'a> {
    cache: Option<&'a Arc<Mutex<Cache>>>,
    exe_dir: Option<&'a Arc<PathBuf>>,
    cache_needs_update: Option<&'a Arc<AtomicBool>>,
    command_runtime: Option<&'a runtime::Handle>,
    local_dir: Option<&'a Arc<PathBuf>>,
    h2m_console_history: Option<&'a Arc<Mutex<Vec<String>>>>,
    h2m_server_connection_history: Option<&'a Arc<Mutex<Vec<HostName>>>>,
    h2m_handle: Option<Arc<PTY>>,
    connected_to_pseudoterminal: Option<&'a Arc<AtomicBool>>,
}

impl<'a> CommandContextBuilder<'a> {
    pub fn new() -> Self {
        CommandContextBuilder::default()
    }

    pub fn cache(mut self, cache: &'a Arc<Mutex<Cache>>) -> Self {
        self.cache = Some(cache);
        self
    }
    pub fn exe_dir(mut self, exe_dir: &'a Arc<PathBuf>) -> Self {
        self.exe_dir = Some(exe_dir);
        self
    }
    pub fn cache_needs_update(mut self, cache_needs_update: &'a Arc<AtomicBool>) -> Self {
        self.cache_needs_update = Some(cache_needs_update);
        self
    }
    pub fn command_runtime(mut self, command_runtime: &'a runtime::Handle) -> Self {
        self.command_runtime = Some(command_runtime);
        self
    }
    pub fn local_dir(mut self, local_dir: Option<&'a Arc<PathBuf>>) -> Self {
        self.local_dir = local_dir;
        self
    }
    pub fn h2m_console_history(mut self, h2m_console_history: &'a Arc<Mutex<Vec<String>>>) -> Self {
        self.h2m_console_history = Some(h2m_console_history);
        self
    }
    pub fn h2m_server_connection_history(
        mut self,
        h2m_server_connection_history: &'a Arc<Mutex<Vec<HostName>>>,
    ) -> Self {
        self.h2m_server_connection_history = Some(h2m_server_connection_history);
        self
    }
    pub fn h2m_handle(mut self, handle: Option<PTY>) -> Self {
        self.h2m_handle = handle.map(Arc::new);
        self
    }
    pub fn connected_to_pseudoterminal(
        mut self,
        connected_to_pseudoterminal: &'a Arc<AtomicBool>,
    ) -> Self {
        self.connected_to_pseudoterminal = Some(connected_to_pseudoterminal);
        self
    }

    pub fn build(self) -> Result<CommandContext<'a>, &'static str> {
        Ok(CommandContext {
            cache: self.cache.ok_or("cache is required")?,
            exe_dir: self.exe_dir.ok_or("exe_dir is required")?,
            cache_needs_update: self
                .cache_needs_update
                .ok_or("cache_needs_update is required")?,
            h2m_console_history: self
                .h2m_console_history
                .ok_or("h2m_console_history is required")?,
            h2m_server_connection_history: self
                .h2m_server_connection_history
                .ok_or("h2m_server_connection_history is required")?,
            h2m_handle: self.h2m_handle,
            connected_to_pseudoterminal: self
                .connected_to_pseudoterminal
                .ok_or("connected_to_pseudoterminal is required")?,
            command_runtime: self.command_runtime.ok_or("command_runtime is required")?,
            command_entered: false,
            local_dir: self.local_dir,
        })
    }
}

#[derive(Default)]
pub struct CommandHandle {
    pub handle: Option<JoinHandle<()>>,
    pub exit: bool,
}

impl CommandHandle {
    fn exit() -> Self {
        CommandHandle {
            handle: None,
            exit: true,
        }
    }

    fn with_handle(handle: JoinHandle<()>) -> Self {
        CommandHandle {
            handle: Some(handle),
            exit: false,
        }
    }
}

pub fn try_execute_command(
    mut user_args: Vec<String>,
    command_context: &mut CommandContext,
) -> CommandHandle {
    let mut input_tokens = vec![String::new()];
    input_tokens.append(&mut user_args);
    command_context.command_entered();
    match UserCommand::try_parse_from(input_tokens) {
        Ok(cli) => match cli.command {
            Command::Filter { args } => new_favorites_with(args, command_context),
            Command::Reconnect { args } => reconnect(args, command_context),
            Command::GameDir => open_dir(Some(command_context.exe_dir.as_path())),
            Command::LocalEnv => open_dir(command_context.local_dir.map(|i| i.as_path())),
            Command::Quit => CommandHandle::exit(),
        },
        Err(err) => {
            if let Err(err) = err.print() {
                error!("{err}");
            }
            CommandHandle::default()
        }
    }
}

fn new_favorites_with(args: Option<Filters>, command_context: &CommandContext) -> CommandHandle {
    let cache = command_context.cache();
    let exe_dir = command_context.exe_dir();
    let cache_needs_update = command_context.cache_needs_update();
    let task_join = command_context.command_runtime.spawn(async move {
        let result = build_favorites(exe_dir, &args.unwrap_or_default(), cache)
            .await
            .unwrap_or_else(|err| {
                error!("{err}");
                false
            });
        if result {
            cache_needs_update.store(true, Ordering::SeqCst);
        }
    });
    CommandHandle::with_handle(task_join)
}

fn open_dir(path: Option<&Path>) -> CommandHandle {
    if let Some(dir) = path {
        if let Err(err) = std::process::Command::new("explorer").arg(dir).spawn() {
            error!("{err}")
        };
    } else {
        error!("Could not find local dir");
    }
    CommandHandle::default()
}
