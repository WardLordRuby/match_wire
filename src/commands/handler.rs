use crate::{
    cli::{Command, Filters, UserCommand},
    commands::{filter::build_favorites, reconnect::reconnect},
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

pub struct CommandContext<'a> {
    pub cache: &'a Arc<Mutex<Cache>>,
    pub exe_dir: &'a Arc<PathBuf>,
    pub local_dir: &'a Arc<Option<PathBuf>>,
    pub cache_needs_update: &'a Arc<AtomicBool>,
    pub command_runtime: &'a runtime::Handle,
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
    command_context: &CommandContext,
) -> CommandHandle {
    let mut input_tokens = vec![String::new()];
    input_tokens.append(&mut user_args);
    match UserCommand::try_parse_from(input_tokens) {
        Ok(cli) => match cli.command {
            Command::Filter { args } => new_favorites_with(args, command_context),
            Command::Reconnect { history } => reconnect(history),
            Command::GameDir => open_dir(Some(command_context.exe_dir.as_path())),
            Command::LocalEnv => open_dir(command_context.local_dir.as_ref().as_ref()),
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
    let cache = Arc::clone(command_context.cache);
    let exe_dir = Arc::clone(command_context.exe_dir);
    let cache_needs_update = Arc::clone(command_context.cache_needs_update);
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

fn open_dir<P: AsRef<Path>>(path: Option<P>) -> CommandHandle {
    if let Some(dir) = path {
        if let Err(err) = std::process::Command::new("explorer")
            .arg(dir.as_ref())
            .spawn()
        {
            error!("{err}")
        };
    } else {
        error!("Could not find local dir");
    }
    CommandHandle::default()
}
