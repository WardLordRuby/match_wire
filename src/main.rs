use clap::{CommandFactory, Parser};
use cli::{Cli, UserCommand};
use commands::{
    handler::{try_execute_command, CommandContext},
    reconnect::try_locate_h2m_console,
};
use crossterm::{cursor, event::EventStream, execute, terminal};
use h2m_favorites::*;
use std::{
    io::ErrorKind,
    path::{Path, PathBuf},
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
};
use tokio::sync::{mpsc, Mutex};
use tokio_stream::StreamExt;
use tracing::{error, info, instrument};
use utils::{
    caching::{build_cache, read_cache, update_cache, Cache},
    input_line::*,
    json_data::CacheFile,
    subscriber::init_subscriber,
};

fn main() {
    let prev = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        error!(name: "PANIC", "{}", format_panic_info(info));
        prev(info);
    }));

    let mut term = std::io::stdout();
    let term_size = terminal::size().unwrap();
    execute!(
        term,
        cursor::Hide,
        terminal::SetTitle(env!("CARGO_PKG_NAME")),
    )
    .unwrap();

    let main_runtime = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .expect("Failed to create single-threaded runtime");

    let cli = Cli::parse();
    let command_runtime = if cli.single_thread {
        new_io_error!(
            ErrorKind::PermissionDenied,
            "User chose to run on a single thread"
        )
    } else {
        tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .build()
    };

    let command_handle = command_runtime
        .as_ref()
        .map(|rt| rt.handle())
        .unwrap_or_else(|err| {
            if err.kind() != ErrorKind::PermissionDenied {
                error!("{err}");
            }
            main_runtime.handle()
        });

    main_runtime.block_on(async {
        let app_startup = command_handle.spawn(async move {
            app_startup().await
        });

        get_latest_version()
            .await
            .unwrap_or_else(|err| error!("{err}"));

        let (update_cache_tx, mut update_cache_rx) = mpsc::channel(20);
        let cache_needs_update_arc = Arc::new(AtomicBool::new(false));

        tokio::spawn({
            let cache_needs_update = cache_needs_update_arc.clone();
            async move {
                loop {
                    if cache_needs_update.compare_exchange(true, false, Ordering::SeqCst, Ordering::SeqCst).is_ok()
                        && update_cache_tx.send(true).await.is_err() {
                            break;
                    }
                    tokio::time::sleep(tokio::time::Duration::from_secs(240)).await;
                }
            }
        });

        let (cache, local_env_dir, exe_dir) = match app_startup.await {
            Ok(startup_result) => match startup_result {
                Ok(data) => data,
                Err(err) => {
                    eprintln!("{err}");
                    await_user_for_end().await;
                    return;
                }
            },
            Err(err) => {
                error!("{err}");
                await_user_for_end().await;
                return
            }
        };

        let exe_dir_arc = Arc::new(exe_dir);
        let local_env_dir_arc = Arc::new(local_env_dir);
        let cache_arc = Arc::new(Mutex::new(cache));

        let command_context = CommandContext {
            cache: &cache_arc,
            exe_dir: &exe_dir_arc,
            local_dir: &local_env_dir_arc,
            cache_needs_update: &cache_needs_update_arc,
            command_runtime: command_handle,
        };

        let h2m_handle = try_locate_h2m_console();
        match h2m_handle {
            Ok((h2m_stdin_handle, h2m_stdout_handle)) => {
                info!("Found h2m stdin handle: {h2m_stdin_handle:?}, found h2m stdout handle: {h2m_stdout_handle:?}");
            }
            Err(err) => error!("{err}"), 
        }

        let mut close_listener = tokio::signal::windows::ctrl_close().unwrap();

        UserCommand::command().print_help().expect("Failed to print help");

        execute!(term, cursor::Show).unwrap();

        let mut reader = EventStream::new();
        let mut line_handle = LineReader::new("h2m_favorites.exe> ", &mut term, term_size).unwrap();

        terminal::enable_raw_mode().unwrap();

        let mut command_processed = false;

        loop {
            if command_processed {
                line_handle.clear_unwanted_inputs(&mut reader).await.unwrap();
                command_processed = false;
            }
            line_handle.render().unwrap();
            let mut processing_taks = Vec::new();
            let event = reader.next();
            tokio::select! {
                Some(_) = update_cache_rx.recv() => {
                    update_cache(cache_arc.clone(), local_env_dir_arc.clone()).await.unwrap_or_else(|err| error!("{err}"));
                    continue;
                }
                _ = close_listener.recv() => {
                    terminal::disable_raw_mode().unwrap();
                    return;
                }

                Some(event_result) = event => {
                    match event_result {
                        Ok(event) => {
                            match line_handle.process_input_event(event) {
                                Ok(EventLoop::Continue) => continue,
                                Ok(EventLoop::Break) => break,
                                Ok(EventLoop::TryProcessCommand) => {
                                    command_processed = true;
                                    let command_handle = match shellwords::split(line_handle.history.last()) {
                                        Ok(user_args) => try_execute_command(user_args, &command_context),
                                        Err(err) => {
                                            error!("{err}");
                                            continue;
                                        }
                                    };
                                    if command_handle.exit {
                                        break;
                                    }
                                    if let Some(join_handle) = command_handle.handle {
                                        processing_taks.push(join_handle);
                                    }
                                }
                                Err(err) => {
                                    error!("{err}");
                                    break;
                                }
                            }
                        }
                        Err(err) => {
                            error!("{err}");
                            break;
                        },
                    }
                }
            }
            for task in processing_taks {
                if let Err(err) = task.await {
                    error!("{err}");
                }
            }
        }

        if cache_needs_update_arc.load(Ordering::SeqCst) {
            update_cache(cache_arc, local_env_dir_arc).await.unwrap_or_else(|err| error!("{err}"));
        }
        // MARK: TODO
        // need to make sure we do all winapi cleanup
        terminal::disable_raw_mode().unwrap();
    });
}

#[instrument(skip_all)]
async fn app_startup() -> std::io::Result<(Cache, Option<PathBuf>, PathBuf)> {
    let exe_dir = std::env::current_dir()
        .map_err(|err| std::io::Error::other(format!("Failed to get current dir, {err:?}")))?;

    #[cfg(not(debug_assertions))]
    match does_dir_contain(&exe_dir, Operation::Count, &REQUIRED_FILES)
        .expect("Failed to read contents of current dir")
    {
        OperationResult::Count((count, _)) if count == REQUIRED_FILES.len() => (),
        OperationResult::Count((_, files)) => {
            if !files.contains(REQUIRED_FILES[0]) {
                return new_io_error!(
                    ErrorKind::Other,
                    "Move h2m_favorites.exe into your 'Call of Duty Modern Warfare Remastered' directory"
                );
            } else if !files.contains(REQUIRED_FILES[1]) {
                return new_io_error!(
                    ErrorKind::Other,
                    "H2M mod files not found, h2m_favorites.exe must be placed in 'Call of Duty Modern Warfare Remastered' directory"
                );
            }
            if !files.contains(REQUIRED_FILES[2]) {
                std::fs::create_dir(exe_dir.join(REQUIRED_FILES[2]))
                    .expect("Failed to create players2 folder");
                println!("players2 folder is missing, a new one was created");
            }
        }
        _ => unreachable!(),
    }

    let mut local_env_dir = None;
    if let Some(path) = std::env::var_os(LOCAL_DATA) {
        let mut dir = PathBuf::from(path);

        if let Err(err) = check_app_dir_exists(&mut dir) {
            error!(name: LOG_ONLY, "{err:?}");
        } else {
            init_subscriber(&dir).unwrap_or_else(|err| eprintln!("{err}"));
            info!(name: LOG_ONLY, "App startup");
            local_env_dir = Some(dir);
            match read_cache(local_env_dir.as_ref().unwrap()) {
                Ok(cache) => return Ok((cache, local_env_dir, exe_dir)),
                Err(err) => info!("{err}"),
            }
        }
    } else {
        error!(name: LOG_ONLY, "Could not find %appdata%/local");
        if cfg!(debug_assertions) {
            init_subscriber(Path::new("")).unwrap();
        }
    }
    let server_cache = build_cache().await.map_err(std::io::Error::other)?;
    if let Some(ref dir) = local_env_dir {
        match std::fs::File::create(dir.join(CACHED_DATA)) {
            Ok(file) => {
                let data = CacheFile {
                    version: env!("CARGO_PKG_VERSION").to_string(),
                    created: std::time::SystemTime::now(),
                    cache: server_cache,
                };
                if let Err(err) = serde_json::to_writer_pretty(file, &data) {
                    error!("{err}")
                }
                return Ok((
                    Cache::from(data.cache, data.created),
                    local_env_dir,
                    exe_dir,
                ));
            }
            Err(err) => error!("{err}"),
        }
    }
    Ok((
        Cache::from(server_cache, std::time::SystemTime::now()),
        local_env_dir,
        exe_dir,
    ))
}
