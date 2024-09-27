use commands::{
    handler::{listener_routine, try_execute_command, CommandContextBuilder, CommandHandle},
    launch_h2m::launch_h2m_pseudo,
};
use crossterm::{cursor, event::EventStream, execute, terminal};
use match_wire::*;
use std::{
    io,
    path::{Path, PathBuf},
    sync::{atomic::Ordering, LazyLock},
};
use tokio::{sync::mpsc, task::JoinHandle};
use tokio_stream::StreamExt;
use tracing::{error, info, instrument, warn};
use utils::{
    caching::{build_cache, read_cache, write_cache, Cache},
    input::{
        completion::{init_completion, CommandScheme},
        line::{EventLoop, LineReader},
        style::{RED, WHITE},
    },
    subscriber::init_subscriber,
};
use winptyrs::PTY;

static COMPLETION: LazyLock<CommandScheme> = LazyLock::new(init_completion);

fn main() {
    let prev = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        error!(name: "PANIC", "{}", format_panic_info(info));
        prev(info);
    }));

    let mut term = std::io::stdout();

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

    main_runtime.block_on(async {
        let startup_data = match app_startup().await {
            Ok(data) => data,
            Err(err) => {
                eprintln!("{RED}{err}{WHITE}");
                await_user_for_end().await;
                return;
            }
        };

        startup_data.splash_task.await.unwrap().unwrap();

        let launch_res = startup_data.launch_task.await.unwrap_or_else(|err| Err(err.to_string()));

        let (message_tx, mut message_rx) = mpsc::channel(200);

        let mut command_context = CommandContextBuilder::new()
            .cache(startup_data.cache)
            .launch_res(launch_res)
            .exe_dir(startup_data.exe_dir)
            .msg_sender(message_tx)
            .local_dir(startup_data.local_dir)
            .build()
            .unwrap();

        let (update_cache_tx, mut update_cache_rx) = mpsc::channel(20);

        tokio::spawn({
            let cache_needs_update = command_context.cache_needs_update();
            async move {
                loop {
                    if cache_needs_update.compare_exchange(true, false, Ordering::Acquire, Ordering::SeqCst).is_ok()
                        && update_cache_tx.send(true).await.is_err() {
                            break;
                    }
                    tokio::time::sleep(tokio::time::Duration::from_secs(240)).await;
                }
            }
        });

        listener_routine(&mut command_context).await.unwrap_or_else(|err| warn!("{err}"));

        let mut close_listener = tokio::signal::windows::ctrl_close().unwrap();

        print_help();

        execute!(term, cursor::Show).unwrap();

        let mut reader = EventStream::new();
        let mut line_handle = LineReader::new(String::new(), &mut term, &COMPLETION).unwrap();

        terminal::enable_raw_mode().unwrap();

        loop {
            if line_handle.command_entered() {
                line_handle.clear_unwanted_inputs(&mut reader).await.unwrap();
            }
            if !line_handle.uneventful() {
                line_handle.render().unwrap();
            }
            tokio::select! {
                Some(_) = update_cache_rx.recv() => {
                    write_cache(&command_context).await
                        .unwrap_or_else(|err| error!("{err}"));
                }
                Some(msg) = message_rx.recv() => {
                    if let Err(err) = line_handle.print_background_msg(msg) {
                        error!("{err}");
                        break;
                    }
                }
                _ = close_listener.recv() => {
                    info!(name: LOG_ONLY, "app shutdown");
                    terminal::disable_raw_mode().unwrap();
                    return;
                }
                Some(event_result) = reader.next() => {
                    match event_result {
                        Ok(event) => {
                            match line_handle.process_input_event(event) {
                                Ok(EventLoop::Continue) => (),
                                Ok(EventLoop::Break) => break,
                                Ok(EventLoop::Callback(callback)) => callback(&mut command_context),
                                Ok(EventLoop::AsyncCallback(callback)) => {
                                    if let Err(err) = callback(&mut command_context).await {
                                        error!("{err}");
                                        conditionally_remove_hook(&mut command_context, &mut line_handle, err.uid());
                                    }
                                },
                                Ok(EventLoop::TryProcessCommand) => {
                                    let command_handle = match shellwords::split(line_handle.last_line()) {
                                        Ok(user_args) => try_execute_command(user_args, &mut command_context).await,
                                        Err(err) => {
                                            error!("{err}");
                                            continue;
                                        }
                                    };
                                    match command_handle {
                                        CommandHandle::Processed => (),
                                        CommandHandle::Callback((init_callback, input_hook)) => {
                                            if let Err(err) = init_callback(&mut line_handle) {
                                                error!("{err}");
                                                break;
                                            }
                                            line_handle.register_callback(input_hook);
                                        },
                                        CommandHandle::Exit => break,
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
        }
        if command_context.cache_needs_update().load(Ordering::SeqCst) {
            match write_cache(&command_context).await {
                Ok(_) => info!(name: LOG_ONLY, "Cache updated locally"),
                Err(err) => error!(name: LOG_ONLY, "{err}")
            }
        }
        info!(name: LOG_ONLY, "app shutdown");
        terminal::disable_raw_mode().unwrap();
    });
}

struct StartupData {
    cache: Cache,
    exe_dir: PathBuf,
    local_dir: Option<PathBuf>,
    splash_task: JoinHandle<io::Result<()>>,
    launch_task: JoinHandle<Result<(PTY, f64), String>>,
}

#[instrument(level = "trace", skip_all)]
async fn app_startup() -> std::io::Result<StartupData> {
    get_latest_version()
        .await
        .unwrap_or_else(|err| error!("{err}"));

    let exe_dir = std::env::current_dir()
        .map_err(|err| std::io::Error::other(format!("Failed to get current dir, {err:?}")))?;

    #[cfg(not(debug_assertions))]
    contains_required_files(&exe_dir)?;

    let splash_task = tokio::task::spawn(splash_screen());

    let launch_task = tokio::task::spawn({
        let exe_dir = exe_dir.clone();
        async move {
            // delay h2m doesn't block splash screen
            tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
            launch_h2m_pseudo(&exe_dir)
        }
    });

    let mut local_dir = None;
    let mut connection_history = None;
    let mut region_cache = None;
    if let Some(path) = std::env::var_os(LOCAL_DATA) {
        let mut dir = PathBuf::from(path);

        if let Err(err) = check_app_dir_exists(&mut dir) {
            eprintln!("{RED}{err}{WHITE}");
        } else {
            init_subscriber(&dir).unwrap_or_else(|err| eprintln!("{RED}{err}{WHITE}"));
            info!(name: LOG_ONLY, "App startup");
            local_dir = Some(dir);
            match read_cache(local_dir.as_ref().unwrap()).await {
                Ok(cache) => {
                    return Ok(StartupData {
                        cache,
                        exe_dir,
                        local_dir,
                        splash_task,
                        launch_task,
                    })
                }
                Err(err) => {
                    warn!("{err}");
                    connection_history = err.connection_history;
                    region_cache = err.region_cache;
                }
            }
        }
    } else {
        eprintln!("{RED}Could not find %appdata%/local{WHITE}");
        if cfg!(debug_assertions) {
            init_subscriber(Path::new("")).unwrap();
        }
    }
    let cache_file = build_cache(connection_history.as_deref(), region_cache.as_ref())
        .await
        .map_err(std::io::Error::other)?;
    if let Some(ref dir) = local_dir {
        match std::fs::File::create(dir.join(CACHED_DATA)) {
            Ok(file) => {
                if let Err(err) = serde_json::to_writer_pretty(file, &cache_file) {
                    error!("{err}")
                }
                return Ok(StartupData {
                    cache: Cache::from(cache_file),
                    exe_dir,
                    local_dir,
                    splash_task,
                    launch_task,
                });
            }
            Err(err) => error!("{err}"),
        }
    }
    Ok(StartupData {
        cache: Cache::from(cache_file),
        exe_dir,
        local_dir,
        splash_task,
        launch_task,
    })
}
