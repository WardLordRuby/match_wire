use crossterm::{cursor, event::EventStream, execute, terminal};
use match_wire::{
    await_user_for_end, break_if, check_app_dir_exists,
    commands::{
        handler::{
            listener_routine, try_execute_command, AppDetails, CommandContextBuilder,
            CommandHandle, GameDetails,
        },
        launch_h2m::{launch_h2m_pseudo, LaunchError},
    },
    get_latest_hmw_hash, get_latest_version, print_help, splash_screen,
    utils::{
        caching::{build_cache, read_cache, write_cache, Cache},
        display::DisplayPanic,
        input::{
            completion::CommandScheme,
            line::{EventLoop, LineReader},
            style::{RED, WHITE},
        },
        subscriber::init_subscriber,
    },
    CACHED_DATA, LOCAL_DATA, LOG_ONLY,
};
use std::{borrow::Cow, io, path::PathBuf};
use tokio::task::JoinHandle;
use tokio_stream::StreamExt;
use tracing::{error, info, instrument, warn};
use winptyrs::PTY;

const COMPLETION: CommandScheme = CommandScheme::init();

fn main() {
    let prev = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        error!(name: "PANIC", "{}", DisplayPanic(info));
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
                await_user_for_end();
                return;
            }
        };
        
        let (splash_res, launch_res, app_ver_res, hmw_hash_res) = tokio::join!(
            startup_data.splash_task,
            startup_data.launch_task,
            startup_data.version_task,
            startup_data.hmw_hash_task
        );

        splash_res.unwrap().unwrap();
        
        let try_start_listener = matches!(launch_res, Ok(Ok(_)));

        let (mut command_context, mut message_rx, mut update_cache_rx) = CommandContextBuilder::new()
            .cache(startup_data.cache)
            .launch_res(launch_res)
            .app_ver_res(app_ver_res)
            .hmw_hash_res(hmw_hash_res)
            .game_details(startup_data.game)
            .local_dir(startup_data.local_dir)
            .build()
            .expect("all required inputs are provided");

        if try_start_listener {
            listener_routine(&mut command_context).await.unwrap_or_else(|err| warn!("{err}"));
        }

        let mut close_listener = tokio::signal::windows::ctrl_close().unwrap();

        print_help();

        execute!(term, cursor::Show).unwrap();

        let mut reader = EventStream::new();
        let mut line_handle = LineReader::new(String::new(), &mut term, &COMPLETION).unwrap();

        terminal::enable_raw_mode().unwrap();

        loop {
            if line_handle.command_entered() {
                break_if!(line_handle.clear_unwanted_inputs(&mut reader).await, is_err);
            }
            if !line_handle.uneventful() {
                break_if!(line_handle.try_init_input_hook(), is_some_err);
                break_if!(line_handle.render(), is_err);
            }
            tokio::select! {
                biased;

                _ = close_listener.recv() => {
                    info!(name: LOG_ONLY, "forceful app shutdown");
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
                                        line_handle.conditionally_remove_hook(&mut command_context, err.uid());
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
                                        CommandHandle::InsertHook(input_hook) => line_handle.register_input_hook(input_hook),
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

                Some(msg) = message_rx.recv() => {
                    break_if!(line_handle.print_background_msg(msg), is_err)
                }

                Some(_) = update_cache_rx.recv() => {
                    write_cache(&command_context).await
                        .unwrap_or_else(|err| error!("{err}"));
                }
            }
        }
        command_context.graceful_shutdown().await;
    });
}

struct StartupData {
    cache: Cache,
    local_dir: Option<PathBuf>,
    game: GameDetails,
    splash_task: JoinHandle<io::Result<()>>,
    launch_task: JoinHandle<Result<PTY, LaunchError>>,
    version_task: JoinHandle<reqwest::Result<AppDetails>>,
    hmw_hash_task: JoinHandle<reqwest::Result<Option<String>>>,
}

#[instrument(level = "trace", skip_all)]
async fn app_startup() -> Result<StartupData, Cow<'static, str>> {
    let exe_dir =
        std::env::current_dir().map_err(|err| format!("Failed to get current dir, {err:?}"))?;

    #[cfg(not(debug_assertions))]
    let game = {
        let game_exe_path = match_wire::contains_required_files(&exe_dir)?;
        let (version, hash) = match_wire::exe_details(&game_exe_path);
        GameDetails::new(game_exe_path, version, hash)
    };

    #[cfg(debug_assertions)]
    let game = GameDetails::default(&exe_dir);

    let version_task = tokio::spawn(get_latest_version());
    let hmw_hash_task = tokio::spawn(get_latest_hmw_hash());

    let splash_task = tokio::spawn(splash_screen());

    let launch_task = tokio::spawn({
        let game_exe_path = game.path.clone();
        async move {
            // delay h2m doesn't block splash screen
            tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
            launch_h2m_pseudo(&game_exe_path)
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
                        local_dir,
                        game,
                        splash_task,
                        launch_task,
                        version_task,
                        hmw_hash_task,
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

        #[cfg(debug_assertions)]
        init_subscriber(std::path::Path::new("")).unwrap();
    }

    let cache_file = build_cache(connection_history.as_deref(), region_cache.as_ref())
        .await
        .unwrap_or_else(|(err, backup)| {
            error!("{err}");
            backup
        });

    if let Some(ref dir) = local_dir {
        match std::fs::File::create(dir.join(CACHED_DATA)) {
            Ok(file) => {
                if let Err(err) = serde_json::to_writer_pretty(file, &cache_file) {
                    error!("{err}")
                }
            }
            Err(err) => error!("{err}"),
        }
    }
    Ok(StartupData {
        cache: Cache::from(cache_file),
        local_dir,
        game,
        splash_task,
        launch_task,
        version_task,
        hmw_hash_task,
    })
}
