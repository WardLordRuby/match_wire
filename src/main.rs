use crossterm::{cursor, event::EventStream, execute, terminal};
use match_wire::{
    CACHED_DATA, LOCAL_DATA, LOG_ONLY, MAIN_PROMPT, await_user_for_end, check_app_dir_exists,
    commands::{
        handler::{CommandContext, GameDetails, Message, StartupData},
        launch_h2m::launch_h2m_pseudo,
    },
    get_latest_hmw_hash, get_latest_version, print_during_splash, print_help, splash_screen,
    utils::{
        caching::{Cache, build_cache, read_cache, write_cache},
        display::DisplayPanic,
        subscriber::init_subscriber,
    },
};
use repl_oxide::{
    EventLoop,
    ansi_code::{RED, RESET},
    executor::{CommandHandle, Executor},
    repl_builder,
};
use std::{borrow::Cow, io, path::PathBuf};
use tokio::sync::mpsc::Receiver;
use tokio_stream::StreamExt;
use tracing::{error, info, instrument, warn};

#[tokio::main(flavor = "current_thread")]
async fn main() {
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

    let (mut command_context, receivers) = match app_startup() {
        Ok(startup_data) => CommandContext::from(startup_data).await,
        Err(err) => {
            eprintln!("{RED}{err}{RESET}");
            await_user_for_end();
            return;
        }
    };

    print_help();

    run_eval_print_loop(&mut command_context, receivers, term)
        .await
        .unwrap_or_else(|err| error!("{err}"));

    command_context.graceful_shutdown().await;
}

async fn run_eval_print_loop(
    command_context: &mut CommandContext,
    receivers: (Receiver<Message>, Receiver<()>),
    term: io::Stdout,
) -> io::Result<()> {
    let (mut message_rx, mut update_cache_rx) = receivers;
    let mut close_listener = tokio::signal::windows::ctrl_close().unwrap();

    let mut reader = EventStream::new();
    let mut line_handle = repl_builder(term)
        .with_prompt(MAIN_PROMPT)
        .with_completion(&match_wire::command_scheme::COMPLETION)
        .with_custom_quit_command("quit")
        .build()
        .expect("input writer accepts crossterm commands");

    loop {
        line_handle.clear_unwanted_inputs(&mut reader).await?;
        line_handle.render()?;

        tokio::select! {
            biased;

            _ = close_listener.recv() => {
                info!(name: LOG_ONLY, "forceful app shutdown");
                // Appon a forceful close the app does not have enough time to aquire the cache lock and properly
                // save the cache to file in the case that the `update_cache` flag is set so we force the process
                // to exit avoiding the possibility of an ill formed cache to be saved locally
                std::process::exit(0)
            }

            Some(event_result) = reader.next() => {
                match line_handle.process_input_event(event_result?)? {
                    EventLoop::Continue => (),
                    EventLoop::Break => break,
                    EventLoop::Callback(callback) => {
                        callback(command_context).expect("`end_forward_logs` does not error")
                    },
                    EventLoop::AsyncCallback(callback) => {
                        if let Err(err) = callback(command_context).await {
                            error!("{err}");
                            if let Some(on_err_callback) = line_handle.conditionally_remove_hook(&err)? {
                                on_err_callback(command_context).expect("`end_forward_logs` does not error");
                            }
                        }
                    },
                    EventLoop::TryProcessInput(Ok(user_tokens)) => {
                        match command_context.try_execute_command(user_tokens).await? {
                            CommandHandle::Processed => (),
                            CommandHandle::InsertHook(input_hook) => line_handle.register_input_hook(input_hook),
                            CommandHandle::Exit => break,
                        }
                    }
                    EventLoop::TryProcessInput(Err(mismatched_quotes)) => {
                        eprintln!("{RED}{mismatched_quotes}{RESET}")
                    },
                }
            }

            Some(msg) = message_rx.recv() => {
                line_handle.print_background_msg(msg)?
            }

            Some(_) = update_cache_rx.recv() => {
                if let Err(err) = write_cache(command_context).await {
                    command_context.send_message(err).await
                };
                line_handle.set_uneventful();
            }
        }
    }
    Ok(())
}

#[instrument(level = "trace", skip_all)]
fn app_startup() -> Result<StartupData, Cow<'static, str>> {
    let exe_dir =
        std::env::current_dir().map_err(|err| format!("Failed to get current dir, {err:?}"))?;

    let no_launch = {
        #[cfg(not(debug_assertions))]
        {
            use clap::Parser;
            match_wire::cli::Cli::parse().no_launch
        }

        #[cfg(debug_assertions)]
        true
    };

    let game = {
        #[cfg(not(debug_assertions))]
        match match_wire::contains_required_files(&exe_dir) {
            Ok(game_exe_path) => {
                let (version, hash) = match_wire::exe_details(&game_exe_path);
                GameDetails::new(game_exe_path, version, hash)
            }
            Err(err) if no_launch => {
                print_during_splash(Message::error(err));
                GameDetails::default(&exe_dir)
            }
            Err(err) => return Err(err.into()),
        }

        #[cfg(debug_assertions)]
        GameDetails::default(&exe_dir)
    };

    let version_task = tokio::spawn(get_latest_version());
    let hmw_hash_task = tokio::spawn(get_latest_hmw_hash());

    let splash_task = tokio::spawn(splash_screen());

    let launch_task = tokio::spawn({
        let game_exe_path = game.path.clone();
        async move {
            if no_launch {
                return None;
            }
            // delay h2m doesn't block splash screen
            tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
            Some(launch_h2m_pseudo(&game_exe_path))
        }
    });

    let mut local_dir = std::env::var_os(LOCAL_DATA).map(PathBuf::from);
    let mut cache_res = None;
    if let Some(ref mut dir) = local_dir {
        if let Err(err) = check_app_dir_exists(dir) {
            print_during_splash(Message::error(err.to_string()));
        } else {
            init_subscriber(dir)
                .unwrap_or_else(|err| print_during_splash(Message::error(err.to_string())));
            info!(name: LOG_ONLY, "App startup");
            cache_res = Some(read_cache(dir));
        }
    } else {
        print_during_splash(Message::error("Could not find %appdata%/local"));

        #[cfg(debug_assertions)]
        init_subscriber(std::path::Path::new("")).unwrap();
    }

    let cache_task = tokio::spawn({
        let cache_path = local_dir.as_deref().map(|local| local.join(CACHED_DATA));
        async move {
            let (connection_history, region_cache) = match cache_res {
                Some(Ok(cache)) => return cache,
                Some(Err(err)) => {
                    error!("{err}");
                    (err.connection_history, err.region_cache)
                }
                None => (None, None),
            };
            let cache_file = build_cache(connection_history, region_cache)
                .await
                .unwrap_or_else(|(err, backup)| {
                    error!("{err}");
                    backup
                });
            if let Some(cache) = cache_path.as_deref() {
                match std::fs::File::create(cache) {
                    Ok(file) => match serde_json::to_writer_pretty(file, &cache_file) {
                        Ok(()) => info!(name: LOG_ONLY, "Cache saved locally"),
                        Err(err) => error!("{err}"),
                    },
                    Err(err) => error!("{err}"),
                }
            }
            Cache::from(cache_file)
        }
    });

    Ok(StartupData {
        local_dir,
        game,
        cache_task,
        splash_task,
        launch_task,
        version_task,
        hmw_hash_task,
    })
}
