use crossterm::{cursor, event::EventStream, execute, terminal};
use match_wire::{
    await_user_for_end, check_app_dir_exists,
    commands::{
        handler::{CommandContext, GameDetails, Message, ReplHandle, StartupData},
        launch_h2m::launch_h2m_pseudo,
    },
    get_latest_hmw_hash, get_latest_version, print_during_splash, print_help, splash_screen,
    startup_cache_task,
    utils::{
        caching::{read_cache, write_cache},
        display::DisplayPanic,
        subscriber::init_subscriber,
    },
    LOCAL_DATA, LOG_ONLY, SAVED_HISTORY_CAP,
};
use repl_oxide::{
    ansi_code::{RED, RESET},
    executor::Executor,
    general_event_process,
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

    let (mut line_handle, mut command_context, receivers) = match app_startup() {
        Ok(startup_data) => CommandContext::from(startup_data, term).await,
        Err(err) => {
            eprintln!("{RED}{err}{RESET}");
            await_user_for_end();
            return;
        }
    };

    print_help();

    run_eval_print_loop(&mut line_handle, &mut command_context, receivers)
        .await
        .unwrap_or_else(|err| error!("{err}"));

    command_context
        .graceful_shutdown(&line_handle.export_history(Some(SAVED_HISTORY_CAP)))
        .await;
}

async fn run_eval_print_loop(
    line_handle: &mut ReplHandle,
    command_context: &mut CommandContext,
    receivers: (Receiver<Message>, Receiver<()>),
) -> io::Result<()> {
    let (mut message_rx, mut update_cache_rx) = receivers;
    let mut close_listener = tokio::signal::windows::ctrl_close().unwrap();

    let mut reader = EventStream::new();

    loop {
        line_handle.clear_unwanted_inputs(&mut reader).await?;
        line_handle.render(command_context)?;

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
                general_event_process!(line_handle, command_context, event_result)
            }

            Some(msg) = message_rx.recv() => {
                line_handle.println(msg)?
            }

            Some(_) = update_cache_rx.recv() => {
                if let Err(err) = write_cache(command_context, &line_handle.export_history(Some(SAVED_HISTORY_CAP))).await {
                    line_handle.prep_for_background_msg()?;
                    error!("{err}");
                } else {
                    line_handle.set_uneventful();
                };
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

    Ok(StartupData {
        local_dir,
        game,
        cache_task: tokio::spawn(startup_cache_task(cache_res)),
        splash_task,
        launch_task,
        version_task,
        hmw_hash_task,
    })
}
