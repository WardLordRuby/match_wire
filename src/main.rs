use match_wire::{
    CRATE_NAME, LOG_ONLY, SAVED_HISTORY_CAP, await_user_for_end,
    commands::{
        handler::{
            AppDetails, CommandContext, GameDetails, HistoryTag, Message, ReplHandle, StartupData,
        },
        settings::Settings,
    },
    get_latest_hmw_manifest,
    models::json_data::Version,
    print_help, splash_screen, startup_cache_task, try_init_launch, try_init_logger,
    utils::{
        caching::read_cache,
        display::{self, DisplayPanic},
        main_thread_state,
    },
};

use std::{borrow::Cow, io, path::PathBuf};

use crossterm::{cursor, event::EventStream, execute, terminal::SetTitle};
use repl_oxide::{executor::Executor, general_event_process};
use tokio::{
    sync::mpsc::Receiver,
    time::{Duration, interval},
};
use tokio_stream::StreamExt;
use tracing::{error, info, instrument};

#[tokio::main(flavor = "current_thread")]
async fn main() {
    let prev = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        error!(name: "PANIC", "{}", DisplayPanic(info));
        prev(info);
    }));

    let mut term = std::io::stdout();
    execute!(term, cursor::Hide, SetTitle(CRATE_NAME)).unwrap();

    let splash_task = splash_screen::enter();

    let appdata_local = try_init_logger();
    let latest_version_data = main_thread_state::Endpoints::init().await;

    let (mut line_handle, mut command_context, message_rx) =
        match app_startup(appdata_local, latest_version_data) {
            Ok(startup_data) => CommandContext::from(startup_data, splash_task, term).await,
            Err(err) => {
                await_user_for_end(err);
                return;
            }
        };

    print_help();

    run_eval_print_loop(&mut line_handle, &mut command_context, message_rx)
        .await
        .unwrap_or_else(display::error);

    command_context.graceful_shutdown(
        &line_handle.export_filtered_history(HistoryTag::filter, Some(SAVED_HISTORY_CAP)),
    )
}

async fn run_eval_print_loop(
    line_handle: &mut ReplHandle,
    command_context: &mut CommandContext,
    mut message_rx: Receiver<Message>,
) -> io::Result<()> {
    let mut save_cache_interval = interval(Duration::from_secs(240));
    let mut close_listener = tokio::signal::windows::ctrl_close().unwrap();
    let mut reader = EventStream::new();

    loop {
        line_handle.clear_unwanted_inputs(&mut reader).await?;
        line_handle.render(command_context)?;

        tokio::select! {
            biased;

            _ = close_listener.recv() => {
                info!(name: LOG_ONLY, "forceful app shutdown");
                // Upon a forceful close the app does not have enough time to acquire the cache lock and properly
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

            _ = save_cache_interval.tick() => {
                if let Err(err) = command_context.save_cache_if_needed(line_handle) {
                    line_handle.prep_for_background_msg()?;
                    error!("Could not save cache: {err}");
                } else {
                    line_handle.set_uneventful();
                }
            }

            _ = command_context.game_state_change.notified() => {
                if let Err(err) = command_context.handle_game_state_change(line_handle)? {
                    line_handle.prep_for_background_msg()?;
                    error!("{err}");
                }
            }
        }
    }
    Ok(())
}

#[instrument(level = "trace", skip_all)]
fn app_startup(
    local_dir: Option<PathBuf>,
    version_data: Option<Version>,
) -> Result<StartupData, Cow<'static, str>> {
    let exe_path = main_thread_state::ExePath::get()
        .map_err(|err| format!("Failed to get exe directory, {err:?}"))?;

    let appdata = AppDetails::from(version_data, exe_path);

    let exe_dir = exe_path
        .parent()
        .ok_or("Failed to get exe parent directory")?;

    let cache_res = local_dir.as_deref().map(read_cache);
    let settings = Settings::init(local_dir.as_deref());
    let (game, no_launch) = GameDetails::get(
        cache_res.as_ref().and_then(|res| match res {
            Ok(cache) => Some(cache),
            Err(backup) => backup.cache.as_ref(),
        }),
        exe_dir,
        settings,
    )?;

    let launch_task = tokio::spawn(try_init_launch(game.path.clone(), no_launch));

    Ok(StartupData {
        local_dir,
        game,
        appdata,
        settings,
        cache_task: tokio::spawn(startup_cache_task(cache_res)),
        launch_task,
        hmw_manifest_task: tokio::spawn(get_latest_hmw_manifest()),
    })
}
