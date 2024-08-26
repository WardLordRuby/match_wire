use clap::{Parser, CommandFactory};
use cli::{Cli, Command};
use commands::filter::{build_favorites, MASTER_URL};
use h2m_favorites::*;
use std::{
    path::{Path, PathBuf},
    sync::{atomic::{AtomicBool, Ordering}, Arc},
};
use tokio::{
    io::{AsyncBufReadExt, AsyncWriteExt, BufReader, BufWriter},
    signal,
    sync::Mutex,
};
use tracing::{error, info, instrument};
use utils::{
    caching::{build_cache, read_cache, update_cache, Cache},
    json_data::CacheFile,
    subscriber::init_subscriber,
};

fn main() {
    let prev = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        error!(name: "PANIC", "{}", format_panic_info(info));
        prev(info);
    }));

    let main_runtime = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .expect("Failed to create single-threaded runtime");

    let command_runtime = tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .expect("Failed to create multi-threaded runtime");

    main_runtime.block_on(async {
        let exe_dir = match std::env::current_dir() {
            Ok(dir) => dir,
            Err(err) => {
                eprintln!("Failed to get current dir, {err:?}");
                await_user_for_end().await;
                return;
            }
        };

        #[cfg(not(debug_assertions))]
        match does_dir_contain(&exe_dir, Operation::Count, &REQUIRED_FILES)
            .expect("Failed to read contents of current dir")
        {
            OperationResult::Count((count, _)) if count == REQUIRED_FILES.len() => (),
            OperationResult::Count((_, files)) => {
                if !files.contains(REQUIRED_FILES[0]) {
                    eprintln!("Move h2m_favorites.exe into your 'Call of Duty Modern Warfare Remastered' directory");
                    await_user_for_end().await;
                    return;
                } else if !files.contains(REQUIRED_FILES[1]) {
                    eprintln!("H2M mod files not found, h2m_favorites.exe must be placed in 'Call of Duty Modern Warfare Remastered' directory");
                    await_user_for_end().await;
                    return;
                }
                if !files.contains(REQUIRED_FILES[2]) {
                    std::fs::create_dir(exe_dir.join(REQUIRED_FILES[2]))
                        .expect("Failed to create players2 folder");
                    println!("players2 folder is missing, a new one was created");
                }
            }
            _ => unreachable!(),
        }

        let arc_exe_dir = Arc::new(exe_dir);
    
        let app_startup = command_runtime.spawn(async move {
            app_startup().await
        });

        // CACHE.set(RwLock::new(cache)).unwrap();
        // LOCAL_ENV_DIR.set(local_env_dir).unwrap();
    
        get_latest_version()
            .await
            .unwrap_or_else(|err| error!("{err}"));

        let mut ctrl_close_task = signal::windows::ctrl_close().expect("Failed to listen to close");
    
        let local = tokio::task::LocalSet::new();
    
        local.spawn_local(async {});
    
        let mut stdin = tokio::io::BufReader::new(tokio::io::stdin()).lines();
        let mut stdout = BufWriter::new(tokio::io::stdout());
        let mut shutdown_signal = false;
        let cache_needs_update_arc = Arc::new(AtomicBool::new(false));

        let (cache, local_env_dir) = match app_startup.await {
            Ok(cache) => match cache {
                Ok(data) => data,
                Err(err) => {
                    eprintln!("Failed to reach server host {MASTER_URL}, {err:?}");
                    await_user_for_end().await;
                    return;
                }
            },
            Err(err) => {
                error!("{err}");
                return
            }
        };

        let cache = Arc::new(Mutex::new(cache));
        let local_env_dir = Arc::new(local_env_dir);

        Cli::command().print_help().expect("Failed to print help");

        loop {
            let mut processing_taks = Vec::new();
            print_stdin_ready(&mut stdout).await.unwrap();
            tokio::select! {
                _ = ctrl_close_task.recv() => {
                    // MARK: DEBUG
                    // we get the signal but we don't actually save any data just blank file
                    shutdown_signal = true;
                    break;
                }
                _ = signal::ctrl_c() => {
                    shutdown_signal = true;
                    break;
                }
    
                result = stdin.next_line() => {
                    match result {
                        Ok(None) => break,
                        Ok(Some(line)) if line.is_empty() => continue,
                        Ok(Some(line)) => {
                            // MARK: TODO

                            // clean up this mess
                            //          +
                            // need to lock input while commands are being processed
                            let mut args = vec![String::new()];
                            let mut user_args = match shellwords::split(&line) {
                                Ok(val) => val,
                                Err(err) => {
                                    eprintln!("{err}");
                                    error!("{err}");
                                    continue;
                                }
                            };
                            args.append(&mut user_args);
                            match Cli::try_parse_from(args) {
                                Ok(cli) => match cli.command {
                                    Command::Filter { args } => {
                                        let cache = cache.clone();
                                        let exe_dir = arc_exe_dir.clone();
                                        let cache_needs_update = cache_needs_update_arc.clone();
                                        processing_taks.push(command_runtime.spawn(async move {
                                            let result = build_favorites(exe_dir, &args.unwrap_or_default(), &mut *cache.lock().await)
                                                .await
                                                .unwrap_or_else(|err| {
                                                    error!("{err:?}");
                                                    eprintln!("{err:?}");
                                                    false
                                                });
                                            if result {
                                                cache_needs_update.store(true, Ordering::SeqCst);
                                            }
                                        }));
                                    }
                                    Command::Reconnect {
                                        history: show_history,
                                    } => {
                                        if show_history {
                                            todo!()
                                        } else {
                                            todo!()
                                        }
                                    }
                                    Command::LocalEnv => todo!(),
                                    Command::Quit => break,
                                },
                                Err(err) => if let Err(err) = err.print() {
                                    error!("{err}");
                                },
                            }
                        }
                        Err(err) => {
                            error!("{err}");
                            break
                        }
                    }
                }
            }
            for task in processing_taks {
                if let Err(err) = task.await {
                    // MARK: TODO
                    // we really need to have tracing error print to terminal as well
                    eprintln!("{err}");
                    error!("{err}");
                }
            }
        }
        if cache_needs_update_arc.load(Ordering::SeqCst) {
            if let Some(ref dir) = *local_env_dir {
                update_cache(std::mem::take(&mut *cache.lock().await), dir).unwrap_or_else(|err| error!("{err}"));
            }
        }
        if shutdown_signal {
            std::process::exit(0);
        }
    });

}

async fn print_stdin_ready(buf_writer: &mut BufWriter<tokio::io::Stdout>) -> std::io::Result<()> {
    buf_writer.write_all(b"h2m_favorites.exe > ").await?;
    buf_writer.flush().await
}

pub async fn await_user_for_end() {
    println!("Press enter to exit...");
    let stdin = tokio::io::stdin();
    let mut reader = BufReader::new(stdin);
    let _ = reader.read_line(&mut String::new()).await;
}

#[instrument(skip_all)]
async fn app_startup() -> std::io::Result<(Cache, Option<PathBuf>)> {
    let mut local_env_dir = None;
    if let Some(path) = std::env::var_os(LOCAL_DATA) {
        let mut dir = PathBuf::from(path);

        if let Err(err) = check_app_dir_exists(&mut dir) {
            error!("{err:?}");
        } else {
            init_subscriber(&dir).unwrap_or_else(|err| eprintln!("{err}"));
            local_env_dir = Some(dir);
            match read_cache(local_env_dir.as_ref().unwrap()) {
                Ok(cache) => return Ok((cache, local_env_dir)),
                Err(err) => info!("{err}"),
            }
        }
    } else {
        error!("Could not find %appdata%/local");
        if cfg!(debug_assertions) {
            init_subscriber(Path::new("")).unwrap();
        }
    }
    let server_cache = build_cache().await.map_err(std::io::Error::other)?;
    if let Some(ref dir) = local_env_dir {
        // MARK: TODO
        // after testing the save on exit no point to save on creation and most likely on exit as well
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
                return Ok((Cache::from(data.cache, data.created), local_env_dir));
            }
            Err(err) => error!("{err}"),
        }
    }
    Ok((
        Cache::from(server_cache, std::time::SystemTime::now()),
        local_env_dir,
    ))
}

// #[inline]
// fn get_env_dir() -> Option<&'static PathBuf> {
//     LOCAL_ENV_DIR.get_or_init(|| None).as_ref()
// }

// #[inline]
// async fn get_mut_cache() -> tokio::sync::RwLockWriteGuard<'static, Cache> {
//     CACHE.get_or_init(|| RwLock::new(Cache::default())).write().await
// }

// #[inline]
// async fn take_cache() -> Cache {
//     std::mem::take(&mut *CACHE.get_or_init(|| RwLock::new(Cache::default())).write().await)
// }
