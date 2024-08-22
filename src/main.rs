use clap::Parser;
use cli::Cli;
use h2m_favorites::*;

#[tokio::main]
async fn main() {
    let exe_dir = std::env::current_dir().expect("Failed to get current dir");

    #[cfg(not(debug_assertions))]
    match does_dir_contain(&exe_dir, Operation::Count, &REQUIRED_FILES)
        .expect("Failed to read contents of current dir")
    {
        OperationResult::Count((count, _)) if count == REQUIRED_FILES.len() => (),
        OperationResult::Count((_, files)) => {
            if !files.contains(REQUIRED_FILES[0]) {
                eprintln!("Move h2m_favorites.exe into your 'Call of Duty Modern Warfare Remastered' directory");
                await_user_for_end();
                return;
            } else if !files.contains(REQUIRED_FILES[1]) {
                eprintln!("H2M mod files not found, h2m_favorites.exe must be placed in 'Call of Duty Modern Warfare Remastered' directory");
                await_user_for_end();
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

    let cli = Cli::parse();
    build_favorites(&exe_dir, cli)
        .await
        .unwrap_or_else(|err| eprintln!("{err:?}"));
    await_user_for_end();
}
