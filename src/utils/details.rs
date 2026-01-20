use crate::{
    commands::{handler::status::ModFileStatus, settings::Settings},
    files::*,
    hash_file_hex,
    models::json_data::{CacheFile, Version},
    utils::display::{self, DISP_NAME_HMW},
};

use std::{
    borrow::Cow,
    path::{Path, PathBuf},
};

#[derive(Debug)]
pub struct GameDetails {
    path: PathBuf,
    game_name: Cow<'static, str>,
    pub version: Option<f64>,
    pub hash_curr: Option<String>,
    pub hash_latest: Option<String>,
    pub(crate) mod_verification: ModFileStatus,
}

impl GameDetails {
    pub fn get(
        cache: Option<&CacheFile>,
        exe_dir: &Path,
        settings: Settings,
    ) -> Result<(Self, bool), Cow<'static, str>> {
        let mod_verification = if cache.is_some_and(|cache| cache.hmw_manifest.verified) {
            ModFileStatus::UpToDate
        } else {
            ModFileStatus::Initial
        };

        let no_launch = {
            #[cfg(not(debug_assertions))]
            {
                use clap::Parser;
                crate::models::cli::Cli::parse().no_launch
            }

            #[cfg(debug_assertions)]
            true
        } || !settings.launch_on_startup;

        #[cfg(not(debug_assertions))]
        {
            let game = match crate::contains_required_files(&exe_dir) {
                Ok(game_exe_path) => {
                    let (version, hash) = crate::exe_details(&game_exe_path);
                    GameDetails::new(game_exe_path, version, hash, mod_verification)
                }
                Err(err) if no_launch => {
                    crate::main_thread_state::alt_screen::push_message(
                        crate::commands::Message::error(err),
                    );
                    GameDetails::default(&exe_dir, mod_verification)
                }
                Err(err) => return Err(err),
            };

            Ok((game, no_launch))
        }

        #[cfg(debug_assertions)]
        Ok((GameDetails::default(exe_dir, mod_verification), no_launch))
    }

    fn default(exe_dir: &Path, mod_verification: ModFileStatus) -> Self {
        GameDetails {
            path: exe_dir.join(FNAME_HMW),
            game_name: Cow::Borrowed(DISP_NAME_HMW),
            version: None,
            hash_curr: None,
            hash_latest: None,
            mod_verification,
        }
    }

    #[cfg(not(debug_assertions))]
    fn new(
        exe_path: PathBuf,
        version: Option<f64>,
        hash_curr: Option<String>,
        mod_verification: ModFileStatus,
    ) -> Self {
        fn set_game_name(exe_path: &Path) -> Cow<'static, str> {
            let file_name = exe_path
                .file_name()
                .expect("path points to exe")
                .to_string_lossy();

            match file_name.as_ref() {
                FNAME_HMW => Cow::Borrowed(DISP_NAME_HMW),
                FNAME_H2M_1 | FNAME_H2M_2 => Cow::Borrowed(crate::utils::display::DISP_NAME_H2M),
                _ => Cow::Owned(file_name.into_owned()),
            }
        }

        GameDetails {
            game_name: set_game_name(&exe_path),
            path: exe_path,
            version,
            hash_curr,
            hash_latest: None,
            mod_verification,
        }
    }

    #[inline]
    pub fn path(&self) -> &Path {
        &self.path
    }

    #[inline]
    pub(crate) fn game_name_owned(&self) -> Cow<'static, str> {
        Cow::clone(&self.game_name)
    }

    #[inline]
    pub(crate) fn game_name(&self) -> &str {
        &self.game_name
    }

    pub(crate) fn get_exe_dir(&self) -> &Path {
        self.path
            .parent()
            .expect("self can not be created without a file added to path")
    }

    pub(crate) fn game_file_name(&self) -> Cow<'_, str> {
        self.path
            .file_name()
            .expect("was not modified since it was set")
            .to_string_lossy()
    }

    pub(crate) fn update(&mut self, from: (Option<f64>, Option<String>)) {
        if from.0.is_some() {
            self.version = from.0;
        }
        if from.1.is_some() {
            self.hash_curr = from.1;
        }
    }
}

#[derive(Default)]
pub struct AppDetails {
    pub hash_curr: Option<String>,
    pub hash_latest: Option<String>,
    pub update_msg: Option<String>,
}

impl AppDetails {
    pub fn from(remote: Option<Version>, exe_path: &Path) -> Self {
        let (hash_latest, update_msg) = remote
            .map(|ver| (Some(ver.latest), Some(ver.message)))
            .unwrap_or_default();

        AppDetails {
            hash_curr: hash_file_hex(exe_path).map_err(display::error).ok(),
            hash_latest,
            update_msg,
        }
    }
}
