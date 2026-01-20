use crate::{
    LOG_ONLY, MOD_FILES_MODULE_NAME,
    files::FNAME_HMW,
    hash_file_hex,
    models::json_data::{CondManifest, HmwManifest},
    utils::{
        details::GameDetails,
        display::indicator::ProgressBar,
        main_thread_state::{self, Cache},
    },
};

use std::{io::ErrorKind, num::NonZero};

use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use tracing::{error, info};

#[derive(Debug)]
#[repr(u8)]
pub(crate) enum ModFileStatus {
    Initial,
    MissingFiles(Vec<String>),
    VerifyReady,
    Outdated(Vec<String>),
    UpToDate,
}

impl ModFileStatus {
    fn discriminant(&self) -> u8 {
        // SAFETY: Because `Self` is marked `repr(u8)`, its layout is a `repr(C)` `union`
        // between `repr(C)` structs, each of which has the `u8` discriminant as its first
        // field, so we can read the discriminant without offsetting the pointer.
        unsafe { *<*const _>::from(self).cast::<u8>() }
    }
}

impl Ord for ModFileStatus {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.discriminant().cmp(&other.discriminant())
    }
}

impl PartialOrd for ModFileStatus {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for ModFileStatus {
    fn eq(&self, other: &Self) -> bool {
        self.discriminant() == other.discriminant()
    }
}

impl Eq for ModFileStatus {}

impl GameDetails {
    /// Finds if the [`CondManifest`] stored in [`main_thread_state::Cache`] has been verified genuine
    pub(crate) fn manifest_verified(&self) -> bool {
        self.hash_latest.is_some()
    }

    /// Returns the guid of the previous manifest if a newer manifest was available
    pub(in super::super) fn conditional_condense_manifest(
        &mut self,
        mut man: HmwManifest,
    ) -> Option<String> {
        const NULL_256_HASH: &str =
            "0000000000000000000000000000000000000000000000000000000000000000";

        fn try_get_exe_hash(cache: &Cache) -> Option<String> {
            cache.hmw_manifest.files_with_hashes.get(FNAME_HMW).cloned()
        }

        main_thread_state::Cache::with_borrow_mut(|cache| {
            if man.modules.is_empty() {
                error!("HMW manifest formatting has changed, failed to verify version");
                self.hash_latest = None;
                return None;
            }

            if cache.hmw_manifest.guid == man.manifest_guid {
                self.hash_latest = try_get_exe_hash(cache);
                return None;
            }

            self.mod_verification = ModFileStatus::Initial;

            man.modules.retain(|m| m.name == MOD_FILES_MODULE_NAME);
            man.modules.sort_unstable_by_key(|m| m.version.clone());

            let mut files_with_hashes = std::mem::take(&mut man.modules[0].files_with_hashes);

            man.modules
                .into_iter()
                .skip(1)
                .for_each(|m| files_with_hashes.extend(m.files_with_hashes));

            files_with_hashes.retain(|_, hash| !hash.is_empty() && hash != NULL_256_HASH);

            let prev = std::mem::take(&mut cache.hmw_manifest.guid);

            cache.hmw_manifest = CondManifest {
                guid: man.manifest_guid,
                files_with_hashes,
                verified: false,
            };

            self.hash_latest = try_get_exe_hash(cache);
            Some(prev)
        })
    }

    pub(in super::super) fn prep_verification_state(&mut self) -> Result<(), ()> {
        let exe_dir = self.get_exe_dir();

        let missing = main_thread_state::Cache::with_borrow(|cache| {
            let mut missing = Vec::new();

            for mod_file in cache.hmw_manifest.files_with_hashes.keys() {
                let file_path = exe_dir.join(mod_file);

                match file_path.try_exists() {
                    Ok(true) => continue,
                    Ok(false) => (),
                    Err(err) => error!(name: LOG_ONLY, "{err}, path: {}", file_path.display()),
                }

                missing.push(mod_file.clone())
            }

            missing
        });

        if !missing.is_empty() {
            error!(name: LOG_ONLY, "Missing HMW files: {}", missing.join(", "));
            self.mod_verification = ModFileStatus::MissingFiles(missing);
            return Err(());
        }

        if self.mod_verification < ModFileStatus::VerifyReady {
            self.mod_verification = ModFileStatus::VerifyReady;
        }
        Ok(())
    }

    pub(super) fn verify_hmw_files(&mut self) -> Result<(), ()> {
        if self.mod_verification < ModFileStatus::VerifyReady
            && self.prep_verification_state().is_err()
        {
            return Ok(());
        }

        let exe_dir = self.get_exe_dir();
        let start = std::time::Instant::now();

        let outdated = main_thread_state::Cache::with_borrow(|cache| {
            if cache.hmw_manifest.files_with_hashes.is_empty() {
                error!("No manifest data available to verify files");
                return Vec::new();
            }

            let file_ct =
                NonZero::new(cache.hmw_manifest.files_with_hashes.len()).expect("early return");

            let progress = ProgressBar::new("Verifying", "Files", file_ct);

            let outdated = cache
                .hmw_manifest
                .files_with_hashes
                .par_iter()
                .filter_map(|(file, expected)| {
                    let hash = hash_file_hex(&exe_dir.join(file)).unwrap_or_else(|err| {
                        if err.kind() == ErrorKind::NotFound {
                            error!("file: {file}, not found",);
                        } else {
                            error!("file: {file}, {err}",);
                        }
                        String::new()
                    });
                    progress.tick();
                    (hash != *expected).then(|| file.clone())
                })
                .collect::<Vec<_>>();

            progress.finish();
            outdated
        });

        if !outdated.is_empty() {
            error!(name: LOG_ONLY, "Outdated HMW files: {}", outdated.join(", "));
            self.mod_verification = ModFileStatus::Outdated(outdated);
            return Err(());
        }

        self.mod_verification = ModFileStatus::UpToDate;
        info!(name: LOG_ONLY, "Verified files in {:#?}", start.elapsed());

        Ok(())
    }
}
