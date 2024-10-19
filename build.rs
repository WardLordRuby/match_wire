#[cfg(target_os = "windows")]
extern crate winresource;

/// `MAJOR << 48 | MINOR << 32 | PATCH << 16 | RELEASE`
const MAJOR: u64 = 0;
const MINOR: u64 = 5;
const PATCH: u64 = 6;
const RELEASE: u64 = 0;

fn main() {
    if cfg!(target_os = "windows") {
        let mut res = winresource::WindowsResource::new();
        res.set_icon("assets/match_wire.ico");
        let version: u64 = (MAJOR << 48) | (MINOR << 32) | (PATCH << 16) | RELEASE;
        res.set_version_info(winresource::VersionInfo::FILEVERSION, version);
        res.compile().unwrap();
    }
}
