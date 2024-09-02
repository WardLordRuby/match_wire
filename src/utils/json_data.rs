use serde::{Deserialize, Serialize};

use crate::commands::launch_h2m::HostName;

#[derive(Deserialize, Debug)]
pub struct HostData {
    pub servers: Vec<ServerInfo>,
    pub uptime: u32,
    pub id: String,
    pub last_heartbeat: u64,
    pub ip_address: String,
    pub webfront_url: String,
    pub version: String,
}

#[derive(Deserialize, Debug)]
pub struct ServerInfo {
    pub ip: String,
    pub clientnum: u8,
    pub gametype: String,
    pub id: i64,
    pub maxclientnum: u8,
    pub port: u32,
    pub map: String,
    pub version: String,
    pub game: String,
    pub hostname: String,
}

#[derive(Deserialize, Debug)]
pub struct ServerLocation {
    pub continent: Option<Continent>,
    #[serde(rename = "Message")]
    pub message: Option<String>,
}

#[derive(Deserialize, Debug)]
pub struct Continent {
    pub code: String,
}

#[derive(Deserialize, Debug)]
pub struct Version {
    pub latest: String,
    pub message: String,
}

#[derive(Deserialize, Serialize, Debug)]
pub struct CacheFile {
    pub version: String,
    pub created: std::time::SystemTime,
    pub connection_history: Vec<HostName>,
    pub cache: Vec<ServerCache>,
}

#[derive(Deserialize, Serialize, Debug, Clone)]
pub struct ServerCache {
    pub hostname: String,
    pub ip: String,
    pub port: u32,
    pub region: String,
}
