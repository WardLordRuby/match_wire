use std::collections::HashMap;

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
    #[serde(rename = "clientnum")]
    pub clients: u8,
    #[serde(rename = "gametype")]
    pub game_type: String,
    pub id: i64,
    #[serde(rename = "maxclientnum")]
    pub max_clients: u8,
    pub port: u32,
    pub map: String,
    pub version: String,
    pub game: String,
    #[serde(rename = "hostname")]
    pub host_name: String,
}

#[derive(Deserialize, Debug, Clone)]
pub struct GetInfo {
    #[serde(deserialize_with = "from_string_u8")]
    pub clients: u8,
    #[serde(rename = "sv_maxclients")]
    #[serde(deserialize_with = "from_string_u8")]
    pub max_clients: u8,
    #[serde(rename = "sv_privateClients")]
    #[serde(deserialize_with = "from_string_i8")]
    pub private_clients: i8,
    #[serde(deserialize_with = "from_string_u8")]
    pub bots: u8,
    #[serde(rename = "gamename")]
    pub game_name: String,
    #[serde(rename = "gametype")]
    pub game_type: String,
    #[serde(rename = "hostname")]
    pub host_name: String,
}

use serde::Deserializer;

fn from_string_u8<'de, D>(deserializer: D) -> Result<u8, D::Error>
where
    D: Deserializer<'de>,
{
    let value = String::deserialize(deserializer)?;
    value.parse::<u8>().map_err(serde::de::Error::custom)
}

fn from_string_i8<'de, D>(deserializer: D) -> Result<i8, D::Error>
where
    D: Deserializer<'de>,
{
    let value = String::deserialize(deserializer)?;
    value.parse::<i8>().map_err(serde::de::Error::custom)
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
    pub cache: ServerCache,
}

#[derive(Deserialize, Serialize, Debug, Clone)]
pub struct ServerCache {
    /// IP -> ports
    pub iw4m: HashMap<String, Vec<u32>>,
    /// IP -> ports
    pub hmw: HashMap<String, Vec<u32>>,
    /// IP -> 2 char cont. code
    pub regions: HashMap<String, String>,
}
