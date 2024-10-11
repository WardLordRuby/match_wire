use std::{
    collections::HashMap,
    net::{IpAddr, SocketAddr},
    str::FromStr,
};

use serde::{ser::SerializeMap, Deserialize, Deserializer, Serialize, Serializer};

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
    pub port: u16,
    pub map: String,
    pub version: String,
    pub game: String,
    #[serde(rename = "hostname")]
    pub host_name: String,
}

#[derive(Deserialize, Debug)]
pub struct GetInfo {
    #[serde(deserialize_with = "from_string::<_, u8>")]
    pub clients: u8,
    #[serde(rename = "sv_maxclients")]
    #[serde(deserialize_with = "from_string::<_, u8>")]
    pub max_clients: u8,
    #[serde(rename = "sv_privateClients")]
    #[serde(deserialize_with = "from_string::<_, i8>")]
    pub private_clients: i8,
    #[serde(deserialize_with = "from_string::<_, u8>")]
    pub bots: u8,
    #[serde(rename = "gamename")]
    pub game_name: String,
    #[serde(rename = "gametype")]
    pub game_type: String,
    #[serde(rename = "hostname")]
    pub host_name: String,
}

fn from_string<'de, D, T>(deserializer: D) -> Result<T, D::Error>
where
    D: Deserializer<'de>,
    T: FromStr,
    T::Err: std::fmt::Display,
{
    let string = String::deserialize(deserializer)?;
    string.parse::<T>().map_err(serde::de::Error::custom)
}

#[derive(Deserialize, Debug)]
pub struct ServerLocation {
    pub continent: Option<Continent>,
    #[serde(rename = "Message")]
    pub message: Option<String>,
}

#[derive(Deserialize, Debug)]
pub struct Continent {
    #[serde(deserialize_with = "deserialize_country_code")]
    pub code: [char; 2],
}

fn deserialize_country_code<'de, D>(deserializer: D) -> Result<[char; 2], D::Error>
where
    D: Deserializer<'de>,
{
    let string = String::deserialize(deserializer)?;
    string.chars().collect::<Vec<_>>().try_into().map_err(|_| {
        serde::de::Error::custom(format!(
            "Expected 2 character Country code, found: {string}"
        ))
    })
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
    pub iw4m: HashMap<IpAddr, Vec<u16>>,
    pub hmw: HashMap<IpAddr, Vec<u16>>,
    #[serde(
        deserialize_with = "deserialize_country_code_map",
        serialize_with = "serialize_country_code_map"
    )]
    pub regions: HashMap<IpAddr, [char; 2]>,
    pub host_names: HashMap<String, SocketAddr>,
}

fn deserialize_country_code_map<'de, D>(
    deserializer: D,
) -> Result<HashMap<IpAddr, [char; 2]>, D::Error>
where
    D: Deserializer<'de>,
{
    let string_map: HashMap<IpAddr, String> = HashMap::deserialize(deserializer)?;

    string_map
        .into_iter()
        .map(|(ip, code)| {
            let chars = code.chars().collect::<Vec<_>>().try_into().map_err(|_| {
                serde::de::Error::custom(format!(
                    "Expected 2 character Country code, found: {code}"
                ))
            })?;
            Ok((ip, chars))
        })
        .collect()
}

fn serialize_country_code_map<S>(
    map: &HashMap<IpAddr, [char; 2]>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    let mut map_serializer = serializer.serialize_map(Some(map.len()))?;
    for (ip, code) in map {
        let code_str = code.iter().collect::<String>();
        map_serializer.serialize_entry(ip, &code_str)?;
    }
    map_serializer.end()
}

#[derive(Deserialize)]
pub struct HmwManifest {
    #[serde(rename = "Modules")]
    pub modules: Vec<Module>,
    // #[serde(rename = "IgnorePaths")]
    // pub ignore_paths: Vec<String>,
    // #[serde(rename = "ManifestGuid")]
    // pub manifest_guid: String,
}

#[derive(Deserialize)]
pub struct Module {
    // #[serde(rename = "Name")]
    // pub name: String,
    // #[serde(rename = "Version")]
    // pub version: String,
    #[serde(rename = "FilesWithHashes")]
    pub files_with_hashes: HashMap<String, String>,
    // #[serde(rename = "DownloadInfo")]
    // pub download_info: DownloadInfo,
}

// #[derive(Deserialize)]
// pub struct DownloadInfo {
//     #[serde(rename = "DownloadPath")]
//     pub download_path: String,
// }
