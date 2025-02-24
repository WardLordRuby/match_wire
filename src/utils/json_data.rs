use std::{
    collections::HashMap,
    net::{IpAddr, SocketAddr},
    str::FromStr,
};

use serde::{Deserialize, Deserializer, Serialize, Serializer, ser::SerializeMap};

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

trait ContCodeDeserialize {
    fn to_cont_code<'de, D>(self) -> Result<[char; 2], D::Error>
    where
        D: Deserializer<'de>;
}

impl ContCodeDeserialize for String {
    fn to_cont_code<'de, D>(self) -> Result<[char; 2], D::Error>
    where
        D: Deserializer<'de>,
    {
        self.chars().collect::<Vec<_>>().try_into().map_err(|_| {
            serde::de::Error::custom(format!("Expected 2 character Country code, found: {self}"))
        })
    }
}

fn deserialize_country_code<'de, D>(deserializer: D) -> Result<[char; 2], D::Error>
where
    D: Deserializer<'de>,
{
    String::deserialize(deserializer)?.to_cont_code::<D>()
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

#[derive(Deserialize, Serialize, Debug, Clone, Default)]
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
    let string_map = HashMap::<IpAddr, String>::deserialize(deserializer)?;

    string_map
        .into_iter()
        .map(|(ip, code)| Ok((ip, code.to_cont_code::<D>()?)))
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
pub struct ContCodeMap<'a>(pub &'a HashMap<IpAddr, [char; 2]>);

impl Serialize for ContCodeMap<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serialize_country_code_map(self.0, serializer)
    }
}

#[derive(Deserialize)]
#[serde(rename_all = "PascalCase")]
pub struct HmwManifest {
    pub modules: Vec<Module>,
    // pub ignore_paths: Vec<String>,
    // pub manifest_guid: String,
}

#[derive(Deserialize)]
#[serde(rename_all = "PascalCase")]
pub struct Module {
    pub name: String,
    // pub version: String,
    pub files_with_hashes: HashMap<String, String>,
    // pub download_info: DownloadInfo,
}

// #[derive(Deserialize)]
// #[serde(rename_all = "PascalCase")]
// pub struct DownloadInfo {
//     pub download_path: String,
// }
