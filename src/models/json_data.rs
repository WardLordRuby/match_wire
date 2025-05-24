use crate::{commands::launch_h2m::HostName, utils::caching::AddrMap};

use std::{
    borrow::Cow,
    collections::HashMap,
    net::{IpAddr, SocketAddr},
    str::FromStr,
};

use serde::{Deserialize, Deserializer, Serialize, Serializer, de, ser::SerializeMap};

#[derive(Deserialize, Debug)]
pub(crate) struct HostData {
    pub(crate) servers: Vec<ServerInfo>,
    // pub(crate) uptime: u32,
    // pub(crate) id: String,
    // pub(crate) last_heartbeat: u64,
    pub(crate) ip_address: String,
    pub(crate) webfront_url: String,
    // pub(crate) version: String,
}

#[derive(Deserialize, Debug, Default)]
pub(crate) struct ServerInfo {
    #[serde(rename = "resolved_external_ip_address")]
    pub(crate) resolved_ip: Option<String>,
    pub(crate) ip: String,
    #[serde(rename = "clientnum")]
    pub(crate) clients: u8,
    #[serde(rename = "gametype")]
    pub(crate) game_type: String,
    // pub(crate) id: i64,
    #[serde(rename = "maxclientnum")]
    pub(crate) max_clients: u8,
    pub(crate) port: u16,
    pub(crate) map: String,
    // pub(crate) version: String, // iw4m version
    pub(crate) game: String,
    #[serde(rename = "hostname")]
    pub(crate) host_name: String,
}

#[derive(Deserialize, Debug, Default)]
pub(crate) struct GetInfo {
    #[serde(deserialize_with = "from_string::<_, u8>")]
    pub(crate) clients: u8,
    #[serde(rename = "sv_maxclients")]
    #[serde(deserialize_with = "from_string::<_, u8>")]
    pub(crate) max_clients: u8,
    // #[serde(rename = "gamename")]
    // pub(crate) game_name: String,
    #[serde(rename = "gametype")]
    pub(crate) game_type: Cow<'static, str>,
    #[serde(rename = "hostname")]
    pub(crate) host_name: String,
    #[serde(rename = "mapname")]
    pub(crate) map_name: Cow<'static, str>,

    // Does not appear in the iw4 master server
    #[serde(deserialize_with = "from_string::<_, u8>")]
    pub(crate) bots: u8,
    #[serde(rename = "isPrivate")]
    #[serde(deserialize_with = "from_bool_string::<_>")]
    pub(crate) private: bool,
    #[serde(rename = "sv_privateClients")]
    #[serde(deserialize_with = "from_string::<_, i8>")]
    pub(crate) private_clients: i8,
    #[serde(rename = "gameversion")]
    pub(crate) game_version: String,
}

fn from_string<'de, D, T>(deserializer: D) -> Result<T, D::Error>
where
    D: Deserializer<'de>,
    T: FromStr,
    T::Err: std::fmt::Display,
{
    String::deserialize(deserializer)?
        .parse::<T>()
        .map_err(de::Error::custom)
}

fn from_bool_string<'de, D>(deserializer: D) -> Result<bool, D::Error>
where
    D: Deserializer<'de>,
{
    Ok(match String::deserialize(deserializer)?.as_str() {
        "0" | "false" => false,
        "1" | "true" => true,
        unexpected => {
            return Err(de::Error::invalid_value(
                de::Unexpected::Str(unexpected),
                &"0 or 1",
            ));
        }
    })
}

pub(crate) type ContCode = [u8; 2];

#[derive(Deserialize, Debug)]
pub(crate) struct LocationApiResponse {
    #[serde(rename = "continentCode")]
    #[serde(deserialize_with = "deserialize_country_code")]
    pub(crate) cont_code: Option<ContCode>,
    pub(crate) message: Option<String>,
}

trait ContCodeDeserialize {
    fn to_cont_code<'de, D>(self) -> Result<ContCode, D::Error>
    where
        D: Deserializer<'de>;
}

impl ContCodeDeserialize for String {
    fn to_cont_code<'de, D>(self) -> Result<ContCode, D::Error>
    where
        D: Deserializer<'de>,
    {
        self.chars()
            .map(|c| c as u8)
            .collect::<Vec<_>>()
            .try_into()
            .map_err(|_| {
                serde::de::Error::invalid_value(
                    de::Unexpected::Str(&self),
                    &"2 character Country code",
                )
            })
    }
}

fn deserialize_country_code<'de, D>(deserializer: D) -> Result<Option<ContCode>, D::Error>
where
    D: Deserializer<'de>,
{
    Option::<String>::deserialize(deserializer)?
        .map(String::to_cont_code::<D>)
        .transpose()
}

// `Endpoints` now lives in utils/global_state since the same struct is used inside
// thread_local storage and should maintain private fields

#[derive(Deserialize, Debug)]
pub(crate) struct StartupInfo {
    pub(crate) version: Version,
    pub(crate) endpoints: crate::utils::global_state::Endpoints,
}

#[derive(Deserialize, Debug)]
pub(crate) struct Version {
    pub(crate) latest: String,
    pub(crate) message: String,
}

#[derive(Deserialize, Serialize, Debug)]
pub struct CacheFile {
    pub version: String,
    pub created: std::time::SystemTime,
    #[serde(default)]
    pub cmd_history: Vec<String>,
    #[serde(default)]
    pub connection_history: Vec<HostName>,
    #[serde(default)]
    pub cache: ServerCache,
}

#[derive(Deserialize, Serialize, Debug, Clone, Default)]
pub struct ServerCache {
    pub iw4m: AddrMap,
    pub hmw: AddrMap,
    #[serde(
        deserialize_with = "deserialize_country_code_map",
        serialize_with = "serialize_country_code_map"
    )]
    pub regions: HashMap<IpAddr, ContCode>,
    pub host_names: HashMap<String, SocketAddr>,
}

fn deserialize_country_code_map<'de, D>(
    deserializer: D,
) -> Result<HashMap<IpAddr, ContCode>, D::Error>
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
    map: &HashMap<IpAddr, ContCode>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    let mut map_serializer = serializer.serialize_map(Some(map.len()))?;
    for (ip, code) in map {
        let code_str = code.iter().map(|&c| c as char).collect::<String>();
        map_serializer.serialize_entry(ip, &code_str)?;
    }
    map_serializer.end()
}
pub(crate) struct ContCodeMap<'a>(pub &'a HashMap<IpAddr, ContCode>);

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
pub(crate) struct HmwManifest {
    pub(crate) modules: Vec<Module>,
    // pub(crate) ignore_paths: Vec<String>,
    // pub(crate) manifest_guid: String,
}

#[derive(Deserialize)]
#[serde(rename_all = "PascalCase")]
pub(crate) struct Module {
    pub(crate) name: String,
    pub(crate) version: String,
    pub(crate) files_with_hashes: HashMap<String, String>,
    // pub(crate) download_info: DownloadInfo,
}

// #[derive(Deserialize)]
// #[serde(rename_all = "PascalCase")]
// pub(crate) struct DownloadInfo {
//     pub(crate) download_path: String,
// }
