use crate::{commands::launch_h2m::HostName, ENDPOINTS};

use std::{
    borrow::Cow,
    collections::HashMap,
    net::{IpAddr, SocketAddr},
    str::FromStr,
};

use serde::{ser::SerializeMap, Deserialize, Deserializer, Serialize, Serializer};

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

#[derive(Deserialize, Debug)]
pub(crate) struct ServerInfo {
    pub(crate) ip: String,
    #[serde(rename = "clientnum")]
    pub(crate) clients: u8,
    // #[serde(rename = "gametype")]
    // pub(crate) game_type: String,
    // pub(crate) id: i64,
    #[serde(rename = "maxclientnum")]
    pub(crate) max_clients: u8,
    pub(crate) port: u16,
    // pub(crate) map: String,
    // pub(crate) version: String,
    pub(crate) game: String,
    #[serde(rename = "hostname")]
    pub(crate) host_name: String,
}

#[derive(Deserialize, Debug)]
pub(crate) struct GetInfo {
    #[serde(deserialize_with = "from_string::<_, u8>")]
    pub(crate) clients: u8,
    #[serde(rename = "sv_maxclients")]
    #[serde(deserialize_with = "from_string::<_, u8>")]
    pub(crate) max_clients: u8,
    // #[serde(rename = "sv_privateClients")]
    // #[serde(deserialize_with = "from_string::<_, i8>")]
    // pub(crate) private_clients: i8,
    #[serde(deserialize_with = "from_string::<_, u8>")]
    pub(crate) bots: u8,
    // #[serde(rename = "gamename")]
    // pub(crate) game_name: String,
    // #[serde(rename = "gametype")]
    // pub(crate) game_type: String,
    #[serde(rename = "hostname")]
    pub(crate) host_name: String,
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
pub(crate) struct ServerLocation {
    pub(crate) continent: Option<Continent>,
    #[serde(rename = "Message")]
    pub(crate) message: Option<String>,
}

#[derive(Deserialize, Debug)]
pub(crate) struct Continent {
    #[serde(deserialize_with = "deserialize_country_code")]
    pub(crate) code: [u8; 2],
}

trait ContCodeDeserialize {
    fn to_cont_code<'de, D>(self) -> Result<[u8; 2], D::Error>
    where
        D: Deserializer<'de>;
}

impl ContCodeDeserialize for String {
    fn to_cont_code<'de, D>(self) -> Result<[u8; 2], D::Error>
    where
        D: Deserializer<'de>,
    {
        self.chars()
            .map(|c| c as u8)
            .collect::<Vec<_>>()
            .try_into()
            .map_err(|_| {
                serde::de::Error::custom(format!(
                    "Expected 2 character Country code, found: {self}"
                ))
            })
    }
}

fn deserialize_country_code<'de, D>(deserializer: D) -> Result<[u8; 2], D::Error>
where
    D: Deserializer<'de>,
{
    String::deserialize(deserializer)?.to_cont_code::<D>()
}

#[derive(Deserialize, Debug)]
pub(crate) struct StartupInfo {
    pub(crate) version: Version,
    pub(crate) endpoints: Endpoints,
}

#[derive(Deserialize, Debug)]
pub(crate) struct Version {
    pub(crate) latest: String,
    pub(crate) message: String,
}

#[derive(Deserialize, Debug)]
pub(crate) struct Endpoints {
    iw4_master_server: Cow<'static, str>,
    hmw_master_server: Cow<'static, str>,
    hmw_manifest: Cow<'static, str>,
    hmw_download: Cow<'static, str>,
    manifest_hash_path: Option<Cow<'static, str>>,
    server_info_endpoint: Cow<'static, str>,
}

impl Default for Endpoints {
    fn default() -> Self {
        Self {
            iw4_master_server: Cow::Borrowed("https://master.iw4.zip/instance"),
            hmw_master_server: Cow::Borrowed("https://ms.horizonmw.org/game-servers"),
            hmw_manifest: Cow::Borrowed("https://price.horizonmw.org/manifest.json"),
            hmw_download: Cow::Borrowed("https://docs.horizonmw.org/download"),
            manifest_hash_path: None,
            server_info_endpoint: Cow::Borrowed("/getInfo"),
        }
    }
}

impl Endpoints {
    #[inline]
    fn get() -> &'static Self {
        ENDPOINTS
            .get()
            .expect("tried to access endpoints before startup process")
    }

    #[inline]
    pub(crate) fn iw4_master_server() -> &'static str {
        &Self::get().iw4_master_server
    }
    #[inline]
    pub(crate) fn hmw_master_server() -> &'static str {
        &Self::get().hmw_master_server
    }
    #[inline]
    pub(crate) fn hmw_manifest() -> &'static str {
        &Self::get().hmw_manifest
    }
    #[inline]
    pub(crate) fn hmw_download() -> &'static str {
        &Self::get().hmw_download
    }
    #[inline]
    pub(crate) fn manifest_hash_path() -> Option<&'static str> {
        Self::get().manifest_hash_path.as_deref()
    }
    #[inline]
    pub(crate) fn server_info_endpoint() -> &'static str {
        &Self::get().server_info_endpoint
    }
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
    pub iw4m: HashMap<IpAddr, Vec<u16>>,
    pub hmw: HashMap<IpAddr, Vec<u16>>,
    #[serde(
        deserialize_with = "deserialize_country_code_map",
        serialize_with = "serialize_country_code_map"
    )]
    pub regions: HashMap<IpAddr, [u8; 2]>,
    pub host_names: HashMap<String, SocketAddr>,
}

fn deserialize_country_code_map<'de, D>(
    deserializer: D,
) -> Result<HashMap<IpAddr, [u8; 2]>, D::Error>
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
    map: &HashMap<IpAddr, [u8; 2]>,
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
pub(crate) struct ContCodeMap<'a>(pub &'a HashMap<IpAddr, [u8; 2]>);

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
    // pub(crate) version: String,
    pub(crate) files_with_hashes: HashMap<String, String>,
    // pub(crate) download_info: DownloadInfo,
}

// #[derive(Deserialize)]
// #[serde(rename_all = "PascalCase")]
// pub(crate) struct DownloadInfo {
//     pub(crate) download_path: String,
// }
