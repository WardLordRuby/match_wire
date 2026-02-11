use super::{JOINING_STR, send_msg_over};
use crate::{
    LOG_ONLY,
    commands::{
        Message,
        filter::{Addressable, GetInfoMetaData, Request, Sourced, try_get_info},
    },
    parse_hostname,
    utils::{
        main_thread_state::{self, ThreadCopyState},
        request::client_with_timeout,
    },
};

use std::net::{AddrParseError, SocketAddr};

use repl_oxide::strip_ansi;
use serde::{Deserialize, Serialize};
use tokio::sync::mpsc::Sender;
use tracing::info;

pub(super) enum Connection {
    Browser,
    Direct(usize),
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct HostName {
    pub parsed: String,
    pub raw: String,
}

pub struct HostNameRequestMeta {
    pub host_name: HostName,
    pub socket_addr: Option<Result<SocketAddr, String>>,
}

impl HostNameRequestMeta {
    fn new(host_name_raw: String, socket_addr: Option<Result<SocketAddr, String>>) -> Self {
        HostNameRequestMeta {
            host_name: HostName {
                parsed: parse_hostname(&host_name_raw),
                raw: host_name_raw,
            },
            socket_addr,
        }
    }
}

enum HostRequestErr {
    AddrParseErr(AddrParseError),
    RequestErr(String),
}

impl From<AddrParseError> for HostRequestErr {
    fn from(value: AddrParseError) -> Self {
        HostRequestErr::AddrParseErr(value)
    }
}

impl From<GetInfoMetaData> for HostRequestErr {
    fn from(value: GetInfoMetaData) -> Self {
        // meta data discarded since the caller doesn't use it / avoids triggering large enum variant size diff
        HostRequestErr::RequestErr(format!(
            "Server at: {}, did not respond to a 'getInfo' request",
            value.meta.socket_addr()
        ))
    }
}

impl HostName {
    pub fn from_browser(stripped: &str, version: f64) -> Result<HostNameRequestMeta, String> {
        let (host_name, socket_addr) = if version < 1.0 {
            let host_name = stripped
                .split_once(JOINING_STR)
                .expect("`Connection::Browser` is found and client is 'original h2m', meaning `JOINING_STR` was found in the `value` array")
                .1
                .strip_suffix("...")
                .ok_or_else(|| {
                    format!("Unexpected H2M console output found. ansi_stripped_input: '{stripped}' does not end in: '...'")
                })?;

            (host_name, None)
        } else {
            let (pre, host_name) = stripped.split_once("} ").ok_or_else(|| {
                format!("Unexpected HMW console output found. ansi_stripped_input: '{stripped}', does not contain: '}} '")
            })?;
            let ip = pre
                .rsplit_once('{')
                .ok_or_else(|| format!("Unexpected HMW console output found. left_stripped_split: '{pre}', does not contain '{{'"))
                .and_then(|(_, ip_str)| {
                    ip_str.parse::<SocketAddr>()
                        .map_err(|err| format!("Failed to parse: {ip_str}, {err}"))
                });

            (host_name, Some(ip))
        };

        Ok(HostNameRequestMeta::new(host_name.to_string(), socket_addr))
    }

    async fn from_request(
        stripped: &str,
        offset: usize,
    ) -> Result<HostNameRequestMeta, HostRequestErr> {
        let socket_addr = stripped[offset..].trim().parse::<SocketAddr>()?;
        let server_info = try_get_info(
            Request::New(Sourced::Hmw(socket_addr)),
            client_with_timeout(5),
            main_thread_state::Endpoints::server_info_endpoint(),
        )
        .await?;

        Ok(HostNameRequestMeta::new(
            server_info.info.host_name,
            Some(Ok(socket_addr)),
        ))
    }
}

/// Caller must guarantee that the given `Connection` kind is valid within `line`
pub(super) async fn add_to_history(
    msg_sender: &Sender<Message>,
    line: &str,
    kind: Connection,
    version: f64,
) {
    debug_assert_eq!(strip_ansi(line), line);
    let res = match kind {
        Connection::Browser => match HostName::from_browser(line, version) {
            Ok(mut data) => {
                if let Some(Err(ref mut err)) = data.socket_addr {
                    send_msg_over(msg_sender, Message::error(std::mem::take(err))).await;
                }
                Ok(data)
            }
            Err(err) => Err(err),
        },
        Connection::Direct(offset) => match HostName::from_request(line, offset).await {
            Ok(data) => Ok(data),
            Err(HostRequestErr::AddrParseErr(err)) => Err(err.to_string()),
            Err(HostRequestErr::RequestErr(err)) => Err(err),
        },
    };
    match res {
        Ok(meta) => {
            info!(name: LOG_ONLY, "Connected to {}", meta.host_name.parsed);
            cache_insert(meta);
        }
        Err(err) => send_msg_over(msg_sender, Message::error(err)).await,
    }
}

fn cache_insert(host_name_meta: HostNameRequestMeta) {
    main_thread_state::Cache::with_borrow_mut(|cache| {
        let mut modified = true;
        let ip_opt = if let Some(Ok(ip)) = host_name_meta.socket_addr {
            cache
                .host_to_connect
                .entry(host_name_meta.host_name.raw.clone())
                .and_modify(|cache_ip| {
                    if *cache_ip == ip {
                        modified = false
                    } else {
                        *cache_ip = ip
                    }
                })
                .or_insert(ip);
            Some(ip)
        } else {
            cache
                .host_to_connect
                .get(&host_name_meta.host_name.raw)
                .copied()
        };
        if let Some(curr_ip) = ip_opt
            && let Some(i) = cache.connection_history.iter().position(|prev| {
                prev.raw != host_name_meta.host_name.raw
                    && cache
                        .host_to_connect
                        .get(&prev.raw)
                        .is_some_and(|&ip| ip == curr_ip)
            })
        {
            cache.connection_history.remove(i);
            modified = true
        }
        if let Some(i) = cache
            .connection_history
            .iter()
            .position(|prev| prev.raw == host_name_meta.host_name.raw)
        {
            if i != cache.connection_history.len() - 1 {
                let entry = cache.connection_history.remove(i);
                cache.connection_history.push(entry);
                modified = true
            }
        } else {
            cache.connection_history.push(host_name_meta.host_name);
            modified = true
        }
        main_thread_state::UpdateCache::and_modify(|curr| curr | modified);
    });
}
