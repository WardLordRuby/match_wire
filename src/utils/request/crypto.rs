use super::{ResponseErr, STATUS_OK, client_with_timeout};
use crate::{
    CRATE_NAME, LOG_ONLY,
    models::json_data::{HmwManifest, StartupInfo},
    utils::main_thread_state,
};

use pgp::composed::{CleartextSignedMessage, Deserializable, SignedPublicKey};
use serde::de::DeserializeOwned;
use tracing::{info, warn};

const SIGNED_STARTUP_INFO: &str =
    "https://gist.githubusercontent.com/WardLordRuby/795d840e208df2de08735c152889b2e4/raw";
const MATCH_WIRE_PUBLIC_PGP_KEY: &str =
    "https://gist.githubusercontent.com/WardLordRuby/ca630bae78429d56a9484a099ddbefde/raw";

pub(in crate::utils) struct ClearTextJsonUrl {
    json: &'static str,
    key: &'static str,
}

impl ClearTextJsonUrl {
    pub(in crate::utils) fn new(json: &'static str, key: &'static str) -> Self {
        Self { json, key }
    }

    const fn startup_info() -> UrlSource {
        UrlSource::Static(Self {
            json: SIGNED_STARTUP_INFO,
            key: MATCH_WIRE_PUBLIC_PGP_KEY,
        })
    }
}

pub(in crate::utils) enum UrlSource {
    Static(ClearTextJsonUrl),
    Getter(fn() -> Result<ClearTextJsonUrl, ResponseErr>),
}

pub(in crate::utils) trait ClearTextJson {
    const CTX: &str;
    const KEY_CTX: &str;
    const URLS: UrlSource;

    fn get_urls() -> Result<ClearTextJsonUrl, ResponseErr> {
        match Self::URLS {
            UrlSource::Static(url) => Ok(url),
            UrlSource::Getter(get_urls) => get_urls(),
        }
    }
}

macro_rules! context_msg {
    ($owner:expr, $object_type:expr) => {
        const CTX: &str = constcat::concat!($owner, " ", $object_type);
        const KEY_CTX: &str = constcat::concat!($owner, " PGP public key");
    };
}

impl ClearTextJson for StartupInfo {
    context_msg!(CRATE_NAME, "startup json");
    const URLS: UrlSource = ClearTextJsonUrl::startup_info();
}

impl ClearTextJson for HmwManifest {
    context_msg!("HMW", "manifest");
    const URLS: UrlSource = UrlSource::Getter(main_thread_state::Endpoints::hmw_signed_urls);
}

pub async fn get_latest_hmw_manifest() -> Result<HmwManifest, ResponseErr> {
    if main_thread_state::Endpoints::skip_verification() {
        try_parse_signed_json_unverified::<HmwManifest>().await
    } else {
        try_parse_signed_json::<HmwManifest>().await
    }
}

async fn try_parse_signed_json_unverified<T>() -> Result<T, ResponseErr>
where
    T: DeserializeOwned + ClearTextJson,
{
    let url = T::get_urls()?;
    let hmw_response = reqwest::get(url.json).await?;

    if hmw_response.status() != STATUS_OK {
        return Err(ResponseErr::bad_status(T::CTX, hmw_response));
    }

    let (msg, _headers_msg) =
        CleartextSignedMessage::from_string(hmw_response.text().await?.trim())?;

    warn!("{} verification bypassed", T::CTX);
    serde_json::from_str::<T>(&msg.signed_text())
        .map_err(|err| ResponseErr::Serialize { ctx: T::CTX, err })
}

pub(in crate::utils) async fn try_parse_signed_json<T>() -> Result<T, ResponseErr>
where
    T: DeserializeOwned + ClearTextJson,
{
    let client = client_with_timeout(6);
    let url = T::get_urls()?;

    let (cleartext_response, public_key_response) =
        tokio::try_join!(client.get(url.json).send(), client.get(url.key).send())?;

    if cleartext_response.status() != STATUS_OK {
        return Err(ResponseErr::bad_status(T::CTX, cleartext_response));
    }

    if public_key_response.status() != STATUS_OK {
        return Err(ResponseErr::bad_status(T::KEY_CTX, public_key_response));
    }

    let (cleartext_string, public_key_string) =
        tokio::try_join!(cleartext_response.text(), public_key_response.text())?;

    let signed_string = pgp_verify_cleartext(&cleartext_string, &public_key_string)?;
    info!(name: LOG_ONLY, "{} PGP signature verified!", T::CTX);

    serde_json::from_str::<T>(&signed_string)
        .map_err(|err| ResponseErr::Serialize { ctx: T::CTX, err })
}

fn pgp_verify_cleartext(cleartext: &str, public_key: &str) -> Result<String, ResponseErr> {
    let (public_key, _headers_public) = SignedPublicKey::from_string(public_key.trim())?;
    let (msg, _headers_msg) = CleartextSignedMessage::from_string(cleartext.trim())?;

    msg.verify(&public_key)?;
    Ok(msg.signed_text())
}

#[cfg(test)]
mod test {
    use super::{ClearTextJson, ClearTextJsonUrl, UrlSource, pgp_verify_cleartext};
    use crate::{
        models::json_data::StartupInfo,
        utils::{main_thread_state::Endpoints, request::ResponseErr},
    };
    use reqwest::{StatusCode, blocking::get};

    fn blocking_verify<T: ClearTextJson>() -> Result<String, ResponseErr> {
        let url = T::get_urls()?;

        let cleartext_response = get(url.json).unwrap();
        let public_key_response = get(url.key).unwrap();

        if cleartext_response.status() != StatusCode::OK {
            panic!(
                "{} returned status: {}",
                T::CTX,
                cleartext_response.status()
            )
        }

        if public_key_response.status() != StatusCode::OK {
            panic!(
                "{} returned status: {}",
                T::KEY_CTX,
                public_key_response.status()
            )
        }

        let cleartext_string = cleartext_response.text().unwrap();
        let public_key_string = public_key_response.text().unwrap();

        pgp_verify_cleartext(&cleartext_string, &public_key_string)
    }

    struct TestHmwManifest;

    static MANIFEST_URL: std::sync::OnceLock<Endpoints> = std::sync::OnceLock::new();

    impl ClearTextJson for TestHmwManifest {
        context_msg!("HMW", "manifest");
        const URLS: UrlSource = UrlSource::Getter(|| {
            let manifest = MANIFEST_URL.get().unwrap();
            Ok(ClearTextJsonUrl::new(
                manifest.TESTING_get_hmw_manifest_signed().unwrap(),
                manifest.TESTING_get_hmw_pgp_public_key().unwrap(),
            ))
        });
    }

    #[test]
    fn pgp_verify_manifest() {
        let remote_endpoints = blocking_verify::<StartupInfo>()
            .map(|data| {
                serde_json::from_str::<StartupInfo>(&data)
                    .unwrap()
                    .endpoints
            })
            .unwrap();

        MANIFEST_URL.set(remote_endpoints).unwrap();

        let hmw_manifest_res = blocking_verify::<TestHmwManifest>();
        assert!(hmw_manifest_res.is_ok())
    }
}
