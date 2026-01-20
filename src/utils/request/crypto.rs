use super::{ResponseErr, STATUS_OK, client_with_timeout};
use crate::{LOG_ONLY, models::json_data::HmwManifest, utils::main_thread_state};

use pgp::composed::{CleartextSignedMessage, Deserializable, SignedPublicKey};
use tracing::{info, warn};

pub async fn get_latest_hmw_manifest() -> Result<HmwManifest, ResponseErr> {
    let (Some(hmw_manifest), Some(hmw_pgp_public_key)) = (
        main_thread_state::Endpoints::hmw_signed_manifest(),
        main_thread_state::Endpoints::hmw_pgp_public_key(),
    ) else {
        return Err(ResponseErr::other(
            "Could not verify encryption of HMW manifest location\n\
                Restart MatchWire to try again",
        ));
    };

    if main_thread_state::Endpoints::skip_verification() {
        return try_parse_signed_json_unverified::<HmwManifest>(hmw_manifest, "HMW env manifest")
            .await;
    }

    try_parse_signed_json::<HmwManifest>(
        hmw_manifest,
        "HMW manifest",
        hmw_pgp_public_key,
        "HMW PGP public key",
    )
    .await
}

async fn try_parse_signed_json_unverified<T: serde::de::DeserializeOwned>(
    cleartext_json_url: &str,
    cleartext_ctx: &'static str,
) -> Result<T, ResponseErr> {
    let hmw_response = reqwest::get(cleartext_json_url).await?;

    if hmw_response.status() != STATUS_OK {
        return Err(ResponseErr::bad_status(cleartext_ctx, hmw_response));
    }

    let (msg, _headers_msg) =
        CleartextSignedMessage::from_string(hmw_response.text().await?.trim())?;

    warn!("{cleartext_ctx} verification bypassed");
    serde_json::from_str::<T>(&msg.signed_text()).map_err(|err| ResponseErr::Serialize {
        ctx: cleartext_ctx,
        err,
    })
}

pub(crate) async fn try_parse_signed_json<T: serde::de::DeserializeOwned>(
    cleartext_json_url: &str,
    cleartext_ctx: &'static str,
    public_key_url: &str,
    public_key_ctx: &'static str,
) -> Result<T, ResponseErr> {
    let client = client_with_timeout(6);

    let (cleartext_response, public_key_response) = tokio::try_join!(
        client.get(cleartext_json_url).send(),
        client.get(public_key_url).send()
    )?;

    if cleartext_response.status() != STATUS_OK {
        return Err(ResponseErr::bad_status(cleartext_ctx, cleartext_response));
    }

    if public_key_response.status() != STATUS_OK {
        return Err(ResponseErr::bad_status(public_key_ctx, public_key_response));
    }

    let (cleartext_string, public_key_string) =
        tokio::try_join!(cleartext_response.text(), public_key_response.text())?;

    let signed_string = pgp_verify_cleartext(&cleartext_string, &public_key_string)?;
    info!(name: LOG_ONLY, "{cleartext_ctx} PGP signature verified!");

    serde_json::from_str::<T>(&signed_string).map_err(|err| ResponseErr::Serialize {
        ctx: cleartext_ctx,
        err,
    })
}

pub fn pgp_verify_cleartext(cleartext: &str, public_key: &str) -> Result<String, ResponseErr> {
    let (public_key, _headers_public) = SignedPublicKey::from_string(public_key.trim())?;
    let (msg, _headers_msg) = CleartextSignedMessage::from_string(cleartext.trim())?;

    msg.verify(&public_key)?;
    Ok(msg.signed_text())
}
