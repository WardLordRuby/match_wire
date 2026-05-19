pub mod crypto;

use crate::{LOG_ONLY, commands::filter::ops::MasterServer};
use crypto::ClearTextJson;

use std::{borrow::Cow, time::Duration};

use reqwest::{Client, Response, StatusCode};
use serde::de::DeserializeOwned;
use tracing::error;

pub(crate) const STATUS_OK: StatusCode = StatusCode::OK;

pub(crate) async fn serde_deserialize<T: DeserializeOwned>(
    response: Response,
) -> Result<T, ResponseErr> {
    let response_bytes = response
        .bytes()
        .await
        .map_err(|err| ResponseErr::reqwest("Location response", err))?;

    serde_json::from_slice::<T>(&response_bytes)
        .map_err(|err| ResponseErr::serialize("Location response body", err))
}

pub struct ResponseErr {
    ctx: Cow<'static, str>,
    err: ResponseErrType,
}

impl ResponseErr {
    pub(super) fn cleartext_reqwest<T: ClearTextJson>(err: reqwest::Error) -> Self {
        Self {
            ctx: T::CTX.into(),
            err: ResponseErrType::Reqwest(err),
        }
    }

    pub(crate) fn master_server_reqwest<T: MasterServer>(err: reqwest::Error) -> Self {
        Self {
            ctx: T::NAME.into(),
            err: ResponseErrType::Reqwest(err),
        }
    }

    pub(crate) fn reqwest<T: Into<Cow<'static, str>>>(ctx: T, err: reqwest::Error) -> Self {
        Self {
            ctx: ctx.into(),
            err: ResponseErrType::Reqwest(err),
        }
    }

    pub(crate) fn bad_status<T: Into<Cow<'static, str>>>(ctx: T, response: Response) -> Self {
        Self {
            ctx: ctx.into(),
            err: ResponseErrType::Status(response.status()),
        }
    }

    pub(super) fn cleartext_pgp<T: ClearTextJson>(err: pgp::errors::Error) -> Self {
        Self {
            ctx: T::CTX.into(),
            err: ResponseErrType::Pgp(err),
        }
    }

    pub(super) fn cleartext_serialize<T: ClearTextJson>(err: serde_json::Error) -> Self {
        Self {
            ctx: T::CTX.into(),
            err: ResponseErrType::Serialize(err),
        }
    }

    pub(crate) fn serialize<T: Into<Cow<'static, str>>>(ctx: T, err: serde_json::Error) -> Self {
        Self {
            ctx: ctx.into(),
            err: ResponseErrType::Serialize(err),
        }
    }

    pub(crate) fn other<T: Into<Cow<'static, str>>>(ctx: T) -> Self {
        Self {
            ctx: ctx.into(),
            err: ResponseErrType::Other,
        }
    }

    pub(crate) fn has_debug_info(&self) -> bool {
        matches!(
            &self.err,
            ResponseErrType::Reqwest(_) | ResponseErrType::Pgp(_) | ResponseErrType::Serialize(_)
        )
    }

    pub(crate) fn display_and_log(&self) {
        error!("{self}");
        if self.has_debug_info() {
            error!(name: LOG_ONLY, "{self:?}");
        }
    }

    pub(crate) fn log_only(&self) {
        if self.has_debug_info() {
            error!(name: LOG_ONLY, "{self}: {self:?}");
        } else {
            error!(name: LOG_ONLY, "{self}");
        }
    }
}

#[derive(Debug)]
pub enum ResponseErrType {
    Reqwest(reqwest::Error),
    Status(StatusCode),
    Pgp(pgp::errors::Error),
    Serialize(serde_json::Error),
    Other,
}

impl std::fmt::Display for ResponseErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.err {
            ResponseErrType::Reqwest(_err) => write!(f, "Failed to request {} data", self.ctx),
            ResponseErrType::Status(code) => {
                write!(f, "{}, returned invalid status: {code}", self.ctx)
            }
            ResponseErrType::Pgp(_err) => write!(f, "{} PGP verification failed", self.ctx),
            ResponseErrType::Serialize(_err) => {
                write!(f, "{} formatting has changed", self.ctx)
            }
            ResponseErrType::Other => write!(f, "{}", self.ctx),
        }
    }
}

impl std::fmt::Debug for ResponseErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.err {
            ResponseErrType::Reqwest(err) => write!(f, "{err:?}"),
            ResponseErrType::Pgp(err) => write!(f, "{err:?}"),
            ResponseErrType::Serialize(err) => write!(f, "{err:?}"),
            ResponseErrType::Status(_) | ResponseErrType::Other => write!(f, "{self}"),
        }
    }
}

pub(crate) fn client_with_timeout(secs: u64) -> Client {
    Client::builder().timeout(Duration::from_secs(secs)).build().expect(
        "TLS backend cannot be initialized, or the resolver cannot load the system configuration",
    )
}
