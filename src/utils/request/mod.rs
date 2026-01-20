pub mod crypto;

use std::{borrow::Cow, time::Duration};

use reqwest::Client;

pub(crate) const STATUS_OK: reqwest::StatusCode = reqwest::StatusCode::OK;

#[derive(Debug)]
pub enum ResponseErr {
    Reqwest(reqwest::Error),
    Status {
        msg: Cow<'static, str>,
        status: reqwest::StatusCode,
    },
    Pgp(pgp::errors::Error),
    Serialize {
        ctx: &'static str,
        err: serde_json::Error,
    },
    Other(Cow<'static, str>),
}

impl ResponseErr {
    pub(crate) fn bad_status<T: Into<Cow<'static, str>>>(
        ctx: T,
        response: reqwest::Response,
    ) -> Self {
        Self::Status {
            msg: ctx.into(),
            status: response.status(),
        }
    }
    crate::from_static_cow_fn!(Other, other);
}

impl From<reqwest::Error> for ResponseErr {
    fn from(err: reqwest::Error) -> Self {
        Self::Reqwest(err)
    }
}

impl From<pgp::errors::Error> for ResponseErr {
    fn from(err: pgp::errors::Error) -> Self {
        Self::Pgp(err)
    }
}

impl std::fmt::Display for ResponseErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResponseErr::Reqwest(err) => write!(f, "{err}"),
            ResponseErr::Status { msg, status } => write!(f, "{msg}: {status}"),
            ResponseErr::Other(msg) => write!(f, "{msg}"),
            ResponseErr::Pgp(err) => write!(f, "PGP verification failed: {err}"),
            ResponseErr::Serialize { ctx, err } => write!(f, "{ctx} formatting has changed: {err}"),
        }
    }
}

pub(crate) fn client_with_timeout(secs: u64) -> Client {
    Client::builder().timeout(Duration::from_secs(secs)).build().expect(
        "TLS backend cannot be initialized, or the resolver cannot load the system configuration",
    )
}
