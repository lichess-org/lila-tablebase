use std::{error::Error, fmt, sync::Arc};

use axum::{
    http::StatusCode,
    response::{IntoResponse, Response},
};
use shakmaty::{PositionError, fen::ParseFenError, variant::VariantPosition};
use shakmaty_syzygy::SyzygyError;

macro_rules! dyn_event {
    ($lvl:expr, $($arg:tt)+) => {
        match $lvl {
            ::tracing::Level::TRACE => ::tracing::trace!($($arg)+),
            ::tracing::Level::DEBUG => ::tracing::debug!($($arg)+),
            ::tracing::Level::INFO => ::tracing::info!($($arg)+),
            ::tracing::Level::WARN => ::tracing::warn!($($arg)+),
            ::tracing::Level::ERROR => ::tracing::error!($($arg)+),
        }
    };
}

#[derive(Debug, Clone)]
pub enum TablebaseError {
    Fen(ParseFenError),
    Position(Arc<PositionError<VariantPosition>>),
    Syzygy(Arc<SyzygyError>),
    UpstreamRequest(Arc<reqwest::Error>),
}

impl TablebaseError {
    pub fn tracing_level(&self) -> tracing::Level {
        match self {
            TablebaseError::Fen(_) => tracing::Level::WARN,
            TablebaseError::Position(_) => tracing::Level::WARN,
            TablebaseError::Syzygy(err) => match **err {
                SyzygyError::Castling | SyzygyError::TooManyPieces => tracing::Level::INFO,
                SyzygyError::MissingTable { .. } | SyzygyError::ProbeFailed { .. } => {
                    tracing::Level::ERROR
                }
            },
            TablebaseError::UpstreamRequest(_) => tracing::Level::ERROR,
        }
    }
}

impl fmt::Display for TablebaseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TablebaseError::Fen(err) => err.fmt(f),
            TablebaseError::Position(err) => err.fmt(f),
            TablebaseError::Syzygy(err) => err.fmt(f),
            TablebaseError::UpstreamRequest(err) => {
                let mut err: &dyn Error = err;
                write!(f, "upstream request failed: {err}")?;
                while let Some(src) = err.source() {
                    write!(f, "-> {src}")?;
                    err = src;
                }
                Ok(())
            }
        }
    }
}

impl From<ParseFenError> for TablebaseError {
    fn from(v: ParseFenError) -> TablebaseError {
        TablebaseError::Fen(v)
    }
}

impl From<Arc<PositionError<VariantPosition>>> for TablebaseError {
    fn from(v: Arc<PositionError<VariantPosition>>) -> TablebaseError {
        TablebaseError::Position(v)
    }
}

impl From<SyzygyError> for TablebaseError {
    fn from(v: SyzygyError) -> TablebaseError {
        TablebaseError::Syzygy(Arc::new(v))
    }
}

impl From<reqwest::Error> for TablebaseError {
    fn from(v: reqwest::Error) -> TablebaseError {
        TablebaseError::UpstreamRequest(Arc::new(v))
    }
}

impl IntoResponse for TablebaseError {
    fn into_response(self) -> Response {
        (match self {
            TablebaseError::Fen(err) => (StatusCode::BAD_REQUEST, err.to_string()),
            TablebaseError::Position(err) => (StatusCode::BAD_REQUEST, err.to_string()),
            TablebaseError::Syzygy(err) => match *err {
                SyzygyError::Castling
                | SyzygyError::TooManyPieces
                | SyzygyError::MissingTable { .. } => (StatusCode::NOT_FOUND, err.to_string()),
                SyzygyError::ProbeFailed { .. } => {
                    (StatusCode::INTERNAL_SERVER_ERROR, err.to_string())
                }
            },
            TablebaseError::UpstreamRequest(err) => {
                (StatusCode::INTERNAL_SERVER_ERROR, err.to_string())
            }
        })
        .into_response()
    }
}
