use std::sync::Arc;

use axum::{
    http::StatusCode,
    response::{IntoResponse, Response},
};
use shakmaty::{fen::ParseFenError, variant::VariantPosition, PositionError};
use shakmaty_syzygy::SyzygyError;
use tracing::{error, warn};

#[derive(Debug, Clone)]
pub enum TablebaseError {
    Fen(ParseFenError),
    Position(Arc<PositionError<VariantPosition>>),
    Syzygy(Arc<SyzygyError>),
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

impl IntoResponse for TablebaseError {
    fn into_response(self) -> Response {
        (match self {
            TablebaseError::Fen(err) => (StatusCode::BAD_REQUEST, err.to_string()),
            TablebaseError::Position(err) => (StatusCode::BAD_REQUEST, err.to_string()),
            TablebaseError::Syzygy(err) => match *err {
                SyzygyError::Castling | SyzygyError::TooManyPieces => {
                    (StatusCode::NOT_FOUND, err.to_string())
                }
                SyzygyError::MissingTable { .. } => {
                    warn!("{err}");
                    (StatusCode::NOT_FOUND, err.to_string())
                }
                SyzygyError::ProbeFailed { .. } => {
                    error!("{err}");
                    (StatusCode::INTERNAL_SERVER_ERROR, err.to_string())
                }
            },
        })
        .into_response()
    }
}
