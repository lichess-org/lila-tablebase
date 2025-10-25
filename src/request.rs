use std::sync::Arc;

use serde::Deserialize;
use shakmaty::{
    fen::Fen,
    variant::{Variant, VariantPosition},
    CastlingMode, PositionError,
};

#[derive(Deserialize, Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[serde(rename_all = "lowercase")]
pub enum TablebaseVariant {
    Standard,
    Atomic,
    Antichess,
}

impl From<TablebaseVariant> for Variant {
    fn from(variant: TablebaseVariant) -> Variant {
        match variant {
            TablebaseVariant::Standard => Variant::Chess,
            TablebaseVariant::Atomic => Variant::Atomic,
            TablebaseVariant::Antichess => Variant::Antichess,
        }
    }
}

impl TablebaseVariant {
    pub fn position(
        self,
        fen: Fen,
    ) -> Result<VariantPosition, Arc<PositionError<VariantPosition>>> {
        VariantPosition::from_setup(self.into(), fen.into_setup(), CastlingMode::Standard)
            .or_else(PositionError::ignore_invalid_castling_rights)
            .or_else(PositionError::ignore_invalid_ep_square)
            .or_else(PositionError::ignore_impossible_check)
            .map_err(Arc::new)
    }
}

#[derive(Deserialize, Default, Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[serde(rename_all = "lowercase")]
pub enum Op1Mode {
    #[default]
    Never,
    Auxiliary,
    Always,
}

#[derive(Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct TablebaseQuery {
    pub fen: Fen,
    #[serde(default)]
    pub dtc: Op1Mode,
}
