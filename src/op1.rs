use std::{collections::HashMap, time::Duration};

use serde::{Deserialize, Serialize};
use shakmaty::{
    fen::Fen, uci::UciMove, variant::VariantPosition, ByColor, ByRole, Chess, EnPassantMode,
    Position,
};

#[derive(Debug, Serialize, Deserialize, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Dtc(pub i32);

impl Dtc {
    #[inline]
    pub fn is_zero(self) -> bool {
        self.0 == 0
    }

    #[inline]
    pub fn is_negative(self) -> bool {
        self.0 < 0
    }

    #[must_use]
    pub fn add_plies_saturating(self, plies: u32) -> Dtc {
        match self {
            Dtc(0) => Dtc(0),
            Dtc(n) if n > 0 => i32::try_from(plies)
                .ok()
                .and_then(|plies| n.checked_add(plies))
                .map_or(Dtc(i32::MAX), Dtc),
            Dtc(n) => i32::try_from(plies)
                .ok()
                .and_then(|plies| n.checked_sub(plies))
                .map_or(Dtc(i32::MIN), Dtc),
        }
    }
}

#[derive(Deserialize, Debug, Default)]
pub struct Op1Response {
    pub parent: Option<Dtc>,
    pub children: HashMap<UciMove, Option<Dtc>>,
}

#[derive(Debug)]
pub struct Op1Client {
    endpoint: String,
    client: reqwest::Client,
}

fn is_currently_supported(pos: &Chess) -> bool {
    let kbp = ByRole {
        king: 1,
        bishop: 1,
        pawn: 1,
        ..Default::default()
    };
    let kpppp = ByRole {
        king: 1,
        pawn: 4,
        ..Default::default()
    };

    let material = pos.board().material();
    material
        == ByColor {
            white: kbp,
            black: kpppp,
        }
        || material
            == ByColor {
                white: kpppp,
                black: kbp,
            }
}

fn is_op1(pos: &Chess) -> bool {
    let white_pawns = pos.board().white() & pos.board().pawns();
    let white_pawn_paths = white_pawns.shift(8)
        | white_pawns.shift(16)
        | white_pawns.shift(24)
        | white_pawns.shift(32)
        | white_pawns.shift(40);

    pos.board().occupied().count() == 8
        && !pos.castles().any()
        && (white_pawn_paths & pos.board().black() & pos.board().pawns()).any()
}

impl Op1Client {
    pub fn new(endpoint: &str) -> Op1Client {
        Op1Client {
            endpoint: endpoint.to_owned(),
            client: reqwest::Client::builder()
                .timeout(Duration::from_secs(5))
                .build()
                .expect("op1 client"),
        }
    }

    pub async fn probe_dtc(&self, pos: &VariantPosition) -> Result<Op1Response, reqwest::Error> {
        let VariantPosition::Chess(ref pos) = pos else {
            return Ok(Op1Response::default());
        };

        if pos.board().occupied().count() > 9 || pos.board().occupied().count() < 8 {
            return Ok(Op1Response::default());
        }

        if !(is_op1(pos) && is_currently_supported(pos))
            && !pos.legal_moves().into_iter().any(|m| {
                let mut after = pos.clone();
                after.play_unchecked(&m);
                is_op1(&after) && is_currently_supported(&after)
            })
        {
            return Ok(Op1Response::default());
        }

        let fen = Fen::from_setup(pos.clone().into_setup(EnPassantMode::Legal));
        self.client
            .get(&self.endpoint)
            .query(&[("fen", fen.to_string())])
            .send()
            .await?
            .error_for_status()?
            .json()
            .await
    }
}
