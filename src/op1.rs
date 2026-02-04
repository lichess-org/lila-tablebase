use std::{collections::HashMap, time::Duration};

use serde::Deserialize;
use shakmaty::{
    Board, Chess, Color, EnPassantMode, Position, Rank, fen::Fen, uci::UciMove,
    variant::VariantPosition,
};

use crate::{metric::Dtc, request::Op1Mode};

#[derive(Deserialize, Debug, Default)]
pub struct Op1Response {
    pub root: Option<Dtc>,
    pub children: HashMap<UciMove, Option<Dtc>>,
}

#[derive(Debug)]
pub struct Op1Client {
    endpoint: String,
    client: reqwest::Client,
}

fn is_op1(board: &Board) -> bool {
    let white_pawns = board.white() & board.pawns();
    let white_pawn_paths = white_pawns.shift(8)
        | white_pawns.shift(16)
        | white_pawns.shift(24)
        | white_pawns.shift(32)
        | white_pawns.shift(40);
    (white_pawn_paths & board.black() & board.pawns()).any()
}

fn more_than_lone_pawn(board: &Board, color: Color) -> bool {
    board.by_piece(color.pawn()).more_than_one()
        || board
            .by_color(color)
            .without(board.kings())
            .without(board.pawns())
            .any()
}

fn use_op1(pos: &Chess, op1_mode: Op1Mode) -> bool {
    if op1_mode == Op1Mode::Never || pos.castles().any() {
        false
    } else if pos.board().occupied().count() < 8 {
        op1_mode == Op1Mode::Always
    } else if pos.board().occupied().count() == 8 {
        is_op1(pos.board())
            && more_than_lone_pawn(pos.board(), Color::White)
            && more_than_lone_pawn(pos.board(), Color::Black)
    } else {
        false
    }
}

pub fn zeroing_is_conversion(board: &Board) -> bool {
    // All pawns are blocked or about to promote, so that each zeroing
    // move is also a conversion (DTC and DTZ coincide).
    Color::ALL.into_iter().all(|color| {
        (board.by_color(color) & board.pawns())
            .shift(color.fold_wb(8, -8))
            .is_subset(board.pawns() | color.relative_rank(Rank::Eighth))
    })
}

impl Op1Client {
    pub fn new(endpoint: &str) -> Op1Client {
        Op1Client {
            endpoint: endpoint.to_owned(),
            client: reqwest::Client::builder()
                .user_agent("lila-tablebase")
                .timeout(Duration::from_secs(10))
                .build()
                .expect("op1 client"),
        }
    }

    pub async fn probe_dtc(
        &self,
        pos: &VariantPosition,
        op1_mode: Op1Mode,
    ) -> Result<Op1Response, reqwest::Error> {
        let VariantPosition::Chess(pos) = pos else {
            return Ok(Op1Response::default());
        };

        let min_pieces = match op1_mode {
            Op1Mode::Never => return Ok(Op1Response::default()),
            Op1Mode::Always => 0,
            Op1Mode::Auxiliary => 8,
        };

        if pos.board().occupied().count() < min_pieces || pos.board().occupied().count() > 9 {
            return Ok(Op1Response::default());
        }

        if !use_op1(pos, op1_mode)
            && pos.legal_moves().into_iter().all(|m| {
                let mut after = pos.clone();
                after.play_unchecked(m);
                !use_op1(&after, op1_mode)
            })
        {
            return Ok(Op1Response::default());
        }

        self.client
            .get(&self.endpoint)
            .query(&[(
                "fen",
                Fen::from_position(pos, EnPassantMode::Legal).to_string(),
            )])
            .send()
            .await?
            .error_for_status()?
            .json()
            .await
    }
}

#[cfg(test)]
mod tests {
    use shakmaty::CastlingMode;

    use super::*;

    #[test]
    fn test_use_op1() {
        for (fen, mode, expectation) in [
            // 8-piece op1 position
            (
                "R7/8/8/8/7q/2K1B2p/7P/2Bk4 w - - 0 1",
                Op1Mode::Always,
                true,
            ),
            (
                "R7/8/8/8/7q/2K1B2p/7P/2Bk4 w - - 0 1",
                Op1Mode::Auxiliary,
                true,
            ),
            (
                "R7/8/8/8/7q/2K1B2p/7P/2Bk4 w - - 0 1",
                Op1Mode::Never,
                false,
            ),
            // 7-piece arbitrary position
            (
                "QN4n1/6r1/3k4/8/b2K4/8/8/8 b - - 0 1",
                Op1Mode::Always,
                true,
            ),
            (
                "QN4n1/6r1/3k4/8/b2K4/8/8/8 b - - 0 1",
                Op1Mode::Auxiliary,
                false,
            ),
            // 8-piece positions with weak side
            (
                "4k3/4p3/8/8/8/8/3PPPPP/4K3 w - - 0 1",
                Op1Mode::Always,
                false,
            ),
            (
                "4k3/4p3/8/8/8/8/3PPPPP/4K3 w - - 0 1",
                Op1Mode::Auxiliary,
                false,
            ),
        ] {
            let pos = fen
                .parse::<Fen>()
                .expect("valid fen")
                .into_position::<Chess>(CastlingMode::Chess960)
                .expect("legal fen");
            assert_eq!(use_op1(&pos, mode), expectation, "fen: {}", fen);
        }
    }

    #[test]
    fn test_zeroing_is_conversion() {
        for (fen, expectation) in [
            ("4k3/8/p7/P7/8/8/8/4K3", true),
            ("4k3/8/p7/P7/P7/P7/8/4K3", true),
            ("4k3/8/p7/P7/8/P7/8/4K3 w - - 0 1", false),
            ("4k3/p5PP/Pp6/1P2p3/3pPp2/3PPP2/pn2P2p/4K3", true),
            ("4k3/p5PP/Pp6/1P2p3/3pPp2/3PPP1p/pn2P3/4K3", false),
            ("4k3/p5PP/Pp6/1P6/3pPp2/3PPP1p/pn2P2P/4K3", false),
        ] {
            let board = fen.parse::<Fen>().expect("valid fen").into_setup().board;
            assert_eq!(zeroing_is_conversion(&board), expectation, "fen: {}", fen);
        }
    }
}
