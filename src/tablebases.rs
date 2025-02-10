use std::cmp::Reverse;

use futures_util::{stream::FuturesUnordered, StreamExt as _};
use shakmaty::{
    san::SanPlus,
    variant::{Antichess, Atomic, Chess, VariantPosition},
    Move, Outcome, Position as _,
};
use shakmaty_syzygy::{aio::Tablebase, Dtz, MaybeRounded, SyzygyError};
use tokio::{join, task};
use tracing::info;

use crate::{
    antichess_tb,
    antichess_tb::Dtw,
    filesystem::{HotPrefixFilesystem, TokioUringFilesystem},
    gaviota,
    gaviota::Dtm,
    response::{
        Category, MainlineResponse, MainlineStep, MoveInfo, PessimisticUnknown, PositionInfo,
        TablebaseResponse,
    },
};

pub struct Tablebases {
    pub standard: Tablebase<Chess, HotPrefixFilesystem<TokioUringFilesystem>>,
    pub atomic: Tablebase<Atomic, HotPrefixFilesystem<TokioUringFilesystem>>,
    pub antichess: Tablebase<Antichess, HotPrefixFilesystem<TokioUringFilesystem>>,
}

impl Tablebases {
    pub fn with_filesystem(filesystem: HotPrefixFilesystem<TokioUringFilesystem>) -> Tablebases {
        Tablebases {
            standard: Tablebase::with_filesystem(filesystem.clone()),
            atomic: Tablebase::with_filesystem(filesystem.clone()),
            antichess: Tablebase::with_filesystem(filesystem),
        }
    }

    async fn probe_dtz(&self, pos: &VariantPosition) -> Result<MaybeRounded<Dtz>, SyzygyError> {
        match *pos {
            VariantPosition::Chess(ref pos) => self.standard.probe_dtz(pos).await,
            VariantPosition::Atomic(ref pos) => self.atomic.probe_dtz(pos).await,
            VariantPosition::Antichess(ref pos) => self.antichess.probe_dtz(pos).await,
            _ => unimplemented!("variant not supported"),
        }
    }

    async fn probe_dtm(&self, pos: &VariantPosition) -> Option<Dtm> {
        return None;
        let VariantPosition::Chess(ref pos) = *pos else {
            return None;
        };

        let pos = pos.clone();
        task::spawn_blocking(move || unsafe { gaviota::probe_dtm(&pos) })
            .await
            .expect("blocking probe_dtm")
    }

    async fn probe_dtw(&self, pos: &VariantPosition) -> Option<Dtw> {
        return None;
        let VariantPosition::Antichess(ref pos) = *pos else {
            return None;
        };

        let pos = pos.clone();
        task::spawn_blocking(move || unsafe { antichess_tb::probe_dtw(&pos) })
            .await
            .expect("blocking probe_dtw")
    }

    async fn best_move(
        &self,
        pos: &VariantPosition,
    ) -> Result<Option<(Move, MaybeRounded<Dtz>)>, SyzygyError> {
        match *pos {
            VariantPosition::Chess(ref pos) => self.standard.best_move(pos).await,
            VariantPosition::Atomic(ref pos) => self.atomic.best_move(pos).await,
            VariantPosition::Antichess(ref pos) => self.antichess.best_move(pos).await,
            _ => unimplemented!("variant not supported"),
        }
    }

    async fn position_info(&self, pos: &VariantPosition) -> Result<PositionInfo, SyzygyError> {
        let (variant_win, variant_loss) = match pos.variant_outcome() {
            Some(Outcome::Decisive { winner }) => (winner == pos.turn(), winner != pos.turn()),
            _ => (false, false),
        };

        let (dtz_result, dtm, dtw) = join!(
            self.probe_dtz(pos),
            self.probe_dtm(pos),
            self.probe_dtw(pos)
        );

        let dtz = match dtz_result {
            Err(
                SyzygyError::Castling
                | SyzygyError::TooManyPieces
                | SyzygyError::MissingTable { .. },
            ) => None, // user error
            Err(err) => return Err(err), // server error
            Ok(res) => Some(res),
        };

        Ok(PositionInfo {
            checkmate: pos.is_checkmate(),
            stalemate: pos.is_stalemate(),
            variant_win,
            variant_loss,
            insufficient_material: pos.is_insufficient_material(),
            maybe_rounded_dtz: dtz.map(MaybeRounded::ignore_rounding),
            precise_dtz: dtz.and_then(MaybeRounded::precise),
            dtz,
            dtm,
            dtw,
            halfmoves: pos.halfmoves(),
        })
    }

    pub async fn probe(
        &'static self,
        pos: VariantPosition,
    ) -> Result<TablebaseResponse, SyzygyError> {
        info!("begin");

        let halfmoves = pos.halfmoves();

        let move_info: FuturesUnordered<_> = pos
            .legal_moves()
            .into_iter()
            .map(|m| {
                let uci = m.to_uci(pos.castles().mode());
                let mut after = pos.clone();
                let san = SanPlus::from_move_and_play_unchecked(&mut after, &m);
                async move {
                    let after_info = self.position_info(&after).await?;
                    Ok(MoveInfo {
                        uci,
                        san,
                        capture: m.capture(),
                        promotion: m.promotion(),
                        zeroing: m.is_zeroing(),
                        category: after_info.category(halfmoves),
                        pos: after_info,
                    })
                }
            })
            .collect();

        let (pos_info, move_info) = join!(self.position_info(&pos), move_info.collect::<Vec<_>>());
        let pos_info = pos_info?;
        let mut move_info = move_info.into_iter().collect::<Result<Vec<_>, _>>()?;

        move_info.sort_by_key(|m: &MoveInfo| {
            (
                PessimisticUnknown(m.category),
                (
                    Reverse(m.pos.checkmate),
                    Reverse(m.pos.variant_loss),
                    m.pos.variant_win,
                ),
                (
                    Reverse(m.pos.stalemate),
                    Reverse(m.pos.insufficient_material),
                ),
                if m.pos
                    .dtz
                    .unwrap_or(MaybeRounded::Precise(Dtz(0)))
                    .is_negative()
                {
                    Reverse(m.pos.dtm.map(i32::from).or(m.pos.dtw.map(i32::from)))
                } else {
                    Reverse(None)
                },
                if m.pos
                    .dtz
                    .unwrap_or(MaybeRounded::Precise(Dtz(0)))
                    .is_positive()
                {
                    m.pos
                        .dtm
                        .map(i32::from)
                        .or(m.pos.dtw.map(i32::from))
                        .map(Reverse)
                } else {
                    None
                },
                m.zeroing
                    ^ !m.pos
                        .dtz
                        .unwrap_or(MaybeRounded::Precise(Dtz(0)))
                        .is_positive(),
                m.capture.is_some()
                    ^ !m.pos
                        .dtz
                        .unwrap_or(MaybeRounded::Precise(Dtz(0)))
                        .is_positive(),
                m.pos.maybe_rounded_dtz.map(Reverse),
                (Reverse(m.capture), Reverse(m.promotion)),
            )
        });

        let category = pos_info.category(halfmoves.saturating_sub(1));

        // Use category of previous position to infer maybe-win / maybe-loss,
        // if possible.
        for (prev, maybe, correct) in [
            (Category::Win, Category::MaybeLoss, Category::Loss),
            (
                Category::CursedWin,
                Category::MaybeLoss,
                Category::BlessedLoss,
            ),
            (
                Category::BlessedLoss,
                Category::MaybeWin,
                Category::CursedWin,
            ),
            (Category::Loss, Category::MaybeWin, Category::Win),
        ] {
            if category == prev {
                for m in &mut move_info {
                    if m.category != maybe {
                        break;
                    }
                    m.category = correct;
                }
            }
        }

        Ok(TablebaseResponse {
            pos: pos_info,
            category: move_info.first().map_or(category, |m| -m.category),
            moves: move_info,
        })
    }

    pub async fn mainline(
        &self,
        mut pos: VariantPosition,
    ) -> Result<MainlineResponse, SyzygyError> {
        info!("begin");

        let dtz = self.probe_dtz(&pos).await?;
        let mut mainline = Vec::new();

        if !dtz.is_zero() {
            while pos.halfmoves() < 100 {
                if let Some((m, dtz)) = self.best_move(&pos).await? {
                    mainline.push(MainlineStep {
                        uci: m.to_uci(pos.castles().mode()),
                        dtz: dtz.ignore_rounding(),
                        precise_dtz: dtz.precise(),
                        san: SanPlus::from_move_and_play_unchecked(&mut pos, &m),
                    });
                } else {
                    break;
                }
            }
        }

        Ok(MainlineResponse {
            dtz: dtz.ignore_rounding(),
            precise_dtz: dtz.precise(),
            mainline,
            winner: pos
                .outcome()
                .and_then(|o| o.winner())
                .map(|winner| winner.char()),
        })
    }
}
