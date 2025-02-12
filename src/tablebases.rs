use std::{cmp::Reverse, sync::Arc};

use shakmaty::{
    san::SanPlus,
    variant::{Antichess, Atomic, Chess, VariantPosition},
    Move, Outcome, Position as _,
};
use shakmaty_syzygy::{
    filesystem::Filesystem, Dtz, MaybeRounded, SyzygyError, Tablebase as SyzygyTablebase,
};
use tokio::{sync::Semaphore, task};
use tracing::info;

use crate::{
    antichess_tb, gaviota,
    response::{
        Category, MainlineResponse, MainlineStep, MoveInfo, PessimisticUnknown, PositionInfo,
        TablebaseResponse,
    },
};

#[derive(Debug)]
pub struct Tablebases {
    pub standard: SyzygyTablebase<Chess>,
    pub atomic: SyzygyTablebase<Atomic>,
    pub antichess: SyzygyTablebase<Antichess>,
    semaphore: Semaphore,
}

impl Tablebases {
    pub fn with_filesystem(fs: Arc<dyn Filesystem>) -> Tablebases {
        Tablebases {
            standard: SyzygyTablebase::with_filesystem(Arc::clone(&fs)),
            atomic: SyzygyTablebase::with_filesystem(Arc::clone(&fs)),
            antichess: SyzygyTablebase::with_filesystem(fs),
            semaphore: Semaphore::new(256),
        }
    }

    fn probe_dtz_blocking(&self, pos: &VariantPosition) -> Result<MaybeRounded<Dtz>, SyzygyError> {
        match *pos {
            VariantPosition::Chess(ref pos) => self.standard.probe_dtz(pos),
            VariantPosition::Atomic(ref pos) => self.atomic.probe_dtz(pos),
            VariantPosition::Antichess(ref pos) => self.antichess.probe_dtz(pos),
            _ => unimplemented!("variant not supported"),
        }
    }

    fn best_move_blocking(
        &self,
        pos: &VariantPosition,
    ) -> Result<Option<(Move, MaybeRounded<Dtz>)>, SyzygyError> {
        match *pos {
            VariantPosition::Chess(ref pos) => self.standard.best_move(pos),
            VariantPosition::Atomic(ref pos) => self.atomic.best_move(pos),
            VariantPosition::Antichess(ref pos) => self.antichess.best_move(pos),
            _ => unimplemented!("variant not supported"),
        }
    }

    fn position_info_blocking(&self, pos: &VariantPosition) -> Result<PositionInfo, SyzygyError> {
        let (variant_win, variant_loss) = match pos.variant_outcome() {
            Some(Outcome::Decisive { winner }) => (winner == pos.turn(), winner != pos.turn()),
            _ => (false, false),
        };

        let dtz = match self.probe_dtz_blocking(pos) {
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
            dtm: unsafe { gaviota::probe_dtm(pos) },
            dtw: unsafe { antichess_tb::probe_dtw(pos) },
            halfmoves: pos.halfmoves(),
        })
    }

    pub async fn probe(
        &'static self,
        pos: VariantPosition,
    ) -> Result<TablebaseResponse, SyzygyError> {
        let _permit = self
            .semaphore
            .acquire()
            .await
            .expect("semaphore not closed");
        info!("begin");

        let halfmoves = pos.halfmoves();

        let move_info_handles = pos
            .legal_moves()
            .into_iter()
            .map(|m| {
                let uci = m.to_uci(pos.castles().mode());

                let mut after = pos.clone();
                let san = SanPlus::from_move_and_play_unchecked(&mut after, &m);

                task::spawn_blocking(move || {
                    let after_info = self.position_info_blocking(&after)?;

                    Ok(MoveInfo {
                        uci,
                        san,
                        capture: m.capture(),
                        promotion: m.promotion(),
                        zeroing: m.is_zeroing(),
                        category: after_info.category(halfmoves),
                        pos: after_info,
                    })
                })
            })
            .collect::<Vec<_>>();

        let pos_info_handle = task::spawn_blocking(move || self.position_info_blocking(&pos));

        let mut move_info = Vec::with_capacity(move_info_handles.len());
        for handle in move_info_handles {
            move_info.push(handle.await.expect("move info")?);
        }
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
                    Reverse(m.pos.dtm.or(m.pos.dtw))
                } else {
                    Reverse(None)
                },
                if m.pos
                    .dtz
                    .unwrap_or(MaybeRounded::Precise(Dtz(0)))
                    .is_positive()
                {
                    m.pos.dtm.or(m.pos.dtw).map(Reverse)
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

        let pos_info = pos_info_handle.await.expect("pos info")?;
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

    fn mainline_blocking(&self, mut pos: VariantPosition) -> Result<MainlineResponse, SyzygyError> {
        let dtz = self.probe_dtz_blocking(&pos)?;
        let mut mainline = Vec::new();

        if !dtz.is_zero() {
            while pos.halfmoves() < 100 {
                if let Some((m, dtz)) = self.best_move_blocking(&pos)? {
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

    pub async fn mainline(
        &'static self,
        pos: VariantPosition,
    ) -> Result<MainlineResponse, SyzygyError> {
        let _permit = self
            .semaphore
            .acquire()
            .await
            .expect("semaphore not closed");
        info!("begin");

        task::spawn_blocking(move || self.mainline_blocking(pos))
            .await
            .expect("mainline blocking")
    }
}
