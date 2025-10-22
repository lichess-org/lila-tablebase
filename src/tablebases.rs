use std::{cmp::Reverse, sync::Arc};

use arrayvec::ArrayVec;
use shakmaty::{
    san::SanPlus,
    variant::{Antichess, Atomic, Chess, VariantPosition},
    KnownOutcome, Move, Outcome, Position as _,
};
use shakmaty_syzygy::{
    filesystem::Filesystem, Dtz, MaybeRounded, SyzygyError, Tablebase as SyzygyTablebase,
};
use tokio::{sync::Semaphore, task};
use tracing::info;

use crate::{
    antichess_tb,
    errors::TablebaseError,
    gaviota,
    op1::{Dtc, Op1Client, Op1Response},
    request::Op1Mode,
    response::{
        Category, MainlineResponse, MainlineStep, MoveInfo, PartialMoveInfo, PartialPositionInfo,
        PessimisticUnknown, TablebaseResponse,
    },
};

#[derive(Debug)]
pub struct Tablebases {
    pub standard: SyzygyTablebase<Chess>,
    pub atomic: SyzygyTablebase<Atomic>,
    pub antichess: SyzygyTablebase<Antichess>,
    pub op1: Option<Op1Client>,
    semaphore: Semaphore,
}

impl Tablebases {
    pub fn with_filesystem(fs: Arc<dyn Filesystem>) -> Tablebases {
        Tablebases {
            standard: SyzygyTablebase::with_filesystem(Arc::clone(&fs)),
            atomic: SyzygyTablebase::with_filesystem(Arc::clone(&fs)),
            antichess: SyzygyTablebase::with_filesystem(fs),
            op1: None,
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

    fn partial_position_info_blocking(
        &self,
        pos: &VariantPosition,
    ) -> Result<PartialPositionInfo, SyzygyError> {
        let (variant_win, variant_loss) = match pos.variant_outcome() {
            Outcome::Known(KnownOutcome::Decisive { winner }) => {
                (winner == pos.turn(), winner != pos.turn())
            }
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

        Ok(PartialPositionInfo {
            checkmate: pos.is_checkmate(),
            stalemate: pos.is_stalemate(),
            variant_win,
            variant_loss,
            insufficient_material: pos.is_insufficient_material(),
            halfmoves: pos.halfmoves(),
            dtz,
            dtm: unsafe { gaviota::probe_dtm(pos) },
            dtw: unsafe { antichess_tb::probe_dtw(pos) },
        })
    }

    pub async fn probe(
        &'static self,
        pos: VariantPosition,
        op1_mode: Op1Mode,
    ) -> Result<TablebaseResponse, TablebaseError> {
        let _permit = self
            .semaphore
            .acquire()
            .await
            .expect("semaphore not closed");
        info!("begin");

        let move_info_handles = pos
            .legal_moves()
            .into_iter()
            .map(|m| {
                let uci = m.to_uci(pos.castles().mode());

                let mut after = pos.clone();
                let san = SanPlus::from_move_and_play_unchecked(&mut after, m);

                task::spawn_blocking(move || {
                    self.partial_position_info_blocking(&after)
                        .map(|pos| PartialMoveInfo {
                            uci,
                            san,
                            capture: m.capture(),
                            promotion: m.promotion(),
                            zeroing: m.is_zeroing(),
                            conversion: m.is_conversion(),
                            pos,
                        })
                })
            })
            .collect::<ArrayVec<_, 256>>();

        let pos_info_handle = {
            let pos = pos.clone();
            task::spawn_blocking(move || self.partial_position_info_blocking(&pos))
        };

        let op1_response = match &self.op1 {
            Some(op1_client) => op1_client.probe_dtc(&pos, op1_mode).await?,
            None => Op1Response::default(),
        };

        let mut move_info = Vec::with_capacity(move_info_handles.len());
        for handle in move_info_handles {
            let partial = handle.await.expect("move info")?;
            let dtc = op1_response
                .children
                .get(&partial.uci)
                .copied()
                .unwrap_or_default();
            move_info.push(partial.with_dtc(dtc, pos.halfmoves()));
        }

        move_info.sort_unstable_by_key(|m: &MoveInfo| {
            (
                PessimisticUnknown(m.pos.category),
                (
                    Reverse(m.pos.checkmate),
                    Reverse(m.pos.variant_loss),
                    m.pos.variant_win,
                ),
                (
                    Reverse(m.pos.stalemate),
                    Reverse(m.pos.insufficient_material),
                ),
                if m.pos.category.is_negative() {
                    (
                        Reverse(m.pos.dtm.or(m.pos.dtw)),
                        Reverse(if m.conversion {
                            Some(Dtc(0))
                        } else {
                            m.pos.dtc
                        }),
                    )
                } else {
                    (Reverse(None), Reverse(None))
                },
                if m.pos.category.is_positive() {
                    (
                        m.pos.dtm.or(m.pos.dtw).map(Reverse),
                        if m.conversion {
                            Some(Reverse(Dtc(0)))
                        } else {
                            m.pos.dtc.map(Reverse)
                        },
                    )
                } else {
                    (None, None)
                },
                m.zeroing ^ !m.pos.category.is_positive(),
                m.pos.maybe_rounded_dtz.map(Reverse),
                (Reverse(m.capture), Reverse(m.promotion)),
            )
        });

        let mut pos_info = pos_info_handle
            .await
            .expect("pos info")?
            .with_dtc(op1_response.root, pos.halfmoves().saturating_sub(1));

        // Use category of previous position to infer syzygy-win / syzygy-loss,
        // if possible.
        for (prev, ambiguous, correct) in [
            (Category::Win, Category::SyzygyLoss, Category::Loss),
            (
                Category::CursedWin,
                Category::SyzygyLoss,
                Category::BlessedLoss,
            ),
            (
                Category::BlessedLoss,
                Category::SyzygyWin,
                Category::CursedWin,
            ),
            (Category::Loss, Category::SyzygyWin, Category::Win),
        ] {
            if pos_info.category == prev {
                for m in &mut move_info {
                    if m.pos.category != ambiguous {
                        break;
                    }
                    m.pos.category = correct;
                }
            }
        }
        if let Some(best_move) = move_info.first() {
            pos_info.category = -best_move.pos.category;
        }

        Ok(TablebaseResponse {
            pos: pos_info,
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
                        san: SanPlus::from_move_and_play_unchecked(&mut pos, m),
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
            winner: pos.outcome().winner().map(|winner| winner.char()),
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
