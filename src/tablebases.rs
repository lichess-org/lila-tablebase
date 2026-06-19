use arrayvec::ArrayVec;
use futures_util::{StreamExt as _, stream::FuturesUnordered};
use shakmaty::{
    KnownOutcome, Move, Outcome, Position as _,
    san::SanPlus,
    variant::{Antichess, Atomic, Chess, VariantPosition},
};
use shakmaty_syzygy::{Dtz, MaybeRounded, SyzygyError, aio::Tablebase as SyzygyTablebase};
use tokio::{join, task};
use tracing::info;

use crate::{
    antichess_tb,
    errors::TablebaseError,
    filesystem::{HotPrefixFilesystem, TokioUringFilesystem},
    op1::{Op1Client, Op1Response, zeroing_is_conversion},
    prophet,
    request::Op1Mode,
    response::{
        Category, MainlineResponse, MainlineStep, MoveInfo, PartialMoveInfo, PartialPositionInfo,
        TablebaseResponse,
    },
};

/// The Syzygy filesystem backing each tablebase, bound to one `tokio-uring`
/// runtime.
type Fs = HotPrefixFilesystem<TokioUringFilesystem>;

pub struct Tablebases {
    pub standard: SyzygyTablebase<Chess, Fs>,
    pub atomic: SyzygyTablebase<Atomic, Fs>,
    pub antichess: SyzygyTablebase<Antichess, Fs>,
    pub op1: Option<Op1Client>,
}

impl Tablebases {
    pub fn with_filesystem(fs: Fs) -> Tablebases {
        Tablebases {
            standard: SyzygyTablebase::with_filesystem(fs.clone()),
            atomic: SyzygyTablebase::with_filesystem(fs.clone()),
            antichess: SyzygyTablebase::with_filesystem(fs),
            op1: None,
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

    /// DTM from the (blocking, FFI) Prophet tables, off the uring thread.
    async fn probe_dtm(pos: &VariantPosition) -> Option<crate::metric::Dtw> {
        let pos = pos.clone();
        task::spawn_blocking(move || unsafe { prophet::probe_dtm(&pos) })
            .await
            .expect("blocking probe_dtm")
    }

    /// DTW from the (blocking, FFI) antichess tables, off the uring thread.
    async fn probe_dtw(pos: &VariantPosition) -> Option<crate::metric::Dtw> {
        let pos = pos.clone();
        task::spawn_blocking(move || unsafe { antichess_tb::probe_dtw(&pos) })
            .await
            .expect("blocking probe_dtw")
    }

    async fn partial_position_info(
        &self,
        pos: &VariantPosition,
    ) -> Result<PartialPositionInfo, SyzygyError> {
        let (variant_win, variant_loss) = match pos.variant_outcome() {
            Outcome::Known(KnownOutcome::Decisive { winner }) => {
                (winner == pos.turn(), winner != pos.turn())
            }
            _ => (false, false),
        };

        // Probe Syzygy (async, io_uring) and the FFI metrics concurrently.
        let (dtz_result, dtm, dtw) = join!(
            self.probe_dtz(pos),
            Self::probe_dtm(pos),
            Self::probe_dtw(pos),
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

        Ok(PartialPositionInfo {
            checkmate: pos.is_checkmate(),
            stalemate: pos.is_stalemate(),
            variant_win,
            variant_loss,
            insufficient_material: pos.is_insufficient_material(),
            halfmoves: pos.halfmoves(),
            dtz,
            dtm,
            dtw,
            zeroing_is_conversion: zeroing_is_conversion(pos.board()),
        })
    }

    pub async fn probe(
        &'static self,
        pos: VariantPosition,
        op1_mode: Op1Mode,
    ) -> Result<TablebaseResponse, TablebaseError> {
        info!("begin");

        // Probe every successor position concurrently.
        let move_infos: FuturesUnordered<_> = pos
            .legal_moves()
            .into_iter()
            .map(|m| {
                let uci = m.to_uci(pos.castles().mode());
                let mut after = pos.clone();
                let san = SanPlus::from_move_and_play_unchecked(&mut after, m);
                async move {
                    self.partial_position_info(&after)
                        .await
                        .map(|info| PartialMoveInfo {
                            uci,
                            san,
                            capture: m.capture(),
                            promotion: m.promotion(),
                            zeroing: m.is_zeroing(),
                            conversion: m.is_conversion(),
                            pos: info,
                        })
                }
            })
            .collect();

        let op1 = async {
            match &self.op1 {
                Some(op1_client) => op1_client.probe_dtc(&pos, op1_mode).await,
                None => Ok(Op1Response::default()),
            }
        };

        // Run successor probes, the root probe and the upstream OP1 request all
        // concurrently.
        let (partials, pos_partial, op1_response) = join!(
            move_infos.collect::<ArrayVec<_, 256>>(),
            self.partial_position_info(&pos),
            op1,
        );
        let pos_partial = pos_partial?;
        let op1_response = op1_response?;

        let mut move_info = Vec::with_capacity(partials.len());
        for partial in partials {
            let partial = partial?;
            let dtc = op1_response
                .children
                .get(&partial.uci)
                .copied()
                .unwrap_or_default();
            move_info.push(partial.with_dtc(dtc, pos.halfmoves()));
        }

        move_info.sort_unstable_by_key(MoveInfo::sort_key);

        let mut pos_info = pos_partial
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

    pub async fn mainline(
        &'static self,
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
}
