use std::{
    cmp::{Ordering, Reverse},
    ops::Neg,
};

use serde::Serialize;
use serde_with::{serde_as, DisplayFromStr, FromInto};
use shakmaty::{san::SanPlus, uci::UciMove, Role};
use shakmaty_syzygy::{AmbiguousWdl, Dtz, MaybeRounded};

use crate::metric::{tightening_metric_sort_key, Dtc, Dtw};

#[derive(Serialize, Debug, Clone)]
pub struct TablebaseResponse {
    #[serde(flatten)]
    pub pos: PositionInfo,
    pub moves: Vec<MoveInfo>,
}

#[serde_as]
#[derive(Serialize, Debug, Clone)]
pub struct MoveInfo {
    pub uci: UciMove,
    pub san: SanPlus,
    pub zeroing: bool,
    pub conversion: bool,
    #[serde(skip)]
    pub capture: Option<Role>,
    #[serde(skip)]
    pub promotion: Option<Role>,
    #[serde(flatten)]
    pub pos: PositionInfo,
}

impl MoveInfo {
    pub fn sort_key(&self) -> impl Ord {
        (
            PessimisticUnknown(self.pos.category),
            (
                Reverse(self.pos.checkmate),
                Reverse(self.pos.variant_loss),
                self.pos.variant_win,
            ),
            (
                Reverse(self.pos.stalemate),
                Reverse(self.pos.insufficient_material),
            ),
            tightening_metric_sort_key(self.pos.dtm.or(self.pos.dtw).map(i32::from)),
            (
                self.conversion ^ !self.pos.category.is_positive(),
                tightening_metric_sort_key(self.pos.dtc.map(i32::from)),
            ),
            (
                self.zeroing ^ !self.pos.category.is_positive(),
                self.pos.maybe_rounded_dtz.map(Reverse),
            ),
            (Reverse(self.capture), Reverse(self.promotion)),
        )
    }
}

pub struct PartialMoveInfo {
    pub uci: UciMove,
    pub san: SanPlus,
    pub zeroing: bool,
    pub conversion: bool,
    pub capture: Option<Role>,
    pub promotion: Option<Role>,
    pub pos: PartialPositionInfo,
}

impl PartialMoveInfo {
    pub fn with_dtc(self, dtc: Option<Dtc>, halfmoves_before: u32) -> MoveInfo {
        MoveInfo {
            uci: self.uci,
            san: self.san,
            zeroing: self.zeroing,
            conversion: self.conversion,
            capture: self.capture,
            promotion: self.promotion,
            pos: self.pos.with_dtc(dtc, halfmoves_before),
        }
    }
}

#[serde_as]
#[derive(Serialize, Debug, Clone)]
pub struct PositionInfo {
    pub checkmate: bool,
    pub stalemate: bool,
    pub variant_win: bool,
    pub variant_loss: bool,
    pub insufficient_material: bool,
    #[serde_as(as = "Option<FromInto<i32>>")]
    #[serde(rename = "dtz")]
    pub maybe_rounded_dtz: Option<Dtz>,
    #[serde_as(as = "Option<FromInto<i32>>")]
    pub precise_dtz: Option<Dtz>,
    pub dtm: Option<Dtw>,
    pub dtw: Option<Dtw>,
    pub dtc: Option<Dtc>,
    pub category: Category,
}

pub struct PartialPositionInfo {
    pub checkmate: bool,
    pub stalemate: bool,
    pub variant_win: bool,
    pub variant_loss: bool,
    pub insufficient_material: bool,
    pub halfmoves: u32,
    pub dtz: Option<MaybeRounded<Dtz>>,
    pub dtm: Option<Dtw>,
    pub dtw: Option<Dtw>,
    pub zeroing_is_conversion: bool,
}

impl PartialPositionInfo {
    pub fn with_dtc(self, dtc: Option<Dtc>, halfmoves_before: u32) -> PositionInfo {
        PositionInfo {
            category: {
                if !self.variant_win
                    && !self.variant_loss
                    && (self.stalemate
                        || self.insufficient_material
                        || self.dtz.is_some_and(|dtz| dtz.is_zero())
                        || dtc.is_some_and(|dtc| dtc.is_zero()))
                {
                    Category::Draw
                } else if self.checkmate || self.variant_loss {
                    if halfmoves_before < 100 {
                        Category::Loss
                    } else {
                        Category::BlessedLoss
                    }
                } else if self.variant_win {
                    if halfmoves_before < 100 {
                        Category::Win
                    } else {
                        Category::CursedWin
                    }
                } else if let Some(dtz) = self.dtz {
                    if halfmoves_before < 100 {
                        match AmbiguousWdl::from_dtz_and_halfmoves(dtz, self.halfmoves) {
                            AmbiguousWdl::Win => Category::Win,
                            AmbiguousWdl::MaybeWin => Category::SyzygyWin,
                            AmbiguousWdl::CursedWin => Category::CursedWin,
                            AmbiguousWdl::Draw => Category::Draw,
                            AmbiguousWdl::BlessedLoss => Category::BlessedLoss,
                            AmbiguousWdl::MaybeLoss => Category::SyzygyLoss,
                            AmbiguousWdl::Loss => Category::Loss,
                        }
                    } else if dtz.is_negative() {
                        Category::BlessedLoss
                    } else {
                        Category::CursedWin
                    }
                } else if let Some(dtc) = dtc {
                    if halfmoves_before < 100 {
                        match dtc
                            .assume_zeroing_is_conversion()
                            .add_plies_saturating(self.halfmoves)
                        {
                            Dtz(n) if n < -100 => {
                                if self.zeroing_is_conversion {
                                    Category::BlessedLoss
                                } else {
                                    Category::MaybeLoss
                                }
                            }
                            Dtz(-100) => Category::MaybeLoss,
                            Dtz(n) if n < 0 => Category::MaybeLoss, // Unknown due to later phase
                            Dtz(0) => Category::Draw,
                            Dtz(n) if n < 100 => Category::MaybeWin, // Unknown due to later phase
                            Dtz(100) => Category::MaybeWin,
                            Dtz(_) => {
                                if self.zeroing_is_conversion {
                                    Category::CursedWin
                                } else {
                                    Category::MaybeWin
                                }
                            }
                        }
                    } else if dtc.is_negative() {
                        Category::BlessedLoss
                    } else {
                        Category::CursedWin
                    }
                } else {
                    Category::Unknown
                }
            },
            checkmate: self.checkmate,
            stalemate: self.stalemate,
            variant_win: self.variant_win,
            variant_loss: self.variant_loss,
            insufficient_material: self.insufficient_material,
            maybe_rounded_dtz: self.dtz.map(MaybeRounded::ignore_rounding),
            precise_dtz: self.dtz.and_then(MaybeRounded::precise),
            dtm: self.dtm,
            dtw: self.dtw,
            dtc,
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum Category {
    Loss,
    Unknown,
    SyzygyLoss,
    MaybeLoss,
    BlessedLoss,
    Draw,
    CursedWin,
    MaybeWin,
    SyzygyWin,
    Win,
}

impl Category {
    pub fn is_positive(&self) -> bool {
        matches!(
            self,
            Category::CursedWin | Category::MaybeWin | Category::SyzygyWin | Category::Win
        )
    }
}

impl Neg for Category {
    type Output = Category;

    fn neg(self) -> Category {
        match self {
            Category::Loss => Category::Win,
            Category::Unknown => Category::Unknown,
            Category::SyzygyLoss => Category::SyzygyWin,
            Category::MaybeLoss => Category::MaybeWin,
            Category::BlessedLoss => Category::CursedWin,
            Category::Draw => Category::Draw,
            Category::CursedWin => Category::BlessedLoss,
            Category::MaybeWin => Category::MaybeLoss,
            Category::SyzygyWin => Category::SyzygyLoss,
            Category::Win => Category::Loss,
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct PessimisticUnknown(pub Category); // loss < unknown < maybe-loss < ...

impl Ord for PessimisticUnknown {
    fn cmp(&self, other: &PessimisticUnknown) -> Ordering {
        (self.0 as u32).cmp(&(other.0 as u32))
    }
}

impl PartialOrd for PessimisticUnknown {
    fn partial_cmp(&self, other: &PessimisticUnknown) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[serde_as]
#[derive(Serialize, Debug)]
pub struct MainlineResponse {
    pub mainline: Vec<MainlineStep>,
    pub winner: Option<char>,
    #[serde_as(as = "FromInto<i32>")]
    pub dtz: Dtz,
    #[serde_as(as = "Option<FromInto<i32>>")]
    pub precise_dtz: Option<Dtz>,
}

#[serde_as]
#[derive(Serialize, Debug)]
pub struct MainlineStep {
    #[serde_as(as = "DisplayFromStr")]
    pub uci: UciMove,
    #[serde_as(as = "DisplayFromStr")]
    pub san: SanPlus,
    #[serde_as(as = "FromInto<i32>")]
    pub dtz: Dtz,
    #[serde_as(as = "Option<FromInto<i32>>")]
    pub precise_dtz: Option<Dtz>,
}
