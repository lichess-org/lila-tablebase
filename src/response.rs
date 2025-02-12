use std::{cmp::Ordering, ops::Neg};

use serde::Serialize;
use serde_with::{serde_as, DisplayFromStr, FromInto};
use shakmaty::{san::SanPlus, uci::UciMove, Role};
use shakmaty_syzygy::{AmbiguousWdl, Dtz, MaybeRounded};

use crate::{antichess_tb::Dtw, gaviota::Dtm};

#[derive(Serialize, Debug, Clone)]
pub struct TablebaseResponse {
    #[serde(flatten)]
    pub pos: PositionInfo,
    pub category: Category,
    pub moves: Vec<MoveInfo>,
}

#[serde_as]
#[derive(Serialize, Debug, Clone)]
pub struct MoveInfo {
    #[serde_as(as = "DisplayFromStr")]
    pub uci: UciMove,
    #[serde_as(as = "DisplayFromStr")]
    pub san: SanPlus,
    pub zeroing: bool,
    #[serde(skip)]
    pub capture: Option<Role>,
    #[serde(skip)]
    pub promotion: Option<Role>,
    #[serde(flatten)]
    pub pos: PositionInfo,
    pub category: Category,
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
    #[serde(skip)]
    pub dtz: Option<MaybeRounded<Dtz>>,
    #[serde_as(as = "Option<FromInto<i32>>")]
    pub dtm: Option<Dtm>,
    #[serde_as(as = "Option<FromInto<i32>>")]
    pub dtw: Option<Dtw>,
    #[serde(skip)]
    pub halfmoves: u32,
}

impl PositionInfo {
    pub fn category(&self, halfmoves_before: u32) -> Category {
        if !self.variant_win
            && !self.variant_loss
            && (self.stalemate
                || self.insufficient_material
                || self.dtz.is_some_and(|dtz| dtz.is_zero()))
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
                    AmbiguousWdl::MaybeWin => Category::MaybeWin,
                    AmbiguousWdl::CursedWin => Category::CursedWin,
                    AmbiguousWdl::Draw => Category::Draw,
                    AmbiguousWdl::BlessedLoss => Category::BlessedLoss,
                    AmbiguousWdl::MaybeLoss => Category::MaybeLoss,
                    AmbiguousWdl::Loss => Category::Loss,
                }
            } else if dtz.is_negative() {
                Category::BlessedLoss
            } else {
                Category::CursedWin
            }
        } else {
            Category::Unknown
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum Category {
    Loss,
    Unknown,
    MaybeLoss,
    BlessedLoss,
    Draw,
    CursedWin,
    MaybeWin,
    Win,
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

impl Neg for Category {
    type Output = Category;

    fn neg(self) -> Category {
        match self {
            Category::Loss => Category::Win,
            Category::Unknown => Category::Unknown,
            Category::MaybeLoss => Category::MaybeWin,
            Category::BlessedLoss => Category::CursedWin,
            Category::Draw => Category::Draw,
            Category::CursedWin => Category::BlessedLoss,
            Category::MaybeWin => Category::MaybeLoss,
            Category::Win => Category::Loss,
        }
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
