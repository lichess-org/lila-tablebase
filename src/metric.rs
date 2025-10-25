use std::cmp::Reverse;

use serde::{Deserialize, Serialize};

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
    pub fn add_moves_saturating(self, moves: u32) -> Dtc {
        match self {
            Dtc(0) => Dtc(0),
            Dtc(n) if n > 0 => i32::try_from(moves)
                .ok()
                .and_then(|moves| n.checked_add(moves))
                .map_or(Dtc(i32::MAX), Dtc),
            Dtc(n) => i32::try_from(moves)
                .ok()
                .and_then(|moves| n.checked_sub(moves))
                .map_or(Dtc(i32::MIN), Dtc),
        }
    }
}

impl From<Dtc> for i32 {
    #[inline]
    fn from(dtc: Dtc) -> Self {
        dtc.0
    }
}

#[derive(Debug, Serialize, Deserialize, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Dtw(pub i32);

impl From<Dtw> for i32 {
    #[inline]
    fn from(dtc: Dtw) -> Self {
        dtc.0
    }
}

pub fn tightening_metric_sort_key(metric: Option<i32>) -> impl Ord {
    (
        Reverse(metric.filter(|m| *m < 0)),
        metric.filter(|m| *m > 0).map(Reverse),
    )
}

#[cfg(test)]
mod tests {
    use crate::metric::tightening_metric_sort_key;

    #[test]
    fn test_tightening_metric_sort_key() {
        assert!(&[Some(-1), Some(-2), None, Some(0), Some(2), Some(1)]
            .is_sorted_by_key(|m| tightening_metric_sort_key(*m)));
    }
}
