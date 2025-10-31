use std::cmp::Reverse;

use serde::{Deserialize, Serialize};
use shakmaty_syzygy::Dtz;

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

    pub fn assume_zeroing_is_conversion(self) -> Dtz {
        Dtz(self.0.saturating_mul(2))
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
