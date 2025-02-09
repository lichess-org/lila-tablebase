use std::{
    ffi::CString,
    os::raw::{c_int, c_uchar, c_uint},
    path::PathBuf,
};

use arrayvec::ArrayVec;
use shakmaty::{Chess, EnPassantMode, Position as _};
use tracing::warn;

#[derive(Debug, Copy, Clone)]
pub struct Dtm(pub i32);

impl From<Dtm> for i32 {
    #[inline]
    fn from(Dtm(dtm): Dtm) -> i32 {
        dtm
    }
}

impl From<i32> for Dtm {
    #[inline]
    fn from(dtm: i32) -> Self {
        Dtm(dtm)
    }
}

pub unsafe fn init(directories: &[PathBuf]) {
    unsafe {
        assert_ne!(
            gaviota_sys::tbcache_init(1014 * 1024, 50),
            0,
            "tbcache_init"
        );

        let mut paths = gaviota_sys::tbpaths_init();
        assert!(!paths.is_null());

        for path in directories {
            let path = CString::new(path.as_os_str().to_str().unwrap()).unwrap();
            paths = gaviota_sys::tbpaths_add(paths, path.as_ptr());
            assert!(!paths.is_null(), "tbpaths_add");
            drop(path);
        }

        assert!(
            !gaviota_sys::tb_init(
                1,
                gaviota_sys::TB_compression_scheme::tb_CP4 as c_int,
                paths
            )
            .is_null(),
            "tb_init"
        );
    }
}

pub unsafe fn probe_dtm(pos: &Chess) -> Option<Dtm> {
    if pos.board().occupied().count() > 5 || pos.castles().any() {
        return None;
    }

    let mut ws = ArrayVec::<c_uint, 6>::new();
    let mut bs = ArrayVec::<c_uint, 6>::new();
    let mut wp = ArrayVec::<c_uchar, 6>::new();
    let mut bp = ArrayVec::<c_uchar, 6>::new();

    for (sq, piece) in pos.board().clone() {
        piece.color.fold_wb(&mut ws, &mut bs).push(c_uint::from(sq));
        piece
            .color
            .fold_wb(&mut wp, &mut bp)
            .push(c_uchar::from(piece.role));
    }

    ws.push(gaviota_sys::TB_squares::tb_NOSQUARE as c_uint);
    bs.push(gaviota_sys::TB_squares::tb_NOSQUARE as c_uint);
    wp.push(gaviota_sys::TB_pieces::tb_NOPIECE as c_uchar);
    bp.push(gaviota_sys::TB_pieces::tb_NOPIECE as c_uchar);

    let mut info: c_uint = 0;
    let mut plies: c_uint = 0;

    let result = unsafe {
        gaviota_sys::tb_probe_hard(
            pos.turn().fold_wb(
                gaviota_sys::TB_sides::tb_WHITE_TO_MOVE,
                gaviota_sys::TB_sides::tb_BLACK_TO_MOVE,
            ) as c_uint,
            pos.ep_square(EnPassantMode::Legal)
                .map_or(gaviota_sys::TB_squares::tb_NOSQUARE as c_uint, c_uint::from),
            gaviota_sys::TB_castling::tb_NOCASTLE.0,
            ws.as_ptr(),
            bs.as_ptr(),
            wp.as_ptr(),
            bp.as_ptr(),
            &mut info,
            &mut plies,
        )
    };

    let plies = plies as i32;

    match gaviota_sys::TB_return_values(info) {
        gaviota_sys::TB_return_values::tb_DRAW if result != 0 => Some(Dtm(0)),
        gaviota_sys::TB_return_values::tb_WMATE if result != 0 => {
            Some(Dtm(pos.turn().fold_wb(plies, -plies)))
        }
        gaviota_sys::TB_return_values::tb_BMATE if result != 0 => {
            Some(Dtm(pos.turn().fold_wb(-plies, plies)))
        }
        gaviota_sys::TB_return_values::tb_FORBID => None,
        _ => {
            warn!("gaviota probe failed with result {result} and info {info}");
            None
        }
    }
}
