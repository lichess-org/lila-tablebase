use std::{
    ffi::{c_char, c_int},
    path::PathBuf,
};

use antichess_tb_sys::{antichess_tb_add_path, antichess_tb_init, antichess_tb_probe_dtw};
use arrayvec::ArrayVec;
use shakmaty::{variant::VariantPosition, EnPassantMode, Position as _};
use tracing::warn;

pub unsafe fn init(directories: &[PathBuf]) {
    unsafe {
        assert_eq!(antichess_tb_init(), 0, "antichess_tb_init");
        for path in directories {
            let path = path.as_os_str().to_str().unwrap();
            assert_eq!(
                antichess_tb_add_path(path.as_ptr() as *const c_char, path.len()),
                0,
                "antichess_tb_add_path"
            );
        }
    }
}

pub unsafe fn probe_dtw(pos: &VariantPosition) -> Option<i32> {
    let VariantPosition::Antichess(ref pos) = pos else {
        return None;
    };

    if pos.board().occupied().count() > 4 || pos.castles().any() {
        return None;
    }

    let mut ws = ArrayVec::<c_int, 4>::new();
    let mut bs = ArrayVec::<c_int, 4>::new();
    let mut wp = ArrayVec::<c_int, 4>::new();
    let mut bp = ArrayVec::<c_int, 4>::new();

    for (sq, piece) in pos.board().clone() {
        piece.color.fold_wb(&mut ws, &mut bs).push(c_int::from(sq));
        piece
            .color
            .fold_wb(&mut wp, &mut bp)
            .push(c_int::from(piece.role));
    }

    let mut dtw: c_int = 0;

    let result = unsafe {
        antichess_tb_probe_dtw(
            ws.as_ptr(),
            wp.as_ptr(),
            ws.len(),
            bs.as_ptr(),
            bp.as_ptr(),
            bs.len(),
            pos.turn().fold_wb(0, 1),
            pos.ep_square(EnPassantMode::Legal).map_or(-1, c_int::from),
            &mut dtw,
        )
    };

    match result {
        0 => Some(dtw as i32),
        1 => Some(dtw as i32), // cursed win/loss
        2 => Some(0),          // draw
        error => {
            warn!("antichess tb probe failed with error code {error}");
            None
        }
    }
}
