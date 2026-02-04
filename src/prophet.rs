use std::{ffi::CString, iter::zip, os::raw::c_int, path::PathBuf};

use prophet_sys::{
    prophet_tb_add_path, prophet_tb_create_decompress_ctx, prophet_tb_free_decompress_ctx,
    prophet_tb_init, prophet_tb_probe_dtm_dctx,
};
use shakmaty::{EnPassantMode, Position as _, variant::VariantPosition};
use tracing::info;

use crate::metric::Dtw;

pub unsafe fn init(directories: &[PathBuf]) {
    unsafe {
        prophet_tb_init();
    }

    for path in directories.iter().rev() {
        let c_string = CString::new(path.as_os_str().to_str().unwrap()).unwrap();
        let n = unsafe { prophet_tb_add_path(c_string.as_ptr()) };
        info!("added {} prophet tables from {}", n, path.display());
    }
}

pub unsafe fn probe_dtm(pos: &VariantPosition) -> Option<Dtw> {
    let VariantPosition::Chess(pos) = pos else {
        return None;
    };

    if pos.board().occupied().count() > 6 || pos.castles().any() || pos.is_checkmate() {
        return None;
    }

    let mut pieces: [c_int; 6] = [0; 6];
    let mut squares: [c_int; 6] = [0; 6];
    for ((square, piece), (p, s)) in zip(pos.board(), zip(&mut pieces, &mut squares)) {
        *p = c_int::from(piece.role) + piece.color.fold_wb(0, 8);
        *s = c_int::from(square);
    }

    let dtm;
    unsafe {
        let dctx = prophet_tb_create_decompress_ctx();
        dtm = prophet_tb_probe_dtm_dctx(
            pieces.as_ptr(),
            squares.as_ptr(),
            pos.turn().fold_wb(0, 1),
            pos.ep_square(EnPassantMode::Legal).map_or(0, c_int::from),
            dctx,
        );
        prophet_tb_free_decompress_ctx(dctx);
    };

    (dtm != -1001).then_some(Dtw(dtm))
}
