#![warn(rust_2018_idioms)]

use async_std;
use async_std::task;
use tide;
use tide::{Response, IntoResponse, Request};
use tide::http::status::StatusCode;

use arrayvec::ArrayVec;
use log::{error, info, warn};
use serde::{Deserialize, Serialize};
use shakmaty::fen::{Fen, FenOpts, ParseFenError};
use shakmaty::san::SanPlus;
use shakmaty::uci::Uci;
use shakmaty::variants::{Atomic, Chess, Giveaway};
use shakmaty::{Move, MoveList, Outcome, Position, PositionError, Role, Setup};
use shakmaty_syzygy::{Dtz, SyzygyError, Tablebase as SyzygyTablebase, Wdl};
use std::cmp::{min, Reverse};
use std::ffi::CString;
use std::net::SocketAddr;
use std::os::raw::{c_int, c_uchar, c_uint};
use std::path::PathBuf;
use std::sync::Arc;
use structopt::StructOpt;

#[derive(Copy, Clone, Debug)]
enum Variant {
    Standard,
    Atomic,
    Antichess,
}

impl Variant {
    fn position(self, fen: &Fen) -> Result<VariantPosition, PositionError> {
        match self {
            Variant::Standard => fen.position().map(VariantPosition::Standard),
            Variant::Atomic => fen.position().map(VariantPosition::Atomic),
            Variant::Antichess => fen.position().map(VariantPosition::Antichess),
        }
    }
}

#[derive(Clone, Debug)]
enum VariantPosition {
    Standard(Chess),
    Atomic(Atomic),
    Antichess(Giveaway),
}

impl VariantPosition {
    fn borrow(&self) -> &dyn Position {
        match *self {
            VariantPosition::Standard(ref pos) => pos,
            VariantPosition::Atomic(ref pos) => pos,
            VariantPosition::Antichess(ref pos) => pos,
        }
    }

    fn borrow_mut(&mut self) -> &mut dyn Position {
        match *self {
            VariantPosition::Standard(ref mut pos) => pos,
            VariantPosition::Atomic(ref mut pos) => pos,
            VariantPosition::Antichess(ref mut pos) => pos,
        }
    }

    fn uci(&self, m: &Move) -> Uci {
        match *self {
            VariantPosition::Standard(ref pos) => Uci::from_move(pos, m),
            VariantPosition::Atomic(ref pos) => Uci::from_move(pos, m),
            VariantPosition::Antichess(ref pos) => Uci::from_move(pos, m),
        }
    }

    fn san_plus(self, m: &Move) -> SanPlus {
        match self {
            VariantPosition::Standard(pos) => SanPlus::from_move(pos, m),
            VariantPosition::Atomic(pos) => SanPlus::from_move(pos, m),
            VariantPosition::Antichess(pos) => SanPlus::from_move(pos, m),
        }
    }
}

/// Serialize a `T` (e.g. `Wdl`, `Dtz`) that can be converted to an integer.
fn into_i32<T, S>(value: &T, s: S) -> Result<S::Ok, S::Error>
where
    T: Into<i32> + Clone,
    S: serde::Serializer,
{
    s.serialize_i32(value.clone().into())
}

/// Serialize an `Option<T>` where `T` can be converted to an integer.
///
/// In the future https://github.com/serde-rs/serde/issues/723 might make this
/// obsolete.
fn into_i32_option<T, S>(value: &Option<T>, s: S) -> Result<S::Ok, S::Error>
where
    T: Into<i32> + Clone,
    S: serde::Serializer,
{
    match value {
        None => s.serialize_none(),
        Some(value) => s.serialize_some(&value.clone().into()),
    }
}

#[derive(Serialize, Debug)]
struct TablebaseResponse {
    #[serde(flatten)]
    pos: PositionInfo,
    moves: ArrayVec<[MoveInfo; 256]>,
}

#[derive(Serialize, Debug)]
struct MoveInfo {
    uci: String,
    san: String,
    zeroing: bool,
    #[serde(skip)]
    capture: Option<Role>,
    #[serde(skip)]
    promotion: Option<Role>,
    #[serde(flatten)]
    pos: PositionInfo,
}

#[derive(Serialize, Debug)]
struct PositionInfo {
    checkmate: bool,
    stalemate: bool,
    variant_win: bool,
    variant_loss: bool,
    insufficient_material: bool,
    #[serde(serialize_with = "into_i32_option")]
    wdl: Option<Wdl>,
    #[serde(serialize_with = "into_i32_option")]
    dtz: Option<Dtz>,
    dtm: Option<i32>,
}

#[derive(Serialize, Debug)]
struct MainlineResponse {
    mainline: Vec<MainlineStep>,
    winner: Option<char>,
    #[serde(serialize_with = "into_i32")]
    dtz: Dtz,
}

#[derive(Serialize, Debug)]
struct MainlineStep {
    uci: String,
    #[serde(serialize_with = "into_i32")]
    dtz: Dtz,
}

unsafe fn probe_dtm(pos: &VariantPosition) -> Option<i32> {
    let pos = match *pos {
        VariantPosition::Standard(ref pos) => pos,
        _ => return None,
    };

    if pos.board().occupied().count() > 5 || pos.castles().any() {
        return None;
    }

    if gaviota_sys::tb_is_initialized() == 0 {
        return None;
    }

    let mut ws = ArrayVec::<[c_uint; 6]>::new();
    let mut bs = ArrayVec::<[c_uint; 6]>::new();
    let mut wp = ArrayVec::<[c_uchar; 6]>::new();
    let mut bp = ArrayVec::<[c_uchar; 6]>::new();

    for (sq, piece) in pos.board().pieces() {
        piece.color.fold(&mut ws, &mut bs).push(c_uint::from(sq));
        piece.color.fold(&mut wp, &mut bp).push(c_uchar::from(piece.role));
    }

    ws.push(gaviota_sys::TB_squares::tb_NOSQUARE as c_uint);
    bs.push(gaviota_sys::TB_squares::tb_NOSQUARE as c_uint);
    wp.push(gaviota_sys::TB_pieces::tb_NOPIECE as c_uchar);
    bp.push(gaviota_sys::TB_pieces::tb_NOPIECE as c_uchar);

    let mut info: c_uint = 0;
    let mut plies: c_uint = 0;

    let result = gaviota_sys::tb_probe_hard(
        pos.turn().fold(gaviota_sys::TB_sides::tb_WHITE_TO_MOVE, gaviota_sys::TB_sides::tb_BLACK_TO_MOVE) as c_uint,
        pos.ep_square().map_or(gaviota_sys::TB_squares::tb_NOSQUARE as c_uint, c_uint::from),
        gaviota_sys::TB_castling::tb_NOCASTLE.0,
        ws.as_ptr(),
        bs.as_ptr(),
        wp.as_ptr(),
        bp.as_ptr(),
        &mut info as *mut c_uint,
        &mut plies as *mut c_uint);

    let plies = plies as i32;

    match gaviota_sys::TB_return_values(info) {
        gaviota_sys::TB_return_values::tb_DRAW if result != 0 => Some(0),
        gaviota_sys::TB_return_values::tb_WMATE if result != 0 => Some(pos.turn().fold(plies, -plies)),
        gaviota_sys::TB_return_values::tb_BMATE if result != 0 => Some(pos.turn().fold(-plies, plies)),
        gaviota_sys::TB_return_values::tb_FORBID => None,
        _ => {
            warn!("gaviota probe failed with result {} and info {}", result, info);
            None
        }
    }
}

#[derive(Clone, Debug)]
struct Tablebases {
    sloppy_real_wdl: bool,
    standard: Arc<SyzygyTablebase<Chess>>,
    atomic: Arc<SyzygyTablebase<Atomic>>,
    antichess: Arc<SyzygyTablebase<Giveaway>>,
}

impl Tablebases {
    fn probe_dtz(&self, pos: &VariantPosition) -> Result<Dtz, SyzygyError> {
        match *pos {
            VariantPosition::Standard(ref pos) => self.standard.probe_dtz(pos),
            VariantPosition::Atomic(ref pos) => self.atomic.probe_dtz(pos),
            VariantPosition::Antichess(ref pos) => self.antichess.probe_dtz(pos),
        }
    }

    fn best_move(&self, pos: &VariantPosition) -> Result<Option<(Move, Dtz)>, SyzygyError> {
        match *pos {
            VariantPosition::Standard(ref pos) => self.standard.best_move(pos),
            VariantPosition::Atomic(ref pos) => self.atomic.best_move(pos),
            VariantPosition::Antichess(ref pos) => self.antichess.best_move(pos),
        }
    }

    fn real_wdl(&self, pos: &VariantPosition, dtz: Dtz) -> Result<Wdl, SyzygyError> {
        if let Some(outcome) = pos.borrow().outcome() {
            return Ok(Wdl::from_outcome(outcome, pos.borrow().turn()));
        }

        let halfmoves = min(101, pos.borrow().halfmoves()) as i32;
        let before_zeroing = dtz.add_plies(halfmoves);

        if self.sloppy_real_wdl {
            // Expensive disambiguation disabled.
            return Ok(Wdl::from_dtz_after_zeroing(before_zeroing));
        }

        if before_zeroing.0.abs() != 100 || halfmoves == 0 {
            // Unambiguous.
            return Ok(Wdl::from_dtz_after_zeroing(before_zeroing));
        }

        if halfmoves == 1 && dtz.0.abs() == 99 {
            // This could only be a cursed/blessed result if the real DTZ was
            // 100 instead of 99. But tables with DTZ 100 will always
            // store precise DTZ values, hence it could not have been 100.
            return Ok(Wdl::from_dtz_after_zeroing(before_zeroing));
        }

        let best = self.best_move(pos)?.expect("has moves");
        let mut after = pos.clone();
        after.borrow_mut().play_unchecked(&best.0);
        Ok(-self.real_wdl(&after, best.1)?)
    }

    fn position_info(&self, pos: &VariantPosition) -> Result<PositionInfo, SyzygyError> {
        let (variant_win, variant_loss) = match pos.borrow().variant_outcome() {
            Some(Outcome::Decisive { winner }) =>
                (winner == pos.borrow().turn(), winner != pos.borrow().turn()),
            _ =>
                (false, false),
        };

        fn user_error_as_none<T>(res: Result<T, SyzygyError>) -> Result<Option<T>, SyzygyError> {
            match res {
                Err(SyzygyError::Castling)
                | Err(SyzygyError::TooManyPieces)
                | Err(SyzygyError::MissingTable { .. }) => Ok(None), // user error
                Err(err) => Err(err), // server error
                Ok(res) => Ok(Some(res)),
            }
        }

        let dtz = user_error_as_none(self.probe_dtz(pos))?;

        let wdl = if let Some(dtz) = dtz {
            user_error_as_none(self.real_wdl(pos, dtz))?
        } else {
            None
        };

        Ok(PositionInfo {
            checkmate: pos.borrow().is_checkmate(),
            stalemate: pos.borrow().is_stalemate(),
            variant_win,
            variant_loss,
            insufficient_material: pos.borrow().is_insufficient_material(),
            dtz,
            wdl,
            dtm: unsafe { probe_dtm(pos) },
        })
    }

    fn probe(&self, pos: &VariantPosition) -> Result<TablebaseResponse, SyzygyError> {
        let mut moves = MoveList::new();
        pos.borrow().legal_moves(&mut moves);

        let mut move_info = moves.iter().map(|m| {
            let mut after = pos.clone();
            after.borrow_mut().play_unchecked(m);

            Ok(MoveInfo {
                uci: pos.uci(m).to_string(),
                san: pos.clone().san_plus(m).to_string(),
                pos: self.position_info(&after)?,
                capture: m.capture(),
                promotion: m.promotion(),
                zeroing: m.is_zeroing(),
            })
        }).collect::<Result<ArrayVec<[_; 256]>, SyzygyError>>()?;

        move_info.sort_by_key(|m: &MoveInfo| (
            (Reverse(m.pos.checkmate), Reverse(m.pos.variant_loss), m.pos.variant_win),
            (Reverse(m.pos.wdl == Some(Wdl::Loss)), m.pos.wdl),
            (Reverse(m.pos.stalemate), Reverse(m.pos.insufficient_material)),
            if m.pos.wdl.unwrap_or(Wdl::Draw) < Wdl::Draw { Reverse(m.pos.dtm) } else { Reverse(None) },
            if m.pos.wdl.unwrap_or(Wdl::Draw) > Wdl::Draw { m.pos.dtm.map(Reverse) } else { None },
            m.zeroing ^ (m.pos.wdl.unwrap_or(Wdl::Draw) < Wdl::Draw),
            m.capture.is_some() ^ (m.pos.wdl.unwrap_or(Wdl::Draw) < Wdl::Draw),
            m.pos.dtz.map(Reverse),
            (Reverse(m.capture), Reverse(m.promotion)),
        ));

        Ok(TablebaseResponse {
            pos: self.position_info(&pos)?,
            moves: move_info,
        })
    }

    fn mainline(&self, mut pos: VariantPosition) -> Result<MainlineResponse, SyzygyError> {
        let dtz = self.probe_dtz(&pos)?;
        let mut mainline = Vec::new();

        if dtz != Dtz(0) {
            while pos.borrow().halfmoves() < 100 {
                if let Some((m, dtz)) = self.best_move(&pos)? {
                    mainline.push(MainlineStep {
                        uci: pos.uci(&m).to_string(),
                        dtz,
                    });

                    pos.borrow_mut().play_unchecked(&m);
                } else {
                    break;
                }
            }
        }

        Ok(MainlineResponse {
            dtz,
            mainline,
            winner: pos.borrow().outcome().and_then(|o| o.winner()).map(|winner| winner.char()),
        })
    }
}

#[derive(Debug)]
enum TablebaseError {
    MissingFen,
    ParseFenError(ParseFenError),
    PositionError(PositionError),
    SyzygyError(SyzygyError),
}

impl From<ParseFenError> for TablebaseError {
    fn from(v: ParseFenError) -> TablebaseError {
        TablebaseError::ParseFenError(v)
    }
}

impl From<PositionError> for TablebaseError {
    fn from(v: PositionError) -> TablebaseError {
        TablebaseError::PositionError(v)
    }
}

impl From<SyzygyError> for TablebaseError {
    fn from(v: SyzygyError) -> TablebaseError {
        TablebaseError::SyzygyError(v)
    }
}

impl IntoResponse for TablebaseError {
    fn into_response(self) -> Response {
        match self {
            TablebaseError::MissingFen =>
                "missing fen".with_status(StatusCode::BAD_REQUEST).into_response(),
            TablebaseError::ParseFenError(_) =>
                "invalid fen".with_status(StatusCode::BAD_REQUEST).into_response(),
            TablebaseError::PositionError(_) =>
                "illegal fen".with_status(StatusCode::BAD_REQUEST).into_response(),
            TablebaseError::SyzygyError(err @ SyzygyError::Castling) |
            TablebaseError::SyzygyError(err @ SyzygyError::TooManyPieces) =>
                err.to_string().with_status(StatusCode::NOT_FOUND).into_response(),
            TablebaseError::SyzygyError(err @ SyzygyError::MissingTable { .. }) => {
                warn!("{}", err);
                err.to_string().with_status(StatusCode::NOT_FOUND).into_response()
            },
            TablebaseError::SyzygyError(err @ SyzygyError::ProbeFailed { .. }) => {
                error!("{}", err);
                err.to_string().with_status(StatusCode::INTERNAL_SERVER_ERROR).into_response()
            },
        }
    }
}

#[derive(Deserialize, Debug)]
struct Query {
    fen: String,
}

impl Query {
    fn fen(&self) -> Result<Fen, ParseFenError> {
        str::replace(&self.fen, '_', " ").parse()
    }
}

fn try_probe(variant: Variant, req: Request<Tablebases>) -> Result<TablebaseResponse, TablebaseError> {
    let query: Query = req.query().map_err(|_| TablebaseError::MissingFen)?;
    let fen = query.fen()?;
    let pos = variant.position(&fen)?;
    let tbs = req.state();

    match pos {
        VariantPosition::Standard(ref pos) =>
            info!("standard: {}", FenOpts::default().fen(pos)),
        VariantPosition::Atomic(ref pos) =>
            info!("atomic: {}", FenOpts::default().fen(pos)),
        VariantPosition::Antichess(ref pos) =>
            info!("antichess: {}", FenOpts::default().promoted(true).fen(pos)),
    }

    Ok(tbs.probe(&pos)?)
}

async fn probe(variant: Variant, req: Request<Tablebases>) -> Response {
    match task::spawn_blocking(move || try_probe(variant, req)).await {
        Ok(res) => Response::new(200).body_json(&res).expect("body json"),
        Err(err) => err.into_response(),
    }
}

fn try_mainline(variant: Variant, req: Request<Tablebases>) -> Result<MainlineResponse, TablebaseError> {
    let query: Query = req.query().map_err(|_| TablebaseError::MissingFen)?;
    let fen = query.fen()?;
    let pos = variant.position(&fen)?;
    let tbs = req.state();

    match pos {
        VariantPosition::Standard(ref pos) =>
            info!("standard mainline: {}", FenOpts::default().fen(pos)),
        VariantPosition::Atomic(ref pos) =>
            info!("atomic mainline: {}", FenOpts::default().fen(pos)),
        VariantPosition::Antichess(ref pos) =>
            info!("antichess mainline: {}", FenOpts::default().promoted(true).fen(pos)),
    }

    Ok(tbs.mainline(pos.clone())?)
}

async fn mainline(variant: Variant, req: Request<Tablebases>) -> Response {
    match task::spawn_blocking(move || try_mainline(variant, req)).await {
        Ok(res) => Response::new(200).body_json(&res).expect("body json"),
        Err(err) => err.into_response(),
    }
}

#[derive(StructOpt, Debug)]
struct Opt {
    /// Directory with tablebase files for standard chess.
    #[structopt(long = "standard", parse(from_os_str))]
    standard: Vec<PathBuf>,
    /// Directory with tablebase files for atomic chess.
    #[structopt(long = "atomic", parse(from_os_str))]
    atomic: Vec<PathBuf>,
    /// Directory with tablebase files for antichess.
    #[structopt(long = "antichess", parse(from_os_str))]
    antichess: Vec<PathBuf>,
    /// Directory with Gaviota tablebase files.
    #[structopt(long = "gaviota", parse(from_os_str))]
    gaviota: Vec<PathBuf>,

    /// Disable expensive search that resolves ambiguous WDLs.
    ///
    /// Results may be incorrect for positions with halfmove clock > 1 that are
    /// on the edge of the 50-move rule.
    #[structopt(long = "sloppy-real-wdl")]
    sloppy_real_wdl: bool,

    /// Listen on this address.
    #[structopt(long = "address", default_value = "127.0.0.1")]
    address: String,
    /// Listen on this port.
    #[structopt(long = "port", default_value = "9000")]
    port: u16,
}

#[async_std::main]
async fn main() -> Result<(), std::io::Error> {
    env_logger::init();

    // Parse arguments.
    let opt = Opt::from_args();
    if opt.standard.is_empty() && opt.atomic.is_empty() && opt.antichess.is_empty() && opt.gaviota.is_empty() {
        Opt::clap().print_help().expect("usage");
        println!();
        return Ok(());
    }

    let bind = SocketAddr::new(opt.address.parse().expect("valid address"), opt.port);

    // Initialize Syzygy tablebases.
    let tablebases = {
        let mut standard = SyzygyTablebase::<Chess>::new();
        let mut atomic = SyzygyTablebase::<Atomic>::new();
        let mut antichess = SyzygyTablebase::<Giveaway>::new();

        for path in opt.standard {
            standard.add_directory(path).expect("open standard directory");
        }
        for path in opt.atomic {
            atomic.add_directory(path).expect("open atomic directory");
        }
        for path in opt.antichess {
            antichess.add_directory(path).expect("open antichess directory");
        }

        Tablebases {
            sloppy_real_wdl: opt.sloppy_real_wdl,
            standard: Arc::new(standard),
            atomic: Arc::new(atomic),
            antichess: Arc::new(antichess),
        }
    };

    // Initialize Gaviota tablebase.
    if !opt.gaviota.is_empty() {
        unsafe {
            assert!(gaviota_sys::tbcache_init(1014 * 1024, 50) != 0);
            let mut paths = gaviota_sys::tbpaths_init();
            assert!(!paths.is_null());
            for path in opt.gaviota {
                let path = CString::new(path.as_os_str().to_str().unwrap()).unwrap();
                paths = gaviota_sys::tbpaths_add(paths, path.as_ptr());
                assert!(!paths.is_null());
            }
            assert!(!gaviota_sys::tb_init(1, gaviota_sys::TB_compression_scheme::tb_CP4 as c_int, paths).is_null());
        }
    }

    // Start server.
    let mut app = tide::with_state(tablebases);
    app.at("/standard").get(|req| probe(Variant::Standard, req));
    app.at("/standard/mainline").get(|req| mainline(Variant::Standard, req));
    app.at("/atomic").get(|req| probe(Variant::Atomic, req));
    app.at("/atomic/mainline").get(|req| mainline(Variant::Atomic, req));
    app.at("/antichess").get(|req| probe(Variant::Antichess, req));
    app.at("/antichess/mainline").get(|req| mainline(Variant::Antichess, req));
    app.listen(bind).await
}
