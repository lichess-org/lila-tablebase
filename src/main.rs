#![warn(rust_2018_idioms)]

use warp::Filter;
use warp::http::StatusCode;

use arrayvec::ArrayVec;
use log::{error, info, warn};
use serde::{Deserialize, Serialize};
use serde_with::{serde_as, DisplayFromStr};
use shakmaty::fen::{Fen, FenOpts, ParseFenError};
use shakmaty::san::SanPlus;
use shakmaty::uci::Uci;
use shakmaty::variants::{Atomic, Chess, Antichess};
use shakmaty::{CastlingMode, Move, MoveList, Outcome, Position, PositionErrorKinds, Role, Setup};
use shakmaty_syzygy::{Dtz, SyzygyError, Tablebase as SyzygyTablebase, Wdl};
use std::cmp::{min, Reverse};
use std::ffi::CString;
use std::net::SocketAddr;
use std::os::raw::{c_int, c_uchar, c_uint};
use std::path::PathBuf;
use std::str::FromStr;
use structopt::StructOpt;

#[derive(Copy, Clone, Debug)]
enum Variant {
    Standard,
    Atomic,
    Antichess,
}

#[derive(Debug)]
struct VariantNotFound;

impl FromStr for Variant {
    type Err = VariantNotFound;

    fn from_str(s: &str) -> Result<Variant, VariantNotFound> {
        Ok(match s {
            "standard" => Variant::Standard,
            "atomic" => Variant::Atomic,
            "antichess" => Variant::Antichess,
            _ => return Err(VariantNotFound),
        })
    }
}

impl Variant {
    fn position(self, fen: &Fen) -> Result<VariantPosition, PositionErrorKinds> {
        match self {
            Variant::Standard => fen.position(CastlingMode::Chess960).map(VariantPosition::Standard).map_err(|e| e.kinds()),
            Variant::Atomic => fen.position(CastlingMode::Chess960).map(VariantPosition::Atomic).map_err(|e| e.kinds()),
            Variant::Antichess => fen.position(CastlingMode::Chess960).map(VariantPosition::Antichess).map_err(|e| e.kinds()),
        }
    }
}

#[derive(Clone, Debug)]
enum VariantPosition {
    Standard(Chess),
    Atomic(Atomic),
    Antichess(Antichess),
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

    fn san_plus_and_play_unchecked(&mut self, m: &Move) -> SanPlus {
        match self {
            VariantPosition::Standard(pos) => SanPlus::from_move_and_play_unchecked(pos, m),
            VariantPosition::Atomic(pos) => SanPlus::from_move_and_play_unchecked(pos, m),
            VariantPosition::Antichess(pos) => SanPlus::from_move_and_play_unchecked(pos, m),
        }
    }

    fn san_plus(mut self, m: &Move) -> SanPlus {
        self.san_plus_and_play_unchecked(m)
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

#[serde_as]
#[derive(Serialize, Debug)]
struct MoveInfo {
    #[serde_as(as = "DisplayFromStr")]
    uci: Uci,
    #[serde_as(as = "DisplayFromStr")]
    san: SanPlus,
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

#[serde_as]
#[derive(Serialize, Debug)]
struct MainlineStep {
    #[serde_as(as = "DisplayFromStr")]
    uci: Uci,
    #[serde_as(as = "DisplayFromStr")]
    san: SanPlus,
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

#[derive(Debug)]
struct Tablebases {
    sloppy_real_wdl: bool,
    standard: SyzygyTablebase<Chess>,
    atomic: SyzygyTablebase<Atomic>,
    antichess: SyzygyTablebase<Antichess>,
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
                uci: m.to_uci(pos.borrow().castles().mode()),
                san: pos.clone().san_plus(m),
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
            m.zeroing ^ (m.pos.wdl.unwrap_or(Wdl::Draw) <= Wdl::Draw),
            m.capture.is_some() ^ (m.pos.wdl.unwrap_or(Wdl::Draw) <= Wdl::Draw),
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
                        uci: m.to_uci(pos.borrow().castles().mode()),
                        dtz,
                        san: pos.san_plus_and_play_unchecked(&m),
                    });
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
    PositionError(PositionErrorKinds),
    SyzygyError(SyzygyError),
}

impl From<ParseFenError> for TablebaseError {
    fn from(v: ParseFenError) -> TablebaseError {
        TablebaseError::ParseFenError(v)
    }
}

impl From<PositionErrorKinds> for TablebaseError {
    fn from(v: PositionErrorKinds) -> TablebaseError {
        TablebaseError::PositionError(v)
    }
}

impl From<SyzygyError> for TablebaseError {
    fn from(v: SyzygyError) -> TablebaseError {
        TablebaseError::SyzygyError(v)
    }
}

impl TablebaseError {
    fn to_response(&self) -> warp::reply::WithStatus<String> {
        match self {
            TablebaseError::MissingFen =>
                warp::reply::with_status("missing fen".to_owned(), StatusCode::BAD_REQUEST),
            TablebaseError::ParseFenError(_) =>
                warp::reply::with_status("invalid fen".to_owned(), StatusCode::BAD_REQUEST),
            TablebaseError::PositionError(_) =>
                warp::reply::with_status("illegal fen".to_owned(), StatusCode::BAD_REQUEST),
            TablebaseError::SyzygyError(err @ SyzygyError::Castling) |
            TablebaseError::SyzygyError(err @ SyzygyError::TooManyPieces) =>
                warp::reply::with_status(err.to_string(), StatusCode::NOT_FOUND),
            TablebaseError::SyzygyError(err @ SyzygyError::MissingTable { .. }) => {
                warn!("{}", err);
                warp::reply::with_status(err.to_string(), StatusCode::NOT_FOUND)
            },
            TablebaseError::SyzygyError(err @ SyzygyError::ProbeFailed { .. }) => {
                error!("{}", err);
                warp::reply::with_status(err.to_string(), StatusCode::INTERNAL_SERVER_ERROR)
            },
        }
    }
}

impl warp::reject::Reject for TablebaseError { }

#[derive(Deserialize, Debug)]
struct Query {
    fen: String,
}

impl Query {
    fn fen(&self) -> Result<Fen, ParseFenError> {
        str::replace(&self.fen, '_', " ").parse()
    }
}

fn try_probe(tbs: &Tablebases, variant: Variant, query: Query) -> Result<TablebaseResponse, TablebaseError> {
    let fen = query.fen()?;
    let pos = variant.position(&fen)?;

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

fn try_mainline(tbs: &Tablebases, variant: Variant, query: Query) -> Result<MainlineResponse, TablebaseError> {
    let fen = query.fen()?;
    let pos = variant.position(&fen)?;

    match pos {
        VariantPosition::Standard(ref pos) =>
            info!("standard mainline: {}", FenOpts::default().fen(pos)),
        VariantPosition::Atomic(ref pos) =>
            info!("atomic mainline: {}", FenOpts::default().fen(pos)),
        VariantPosition::Antichess(ref pos) =>
            info!("antichess mainline: {}", FenOpts::default().promoted(true).fen(pos)),
    }

    Ok(tbs.mainline(pos)?)
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

#[tokio::main(core_threads=5)]
async fn main() {
    env_logger::init();

    // Parse arguments.
    let opt = Opt::from_args();
    if opt.standard.is_empty() && opt.atomic.is_empty() && opt.antichess.is_empty() && opt.gaviota.is_empty() {
        Opt::clap().print_help().expect("usage");
        println!();
        return;
    }

    let bind = SocketAddr::new(opt.address.parse().expect("valid address"), opt.port);

    // Initialize Syzygy tablebases.
    let tbs: &'static Tablebases = {
        let mut standard = SyzygyTablebase::<Chess>::new();
        let mut atomic = SyzygyTablebase::<Atomic>::new();
        let mut antichess = SyzygyTablebase::<Antichess>::new();

        for path in opt.standard {
            standard.add_directory(path).expect("open standard directory");
        }
        for path in opt.atomic {
            atomic.add_directory(path).expect("open atomic directory");
        }
        for path in opt.antichess {
            antichess.add_directory(path).expect("open antichess directory");
        }

        Box::leak(Box::new(Tablebases {
            sloppy_real_wdl: opt.sloppy_real_wdl,
            standard,
            atomic,
            antichess,
        }))
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
    let probe = warp::get()
        .map(move || tbs)
        .and(warp::path!(Variant))
        .and(warp::query::query())
        .and_then(|tbs: &'static Tablebases, variant: Variant, query: Query| async move {
            tokio::task::spawn_blocking(move || {
                match try_probe(tbs, variant, query) {
                    Ok(res) => Ok(warp::reply::json(&res)),
                    Err(err) => Err(warp::reject::custom(err)),
                }
            }).await.expect("probe")
        });
    let mainline = warp::get()
        .map(move || tbs)
        .and(warp::path!(Variant / "mainline"))
        .and(warp::query::query())
        .and_then(|tbs: &'static Tablebases, variant: Variant, query: Query| async move {
            tokio::task::spawn_blocking(move || {
                match try_mainline(tbs, variant, query) {
                    Ok(res) => Ok(warp::reply::json(&res)),
                    Err(err) => Err(warp::reject::custom(err)),
                }
            }).await.expect("mainline")
        });
    let api = probe.or(mainline).recover(|rejection: warp::reject::Rejection| async move {
        if rejection.find::<warp::reject::InvalidQuery>().is_some() {
            Ok(TablebaseError::MissingFen.to_response())
        } else if let Some(err) = rejection.find::<TablebaseError>() {
            Ok(err.to_response())
        } else {
            Err(rejection)
        }
    });
    warp::serve(api).run(bind).await;
}
