#![forbid(unsafe_op_in_unsafe_fn)]

use std::{
    cmp::Reverse,
    ffi::CString,
    net::SocketAddr,
    ops::Neg,
    os::raw::{c_int, c_uchar, c_uint},
    path::PathBuf,
    str::FromStr,
};

use arrayvec::ArrayVec;
use clap::{IntoApp as _, Parser};
use log::{error, info, warn};
use serde::{Deserialize, Serialize};
use serde_with::{serde_as, DisplayFromStr, FromInto};
use shakmaty::{
    fen::{fen, Fen, ParseFenError},
    san::SanPlus,
    uci::Uci,
    variant::{Antichess, Atomic, Chess},
    CastlingMode, Move, Outcome, Position, PositionError, PositionErrorKinds, Role, Setup,
};
use shakmaty_syzygy::{AmbiguousWdl, Dtz, MaybeRounded, SyzygyError, Tablebase as SyzygyTablebase};
use warp::{http::StatusCode, Filter};

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
            Variant::Standard => fen
                .position(CastlingMode::Chess960)
                .or_else(PositionError::ignore_invalid_castling_rights)
                .or_else(PositionError::ignore_invalid_ep_square)
                .or_else(PositionError::ignore_impossible_check)
                .map(VariantPosition::Standard)
                .map_err(|e| e.kinds()),
            Variant::Atomic => fen
                .position(CastlingMode::Chess960)
                .or_else(PositionError::ignore_invalid_castling_rights)
                .or_else(PositionError::ignore_invalid_ep_square)
                .or_else(PositionError::ignore_impossible_check)
                .map(VariantPosition::Atomic)
                .map_err(|e| e.kinds()),
            Variant::Antichess => fen
                .position(CastlingMode::Chess960)
                .or_else(PositionError::ignore_invalid_castling_rights)
                .or_else(PositionError::ignore_invalid_ep_square)
                .or_else(PositionError::ignore_impossible_check)
                .map(VariantPosition::Antichess)
                .map_err(|e| e.kinds()),
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

#[derive(Serialize, Debug)]
struct TablebaseResponse {
    #[serde(flatten)]
    pos: PositionInfo,
    category: Category,
    moves: ArrayVec<MoveInfo, 256>,
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
    category: Category,
}

#[serde_as]
#[derive(Serialize, Debug)]
struct PositionInfo {
    checkmate: bool,
    stalemate: bool,
    variant_win: bool,
    variant_loss: bool,
    insufficient_material: bool,
    #[serde_as(as = "Option<FromInto<i32>>")]
    #[serde(rename = "dtz")]
    maybe_rounded_dtz: Option<Dtz>,
    #[serde_as(as = "Option<FromInto<i32>>")]
    precise_dtz: Option<Dtz>,
    #[serde(skip)]
    dtz: Option<MaybeRounded<Dtz>>,
    dtm: Option<i32>,
    #[serde(skip)]
    halfmoves: u32,
}

impl PositionInfo {
    fn category(&self, halfmoves_before: u32) -> Category {
        if !self.variant_win
            && !self.variant_loss
            && (self.stalemate
                || self.insufficient_material
                || self.dtz.map_or(false, |dtz| dtz.is_zero()))
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
            } else {
                if dtz.is_negative() {
                    Category::BlessedLoss
                } else {
                    Category::CursedWin
                }
            }
        } else {
            Category::Unknown
        }
    }
}

#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Serialize)]
enum Category {
    #[serde(rename = "loss")]
    Loss,
    #[serde(rename = "unknown")]
    Unknown,
    #[serde(rename = "maybe-loss")]
    MaybeLoss,
    #[serde(rename = "blessed-loss")]
    BlessedLoss,
    #[serde(rename = "draw")]
    Draw,
    #[serde(rename = "cursed-win")]
    CursedWin,
    #[serde(rename = "maybe-win")]
    MaybeWin,
    #[serde(rename = "win")]
    Win,
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
struct MainlineResponse {
    mainline: Vec<MainlineStep>,
    winner: Option<char>,
    #[serde_as(as = "FromInto<i32>")]
    dtz: Dtz,
    #[serde_as(as = "Option<FromInto<i32>>")]
    precise_dtz: Option<Dtz>,
}

#[serde_as]
#[derive(Serialize, Debug)]
struct MainlineStep {
    #[serde_as(as = "DisplayFromStr")]
    uci: Uci,
    #[serde_as(as = "DisplayFromStr")]
    san: SanPlus,
    #[serde_as(as = "FromInto<i32>")]
    dtz: Dtz,
    #[serde_as(as = "Option<FromInto<i32>>")]
    precise_dtz: Option<Dtz>,
}

unsafe fn probe_dtm(pos: &VariantPosition) -> Option<i32> {
    let pos = match *pos {
        VariantPosition::Standard(ref pos) => pos,
        _ => return None,
    };

    if pos.board().occupied().count() > 5 || pos.castles().any() {
        return None;
    }

    if unsafe { gaviota_sys::tb_is_initialized() } == 0 {
        return None;
    }

    let mut ws = ArrayVec::<c_uint, 6>::new();
    let mut bs = ArrayVec::<c_uint, 6>::new();
    let mut wp = ArrayVec::<c_uchar, 6>::new();
    let mut bp = ArrayVec::<c_uchar, 6>::new();

    for (sq, piece) in pos.board().pieces() {
        piece.color.fold(&mut ws, &mut bs).push(c_uint::from(sq));
        piece
            .color
            .fold(&mut wp, &mut bp)
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
            pos.turn().fold(
                gaviota_sys::TB_sides::tb_WHITE_TO_MOVE,
                gaviota_sys::TB_sides::tb_BLACK_TO_MOVE,
            ) as c_uint,
            pos.ep_square()
                .map_or(gaviota_sys::TB_squares::tb_NOSQUARE as c_uint, c_uint::from),
            gaviota_sys::TB_castling::tb_NOCASTLE.0,
            ws.as_ptr(),
            bs.as_ptr(),
            wp.as_ptr(),
            bp.as_ptr(),
            &mut info as *mut c_uint,
            &mut plies as *mut c_uint,
        )
    };

    let plies = plies as i32;

    match gaviota_sys::TB_return_values(info) {
        gaviota_sys::TB_return_values::tb_DRAW if result != 0 => Some(0),
        gaviota_sys::TB_return_values::tb_WMATE if result != 0 => {
            Some(pos.turn().fold(plies, -plies))
        }
        gaviota_sys::TB_return_values::tb_BMATE if result != 0 => {
            Some(pos.turn().fold(-plies, plies))
        }
        gaviota_sys::TB_return_values::tb_FORBID => None,
        _ => {
            warn!(
                "gaviota probe failed with result {} and info {}",
                result, info
            );
            None
        }
    }
}

#[derive(Debug)]
struct Tablebases {
    standard: SyzygyTablebase<Chess>,
    atomic: SyzygyTablebase<Atomic>,
    antichess: SyzygyTablebase<Antichess>,
}

impl Tablebases {
    fn probe_dtz(&self, pos: &VariantPosition) -> Result<MaybeRounded<Dtz>, SyzygyError> {
        match *pos {
            VariantPosition::Standard(ref pos) => self.standard.probe_dtz(pos),
            VariantPosition::Atomic(ref pos) => self.atomic.probe_dtz(pos),
            VariantPosition::Antichess(ref pos) => self.antichess.probe_dtz(pos),
        }
    }

    fn best_move(
        &self,
        pos: &VariantPosition,
    ) -> Result<Option<(Move, MaybeRounded<Dtz>)>, SyzygyError> {
        match *pos {
            VariantPosition::Standard(ref pos) => self.standard.best_move(pos),
            VariantPosition::Atomic(ref pos) => self.atomic.best_move(pos),
            VariantPosition::Antichess(ref pos) => self.antichess.best_move(pos),
        }
    }

    fn position_info(&self, pos: &VariantPosition) -> Result<PositionInfo, SyzygyError> {
        let (variant_win, variant_loss) = match pos.borrow().variant_outcome() {
            Some(Outcome::Decisive { winner }) => {
                (winner == pos.borrow().turn(), winner != pos.borrow().turn())
            }
            _ => (false, false),
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

        Ok(PositionInfo {
            checkmate: pos.borrow().is_checkmate(),
            stalemate: pos.borrow().is_stalemate(),
            variant_win,
            variant_loss,
            insufficient_material: pos.borrow().is_insufficient_material(),
            maybe_rounded_dtz: dtz.map(MaybeRounded::ignore_rounding),
            precise_dtz: dtz.and_then(MaybeRounded::precise),
            dtz,
            dtm: unsafe { probe_dtm(pos) },
            halfmoves: pos.borrow().halfmoves(),
        })
    }

    fn probe(&self, pos: &VariantPosition) -> Result<TablebaseResponse, SyzygyError> {
        let halfmoves = pos.borrow().halfmoves();

        let mut move_info = pos
            .borrow()
            .legal_moves()
            .iter()
            .map(|m| {
                let mut after = pos.clone();
                after.borrow_mut().play_unchecked(m);

                let after_info = self.position_info(&after)?;

                Ok(MoveInfo {
                    uci: m.to_uci(pos.borrow().castles().mode()),
                    san: pos.clone().san_plus(m),
                    capture: m.capture(),
                    promotion: m.promotion(),
                    zeroing: m.is_zeroing(),
                    category: after_info.category(halfmoves),
                    pos: after_info,
                })
            })
            .collect::<Result<ArrayVec<_, 256>, SyzygyError>>()?;

        move_info.sort_by_key(|m: &MoveInfo| {
            (
                m.category,
                (
                    Reverse(m.pos.checkmate),
                    Reverse(m.pos.variant_loss),
                    m.pos.variant_win,
                ),
                (
                    Reverse(m.pos.stalemate),
                    Reverse(m.pos.insufficient_material),
                ),
                if m.pos
                    .dtz
                    .unwrap_or(MaybeRounded::Precise(Dtz(0)))
                    .is_negative()
                {
                    Reverse(m.pos.dtm)
                } else {
                    Reverse(None)
                },
                if m.pos
                    .dtz
                    .unwrap_or(MaybeRounded::Precise(Dtz(0)))
                    .is_positive()
                {
                    m.pos.dtm.map(Reverse)
                } else {
                    None
                },
                m.zeroing
                    ^ !m.pos
                        .dtz
                        .unwrap_or(MaybeRounded::Precise(Dtz(0)))
                        .is_positive(),
                m.capture.is_some()
                    ^ !m.pos
                        .dtz
                        .unwrap_or(MaybeRounded::Precise(Dtz(0)))
                        .is_positive(),
                m.pos.maybe_rounded_dtz.map(Reverse),
                (Reverse(m.capture), Reverse(m.promotion)),
            )
        });

        let pos_info = self.position_info(pos)?;
        let category = pos_info.category(halfmoves.saturating_sub(1));

        // Use category of previous position to infer maybe-win / maybe-loss,
        // if possible.
        for (prev, maybe, correct) in [
            (Category::Win, Category::MaybeLoss, Category::Loss),
            (
                Category::CursedWin,
                Category::MaybeLoss,
                Category::BlessedLoss,
            ),
            (
                Category::BlessedLoss,
                Category::MaybeWin,
                Category::CursedWin,
            ),
            (Category::Loss, Category::MaybeWin, Category::Win),
        ] {
            if category == prev {
                for m in &mut move_info {
                    if m.category != maybe {
                        break;
                    }
                    m.category = correct;
                }
            }
        }

        Ok(TablebaseResponse {
            pos: pos_info,
            category: move_info.first().map(|m| -m.category).unwrap_or(category),
            moves: move_info,
        })
    }

    fn mainline(&self, mut pos: VariantPosition) -> Result<MainlineResponse, SyzygyError> {
        let dtz = self.probe_dtz(&pos)?;
        let mut mainline = Vec::new();

        if !dtz.is_zero() {
            while pos.borrow().halfmoves() < 100 {
                if let Some((m, dtz)) = self.best_move(&pos)? {
                    mainline.push(MainlineStep {
                        uci: m.to_uci(pos.borrow().castles().mode()),
                        dtz: dtz.ignore_rounding(),
                        precise_dtz: dtz.precise(),
                        san: pos.san_plus_and_play_unchecked(&m),
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
            winner: pos
                .borrow()
                .outcome()
                .and_then(|o| o.winner())
                .map(|winner| winner.char()),
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
            TablebaseError::MissingFen => {
                warp::reply::with_status("missing fen".to_owned(), StatusCode::BAD_REQUEST)
            }
            TablebaseError::ParseFenError(_) => {
                warp::reply::with_status("invalid fen".to_owned(), StatusCode::BAD_REQUEST)
            }
            TablebaseError::PositionError(_) => {
                warp::reply::with_status("illegal fen".to_owned(), StatusCode::BAD_REQUEST)
            }
            TablebaseError::SyzygyError(err @ SyzygyError::Castling)
            | TablebaseError::SyzygyError(err @ SyzygyError::TooManyPieces) => {
                warp::reply::with_status(err.to_string(), StatusCode::NOT_FOUND)
            }
            TablebaseError::SyzygyError(err @ SyzygyError::MissingTable { .. }) => {
                warn!("{}", err);
                warp::reply::with_status(err.to_string(), StatusCode::NOT_FOUND)
            }
            TablebaseError::SyzygyError(err @ SyzygyError::ProbeFailed { .. }) => {
                error!("{}", err);
                warp::reply::with_status(err.to_string(), StatusCode::INTERNAL_SERVER_ERROR)
            }
        }
    }
}

impl warp::reject::Reject for TablebaseError {}

#[derive(Deserialize, Debug)]
struct Query {
    fen: String,
}

impl Query {
    fn lax_fen(&self) -> Result<Fen, ParseFenError> {
        str::replace(&self.fen, '_', " ").parse()
    }
}

fn try_probe(
    tbs: &Tablebases,
    variant: Variant,
    query: Query,
) -> Result<TablebaseResponse, TablebaseError> {
    let lax_fen = query.lax_fen()?;
    let pos = variant.position(&lax_fen)?;

    match pos {
        VariantPosition::Standard(ref pos) => info!("standard: {}", fen(pos)),
        VariantPosition::Atomic(ref pos) => info!("atomic: {}", fen(pos)),
        VariantPosition::Antichess(ref pos) => info!("antichess: {}", fen(pos)),
    }

    Ok(tbs.probe(&pos)?)
}

fn try_mainline(
    tbs: &Tablebases,
    variant: Variant,
    query: Query,
) -> Result<MainlineResponse, TablebaseError> {
    let lax_fen = query.lax_fen()?;
    let pos = variant.position(&lax_fen)?;

    match pos {
        VariantPosition::Standard(ref pos) => info!("standard mainline: {}", fen(pos)),
        VariantPosition::Atomic(ref pos) => info!("atomic mainline: {}", fen(pos)),
        VariantPosition::Antichess(ref pos) => info!("antichess mainline: {}", fen(pos)),
    }

    Ok(tbs.mainline(pos)?)
}

#[derive(Parser, Debug)]
struct Opt {
    /// Directory with tablebase files for standard chess.
    #[clap(long, multiple_occurrences = true, parse(from_os_str))]
    standard: Vec<PathBuf>,
    /// Directory with tablebase files for atomic chess.
    #[clap(long, multiple_occurrences = true, parse(from_os_str))]
    atomic: Vec<PathBuf>,
    /// Directory with tablebase files for antichess.
    #[clap(long, multiple_occurrences = true, parse(from_os_str))]
    antichess: Vec<PathBuf>,
    /// Directory with Gaviota tablebase files.
    #[clap(long, multiple_occurrences = true, parse(from_os_str))]
    gaviota: Vec<PathBuf>,

    /// Listen on this socket address.
    #[clap(long, default_value = "127.0.0.1:9000")]
    bind: SocketAddr,
}

#[tokio::main]
async fn main() {
    env_logger::init();

    // Parse arguments.
    let opt = Opt::parse();
    if opt.standard.is_empty()
        && opt.atomic.is_empty()
        && opt.antichess.is_empty()
        && opt.gaviota.is_empty()
    {
        Opt::into_app().print_help().expect("usage");
        println!();
        return;
    }

    // Initialize Syzygy tablebases.
    let tbs: &'static Tablebases = {
        let mut standard = SyzygyTablebase::<Chess>::new();
        let mut atomic = SyzygyTablebase::<Atomic>::new();
        let mut antichess = SyzygyTablebase::<Antichess>::new();

        for path in opt.standard {
            standard
                .add_directory(path)
                .expect("open standard directory");
        }
        for path in opt.atomic {
            atomic.add_directory(path).expect("open atomic directory");
        }
        for path in opt.antichess {
            antichess
                .add_directory(path)
                .expect("open antichess directory");
        }

        Box::leak(Box::new(Tablebases {
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
                drop(path);
            }
            assert!(!gaviota_sys::tb_init(
                1,
                gaviota_sys::TB_compression_scheme::tb_CP4 as c_int,
                paths
            )
            .is_null());
        }
    }

    // Start server.
    let probe = warp::get()
        .map(move || tbs)
        .and(warp::path!(Variant))
        .and(warp::query::query())
        .and_then(
            |tbs: &'static Tablebases, variant: Variant, query: Query| async move {
                tokio::task::spawn_blocking(move || match try_probe(tbs, variant, query) {
                    Ok(res) => Ok(warp::reply::json(&res)),
                    Err(err) => Err(warp::reject::custom(err)),
                })
                .await
                .expect("probe")
            },
        );
    let mainline = warp::get()
        .map(move || tbs)
        .and(warp::path!(Variant / "mainline"))
        .and(warp::query::query())
        .and_then(
            |tbs: &'static Tablebases, variant: Variant, query: Query| async move {
                tokio::task::spawn_blocking(move || match try_mainline(tbs, variant, query) {
                    Ok(res) => Ok(warp::reply::json(&res)),
                    Err(err) => Err(warp::reject::custom(err)),
                })
                .await
                .expect("mainline")
            },
        );
    let api = probe
        .or(mainline)
        .recover(|rejection: warp::reject::Rejection| async move {
            if rejection.find::<warp::reject::InvalidQuery>().is_some() {
                Ok(TablebaseError::MissingFen.to_response())
            } else if let Some(err) = rejection.find::<TablebaseError>() {
                Ok(err.to_response())
            } else {
                Err(rejection)
            }
        });
    warp::serve(api).run(opt.bind).await;
}
