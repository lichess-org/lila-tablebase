#![feature(plugin)]
#![feature(custom_derive)]
#![plugin(rocket_codegen)]

#![warn(bare_trait_objects)]

extern crate arrayvec;
extern crate gaviota_sys;
#[macro_use]
extern crate log;
extern crate rocket;
extern crate rocket_contrib;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate shakmaty;
extern crate shakmaty_syzygy;
#[macro_use]
extern crate structopt;

use arrayvec::ArrayVec;
use rocket::State;
use rocket::config::{Config, Environment};
use rocket::http::{RawStr, Status};
use rocket::request::FromParam;
use rocket::response::status;
use rocket_contrib::Json;
use shakmaty::fen::{Fen, FenError, FenOpts};
use shakmaty::san::SanPlus;
use shakmaty::uci::Uci;
use shakmaty::variants::{Atomic, Chess, Giveaway};
use shakmaty::{Move, MoveList, Outcome, Position, PositionError, Role, Setup};
use shakmaty_syzygy::{Dtz, SyzygyError, Tablebase as SyzygyTablebase, Wdl};
use std::borrow::Borrow;
use std::cmp::{min, Reverse};
use std::ffi::CString;
use std::os::raw::{c_int, c_uchar, c_uint};
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(Debug)]
enum Variant {
    Standard,
    Atomic,
    Antichess,
}

impl<'a> FromParam<'a> for Variant {
    type Error = &'a RawStr;

    fn from_param(param: &'a RawStr) -> Result<Self, Self::Error> {
        match param.percent_decode().map_err(|_| param)?.borrow() {
            "standard" => Ok(Variant::Standard),
            "atomic" => Ok(Variant::Atomic),
            "antichess" => Ok(Variant::Antichess),
            _ => Err(param),
        }
    }
}

impl Variant {
    fn position(&self, fen: &Fen) -> Result<VariantPosition, PositionError> {
        match self {
            Variant::Standard => fen.position().map(VariantPosition::Standard),
            Variant::Atomic => fen.position().map(VariantPosition::Atomic),
            Variant::Antichess => fen.position().map(VariantPosition::Antichess),
        }
    }
}

#[derive(Clone)]
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

#[derive(Serialize)]
struct TablebaseResponse {
    #[serde(flatten)]
    pos: PositionInfo,
    moves: ArrayVec<[MoveInfo; 256]>,
}

#[derive(Serialize)]
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

#[derive(Serialize)]
struct PositionInfo {
    checkmate: bool,
    stalemate: bool,
    variant_win: bool,
    variant_loss: bool,
    insufficient_material: bool,
    wdl: Option<Wdl>,
    dtz: Option<Dtz>,
    dtm: Option<i32>,
}

#[derive(Serialize)]
struct MainlineResponse {
    mainline: Vec<MainlineStep>,
    winner: Option<char>,
    dtz: Dtz,
}

#[derive(Serialize)]
struct MainlineStep {
    uci: String,
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

struct Tablebases {
    standard: SyzygyTablebase<Chess>,
    atomic: SyzygyTablebase<Atomic>,
    antichess: SyzygyTablebase<Giveaway>,
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

        let halfmove_clock = min(101, pos.borrow().halfmove_clock()) as i32;
        let real_dtz = dtz.add_plies(halfmove_clock);

        if real_dtz.0.abs() != 100 || halfmove_clock == 0 {
            // Unambiguous.
            return Ok(Wdl::from_dtz_after_zeroing(real_dtz));
        }

        if halfmove_clock == 1 && dtz.0.abs() == 99 {
            // This could only be a cursed/blessed result if the DTZ was
            // actually 100 instead of 99. But tables with DTZ 100 will always
            // store precise DTZ values, hence it could not have been 100.
            return Ok(Wdl::from_dtz_after_zeroing(real_dtz));
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
            while pos.borrow().halfmove_clock() < 100 {
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

#[derive(FromForm)]
struct QueryString {
    fen: String,
}

impl QueryString {
    fn fen(&self) -> Result<Fen, FenError> {
        str::replace(&self.fen, '_', " ").parse()
    }
}

#[get("/<variant>?<query>")]
fn probe(tablebases: State<Tablebases>, variant: Variant, query: QueryString) -> Result<Json<TablebaseResponse>, status::Custom<&'static str>> {
    let fen = match query.fen() {
        Ok(fen) => fen,
        Err(_) => return Err(status::Custom(Status::BadRequest, "fen invalid")),
    };

    let pos = match variant.position(&fen) {
        Ok(pos) => pos,
        Err(_) => return Err(status::Custom(Status::BadRequest, "fen illegal")),
    };

    match pos {
        VariantPosition::Standard(ref pos) =>
            info!("standard: {}", FenOpts::default().fen(pos)),
        VariantPosition::Atomic(ref pos) =>
            info!("atomic: {}", FenOpts::default().fen(pos)),
        VariantPosition::Antichess(ref pos) =>
            info!("antichess: {}", FenOpts::default().promoted(true).fen(pos)),
    }

    match tablebases.probe(&pos) {
        Ok(res) => Ok(Json(res)),
        Err(err) => {
            error!("probe failed: {} ({:?} position {})", err.to_string(), variant, fen);
            Err(status::Custom(Status::InternalServerError, "probe failed"))
        }
    }
}

#[get("/<variant>/mainline?<query>")]
fn mainline(tablebases: State<Tablebases>, variant: Variant, query: QueryString) -> Result<Json<MainlineResponse>, status::Custom<&'static str>> {
    let fen = match query.fen() {
        Ok(fen) => fen,
        Err(_) => return Err(status::Custom(Status::BadRequest, "fen invalid")),
    };

    let pos = match variant.position(&fen) {
        Ok(pos) => pos,
        Err(_) => return Err(status::Custom(Status::BadRequest, "fen illegal")),
    };

    match pos {
        VariantPosition::Standard(ref pos) =>
            info!("standard mainline: {}", FenOpts::default().fen(pos)),
        VariantPosition::Atomic(ref pos) =>
            info!("atomic mainline: {}", FenOpts::default().fen(pos)),
        VariantPosition::Antichess(ref pos) =>
            info!("antichess mainline: {}", FenOpts::default().promoted(true).fen(pos)),
    }

    match tablebases.mainline(pos) {
        Ok(res) => Ok(Json(res)),
        Err(SyzygyError::Castling) | Err(SyzygyError::TooManyPieces) | Err(SyzygyError::MissingTable { .. }) => {
            Err(status::Custom(Status::NotFound, "position not found in tablebase"))
        }
        Err(err) => {
            error!("mainline probe failed: {} ({:?} position {})", err.to_string(), variant, fen);
            Err(status::Custom(Status::InternalServerError, "failed to probe mainline"))
        }
    }
}

#[derive(StructOpt)]
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

    /// Listen on this address.
    #[structopt(long = "address", default_value = "127.0.0.1")]
    address: String,
    /// Listen on this port.
    #[structopt(long = "port", default_value = "9000")]
    port: u16,
    /// Number of worker threads.
    #[structopt(long = "workers", default_value = "5")]
    workers: u16
}

fn main() {
    // Parse arguments.
    let opt = Opt::from_args();
    if opt.standard.is_empty() && opt.atomic.is_empty() && opt.antichess.is_empty() && opt.gaviota.is_empty() {
        Opt::clap().print_help().expect("usage");
        println!();
        return;
    }

    let config = Config::build(Environment::Production)
        .address(opt.address)
        .port(opt.port)
        .workers(opt.workers)
        .secret_key("+++++++++++++unused++secret++key++++++++++++")
        .unwrap();

    // Initialize Syzygy tablebases.
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
    rocket::custom(config, true)
        .manage(Tablebases { standard, atomic, antichess })
        .mount("/", routes![probe, mainline])
        .launch();
}
