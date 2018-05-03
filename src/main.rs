extern crate actix;
extern crate actix_web;
extern crate arrayvec;
extern crate gaviota_sys;
#[macro_use]
extern crate log;
extern crate env_logger;
extern crate futures;
extern crate serde;
extern crate serde_json;
extern crate shakmaty;
extern crate shakmaty_syzygy;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate structopt;

use actix::dev::Request;
use actix::{Actor, Addr, Handler, Message, Syn, SyncArbiter, SyncContext};
use actix_web::{server, App, AsyncResponder, Error, HttpRequest, HttpResponse, Result};
use arrayvec::ArrayVec;
use futures::future::{ok, Future};
use shakmaty::fen::{Fen, FenOpts};
use shakmaty::san::{san_plus, SanPlus};
use shakmaty::uci::{uci, Uci};
use shakmaty::variants::{Atomic, Chess, Giveaway};
use shakmaty::{Move, MoveList, Outcome, Position, Setup, Role};
use shakmaty_syzygy::{Dtz, SyzygyError, Tablebase as SyzygyTablebase, Wdl};
use structopt::StructOpt;
use std::cmp::{min, Reverse};
use std::path::PathBuf;
use std::sync::Arc;
use std::ffi::CString;
use std::os::raw::{c_int, c_uchar, c_uint};

struct State {
    tablebase: TablebaseStub,
}

#[derive(Clone)]
enum VariantPosition {
    Standard(Chess),
    Atomic(Atomic),
    Antichess(Giveaway),
}

impl VariantPosition {
    fn borrow(&self) -> &Position {
        match *self {
            VariantPosition::Standard(ref pos) => pos,
            VariantPosition::Atomic(ref pos) => pos,
            VariantPosition::Antichess(ref pos) => pos,
        }
    }

    fn borrow_mut(&mut self) -> &mut Position {
        match *self {
            VariantPosition::Standard(ref mut pos) => pos,
            VariantPosition::Atomic(ref mut pos) => pos,
            VariantPosition::Antichess(ref mut pos) => pos,
        }
    }

    fn uci(&self, m: &Move) -> Uci {
        match *self {
            VariantPosition::Standard(ref pos) => uci(pos, m),
            VariantPosition::Atomic(ref pos) => uci(pos, m),
            VariantPosition::Antichess(ref pos) => uci(pos, m),
        }
    }

    fn san_plus(self, m: &Move) -> SanPlus {
        match self {
            VariantPosition::Standard(pos) => san_plus(pos, m),
            VariantPosition::Atomic(pos) => san_plus(pos, m),
            VariantPosition::Antichess(pos) => san_plus(pos, m),
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
    #[serde(flatten)]
    pos: PositionInfo,
}

impl MoveInfo {
    fn order(&self) -> MoveOrder {
        if self.pos.checkmate {
            MoveOrder::Checkmate
        } else if self.pos.variant_loss {
            MoveOrder::VariantLoss
        } else if self.pos.variant_win {
            MoveOrder::VariantWin
        } else if self.pos.stalemate {
            MoveOrder::Stalemate
        } else if self.pos.insufficient_material {
            MoveOrder::InsufficientMaterial
        } else if let (Some(wdl), Some(dtz)) = (self.pos.wdl, self.pos.dtz) {
            if wdl == Wdl::CursedWin {
                MoveOrder::CursedWin { dtz: Reverse(dtz) }
            } else if wdl == Wdl::BlessedLoss {
                MoveOrder::BlessedLoss { dtz: Reverse(dtz) }
            } else if self.zeroing {
                if wdl == Wdl::Win {
                    MoveOrder::WinningZeroing {
                        capture: self.capture.map(Reverse),
                        dtz: Reverse(dtz),
                    }
                } else if wdl == Wdl::Loss {
                    MoveOrder::LosingZeroing {
                        capture: Reverse(self.capture),
                        dtz: Reverse(dtz),
                    }
                } else {
                    MoveOrder::ZeroingDraw
                }
            } else if wdl == Wdl::Win {
                MoveOrder::Winning { dtz: Reverse(dtz) }
            } else if wdl == Wdl::Loss {
                MoveOrder::Losing { dtz: Reverse(dtz) }
            } else {
                MoveOrder::Draw
            }
        } else {
            MoveOrder::Unknown
        }
    }
}

#[derive(Eq, PartialEq, Ord, PartialOrd)]
enum MoveOrder {
    Checkmate,
    VariantLoss,
    LosingZeroing {
        capture: Reverse<Option<Role>>,
        dtz: Reverse<Dtz>,
    },
    Losing {
        dtz: Reverse<Dtz>,
    },
    Unknown,
    BlessedLoss {
        dtz: Reverse<Dtz>,
    },
    Stalemate,
    InsufficientMaterial,
    ZeroingDraw,
    Draw,
    CursedWin {
        dtz: Reverse<Dtz>,
    },
    Winning {
        dtz: Reverse<Dtz>,
    },
    WinningZeroing {
        capture: Option<Reverse<Role>>,
        dtz: Reverse<Dtz>,
    },
    VariantWin,
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

#[derive(Clone)]
struct TablebaseStub {
    addr: Addr<Syn, Tablebase>,
}

impl TablebaseStub {
    fn new(addr: Addr<Syn, Tablebase>) -> TablebaseStub {
        TablebaseStub { addr }
    }

    fn query(&self, pos: VariantPosition) -> Request<Syn, Tablebase, VariantPosition> {
        self.addr.send(pos)
    }
}

unsafe fn probe_dtm(pos: &VariantPosition) -> Option<i32> {
    let pos = match *pos {
        VariantPosition::Standard(ref pos) => pos,
        _ => return None,
    };

    if pos.board().occupied().count() > 5 || pos.castling_rights().any() {
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
        if piece.color.is_white() {
            ws.push(c_uint::from(sq));
            wp.push(piece.role as c_uchar + 1);
        } else {
            bs.push(c_uint::from(sq));
            bp.push(piece.role as c_uchar + 1);
        }
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
        0,
        ws.as_ptr(),
        bs.as_ptr(),
        wp.as_ptr(),
        bp.as_ptr(),
        &mut info as *mut c_uint,
        &mut plies as *mut c_uint);

    let plies = plies as i32;

    if result == 0 {
        warn!("gaviota_sys::tb_probe_hard() returned {}", result);
        return None;
    }

    match gaviota_sys::TB_return_values(info) {
        gaviota_sys::TB_return_values::tb_DRAW => Some(0),
        gaviota_sys::TB_return_values::tb_WMATE => Some(pos.turn().fold(plies, -plies)),
        gaviota_sys::TB_return_values::tb_BMATE => Some(pos.turn().fold(-plies, plies)),
        _ => {
            warn!("gaviota probe failed with info {}", info);
            None
        }
    }
}

struct Tablebase {
    standard: Arc<SyzygyTablebase<Chess>>,
    atomic: Arc<SyzygyTablebase<Atomic>>,
    antichess: Arc<SyzygyTablebase<Giveaway>>,
}

impl Tablebase {
    fn probe_wdl(&self, pos: &VariantPosition) -> Result<Wdl, SyzygyError> {
        match *pos {
            VariantPosition::Standard(ref pos) => self.standard.probe_wdl(pos),
            VariantPosition::Atomic(ref pos) => self.atomic.probe_wdl(pos),
            VariantPosition::Antichess(ref pos) => self.antichess.probe_wdl(pos),
        }
    }

    fn probe_dtz(&self, pos: &VariantPosition) -> Result<Dtz, SyzygyError> {
        match *pos {
            VariantPosition::Standard(ref pos) => self.standard.probe_dtz(pos),
            VariantPosition::Atomic(ref pos) => self.atomic.probe_dtz(pos),
            VariantPosition::Antichess(ref pos) => self.antichess.probe_dtz(pos),
        }
    }

    fn best_move(&self, pos: &VariantPosition) -> Result<Option<(Move, Dtz)>, SyzygyError> {
        struct MoveEval {
            m: Move,
            zeroing: bool,
            wdl: Wdl,
            dtz: Dtz,
        }

        let mut legals = MoveList::new();
        pos.borrow().legal_moves(&mut legals);

        let mut moves: ArrayVec<[MoveEval; 256]> = ArrayVec::new();
        for m in legals {
            let mut after = pos.clone();
            after.borrow_mut().play_unchecked(&m);

            moves.push(MoveEval {
                zeroing: m.is_zeroing(),
                m,
                wdl: self.probe_wdl(&after)?,
                dtz: self.probe_dtz(&after)?,
            });
        }

        Ok(moves.iter().min_by_key(|m| (
            m.wdl,
            if m.wdl > Wdl::Draw { m.zeroing } else { !m.zeroing },
            m.dtz,
        )).map(|m| (m.m.clone(), m.dtz)))
    }

    fn real_wdl(&self, pos: &VariantPosition, dtz: Dtz) -> Result<Wdl, SyzygyError> {
        let halfmove_clock = min(101, pos.borrow().halfmove_clock()) as i32;
        if halfmove_clock == 0 {
            return self.probe_wdl(pos);
        }

        if let Some(ref outcome) = pos.borrow().outcome() {
            return Ok(Wdl::from_outcome(outcome, pos.borrow().turn()));
        }

        let dtz = dtz.add_plies(halfmove_clock);
        if dtz.0.abs() != 100 {
            return Ok(Wdl::from_dtz_after_zeroing(dtz));
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
}

impl Actor for Tablebase {
    type Context = SyncContext<Self>;
}

impl Handler<VariantPosition> for Tablebase {
    type Result = Result<TablebaseResponse, SyzygyError>;

    fn handle(&mut self, pos: VariantPosition, _: &mut Self::Context) -> Self::Result {
        match pos {
            VariantPosition::Standard(ref pos) =>
                info!("standard fen: {}", FenOpts::default().fen(pos)),
            VariantPosition::Atomic(ref pos) =>
                info!("atomic fen: {}", FenOpts::default().fen(pos)),
            VariantPosition::Antichess(ref pos) =>
                info!("antichess fen: {}", FenOpts::default().promoted(true).fen(pos)),
        }

        let mut moves = MoveList::new();
        pos.borrow().legal_moves(&mut moves);
        moves.sort_by_key(|m| Reverse(m.promotion()));

        let mut move_info = ArrayVec::new();

        for m in moves {
            let mut after = pos.clone();
            after.borrow_mut().play_unchecked(&m);

            move_info.push(MoveInfo {
                uci: pos.uci(&m).to_string(),
                san: pos.clone().san_plus(&m).to_string(),
                pos: self.position_info(&after)?,
                capture: m.capture(),
                zeroing: m.is_zeroing(),
            });
        }

        move_info.sort_by_key(|m: &MoveInfo| m.order());

        Ok(TablebaseResponse {
            pos: self.position_info(&pos)?,
            moves: move_info,
        })
    }
}

impl Message for VariantPosition {
    type Result = Result<TablebaseResponse, SyzygyError>;
}

fn api(req: HttpRequest<State>) -> Box<Future<Item = HttpResponse, Error = Error>> {
    let fen = if let Some(fen) = req.query().get("fen") {
        fen
    } else {
        return Box::new(ok(HttpResponse::BadRequest().body("fen missing")));
    };

    let fen = if let Ok(fen) = fen.parse::<Fen>() {
        fen
    } else {
        return Box::new(ok(HttpResponse::BadRequest().body("fen invalid")));
    };

    let pos = match req.match_info().get("variant") {
        Some("standard") => fen.position().map(VariantPosition::Standard),
        Some("atomic") => fen.position().map(VariantPosition::Atomic),
        Some("antichess") => fen.position().map(VariantPosition::Antichess),
        _ => return Box::new(ok(HttpResponse::NotFound().body("variant not found"))),
    };

    let pos = if let Ok(pos) = pos {
        pos
    } else {
        return Box::new(ok(HttpResponse::BadRequest().body("fen illegal")));
    };

    req.state().tablebase.query(pos)
        .from_err()
        .map(|res| match res {
            Ok(res) => HttpResponse::Ok().json(res),
            Err(err) => {
                error!("probe failed: {}", err.to_string());
                HttpResponse::InternalServerError().body(err.to_string())
            },
        })
        .responder()
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
    /// Bind the HTTP server on this address.
    #[structopt(long = "bind", default_value = "127.0.0.1:8080")]
    bind: String,
    /// Number of HTTP server threads.
    #[structopt(long = "threads", default_value = "1")]
    threads: usize,
    /// Number of probing threads. A good default is the number of disks.
    #[structopt(long = "disks", default_value = "5")]
    disks: usize,
}

fn main() {
    let opt = Opt::from_args();

    let _ = env_logger::Builder::from_default_env()
        .default_format_timestamp(false)
        .init();

    let system = actix::System::new("lila-tablebase");

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

    let standard = Arc::new(standard);
    let atomic = Arc::new(atomic);
    let antichess = Arc::new(antichess);

    let tablebase = TablebaseStub::new(SyncArbiter::start(opt.disks, move || Tablebase {
        standard: standard.clone(),
        atomic: atomic.clone(),
        antichess: antichess.clone(),
    }));

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

    let server = server::new(move || {
        App::with_state(State { tablebase: tablebase.clone() })
            .resource("/{variant}", |r| r.get().f(api))
    });

    server.threads(opt.threads).bind(opt.bind).unwrap().start();
    system.run();
}
