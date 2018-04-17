extern crate actix;
extern crate actix_web;
extern crate arrayvec;
extern crate clap;
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

use actix::dev::Request;
use actix::{Actor, Addr, Handler, Message, Syn, SyncArbiter, SyncContext};
use actix_web::{server, App, AsyncResponder, Error, HttpRequest, HttpResponse, Result};
use arrayvec::ArrayVec;
use futures::future::{ok, Future};
use shakmaty::fen::{Fen, FenOpts};
use shakmaty::san::{san_plus, SanPlus};
use shakmaty::uci::{uci, Uci};
use shakmaty::variants::{Atomic, Chess, Giveaway};
use shakmaty::{Color, Move, MoveList, Outcome, Position, Role, Setup};
use shakmaty_syzygy::{Dtz, SyzygyError, Tablebases, Wdl};
use std::cmp::{min, Reverse};
use std::sync::Arc;

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
    fn is_checkmate(&self) -> bool {
        match *self {
            VariantPosition::Standard(ref pos) => pos.is_checkmate(),
            VariantPosition::Atomic(ref pos) => pos.is_checkmate(),
            VariantPosition::Antichess(ref pos) => pos.is_checkmate(),
        }
    }

    fn is_stalemate(&self) -> bool {
        match *self {
            VariantPosition::Standard(ref pos) => pos.is_stalemate(),
            VariantPosition::Atomic(ref pos) => pos.is_stalemate(),
            VariantPosition::Antichess(ref pos) => pos.is_stalemate(),
        }
    }

    fn turn(&self) -> Color {
        match *self {
            VariantPosition::Standard(ref pos) => pos.turn(),
            VariantPosition::Atomic(ref pos) => pos.turn(),
            VariantPosition::Antichess(ref pos) => pos.turn(),
        }
    }

    fn is_insufficient_material(&self) -> bool {
        match *self {
            VariantPosition::Standard(ref pos) => pos.is_insufficient_material(),
            VariantPosition::Atomic(ref pos) => pos.is_insufficient_material(),
            VariantPosition::Antichess(ref pos) => pos.is_insufficient_material(),
        }
    }

    fn outcome(&self) -> Option<Outcome> {
        match *self {
            VariantPosition::Standard(ref pos) => pos.outcome(),
            VariantPosition::Atomic(ref pos) => pos.outcome(),
            VariantPosition::Antichess(ref pos) => pos.outcome(),
        }
    }

    fn variant_outcome(&self) -> Option<Outcome> {
        match *self {
            VariantPosition::Standard(ref pos) => pos.variant_outcome(),
            VariantPosition::Atomic(ref pos) => pos.variant_outcome(),
            VariantPosition::Antichess(ref pos) => pos.variant_outcome(),
        }
    }

    fn halfmove_clock(&self) -> u32 {
        match *self {
            VariantPosition::Standard(ref pos) => pos.halfmove_clock(),
            VariantPosition::Atomic(ref pos) => pos.halfmove_clock(),
            VariantPosition::Antichess(ref pos) => pos.halfmove_clock(),
        }
    }

    fn legal_moves(&self, moves: &mut MoveList) {
        match *self {
            VariantPosition::Standard(ref pos) => pos.legal_moves(moves),
            VariantPosition::Atomic(ref pos) => pos.legal_moves(moves),
            VariantPosition::Antichess(ref pos) => pos.legal_moves(moves),
        }
    }

    fn play_unchecked(&mut self, m: &Move) {
        match *self {
            VariantPosition::Standard(ref mut pos) => pos.play_unchecked(m),
            VariantPosition::Atomic(ref mut pos) => pos.play_unchecked(m),
            VariantPosition::Antichess(ref mut pos) => pos.play_unchecked(m),
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

struct Tablebase {
    standard: Arc<Tablebases<Chess>>,
    atomic: Arc<Tablebases<Atomic>>,
    antichess: Arc<Tablebases<Giveaway>>,
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
        pos.legal_moves(&mut legals);

        let mut moves: ArrayVec<[MoveEval; 256]> = ArrayVec::new();
        for m in legals {
            moves.push(MoveEval {
                zeroing: m.is_zeroing(),
                m,
                wdl: self.probe_wdl(pos)?,
                dtz: self.probe_dtz(pos)?,
            });
        }

        moves.sort_unstable_by_key(|m| m.dtz);
        moves.sort_by_key(|m| if m.wdl > Wdl::Draw { m.zeroing } else { !m.zeroing });

        Ok(moves.first().map(|m| (m.m.clone(), m.dtz)))
    }

    fn real_wdl(&self, pos: &VariantPosition, dtz: Dtz) -> Result<Wdl, SyzygyError> {
        let halfmove_clock = min(101, pos.halfmove_clock()) as i16;
        if halfmove_clock == 0 {
            return self.probe_wdl(pos);
        }

        if let Some(ref outcome) = pos.outcome() {
            return Ok(Wdl::from_outcome(outcome, pos.turn()));
        }

        let dtz = dtz.add_plies(halfmove_clock);
        if dtz.0.abs() != 100 {
            return Ok(Wdl::from_dtz_after_zeroing(dtz));
        }

        let best = self.best_move(pos)?.expect("has moves");
        let mut after = pos.clone();
        after.play_unchecked(&best.0);
        Ok(-self.real_wdl(&after, best.1)?)
    }

    fn position_info(&self, pos: &VariantPosition) -> Result<PositionInfo, SyzygyError> {
        let (variant_win, variant_loss) = match pos.variant_outcome() {
            Some(Outcome::Decisive { winner }) => (winner == pos.turn(), winner != pos.turn()),
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

        let wdl = if let Some(dtz) = dtz {
            user_error_as_none(self.real_wdl(pos, dtz))?
        } else {
            None
        };

        Ok(PositionInfo {
            checkmate: pos.is_checkmate(),
            stalemate: pos.is_stalemate(),
            variant_win,
            variant_loss,
            insufficient_material: pos.is_insufficient_material(),
            dtz,
            wdl,
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
        pos.legal_moves(&mut moves);
        moves.sort_by_key(|m| Reverse(m.promotion()));

        let mut move_info = ArrayVec::new();

        for m in moves {
            let mut after = pos.clone();
            after.play_unchecked(&m);

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

fn api_v1(req: HttpRequest<State>) -> Box<Future<Item = HttpResponse, Error = Error>> {
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
        Some("standard") | Some("chess960") => fen.position().map(VariantPosition::Standard),
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
            Err(err) => HttpResponse::InternalServerError().body(err.to_string()),
        })
        .responder()
}

fn main() {
    let args = clap::App::new("lila-tablebase")
        .version("0.1.0")
        .about("Tablebase server for lichess.org")
        .author("Niklas Fiekas")
        .arg(clap::Arg::with_name("standard")
            .long("standard")
            .takes_value(true)
            .help("Directory with .rtbw and .rtbz files")
            .multiple(true))
        .arg(clap::Arg::with_name("atomic")
            .long("atomic")
            .takes_value(true)
            .help("Directory with .atbw and .atbz files")
            .multiple(true))
        .arg(clap::Arg::with_name("antichess")
            .long("antichess")
            .takes_value(true)
            .help("Directory with .gtbw, .gtbz, and pawnless .stbw and .stbz files")
            .multiple(true))
        .arg(clap::Arg::with_name("bind")
            .long("bind")
            .help("Listening address")
            .default_value("127.0.0.1:8080"))
        .get_matches();

    ::std::env::set_var("RUST_LOG", "info");
    let _ = env_logger::init();
    let system = actix::System::new("lila-tablebase");

    let mut standard = Tablebases::<Chess>::new();
    let mut atomic = Tablebases::<Atomic>::new();
    let mut antichess = Tablebases::<Giveaway>::new();

    if let Some(paths) = args.values_of_os("standard") {
        for path in paths {
            standard.add_directory(path).expect("open standard directory");
        }
    }

    if let Some(paths) = args.values_of_os("atomic") {
        for path in paths {
            atomic.add_directory(path).expect("open atomic directory");
        }
    }

    if let Some(paths) = args.values_of_os("antichess") {
        for path in paths {
            antichess.add_directory(path).expect("open antichess directory");
        }
    }

    let standard = Arc::new(standard);
    let atomic = Arc::new(atomic);
    let antichess = Arc::new(antichess);

    // Throughput of the tablebase actor is limited by rotating disk I/O.
    let num_disks = 5;
    // Throughput should not be limited by the HTTP server itself.
    let num_threads = 1;

    let tablebase = TablebaseStub::new(SyncArbiter::start(num_disks, move || Tablebase {
        standard: standard.clone(),
        atomic: atomic.clone(),
        antichess: antichess.clone(),
    }));

    let server = server::new(move || {
        App::with_state(State { tablebase: tablebase.clone() })
            .resource("/{variant}", |r| r.get().f(api_v1))
    });

    server.threads(num_threads).bind(args.value_of("bind").unwrap()).unwrap().start();
    system.run();
}
