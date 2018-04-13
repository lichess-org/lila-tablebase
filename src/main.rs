extern crate arrayvec;
extern crate actix;
extern crate actix_web;
extern crate clap;
extern crate env_logger;
extern crate shakmaty;
extern crate shakmaty_syzygy;
extern crate futures;
extern crate serde;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;

use std::cmp::min;
use std::sync::Arc;
use arrayvec::ArrayVec;
use actix::{Addr, Syn, Message, Actor, SyncContext, SyncArbiter, Handler, MailboxError};
use actix_web::{server, App, HttpRequest, HttpResponse, AsyncResponder, Error, Result};
use shakmaty::{Color, Move, Chess, Setup, Position, MoveList, Outcome};
use shakmaty::fen::Fen;
use shakmaty::uci::{uci, Uci};
use shakmaty::san::{san, San};
use shakmaty_syzygy::{Tablebases, Wdl, Dtz, SyzygyError};
use futures::future::{Future, ok};

struct State {
    tablebase: TablebaseStub,
}

#[derive(Clone)]
enum VariantPosition {
    Standard(Chess),
}

impl VariantPosition {
    fn is_checkmate(&self) -> bool {
        match self {
            VariantPosition::Standard(pos) => pos.is_checkmate(),
        }
    }

    fn is_stalemate(&self) -> bool {
        match self {
            VariantPosition::Standard(pos) => pos.is_stalemate(),
        }
    }

    fn turn(&self) -> Color {
        match self {
            VariantPosition::Standard(pos) => pos.turn(),
        }
    }

    fn is_insufficient_material(&self) -> bool {
        match self {
            VariantPosition::Standard(pos) => pos.is_insufficient_material(),
        }
    }

    fn outcome(&self) -> Option<Outcome> {
        match self {
            VariantPosition::Standard(pos) => pos.outcome(),
        }
    }

    fn variant_outcome(&self) -> Option<Outcome> {
        match self {
            VariantPosition::Standard(pos) => pos.variant_outcome(),
        }
    }

    fn halfmove_clock(&self) -> u32 {
        match self {
            VariantPosition::Standard(pos) => pos.halfmove_clock(),
        }
    }

    fn legal_moves(&self, moves: &mut MoveList) {
        match self {
            VariantPosition::Standard(pos) => pos.legal_moves(moves),
        }
    }

    fn play_unchecked(&mut self, m: &Move) {
        match self {
            VariantPosition::Standard(pos) => pos.play_unchecked(m),
        }
    }

    fn uci(&self, m: &Move) -> Uci {
        match self {
            VariantPosition::Standard(pos) => uci(pos, m)
        }
    }

    fn san(&self, m: &Move) -> San {
        match self {
            VariantPosition::Standard(pos) => san(pos, m)
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
                MoveOrder::CursedWin { dtz }
            } else if wdl == Wdl::BlessedLoss {
                MoveOrder::BlessedLoss { dtz }
            } else if self.zeroing {
                if wdl == Wdl::Win  {
                    MoveOrder::WinningZeroing { dtz }
                } else if wdl == Wdl::Loss {
                    MoveOrder::LosingZeroing { dtz }
                } else {
                    MoveOrder::ZeroingDraw
                }
            } else {
                if wdl == Wdl::Win {
                    MoveOrder::Winning { dtz }
                } else if wdl == Wdl::Loss {
                    MoveOrder::Losing { dtz }
                } else {
                    MoveOrder::Draw
                }
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
    WinningZeroing {
        dtz: Dtz,
    },
    Winning {
        dtz: Dtz,
    },
    Unknown,
    CursedWin {
        dtz: Dtz,
    },
    Stalemate,
    InsufficientMaterial,
    ZeroingDraw,
    Draw,
    BlessedLoss {
        dtz: Dtz,
    },
    Losing {
        dtz: Dtz,
    },
    LosingZeroing {
        dtz: Dtz,
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

    fn query(&self, pos: VariantPosition) -> impl Future<Item=Result<TablebaseResponse, SyzygyError>, Error=MailboxError> {
        self.addr.send(pos)
    }
}

struct Tablebase {
    standard: Arc<Tablebases<Chess>>,
}

impl Tablebase {
    fn position_info(&self, pos: &VariantPosition) -> Result<PositionInfo, SyzygyError> {
        let (variant_win, variant_loss) = match pos.variant_outcome() {
            Some(Outcome::Decisive { winner }) =>
                (winner == pos.turn(), winner != pos.turn()),
            _ =>
                (false, false),
        };

        let halfmove_clock = min(101, pos.halfmove_clock()) as i16;

        let dtz = match pos {
            VariantPosition::Standard(pos) => self.standard.probe_dtz(pos),
        };

        let dtz = match dtz {
            Err(SyzygyError::Castling) |
            Err(SyzygyError::TooManyPieces) |
            Err(SyzygyError::MissingTable { .. }) =>
                None, // acceptable errors
            _ =>
                Some(dtz?),
        };

        let wdl = pos.outcome()
            .map(|o| Wdl::from_outcome(&o, pos.turn()))
            .or_else(|| {
                dtz.map(|dtz| Wdl::from(Dtz(dtz.0.signum() * (dtz.0.abs() + halfmove_clock))))
            });

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
        let mut moves = MoveList::new();
        pos.legal_moves(&mut moves);

        let mut move_info = ArrayVec::new();

        for m in moves {
            let mut after = pos.clone();
            after.play_unchecked(&m);

            move_info.push(MoveInfo {
                uci: pos.uci(&m).to_string(),
                san: pos.san(&m).to_string(),
                pos: self.position_info(&after)?,
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

fn api_v1(req: HttpRequest<State>) -> Box<Future<Item=HttpResponse, Error=Error>> {
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
            .default_value("127.0.0.1:8080"))
        .get_matches();

    ::std::env::set_var("RUST_LOG", "actix_web=info");
    let _ = env_logger::init();
    let system = actix::System::new("lila-tablebase");

    let mut standard = Tablebases::<Chess>::new();

    if let Some(paths) = args.values_of_os("standard") {
        for path in paths {
            standard.add_directory(path).expect("open standard directory");
        }
    }

    let standard = Arc::new(standard);

    let tablebase = TablebaseStub::new(SyncArbiter::start(4, move || Tablebase {
        standard: standard.clone(),
    }));

    let server = server::new(move || {
        App::with_state(State { tablebase: tablebase.clone() })
            .resource("/api/v1/{variant}", |r| r.get().f(api_v1))
    });

    server.bind(args.value_of("bind").unwrap()).unwrap().start();
    system.run();
}
