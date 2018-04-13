extern crate arrayvec;
extern crate actix;
extern crate actix_web;
extern crate env_logger;
extern crate shakmaty;
extern crate shakmaty_syzygy;
extern crate futures;
extern crate serde;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;

use std::cmp::{min, Ord, PartialOrd, Ordering};
use std::sync::Arc;
use arrayvec::ArrayVec;
use actix::{Addr, Syn, Message, Actor, SyncContext, SyncArbiter, Handler, MailboxError};
use actix_web::{server, App, HttpRequest, HttpResponse, AsyncResponder, Error, Result};
use shakmaty::{Color, Move, Chess, Setup, Position, MoveList, Outcome};
use shakmaty::uci::{uci, Uci};
use shakmaty::san::{san, San};
use shakmaty_syzygy::{Tablebases, Wdl, Dtz, SyzygyError, Syzygy};
use futures::future::{Future, ok};

struct State {
    tablebase: TablebaseStub,
}

#[derive(Clone)]
enum VariantPosition {
    Regular(Chess),
}

impl VariantPosition {
    fn is_checkmate(&self) -> bool {
        match self {
            VariantPosition::Regular(pos) => pos.is_checkmate(),
        }
    }

    fn is_stalemate(&self) -> bool {
        match self {
            VariantPosition::Regular(pos) => pos.is_stalemate(),
        }
    }

    fn turn(&self) -> Color {
        match self {
            VariantPosition::Regular(pos) => pos.turn(),
        }
    }

    fn is_insufficient_material(&self) -> bool {
        match self {
            VariantPosition::Regular(pos) => pos.is_insufficient_material(),
        }
    }

    fn outcome(&self) -> Option<Outcome> {
        match self {
            VariantPosition::Regular(pos) => pos.outcome(),
        }
    }

    fn variant_outcome(&self) -> Option<Outcome> {
        match self {
            VariantPosition::Regular(pos) => pos.variant_outcome(),
        }
    }

    fn halfmove_clock(&self) -> u32 {
        match self {
            VariantPosition::Regular(pos) => pos.halfmove_clock(),
        }
    }

    fn legal_moves(&self, moves: &mut MoveList) {
        match self {
            VariantPosition::Regular(pos) => pos.legal_moves(moves),
        }
    }

    fn play_unchecked(&mut self, m: &Move) {
        match self {
            VariantPosition::Regular(pos) => pos.play_unchecked(m),
        }
    }

    fn uci(&self, m: &Move) -> Uci {
        match self {
            VariantPosition::Regular(pos) => uci(pos, m)
        }
    }

    fn san(&self, m: &Move) -> San {
        match self {
            VariantPosition::Regular(pos) => san(pos, m)
        }
    }
}

#[derive(Serialize)]
struct TablebaseResult {
    #[serde(flatten)]
    probe: ProbeResult,
    moves: ArrayVec<[MoveInfo; 256]>,
}

#[derive(Serialize)]
struct MoveInfo {
    uci: String,
    san: String,
    zeroing: bool,
    #[serde(flatten)]
    probe: ProbeResult,
}

impl MoveInfo {
    fn order(&self) -> MoveOrder {
        if self.probe.checkmate {
            MoveOrder::Checkmate
        } else if self.probe.variant_loss {
            MoveOrder::VariantLoss
        } else if self.probe.variant_win {
            MoveOrder::VariantWin
        } else if self.probe.stalemate {
            MoveOrder::Stalemate
        } else if self.probe.insufficient_material {
            MoveOrder::InsufficientMaterial
        } else if let (Some(wdl), Some(dtz)) = (self.probe.wdl, self.probe.dtz) {
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
struct ProbeResult {
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

    fn query(&self, pos: Chess) -> impl Future<Item=Result<TablebaseResult, SyzygyError>, Error=MailboxError> {
        self.addr.send(VariantPosition::Regular(pos))
    }
}

struct Tablebase {
    regular: Arc<Tablebases<Chess>>,
}

impl Tablebase {
    fn probe(&self, pos: &VariantPosition) -> Result<ProbeResult, SyzygyError> {
        let (variant_win, variant_loss) = match pos.variant_outcome() {
            Some(Outcome::Decisive { winner }) =>
                (winner == pos.turn(), winner != pos.turn()),
            _ =>
                (false, false),
        };

        let halfmove_clock = min(100, pos.halfmove_clock()) as i16;

        let dtz = match pos {
            VariantPosition::Regular(pos) => self.regular.probe_dtz(pos),
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

        Ok(ProbeResult {
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
    type Result = Result<TablebaseResult, SyzygyError>;

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
                probe: self.probe(&after)?,
                zeroing: m.is_zeroing(),
            });
        }

        move_info.sort_by_key(|m: &MoveInfo| m.order());

        Ok(TablebaseResult {
            probe: self.probe(&pos)?,
            moves: move_info,
        })
    }
}

impl Message for VariantPosition {
    type Result = Result<TablebaseResult, SyzygyError>;
}

fn get_fen(req: HttpRequest<State>) -> Box<Future<Item=HttpResponse, Error=Error>> {
    /* let fen = req.query().get("fen").unwrap_or(""); */
    req.state().tablebase.query(Chess::default())
        .from_err()
        .map(|res| {
            HttpResponse::Ok().json(res.ok())
        })
        .responder()
}

fn main() {
    ::std::env::set_var("RUST_LOG", "actix_web=info");
    let _ = env_logger::init();
    let system = actix::System::new("lila-tablebase");

    let mut tables = Tablebases::<Chess>::new();
    tables.add_directory("/opt/syzygy/regular/syzygy").expect("open dir");
    let tables = Arc::new(tables);

    let tablebase = TablebaseStub::new(SyncArbiter::start(1, move || Tablebase { regular: tables.clone() } ));

    let server = server::new(move || {
        App::with_state(State { tablebase: tablebase.clone() })
            .resource("/", |r| r.get().f(get_fen))
    });

    server.bind("127.0.0.1:8080").unwrap().start();
    system.run();
}
