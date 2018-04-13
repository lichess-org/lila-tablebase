extern crate actix;
extern crate actix_web;
extern crate env_logger;
extern crate shakmaty;
extern crate shakmaty_syzygy;
extern crate futures;

use std::sync::Arc;
use actix::{Addr, Syn, Message, Actor, SyncContext, SyncArbiter, Handler, MailboxError};
use actix_web::{server, App, HttpRequest, HttpResponse, AsyncResponder, Error, Result};
use shakmaty::{Color, Chess, Setup, Position, MoveList, Outcome};
use shakmaty_syzygy::{Tablebases, Wdl, Dtz, SyzygyError, Syzygy};
use futures::future::{Future, ok};

struct State {
    tablebase: TablebaseStub,
}

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
}

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

    fn query(&self, pos: Chess) -> impl Future<Item=Result<ProbeResult, SyzygyError>, Error=MailboxError> {
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

        let dtz = match pos {
            VariantPosition::Regular(pos) => self.regular.probe_dtz(pos).ok(),
        };

        let wdl = dtz.map(Wdl::from).or_else(|| match pos {
            VariantPosition::Regular(pos) => self.regular.probe_wdl(pos).ok(),
        });

        Ok(ProbeResult {
            checkmate: pos.is_checkmate(),
            stalemate: pos.is_stalemate(),
            variant_win,
            variant_loss,
            insufficient_material: pos.is_insufficient_material(),
            dtz: dtz,
            wdl: wdl,
        })
    }
}

impl Actor for Tablebase {
    type Context = SyncContext<Self>;
}

impl Handler<VariantPosition> for Tablebase {
    type Result = Result<ProbeResult, SyzygyError>;

    fn handle(&mut self, msg: VariantPosition, _: &mut Self::Context) -> Self::Result {
        self.probe(&msg)
    }
}

impl Message for VariantPosition {
    type Result = Result<ProbeResult, SyzygyError>;
}

fn get_fen(req: HttpRequest<State>) -> Box<Future<Item=HttpResponse, Error=Error>> {
    /* let fen = req.query().get("fen").unwrap_or(""); */
    req.state().tablebase.query(Chess::default())
        .from_err()
        .and_then(|res| {
            ok(HttpResponse::Ok().body("foo".to_string()))
        })
        .responder()
}

fn main() {
    ::std::env::set_var("RUST_LOG", "actix_web=info");
    let _ = env_logger::init();
    let system = actix::System::new("lila-tablebase");

    let mut tables = Tablebases::<Chess>::new();
    let tables = Arc::new(tables);

    let tablebase = TablebaseStub::new(SyncArbiter::start(1, move || Tablebase { regular: tables.clone() } ));

    let server = server::new(move || {
        App::with_state(State { tablebase: tablebase.clone() })
            .resource("/", |r| r.get().f(get_fen))
    });

    server.bind("127.0.0.1:8080").unwrap().start();
    system.run();
}
