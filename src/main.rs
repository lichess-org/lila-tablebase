extern crate actix;
extern crate actix_web;
extern crate env_logger;
extern crate shakmaty;
extern crate shakmaty_syzygy;
extern crate futures;

use std::sync::Arc;
use actix::{Addr, Syn, Message, Actor, SyncContext, SyncArbiter, Handler, MailboxError};
use actix_web::{server, App, HttpRequest, HttpResponse, AsyncResponder, Error, Result};
use shakmaty::Chess;
use shakmaty_syzygy::{Tablebases, Wdl, SyzygyError, Syzygy};
use futures::future::{Future, ok};

struct State {
    tablebase: TablebaseStub,
}

#[derive(Clone)]
struct TablebaseStub {
    addr: Addr<Syn, Tablebase>,
}

impl TablebaseStub {
    fn new(addr: Addr<Syn, Tablebase>) -> TablebaseStub {
        TablebaseStub { addr }
    }

    fn query(&self, pos: Chess) -> impl Future<Item=Result<Wdl, SyzygyError>, Error=MailboxError> {
        self.addr.send(Query { chess: pos })
    }
}

struct Tablebase {
    tables: Arc<Tablebases<Chess>>,
}

impl Actor for Tablebase {
    type Context = SyncContext<Self>;
}

struct Query {
    chess: Chess,
}

impl Handler<Query> for Tablebase {
    type Result = Result<Wdl, SyzygyError>;

    fn handle(&mut self, msg: Query, _: &mut Self::Context) -> Self::Result {
        Ok(Wdl::Win)
    }
}

impl Message for Query {
    type Result = Result<Wdl, SyzygyError>;
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

    let tablebase = TablebaseStub::new(SyncArbiter::start(1, move || Tablebase { tables: tables.clone() } ));

    let server = server::new(move || {
        App::with_state(State { tablebase: tablebase.clone() })
            .resource("/", |r| r.get().f(get_fen))
    });

    server.bind("127.0.0.1:8080").unwrap().start();
    system.run();
}
