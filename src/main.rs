#![feature(futures_api, async_await, await_macro)]

use serde_derive::Deserialize;

use shakmaty::variants::{Atomic, Chess, Giveaway};
use shakmaty::fen::{Fen, ParseFenError};
use shakmaty_syzygy::{Tablebase as SyzygyTablebase};

use tide::{App, IntoResponse};
use tide::head::QueryParams;

use http::status::StatusCode;

use structopt::StructOpt;

use futures_01;
use futures::task::Poll;
use futures::future::{self, lazy, poll_fn};
use futures::compat::Future01CompatExt;

use std::sync::Arc;
use std::path::PathBuf;

#[derive(Copy, Clone, Debug)]
enum Variant {
    Standard,
    Atomic,
    Antichess,
}

#[derive(Debug)]
struct Tablebases {
    standard: SyzygyTablebase<Chess>,
    atomic: SyzygyTablebase<Atomic>,
    antichess: SyzygyTablebase<Giveaway>,
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

async fn hello_world(variant: Variant, QueryParams(query): QueryParams<Query>) -> Result<String, impl IntoResponse> {
    let fen = match query.fen() {
        Ok(fen) => fen,
        Err(_) => return Err("fen invalid".with_status(StatusCode::BAD_REQUEST)),
    };

    let old = futures_01::future::lazy(|| futures_01::future::poll_fn(|| {
        tokio_threadpool::blocking(|| {
            std::thread::sleep(std::time::Duration::from_millis(10_000));
            "hello again!"
        }) // .map_err(|_| panic!("tokio threadpool active"))

        /* match poll_01 {
            futures_01::Async::Ready(r) => Poll::Ready(r),
            futures_01::Async::NotReady => Poll::Pending,
            //Err(_) => panic!("poll err"),
        } */
    }));

    let r = await!(Future01CompatExt::compat(old));

    Ok(r.expect("infallible").into())
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

    /// Listen on this address.
    #[structopt(long = "address", default_value = "127.0.0.1")]
    address: String,
    /// Listen on this port.
    #[structopt(long = "port", default_value = "9000")]
    port: u16,
}

fn main() {
    // Parse arguments.
    let opt = Opt::from_args();
    if opt.standard.is_empty() && opt.atomic.is_empty() && opt.antichess.is_empty() && opt.gaviota.is_empty() {
        Opt::clap().print_help().expect("usage");
        println!();
        return;
    }

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

    // Start server.
    let mut app = App::new(Arc::new(Tablebases { standard, atomic, antichess }));
    app.at("/").get(|q| hello_world(Variant::Standard, q));
    app.serve();
}
