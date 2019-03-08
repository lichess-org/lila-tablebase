#![feature(futures_api, async_await, await_macro)]

#[macro_use]
extern crate serde_derive;

use shakmaty::fen::{Fen, ParseFenError};

use tide::{App, IntoResponse};
use tide::head::QueryParams;

use http::status::StatusCode;

use futures_01;
use futures::task::Poll;
use futures::future::{self, lazy, poll_fn};
use futures::compat::Future01CompatExt;

#[derive(Copy, Clone, Debug)]
enum Variant {
    Standard,
    Atomic,
    Antichess,
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

fn main() {
    let mut app = App::new(());
    app.at("/").get(|q| hello_world(Variant::Standard, q));
    app.serve();
}
