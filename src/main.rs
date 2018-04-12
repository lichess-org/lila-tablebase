extern crate actix_web;

use actix_web::{server, App, HttpRequest, HttpResponse};

fn index(req: HttpRequest) -> &'static str {
    "Hello world!"
}

fn main() {
    server::new(|| App::new().resource("/", |r| r.f(index)))
        .bind("127.0.0.1:5005").expect("bind 5005")
        .run();
}
