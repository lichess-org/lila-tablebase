# syntax=docker/dockerfile:1

FROM docker.io/rust:1-slim-bookworm AS chef
RUN apt-get update && apt-get install --yes libclang-dev libssl-dev gcc g++ make pkg-config && \
    rm -rf /var/lib/apt/lists/* && \
    cargo install cargo-chef --locked
WORKDIR /app

FROM chef AS planner
COPY . .
RUN cargo chef prepare

FROM chef AS builder
COPY --from=planner /app/recipe.json recipe.json
RUN cargo chef cook --tests && \
    cargo chef cook --release
COPY . .
RUN cargo test && \
    cargo build --release

FROM docker.io/debian:bookworm-slim AS runtime
RUN apt-get update && \
    apt-get install --yes --no-install-recommends ca-certificates && \
    rm -rf /var/lib/apt/lists/*
COPY --from=builder /app/target/release/lila-tablebase /usr/local/bin/lila-tablebase
USER lichess
ENTRYPOINT ["/usr/local/bin/lila-tablebase"]
