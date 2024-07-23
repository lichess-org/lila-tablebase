FROM docker.io/rust:1-slim-bookworm AS builder
WORKDIR /lila-tablebase
RUN apt-get update && apt-get upgrade --yes && apt-get install --yes libclang-dev gcc g++
COPY Cargo.toml Cargo.lock .
COPY src src
RUN cargo build --release

FROM docker.io/debian:bookworm-slim
RUN apt-get update && apt-get upgrade --yes
COPY --from=builder /lila-tablebase/target/release/lila-tablebase /usr/local/bin/lila-tablebase
ENTRYPOINT ["/usr/local/bin/lila-tablebase"]
