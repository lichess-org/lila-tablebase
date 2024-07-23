FROM docker.io/alpine:3 AS builder
WORKDIR /lila-tablebase
RUN apk --no-cache add cargo clang-libclang g++
COPY Cargo.toml Cargo.lock build.rs .
COPY src src
RUN cargo build --release

FROM docker.io/alpine:3
COPY --from=builder /lila-tablebase/target/release/lila-tablebase /usr/local/bin/lila-tablebase
CMD ["/usr/local/bin/lila-tablebase"]
