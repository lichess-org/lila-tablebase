FROM docker.io/alpine:3 AS builder
WORKDIR /lila-tablebase
RUN apk --no-cache add cargo clang-libclang g++
COPY Cargo.toml Cargo.lock .
COPY src src
RUN cargo build --release

FROM docker.io/alpine:3
RUN apk --no-cache add libstdc++
COPY --from=builder /lila-tablebase/target/release/lila-tablebase /usr/local/bin/lila-tablebase
ENTRYPOINT ["/usr/local/bin/lila-tablebase"]
