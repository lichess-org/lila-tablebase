#!/bin/sh -e
cargo build --release --target x86_64-unknown-linux-musl
ssh "root@$1.lichess.ovh" mv /usr/local/bin/lila-tablebase /usr/local/bin/lila-tablebase.bak || (echo "first deploy on this server? set up service and comment out this line" && false)
scp ./target/x86_64-unknown-linux-musl/release/lila-tablebase "root@$1.lichess.ovh":/usr/local/bin/lila-tablebase
ssh "root@$1.lichess.ovh" systemctl restart lila-tablebase
