lila-tablebase
==============

Tablebase server for [lichess.org](https://tablebase.lichess.org).

Usage
-----

```
lila-tablebase 0.1.0
Niklas Fiekas
Tablebase server for lichess.org

USAGE:
    lila-tablebase [OPTIONS]

FLAGS:
    -h, --help       Prints help information
    -V, --version    Prints version information

OPTIONS:
        --antichess <antichess>...    Directory with .gtbw, .gtbz, and pawnless .stbw and .stbz files
        --atomic <atomic>...          Directory with .atbw and .atbz files
        --bind <bind>                  [default: 127.0.0.1:8080]
        --standard <standard>...      Directory with .rtbw and .rtbz files
```

HTTP API
--------

### `GET /v1/standard`

```
curl https://tablebase.lichess.org/v1/standard?fen=8/6B1/8/8/B7/8/K1pk4/8%20b%20-%20-%200%201
```

name | type | default | description
--- | --- | --- | ---
**fen** | string | *required* | FEN of the position. The halfmove clock is taken into account for WDL values.

```javascript
{
   "checkmate":false,
   "stalemate":false,
   "variant_win":false,
   "variant_loss":false,
   "insufficient_material":false,
   "wdl":null,
   "dtz":null,
   "moves":[
      {
         "uci":"d2e1",
         "san":"Ke1",
         "zeroing":false,
         "checkmate":false,
         "stalemate":false,
         "variant_win":false,
         "variant_loss":false,
         "insufficient_material":false,
         "wdl":2,
         "dtz":1
      },
      {
         "uci":"d2e2",
         "san":"Ke2",
         "zeroing":false,
         "checkmate":false,
         "stalemate":false,
         "variant_win":false,
         "variant_loss":false,
         "insufficient_material":false,
         "wdl":2,
         "dtz":1
      },
      {
         "uci":"d2e3",
         "san":"Ke3",
         "zeroing":false,
         "checkmate":false,
         "stalemate":false,
         "variant_win":false,
         "variant_loss":false,
         "insufficient_material":false,
         "wdl":2,
         "dtz":1
      },
      {
         "uci":"c2c1q",
         "san":"c1=Q",
         "zeroing":true,
         "checkmate":false,
         "stalemate":false,
         "variant_win":false,
         "variant_loss":false,
         "insufficient_material":false,
         "wdl":null,
         "dtz":null
      },
      {
         "uci":"c2c1r",
         "san":"c1=R",
         "zeroing":true,
         "checkmate":false,
         "stalemate":false,
         "variant_win":false,
         "variant_loss":false,
         "insufficient_material":false,
         "wdl":null,
         "dtz":null
      },
      {
         "uci":"c2c1b",
         "san":"c1=B",
         "zeroing":true,
         "checkmate":false,
         "stalemate":false,
         "variant_win":false,
         "variant_loss":false,
         "insufficient_material":false,
         "wdl":null,
         "dtz":null
      },
      {
         "uci":"c2c1n",
         "san":"c1=N",
         "zeroing":true,
         "checkmate":false,
         "stalemate":false,
         "variant_win":false,
         "variant_loss":false,
         "insufficient_material":false,
         "wdl":null,
         "dtz":null
      },
      {
         "uci":"d2c1",
         "san":"Kc1",
         "zeroing":false,
         "checkmate":false,
         "stalemate":false,
         "variant_win":false,
         "variant_loss":false,
         "insufficient_material":false,
         "wdl":null,
         "dtz":null
      },
      {
         "uci":"d2d1",
         "san":"Kd1",
         "zeroing":false,
         "checkmate":false,
         "stalemate":false,
         "variant_win":false,
         "variant_loss":false,
         "insufficient_material":false,
         "wdl":null,
         "dtz":null
      },
      {
         "uci":"d2d3",
         "san":"Kd3",
         "zeroing":false,
         "checkmate":false,
         "stalemate":false,
         "variant_win":false,
         "variant_loss":false,
         "insufficient_material":false,
         "wdl":null,
         "dtz":null
      }
   ]
}
```

### `GET /v1/atomic`

### `GET /v1/antichess`


Example
-------

```rust
use shakmaty::Chess;
use shakmaty::fen::Fen;
use shakmaty_syzygy::{Tablebases, Wdl, Dtz, Syzygy};

let mut tables = Tablebases::new();
tables.add_directory("tables/regular")?;

let pos: Chess = "8/8/8/8/B7/N7/K2k4/8 b - - 0 1"
    .parse::<Fen>()?
    .position()?;

let wdl = tables.probe_wdl(&pos)?;
assert_eq!(wdl, Wdl::Loss);

let dtz = tables.probe_dtz(&pos)?;
assert_eq!(dtz, Dtz(-59));
```


License
-------

lila-tablebase is licensed under the GPL-3.0 (or any later version at your
option). See the COPYING file for the full license text.
