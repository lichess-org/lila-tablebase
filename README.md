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
curl https://tablebase.lichess.org/v1/standard?fen=7K/8/k1P5/7p/8/8/8/8%20w%20-%20-%200%201
```

name | type | default | description
--- | --- | --- | ---
**fen** | string | *required* | FEN of the position. The halfmove clock is taken into account for WDL values.

```javascript
{
    "wdl": 0,
    "dtz": 0,
    "checkmate": false,
    "stalemate": false,
    "variant_win": false,
    "variant_loss": false,
    "insufficient_material": false,
    "moves": [
        { "uci": "h8g7", "san": "Kg7", "wdl": 0, "dtz": 0, "zeroing": false, "checkmate": false, "stalemate": false, "variant_win":false, "variant_loss":false, "insufficient_material":false },
        { "uci": "h8h7", "san": "Kh7", "wdl": 2, "dtz": 1, "zeroing": false, "checkmate": false, "stalemate": false, "variant_win": false, "variant_loss": false, "insufficient_material": false },
        { "uci": "h8g8", "san": "Kg8", "wdl": 2, "dtz": 1, "zeroing": false, "checkmate": false, "stalemate": false, "variant_win": false, "variant_loss": false, "insufficient_material": false },
        { "uci":"c6c7", "san": "c7", "wdl": 2, "dtz": 3, "zeroing": true, "checkmate": false, "stalemate": false, "variant_win": false, "variant_loss": false, "insufficient_material": false }
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
