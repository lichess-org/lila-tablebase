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
        --standard <standard>...      Directory with .rtbw and .rtbz files
        --bind <bind>                 Listening address [default: 127.0.0.1:8080]
```

HTTP API
--------

### `GET /standard`

```
curl https://tablebase.lichess.org/standard?fen=7K/8/k1P5/7p/8/8/8/8%20w%20-%20-%200%201
```

name | type | default | description
--- | --- | --- | ---
**fen** | string | *required* | FEN of the position. The halfmove clock is taken into account for WDL values.

```javascript
{
    "wdl": 0, // 2 win, 1 cursed win, 0 draw, -1 blessed loss, -2 loss, null unknown
    "dtz": 0, // distance to zeroing or null if unknown
    "checkmate": false,
    "stalemate": false,
    "variant_win": false, // only useful in chess variants (atomic, antichess)
    "variant_loss": false, // only useful in chess variants
    "insufficient_material": false,
    "moves": [
        { "uci": "h8g7", "san": "Kg7", "wdl": 0, "dtz": 0, "zeroing": false, "checkmate": false, "stalemate": false, "variant_win":false, "variant_loss":false, "insufficient_material":false },
        { "uci": "h8h7", "san": "Kh7", "wdl": 2, "dtz": 1, "zeroing": false, "checkmate": false, "stalemate": false, "variant_win": false, "variant_loss": false, "insufficient_material": false },
        { "uci": "h8g8", "san": "Kg8", "wdl": 2, "dtz": 1, "zeroing": false, "checkmate": false, "stalemate": false, "variant_win": false, "variant_loss": false, "insufficient_material": false },
        { "uci": "c6c7", "san": "c7", "wdl": 2, "dtz": 3, "zeroing": true, "checkmate": false, "stalemate": false, "variant_win": false, "variant_loss": false, "insufficient_material": false }
    ]
}
```

### `GET /atomic`

### `GET /antichess`

Acknowledgements
----------------

Thanks to Ronald de Man for his [Syzygy endgame tables](https://github.com/syzygy1/tb).
Thanks to Bojun Guo for generating and [sharing](http://www.talkchess.com/forum/viewtopic.php?t=66797) experimental 7-man tables.

License
-------

lila-tablebase is licensed under the GNU Affero General Public License 3.0 (or any later version at your
option). See the COPYING file for the full license text.
