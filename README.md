lila-tablebase
==============

Tablebase server for [lichess.org](https://tablebase.lichess.ovh),
based on [shakmaty-syzygy](https://crates.io/crates/shakmaty-syzygy).

[![Build Status](https://travis-ci.org/niklasf/lila-tablebase.svg?branch=master)](https://travis-ci.org/niklasf/lila-tablebase)

Usage
-----

```
lila-tablebase 0.2.0
Niklas Fiekas <niklas.fiekas@backscattering.de>
A Syzygy tablebase server

USAGE:
    lila-tablebase [OPTIONS]

FLAGS:
    -h, --help               Prints help information
    -V, --version            Prints version information

OPTIONS:
        --address <address>           Listen on this address. [default: 127.0.0.1]
        --port <port>                 Listen on this port. [default: 9000]
        --disks <disks>               Limit concurrent tablebase probes. A good default
                                      is the number of disks. [default: 5]
        --antichess <antichess>...    Directory with tablebase files for antichess.
        --atomic <atomic>...          Directory with tablebase files for atomic chess.
        --gaviota <gaviota>...        Directory with Gaviota tablebase files.
        --standard <standard>...      Directory with tablebase files for standard chess.
```

HTTP API
--------

### `GET /standard`

```
curl http://tablebase.lichess.ovh/standard?fen=4k3/6KP/8/8/8/8/7p/8_w_-_-_0_1
```

name | type | default | description
--- | --- | --- | ---
**fen** | string | *required* | FEN of the position. Underscores allowed. The halfmove clock is *not* taken into account.

```javascript
{
  "dtz": 1, // distance to zeroing or null if unknown
  "dtm": 17, // depth to mate or null if unknown
  "checkmate": false,
  "stalemate": false,
  "variant_win": false, // only in chess variants (atomic, antichess)
  "variant_loss": false, // only in chess variants
  "insufficient_material": false,
  "moves": [ // information about legal moves, best first
    {
      "uci": "h7h8q",
      "san": "h8=Q+",
      "dtz": -2,
      "dtm": -16,
      "zeroing": true,
      "checkmate": false,
      "stalemate": false,
      "variant_win": false,
      "variant_loss": false,
      "insufficient_material": false
    },
    // ...
  ]
}
```

### `GET /standard/mainline`

```
curl http://tablebase.lichess.ovh/standard/mainline?fen=4k3/6KP/8/8/8/8/7p/8_w_-_-_0_1
```

name | type | default | description
--- | --- | --- | ---
**fen** | string | *required* | FEN of the position. Underscores allowed. The halfmove clock is taken into account.

```javascript
{
  "dtz": 1,
  "mainline": [ // dtz mainline or empty if drawn
    { "uci": "h7h8q", "san": "h8=Q+", "dtz": -2 },
    { "uci": "e8d7", "san": "Kd7", "dtz": 1 },
    { "uci": "h8h2", "san": "Qxh2", "dtz": -14 },
    { "uci": "d7c6", "san": "Kc6", "dtz": 13 },
    { "uci": "h2e5", "san": "Qe5", "dtz": -12 },
    { "uci": "c6b6", "san": "Kb6", "dtz": 11 },
    { "uci": "g7f6", "san": "Kf6", "dtz": -10 },
    { "uci": "b6a6", "san": "Ka6", "dtz": 9 },
    { "uci": "e5b2", "san": "Qb2", "dtz": -8 },
    { "uci": "a6a5", "san": "Ka5", "dtz": 7 },
    { "uci": "f6e5", "san": "Ke5", "dtz": -6 },
    { "uci": "a5a4", "san": "Ka4", "dtz": 5 },
    { "uci": "e5d4", "san": "Kd4", "dtz": -4 },
    { "uci": "a4a5", "san": "Ka5", "dtz": 3 },
    { "uci": "d4c5", "san": "Kc5", "dtz": -2 },
    { "uci": "a5a4", "san": "Ka4", "dtz": 1 },
    { "uci": "b2a2", "san": "Qa2#", "dtz": -1 }
  ],
  "winner": "w" // (w) white, (b) black, (null) draw
}
```

Claims draw by 50-move rule as soon as possible and ends the mainline.
Error 404 if not a tablebase position or required tables not present.

### `GET /atomic`

### `GET /antichess`

Acknowledgements
----------------

Thanks to Ronald de Man for his [Syzygy endgame tables](https://github.com/syzygy1/tb).
Thanks to Bojun Guo for generating and [sharing](http://www.talkchess.com/forum/viewtopic.php?t=66797) 7-piece tables.

License
-------

lila-tablebase is licensed under the GNU Affero General Public License 3.0 (or any later version at your
option). See the COPYING file for the full license text.
