lila-tablebase
==============

Tablebase server for [lichess.org](https://tablebase.lichess.ovh),
based on [shakmaty-syzygy](https://crates.io/crates/shakmaty-syzygy).

Blog posts
----------

- [Op1 - Partial 8-piece tablebase available](https://lichess.org/@/Lichess/blog/op1-partial-8-piece-tablebase-available/1ptPBDpC)
- [Optimizing the tablebase server](https://lichess.org/@/revoof/blog/optimizing-the-tablebase-server/MetV0ZQd)
- [7 piece tablebases are complete](https://lichess.org/@/lichess/blog/7-piece-syzygy-tablebases-are-complete/W3WeMyQA)

Usage
-----

```
cargo run --release -- --help
```

HTTP API
--------

### `GET /standard`

```
curl http://tablebase.lichess.ovh/standard?fen=4k3/6KP/8/8/8/8/7p/8_w_-_-_0_1
```

name | type | default | description
--- | --- | --- | ---
**fen** | string | *required* | FEN of the position. Underscores allowed.

```javascript
{
  "dtz": 1, // dtz50'' with rounding or null if unknown
  "precise_dtz": 1, // dtz50'' (only if guaranteed to be not rounded) or null if unknown
  "dtc": null, // depth to conversion or null if unknown
  "dtm": 17, // depth to mate or null if unknown
  "dtw": null, // depth to antichess win or null if unknown
  "checkmate": false,
  "stalemate": false,
  "variant_win": false, // only in chess variants (atomic, antichess)
  "variant_loss": false, // only in chess variants
  "insufficient_material": false,
  "category": "win", // win, unknown, syzygy-win, maybe-win, cursed-win, draw, blessed-loss, maybe-loss, syzygy-loss, loss
  "moves": [ // information about legal moves, best first
    {
      "uci": "h7h8q",
      "san": "h8=Q+",
      "dtz": -2,
      "precise_dtz": -2,
      "dtc": null,
      "dtm": -16,
      "dtw": null,
      "zeroing": true,
      "checkmate": false,
      "stalemate": false,
      "variant_win": false,
      "variant_loss": false,
      "insufficient_material": false,
      "category": "loss" // loss, unknown, syzygy-loss, maybe-loss, blessed-loss, draw, cursed-win, maybe-win, syzygy-win, win
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
**fen** | string | *required* | FEN of the position. Underscores allowed.

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
