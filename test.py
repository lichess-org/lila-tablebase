#!/usr/bin/env python3

import cbor2
import os
import requests
import unittest


TABLEBASE_ENDPOINT = os.environ.get("TABLEBASE_ENDPOINT", "http://localhost:9000")


def standard(fen, session=requests.Session()):
    r = session.get(f"{TABLEBASE_ENDPOINT}/standard", params={
        "fen": fen,
    })
    r.raise_for_status()
    return r.json()


class TablebaseTest(unittest.TestCase):
    def test_six_piece_mate(self):
        r = standard("8/4K2k/5Q1P/6P1/8/8/q7/8 w - - 99 148")
        self.assertEqual(r["category"], "win")
        self.assertEqual(r["moves"][0]["san"], "Qg7#")
        self.assertEqual(r["moves"][0]["category"], "loss")

        r = standard("8/4K2k/5Q1P/6P1/8/8/q7/8 w - - 100 148")
        self.assertEqual(r["category"], "cursed-win")
        self.assertEqual(r["moves"][0]["san"], "Qg7#")
        self.assertEqual(r["moves"][0]["category"], "blessed-loss")

        r = standard("8/4K2k/5Q1P/6P1/8/8/q7/8 w - - 101 148")
        self.assertEqual(r["category"], "cursed-win")
        self.assertEqual(r["moves"][0]["san"], "Qg7#")
        self.assertEqual(r["moves"][0]["category"], "blessed-loss")

    def test_eight_piece_mate(self):
        r = standard("7k/7p/1R6/6P1/P7/5n2/4r3/7K b - - 0 41")
        self.assertEqual(r["category"], "win")

        self.assertEqual(r["moves"][0]["san"], "Rh2#")
        self.assertEqual(r["moves"][0]["category"], "loss")

        self.assertEqual(r["moves"][2]["category"], "unknown")

    def test_maybe_win(self):
        r = standard("K7/2k5/3n4/8/2b5/8/8/8 w - - 97 128")
        self.assertIn(r["category"], ["maybe-loss", "blessed-loss"])
        self.assertEqual(r["moves"][0]["san"], "Ka7")
        self.assertIn(r["moves"][0]["dtz"], [2, 3])
        self.assertIn(r["moves"][0]["category"], ["maybe-win", "cursed-win"])

        r = standard("2n5/K1k5/8/8/2b5/8/8/8 w - - 99 129")
        self.assertEqual(r["category"], "blessed-loss")
        self.assertEqual(r["moves"][0]["san"], "Ka8")
        self.assertEqual(r["moves"][0]["dtz"], 1)
        self.assertEqual(r["moves"][0]["category"], "cursed-win")

    def test_blessed_loss(self):
        r = standard("4K1k1/1N1P4/8/8/8/1q6/8/8 w - - 0 1")
        self.assertEqual(r["category"], "blessed-loss")

        self.assertEqual(r["moves"][0]["san"], "d8=N")
        self.assertEqual(r["moves"][0]["category"], "cursed-win")

        self.assertEqual(r["moves"][1]["san"], "Na5")
        self.assertEqual(r["moves"][1]["category"], "win")

    def test_unknown(self):
        r = standard("4K1k1/1N1P4/8/8/8/1q6/5PPP/8 w - - 0 1")
        self.assertEqual(r["category"], "unknown")
        self.assertEqual(r["moves"][0]["category"], "unknown")

    def test_castling(self):
        r = standard("4k3/5q2/8/8/8/8/8/R3K2R w KQ - 0 1")
        self.assertEqual(r["category"], "unknown")
        self.assertEqual(r["moves"][0]["category"], "unknown")
        self.assertEqual(r["moves"][19]["category"], "draw")

        r = standard("4k2q/5q2/8/8/8/8/8/R3K2R w K - 0 1")
        self.assertEqual(r["category"], "win")
        self.assertEqual(r["moves"][0]["san"], "Rxh8+")
        self.assertEqual(r["moves"][0]["category"], "loss")
        self.assertEqual(r["moves"][1]["category"], "unknown")
        self.assertEqual(r["moves"][11]["category"], "win")

    def test_barely_losing(self):
        r = standard("8/6B1/6B1/8/2K5/3nk3/8/8 b - - 0 1")
        self.assertEqual(r["category"], "loss")
        self.assertEqual(r["moves"][0]["san"], "Nf4")
        self.assertEqual(r["moves"][0]["dtz"], 99)
        self.assertEqual(r["moves"][0]["category"], "win")

        r = standard("8/6B1/6B1/8/2K2n2/4k3/8/8 w - - 1 2")
        self.assertIn(r["category"], ["maybe-win", "win"])
        self.assertEqual(r["moves"][0]["dtz"], -98)
        self.assertIn(r["moves"][0]["category"], ["maybe-loss", "loss"])

    def test_cbor_six_piece_mate(self):
        r = cbor2.loads(requests.get(f"{TABLEBASE_ENDPOINT}/standard", {
            "fen": "8/4K2k/5Q1P/6P1/8/8/q7/8 w - - 99 148"
        }, headers={
            "Accept": "application/cbor"
        }).content)

        self.assertEqual(r["category"], "win")
        self.assertEqual(r["moves"][0]["san"], "Qg7#")
        self.assertEqual(r["moves"][0]["category"], "loss")

    def test_kqvk(self):
        standard("8/8/8/8/1Q4K1/8/k7/8 w - - 0 1")
        standard("8/1k6/8/2K5/8/8/8/Q7 w - - 0 1")
        standard("8/7k/8/4QK2/8/8/8/8 w - - 22 12")
        standard("8/8/k7/2Q5/3K4/8/8/8 w - - 12 7")
        standard("8/8/2k5/8/3K4/8/8/4Q3 w - - 0 1")
        standard("8/8/8/8/8/5K2/5Q2/5k2 b - - 0 1")
        standard("8/8/8/8/1Q4K1/8/8/k7 b - - 6 9")
        standard("8/8/3k4/8/3K4/8/8/4Q3 b - - 1 1")
        standard("8/8/7k/4Q3/4K3/8/8/8 w - - 20 11")

    def test_kqvkb(self):
        standard("8/8/8/8/3K4/4Qb2/5k2/8 b - - 0 1")
        standard("8/8/4K3/8/3Q4/8/4k3/B7 b - - 0 1")


if __name__ == "__main__":
    unittest.main()
