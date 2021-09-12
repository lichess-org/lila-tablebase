#!/usr/bin/env python

import unittest
import os
import requests


TABLEBASE_ENDPOINT = os.environ.get("TABLEBASE_ENDPOINT", "http://localhost:9000")


def standard(fen):
    return requests.get(f"{TABLEBASE_ENDPOINT}/standard", {
        "fen": fen,
    }).json()


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
        self.assertEqual(r["category"], "maybe-loss")
        self.assertEqual(r["moves"][0]["san"], "Ka7")
        self.assertEqual(r["moves"][0]["dtz"], 2)
        self.assertEqual(r["moves"][0]["category"], "maybe-win")

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

        self.assertEqual(r["moves"][2]["san"], "Nc5")
        self.assertEqual(r["moves"][2]["category"], "win")


if __name__ == "__main__":
    unittest.main()
