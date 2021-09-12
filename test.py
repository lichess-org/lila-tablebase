#!/usr/bin/env python

import unittest
import os
import requests


TABLEBASE_ENDPOINT = os.environ.get("TABLEBASE_ENDPOINT", "https://tablebase.lichess.ovh")


def standard(fen):
    return requests.get(f"{TABLEBASE_ENDPOINT}/standard", {
        "fen": fen,
    }).json()


class TablebaseTest(unittest.TestCase):
    def test_mate(self):
        r = standard("8/4K2k/5Q1P/6P1/8/8/q7/8 w - - 99 148")
        self.assertEqual(r["category"], "win")
        self.assertEqual(r["moves"][0]["san"], "Qxg7#")
        self.assertEqual(r["moves"][0]["category"], "loss")


if __name__ == "__main__":
    unittest.main()
