#!/usr/bin/env python3

"""
Extract hot syzygy table prefixes.

Expected totals:
Including sparse index:   50 080 907 852 bytes
Including block lengths: 413 865 080 576 bytes
"""

import argparse
import math
import os.path

import chess.syzygy


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--sparse-index", action="store_true")
    parser.add_argument("--block-lengths", action="store_true")
    parser.add_argument("--yes", action="store_true")
    parser.add_argument("tables", nargs="+")
    args = parser.parse_args()

    total = 0
    for table in args.tables:
        total += handle_table(table, args)
    print("---")
    print(total, "bytes")


def handle_table(path, args):
    sizes = [0]
    kind = "UNK"
    key = None

    with chess.syzygy.Tablebase(max_fds=None) as tb:
        if tb.add_file(path, load_wdl=True, load_dtz=False):
            for table in tb.wdl.values():
                table.init_table_wdl()
                if args.sparse_index:
                    sizes.append(table.precomp[0].sizetable if not table.has_pawns else table.files[0].precomp[0].sizetable)
                if args.block_lengths:
                    sizes.append(table.precomp[0].data if not table.has_pawns else table.files[0].precomp[0].data)
                kind = "wdl"
                key = table.key
        if tb.add_file(path, load_wdl=False, load_dtz=True):
            for table in tb.dtz.values():
                table.init_table_dtz()
                if args.sparse_index:
                    sizes.append(table.precomp.sizetable if not table.has_pawns else table.files[0].precomp.sizetable)
                if args.block_lengths:
                    sizes.append(table.precomp.data if not table.has_pawns else table.files[0].precomp.data)
                kind = "dtz"
                key = table.key

    print(path, kind, max(sizes))

    if args.yes and key and max(sizes):
        _, ext = os.path.splitext(path)
        copy_prefix(path, f"{key}{ext}.prefix", max(sizes))

    return max(sizes)


def copy_prefix(orig, dest, num_bytes):
    with open(orig, "rb") as orig_f, open(dest, "wb") as dest_f:
        size = 0
        while size < num_bytes:
            chunk = orig_f.read(1024 * 16)
            written_len = dest_f.write(chunk)
            assert len(chunk) == written_len
            size += len(chunk)


if __name__ == "__main__":
    main()
