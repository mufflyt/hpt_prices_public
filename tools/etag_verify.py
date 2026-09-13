#!/usr/bin/env python3
"""Verify a download against an S3 multipart ETag ("<md5 of part md5s>-<n parts>").

The Trilliant zip is served from S3-compatible storage (Tigris); its ETag is
not a plain MD5 of the file but the MD5 of the concatenated per-part MD5s,
followed by the part count. The part size is not in the ETag; for the
2026-07-21 zip it is 50 MiB (1,520 parts). Read the ETag from the download
response headers (curl -sI "$TRILLIANT_URL" | grep -i etag).

Per-part digests are cached, so an interrupted run resumes and a partial file
can be hashed early with --upto.

Usage:
  tools/etag_verify.py <file> --etag <etag> [--part-mib 50] [--cache cache.json] [--upto BYTES]
"""
import argparse
import hashlib
import json
import os
import sys

parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
parser.add_argument("file")
parser.add_argument("--etag", required=True, help='e.g. "da6d4a4ae4896ce5b33e8f86da87ab49-1520"')
parser.add_argument("--part-mib", type=int, default=50)
parser.add_argument("--cache", default=None, help="JSON file of per-part MD5s (default: <file>.etag_cache.json)")
parser.add_argument("--upto", type=int, default=None, help="hash only parts that end at or before this byte")
args = parser.parse_args()

expected = args.etag.strip('"')
n_parts = int(expected.rsplit("-", 1)[1])
part = args.part_mib * 1024 * 1024
total = os.path.getsize(args.file)
upto = args.upto if args.upto is not None else total
cache_path = args.cache or args.file + ".etag_cache.json"
cache = json.load(open(cache_path)) if os.path.exists(cache_path) else {}

if -(-total // part) != n_parts and args.upto is None:
    sys.exit(f"{total} bytes in {args.part_mib} MiB parts is {-(-total // part)} parts, but the ETag says {n_parts}: wrong part size or incomplete file")

with open(args.file, "rb") as f:
    for i in range(n_parts):
        start, stop = i * part, min((i + 1) * part, total)
        if str(i) in cache or stop > upto:
            continue
        f.seek(start)
        cache[str(i)] = hashlib.md5(f.read(stop - start)).hexdigest()
        if i % 50 == 0:
            json.dump(cache, open(cache_path, "w"))
            print(f"part {i}/{n_parts}", flush=True)
json.dump(cache, open(cache_path, "w"))

if len(cache) < n_parts:
    print(f"hashed {len(cache)} of {n_parts} parts so far")
    sys.exit(0)
combined = hashlib.md5(b"".join(bytes.fromhex(cache[str(i)]) for i in range(n_parts))).hexdigest()
computed = f"{combined}-{n_parts}"
print(f"computed {computed}\nexpected {expected}")
if computed != expected:
    sys.exit("MISMATCH")
print("MATCH")
