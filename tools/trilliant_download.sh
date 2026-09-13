#!/usr/bin/env bash
# Resumable download of the Trilliant Health "Full Data Download" zip.
#
# Usage:
#   TRILLIANT_URL='<signed link from oria.trillianthealth.com>' \
#     tools/trilliant_download.sh <output.zip> <expected_bytes> [parallel_streams]
#
# The signed URL is personal and expires; it is read from the environment and
# never written to disk. With parallel_streams = 1 (default) the file is
# fetched with `curl -C -` in one stream, resuming after drops. With N > 1 the
# remaining bytes are split into 1 GiB ranges fetched N at a time (each range
# resumable), then appended in order; this was the only way to finish the
# 79.6 GB 2026-07-21 zip at a usable speed.
#
# After it finishes, verify with tools/etag_verify.py and extract with
# tools/fast_unzip.py (see docs/trilliant_download.md).
set -euo pipefail

out="${1:?usage: trilliant_download.sh <output.zip> <expected_bytes> [parallel_streams]}"
expected="${2:?expected size in bytes (shown on the download page)}"
streams="${3:-1}"
url="${TRILLIANT_URL:?set TRILLIANT_URL to the signed download link}"
chunk=$((1024 * 1024 * 1024))

size_of() {  # file size in bytes on macOS and Linux; 0 if missing
  if [ -f "$1" ]; then stat -f %z "$1" 2>/dev/null || stat -c %s "$1"; else echo 0; fi
}

part="$out.part"
[ -f "$out" ] && [ "$(size_of "$out")" -eq "$expected" ] && { echo "already complete: $out"; exit 0; }
mkdir -p "$(dirname "$out")"

if [ "$streams" -le 1 ]; then
  for attempt in $(seq 1 40); do
    curl -fsS --retry 5 --retry-delay 10 --connect-timeout 30 -C - -o "$part" "$url" || true
    have=$(size_of "$part")
    echo "$(date +%H:%M:%S) attempt $attempt: $have of $expected bytes"
    [ "$have" -eq "$expected" ] && { mv "$part" "$out"; echo "DONE $out"; exit 0; }
    [ "$have" -gt "$expected" ] && { echo "file is larger than expected; stopping" >&2; exit 2; }
    sleep 15
  done
  echo "FAILED after 40 attempts; rerun to resume" >&2; exit 1
fi

# parallel byte ranges after whatever the single-stream part already holds
parts_dir="$out.parts"
mkdir -p "$parts_dir"
touch "$part"
if [ ! -f "$parts_dir/ranges.txt" ]; then
  base=$(size_of "$part"); echo "$base" > "$parts_dir/base_offset"
  i=0; start=$base
  while [ "$start" -lt "$expected" ]; do
    stop=$((start + chunk - 1)); [ "$stop" -ge "$expected" ] && stop=$((expected - 1))
    printf "%05d %d %d\n" "$i" "$start" "$stop" >> "$parts_dir/ranges.txt"
    i=$((i + 1)); start=$((stop + 1))
  done
fi

fetch_range() {  # <index> <first_byte> <last_byte>
  local idx=$1 a=$2 b=$3 file="$parts_dir/part_$1" want have
  want=$((b - a + 1))
  for t in $(seq 1 10); do
    have=$(size_of "$file")
    [ "$have" -eq "$want" ] && { echo "$(date +%H:%M:%S) part $idx ok"; return 0; }
    [ "$have" -gt "$want" ] && { rm -f "$file"; have=0; }
    curl -fsS --connect-timeout 30 --speed-limit 50000 --speed-time 60 -r "$((a + have))-$b" "$url" >> "$file" 2>/dev/null || true
    sleep 2
  done
  echo "$(date +%H:%M:%S) part $idx FAILED" >&2; return 1
}
export -f fetch_range size_of
export parts_dir url

echo "$(date +%H:%M:%S) $(wc -l < "$parts_dir/ranges.txt" | tr -d ' ') ranges after byte $(cat "$parts_dir/base_offset"), $streams at a time"
xargs -P "$streams" -n 3 bash -c 'fetch_range "$@"' _ < "$parts_dir/ranges.txt" || true

while read -r idx a b; do
  [ "$(size_of "$parts_dir/part_$idx")" -eq $((b - a + 1)) ] || { echo "part $idx incomplete; rerun to resume" >&2; exit 1; }
done < "$parts_dir/ranges.txt"
[ "$(size_of "$part")" -eq "$(cat "$parts_dir/base_offset")" ] || { echo "$part changed since the ranges were planned; not appending" >&2; exit 1; }

echo "$(date +%H:%M:%S) appending ranges"
while read -r idx a b; do cat "$parts_dir/part_$idx" >> "$part"; done < "$parts_dir/ranges.txt"
[ "$(size_of "$part")" -eq "$expected" ] || { echo "size mismatch after appending" >&2; exit 1; }
mv "$part" "$out"; rm -rf "$parts_dir"
echo "DONE $out"
