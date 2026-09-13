#!/usr/bin/env python3
"""Extract a large zip quickly and resumably.

The Trilliant zip stores its DuckLake files uncompressed; macOS `unzip`
extracted it at about 7 MB/s. This copies with 16 MB buffers (disk speed),
zipfile checks each member's CRC-32 (raising BadZipFile on a mismatch), each
member is written to <name>.part and renamed only when complete, and members
already on disk at the right size are skipped, so an interrupted run resumes.

Usage: tools/fast_unzip.py <zip> <dest_dir>
"""
import os
import shutil
import sys
import time
import zipfile

if len(sys.argv) != 3:
    sys.exit(__doc__)
zip_path, dest = sys.argv[1], sys.argv[2]
t0 = time.time()
done_bytes = 0
with zipfile.ZipFile(zip_path) as zf:
    members = [m for m in zf.infolist() if not m.filename.endswith(".DS_Store")]
    total = sum(m.file_size for m in members)
    for i, m in enumerate(members, 1):
        out = os.path.join(dest, m.filename)
        if m.is_dir():
            os.makedirs(out, exist_ok=True)
            continue
        os.makedirs(os.path.dirname(out), exist_ok=True)
        if os.path.exists(out) and os.path.getsize(out) == m.file_size:
            done_bytes += m.file_size
            continue
        tmp = out + ".part"
        with zf.open(m) as src, open(tmp, "wb") as dst:
            shutil.copyfileobj(src, dst, 16 * 1024 * 1024)
        os.replace(tmp, out)
        done_bytes += m.file_size
        if i % 10 == 0:
            rate = done_bytes / max(time.time() - t0, 1) / 1e6
            print(f"{time.strftime('%H:%M:%S')} {i}/{len(members)} files, {done_bytes / 1e9:.1f}/{total / 1e9:.1f} GB ({rate:.0f} MB/s)", flush=True)
print(f"{time.strftime('%H:%M:%S')} DONE {len(members)} members, {total / 1e9:.1f} GB", flush=True)
