#!/usr/bin/env bash
# Build the public code copy of this repository (github.com/mufflyt/hpt_prices_public).
#
# The private repository holds data-derived material that Trilliant Health's terms of service
# (2.3(i), 2.3(iii)) do not allow us to redistribute: the README figures, the CHANGELOG/NEWS/appendix
# and methods docs (which quote national and state prices), and config/known_answers.csv (per-hospital
# rates read from Trilliant). The public copy keeps the code, tests, config, tools, CI, and the
# download guide, and nothing derived from the data.
#
# Usage (from the repository root, with the changes committed; the export is built from HEAD):
#   tools/export_public.sh <dest_dir>
# If <dest_dir> is a git clone of the public repository, the tree is synced into it (files that no
# longer exist are removed; .git is kept). Then review, commit, and push from <dest_dir>.
set -euo pipefail

dest="${1:?usage: tools/export_public.sh <dest_dir>}"
repo=$(git rev-parse --show-toplevel)
cd "$repo"

private_paths=(
  docs/figures
  docs/appendix.md
  docs/addon_methods.md
  docs/ownership_methods.md
  docs/cleanup_impact.md
  docs/childbirth_methods.md
  docs/turquoise_pricepoints.md
  NEWS.md
  CHANGELOG.md
  config/known_answers.csv
  tools/refresh_readme_figures.sh
)

stage=$(mktemp -d)
trap 'rm -rf "$stage"' EXIT
git archive HEAD | tar -x -C "$stage"
for p in "${private_paths[@]}"; do rm -rf "${stage:?}/$p"; done

# README: public header, no figures, documentation limited to what ships
python3 - "$stage/README.md" <<'PY'
import re, sys
path = sys.argv[1]
s = open(path).read()
s = s.replace("github.com/mufflyt/hpt_prices/actions", "github.com/mufflyt/hpt_prices_public/actions")
s = re.sub(r"\n## Figures\n.*?(?=\n## Documentation\n)", "\n", s, flags=re.S)
s = re.sub(r"\n## Documentation\n.*?(?=\n## Tests\n)", """
## Documentation

- `docs/trilliant_download.md`: getting the Trilliant data onto a machine.
- `docs/childbirth_analytic_spec.md`: design of the NTSV cesarean and midwife supply analysis, with the
  exact CDC WONDER exports it reads.
- Every function file in `R/` opens with a comment block describing its method and rules.

Methods write-ups, figures, and results are kept with the data, not here: they are derived from
Trilliant Health data, which its terms of service do not allow us to redistribute.
""", s, flags=re.S)
notice = """
> **Public code copy.** This repository holds the pipeline code, tests, configuration, tools, and
> download guide from a private working repository. It contains no hospital price data, figures,
> or results. Those are derived from Trilliant Health data and stay private under Trilliant's terms
> of service. To use the pipeline, download the data yourself (`docs/trilliant_download.md`).
"""
s = re.sub(r"(\n\[!\[R tests\][^\n]*\n)", r"\1" + notice, s, count=1)
open(path, "w").write(s)
PY

# leak check: nothing private may survive
fail=0
for p in "${private_paths[@]}"; do
  if [ -e "$stage/$p" ]; then echo "LEAK: $p is present" >&2; fail=1; fi
done
if [ -f config/known_answers.csv ]; then
  # every known-answer value (as written and without its decimals) and every file hash
  python3 - config/known_answers.csv "$stage" <<'PY' || fail=1
import csv, os, re, sys
answers, root = sys.argv[1], sys.argv[2]
needles = set()
for r in csv.DictReader(open(answers)):
    if r["stat"] == "median_negotiated" or r["stat"].startswith("max_"):
        v = r["expected"]
        whole = v.split(".")[0]
        # the value as written, plus its whole-number form when that is specific enough:
        # at least 4 digits and not a year (dates and citations would match otherwise)
        for n in (v, whole):
            if re.fullmatch(r"(19|20)[0-9]{2}", n):
                continue
            if n == whole and len(n) < 4 and "." not in v:
                continue
            if n == whole and n != v and len(n) < 4:
                continue
            needles.add(n)
    if r["mrf_file_id"]:
        needles.add(r["mrf_file_id"])
hits = []
for dirpath, _, files in os.walk(root):
    for f in files:
        p = os.path.join(dirpath, f)
        try:
            text = open(p, encoding="utf-8").read()
        except (UnicodeDecodeError, OSError):
            continue
        for n in needles:
            if re.search(r"(?<![0-9.])" + re.escape(n) + r"(?![0-9])", text):
                hits.append(f"{os.path.relpath(p, root)}: {n}")
if hits:
    print("LEAK: known-answer values or file hashes found:\n  " + "\n  ".join(sorted(hits)), file=sys.stderr)
    sys.exit(1)
PY
fi
[ "$fail" -eq 0 ] || { echo "export refused; nothing written to $dest" >&2; exit 1; }

mkdir -p "$dest"
if [ -d "$dest/.git" ]; then
  rsync -a --delete --exclude .git "$stage/" "$dest/"
else
  rsync -a "$stage/" "$dest/"
fi
echo "public copy written to $dest ($(find "$dest" -path "$dest/.git" -prune -o -type f -print | wc -l | tr -d ' ') files)"
