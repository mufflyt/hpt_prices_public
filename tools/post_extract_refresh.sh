#!/usr/bin/env bash
# Rebuild every downstream output after a fresh Trilliant extract, in order,
# and say what moved.
#
# WHY A SCRIPT. Three changes land together in the next rebuild -- APR-DRG
# delivery codes, conflicted CCN matches no longer credited to a hospital, and
# name matches blocked on ZIP or city -- and each moves numbers that the
# documents quote. Running the stages by hand invites a half-refreshed output
# directory where a state median comes from one build and a figure from
# another, which is how a document ends up quoting two builds at once.
#
# ORDER MATTERS. 09 rebuilds the database from the extract and the crosswalk;
# 10 validates it; 11 recomputes the medians every later stage reads; 12 and 14
# depend on 11; 13, 15, 16 read the database; 17 reads 16's outputs. 18 reads
# 16's outputs too, and additionally needs the CDC WONDER exports, so it runs
# only when they are present and pass the audit (see below).
#
# Usage, from the repository root:
#   tools/post_extract_refresh.sh [before_dir]
# With before_dir (a copy of output/ taken before the rebuild), it writes an
# impact table comparing the headline numbers. Without it, it just rebuilds.
#
# Env: HPT_DATA_DIR, HPT_DUCKDB_MEMORY (default 3GB here: the machine has 16 GB
# and other jobs on it), HPT_BIRTH_APR_DRG=true to include the APR-DRG fallback.
set -uo pipefail

before_dir="${1:-}"
log_dir="${HPT_REFRESH_LOGS:-/tmp/hpt_refresh_$(date +%Y%m%d_%H%M%S)}"
mkdir -p "$log_dir"
export HPT_DUCKDB_MEMORY="${HPT_DUCKDB_MEMORY:-3GB}"

# Validation runs LAST, as docs/appendix.md section L says: it compares the
# saved medians against the database, so running it before 11 rebuilt them
# reported "49,461 saved rows vs 49,773 recomputed" -- a warning about the
# runbook's own order, not about the data.
stages=(
  09_build_database
  11_state_medians
  12_addon_value
  13_ownership_prices
  14_emb_payer_ratios
  15_geographic_figures
  16_childbirth_prices
  17_midwifery_presence
  10_validate
)

echo "Refreshing after the extract. Logs: $log_dir"
failed=()
for stage in "${stages[@]}"; do
  started=$(date +%s)
  if Rscript "analysis/${stage}.R" > "$log_dir/${stage}.log" 2>&1; then
    printf '  ok    %-24s %4ss\n' "$stage" "$(( $(date +%s) - started ))"
  else
    printf '  FAIL  %-24s %4ss  (see %s)\n' "$stage" "$(( $(date +%s) - started ))" "$log_dir/${stage}.log"
    failed+=("$stage")
    # 10 is a report, not a gate: a validation warning must not stop the refresh.
    # Anything else feeds the stages after it, so stop rather than mix builds.
    [ "$stage" = "10_validate" ] || break   # 10 runs last, so nothing depends on it
  fi
done

if [ ${#failed[@]} -gt 0 ]; then
  echo "Stages that failed: ${failed[*]}"
fi

# The childbirth analysis is also run WITH the APR-DRG fallback, which is the
# measurement the extract was rerun for. It writes birth_apr_* files, so the
# MS-DRG build above stays intact and the two can be compared directly. They
# used to share filenames, and then whichever run went last owned output/: an
# impact table compared an MS-DRG "before" against an APR-DRG "after" and
# reported a 14% fall in the Medicaid delivery price that was really 347 extra
# hospitals. Stages 17 and 18 read the unprefixed names, so they always see the
# MS-DRG build rather than whatever ran last.
if [ -z "${HPT_SKIP_APR:-}" ]; then
  echo "APR-DRG fallback run (counts the hospitals it adds):"
  if HPT_BIRTH_APR_DRG=true Rscript analysis/16_childbirth_prices.R > "$log_dir/16_apr_drg.log" 2>&1; then
    grep -i "APR-DRG fallback" "$log_dir/16_apr_drg.log" || echo "  (no APR-DRG rows found in the extract)"
    echo "  MS-DRG build: output/birth_*.csv | fallback build: output/birth_apr_*.csv"
  else
    echo "  FAILED (see $log_dir/16_apr_drg.log)"
  fi
fi

# Stage 18 needs the ten CDC WONDER exports, which are downloaded by hand.
# The preflight reads each file's own Notes block and checks the grouping,
# years and NTSV restrictions against the specification; it takes seconds, and
# it is what stops a mis-built export from reaching a twenty-minute model fit.
# Absent exports are not a failure of the refresh, they are simply not here yet.
if [ -z "${HPT_SKIP_NTSV:-}" ]; then
  echo "CDC WONDER exports (stage 18):"
  if Rscript tools/check_wonder_exports.R > "$log_dir/18_preflight.log" 2>&1; then
    sed 's/^/  /' "$log_dir/18_preflight.log"
    started=$(date +%s)
    if Rscript analysis/18_ntsv_midwife_supply.R > "$log_dir/18_ntsv_midwife_supply.log" 2>&1; then
      printf '  ok    %-24s %4ss\n' "18_ntsv_midwife_supply" "$(( $(date +%s) - started ))"
    else
      printf '  FAIL  %-24s %4ss  (see %s)\n' "18_ntsv_midwife_supply" "$(( $(date +%s) - started ))" "$log_dir/18_ntsv_midwife_supply.log"
      failed+=("18_ntsv_midwife_supply")
    fi
  else
    sed 's/^/  /' "$log_dir/18_preflight.log"
    echo "  skipped: the WONDER exports are not ready (this is not a refresh failure)"
  fi
fi

if [ -n "$before_dir" ] && [ -d "$before_dir" ]; then
  echo
  echo "Impact against $before_dir:"
  Rscript tools/refresh_impact.R "$before_dir" | tee "$log_dir/impact.md"
fi

if [ ${#failed[@]} -gt 0 ]; then
  echo
  echo "FAILED STAGES: ${failed[*]}"
  echo "Done with failures. Logs in $log_dir"
  exit 1
fi
echo "Done. Logs in $log_dir"
