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
# depend on 11; 13, 15, 16 read the database; 17 reads 16's outputs. 18 is
# skipped: it needs the CDC WONDER exports (docs/childbirth_analytic_spec.md).
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

stages=(
  09_build_database
  10_validate
  11_state_medians
  12_addon_value
  13_ownership_prices
  14_emb_payer_ratios
  15_geographic_figures
  16_childbirth_prices
  17_midwifery_presence
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
    [ "$stage" = "10_validate" ] || break
  fi
done

if [ ${#failed[@]} -gt 0 ]; then
  echo "Stages that failed: ${failed[*]}"
fi

# The childbirth analysis is also run WITH the APR-DRG fallback, which is the
# measurement the extract was rerun for. Its outputs overwrite 16's, so it runs
# last and the baseline above is what the documents quote.
if [ -z "${HPT_SKIP_APR:-}" ]; then
  echo "APR-DRG fallback run (counts the hospitals it adds):"
  if HPT_BIRTH_APR_DRG=true Rscript analysis/16_childbirth_prices.R > "$log_dir/16_apr_drg.log" 2>&1; then
    grep -i "APR-DRG fallback" "$log_dir/16_apr_drg.log" || echo "  (no APR-DRG rows found in the extract)"
  else
    echo "  FAILED (see $log_dir/16_apr_drg.log)"
  fi
fi

if [ -n "$before_dir" ] && [ -d "$before_dir" ]; then
  echo
  echo "Impact against $before_dir:"
  Rscript tools/refresh_impact.R "$before_dir" | tee "$log_dir/impact.md"
fi

echo "Done. Logs in $log_dir"
