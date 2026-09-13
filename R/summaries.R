#' Part D: per-hospital summaries and data-quality flags
#'
#' Runs in DuckDB over the Parquet price files so the full national table
#' never has to fit in R's memory. Price rows reach a CCN through their MRF
#' file: `file_ccn` maps each `mrf_file_id` to every CCN the crosswalk (R/ccn_match.R)
#' assigned to it. A system-wide file therefore contributes its rates to
#' each of its hospitals, which is what the file itself asserts.

#' @param price_globs Parquet globs for price rows (Trilliant and own crawl).
#' @param file_ccn_path Parquet with columns mrf_file_id, ccn, ccn_match_method.
#' @param out_path Output Parquet path for the summary.
summarize_prices <- function(price_globs, file_ccn_path, out_path) {
  prices_sql <- base::paste(
    base::sprintf("SELECT * FROM read_parquet(%s, hive_partitioning = true, union_by_name = true)", sql_string(price_globs)),
    collapse = " UNION ALL BY NAME "
  )

  summary_sql <- base::paste0(
    "WITH prices AS (", prices_sql, "),\n",
    "keyed AS (\n",
    "  SELECT f.ccn, f.ccn_match_method, p.*\n",
    "  FROM prices AS p\n",
    "  LEFT JOIN read_parquet(", sql_string(file_ccn_path), ") AS f USING (mrf_file_id)\n",
    ")\n",
    "SELECT\n",
    "  ccn, concept, code, source,\n",
    "  count(DISTINCT mrf_file_id) AS n_files,\n",
    "  min(ccn_match_method) AS ccn_match_method,\n",
    "  count(*) AS n_rows,\n",
    "  count(DISTINCT payer_name) FILTER (WHERE payer_name IS NOT NULL) AS n_payers,\n",
    "  count(DISTINCT payer_name || '|' || coalesce(plan_name, '')) FILTER (WHERE payer_name IS NOT NULL) AS n_payer_plans,\n",
    "  median(gross) AS gross_median,\n",
    "  median(discounted_cash) AS cash_median,\n",
    "  min(negotiated_dollar) AS negotiated_min,\n",
    "  quantile_cont(negotiated_dollar, 0.25) AS negotiated_p25,\n",
    "  median(negotiated_dollar) AS negotiated_median,\n",
    "  quantile_cont(negotiated_dollar, 0.75) AS negotiated_p75,\n",
    "  max(negotiated_dollar) AS negotiated_max,\n",
    "  median(median_amount) AS allowed_median_of_medians,\n",
    "  avg(CASE WHEN negotiated_dollar IS NULL AND negotiated_percentage IS NOT NULL THEN 1.0 ELSE 0.0 END)\n",
    "    FILTER (WHERE payer_name IS NOT NULL) AS share_percent_only,\n",
    "  avg(CASE WHEN lower(methodology) LIKE '%case rate%' THEN 1.0 ELSE 0.0 END)\n",
    "    FILTER (WHERE payer_name IS NOT NULL) AS share_case_rate,\n",
    "  bool_and(type_verified) AS all_type_verified,\n",
    "  max(last_updated_on) AS last_updated_on\n",
    "FROM keyed\n",
    "GROUP BY ALL"
  )

  run_duckdb_sql(base::sprintf("COPY (%s) TO %s (FORMAT parquet);", summary_sql, sql_string(out_path)))
  base::invisible(out_path)
}

#' Flag files whose rates look like placeholders or case rates
#'
#' Two patterns seen in live data on 2026-09-12 (HCA Houston Southeast):
#' - one payer/plan with the identical dollar amount across 3+ different
#'   colonoscopy codes (a case rate, not a code-specific price);
#' - identical MS-DRG 742 and 743 rates for a payer/plan, although the CC/MCC
#'   split should cost more.
#' These are flagged, not dropped, so analysts can decide.
flag_rate_patterns <- function(price_globs, out_path) {
  prices_sql <- base::paste(
    base::sprintf("SELECT * FROM read_parquet(%s, hive_partitioning = true, union_by_name = true)", sql_string(price_globs)),
    collapse = " UNION ALL BY NAME "
  )

  flag_sql <- base::paste0(
    "WITH prices AS (", prices_sql, "),\n",
    "colon AS (\n",
    "  SELECT mrf_file_id, payer_name, plan_name, negotiated_dollar, count(DISTINCT code) AS n_codes\n",
    "  FROM prices WHERE concept = 'colonoscopy' AND negotiated_dollar IS NOT NULL AND payer_name IS NOT NULL\n",
    "  GROUP BY ALL HAVING count(DISTINCT code) >= 3\n",
    "),\n",
    "drg AS (\n",
    "  SELECT mrf_file_id, payer_name, plan_name,\n",
    "         max(negotiated_dollar) FILTER (WHERE code = '742') AS drg742,\n",
    "         max(negotiated_dollar) FILTER (WHERE code = '743') AS drg743\n",
    "  FROM prices WHERE concept = 'drg_uterine_nonmalignant' AND payer_name IS NOT NULL\n",
    "  GROUP BY ALL\n",
    ")\n",
    "SELECT mrf_file_id, 'identical_rate_across_colonoscopy_codes' AS flag, count(*) AS n_payer_plans\n",
    "FROM colon GROUP BY mrf_file_id\n",
    "UNION ALL\n",
    "SELECT mrf_file_id, 'drg_742_equals_743', count(*)\n",
    "FROM drg WHERE drg742 IS NOT NULL AND drg742 = drg743 GROUP BY mrf_file_id"
  )

  run_duckdb_sql(base::sprintf("COPY (%s) TO %s (FORMAT parquet);", flag_sql, sql_string(out_path)))
  base::invisible(out_path)
}
