#' Within-hospital payer-to-Medicare price ratios
#'
#' For each hospital that lists both a payer type's rate and a traditional
#' Medicare rate for the same code and fee type, ratio = payer rate /
#' Medicare rate (each the median across that hospital's payer/plan
#' contracts, the same first two stages as R/state_medians.R). The ratio's
#' median across hospitals is an empirical multiplier: comparing within a
#' hospital removes between-hospital differences in which payers list which
#' codes.
#'
#' Built to replace provisional payer multipliers such as emb_colonoscopy's
#' `medicaid_illustrative` (0.70) and `commercial_illustrative` (1.75)
#' scenarios. Facility and professional fees are kept separate because
#' professional fees are what that model scales.

payer_ratio_sql <- function(codes, exclude_file_ids = NULL) {
  exclude_sql <- if (base::length(exclude_file_ids) > 0L) {
    base::paste0(" AND mrf_file_id NOT IN (", sql_string_list(exclude_file_ids), ")")
  } else {
    ""
  }

  base::paste0(
    "WITH base AS (\n",
    "  SELECT unit_id, CAST(concept AS VARCHAR) AS concept, code,\n",
    "         CAST(fee_type AS VARCHAR) AS fee_type,\n",
    "         CAST(payer_type AS VARCHAR) AS payer_type, payer_name, plan_name, negotiated_dollar\n",
    "  FROM v_hospital_rate\n",
    "  WHERE plausible AND payer_type IS NOT NULL AND code IN (", sql_string_list(codes), ")\n",
    "    AND ", rate_row_filter_sql(), exclude_sql, "\n",
    "),\n",
    "plan_rates AS (\n",
    "  SELECT unit_id, concept, code, fee_type, payer_type, payer_name, plan_name, median(negotiated_dollar) AS plan_median\n",
    "  FROM base GROUP BY ALL\n",
    "),\n",
    "unit_rates AS (\n",
    "  SELECT unit_id, concept, code, fee_type, payer_type, median(plan_median) AS unit_median\n",
    "  FROM plan_rates GROUP BY ALL\n",
    "),\n",
    "medicare AS (\n",
    "  SELECT unit_id, code, fee_type, unit_median AS medicare_rate FROM unit_rates WHERE payer_type = 'medicare'\n",
    "),\n",
    "ratios AS (\n",
    "  SELECT u.concept, u.code, u.fee_type, u.payer_type, u.unit_id, u.unit_median / m.medicare_rate AS ratio\n",
    "  FROM unit_rates AS u JOIN medicare AS m USING (unit_id, code, fee_type)\n",
    "  WHERE u.payer_type <> 'medicare' AND m.medicare_rate > 0\n",
    ")\n",
    "SELECT concept, code, fee_type, payer_type,\n",
    "       count(DISTINCT unit_id) AS n_hospitals,\n",
    "       median(ratio) AS median_ratio,\n",
    "       quantile_cont(ratio, 0.25) AS p25_ratio,\n",
    "       quantile_cont(ratio, 0.75) AS p75_ratio\n",
    "FROM ratios GROUP BY ALL\n",
    "ORDER BY code, fee_type, payer_type"
  )
}

#' Compute and save within-hospital payer ratios
#'
#' @param codes Codes to report.
#' @param exclude_file_ids mrf_file_id values to leave out (the medians
#'   step's exclusion list).
compute_payer_ratios <- function(codes, db_path = hpt_database_path(), out_dir = hpt_path("output"),
                                 exclude_file_ids = NULL) {
  ratios <- duckdb_query(payer_ratio_sql(codes, exclude_file_ids), database = db_path, read_only = TRUE)
  base::dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  write_csv_atomic(ratios, base::file.path(out_dir, "payer_to_medicare_ratios.csv"))
  ratios
}
