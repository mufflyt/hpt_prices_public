#' Median price per state x insurance type x code
#'
#' Three-stage median so that neither repeated charge lines nor a hospital
#' listing 40 plans outweighs the rest:
#'   1. per hospital x code x payer/plan: median of that contract's rows
#'      (files often repeat one contract rate on many charge lines);
#'   2. per hospital (CCN; unmatched files count as their own unit) x code x
#'      insurance type: median across that hospital's payer/plan contracts;
#'   3. per state x code x insurance type: median (and IQR) across hospitals.
#' `n_rates` counts payer/plan contracts.
#' "self_pay_cash" (discounted cash price) and "gross_charge" are added as
#' insurance types so they sit next to the negotiated rates.
#'
#' Rules applied (all documented in the output's `rule` attribute):
#' - negotiated rates: only rows flagged `plausible` at load (R/duckdb_store.R);
#' - facility and professional fees are reported separately (`fee_type`,
#'   set at load: explicit billing class, else inferred from gross where the
#'   code's facility and professional gross charges separate, else facility;
#'   inferred-professional rows are left out of both; see
#'   case_line_multiple() in R/duckdb_store.R);
#' - outpatient procedures (colonoscopy, EMB, IUD, pathology, D&C, hysteroscopy) exclude rows
#'   whose setting is explicitly inpatient and operating-room case lines
#'   (`case_line`, see case_line_multiple() in R/duckdb_store.R); office
#'   procedures (EMB, IUD insertion) also exclude case-rate and per-diem rows
#'   (rate_row_filter_sql());
#' - a national row (state = "US") uses the same two stages over all units.

outpatient_concepts <- function() {
  base::c(
    "colonoscopy", "emb", "iud_insertion", "iud_device", "surgical_pathology",
    "dc", "hysteroscopy_sampling"
  )
}

#' Office procedures: done in a clinic or procedure room as a single service,
#' but also listed by some hospitals as the primary procedure of an
#' operating-room case
office_procedure_concepts <- function() {
  base::c("emb", "iud_insertion")
}

#' SQL predicate that keeps a rate row unless it prices a different product
#' than the procedure itself. Shared by the state medians, the payer ratios,
#' and the ownership prices.
#' - any code: drop rows whose fee type was inferred professional from a
#'   blank billing class (out of facility fees, and too uncertain to count as
#'   professional fees; see case_line_multiple() in R/duckdb_store.R);
#' - outpatient procedures: drop rows on an inpatient line or an
#'   operating-room case line (`case_line`);
#' - office procedures: also drop package rates. CMS defines a case rate as
#'   "a flat rate for a package of items and services triggered by a primary
#'   procedure" (per diem likewise prices a stay), so a case rate for 58300 or
#'   58100 prices the surgical case, not the insertion or the biopsy. In the
#'   2026-07-21 snapshot, commercial 58300 case-rate rows ran about ten times
#'   the fee-schedule and percent-of-charges rows. Colonoscopy is not an
#'   office procedure: its case rate is the endoscopy encounter itself, so it
#'   is kept.
rate_row_filter_sql <- function() {
  base::paste0(
    "NOT (fee_type_inferred AND fee_type = 'professional') ",
    "AND NOT (CAST(concept AS VARCHAR) IN (", sql_string_list(outpatient_concepts()), ") ",
    "AND (setting = 'inpatient' OR case_line)) ",
    "AND NOT (CAST(concept AS VARCHAR) IN (", sql_string_list(office_procedure_concepts()), ") ",
    "AND methodology IN ('case rate', 'per diem'))"
  )
}

state_medians_sql <- function(exclude_file_ids = NULL) {
  exclude_sql <- if (base::length(exclude_file_ids) > 0L) {
    base::paste0(" AND mrf_file_id NOT IN (", sql_string_list(exclude_file_ids), ")")
  } else {
    ""
  }

  base::paste0(
    "WITH base AS (\n",
    "  SELECT unit_id, state, CAST(concept AS VARCHAR) AS concept, code, anchor,\n",
    "         CAST(fee_type AS VARCHAR) AS fee_type,\n",
    "         CAST(payer_type AS VARCHAR) AS payer_type, payer_name, plan_name, negotiated_dollar, plausible, discounted_cash, gross, description\n",
    "  FROM v_hospital_rate\n",
    "  WHERE state IS NOT NULL AND ", rate_row_filter_sql(), exclude_sql, "\n",
    "),\n",
    # a payer/plan contract counts once per hospital, however many charge
    # lines repeat it (HCA lists MS-DRG 742 on 69 lines)
    "plan_rates AS (\n",
    "  SELECT state, unit_id, concept, code, anchor, fee_type, payer_type, payer_name, plan_name,\n",
    "         median(negotiated_dollar) AS plan_median, count(*) AS n_rows\n",
    "  FROM base WHERE plausible AND payer_type IS NOT NULL GROUP BY ALL\n",
    "),\n",
    "unit_rates AS (\n",
    "  SELECT state, unit_id, concept, code, anchor, fee_type, payer_type AS insurance_type,\n",
    "         median(plan_median) AS unit_median, count(*) AS n_rates\n",
    "  FROM plan_rates GROUP BY ALL\n",
    "  UNION ALL\n",
    # cash and gross are per charge line, repeated on every payer row: dedupe lines first
    "  SELECT state, unit_id, concept, code, anchor, fee_type, 'self_pay_cash', median(discounted_cash), count(*)\n",
    "  FROM (SELECT DISTINCT state, unit_id, concept, code, anchor, fee_type, description, discounted_cash FROM base\n",
    "        WHERE discounted_cash > 1 AND discounted_cash < 2000000) GROUP BY ALL\n",
    "  UNION ALL\n",
    "  SELECT state, unit_id, concept, code, anchor, fee_type, 'gross_charge', median(gross), count(*)\n",
    "  FROM (SELECT DISTINCT state, unit_id, concept, code, anchor, fee_type, description, gross FROM base\n",
    "        WHERE gross > 1 AND gross < 5000000) GROUP BY ALL\n",
    "),\n",
    "by_state AS (\n",
    "  SELECT state, concept, code, anchor, fee_type, insurance_type,\n",
    "         median(unit_median) AS median_price, quantile_cont(unit_median, 0.25) AS p25, quantile_cont(unit_median, 0.75) AS p75,\n",
    "         count(DISTINCT unit_id) AS n_hospitals, sum(n_rates) AS n_rates\n",
    "  FROM unit_rates GROUP BY ALL\n",
    "),\n",
    "national AS (\n",
    "  SELECT 'US' AS state, concept, code, anchor, fee_type, insurance_type,\n",
    "         median(unit_median), quantile_cont(unit_median, 0.25), quantile_cont(unit_median, 0.75),\n",
    "         count(DISTINCT unit_id), sum(n_rates)\n",
    "  FROM unit_rates GROUP BY ALL\n",
    ")\n",
    "SELECT * FROM by_state UNION ALL SELECT * FROM national\n",
    "ORDER BY concept, code, fee_type, insurance_type, state"
  )
}

#' Compute and save the state medians
#'
#' @param exclude_file_ids `mrf_file_id` values to leave out, e.g. files the
#'   validation suite flags because most of their rates exceed gross
#'   (output/files_rates_above_gross.csv). Match on mrf_file_id, never
#'   file_id, which is renumbered on every rebuild.
#' @return Tibble with state, concept, code, anchor, fee_type,
#'   insurance_type, median_price, p25, p75, n_hospitals, n_rates.
compute_state_medians <- function(db_path = hpt_database_path(), out_dir = hpt_path("output"), exclude_file_ids = NULL) {
  medians <- duckdb_query(state_medians_sql(exclude_file_ids), database = db_path, read_only = TRUE)
  base::attr(medians, "excluded_file_ids") <- exclude_file_ids
  base::dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  write_parquet_atomic(medians, base::file.path(out_dir, "state_insurance_medians.parquet"))
  write_csv_atomic(medians, base::file.path(out_dir, "state_insurance_medians.csv"))
  medians
}

#' The headline table the study asks for: anchor codes, facility fees,
#' one row per state x insurance type, one column per procedure
state_median_headline <- function(medians, min_hospitals = 3L) {
  anchors <- base::c(
    "45378" = "colonoscopy_45378", "58100" = "emb_58100", "58300" = "iud_insertion_58300",
    "43775" = "sleeve_gastrectomy_43775", "43644" = "gastric_bypass_43644",
    # the inpatient bariatric facility payment; CPT-coded bariatric lines are
    # usually partial (a small fraction of the national Medicare DRG 621 rate)
    "621" = "bariatric_ms_drg_621"
  )

  # Self-pay is reported as the discounted cash price ("self_pay_cash"). The
  # "self_pay" payer-row type is thin (71 hospitals for 58300 vs 1,235 with a
  # cash price) and mixes clinic and operating-room lines, so it stays in the
  # full table only.
  medians |>
    dplyr::filter(.data$code %in% base::names(anchors), .data$fee_type == "facility", .data$insurance_type != "self_pay") |>
    dplyr::mutate(
      procedure = base::unname(anchors[.data$code]),
      value = dplyr::if_else(.data$n_hospitals >= min_hospitals, .data$median_price, NA_real_)
    ) |>
    dplyr::select("state", "insurance_type", "procedure", "value") |>
    tidyr::pivot_wider(names_from = "procedure", values_from = "value") |>
    dplyr::arrange(.data$state != "US", .data$state, .data$insurance_type)
}
