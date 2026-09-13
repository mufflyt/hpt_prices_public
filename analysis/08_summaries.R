#!/usr/bin/env Rscript
#' Part D: per-CCN x code summaries, rate-pattern flags, and coverage.
#' Re-runs the CCN crosswalk first so every parsed file is matched.

base::source("R/00_source_all.R")

reference <- load_reference_inputs()
crosswalk <- run_ccn_crosswalk(reference)

price_globs <- base::c(
  hpt_path("prices", "source=trilliant", "prices", "**", "*.parquet"),
  hpt_path("prices", "source=own_crawl", "*.parquet")
)
price_globs <- price_globs[base::vapply(price_globs, function(g) base::length(Sys.glob(g)) > 0L, base::logical(1))]

if (base::length(price_globs) == 0L) {
  base::stop("No price files found. Run 01 (Trilliant) and/or the gap crawl first.")
}

summary_path <- hpt_path("output", "ccn_code_summary.parquet")
flags_path <- hpt_path("output", "rate_pattern_flags.parquet")
summarize_prices(price_globs, hpt_path("crosswalk", "file_ccn.parquet"), summary_path)
flag_rate_patterns(price_globs, flags_path)

summary <- tibble::as_tibble(arrow::read_parquet(summary_path)) |>
  dplyr::left_join(
    reference$universe |> dplyr::select(ccn = "facility_id", "facility_name", "state", "hospital_type"),
    by = "ccn"
  )
write_csv_atomic(summary, hpt_path("output", "ccn_code_summary.csv"))

base::message("CCNs with a price, by concept:")
summary |>
  dplyr::filter(!base::is.na(.data$ccn)) |>
  dplyr::group_by(.data$concept) |>
  dplyr::summarise(ccns = dplyr::n_distinct(.data$ccn), median_negotiated = stats::median(.data$negotiated_median, na.rm = TRUE)) |>
  base::print()
base::print(crosswalk$coverage$overall)
