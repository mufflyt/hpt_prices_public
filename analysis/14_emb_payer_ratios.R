#!/usr/bin/env Rscript
#' Empirical payer-to-Medicare multipliers for the emb_colonoscopy model's
#' payer scenarios (currently provisional: Medicaid 0.70x, commercial 1.75x),
#' plus national medians by payer type for the codes that model prices.

base::source("R/00_source_all.R")

emb_codes <- base::c("58100", "58120", "58558", "45378", "88305", "99213")
exclude <- if (base::file.exists(hpt_path("output", "median_excluded_file_ids.csv"))) {
  read_csv_chr(hpt_path("output", "median_excluded_file_ids.csv"))$mrf_file_id
}

ratios <- compute_payer_ratios(emb_codes, exclude_file_ids = exclude)
base::print(ratios |>
  dplyr::filter(.data$payer_type %in% base::c("commercial", "medicaid", "medicare_advantage")) |>
  dplyr::mutate(dplyr::across(dplyr::ends_with("ratio"), ~ base::round(.x, 2))), n = 60)

medians <- tibble::as_tibble(arrow::read_parquet(hpt_path("output", "state_insurance_medians.parquet"))) |>
  dplyr::filter(.data$state == "US", .data$code %in% emb_codes,
                .data$insurance_type %in% base::c("commercial", "medicare", "medicare_advantage", "medicaid", "self_pay_cash")) |>
  dplyr::select("code", "fee_type", "insurance_type", "median_price", "p25", "p75", "n_hospitals")
write_csv_atomic(medians, hpt_path("output", "emb_manuscript_national_medians.csv"))
base::message("Wrote payer_to_medicare_ratios.csv and emb_manuscript_national_medians.csv")
