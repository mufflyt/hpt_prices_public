#!/usr/bin/env Rscript
#' Median price per state x insurance type x code, and the headline table
#' for IUD insertion, colonoscopy, endometrial biopsy, and bariatric surgery.
#'
#' Env vars: HPT_MIN_HOSPITALS (default 3): states with fewer hospitals
#' reporting a code get NA in the headline table (the full table keeps them).
#' HPT_EXCLUDE_ABOVE_GROSS (default true): drop files the validation suite
#' flags in output/files_rates_above_gross.csv.

base::source("R/00_source_all.R")

min_hospitals <- base::as.integer(base::Sys.getenv("HPT_MIN_HOSPITALS", unset = "3"))

# Files where most negotiated rates exceed the listed gross charge usually
# have a per-unit gross column; the validation suite lists them. Excluded by
# default (HPT_EXCLUDE_ABOVE_GROSS=false keeps them).
exclude <- NULL
flag_path <- hpt_path("output", "files_rates_above_gross.csv")
if (base::tolower(base::Sys.getenv("HPT_EXCLUDE_ABOVE_GROSS", unset = "true")) %in% base::c("1", "true", "yes") && base::file.exists(flag_path)) {
  exclude <- read_csv_chr(flag_path)$mrf_file_id
  base::message("Excluding ", base::length(exclude), " file(s) flagged for rates above gross.")
}
# a file held by both sources counts once (R/pipeline.R)
exclude <- base::unique(base::c(exclude, cross_source_duplicate_file_ids()))
write_csv_atomic(tibble::tibble(mrf_file_id = exclude), hpt_path("output", "median_excluded_file_ids.csv"))
medians <- compute_state_medians(exclude_file_ids = exclude)
headline <- state_median_headline(medians, min_hospitals = min_hospitals)
write_csv_atomic(headline, hpt_path("output", "state_insurance_headline.csv"))

base::message("National medians (facility fees, anchor codes):")
headline |> dplyr::filter(.data$state == "US") |> base::print(n = 20)
base::message("Headline table: ", hpt_path("output", "state_insurance_headline.csv"))
