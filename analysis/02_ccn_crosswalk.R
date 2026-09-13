#!/usr/bin/env Rscript
#' Part B: link every MRF facility record (Trilliant and own crawl) to CCNs.
#'
#' Downloads the CMS roster, Hospital Enrollments (NPI to CCN), the AHRQ
#' CHSP linkage, and a pinned cms-hpt-tracker snapshot on first run, then
#' matches by MRF URL, NPI, and name/address (R/ccn_match.R). Re-run after
#' the gap crawl (07) so newly parsed files are matched too.
#'
#' Env vars: HPT_REFRESH_REFERENCE=true re-downloads the reference inputs.

base::source("R/00_source_all.R")

refresh <- base::tolower(base::Sys.getenv("HPT_REFRESH_REFERENCE", unset = "false")) %in% base::c("1", "true", "yes")
reference <- load_reference_inputs(refresh = refresh)
result <- run_ccn_crosswalk(reference)

overall <- result$coverage$overall
base::message("CCN coverage:")
base::print(overall)
base::message("Crosswalk: ", hpt_path("crosswalk", "facility_ccn.parquet"))
