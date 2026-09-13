#!/usr/bin/env Rscript
#' Validation suite: integrity, known answers, plausibility, payer typing,
#' cross-source agreement, CMS OPPS benchmark, raw re-check, coverage.
#' Writes output/validation_report.csv and output/validation_report.md.
#'
#' Offline by default. HPT_VALIDATE_NETWORK=true opts in to re-downloading
#' a sample of own-crawl MRFs and, when no Addendum B is cached, fetching it.

base::source("R/00_source_all.R")
if (!base::exists("run_validation", mode = "function")) {
  base::source("R/validation.R")
}
base::options(hpt_repo_root = base::normalizePath("."))

network <- base::tolower(base::Sys.getenv("HPT_VALIDATE_NETWORK", unset = "false")) %in% base::c("1", "true", "yes")
report <- run_validation(hpt_database_path(), out_dir = hpt_path("output"), network = network)

base::print(
  report |>
    dplyr::transmute(.data$check_id, .data$status, metric = base::signif(.data$metric, 4), detail = stringr::str_trunc(.data$detail, 100)) |>
    base::as.data.frame(),
  right = FALSE
)

if (base::any(report$status == "fail")) {
  base::message("Validation has failures: see ", hpt_path("output", "validation_report.md"))
}
