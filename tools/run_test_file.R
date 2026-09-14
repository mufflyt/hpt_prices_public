#!/usr/bin/env Rscript
#' Run one test file with the same setup as tests/testthat.R.
#'
#' Sources every R/ function file, points HPT_DATA_DIR at a temporary
#' directory (tests never touch real data), loads the shared helpers from
#' tests/testthat/helper-fixtures.R, and runs a single file. Faster than the
#' full suite while iterating on one module.
#'
#' Usage (from the repository root):
#'   Rscript tools/run_test_file.R tests/testthat/test-geo.R [reporter]
#' reporter defaults to "summary"; "progress" and "check" also work.

args <- base::commandArgs(trailingOnly = TRUE)
if (base::length(args) < 1L) {
  base::stop("Usage: Rscript tools/run_test_file.R tests/testthat/test-<name>.R [reporter]")
}
if (!base::file.exists("R/00_source_all.R")) {
  base::stop("Run from the repository root.")
}

base::suppressMessages(base::source("R/00_source_all.R"))
base::options(hpt_repo_root = base::normalizePath("."))
base::Sys.setenv(HPT_DATA_DIR = base::file.path(base::tempdir(), "hpt_data"))

helpers <- base::new.env(parent = base::globalenv())
base::sys.source("tests/testthat/helper-fixtures.R", envir = helpers)

testthat::test_file(
  args[[1]],
  reporter = if (base::length(args) >= 2L) args[[2]] else "summary",
  package = NULL,
  load_helpers = FALSE,
  env = helpers,
  stop_on_failure = TRUE
)
