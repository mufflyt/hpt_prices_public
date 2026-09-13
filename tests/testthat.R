#!/usr/bin/env Rscript
#' Test runner. Run from the repository root:
#'   Rscript tests/testthat.R
#'
#' Every test is offline: fixtures are small synthetic files under
#' tests/testthat/fixtures/ built from the verbatim CMS v3.0 template headers.

if (!base::file.exists("R/00_source_all.R")) {
  base::stop("Run tests from the repository root: Rscript tests/testthat.R")
}

base::source("R/00_source_all.R")

base::options(hpt_repo_root = base::normalizePath("."))
base::Sys.setenv(HPT_DATA_DIR = base::file.path(base::tempdir(), "hpt_data"))

if (!base::requireNamespace("testthat", quietly = TRUE)) {
  base::stop("Package 'testthat' is required.")
}

testthat::test_dir(
  "tests/testthat",
  stop_on_failure = TRUE,
  reporter = "summary"
)
