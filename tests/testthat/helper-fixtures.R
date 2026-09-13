#' Shared test fixtures. testthat::test_dir() changes the working directory
#' to tests/testthat/, so resolve paths against the repo root captured in
#' tests/testthat.R.

repo_root_path <- function() {
  base::getOption("hpt_repo_root", ".")
}

fixture_path <- function(...) {
  base::file.path(repo_root_path(), "tests", "testthat", "fixtures", ...)
}

#' The live codebook minus concepts added after the fixtures were written.
#' The MRF fixtures use CPT 99213 (office visit) as a deliberate non-target
#' "noise" line, so the office_visit_em concept is left out here; the real
#' pipeline loads the full codebook.
test_codebook <- function(exclude_concepts = "office_visit_em") {
  codebook <- load_codebook(base::file.path(repo_root_path(), "config", "codebook.csv"))
  codebook[!codebook$concept %in% exclude_concepts, ]
}
