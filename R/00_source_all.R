#' Source every R/ function file in dependency order
#'
#' Like emb_colonoscopy, this project is a set of sourced scripts rather than
#' an installed package. `analysis/` scripts and `tests/testthat.R` call
#' `source("R/00_source_all.R")` once at the top.

required_packages <- c(
  "readr", "dplyr", "tibble", "tidyr", "purrr", "stringr", "rlang",
  "httr2", "curl", "openssl", "jsonlite", "yyjsonr", "arrow",
  "rvest", "xml2", "scales", "ggplot2", "tidyselect"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop(
    "Required packages not installed: ", paste(missing_packages, collapse = ", "),
    "\nInstall with: install.packages(c(",
    paste0('"', missing_packages, '"', collapse = ", "), "))"
  )
}

suppressPackageStartupMessages({
  library(dplyr)
})

source_files <- c(
  "../config/paths.R",
  "utils.R",
  "schema.R",
  "codes.R",
  "http.R",
  "duckdb_cli.R",
  # Part B: hospital universe and CCN crosswalk
  "hospital_universe.R",
  "tracker.R",
  "ccn_match.R",
  # Part A: Trilliant lake extract (needs duckdb_cli.R, codes.R, schema.R)
  "trilliant.R",
  # Part C: gap crawl
  "txt_discovery.R",
  "domain_seeds.R",
  "footer_discovery.R",
  "mrf_download.R",
  "mrf_probe.R",
  "parse_csv.R",
  "parse_json.R",
  # Part D
  "payer_type.R",
  "duckdb_store.R",
  "state_medians.R",
  "ownership.R",
  "payer_ratios.R",
  "addon_economics.R",
  "validation.R",
  "geo_figures.R",
  "birth_prices.R",
  "midwifery_link.R",
  "ntsv_county.R",
  "wonder_preflight.R",
  "summaries.R",
  "pipeline.R"
)

for (source_file in source_files) {
  path <- file.path("R", source_file)
  if (!file.exists(path)) {
    stop("Missing source file: ", path)
  }
  base::source(path, local = FALSE)
}

base::message("All R/ function files sourced (", length(source_files), " files).")
