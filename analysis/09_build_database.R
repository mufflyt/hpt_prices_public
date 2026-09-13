#!/usr/bin/env Rscript
#' Build HPT_DATA_DIR/hpt.duckdb (star schema, readable by R duckdb 1.4.4)
#' from every extracted price source, after re-running the CCN crosswalk.
#'
#' Run after 01 (Trilliant) and/or the gap crawl (03-07).

base::source("R/00_source_all.R")

reference <- load_reference_inputs()
run_ccn_crosswalk(reference)

price_globs <- base::c(
  hpt_path("prices", "source=trilliant", "prices", "**", "*.parquet"),
  hpt_path("prices", "source=own_crawl", "*.parquet")
)
price_globs <- price_globs[base::vapply(price_globs, function(g) base::length(base::Sys.glob(g)) > 0L, base::logical(1))]

if (base::length(price_globs) == 0L) {
  base::stop("No price files found. Run 01 (Trilliant) and/or the gap crawl first.")
}

build_hpt_database(
  price_globs,
  crosswalk_path = hpt_path("crosswalk", "facility_ccn.parquet"),
  universe = reference$universe,
  codebook = load_codebook("config/codebook.csv")
)
# Both copies of a file held by both sources stay in the database so the
# validation suite can compare them; the medians step excludes duplicates.
