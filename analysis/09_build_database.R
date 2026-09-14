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

# per-diem MS-DRG rates become stay prices with CMS Table 5 lengths of stay
# (downloaded on first run to reference/cms_ipps_drg)
download_ipps_drg_tables()
amounts <- load_ipps_standardized_amounts()
# national per-day Medicare payment (wage index 1: Table 1B amounts + capital), to catch mislabeled per diems
national_base <- base::sum(amounts$low_wage) + amounts$capital
drg_los <- load_ipps_drg_weights() |>
  dplyr::transmute(.data$code, .data$gmlos, medicare_per_day = national_base * .data$weight / .data$gmlos)

build_hpt_database(
  price_globs,
  crosswalk_path = hpt_path("crosswalk", "facility_ccn.parquet"),
  universe = reference$universe,
  codebook = load_codebook("config/codebook.csv"),
  drg_los = drg_los
)
# Both copies of a file held by both sources stay in the database so the
# validation suite can compare them; the medians step excludes duplicates.
