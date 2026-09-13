#!/usr/bin/env Rscript
#' Part C2 step 1: candidate website domains for CCNs still uncovered.
#'
#' Seeds: cms-hpt-tracker (manifest + gaps), TPAFS (all three URL hosts),
#' DoltHub (2022-2023 snapshots), HIFLD 2020 hospital websites (matched to
#' the CMS roster by name and address), plus apex/www/parent variants.

base::source("R/00_source_all.R")

reference <- load_reference_inputs()
covered <- base::union(covered_ccns(), {
  state <- read_gap_extract_state()
  ok_urls <- normalize_url_key(state$url[state$status == "ok"])
  reference$tracker$manifest$ccn[normalize_url_key(reference$tracker$manifest$mrf_url) %in% ok_urls]
})
uncovered <- base::setdiff(reference$universe$facility_id, covered)
base::message("Uncovered CCNs: ", scales::comma(base::length(uncovered)), " of ", scales::comma(base::nrow(reference$universe)))

tpafs_path <- hpt_path("seeds", "tpafs_machine_readable_links.csv")
if (!base::file.exists(tpafs_path)) {
  download_public_file(
    "https://raw.githubusercontent.com/TPAFS/transparency-data/main/price_transparency/hospitals/machine_readable_links.csv",
    tpafs_path
  )
}

seeds <- build_domain_seeds(
  ccns = uncovered,
  tracker_manifest = reference$tracker$manifest,
  tracker_gaps = reference$tracker$gaps,
  tpafs_links = read_csv_chr(tpafs_path),
  dolthub_tables = fetch_dolthub_seed_tables(),
  hifld_tbl = fetch_hifld_hospitals(),
  roster_tbl = reference$universe
)

write_parquet_atomic(seeds, hpt_path("seeds", "domain_seeds.parquet"))
base::message(
  "Seeds: ", scales::comma(base::nrow(seeds)), " rows, ", scales::comma(dplyr::n_distinct(seeds$ccn)),
  " CCNs, ", scales::comma(dplyr::n_distinct(seeds$domain)), " distinct domains."
)
base::print(dplyr::count(seeds, .data$seed_source))
