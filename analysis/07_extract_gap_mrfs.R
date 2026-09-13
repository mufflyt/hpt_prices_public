#!/usr/bin/env Rscript
#' Part C2 step 4: download, parse, and store every MRF found by the TXT
#' crawl and footer fallback that no source has extracted yet. Then re-run
#' analysis/02_ccn_crosswalk.R to match the new files to CCNs.
#'
#' Env vars: HPT_MAX_ITEMS (pilot cap), HPT_KEEP_RAW.

base::source("R/00_source_all.R")

codebook <- load_codebook("config/codebook.csv")
already <- extracted_url_keys()

txt_urls <- if (base::file.exists(hpt_path("discovery", "txt_locations.parquet"))) {
  arrow::read_parquet(hpt_path("discovery", "txt_locations.parquet"))$mrf_url
}
footer_urls <- if (base::file.exists(hpt_path("discovery", "footer_links.parquet"))) {
  footer <- arrow::read_parquet(hpt_path("discovery", "footer_links.parquet"))
  footer$mrf_url[footer$status == "ok"]
}

urls <- base::unique(stats::na.omit(base::c(txt_urls, footer_urls)))
urls <- urls[!normalize_url_key(urls) %in% already]
base::message("C2: ", scales::comma(base::length(urls)), " discovered MRF URLs not yet extracted.")

extract_gap_mrfs(urls, codebook, stage = "discovery_c2") |>
  dplyr::count(.data$status) |>
  base::print()
