#!/usr/bin/env Rscript
#' Part C2 step 3: for seeded domains whose cms-hpt.txt could not be used,
#' follow the homepage's "Price Transparency" footer link (45 CFR
#' 180.50(d)(6)(ii)) to the MRF links. Honors robots.txt. Resumable.

base::source("R/00_source_all.R")

seeds <- tibble::as_tibble(arrow::read_parquet(hpt_path("seeds", "domain_seeds.parquet")))
txt_state <- read_csv_chr(hpt_path("state", "txt_state.csv"))
txt_ok <- txt_state$domain[txt_state$status == "ok"]

# a CCN is TXT-covered if any of its seed domains returned a usable file
ccn_ok <- seeds |>
  dplyr::group_by(.data$ccn) |>
  dplyr::summarise(any_ok = base::any(.data$domain %in% txt_ok), .groups = "drop")
fallback_domains <- seeds |>
  dplyr::filter(.data$ccn %in% ccn_ok$ccn[!ccn_ok$any_ok], !.data$is_variant) |>
  dplyr::distinct(.data$domain) |>
  dplyr::pull(.data$domain) |>
  take_max_items()

base::message("Footer fallback on ", scales::comma(base::length(fallback_domains)), " domains.")
footer <- crawl_footer(fallback_domains, state_path = hpt_path("state", "footer_state.csv"))

write_parquet_atomic(footer, hpt_path("discovery", "footer_links.parquet"))
base::print(dplyr::count(footer, .data$status, sort = TRUE))
