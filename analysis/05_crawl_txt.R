#!/usr/bin/env Rscript
#' Part C2 step 2: fetch https://{domain}/cms-hpt.txt for every seeded
#' domain, then snowball to any new host named in an mrf-url or
#' source-page-url (vendor-hosted pointer files). Resumable.
#'
#' Env vars: HPT_MAX_ITEMS (pilot cap on seed domains), HPT_PER_HOST_RPS, HPT_MAX_ACTIVE.

base::source("R/00_source_all.R")

seeds <- tibble::as_tibble(arrow::read_parquet(hpt_path("seeds", "domain_seeds.parquet")))
domains <- take_max_items(base::unique(seeds$domain))

result <- crawl_hpt_txt_snowball(domains, state_path = hpt_path("state", "txt_state.csv"), cache_dir = hpt_path("txt_cache"))

write_parquet_atomic(result$locations, hpt_path("discovery", "txt_locations.parquet"))
base::print(dplyr::count(result$state, .data$status, sort = TRUE))
base::message(
  "Locations: ", scales::comma(base::nrow(result$locations)), " from ",
  scales::comma(dplyr::n_distinct(result$locations$domain)), " domains; ",
  scales::comma(dplyr::n_distinct(result$locations$mrf_url)), " distinct MRF URLs."
)
