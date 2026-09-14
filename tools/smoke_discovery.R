#!/usr/bin/env Rscript
#' Live smoke test for cms-hpt.txt discovery and the footer fallback.
#'
#' Not part of the offline suite: it makes real requests to hospital websites
#' (1 request per second per host, the pipeline's user agent) and counts every
#' one through a pass-through mock. Use it after changing R/txt_discovery.R or
#' R/footer_discovery.R to see real-world status classes, snowballing to
#' vendor hosts, and footer-link detection.
#'
#' Usage (from the repository root):
#'   Rscript tools/smoke_discovery.R [round0|snowball] [gap_sample.csv]
#' round0 (default) crawls the seed domains once; snowball follows new hosts
#' for two more rounds, then runs the footer fallback on five known domains.
#' Seeds: gap_sample.csv (columns ccn, hospital_name, seeded_domain) if given,
#' else a seeded sample of HPT_SMOKE_N (default 20) tracker gaps with a seeded
#' domain, from HPT_DATA_DIR/reference/tracker/<commit>/gaps.csv, plus five
#' manifest domains. Crawl state, the TXT cache, and the output CSVs go to
#' HPT_SMOKE_DIR (default: a new temporary directory), never to the real data
#' directory.

args <- base::commandArgs(trailingOnly = TRUE)
mode <- if (base::length(args) >= 1L) args[[1]] else "round0"
base::stopifnot(mode %in% base::c("round0", "snowball"))

# resolve the seed sample from the real data directory before pointing
# HPT_DATA_DIR at the smoke directory
base::source("config/paths.R")
gap_sample <- if (base::length(args) >= 2L) {
  readr::read_csv(args[[2]], show_col_types = FALSE, col_types = readr::cols(.default = "c"))
} else {
  gaps_path <- base::list.files(hpt_path("reference", "tracker"), pattern = "^gaps\\.csv$", recursive = TRUE, full.names = TRUE)
  if (base::length(gaps_path) == 0L) base::stop("No tracker gaps.csv under ", hpt_path("reference", "tracker"), "; pass a gap sample CSV.")
  gaps <- readr::read_csv(gaps_path[[1]], show_col_types = FALSE, col_types = readr::cols(.default = "c"))
  gaps <- gaps[!base::is.na(gaps$seeded_domain) & base::nzchar(gaps$seeded_domain), ]
  base::set.seed(20260912)
  gaps[base::sample(base::nrow(gaps), base::min(base::nrow(gaps), base::as.integer(base::Sys.getenv("HPT_SMOKE_N", "20")))), ]
}

smoke_dir <- base::Sys.getenv("HPT_SMOKE_DIR", unset = base::tempfile("hpt_smoke_"))
base::dir.create(smoke_dir, recursive = TRUE, showWarnings = FALSE)
base::Sys.setenv(HPT_DATA_DIR = smoke_dir, HPT_PER_HOST_RPS = "1")
base::suppressMessages(base::source("R/00_source_all.R"))
base::message("Smoke directory: ", smoke_dir)

request_log <- base::new.env()
request_log$urls <- base::character(0)
httr2::local_mocked_responses(function(req) {
  request_log$urls <- base::c(request_log$urls, req$url)
  NULL
})

manifest_domains <- base::c("southeasthealth.org", "mmcenters.com", "nyulangone.org", "namccares.com", "mizellmh.com")
seeds <- base::unique(base::c(gap_sample$seeded_domain, manifest_domains))
base::message("Seeds before junk filter: ", base::length(seeds))
seeds <- seeds[!is_junk_hpt_host(seeds)]
base::message("Seeds after junk filter: ", base::length(seeds))

state_path <- hpt_path("smoke_txt_state.csv")
cache_dir <- hpt_path("smoke_txt_cache")
if (mode == "round0" && base::file.exists(state_path)) base::file.remove(state_path)

if (mode == "round0") {
  started <- base::Sys.time()
  state0 <- crawl_hpt_txt(seeds, state_path = state_path, cache_dir = cache_dir)
  elapsed <- base::as.numeric(base::difftime(base::Sys.time(), started, units = "secs"))
  locs0 <- collect_hpt_txt_locations(state0, cache_dir)
  candidates <- snowball_hosts(locs0, base::c(seeds, state0$domain))
  base::message("Round 0: ", base::nrow(state0), " domains, ", base::length(request_log$urls), " requests, ", base::round(elapsed, 1), " s")
  print(base::table(state0$status))
  print(state0[, base::c("domain", "status", "http_status", "n_locations", "final_url")], n = 50, width = 200)
  base::message("Snowball candidates after round 0: ", base::length(candidates))
  print(candidates)
}

if (mode == "snowball") {
  started <- base::Sys.time()
  result <- crawl_hpt_txt_snowball(seeds, state_path = state_path, cache_dir = cache_dir, max_rounds = 2L)
  elapsed <- base::as.numeric(base::difftime(base::Sys.time(), started, units = "secs"))
  txt_requests <- base::length(request_log$urls)
  base::message("Snowball: ", base::nrow(result$state), " domains, ", txt_requests, " requests, ", base::round(elapsed, 1), " s")
  print(dplyr::count(result$state, .data$round, .data$status), n = 50)
  print(result$state |> dplyr::filter(.data$round > 0) |> dplyr::select("round", "domain", "status", "http_status", "n_locations", "final_url"), n = 60, width = 200)
  print(result$snowball, n = 80)
  base::message("Locations: ", base::nrow(result$locations), " from ", dplyr::n_distinct(result$locations$domain), " domains; distinct mrf_url: ", dplyr::n_distinct(result$locations$mrf_url))
  print(dplyr::count(result$locations, .data$domain, sort = TRUE), n = 40)
  set.seed(7)
  examples <- result$locations[base::sample(base::nrow(result$locations), 3), ]
  for (i in 1:3) print(base::as.list(examples[i, ]))
  readr::write_csv(result$locations, base::file.path(smoke_dir, "smoke_locations.csv"))
  readr::write_csv(result$snowball, base::file.path(smoke_dir, "smoke_snowball.csv"))

  footer_domains <- base::c("arcticslope.org", "mysouthwell.com", "butlerhealthsystem.org", "hugohospital.com", "donalsonvillehospital.org")
  request_log$urls <- base::character(0)
  started <- base::Sys.time()
  footer <- crawl_footer(footer_domains, state_path = hpt_path("smoke_footer_state.csv"))
  elapsed <- base::as.numeric(base::difftime(base::Sys.time(), started, units = "secs"))
  base::message("Footer: ", base::length(request_log$urls), " requests, ", base::round(elapsed, 1), " s")
  print(footer |> dplyr::select("domain", "status", "homepage_url", "price_page_url", "link_text", "in_footer", "mrf_url"), n = 40, width = 250)
  print(request_log$urls)
  base::message("Total requests this run: ", txt_requests + base::length(request_log$urls))
}
