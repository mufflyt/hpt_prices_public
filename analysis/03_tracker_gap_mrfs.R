#!/usr/bin/env Rscript
#' Part C1: MRFs the cms-hpt-tracker already knows for CCNs the Trilliant
#' lake does not cover. No discovery needed: download, parse, store, and
#' delete each raw file. Skips any URL whose file Trilliant already parsed.
#'
#' Env vars: HPT_MAX_ITEMS (pilot cap), HPT_KEEP_RAW.

base::source("R/00_source_all.R")

codebook <- load_codebook("config/codebook.csv")
reference <- load_reference_inputs()

covered <- covered_ccns()
already <- extracted_url_keys()

candidates <- tracker_mrf_urls(reference$tracker$manifest) |>
  dplyr::filter(!.data$ccn %in% covered) |>
  dplyr::mutate(url_key = normalize_url_key(.data$mrf_url)) |>
  dplyr::filter(!.data$url_key %in% already) |>
  dplyr::left_join(
    reference$tracker$manifest |> dplyr::distinct(.data$mrf_url, .keep_all = TRUE) |> dplyr::select("mrf_url", "mrf_bytes"),
    by = "mrf_url"
  ) |>
  # small files first so a pilot run (HPT_MAX_ITEMS) finishes quickly; sizes
  # under 1 KB (or unknown) are usually bot blocks or error pages, so last
  dplyr::mutate(size_rank = dplyr::if_else(dplyr::coalesce(base::as.numeric(.data$mrf_bytes), 0) < 1024, Inf, base::as.numeric(.data$mrf_bytes))) |>
  dplyr::arrange(.data$size_rank)

base::message(
  "C1: ", scales::comma(dplyr::n_distinct(candidates$ccn)), " uncovered CCNs have ",
  scales::comma(dplyr::n_distinct(candidates$mrf_url)), " tracker MRF URLs not yet extracted."
)

extract_gap_mrfs(base::unique(candidates$mrf_url), codebook, stage = "tracker_c1") |>
  dplyr::count(.data$status) |>
  base::print()
