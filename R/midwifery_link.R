#' Midwifery presence around each delivery hospital
#'
#' Midwives are placed at the ZIP code of their NPPES practice address and
#' hospitals at their CMS roster ZIP, both at the Census 2023 ZCTA internal
#' point. For each hospital:
#' - `cnm_within`: active AMCB-certified midwives (CNMs and CMs) within
#'   `radius` miles (straight line);
#' - `births_within`: annual births (NVSS) in counties whose centroid lies
#'   within `radius` miles, always including the hospital's own county;
#' - `cnm_per_1k_births`: cnm_within / births_within x 1,000, the headline
#'   exposure (a supply measure scaled to local demand);
#' - `bc_within` and `nearest_bc_miles`: CABC-accredited birth centers;
#' - the county's CNM-attended share of births (CDC WONDER natality
#'   2016-2024, published only for counties of 100,000+ residents) and its
#'   rural-urban continuum code.
#'
#' Inputs come read-only from the midwifery repository (MIDWIFERY_DIR,
#' default ~/midwifery): artifacts/tracked_roster_active_primary_linked.csv
#' (the NPI-linked active roster; the older scraped roster was withdrawn),
#' artifacts/cabc_accredited_birth_centers_master.csv, and
#' artifacts/county_profiles/county_cnm_births.csv.

midwifery_dir <- function() {
  base::Sys.getenv("MIDWIFERY_DIR", unset = base::path.expand("~/midwifery"))
}

zcta_source <- function() {
  base::c(
    gazetteer = "https://www2.census.gov/geo/docs/maps-data/data/gazetteer/2023_Gazetteer/2023_Gaz_zcta_national.zip",
    zcta_county = "https://www2.census.gov/geo/docs/maps-data/data/rel2020/zcta520/tab20_zcta520_county20_natl.txt"
  )
}

#' Download the Census ZCTA gazetteer and ZCTA-county relationship file
download_zcta_files <- function(dest_dir = hpt_path("reference", "census_zcta")) {
  urls <- zcta_source()
  paths <- base::file.path(dest_dir, base::basename(urls))
  for (i in base::seq_along(urls)) download_public_file(urls[[i]], paths[[i]])
  utils::unzip(paths[[1]], exdir = dest_dir)
  provenance_path <- base::file.path(dest_dir, "census_zcta_provenance.csv")
  if (!base::file.exists(provenance_path)) {
    write_csv_atomic(tibble::tibble(url = base::unname(urls), sha256 = base::vapply(paths, sha256_file, base::character(1), USE.NAMES = FALSE),
                                    downloaded_at = utc_timestamp()), provenance_path)
  }
  dest_dir
}

#' ZCTA internal points (Census 2023 gazetteer)
load_zcta_centroids <- function(path = hpt_path("reference", "census_zcta", "2023_Gaz_zcta_national.txt")) {
  tbl <- readr::read_tsv(path, col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE)
  base::names(tbl) <- stringr::str_trim(base::names(tbl))
  tibble::tibble(zip = stringr::str_trim(tbl$GEOID), lat = base::as.numeric(tbl$INTPTLAT), lon = base::as.numeric(stringr::str_trim(tbl$INTPTLONG)))
}

#' County of each ZCTA: the county holding the largest share of its land
load_zcta_county <- function(path = hpt_path("reference", "census_zcta", "tab20_zcta520_county20_natl.txt")) {
  tbl <- readr::read_delim(path, delim = "|", col_types = readr::cols(.default = readr::col_character()),
                           show_col_types = FALSE, locale = readr::locale(encoding = "UTF-8"))
  base::names(tbl) <- stringr::str_remove(base::names(tbl), "^﻿")
  tbl |>
    dplyr::filter(!base::is.na(.data$GEOID_ZCTA5_20), base::nzchar(.data$GEOID_ZCTA5_20)) |>
    dplyr::transmute(zip = .data$GEOID_ZCTA5_20, county_fips = .data$GEOID_COUNTY_20,
                     land = base::as.numeric(.data$AREALAND_PART)) |>
    dplyr::arrange(.data$zip, dplyr::desc(.data$land)) |>
    dplyr::distinct(.data$zip, .keep_all = TRUE) |>
    dplyr::select("zip", "county_fips")
}

#' Active AMCB-certified midwives with their NPPES practice ZIP
load_midwife_roster <- function(path = base::file.path(midwifery_dir(), "artifacts", "tracked_roster_active_primary_linked.csv")) {
  readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE) |>
    dplyr::filter(.data$status == "ACTIVE", !base::is.na(.data$npi)) |>
    dplyr::transmute(.data$npi, zip = stringr::str_sub(.data$nppes_zip, 1, 5), state = .data$nppes_state, .data$certification) |>
    dplyr::distinct(.data$npi, .keep_all = TRUE)
}

#' CABC-accredited birth centers, with the ZIP read from the end of the full
#' address (the file's zip_code column sometimes holds a street number)
load_birth_centers <- function(path = base::file.path(midwifery_dir(), "artifacts", "cabc_accredited_birth_centers_master.csv")) {
  readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE) |>
    dplyr::mutate(
      zip_from_address = stringr::str_match(.data$full_address, "\\b([0-9]{5})(-[0-9]{4})?\\s*$")[, 2],
      zip = dplyr::coalesce(.data$zip_from_address, dplyr::if_else(stringr::str_detect(.data$zip_code, "^[0-9]{5}$"), .data$zip_code, NA_character_))
    ) |>
    dplyr::select("bc_id", "facility_name", "state", "zip")
}

#' County births, midwife and OB/GYN counts, WONDER CNM share, rurality,
#' income, and uninsurance
#'
#' Births are NVSS natality counts (AHRF, the midwifery repository's
#' `births_used`), the same system the cesarean rates come from; the ACS
#' estimate fills the few counties without one.
#' OB/GYNs are the AHRF county count (`ahrf_obgyn`).
load_county_midwifery <- function(path = base::file.path(midwifery_dir(), "artifacts", "county_profiles", "county_cnm_births.csv"),
                                  supply_path = base::file.path(midwifery_dir(), "artifacts", "county_midwifery_supply.csv")) {
  supply <- readr::read_csv(supply_path, col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE) |>
    dplyr::transmute(county_fips = .data$fips, births_nvss = base::as.numeric(.data$births_used),
                     obgyn = base::as.numeric(.data$ahrf_obgyn))
  readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE) |>
    dplyr::left_join(supply, by = base::c(GEOID = "county_fips")) |>
    dplyr::transmute(
      county_fips = .data$GEOID, lat = base::as.numeric(.data$lat), lon = base::as.numeric(.data$lon),
      births = dplyr::coalesce(.data$births_nvss, base::as.numeric(.data$births_past_12mo)), n_cnm_cm = base::as.numeric(.data$n_cnm_cm),
      cnm_share_of_births_pct = base::as.numeric(.data$cnm_share_of_births_pct),
      wonder_county_reported = .data$wonder_county_reported == "TRUE",
      rucc_2023 = base::as.integer(.data$rucc_2023),
      obgyn = .data$obgyn, median_hh_income = base::as.numeric(.data$median_hh_income),
      pct_uninsured = base::as.numeric(.data$pct_uninsured)
    )
}

#' Great-circle distance in miles (vectorized over the second point)
haversine_miles <- function(lat1, lon1, lat2, lon2) {
  to_rad <- base::pi / 180
  dlat <- (lat2 - lat1) * to_rad
  dlon <- (lon2 - lon1) * to_rad
  a <- base::sin(dlat / 2)^2 + base::cos(lat1 * to_rad) * base::cos(lat2 * to_rad) * base::sin(dlon / 2)^2
  3958.8 * 2 * base::asin(base::pmin(1, base::sqrt(a)))
}

#' Midwifery presence per hospital
#'
#' @param hospitals tibble(ccn, zip).
#' @param uncovered roster_uncovered_zctas() rows, or NULL to skip the
#'   roster-coverage check.
hospital_midwifery_presence <- function(hospitals, zcta, zcta_county, midwives, birth_centers, counties, radius = 30,
                                        uncovered = NULL) {
  located <- function(tbl) dplyr::inner_join(tbl, zcta, by = "zip")
  hosp <- hospitals |>
    dplyr::mutate(zip = stringr::str_sub(stringr::str_pad(.data$zip, 5, pad = "0"), 1, 5)) |>
    located() |>
    dplyr::left_join(zcta_county, by = "zip") |>
    dplyr::left_join(dplyr::select(counties, "county_fips", "cnm_share_of_births_pct", "wonder_county_reported", "rucc_2023"), by = "county_fips")
  supply <- midwifery_supply_at(
    dplyr::transmute(hosp, id = .data$ccn, .data$lat, .data$lon, .data$county_fips),
    located(midwives), counties, radius = radius,
    birth_centers = located(dplyr::filter(birth_centers, !base::is.na(.data$zip))), uncovered = uncovered
  )
  supply |>
    dplyr::transmute(ccn = .data$id, .data$roster_covered, .data$cnm_within, .data$births_within, .data$bc_within, .data$nearest_bc_miles,
                     .data$cnm_per_1k_births) |>
    dplyr::left_join(dplyr::select(hosp, "ccn", "zip", "lat", "lon", "county_fips", "cnm_share_of_births_pct",
                                   "wonder_county_reported", "rucc_2023"), by = "ccn")
}
