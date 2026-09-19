#!/usr/bin/env Rscript
#' Midwife supply and the NTSV cesarean rate, by county of residence, with
#' the implied facility price differential (docs/childbirth_analytic_spec.md)
#'
#' Inputs:
#' - CDC WONDER natality exports in reference/cdc_wonder/ (file names, query,
#'   and roles in wonder_ntsv_exports()). The script stops and prints the
#'   exact query for any required export that is missing.
#' - The AMCB roster, birth centers, and county births and OB/GYN counts from
#'   the midwifery repository (R/midwifery_link.R).
#' - 2020 Census county centers of population.
#' - analysis/16 outputs: birth_cesarean_premium.csv (hospital DRG 788 minus
#'   DRG 807 price, by payer).
#'
#' Models (R/ntsv_county.R):
#' - primary: births-weighted linear model of the county NTSV cesarean rate
#'   (percentage points) on log2(midwives per 1,000 births within 30 miles
#'   + 0.5), prespecified covariates, state fixed effects; wild cluster
#'   restricted bootstrap by state;
#' - unadjusted; radii 15 and 60 miles; extended covariates (gestational
#'   hypertension and diabetes, when exported); nearby birth center;
#'   quasi-binomial logit with CR1 errors; state-of-residence model;
#'   placebo outcome (2016-2019 NTSV rate); negative-control outcome
#'   (multiple-birth share).
#' The implied facility price differential is arithmetic on the primary
#' slope: never savings or value.
#'
#' Writes to HPT_DATA_DIR/output/ (never committed): ntsv_county_analysis.csv,
#' ntsv_models.csv, ntsv_implied_facility_price_differential.csv,
#' ntsv_wonder_provenance.csv, and figures/birth5_ntsv_county_map and
#' figures/birth6_ntsv_supply_vs_rate.

base::source("R/00_source_all.R")

radius <- base::as.numeric(base::Sys.getenv("HPT_MIDWIFE_RADIUS_MILES", unset = "30"))
sensitivity_radii <- base::c(15, 60)
B <- base::as.integer(base::Sys.getenv("HPT_WCR_DRAWS", unset = "9999"))
out_dir <- hpt_path("output")
fig_dir <- hpt_path("output", "figures")
base::dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
db_path <- hpt_database_path()

# ---- WONDER exports ---------------------------------------------------------------

exports <- wonder_ntsv_exports() |>
  dplyr::mutate(path = base::file.path(wonder_export_dir(), .data$file), present = base::file.exists(.data$path))
missing_required <- dplyr::filter(exports, .data$role == "required", !.data$present)
if (base::nrow(missing_required)) {
  base::message("Missing CDC WONDER exports (Natality, 2016-2024 expanded; tab-delimited; show totals, zero and suppressed values).")
  base::message("Dataset D149 (Natality, 2016-2024 expanded). Export format: XLS, which is tab-delimited text.")
  base::message("NTSV filters, using the request form's own option labels:")
  base::message("  Live Birth Order = 1; Plurality = Single; Fetal Presentation = Cephalic;")
  base::message("  OE Gestational Age Recode 11 = 37-38 weeks, 39 weeks, 40 weeks, 41 weeks, 42 weeks or more.")
  for (i in base::seq_len(base::nrow(missing_required))) {
    r <- missing_required[i, ]
    base::message(base::sprintf("- %s\n    Group by: %s | Years: %s | NTSV filters: %s", r$path, r$group_by, r$years,
                                if (r$ntsv_filters) "yes" else "no"))
  }
  base::stop(base::nrow(missing_required), " required WONDER export(s) missing; see docs/childbirth_analytic_spec.md")
}
present <- dplyr::filter(exports, .data$present)
skipped <- dplyr::filter(exports, !.data$present)
if (base::nrow(skipped)) base::message("Optional WONDER exports not found (skipped): ", base::paste(skipped$file, collapse = ", "))

#' The Notes block of an export, for provenance and filter checks
wonder_notes <- function(path) {
  lines <- base::readLines(path, warn = FALSE)
  at <- base::which(stringr::str_detect(lines, '^"?---'))[1]
  if (base::is.na(at)) return(base::character())
  stringr::str_remove_all(lines[at:base::length(lines)], '"')
}
#' What the Notes block of a correctly filtered export actually says
#'
#' These are the strings CDC WONDER writes, verified against a live export of
#' the county outcome on 2026-09-19, not the option labels on the request
#' form. The two differ: the form offers Live Birth Order "1", and the Notes
#' record it as `Live Birth Order: 1`, where this check previously looked for
#' "1st child born alive to mother" and so would have rejected every correct
#' export. The gestational-age line lists all five term categories, and all
#' five are required: matching only "37-38 weeks" would pass an export
#' restricted to early-term births.
filter_marks <- base::c(
  "^Live Birth Order: 1$",
  "^Plurality: Single$",
  "^Fetal Presentation: Cephalic$",
  "^OE Gestational Age Recode 11:(?=.*37-38 weeks)(?=.*39 weeks)(?=.*40 weeks)(?=.*41 weeks)(?=.*42 weeks or more)"
)
provenance <- dplyr::bind_rows(base::lapply(base::seq_len(base::nrow(present)), function(i) {
  r <- present[i, ]
  notes <- wonder_notes(r$path)
  has <- base::vapply(filter_marks, function(m) base::any(stringr::str_detect(notes, m)), base::logical(1))
  tibble::tibble(key = r$key, file = r$file, sha256 = sha256_file(r$path),
                 dataset = stringr::str_squish(stringr::str_remove(base::grep("^Dataset:", notes, value = TRUE)[1], "^Dataset:")),
                 query_date = stringr::str_squish(stringr::str_remove(base::grep("^Query Date:", notes, value = TRUE)[1], "^Query Date:")),
                 ntsv_filters_expected = r$ntsv_filters, ntsv_filters_in_notes = base::all(has))
}))
write_csv_atomic(provenance, base::file.path(out_dir, "ntsv_wonder_provenance.csv"))
wrong <- dplyr::filter(provenance, .data$ntsv_filters_expected != .data$ntsv_filters_in_notes)
if (base::nrow(wrong) && !base::identical(base::Sys.getenv("HPT_WONDER_SKIP_FILTER_CHECK"), "true")) {
  base::stop("NTSV filters in the export Notes do not match the specification for: ", base::paste(wrong$file, collapse = ", "),
             ". Re-export, or set HPT_WONDER_SKIP_FILTER_CHECK=true after checking the Notes by hand.")
}
ex <- stats::setNames(base::lapply(present$path, read_wonder_export), present$key)

# ---- outcome and composition ---------------------------------------------------------

county_state <- function(geo_name) stringr::str_match(geo_name, ",\\s*([A-Z]{2})$")[, 2]
outcome <- ntsv_cesarean_rates(ex$county) |>
  dplyr::filter(!.data$pooled) |>
  dplyr::transmute(county_fips = .data$geo_code, county_name = .data$geo_name, state = county_state(.data$geo_name),
                   .data$cesarean, .data$ntsv_births, .data$cesarean_rate, .data$rate_low, .data$rate_high)
placebo <- ntsv_cesarean_rates(ex$placebo) |>
  dplyr::filter(!.data$pooled) |>
  dplyr::transmute(county_fips = .data$geo_code, cesarean_rate_2016_2019 = .data$cesarean_rate, ntsv_births_2016_2019 = .data$ntsv_births)
composition <- ntsv_composition(ex[base::intersect(base::names(ex), base::c("age", "payment", "race", "bmi", "hypertension", "diabetes"))]) |>
  dplyr::rename(county_fips = "geo_code")
negative_control <- if ("plurality" %in% base::names(ex)) {
  wonder_shares(ex$plurality, "^Plurality$", matches("twin|triplet")) |>
    dplyr::filter(!.data$pooled) |>
    dplyr::transmute(county_fips = .data$geo_code, multiple_share = .data$share, births_all = .data$denominator)
} else {
  tibble::tibble(county_fips = base::character(), multiple_share = base::numeric(), births_all = base::numeric())
}

# ---- exposure ------------------------------------------------------------------------

download_county_population_centers()
download_zcta_files()
centers <- load_county_population_centers()
county_inputs <- load_county_midwifery()
zcta <- load_zcta_centroids()
zcta_county <- load_zcta_county()
located <- function(tbl) dplyr::inner_join(tbl, zcta, by = "zip")

# births and OB/GYNs sit at each county's center of population; Connecticut's
# planning regions (AHRF) keep the midwifery repository's coordinates
birth_counties <- county_inputs |>
  dplyr::left_join(dplyr::select(centers, "county_fips", c_lat = "lat", c_lon = "lon"), by = "county_fips") |>
  dplyr::mutate(lat = dplyr::coalesce(.data$c_lat, .data$lat), lon = dplyr::coalesce(.data$c_lon, .data$lon)) |>
  dplyr::select("county_fips", "lat", "lon", "births", "obgyn")
roster <- load_midwife_roster()
national_roster_coverage(roster, strict = TRUE)
midwives <- located(roster)
uncovered <- roster_uncovered_zctas(zcta, zcta_county, roster)
base::message("Midwife roster: ", base::nrow(roster), " active midwives across ",
              base::length(base::unique(roster$state)), " states and territories, all 50 and DC covered; ",
              base::nrow(uncovered), " ZCTAs in uncovered states (expected 0)")
birth_centers <- located(dplyr::filter(load_birth_centers(), !base::is.na(.data$zip)))
hospitals <- duckdb_query("SELECT ccn, zip_code AS zip, state FROM dim_hospital", database = db_path, read_only = TRUE) |>
  dplyr::mutate(zip = stringr::str_sub(stringr::str_pad(.data$zip, 5, pad = "0"), 1, 5))
ld_hospitals <- load_ld_hospitals() |>
  dplyr::filter(.data$provides_ld %in% TRUE) |>
  dplyr::inner_join(hospitals, by = "ccn") |>
  located()

points <- centers |>
  dplyr::filter(.data$county_fips %in% outcome$county_fips) |>
  dplyr::transmute(id = .data$county_fips, .data$lat, .data$lon, .data$county_fips)
supply <- dplyr::bind_rows(base::lapply(base::c(radius, sensitivity_radii), function(r) {
  midwifery_supply_at(points, midwives, birth_counties, radius = r, birth_centers = birth_centers, ld_hospitals = ld_hospitals,
                      uncovered = uncovered) |>
    dplyr::mutate(radius = r)
}))
base::message("Supply computed for ", base::nrow(points), " WONDER-identified counties at radii ",
              base::paste(base::c(radius, sensitivity_radii), collapse = ", "), " miles")

county_covariates <- county_inputs |>
  dplyr::transmute(.data$county_fips, .data$rucc_2023, log_income = base::log(.data$median_hh_income), .data$pct_uninsured)

analysis_at <- function(r) {
  supply |>
    dplyr::filter(.data$radius == r) |>
    dplyr::rename(county_fips = "id") |>
    dplyr::inner_join(outcome, by = "county_fips") |>
    dplyr::left_join(composition, by = "county_fips") |>
    dplyr::left_join(placebo, by = "county_fips") |>
    dplyr::left_join(negative_control, by = "county_fips") |>
    dplyr::left_join(county_covariates, by = "county_fips") |>
    dplyr::mutate(
      log2_cnm = base::log2(.data$cnm_per_1k_births + 0.5),
      log2_obgyn = base::log2(.data$obgyn_per_1k_births + 0.5),
      birth_center = base::as.integer(.data$bc_within > 0),
      rucc_group = base::factor(dplyr::case_when(.data$rucc_2023 == 1 ~ "metro 1M+", .data$rucc_2023 %in% 2:3 ~ "metro under 1M",
                                                 !base::is.na(.data$rucc_2023) ~ "nonmetro"),
                                levels = base::c("metro 1M+", "metro under 1M", "nonmetro"))
    )
}
analysis <- analysis_at(radius)
write_csv_atomic(dplyr::bind_rows(base::lapply(base::c(radius, sensitivity_radii), analysis_at)),
                 base::file.path(out_dir, "ntsv_county_analysis.csv"))

# ---- models --------------------------------------------------------------------------

covariates <- base::c("share_age_35plus", "share_age_under20", "share_medicaid", "share_black_nh", "share_hispanic",
                      "share_asian_nh", "share_obese", "log_income", "pct_uninsured", "log2_obgyn", "ld_per_1k_births", "rucc_group")
extended <- base::intersect(base::c("share_gest_hypertension", "share_gest_diabetes"), base::names(analysis))
rhs <- function(exposure, extra = base::character()) base::paste(base::c(exposure, covariates, extra), collapse = " + ")
run <- function(label, data, formula_rhs, term = "log2_cnm", ...) {
  fit_ntsv_linear(data, formula_rhs, term, B = B, ...) |> dplyr::mutate(model = label, .before = 1)
}

models <- dplyr::bind_rows(
  run("unadjusted (state fixed effects)", analysis, "log2_cnm"),
  run("primary", analysis, rhs("log2_cnm")),
  dplyr::bind_rows(base::lapply(sensitivity_radii, function(r) run(base::sprintf("radius %s miles", r), analysis_at(r), rhs("log2_cnm")))),
  if (base::length(extended)) run("extended covariates", analysis, rhs("log2_cnm", extended)),
  run("birth center within radius", analysis, rhs("log2_cnm", "birth_center"), term = "birth_center"),
  run("placebo outcome: 2016-2019 NTSV rate", analysis, rhs("log2_cnm"), outcome = "cesarean_rate_2016_2019", weight = "ntsv_births_2016_2019"),
  if (base::nrow(negative_control)) run("negative control: multiple-birth share", analysis, rhs("log2_cnm"), outcome = "multiple_share", weight = "births_all")
)
quasi <- fit_ntsv_quasibinomial(analysis, rhs("log2_cnm"), "log2_cnm") |>
  dplyr::transmute(model = "quasi-binomial (CR1, state)", .data$term, estimate_pp = .data$ame_pp, log_odds = .data$log_odds,
                   log_odds_ci_low = .data$ci_low, log_odds_ci_high = .data$ci_high, .data$p_value, .data$n_units, .data$n_states)

# state of residence: complete coverage, no fixed effects, each state its own cluster.
# The roster filter below was what kept the eleven uncovered states out of this
# model; with the national freeze it drops nothing and is kept only as a guard.
state_rates <- ntsv_cesarean_rates(ex$state)
state_supply <- midwives |>
  dplyr::left_join(zcta_county, by = "zip") |>
  dplyr::count(state_fips = stringr::str_sub(.data$county_fips, 1, 2), name = "cnm") |>
  dplyr::inner_join(county_inputs |> dplyr::group_by(state_fips = stringr::str_sub(.data$county_fips, 1, 2)) |>
                      dplyr::summarise(births = base::sum(.data$births, na.rm = TRUE), .groups = "drop"), by = "state_fips")
state_analysis <- state_rates |>
  dplyr::filter(base::unname(state_fips_postal()[.data$geo_code]) %in% roster$state) |>
  dplyr::inner_join(state_supply, by = base::c(geo_code = "state_fips")) |>
  dplyr::mutate(log2_cnm = base::log2(1000 * .data$cnm / .data$births + 0.5), state = .data$geo_code)
models <- dplyr::bind_rows(models, quasi,
                           run("state of residence (unadjusted)", state_analysis, "log2_cnm", fixed_effects = NULL))
write_csv_atomic(models, base::file.path(out_dir, "ntsv_models.csv"))
base::print(dplyr::select(models, "model", "term", "estimate_pp", "ci_low_pp", "ci_high_pp", "p_value", "n_units", "n_states"), n = 40)

# ---- implied facility price differential ----------------------------------------------

primary <- dplyr::filter(models, .data$model == "primary")
contrast <- base::diff(stats::quantile(analysis$log2_cnm, base::c(0.25, 0.75), na.rm = TRUE, names = FALSE))
premium <- readr::read_csv(base::file.path(out_dir, "birth_cesarean_premium.csv"), col_types = readr::cols(ccn = "c"), show_col_types = FALSE) |>
  dplyr::filter(.data$payer_type %in% base::c("commercial", "medicaid")) |>
  dplyr::inner_join(dplyr::select(hospitals, "ccn", "zip"), by = "ccn") |>
  located()
county_points <- analysis |> dplyr::transmute(id = .data$county_fips, .data$state) |>
  dplyr::inner_join(dplyr::select(points, "id", "lat", "lon"), by = "id")
differentials <- local_price_differential(county_points, premium, radius = radius)
implied <- implied_facility_price_differential(
  analysis |> dplyr::filter(.data$roster_covered, !base::is.na(.data$share_private), !base::is.na(.data$share_medicaid)) |>
    dplyr::transmute(id = .data$county_fips, ntsv_births_per_year = .data$ntsv_births / 3, .data$share_private, .data$share_medicaid),
  differentials,
  slope = base::c(estimate = primary$estimate_pp, ci_low = primary$ci_low_pp, ci_high = primary$ci_high_pp),
  contrast = contrast
) |>
  dplyr::mutate(contrast_units = contrast, contrast = "25th to 75th percentile of log2 midwives per 1,000 births",
                share_local_differential = base::mean(differentials$source == "local"))
write_csv_atomic(implied, base::file.path(out_dir, "ntsv_implied_facility_price_differential.csv"))

# ---- figures ----------------------------------------------------------------------

save_figure(
  ntsv_county_map(
    dplyr::select(outcome, "county_fips", "cesarean_rate"),
    subtitle = base::sprintf("%s counties CDC WONDER reports, %s-%s.", base::format(base::nrow(outcome), big.mark = ","),
                             2022, 2024)
  ),
  "birth5_ntsv_county_map", width = 10, height = 6.5
)
save_figure(
  ntsv_supply_rate_plot(dplyr::filter(analysis, .data$roster_covered), slope_pp = primary$estimate_pp),
  "birth6_ntsv_supply_vs_rate", width = 9, height = 6
)
base::message("Figures: ", fig_dir)
base::print(implied)
base::message("Implied facility price differential: arithmetic on the primary slope, annual NTSV births in WONDER-identified counties ",
              "the midwife roster covers; not savings or value.")
