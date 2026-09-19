#' County NTSV cesarean rates, midwife supply, and the implied facility price
#' differential (docs/childbirth_analytic_spec.md)
#'
#' Unit: county of mother's residence (counties CDC WONDER identifies, which
#' are those of 100,000+ residents), with a state-of-residence sensitivity.
#' Outcome: the NTSV (low-risk) cesarean rate from manual CDC WONDER exports
#' listed in wonder_ntsv_exports(); the WONDER API refuses sub-national
#' natality and the NCHS public-use file has no geography, so the exports are
#' archived by hand under reference/cdc_wonder/. Exposure: AMCB-certified
#' midwives within `radius` miles of the county's 2020 Census center of
#' population per 1,000 resident births within the same radius.
#'
#' WONDER suppresses cells of 1-9 births. Suppression is never read as zero:
#' each rate or share is computed with suppressed cells at 5 (the midpoint)
#' and carries the range it could take with them anywhere from 1 to 9, and
#' a value whose range exceeds `max_suppression_range` is set to NA.
#'
#' This file holds code only. It contains no CDC-derived numbers.

# ---- the WONDER exports ----------------------------------------------------------

#' Every WONDER export the analysis reads
#'
#' All come from "Natality, 2016-2024 expanded" (dataset D149, web UI),
#' exported with totals, zero values, and suppressed values shown. On the
#' results page choose Export, then the XLS format, which despite its name is
#' the tab-delimited text this reader expects; TSV and CSV omit the Notes
#' block the filter check reads.
#'
#' "NTSV filters" means Live Birth Order = "1", Plurality = "Single", OE
#' Gestational Age Recode 11 = "37-38 weeks", "39 weeks", "40 weeks",
#' "41 weeks", "42 weeks or more", and Fetal Presentation = "Cephalic".
#' These are the request form's own option labels, verified against a live
#' export on 2026-09-19. The natality help page's wording differs from them,
#' and an earlier version of this file took the labels from there.
#' `role`: "required" exports must exist for the primary model; the others
#' feed extended models and checks and are skipped with a message when absent.
wonder_ntsv_exports <- function() {
  tibble::tribble(
    ~key,               ~file,                                  ~group_by,                                              ~years,      ~ntsv_filters, ~role,         ~purpose,
    "county",           "ntsv_county_2022_2024.txt",           "County of Residence; Delivery Method",                 "2022-2024", TRUE,          "required",    "primary outcome",
    "state",            "ntsv_state_2022_2024.txt",            "State of Residence; Delivery Method",                  "2022-2024", TRUE,          "required",    "complete-coverage state sensitivity",
    "placebo",          "ntsv_county_2016_2019.txt",           "County of Residence; Delivery Method",                 "2016-2019", TRUE,          "required",    "placebo (pre-period) outcome",
    "age",              "ntsv_county_age_2022_2024.txt",       "County of Residence; Age of Mother 9",                 "2022-2024", TRUE,          "required",    "maternal age composition",
    "payment",          "ntsv_county_payment_2022_2024.txt",   "County of Residence; Source of Payment for Delivery",  "2022-2024", TRUE,          "required",    "payer mix; payer split of the implied differential",
    "race",             "ntsv_county_race_2022_2024.txt",      "County of Residence; Mother's Hispanic Origin; Mother's Single Race 6", "2022-2024", TRUE, "required", "race and Hispanic origin composition",
    "bmi",              "ntsv_county_bmi_2022_2024.txt",       "County of Residence; Mother's Pre-pregnancy BMI",      "2022-2024", TRUE,          "required",    "pre-pregnancy obesity share",
    "hypertension",     "ntsv_county_hypertension_2022_2024.txt", "County of Residence; Gestational Hypertension",      "2022-2024", TRUE,          "extended",    "gestational hypertension share",
    "diabetes",         "ntsv_county_diabetes_2022_2024.txt",  "County of Residence; Gestational Diabetes",            "2022-2024", TRUE,          "extended",    "gestational diabetes share",
    "plurality",        "births_county_plurality_2022_2024.txt", "County of Residence; Plurality",                     "2022-2024", FALSE,         "check",       "negative-control outcome (multiple-birth share, all births)"
  )
}

wonder_export_dir <- function() hpt_path("reference", "cdc_wonder")

#' Read a CDC WONDER tab-delimited export
#'
#' Drops the trailing Notes block and the "Total" rows, keeps every value as
#' text (so "Suppressed" survives), and squishes column names.
read_wonder_export <- function(path) {
  lines <- base::readLines(path, warn = FALSE)
  notes_at <- base::which(stringr::str_detect(lines, '^"?---'))[1]
  if (!base::is.na(notes_at)) lines <- lines[base::seq_len(notes_at - 1L)]
  lines <- lines[base::nzchar(stringr::str_trim(lines))]
  tbl <- readr::read_tsv(base::I(base::paste(lines, collapse = "\n")), col_types = readr::cols(.default = readr::col_character()),
                         na = base::character(), show_col_types = FALSE)
  base::names(tbl) <- stringr::str_squish(base::names(tbl))
  if ("Notes" %in% base::names(tbl)) {
    tbl <- dplyr::filter(tbl, .data$Notes != "Total") |> dplyr::select(-"Notes")
  }
  if (!"Births" %in% base::names(tbl)) base::stop("WONDER export lacks a Births column: ", path)
  tbl
}

#' The residence geography of a WONDER export: county or state
wonder_geography <- function(tbl) {
  county <- base::grep("^County( of Residence)? Code$", base::names(tbl), value = TRUE)[1]
  state <- base::grep("^State( of Residence)? Code$", base::names(tbl), value = TRUE)[1]
  if (!base::is.na(county)) {
    return(base::list(level = "county", code = county, name = base::sub(" Code$", "", county), width = 5L))
  }
  if (!base::is.na(state)) {
    return(base::list(level = "state", code = state, name = base::sub(" Code$", "", state), width = 2L))
  }
  base::stop("WONDER export has no county or state of residence code column")
}

#' Births as numbers; suppressed and unavailable cells become NA
wonder_births <- function(x) base::suppressWarnings(base::as.numeric(stringr::str_remove_all(x, ",")))

#' A share whose cells may be suppressed
#'
#' @param count births per cell (NA where suppressed).
#' @param suppressed logical, the cell was suppressed (1-9 births).
#' @param in_num,in_den logical; numerator cells must be a subset of the
#'   denominator cells.
#' @return tibble(numerator, denominator, share, share_low, share_high,
#'   n_suppressed): share with suppressed cells at 5; low and high put
#'   suppressed numerator cells at 1 and 9 and the others at 9 and 1.
bounded_share <- function(count, suppressed, in_num, in_den) {
  fill <- function(num_fill, other_fill) {
    base::ifelse(suppressed, base::ifelse(in_num, num_fill, other_fill), count)
  }
  ratio <- function(x) base::sum(x[in_num]) / base::sum(x[in_den])
  mid <- fill(5, 5)
  tibble::tibble(
    numerator = base::sum(mid[in_num]), denominator = base::sum(mid[in_den]),
    share = ratio(mid), share_low = ratio(fill(1, 9)), share_high = ratio(fill(9, 1)),
    n_suppressed = base::sum(suppressed & in_den)
  )
}

#' The first non-code column whose name matches `pattern`
wonder_column <- function(tbl, pattern) {
  hit <- base::grep(pattern, base::names(tbl), value = TRUE, ignore.case = TRUE)
  hit <- hit[!stringr::str_detect(hit, " Code$")]
  if (!base::length(hit)) base::stop("WONDER export has no column matching: ", pattern)
  hit[[1]]
}

#' Cells of a WONDER export in long form: geography code, category, births
#'
#' @param category regexes naming the category columns; with two or more,
#'   the category is their values joined by " | " in the given order.
wonder_cells <- function(tbl, category) {
  geo <- wonder_geography(tbl)
  cols <- base::vapply(category, function(p) wonder_column(tbl, p), base::character(1))
  tbl |>
    dplyr::transmute(
      geo_code = stringr::str_pad(.data[[geo$code]], geo$width, pad = "0"),
      geo_name = .data[[geo$name]],
      category = base::do.call(base::paste, base::c(base::unname(base::as.list(tbl[cols])), sep = " | ")),
      births = wonder_births(.data$Births),
      suppressed = stringr::str_detect(.data$Births, stringr::regex("^suppressed$", ignore_case = TRUE))
    ) |>
    dplyr::filter(base::nzchar(.data$geo_code), base::nzchar(stringr::str_remove_all(.data$category, "[ |]"))) |>
    dplyr::mutate(pooled = geo$level == "county" & stringr::str_detect(.data$geo_code, "999$"),
                  level = geo$level)
}

#' Share of known-category births meeting a condition, per geography
#'
#' @param match function(category) -> logical: the numerator categories.
#' @param unknown regex for categories left out of the denominator.
wonder_shares <- function(tbl, category, match, unknown = "unknown|not stated|not reported|not available",
                          max_suppression_range = 0.01) {
  cells <- wonder_cells(tbl, category) |>
    dplyr::mutate(known = !stringr::str_detect(.data$category, stringr::regex(unknown, ignore_case = TRUE)),
                  hit = .data$known & match(.data$category))
  bad <- cells |> dplyr::filter(!.data$suppressed, base::is.na(.data$births))
  if (base::nrow(bad)) base::stop("Unreadable WONDER birth counts: ", base::paste(base::unique(bad$geo_code), collapse = ", "))
  cells |>
    dplyr::group_by(.data$level, .data$geo_code, .data$geo_name, .data$pooled) |>
    dplyr::reframe(bounded_share(.data$births, .data$suppressed, .data$hit, .data$known)) |>
    dplyr::mutate(share = dplyr::if_else(.data$share_high - .data$share_low > max_suppression_range, NA_real_, .data$share))
}

#' NTSV cesarean rate per geography from a Delivery Method export
#'
#' Rate = cesarean / (cesarean + vaginal); unknown delivery method is left
#' out of both. Pooled "Unidentified Counties" rows are kept and flagged.
ntsv_cesarean_rates <- function(tbl, max_suppression_range = 0.01) {
  wonder_shares(tbl, "^Delivery Method$", match = function(x) x == "Cesarean",
                unknown = "^(?!Vaginal$|Cesarean$)", max_suppression_range = max_suppression_range) |>
    dplyr::rename(cesarean = "numerator", ntsv_births = "denominator", cesarean_rate = "share",
                  rate_low = "share_low", rate_high = "share_high")
}

#' Category matchers for the composition exports
age_at_least <- function(years) {
  function(x) {
    low <- base::suppressWarnings(base::as.numeric(stringr::str_extract(x, "[0-9]+")))
    low <- dplyr::if_else(stringr::str_detect(x, stringr::regex("^under", ignore_case = TRUE)), 0, low)
    !base::is.na(low) & low >= years
  }
}
matches <- function(pattern, exclude = NULL) {
  function(x) {
    hit <- stringr::str_detect(x, stringr::regex(pattern, ignore_case = TRUE))
    if (!base::is.null(exclude)) hit <- hit & !stringr::str_detect(x, stringr::regex(exclude, ignore_case = TRUE))
    hit
  }
}

#' County composition covariates from whichever composition exports exist
#'
#' @param exports named list of read_wonder_export() tables keyed as in
#'   wonder_ntsv_exports() (age, payment, race, bmi, hypertension, diabetes).
#' @return tibble(geo_code, share_*) for counties, one column per share.
ntsv_composition <- function(exports, max_suppression_range = 0.01) {
  race <- base::c("Hispanic", "Single Race 6")
  specs <- base::list(
    share_age_35plus = base::list(key = "age", category = "^Age of Mother", match = age_at_least(35)),
    share_age_under20 = base::list(key = "age", category = "^Age of Mother", match = function(x) !age_at_least(20)(x)),
    share_medicaid = base::list(key = "payment", category = "Payment", match = matches("^medicaid$")),
    share_private = base::list(key = "payment", category = "Payment", match = matches("^private")),
    share_hispanic = base::list(key = "race", category = race, match = matches("^Hispanic or Latino \\|")),
    share_black_nh = base::list(key = "race", category = race, match = matches("^Not Hispanic or Latino \\| Black")),
    share_asian_nh = base::list(key = "race", category = race, match = matches("^Not Hispanic or Latino \\| Asian")),
    share_obese = base::list(key = "bmi", category = "BMI", match = matches("obes")),
    share_gest_hypertension = base::list(key = "hypertension", category = "Gestational Hypertension", match = matches("^yes$")),
    share_gest_diabetes = base::list(key = "diabetes", category = "Gestational Diabetes", match = matches("^yes$"))
  )
  specs <- specs[base::vapply(specs, function(s) s$key %in% base::names(exports), base::logical(1))]
  if (!base::length(specs)) return(tibble::tibble(geo_code = base::character()))
  parts <- base::lapply(base::names(specs), function(nm) {
    s <- specs[[nm]]
    wonder_shares(exports[[s$key]], s$category, s$match, max_suppression_range = max_suppression_range) |>
      dplyr::filter(!.data$pooled) |>
      dplyr::select("geo_code", !!nm := "share")
  })
  purrr::reduce(parts, dplyr::full_join, by = "geo_code")
}

# ---- exposure ---------------------------------------------------------------------

#' State FIPS code to USPS abbreviation (50 states, DC, Puerto Rico)
state_fips_postal <- function() {
  base::c(
    "01" = "AL", "02" = "AK", "04" = "AZ", "05" = "AR", "06" = "CA", "08" = "CO", "09" = "CT", "10" = "DE", "11" = "DC",
    "12" = "FL", "13" = "GA", "15" = "HI", "16" = "ID", "17" = "IL", "18" = "IN", "19" = "IA", "20" = "KS", "21" = "KY",
    "22" = "LA", "23" = "ME", "24" = "MD", "25" = "MA", "26" = "MI", "27" = "MN", "28" = "MS", "29" = "MO", "30" = "MT",
    "31" = "NE", "32" = "NV", "33" = "NH", "34" = "NJ", "35" = "NM", "36" = "NY", "37" = "NC", "38" = "ND", "39" = "OH",
    "40" = "OK", "41" = "OR", "42" = "PA", "44" = "RI", "45" = "SC", "46" = "SD", "47" = "TN", "48" = "TX", "49" = "UT",
    "50" = "VT", "51" = "VA", "53" = "WA", "54" = "WV", "55" = "WI", "56" = "WY", "72" = "PR"
  )
}

#' ZCTA internal points in states the midwife roster does not cover
#'
#' **This returns no rows on the current input, and that is the point.** The
#' withdrawn 40-state roster made this load-bearing: a catchment reaching a
#' ZCTA in one of the eleven missing states would have counted that state's
#' midwives as zero, so midwifery_supply_at() set the whole catchment's
#' midwife count to NA instead, and 189 of 1,546 hospitals dropped out. The
#' national linkage freeze covers all 50 states and DC, so nothing is masked
#' any more.
#'
#' The check is kept rather than deleted because it is what makes that claim
#' checkable: if a future input covers less ground, this fills up again and
#' the masking comes back on by itself, instead of silently counting real
#' midwives as zero. `national_roster_coverage()` asserts the empty case.
#'
#' @param midwives load_midwife_roster() rows (with `state`).
roster_uncovered_zctas <- function(zcta, zcta_county, midwives) {
  covered <- base::unique(stats::na.omit(midwives$state))
  zcta |>
    dplyr::inner_join(zcta_county, by = "zip") |>
    dplyr::mutate(state = base::unname(state_fips_postal()[stringr::str_sub(.data$county_fips, 1, 2)])) |>
    dplyr::filter(!base::is.na(.data$state), !.data$state %in% covered) |>
    dplyr::select("zip", "state", "lat", "lon")
}

#' States of the 50 and DC with no midwife in the roster
#'
#' Returns the missing postal codes, empty when coverage is national. `strict`
#' turns that into an error, so a partial roster stops a run rather than
#' quietly reinstating the NA masking that this analysis no longer applies.
national_roster_coverage <- function(midwives, strict = FALSE) {
  missing <- base::setdiff(base::c(datasets::state.abb, "DC"), base::unique(stats::na.omit(midwives$state)))
  if (strict && base::length(missing)) {
    base::stop("The midwife roster covers ", 51L - base::length(missing), " of 50 states plus DC; missing ",
               base::paste(missing, collapse = ", "), ".")
  }
  missing
}

county_population_center_source <- function() {
  "https://www2.census.gov/geo/docs/reference/cenpop2020/county/CenPop2020_Mean_CO.txt"
}

download_county_population_centers <- function(dest = hpt_path("reference", "census_cenpop", "CenPop2020_Mean_CO.txt")) {
  download_public_file(county_population_center_source(), dest)
  dest
}

#' 2020 Census county centers of population
load_county_population_centers <- function(path = hpt_path("reference", "census_cenpop", "CenPop2020_Mean_CO.txt")) {
  tbl <- readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE,
                         locale = readr::locale(encoding = "UTF-8"))
  base::names(tbl) <- stringr::str_remove(base::names(tbl), "^﻿")
  tibble::tibble(county_fips = base::paste0(tbl$STATEFP, tbl$COUNTYFP), state_fips = tbl$STATEFP,
                 lat = base::as.numeric(tbl$LATITUDE), lon = base::as.numeric(tbl$LONGITUDE),
                 population = base::as.numeric(tbl$POPULATION))
}

#' Midwives, obstetricians, births, birth centers, and L&D hospitals within
#' `radius` miles of each point
#'
#' @param points tibble(id, lat, lon, county_fips).
#' @param midwives,birth_centers,ld_hospitals tibbles with lat, lon (NULL to skip).
#' @param counties tibble(county_fips, lat, lon, births[, obgyn]): a county's
#'   births and obstetricians count toward a point when its location lies
#'   within the radius or it is the point's own county.
#' @param uncovered tibble(lat, lon) of places the midwife roster does not
#'   cover (roster_uncovered_zctas()); a point with any of them within the
#'   radius gets NA midwife counts and `roster_covered = FALSE`.
midwifery_supply_at <- function(points, midwives, counties, radius = 30, birth_centers = NULL, ld_hospitals = NULL,
                                uncovered = NULL) {
  cty <- dplyr::filter(counties, !base::is.na(.data$lat), !base::is.na(.data$births))
  has_obgyn <- "obgyn" %in% base::names(cty)
  count_within <- function(tbl, lat, lon) {
    if (base::is.null(tbl)) return(NA_real_)
    base::sum(haversine_miles(lat, lon, tbl$lat, tbl$lon) <= radius)
  }
  rows <- base::lapply(base::seq_len(base::nrow(points)), function(i) {
    p <- points[i, ]
    in_radius <- haversine_miles(p$lat, p$lon, cty$lat, cty$lon) <= radius | cty$county_fips %in% p$county_fips
    d_bc <- if (!base::is.null(birth_centers) && base::nrow(birth_centers)) haversine_miles(p$lat, p$lon, birth_centers$lat, birth_centers$lon) else base::numeric()
    covered <- base::is.null(uncovered) || !base::any(haversine_miles(p$lat, p$lon, uncovered$lat, uncovered$lon) <= radius)
    tibble::tibble(
      id = p$id,
      roster_covered = covered,
      cnm_within = if (covered) count_within(midwives, p$lat, p$lon) else NA_real_,
      births_within = base::sum(cty$births[in_radius]),
      obgyn_within = if (has_obgyn) base::sum(cty$obgyn[in_radius], na.rm = TRUE) else NA_real_,
      bc_within = if (base::is.null(birth_centers)) NA_real_ else base::sum(d_bc <= radius),
      nearest_bc_miles = if (base::length(d_bc)) base::min(d_bc) else NA_real_,
      ld_within = count_within(ld_hospitals, p$lat, p$lon)
    )
  })
  per_1k <- function(n, births) dplyr::if_else(births > 0, 1000 * n / births, NA_real_)
  dplyr::bind_rows(rows) |>
    dplyr::mutate(cnm_per_1k_births = per_1k(.data$cnm_within, .data$births_within),
                  obgyn_per_1k_births = per_1k(.data$obgyn_within, .data$births_within),
                  ld_per_1k_births = per_1k(.data$ld_within, .data$births_within))
}

# ---- models -----------------------------------------------------------------------

#' Model matrix with state fixed effects and collinear columns dropped
ntsv_design <- function(data, rhs, fixed_effects = "state") {
  rhs_full <- if (base::is.null(fixed_effects)) rhs else base::paste(rhs, "+", fixed_effects)
  X <- stats::model.matrix(stats::as.formula(base::paste("~", rhs_full)), data = data)
  qr_X <- base::qr(X)
  X[, base::sort(qr_X$pivot[base::seq_len(qr_X$rank)]), drop = FALSE]
}

#' Complete cases for a model
ntsv_complete <- function(data, vars) {
  dplyr::filter(data, dplyr::if_all(dplyr::all_of(vars), ~ !base::is.na(.x)))
}

#' Primary model: births-weighted linear model of the NTSV cesarean rate in
#' percentage points, state fixed effects, wild cluster restricted bootstrap
#' by state
#'
#' Weighted least squares is run as OLS on rows scaled by sqrt(weight), so
#' wild_cluster_bootstrap() (R/ownership.R) applies unchanged. With state
#' fixed effects only states with two or more counties identify the slope;
#' `n_states_identifying` reports how many.
fit_ntsv_linear <- function(data, rhs, term, outcome = "cesarean_rate", weight = "ntsv_births",
                            cluster = "state", fixed_effects = "state", B = 9999L, seed = 20260914L) {
  vars <- base::unique(base::c(outcome, weight, cluster, base::all.vars(stats::as.formula(base::paste("~", rhs)))))
  data <- ntsv_complete(data, vars)
  X <- ntsv_design(data, rhs, fixed_effects)
  if (!term %in% base::colnames(X)) base::stop("Term dropped as collinear or absent: ", term)
  w <- base::sqrt(data[[weight]])
  boot <- wild_cluster_bootstrap(100 * data[[outcome]] * w, X * w, term, data[[cluster]], B = B, seed = seed)
  per_state <- base::table(data[[cluster]])
  tibble::tibble(
    term = term, estimate_pp = boot$estimate, ci_low_pp = boot$ci_low, ci_high_pp = boot$ci_high,
    p_value = boot$p_value, se_crv1_pp = boot$se_crv1, n_units = base::nrow(data), n_states = boot$n_clusters,
    n_states_identifying = if (base::is.null(fixed_effects)) boot$n_clusters else base::sum(per_state >= 2)
  )
}

#' Sensitivity: quasi-binomial logit with CR1 cluster-robust errors by state
#'
#' Returns the log-odds coefficient, its interval (t with G - 1 df), and the
#' births-weighted average marginal effect in percentage points.
fit_ntsv_quasibinomial <- function(data, rhs, term, cluster = "state", fixed_effects = "state", level = 0.95) {
  vars <- base::unique(base::c("cesarean", "ntsv_births", cluster, base::all.vars(stats::as.formula(base::paste("~", rhs)))))
  data <- ntsv_complete(data, vars)
  X <- ntsv_design(data, rhs, fixed_effects)
  y <- data$cesarean / data$ntsv_births
  fit <- stats::glm.fit(X, y, weights = data$ntsv_births, family = stats::quasibinomial())
  mu <- fit$fitted.values
  bread <- base::chol2inv(base::qr.R(fit$qr))[base::order(fit$qr$pivot), base::order(fit$qr$pivot)]
  scores <- base::rowsum(X * (data$ntsv_births * (y - mu)), data[[cluster]])
  G <- base::nrow(scores)
  N <- base::nrow(X)
  vc <- (G / (G - 1)) * ((N - 1) / (N - base::ncol(X))) * bread %*% base::crossprod(scores) %*% bread
  j <- base::match(term, base::colnames(X))
  est <- base::unname(fit$coefficients[j])
  se <- base::sqrt(vc[j, j])
  q <- stats::qt(1 - (1 - level) / 2, df = G - 1)
  ame <- 100 * stats::weighted.mean(mu * (1 - mu), data$ntsv_births) * est
  tibble::tibble(term = term, log_odds = est, ci_low = est - q * se, ci_high = est + q * se,
                 p_value = 2 * stats::pt(-base::abs(est / se), df = G - 1),
                 ame_pp = ame, n_units = N, n_states = G)
}

# ---- implied facility price differential --------------------------------------------

#' Local cesarean-vaginal facility price differential around each point
#'
#' @param hospital_premium tibble(ccn, state, payer_type, premium_dollars)
#'   (analysis/16 birth_cesarean_premium.csv) joined to hospital lat, lon.
#' @return tibble(id, payer_type, differential, n_hospitals, source): the
#'   median differential among hospitals within `radius` miles, or the state
#'   median (source = "state") when none are within reach.
local_price_differential <- function(points, hospital_premium, radius = 30) {
  state_median <- hospital_premium |>
    dplyr::group_by(.data$state, .data$payer_type) |>
    dplyr::summarise(state_differential = stats::median(.data$premium_dollars), .groups = "drop")
  out <- base::lapply(base::seq_len(base::nrow(points)), function(i) {
    p <- points[i, ]
    near <- hospital_premium[haversine_miles(p$lat, p$lon, hospital_premium$lat, hospital_premium$lon) <= radius, ]
    near |>
      dplyr::group_by(.data$payer_type) |>
      dplyr::summarise(differential = stats::median(.data$premium_dollars), n_hospitals = dplyr::n(), .groups = "drop") |>
      dplyr::mutate(id = p$id, state = p$state)
  })
  local <- dplyr::bind_rows(out)
  tidyr::expand_grid(dplyr::select(points, "id", "state"), payer_type = base::unique(hospital_premium$payer_type)) |>
    dplyr::left_join(local, by = base::c("id", "state", "payer_type")) |>
    dplyr::left_join(state_median, by = base::c("state", "payer_type")) |>
    dplyr::mutate(source = dplyr::if_else(base::is.na(.data$differential), "state", "local"),
                  differential = dplyr::coalesce(.data$differential, .data$state_differential),
                  n_hospitals = dplyr::coalesce(.data$n_hospitals, 0L)) |>
    dplyr::select("id", "payer_type", "differential", "n_hospitals", "source")
}

#' Implied facility price differential of a change in the NTSV cesarean rate
#'
#' Arithmetic, not an estimate: change in cesareans = slope (percentage
#' points per exposure unit) / 100 x exposure contrast x annual NTSV births;
#' each payer's share of those births is priced at the local
#' cesarean-vaginal facility price differential (commercial for privately
#' insured births, Medicaid for Medicaid births). Linear in the slope, so the
#' interval bounds carry through directly. Not savings or value: professional
#' fees, downstream care, repeat cesareans, and outcomes are outside it.
#'
#' @param counties tibble(id, ntsv_births_per_year, share_private, share_medicaid).
#' @param differentials local_price_differential() rows.
#' @param slope named numeric c(estimate, ci_low, ci_high), percentage points
#'   per exposure unit.
#' @param contrast exposure units (for example the p25 to p75 difference).
implied_facility_price_differential <- function(counties, differentials, slope, contrast) {
  share_cols <- base::c(commercial = "share_private", medicaid = "share_medicaid")
  births <- counties |>
    tidyr::pivot_longer(dplyr::all_of(base::unname(share_cols)), names_to = "share_col", values_to = "payer_share") |>
    dplyr::mutate(payer_type = base::names(share_cols)[base::match(.data$share_col, share_cols)],
                  payer_births = .data$ntsv_births_per_year * .data$payer_share) |>
    dplyr::inner_join(differentials, by = base::c("id", "payer_type"))
  dplyr::bind_rows(base::lapply(base::names(slope), function(bound) {
    births |>
      dplyr::mutate(change_cesareans = slope[[bound]] / 100 * contrast * .data$payer_births,
                    implied_dollars = .data$change_cesareans * .data$differential) |>
      dplyr::group_by(.data$payer_type) |>
      dplyr::summarise(bound = bound, n_counties = dplyr::n_distinct(.data$id), payer_births = base::sum(.data$payer_births),
                       change_cesareans = base::sum(.data$change_cesareans), implied_facility_price_differential = base::sum(.data$implied_dollars),
                       .groups = "drop")
  }))
}

# ---- figures ----------------------------------------------------------------------

#' County polygons keyed by FIPS
#'
#' `maps` carries county outlines by name, not FIPS, and `maps::county.fips`
#' is the bridge it ships for exactly this. Counties split into several
#' polygons there ("washington:main", "washington:whidbey island") share one
#' FIPS, which is what the join needs.
county_polygons <- function() {
  if (!base::requireNamespace("maps", quietly = TRUE)) {
    base::stop("Package 'maps' is required for the county map.")
  }
  bridge <- maps::county.fips |>
    dplyr::mutate(region = stringr::str_remove(.data$polyname, ":.*$"),
                  county_fips = stringr::str_pad(.data$fips, 5, pad = "0"))
  ggplot2::map_data("county") |>
    dplyr::mutate(polyname = base::paste(.data$region, .data$subregion, sep = ",")) |>
    dplyr::left_join(dplyr::distinct(bridge, .data$polyname, .data$county_fips), by = "polyname")
}

#' Map the NTSV cesarean rate by county of residence
#'
#' Counties WONDER does not report (under 100,000 residents) are the map's
#' main feature, not an omission: they are most of the country and they are
#' drawn in the missing-data grey so the coverage limit is visible rather than
#' implied. Alaska and Hawaii are left out, as in the colonoscopy maps.
#'
#' @param rates tibble(county_fips, cesarean_rate), rate as a proportion.
#' @param title,subtitle plot text.
ntsv_county_map <- function(rates, title = "NTSV cesarean rate by county of residence", subtitle = NULL) {
  polygons <- county_polygons() |>
    dplyr::left_join(dplyr::transmute(rates, .data$county_fips, rate_pct = 100 * .data$cesarean_rate),
                     by = "county_fips")

  ggplot2::ggplot(polygons, ggplot2::aes(x = .data$long, y = .data$lat, group = .data$group, fill = .data$rate_pct)) +
    ggplot2::geom_polygon(colour = "white", linewidth = 0.05) +
    ggplot2::coord_map("albers", lat0 = 29.5, lat1 = 45.5) +
    ggplot2::scale_fill_gradient2(
      name = "NTSV cesarean\nrate (%)", low = "#2166ac", mid = "#f7f7f7", high = "#b2182b",
      midpoint = stats::median(polygons$rate_pct, na.rm = TRUE), na.value = geo_na_fill()
    ) +
    ggplot2::labs(title = title, subtitle = subtitle, x = NULL, y = NULL,
                  caption = "Grey: county not reported by CDC WONDER (under 100,000 residents) or suppressed.") +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                   plot.caption = ggplot2::element_text(size = 8, colour = "grey30", hjust = 0))
}

#' Midwife supply against the NTSV cesarean rate
#'
#' One point per county, area proportional to NTSV births, because a county
#' with 300 first births and one with 30,000 are not equal evidence and a
#' plot that draws them the same size says they are.
#'
#' THE LINE IS NOT THE MODEL. It is the births-weighted bivariate fit, with no
#' covariates and no state fixed effects, so it can and does point the other
#' way from the adjusted estimate: most of the model's work is comparing
#' counties WITHIN a state, and this line compares all counties at once. Both
#' are labelled as what they are, because a plot whose line disagrees with the
#' number printed beside it is worse than either alone.
#'
#' @param data county rows with cnm_per_1k_births, cesarean_rate, ntsv_births,
#'   and optionally `roster_covered` and a metro label.
#' @param slope_pp optional ADJUSTED slope in percentage points per doubling of
#'   supply, reported in the subtitle beside the unadjusted line.
ntsv_supply_rate_plot <- function(data, slope_pp = NULL,
                                  title = "Midwife supply and the NTSV cesarean rate") {
  points <- data |>
    dplyr::filter(!base::is.na(.data$cnm_per_1k_births), !base::is.na(.data$cesarean_rate)) |>
    dplyr::mutate(rate_pct = 100 * .data$cesarean_rate,
                  supply = .data$cnm_per_1k_births + 0.5)
  n_counties <- base::format(base::nrow(points), big.mark = ",")
  subtitle <- if (base::is.null(slope_pp)) {
    base::sprintf("%s counties CDC WONDER reports, sized by NTSV births. Line: unadjusted weighted fit.", n_counties)
  } else {
    base::sprintf(paste0("%s counties, sized by NTSV births. Line: unadjusted weighted fit. ",
                         "Adjusted model, within states: %+.2f points per doubling (association, not effect)."),
                  n_counties, slope_pp)
  }

  ggplot2::ggplot(points, ggplot2::aes(x = .data$supply, y = .data$rate_pct)) +
    ggplot2::geom_point(ggplot2::aes(size = .data$ntsv_births), alpha = 0.35, colour = "#2166ac") +
    ggplot2::geom_smooth(ggplot2::aes(weight = .data$ntsv_births), method = "lm", formula = y ~ x,
                         se = TRUE, colour = "#b2182b", linewidth = 0.7) +
    ggplot2::scale_x_continuous(transform = "log2", breaks = base::c(0.5, 1, 2, 4, 8, 16, 32),
                                labels = function(x) base::round(x - 0.5, 1)) +
    ggplot2::scale_size_area(name = "NTSV births", max_size = 6, labels = scales::label_comma()) +
    ggplot2::labs(title = title, subtitle = subtitle,
                  x = "Midwives within the radius per 1,000 births (log scale)",
                  y = "NTSV cesarean rate (%)",
                  caption = "Midwives: AMCB-certified, NPPES practice ZIP. Rates: CDC WONDER natality, county of residence.") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                   plot.caption = ggplot2::element_text(size = 8, colour = "grey30", hjust = 0))
}
