#' County NTSV cesarean rates, supply, models, and the implied differential
#' (R/ntsv_county.R). Every count here is synthetic.

write_wonder <- function(header, rows, notes = TRUE) {
  path <- base::tempfile(fileext = ".txt")
  quote <- function(x) base::paste0('"', x, '"')
  body <- base::vapply(rows, function(r) base::paste(base::c(quote(r[-base::length(r)]), r[base::length(r)]), collapse = "\t"), "")
  base::writeLines(base::c(base::paste(quote(header), collapse = "\t"), body,
                           if (notes) base::c('"---"', '"Dataset: Natality, 2016-2024 expanded"', '"---"')), path)
  path
}

testthat::test_that("delivery-method exports give NTSV rates without totals, notes, or unknown methods", {
  path <- write_wonder(
    base::c("Notes", "County of Residence", "County of Residence Code", "Delivery Method", "Delivery Method Code", "Births"),
    base::list(
      base::c("", "Alpha County, CO", "08001", "Vaginal", "1", "700"),
      base::c("", "Alpha County, CO", "08001", "Cesarean", "2", "300"),
      base::c("", "Alpha County, CO", "08001", "Unknown or Not Stated", "9", "40"),
      base::c("Total", "Alpha County, CO", "08001", "", "", "1040"),
      base::c("", "Unidentified Counties, CO", "08999", "Vaginal", "1", "900"),
      base::c("", "Unidentified Counties, CO", "08999", "Cesarean", "2", "100")
    )
  )
  rates <- ntsv_cesarean_rates(read_wonder_export(path))
  testthat::expect_equal(base::nrow(rates), 2)
  alpha <- dplyr::filter(rates, .data$geo_code == "08001")
  testthat::expect_equal(alpha$ntsv_births, 1000)
  testthat::expect_equal(alpha$cesarean_rate, 0.3)
  testthat::expect_false(alpha$pooled)
  testthat::expect_true(dplyr::filter(rates, .data$geo_code == "08999")$pooled)
})

testthat::test_that("suppressed cells are bounded, never read as zero", {
  header <- base::c("Notes", "County of Residence", "County of Residence Code", "Delivery Method", "Births")
  path <- write_wonder(header, base::list(
    base::c("", "Small County, CO", "08002", "Vaginal", "40"),
    base::c("", "Small County, CO", "08002", "Cesarean", "Suppressed"),
    base::c("", "Big County, CO", "08003", "Vaginal", "4000"),
    base::c("", "Big County, CO", "08003", "Cesarean", "Suppressed")
  ))
  rates <- ntsv_cesarean_rates(read_wonder_export(path))
  small <- dplyr::filter(rates, .data$geo_code == "08002")
  big <- dplyr::filter(rates, .data$geo_code == "08003")
  # 1 to 9 cesareans over 41 to 49 births: too wide to use
  testthat::expect_equal(small$rate_low, 1 / 41)
  testthat::expect_equal(small$rate_high, 9 / 49)
  testthat::expect_true(base::is.na(small$cesarean_rate))
  # the same suppression in a big county moves the rate by under a point
  testthat::expect_equal(big$cesarean_rate, 5 / 4005)
  testthat::expect_equal(big$n_suppressed, 1)
})

testthat::test_that("state exports are read at state level", {
  path <- write_wonder(base::c("Notes", "State of Residence", "State of Residence Code", "Delivery Method", "Births"), base::list(
    base::c("", "Colorado", "08", "Vaginal", "7500"),
    base::c("", "Colorado", "08", "Cesarean", "2500")
  ))
  rates <- ntsv_cesarean_rates(read_wonder_export(path))
  testthat::expect_equal(rates$level, "state")
  testthat::expect_equal(rates$geo_code, "08")
  testthat::expect_equal(rates$cesarean_rate, 0.25)
})

testthat::test_that("composition shares use known categories and the two-column race export", {
  age <- read_wonder_export(write_wonder(base::c("Notes", "County of Residence", "County of Residence Code", "Age of Mother 9", "Age of Mother 9 Code", "Births"), base::list(
    base::c("", "Alpha County, CO", "08001", "Under 15 years", "1", "Suppressed"),
    base::c("", "Alpha County, CO", "08001", "15-19 years", "2", "95"),
    base::c("", "Alpha County, CO", "08001", "25-29 years", "4", "600"),
    base::c("", "Alpha County, CO", "08001", "35-39 years", "6", "250"),
    base::c("", "Alpha County, CO", "08001", "50 years and over", "9", "50")
  )))
  payment <- read_wonder_export(write_wonder(base::c("Notes", "County of Residence", "County of Residence Code", "Source of Payment for Delivery", "Births"), base::list(
    base::c("", "Alpha County, CO", "08001", "Medicaid", "400"),
    base::c("", "Alpha County, CO", "08001", "Private Insurance", "500"),
    base::c("", "Alpha County, CO", "08001", "Self Pay", "100"),
    base::c("", "Alpha County, CO", "08001", "Unknown or Not Stated", "300")
  )))
  race <- read_wonder_export(write_wonder(base::c("Notes", "County of Residence", "County of Residence Code", "Mother's Hispanic Origin", "Mother's Single Race 6", "Births"), base::list(
    base::c("", "Alpha County, CO", "08001", "Hispanic or Latino", "White", "300"),
    base::c("", "Alpha County, CO", "08001", "Hispanic or Latino", "Black or African American", "100"),
    base::c("", "Alpha County, CO", "08001", "Not Hispanic or Latino", "Black or African American", "200"),
    base::c("", "Alpha County, CO", "08001", "Not Hispanic or Latino", "White", "350"),
    base::c("", "Alpha County, CO", "08001", "Not Hispanic or Latino", "Asian", "50"),
    base::c("", "Alpha County, CO", "08001", "Origin unknown or not stated", "White", "500")
  )))
  comp <- ntsv_composition(base::list(age = age, payment = payment, race = race))
  testthat::expect_equal(comp$share_age_35plus, 300 / 1000)
  testthat::expect_equal(comp$share_age_under20, 100 / 1000)
  testthat::expect_equal(comp$share_medicaid, 0.4)
  testthat::expect_equal(comp$share_private, 0.5)
  testthat::expect_equal(comp$share_hispanic, 0.4)
  testthat::expect_equal(comp$share_black_nh, 0.2)
  testthat::expect_equal(comp$share_asian_nh, 0.05)
  testthat::expect_false("share_obese" %in% base::names(comp))
})

testthat::test_that("supply counts midwives, births, obstetricians, and L&D hospitals within the radius", {
  points <- tibble::tibble(id = "08031", lat = 39.7392, lon = -104.9903, county_fips = "08031")
  counties <- tibble::tibble(county_fips = base::c("08031", "08013", "08077"), lat = base::c(39.7392, 40.0150, 39.0639),
                             lon = base::c(-104.9903, -105.2705, -108.5506), births = base::c(8000, 2000, 1500),
                             obgyn = base::c(90, 30, 10))
  midwives <- tibble::tibble(lat = base::c(39.74, 40.01, 39.06), lon = base::c(-104.99, -105.27, -108.55))
  ld <- tibble::tibble(lat = 40.0150, lon = -105.2705)
  s <- midwifery_supply_at(points, midwives, counties, radius = 30, ld_hospitals = ld)
  testthat::expect_equal(s$cnm_within, 2)
  testthat::expect_equal(s$births_within, 10000)
  testthat::expect_equal(s$obgyn_per_1k_births, 12)
  testthat::expect_equal(s$ld_within, 1)
  testthat::expect_true(base::is.na(s$bc_within))
})

simulate_counties <- function(n_states = 30, per_state = 6, slope = -1.5, seed = 3) {
  base::set.seed(seed)
  n <- n_states * per_state
  state <- base::rep(base::sprintf("S%02d", base::seq_len(n_states)), each = per_state)
  state_effect <- stats::rnorm(n_states, 0, 2)[base::match(state, base::unique(state))]
  x <- stats::rnorm(n)
  ntsv_births <- base::round(stats::runif(n, 800, 6000))
  rate <- (25 + slope * x + state_effect + stats::rnorm(n, 0, 1)) / 100
  tibble::tibble(state, log2_cnm = x, ntsv_births, cesarean = base::round(rate * ntsv_births),
                 cesarean_rate = .data$cesarean / .data$ntsv_births, share_age_35plus = stats::runif(n, 0.1, 0.3))
}

testthat::test_that("the weighted linear model equals weighted least squares and recovers the slope", {
  d <- simulate_counties()
  fit <- fit_ntsv_linear(d, "log2_cnm + share_age_35plus", "log2_cnm", B = 999)
  wls <- stats::lm(I(100 * cesarean_rate) ~ log2_cnm + share_age_35plus + state, data = d, weights = ntsv_births)
  testthat::expect_equal(fit$estimate_pp, base::unname(stats::coef(wls)["log2_cnm"]), tolerance = 1e-8)
  testthat::expect_true(fit$ci_low_pp < -1.5 && fit$ci_high_pp > -1.5)
  testthat::expect_equal(fit$n_states, 30)
  testthat::expect_equal(fit$n_states_identifying, 30)
})

testthat::test_that("the quasi-binomial sensitivity matches glm and gives a clustered interval", {
  d <- simulate_counties()
  fit <- fit_ntsv_quasibinomial(d, "log2_cnm + share_age_35plus", "log2_cnm")
  g <- stats::glm(cbind(cesarean, ntsv_births - cesarean) ~ log2_cnm + share_age_35plus + state, data = d, family = stats::quasibinomial())
  testthat::expect_equal(fit$log_odds, base::unname(stats::coef(g)["log2_cnm"]), tolerance = 1e-6)
  testthat::expect_true(fit$ci_low < fit$log_odds && fit$ci_high > fit$log_odds)
  testthat::expect_true(fit$ame_pp < 0)
  testthat::skip_if_not_installed("sandwich")
  cr1 <- sandwich::vcovCL(g, cluster = ~state, type = "HC1")
  testthat::expect_equal((fit$log_odds - fit$ci_low) / stats::qt(0.975, 29), base::sqrt(cr1["log2_cnm", "log2_cnm"]), tolerance = 1e-4)
})

testthat::test_that("local price differentials fall back to the state median when no hospital is near", {
  hospitals <- tibble::tibble(ccn = base::c("A", "B", "C"), state = "CO", payer_type = "commercial",
                              premium_dollars = base::c(3000, 5000, 9000), lat = base::c(39.74, 39.75, 37.0), lon = base::c(-104.99, -105.0, -108.0))
  points <- tibble::tibble(id = base::c("near", "far"), state = "CO", lat = base::c(39.74, 41.0), lon = base::c(-104.99, -102.1))
  d <- local_price_differential(points, hospitals, radius = 30)
  testthat::expect_equal(dplyr::filter(d, .data$id == "near")$differential, 4000)
  testthat::expect_equal(dplyr::filter(d, .data$id == "near")$source, "local")
  testthat::expect_equal(dplyr::filter(d, .data$id == "far")$differential, 5000)
  testthat::expect_equal(dplyr::filter(d, .data$id == "far")$source, "state")
})

testthat::test_that("the implied facility price differential is the stated arithmetic", {
  counties <- tibble::tibble(id = "c1", ntsv_births_per_year = 1000, share_private = 0.6, share_medicaid = 0.4)
  differentials <- tibble::tibble(id = "c1", payer_type = base::c("commercial", "medicaid"), differential = base::c(4000, 2000),
                                  n_hospitals = 3L, source = "local")
  out <- implied_facility_price_differential(counties, differentials, slope = base::c(estimate = -2, ci_low = -3, ci_high = -1), contrast = 1.5)
  est <- dplyr::filter(out, .data$bound == "estimate")
  # -2 points x 1.5 units x 600 births = -18 cesareans x $4,000; x 400 births = -12 x $2,000
  testthat::expect_equal(dplyr::filter(est, .data$payer_type == "commercial")$change_cesareans, -18)
  testthat::expect_equal(dplyr::filter(est, .data$payer_type == "commercial")$implied_facility_price_differential, -72000)
  testthat::expect_equal(dplyr::filter(est, .data$payer_type == "medicaid")$implied_facility_price_differential, -24000)
  testthat::expect_equal(dplyr::filter(out, .data$bound == "ci_low", .data$payer_type == "commercial")$implied_facility_price_differential, -108000)
})

testthat::test_that("catchments reaching a state the roster does not cover get no midwife count", {
  zcta <- tibble::tibble(zip = base::c("19103", "08002", "19711"), lat = base::c(39.9526, 39.9340, 39.6837),
                         lon = base::c(-75.1652, -75.0246, -75.7497))
  zcta_county <- tibble::tibble(zip = zcta$zip, county_fips = base::c("42101", "34007", "10003"))
  # roster covers PA only: NJ (Cherry Hill, ~8 mi from Philadelphia) and DE (Newark, ~40 mi) are uncovered
  roster <- tibble::tibble(npi = "1", zip = "19103", state = "PA", certification = "CNM")
  uncovered <- roster_uncovered_zctas(zcta, zcta_county, roster)
  testthat::expect_setequal(uncovered$state, base::c("NJ", "DE"))
  counties <- tibble::tibble(county_fips = "42101", lat = 39.9526, lon = -75.1652, births = 20000)
  points <- tibble::tibble(id = base::c("philly", "lancaster"), lat = base::c(39.9526, 40.0379), lon = base::c(-75.1652, -76.3055),
                           county_fips = base::c("42101", "42071"))
  s <- midwifery_supply_at(points, dplyr::inner_join(roster, zcta, by = "zip"), counties, radius = 30, uncovered = uncovered)
  testthat::expect_false(s$roster_covered[s$id == "philly"])
  testthat::expect_true(base::is.na(s$cnm_per_1k_births[s$id == "philly"]))
  # Lancaster is ~50 miles from the nearest uncovered ZCTA here
  testthat::expect_true(s$roster_covered[s$id == "lancaster"])
  testthat::expect_equal(s$cnm_within[s$id == "lancaster"], 0)
})

testthat::test_that("state FIPS codes map to USPS abbreviations", {
  m <- state_fips_postal()
  testthat::expect_length(m, 52)
  testthat::expect_equal(base::unname(m[base::c("06", "11", "34", "56")]), base::c("CA", "DC", "NJ", "WY"))
  testthat::expect_setequal(base::setdiff(m, "PR"), base::c(datasets::state.abb, "DC"))
})
