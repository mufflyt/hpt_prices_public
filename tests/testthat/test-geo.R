#' Geographic figures: the Medicare OPPS benchmark, state summaries, and the
#' hatch-line geometry (R/geo_figures.R).

testthat::test_that("the Medicare benchmark wage-adjusts the OPPS rate, falling back to the state rural index", {
  hospitals <- tibble::tibble(ccn = base::c("060011", "061300"), state = "CO")
  wage_index <- tibble::tibble(ccn = "060011", wage_index = 1.2)
  rural <- tibble::tibble(state = "CO", rural_wage_index = 0.9)

  benchmark <- hospital_medicare_benchmark(hospitals, opps_rate = 1000, wage_index = wage_index, rural_wage_index = rural)
  # 1000 x (0.6 x 1.2 + 0.4) = 1120; critical access hospital 061300: 1000 x (0.6 x 0.9 + 0.4) = 940
  testthat::expect_equal(benchmark$medicare_opps, base::c(1120, 940))
  testthat::expect_equal(benchmark$wage_index_source, base::c("ipps_table_2", "state_rural"))
})

testthat::test_that("hospital ratios and state summaries flag states with few hospitals", {
  prices <- tibble::tibble(
    ccn = base::c("060011", "060024", "060030", "450001"), code = "45378", payer_type = "commercial",
    price = base::c(2000, 3000, 4000, 1500)
  )
  hospitals <- tibble::tibble(ccn = prices$ccn, state = base::c("CO", "CO", "CO", "TX"), hospital_type = "Acute Care Hospitals")
  wage_index <- tibble::tibble(ccn = prices$ccn, wage_index = 1)
  rural <- tibble::tibble(state = base::c("CO", "TX"), rural_wage_index = 0.9)
  opps <- tibble::tibble(code = "45378", payment_rate = 1000)

  ratios <- hospital_price_ratios(prices, hospitals, "45378", opps, wage_index, rural)
  testthat::expect_equal(ratios$ratio, base::c(2, 3, 4, 1.5))

  summary <- state_ratio_summary(ratios, min_hospitals = 3L)
  co <- summary[summary$state == "CO", ]
  testthat::expect_equal(co$median_ratio, 3)
  testthat::expect_equal(base::c(co$p25_ratio, co$p75_ratio), base::c(2.5, 3.5))
  testthat::expect_false(co$low_n)
  testthat::expect_true(summary$low_n[summary$state == "TX"])

  testthat::expect_error(hospital_price_ratios(prices, hospitals, "58300", opps, wage_index, rural), "No OPPS payment rate")
})

testthat::test_that("hatch lines stay inside the hatched state", {
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("maps")
  hatch <- state_hatch_lines("CO")
  testthat::expect_gt(base::nrow(hatch), 10)
  # Colorado's bounding box: 109.06-102.04 W, 36.99-41.00 N
  testthat::expect_true(base::all(hatch$long >= -109.1 & hatch$long <= -102.0))
  testthat::expect_true(base::all(hatch$lat >= 36.9 & hatch$lat <= 41.1))
  testthat::expect_equal(base::nrow(state_hatch_lines(base::character())), 0)
  testthat::expect_equal(state_map_name(base::c("CO", "DC", "PR")), base::c("Colorado", "District of Columbia", NA))
})

testthat::test_that("system weighting gives each health system one vote per state", {
  hospitals <- tibble::tibble(
    ccn = base::c("070001", "070002", "070003", "070004", "070005", "070006"),
    health_sys_id = base::c("S1", "S1", "S1", NA, NA, "S2"),
    health_sys_name = base::c("Big System", "Big System", "Big System", NA, " ", "Small System")
  )
  units <- hospital_system_units(hospitals)
  testthat::expect_equal(units$system_unit, base::c("sys:S1", "sys:S1", "sys:S1", "ccn:070004", "ccn:070005", "sys:S2"))

  # CT: three Big System hospitals at 0.5x, three other units at 3x, 4x, 5x
  ratios <- tibble::tibble(
    ccn = hospitals$ccn, code = "45378", payer_type = "commercial", state = "CT",
    ratio = base::c(0.5, 0.5, 0.5, 3, 4, 5), price = 1000 * base::c(0.5, 0.5, 0.5, 3, 4, 5), medicare_opps = 1000
  )
  hospital_weighted <- state_ratio_summary(ratios, min_hospitals = 5L)
  testthat::expect_equal(hospital_weighted$median_ratio, 1.75)            # median of 0.5, 0.5, 0.5, 3, 4, 5

  system_weighted <- state_system_weighted_summary(ratios, units, min_units = 5L)
  testthat::expect_equal(system_weighted$n_units, 4L)
  testthat::expect_equal(system_weighted$n_hospitals, 6L)
  testthat::expect_equal(system_weighted$largest_unit_share, 0.5)
  testthat::expect_equal(system_weighted$median_ratio, 3.5)               # median of 0.5, 3, 4, 5
  testthat::expect_true(system_weighted$low_n)                            # 4 units < 5

  comparison <- compare_system_weighting(hospital_weighted, system_weighted)
  testthat::expect_equal(comparison$log_change, base::log(3.5 / 1.75))
})

testthat::test_that("system weighting stats report correlation, spread, and movers", {
  set.seed(2)
  states <- base::c("CO", "TX", "NY", "CA", "FL", "OH")
  ratios <- tibble::tibble(
    ccn = base::sprintf("%06d", 1:36), code = "45378", payer_type = "commercial",
    state = base::rep(states, each = 6), ratio = stats::rlnorm(36, base::log(2), 0.3), medicare_opps = 1000
  ) |> dplyr::mutate(price = .data$ratio * .data$medicare_opps)
  units <- tibble::tibble(ccn = ratios$ccn, system_unit = base::paste0("ccn:", ratios$ccn))

  comparison <- compare_system_weighting(state_ratio_summary(ratios, 5L), state_system_weighted_summary(ratios, units, 5L))
  stats <- system_weighting_stats(comparison, ratios, units)
  # every hospital is its own system, so the two weightings agree exactly
  testthat::expect_equal(stats$spearman, 1)
  testthat::expect_equal(stats$p90_p10_hospital_weighted, stats$p90_p10_system_weighted)
  testthat::expect_equal(stats$state_r2_hospital_level, stats$state_r2_system_level)
  testthat::expect_equal(stats$n_states, 6L)
})

testthat::test_that("every state and DC falls in exactly one Census region", {
  abb <- base::c(datasets::state.abb, "DC")
  regions <- census_region(abb)
  testthat::expect_false(base::anyNA(regions))
  testthat::expect_equal(base::as.integer(base::table(regions)[base::c("Northeast", "Midwest", "South", "West")]), base::c(9L, 12L, 17L, 13L))
  testthat::expect_true(base::is.na(census_region("PR")))
})
