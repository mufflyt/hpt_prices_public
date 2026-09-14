#' Childbirth prices (R/birth_prices.R): the Medicare IPPS benchmark.

testthat::test_that("the IPPS benchmark applies the labor share, wage index, capital GAF, and DRG weight", {
  amounts <- base::list(high_wage = base::c(labor = 4456.72, nonlabor = 2295.89),
                        low_wage = base::c(labor = 4186.62, nonlabor = 2565.99), capital = 524.15)
  weights <- tibble::tibble(code = base::c("788", "807"), weight = base::c(0.9588, 0.6742), gmlos = base::c(2.9, 2.0))
  hospitals <- tibble::tibble(ccn = base::c("060011", "061300"), state = "CO")
  benchmark <- hospital_ipps_benchmark(hospitals, weights, amounts,
                                       wage_index = tibble::tibble(ccn = "060011", wage_index = 1.1),
                                       rural_wage_index = tibble::tibble(state = "CO", rural_wage_index = 0.95))

  # wage index above 1 uses Table 1A: 4456.72 x 1.1 + 2295.89 + 524.15 x 1.1^0.6848
  high <- 4456.72 * 1.1 + 2295.89 + 524.15 * 1.1^0.6848
  # at or below 1 uses Table 1B; the CAH-style CCN falls back to the state rural index
  low <- 4186.62 * 0.95 + 2565.99 + 524.15 * 0.95^0.6848
  got <- benchmark[benchmark$code == "807", ]
  testthat::expect_equal(got$medicare_ipps[got$ccn == "060011"], high * 0.6742)
  testthat::expect_equal(got$medicare_ipps[got$ccn == "061300"], low * 0.6742)
  testthat::expect_equal(got$wage_index_source, base::c("ipps_table_2", "state_rural"))
  testthat::expect_equal(base::nrow(benchmark), 4)
})
