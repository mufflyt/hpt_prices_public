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

testthat::test_that("APR-DRG severity-1 prices fill in only where a hospital posts no MS-DRG price", {
  prices <- tibble::tibble(
    ccn = base::c("A", "A", "B", "B", "C", "C"),
    code = base::c("807", "560-1", "560-1", "540-1", "807", "788"),
    payer_type = "commercial",
    price = base::c(9000, 6100, 5800, 8200, 9500, 13000)
  )
  out <- add_apr_drg_delivery_prices(prices)

  # A posts MS-DRG 807 and an APR-DRG vaginal price: the MS-DRG one wins, the
  # APR row is dropped rather than averaged in
  a <- out[out$ccn == "A", ]
  testthat::expect_equal(base::nrow(a), 1)
  testthat::expect_equal(a$price, 9000)
  testthat::expect_equal(a$price_source, "ms_drg")

  # B posts only APR-DRG: both codes arrive, recoded to the MS-DRG anchors
  b <- out[out$ccn == "B", ]
  testthat::expect_setequal(b$code, base::c("807", "788"))
  testthat::expect_equal(b$price[b$code == "788"], 8200)
  testthat::expect_true(base::all(b$price_source == "apr_drg"))

  # C is untouched
  testthat::expect_setequal(out$code[out$ccn == "C"], base::c("807", "788"))
  testthat::expect_true(base::all(out$price_source[out$ccn == "C"] == "ms_drg"))

  # with no APR rows at all the frame is unchanged apart from the label
  ms_only <- add_apr_drg_delivery_prices(prices[prices$code %in% base::c("807", "788"), ])
  testthat::expect_equal(base::nrow(ms_only), 3)
  testthat::expect_true(base::all(ms_only$price_source == "ms_drg"))
})

testthat::test_that("every APR-DRG anchor maps to a delivery DRG that is in the codebook", {
  map <- apr_drg_anchor_map()
  testthat::expect_setequal(base::unname(map), base::unname(birth_drg_anchors()))
  codebook <- test_codebook(exclude_concepts = NULL)
  testthat::expect_true(base::all(base::names(map) %in% codebook$code))
  testthat::expect_true(base::all(base::unname(map) %in% codebook$code))
})
