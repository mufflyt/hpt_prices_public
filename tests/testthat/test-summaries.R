write_price_fixture <- function(rows, dir) {
  base::dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- base::file.path(dir, "prices.parquet")
  arrow::write_parquet(conform_price_table(rows), path)
  path
}

testthat::test_that("summaries attribute a shared file's rates to every CCN it covers", {
  dir <- base::tempfile("summ")
  price_path <- write_price_fixture(
    tibble::tibble(
      source = "trilliant", mrf_file_id = "aaa", concept = "emb", code = "58100",
      type_verified = TRUE, payer_name = base::c("Aetna", "Cigna", NA),
      plan_name = base::c("PPO", "HMO", NA), negotiated_dollar = base::c(190, 210, NA),
      gross = 330, discounted_cash = 115
    ),
    dir
  )
  file_ccn_path <- base::file.path(dir, "file_ccn.parquet")
  arrow::write_parquet(
    tibble::tibble(mrf_file_id = "aaa", ccn = base::c("060011", "06001F"), ccn_match_method = "mrf_url"),
    file_ccn_path
  )
  out_path <- base::file.path(dir, "summary.parquet")

  summarize_prices(price_path, file_ccn_path, out_path)
  summary <- tibble::as_tibble(arrow::read_parquet(out_path))

  testthat::expect_setequal(summary$ccn, base::c("060011", "06001F"))
  testthat::expect_equal(base::unique(summary$n_payers), 2)
  testthat::expect_equal(base::unique(summary$negotiated_median), 200)
  testthat::expect_equal(base::unique(summary$gross_median), 330)
})

testthat::test_that("rate-pattern flags catch colonoscopy case rates and DRG 742 == 743", {
  dir <- base::tempfile("flags")
  price_path <- write_price_fixture(
    tibble::tibble(
      source = "own_crawl",
      mrf_file_id = base::c(base::rep("hca", 5), base::rep("ok", 3)),
      concept = base::c(base::rep("colonoscopy", 3), base::rep("drg_uterine_nonmalignant", 2), base::rep("colonoscopy", 3)),
      code = base::c("45378", "45380", "45385", "742", "743", "45378", "45380", "45385"),
      payer_name = "Payer", plan_name = "Plan",
      negotiated_dollar = base::c(2400, 2400, 2400, 4400, 4400, 1200, 1600, 1600)
    ),
    dir
  )
  out_path <- base::file.path(dir, "flags.parquet")

  flag_rate_patterns(price_path, out_path)
  flags <- tibble::as_tibble(arrow::read_parquet(out_path))

  testthat::expect_setequal(flags$flag, base::c("identical_rate_across_colonoscopy_codes", "drg_742_equals_743"))
  testthat::expect_true(base::all(flags$mrf_file_id == "hca"))
})
