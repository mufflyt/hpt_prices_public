#' Builds hpt.duckdb from a tiny canonical price fixture and checks the
#' schema, the ENUM normalization, the plausibility flag, the payer typing,
#' and the two-stage state medians.

price_fixture <- function() {
  conform_price_table(tibble::tibble(
    source = "own_crawl",
    mrf_file_id = base::c(base::rep("f1", 6), base::rep("f2", 3), "f3"),
    mrf_url = base::c(base::rep("https://a.org/1_a_standardcharges.csv", 6), base::rep("https://b.org/2_b_standardcharges.csv", 3), "https://c.org/3.csv"),
    hospital_name = base::c(base::rep("Alpha", 6), base::rep("Beta", 3), "Gamma"),
    license_state = base::c(base::rep("CO", 6), base::rep("CO", 3), NA),
    concept = "colonoscopy", code = "45378", code_system = "CPT", type_verified = TRUE,
    setting = base::c("outpatient", "Outpatient", "outpatient", "inpatient", "outpatient", "outpatient", "outpatient", "outpatient", "outpatient", "outpatient"),
    billing_class = base::c(NA, "Facility", NA, NA, "professional", NA, NA, NA, NA, NA),
    payer_name = base::c("Aetna", "Cigna", "Colorado Medicaid", "Aetna", "Aetna", "Humana", "Aetna", "UHC", "Health First Colorado Medicaid", "Aetna"),
    plan_name = base::c("PPO", "OAP", "", "PPO", "PPO", "Medicare Advantage HMO", "PPO", "PPO", "", "PPO"),
    negotiated_dollar = base::c(1000, 1400, 300, 5000, 250, 900, 2000, 2200, 0.01, 1500),
    gross = base::c(3000, 3000, 3000, 3000, 400, 3000, 5000, 5000, 5000, 2500),
    discounted_cash = base::c(1200, 1200, 1200, 1200, NA, 1200, 2500, 2500, 2500, 1000),
    methodology = base::c("fee schedule", "Fee Schedule", "case rate", "fee schedule", "fee schedule", "other", "fee schedule", "fee schedule", "fee schedule", "Percent of total billed charge")
  ))
}

testthat::test_that("database builds with ENUMs, plausibility flag, payer types, and CCN bridge", {
  dir <- base::tempfile("db")
  base::dir.create(dir)
  prices_path <- base::file.path(dir, "prices.parquet")
  arrow::write_parquet(price_fixture(), prices_path)

  crosswalk_path <- base::file.path(dir, "crosswalk.parquet")
  arrow::write_parquet(
    # f1 appears twice (two facilities sharing the file, matched by different
    # methods): the bridge must still hold one row. f3 matched no CCN but its
    # facility is in TX; 999999 is not in the roster and must be dropped.
    tibble::tibble(mrf_file_id = base::c("f1", "f1", "f2", "f3", "f2"), ccn = base::c("060011", "060011", "060024", NA, "999999"),
                   ccn_match_method = base::c("mrf_url", "npi", "mrf_url", NA, "name_address"), ccn_match_score = base::c(1, 0.8, 1, NA, 0.7),
                   ccn_conflict = FALSE, ccn_ambiguous = FALSE, state = base::c("CO", "CO", "CO", "TX", "CO")),
    crosswalk_path
  )
  universe <- tibble::tibble(
    facility_id = base::c("060011", "060024"), facility_name = base::c("Alpha", "Beta"), address = "x",
    citytown = "Denver", state = "CO", zip_code = "80204", hospital_type = "Acute Care Hospitals",
    hospital_ownership = "Government", health_sys_id = NA_character_, health_sys_name = "NorthShore \x96 Test"
  )
  db_path <- base::file.path(dir, "hpt.duckdb")

  build_hpt_database(prices_path, crosswalk_path, universe, test_codebook(), db_path = db_path)

  types <- duckdb_query(
    "SELECT column_name, data_type FROM duckdb_columns() WHERE table_name = 'fact_rate' AND column_name IN ('setting', 'billing_class', 'methodology', 'code_id')",
    database = db_path, read_only = TRUE
  )
  testthat::expect_true(base::all(stringr::str_detect(types$data_type[types$column_name != "code_id"], "^ENUM")))
  testthat::expect_equal(types$data_type[types$column_name == "code_id"], "SMALLINT")

  facts <- duckdb_query(
    "SELECT r.*, p.payer_type::VARCHAR AS payer_type FROM fact_rate r LEFT JOIN dim_payer p USING (payer_id)",
    database = db_path, read_only = TRUE
  )
  testthat::expect_equal(base::nrow(facts), 10)
  testthat::expect_equal(base::sum(!facts$plausible), 1)               # the $0.01 sentinel
  testthat::expect_setequal(base::unique(base::as.character(facts$setting)), base::c("outpatient", "inpatient"))
  testthat::expect_true("percent of total billed charges" %in% base::as.character(facts$methodology))
  testthat::expect_setequal(base::unique(facts$payer_type), base::c("commercial", "medicaid", "medicare_advantage"))

  # R duckdb 1.4.4 can open the file (v1.4.0 storage)
  con <- DBI::dbConnect(duckdb::duckdb(), db_path, read_only = TRUE)
  base::on.exit(DBI::dbDisconnect(con), add = TRUE)
  testthat::expect_equal(DBI::dbGetQuery(con, "SELECT count(*) AS n FROM v_hospital_rate")$n, 10)
  testthat::expect_equal(DBI::dbGetQuery(con, "SELECT health_sys_name FROM dim_hospital LIMIT 1")$health_sys_name, "NorthShore – Test")

  medians <- compute_state_medians(db_path, out_dir = dir)
  co_commercial <- medians |> dplyr::filter(.data$state == "CO", .data$insurance_type == "commercial", .data$fee_type == "facility")
  # hospital medians: Alpha = median(1000, 1400) = 1200 (inpatient 5000 excluded); Beta = median(2000, 2200) = 2100
  testthat::expect_equal(co_commercial$median_price, 1650)
  testthat::expect_equal(co_commercial$n_hospitals, 2)
  # professional fee reported separately
  testthat::expect_equal(medians$median_price[medians$state == "CO" & medians$fee_type == "professional" & medians$insurance_type == "commercial"], 250)
  # unmatched file f3 counts as its own unit in TX, via the crosswalk facility state
  testthat::expect_true(base::any(medians$state == "TX"))
  bridge <- duckdb_query("SELECT * FROM bridge_file_ccn", database = db_path, read_only = TRUE)
  testthat::expect_equal(base::nrow(bridge), 2)                           # one row per (file, CCN); 999999 dropped
  testthat::expect_equal(bridge$ccn_match_method[bridge$ccn == "060011"], "mrf_url")
  # Beta's Medicaid rate was the implausible $0.01, so only Alpha contributes
  testthat::expect_equal(medians$n_hospitals[medians$state == "CO" & medians$insurance_type == "medicaid" & medians$fee_type == "facility"], 1)
})

testthat::test_that("a blank billing class is inferred professional from a professional-level gross", {
  dir <- base::tempfile("db")
  base::dir.create(dir)
  # Beta lists a blank-class line at $450 gross (an unlabeled professional
  # fee). Typical gross: facility 3000 (f1's explicit "Facility" row),
  # professional 400 (f1's explicit professional row); cutoff sqrt(3000 x 400)
  # = 1095, so $450 is professional and $2,500-5,000 stay facility.
  prices <- price_fixture()
  pro_line <- prices[7, ]
  pro_line$gross <- 450
  pro_line$negotiated_dollar <- 300
  pro_line$description <- "DIAGNOSTIC COLONOSCOPY"
  prices_path <- base::file.path(dir, "prices.parquet")
  arrow::write_parquet(dplyr::bind_rows(prices, pro_line), prices_path)
  crosswalk_path <- base::file.path(dir, "crosswalk.parquet")
  arrow::write_parquet(
    tibble::tibble(mrf_file_id = base::c("f1", "f2", "f3"), ccn = base::c("060011", "060024", NA),
                   ccn_match_method = "mrf_url", ccn_match_score = 1, ccn_conflict = FALSE, ccn_ambiguous = FALSE,
                   state = base::c("CO", "CO", "TX")),
    crosswalk_path
  )
  universe <- tibble::tibble(
    facility_id = base::c("060011", "060024"), facility_name = base::c("Alpha", "Beta"), address = "x",
    citytown = "Denver", state = "CO", zip_code = "80204", hospital_type = "Acute Care Hospitals",
    hospital_ownership = "Government", health_sys_id = NA_character_, health_sys_name = NA_character_
  )
  db_path <- base::file.path(dir, "hpt.duckdb")
  base::suppressMessages(build_hpt_database(prices_path, crosswalk_path, universe, test_codebook(), db_path = db_path,
                                            fee_type_min_files = 1L))

  facts <- duckdb_query("SELECT gross, CAST(billing_class AS VARCHAR) AS bc, CAST(fee_type AS VARCHAR) AS fee_type, fee_type_inferred FROM fact_rate",
                        database = db_path, read_only = TRUE)
  testthat::expect_equal(facts$fee_type[facts$gross == 450], "professional")
  testthat::expect_true(facts$fee_type_inferred[facts$gross == 450])
  testthat::expect_true(base::all(facts$fee_type[facts$bc == "unknown" & facts$gross >= 2500] == "facility"))

  medians <- compute_state_medians(db_path, out_dir = dir)
  co <- medians |> dplyr::filter(.data$state == "CO", .data$insurance_type == "commercial")
  # Beta's facility median is unchanged (2000, 2200); its inferred $300 is
  # left out of professional fees too, which keep only Alpha's labeled $250
  testthat::expect_equal(co$median_price[co$fee_type == "facility"], 1650)
  testthat::expect_equal(co$median_price[co$fee_type == "professional"], 250)
})

testthat::test_that("operating-room case lines of outpatient codes are flagged and left out of medians", {
  dir <- base::tempfile("db")
  base::dir.create(dir)
  # Alpha also lists 45378 on an OR case line at $60,000 gross (typical
  # facility gross = median of per-file minimums 3000, 5000, 2500 = 3000, so
  # the threshold is 30,000)
  prices <- price_fixture()
  or_line <- prices[1, ]
  or_line$gross <- 60000
  or_line$negotiated_dollar <- 9000
  or_line$description <- "OR CASE COLONOSCOPY"
  # Alpha lists EMB once on a fee schedule ($200) and once as a case rate
  # ($2,500): the case rate prices a surgical case and is left out
  emb <- prices[base::c(1, 1), ]
  emb$concept <- "emb"
  emb$code <- "58100"
  emb$gross <- 500
  emb$negotiated_dollar <- base::c(200, 2500)
  emb$methodology <- base::c("fee schedule", "case rate")
  prices_path <- base::file.path(dir, "prices.parquet")
  arrow::write_parquet(dplyr::bind_rows(prices, or_line, emb), prices_path)

  crosswalk_path <- base::file.path(dir, "crosswalk.parquet")
  arrow::write_parquet(
    tibble::tibble(mrf_file_id = base::c("f1", "f2", "f3"), ccn = base::c("060011", "060024", NA),
                   ccn_match_method = base::c("mrf_url", "mrf_url", NA), ccn_match_score = base::c(1, 1, NA),
                   ccn_conflict = FALSE, ccn_ambiguous = FALSE, state = base::c("CO", "CO", "TX")),
    crosswalk_path
  )
  universe <- tibble::tibble(
    facility_id = base::c("060011", "060024"), facility_name = base::c("Alpha", "Beta"), address = "x",
    citytown = "Denver", state = "CO", zip_code = "80204", hospital_type = "Acute Care Hospitals",
    hospital_ownership = "Government", health_sys_id = NA_character_, health_sys_name = NA_character_
  )
  db_path <- base::file.path(dir, "hpt.duckdb")
  base::suppressMessages(build_hpt_database(prices_path, crosswalk_path, universe, test_codebook(), db_path = db_path))

  ref <- duckdb_query(
    "SELECT code, facility_case_line_gross FROM ref_code_gross JOIN dim_code USING (code_id)",
    database = db_path, read_only = TRUE
  )
  # typical facility gross comes from explicitly classed rows only: f1's
  # "Facility" row (3000)
  testthat::expect_equal(ref$facility_case_line_gross[ref$code == "45378"], 30000)
  flagged <- duckdb_query("SELECT gross FROM fact_rate WHERE case_line", database = db_path, read_only = TRUE)
  testthat::expect_equal(flagged$gross, 60000)

  medians <- compute_state_medians(db_path, out_dir = dir)
  co_commercial <- medians |> dplyr::filter(.data$state == "CO", .data$insurance_type == "commercial", .data$fee_type == "facility")
  # unchanged from the fixture without the OR line: Alpha 1200, Beta 2100
  testthat::expect_equal(co_commercial$median_price[co_commercial$code == "45378"], 1650)
  # EMB keeps only the fee-schedule rate; the colonoscopy case rate (Alpha's
  # Medicaid $300) stays, since the endoscopy encounter is the product
  testthat::expect_equal(co_commercial$median_price[co_commercial$code == "58100"], 200)
  co_medicaid <- medians |> dplyr::filter(.data$state == "CO", .data$insurance_type == "medicaid", .data$code == "45378", .data$fee_type == "facility")
  testthat::expect_equal(co_medicaid$median_price, 300)
})

testthat::test_that("file states keep USPS codes and fall back to the address", {
  con <- DBI::dbConnect(duckdb::duckdb())
  base::on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  sql <- base::paste0(
    "SELECT ", clean_state_sql("state", "address"), " AS clean FROM (VALUES ",
    "('co', NULL), ",                                                     # valid code, any case
    "('PO', '620 West Eighth Street, PO Box 99, Kinsley, KS, 67547'), ",  # street token, state from address
    "('NW', '160 Austin Graybill Rd,North Augusta,SC 29860-9254'), ",     # ZIP+4
    "('SE', NULL), ",                                                     # nothing to fall back on
    "('PO', '921 S Ballancee Ave, Lusk, WYOMING 82225')",                 # no two-letter code before the ZIP
    ") AS t(state, address)"
  )
  testthat::expect_equal(DBI::dbGetQuery(con, sql)$clean, base::c("CO", "KS", "SC", NA, NA))
})
