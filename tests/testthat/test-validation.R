#' Validation suite (R/validation.R). A tiny hpt.duckdb is built at test time
#' through the real build_hpt_database() path: three Trilliant files and one
#' own-crawl copy of the first (same MRF URL, different spelling), four payer
#' types, three Colorado hospitals. Every check is exercised on the clean
#' database (pass) and on a corrupted copy (warn/fail).

if (!base::exists("run_validation", mode = "function")) {
  base::source(base::file.path(repo_root_path(), "R", "validation.R"))
}

validation_base_rates <- function() {
  base::c("45378" = 1200, "45380" = 1600, "58100" = 200, "58300" = 110, "43775" = 15000, "43644" = 20000, "742" = 30000)
}

validation_payers <- function() {
  tibble::tibble(
    payer_name = base::c("Aetna", "Medicare", "Health First Colorado", "Humana"),
    plan_name = base::c("PPO", "Part A/B", "Medicaid", "Medicare Advantage HMO"),
    payer_type = base::c("Commercial", "Medicare", "Medicaid", "Medicare Advantage"),
    payer_mult = base::c(1, 0.8, 0.35, 0.75)
  )
}

validation_files <- function() {
  tibble::tibble(
    source = base::c("trilliant", "trilliant", "trilliant", "own_crawl"),
    mrf_file_id = base::c(
      base::as.character(openssl::md5(base::c("denver", "aurora", "boulder"))),
      base::as.character(openssl::sha256("denver own crawl"))
    ),
    mrf_url = base::c(
      "https://www.denverhealth.org/mrf/84-6000000_denver-health_standardcharges.csv",
      "https://aurora.example.org/mrf/1_aurora_standardcharges.csv",
      "https://boulder.example.org/mrf/2_boulder_standardcharges.csv",
      "http://denverhealth.org/mrf/84-6000000_denver-health_standardcharges.csv?v=2"
    ),
    hospital_name = base::c("Denver Health and Hospital Authority", "Aurora Medical", "Boulder General", "Denver Health and Hospital Authority"),
    license_state = "CO",
    type_2_npi = base::c("1111111111", "2222222222", "3333333333", "1111111111"),
    last_updated_on = base::c("2026-04-30", "2026-05-01", "2026-05-01", "2026-04-30"),
    hospital_mult = base::c(1, 1.1, 0.9, 1),
    ccn = base::c("060011", "060002", "060003", "060011")
  )
}

validation_price_rows <- function() {
  base_rates <- validation_base_rates()
  codebook <- test_codebook()

  rows <- tidyr::expand_grid(validation_files(), code = base::names(base_rates), validation_payers()) |>
    dplyr::mutate(
      base = base::unname(base_rates[.data$code]),
      negotiated_dollar = base::round(.data$base * .data$payer_mult * .data$hospital_mult, 2),
      gross = base::round(.data$base * 2.5 * .data$hospital_mult, 2),
      discounted_cash = base::round(.data$base * 0.9 * .data$hospital_mult, 2),
      setting = dplyr::if_else(.data$code == "742", "inpatient", "outpatient"),
      description = base::paste("Line", .data$code),
      methodology = "fee schedule", type_verified = TRUE
    )
  # a gross/cash-only charge line with no payer
  cash_only <- rows |>
    dplyr::filter(.data$source == "trilliant", .data$ccn == "060002", .data$code == "58300", .data$payer_name == "Aetna") |>
    dplyr::mutate(payer_name = NA_character_, plan_name = NA_character_, payer_type = NA_character_, negotiated_dollar = NA_real_)

  dplyr::bind_rows(rows, cash_only) |>
    dplyr::mutate(payer_type = dplyr::if_else(.data$source == "trilliant", .data$payer_type, NA_character_)) |>
    dplyr::left_join(codebook |> dplyr::select("code", "concept", "code_system"), by = "code")
}

build_validation_fixture <- function(dir = base::tempfile("validation_db")) {
  base::dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  prices_path <- base::file.path(dir, "prices.parquet")
  arrow::write_parquet(conform_price_table(validation_price_rows()), prices_path)

  crosswalk_path <- base::file.path(dir, "crosswalk.parquet")
  arrow::write_parquet(
    validation_files() |>
      dplyr::transmute(.data$mrf_file_id, .data$ccn, ccn_match_method = "mrf_url", ccn_match_score = 1,
                       ccn_conflict = FALSE, ccn_ambiguous = FALSE),
    crosswalk_path
  )
  universe <- tibble::tibble(
    facility_id = base::c("060011", "060002", "060003", "450097", "450001"),
    facility_name = base::c("Denver Health Medical Center", "Aurora Medical", "Boulder General", "HCA Houston Healthcare Southeast", "Other TX"),
    address = "x", citytown = "Town", state = base::c("CO", "CO", "CO", "TX", "TX"), zip_code = "80000",
    hospital_type = "Acute Care Hospitals", hospital_ownership = "Government", health_sys_id = NA_character_, health_sys_name = NA_character_
  )
  db_path <- base::file.path(dir, "hpt.duckdb")
  build_hpt_database(prices_path, crosswalk_path, universe, test_codebook(), db_path = db_path)
  db_path
}

validation_fixture_path <- local({
  cached <- NULL
  function() {
    if (base::is.null(cached) || !base::file.exists(cached)) {
      cached <<- base::suppressMessages(build_validation_fixture())
    }
    cached
  }
})

#' A copy of the fixture with `sql` applied
corrupted_fixture <- function(sql) {
  path <- base::tempfile(fileext = ".duckdb")
  base::file.copy(validation_fixture_path(), path)
  run_duckdb_sql(sql, database = path)
  path
}

fixture_medians <- function(db_path = validation_fixture_path()) {
  validation_medians(db_path)
}

fixture_opps <- function() {
  rates <- parse_opps_addendum_b(fixture_path("validation", "opps_addendum_b_sample.csv"))
  base::attr(rates, "source_path") <- "opps_addendum_b_sample.csv"
  rates
}

fixture_answers <- function(last_updated_on = "2026-04-30", scale = 1) {
  denver_45378 <- validation_base_rates()[["45378"]] * validation_payers()$payer_mult
  tibble::tibble(
    hospital = "Fixture Denver", ccn = "060011", npi = NA_character_,
    hospital_name = "Denver Health and Hospital Authority", trilliant_id = NA_character_,
    last_updated_on = last_updated_on,
    code = base::c("45378", "45378", "45378", "45378", "58100"),
    stat = base::c("median_negotiated", "max_gross", "n_rows", "n_payers", "median_negotiated"),
    expected = scale * base::c(stats::median(denver_45378), validation_base_rates()[["45378"]] * 2.5, 4, 4,
                               stats::median(validation_base_rates()[["58100"]] * validation_payers()$payer_mult))
  )
}

test_rules <- function() {
  load_payer_type_rules(base::file.path(repo_root_path(), "config", "payer_type_rules.csv"))
}

# ---- helpers -----------------------------------------------------------------

testthat::test_that("validation rows reject unknown statuses and grade thresholds", {
  testthat::expect_error(validation_row("x", "y", "z", "ok"), "Unknown validation status")
  testthat::expect_equal(grade_at_least(0.95, 0.9, 0.8), "pass")
  testthat::expect_equal(grade_at_least(0.85, 0.9, 0.8), "warn")
  testthat::expect_equal(grade_at_least(0.5, 0.9, 0.8), "fail")
  testthat::expect_equal(grade_at_most(0.10, 0.05, Inf), "warn")
  testthat::expect_equal(grade_at_most(NA_real_, 0.05), "skip")
})

# ---- 1. integrity ------------------------------------------------------------

testthat::test_that("integrity checks pass on a clean build", {
  db <- validation_fixture_path()
  testthat::expect_equal(check_fact_keys(db)$status, "pass")
  testthat::expect_match(check_fact_keys(db)$detail, "1 gross/cash-only rows")
  testthat::expect_equal(check_dim_keys(db)$status, "pass")
  testthat::expect_equal(check_bridge(db)$status, "pass")
  testthat::expect_equal(check_row_counts(db)$status, "pass")
  testthat::expect_equal(check_file_ids(db)$status, "pass")
})

testthat::test_that("integrity checks fail on orphans, duplicates, missing CCNs, count gaps, bad ids", {
  orphan <- corrupted_fixture("INSERT INTO fact_rate (file_id, code_id, payer_id, negotiated_dollar, plausible) VALUES (999, 1, NULL, 100, true);")
  testthat::expect_equal(check_fact_keys(orphan)$status, "fail")
  testthat::expect_equal(check_row_counts(orphan)$status, "fail")

  dup <- corrupted_fixture("INSERT INTO dim_hospital (ccn, state) VALUES ('060011', 'CO');")
  result <- check_dim_keys(dup)
  testthat::expect_equal(result$status, "fail")
  testthat::expect_match(result$detail, "ccn = 1")

  bridge <- corrupted_fixture("INSERT INTO bridge_file_ccn (file_id, ccn) VALUES (1, '999999'), (1, (SELECT ccn FROM bridge_file_ccn WHERE file_id = 1));")
  result <- check_bridge(bridge)
  testthat::expect_equal(result$status, "fail")
  testthat::expect_match(result$detail, "1 rows \\(1 distinct CCNs, 28 fact rows\\) point at CCNs not in dim_hospital")
  testthat::expect_match(result$detail, "1 duplicate \\(file, ccn\\) pairs repeat 28 rows")

  counts <- corrupted_fixture("UPDATE dim_file SET n_rows = n_rows + 1 WHERE file_id = 1;")
  testthat::expect_equal(check_row_counts(counts)$metric, 1)

  # the pilot-build failure mode: one byte of a sha256 used as the file id
  ids <- corrupted_fixture("UPDATE dim_file SET mrf_file_id = '02' WHERE file_id = 1;")
  testthat::expect_equal(check_file_ids(ids)$status, "fail")
  testthat::expect_match(check_file_ids(ids)$detail, "\\(02\\)")
})

testthat::test_that("saved medians are compared with a fresh computation", {
  db <- validation_fixture_path()
  out_dir <- base::tempfile("medians_out")
  medians <- base::suppressMessages(compute_state_medians(db, out_dir = out_dir))
  testthat::expect_equal(check_medians_current(db, out_dir)$status, "pass")
  testthat::expect_equal(check_medians_current(db, out_dir, medians = fixture_medians())$status, "pass")

  medians$median_price[[1]] <- medians$median_price[[1]] + 1
  write_csv_atomic(medians, base::file.path(out_dir, "state_insurance_medians.csv"))
  testthat::expect_equal(check_medians_current(db, out_dir)$status, "warn")
  testthat::expect_equal(check_medians_current(db, base::tempfile())$status, "skip")
})

testthat::test_that("the medians check leaves out the files the medians step excluded", {
  db <- validation_fixture_path()
  out_dir <- base::tempfile("medians_excluded")
  # analysis/11 drops the own-crawl duplicate of Denver's Trilliant file
  own_id <- duckdb_query("SELECT mrf_file_id FROM dim_file WHERE source = 'own_crawl'", database = db, read_only = TRUE)$mrf_file_id
  base::suppressMessages(compute_state_medians(db, out_dir = out_dir, exclude_file_ids = own_id))
  # drop a Trilliant file too, so the excluded medians really differ
  aurora_id <- duckdb_query("SELECT mrf_file_id FROM dim_file WHERE hospital_name = 'Aurora Medical'", database = db, read_only = TRUE)$mrf_file_id
  base::suppressMessages(compute_state_medians(db, out_dir = out_dir, exclude_file_ids = base::c(own_id, aurora_id)))

  # without the list, the recomputation keeps every file and disagrees
  testthat::expect_equal(check_medians_current(db, out_dir)$status, "warn")

  write_csv_atomic(tibble::tibble(mrf_file_id = base::c(aurora_id, own_id)), base::file.path(out_dir, "median_excluded_file_ids.csv"))
  testthat::expect_equal(read_median_exclusions(out_dir), base::sort(base::c(aurora_id, own_id)))
  result <- check_medians_current(db, out_dir)
  testthat::expect_equal(result$status, "pass")
  testthat::expect_match(result$detail, "excluding 2 files")
  # medians built without the exclusions are recomputed rather than reused
  testthat::expect_equal(check_medians_current(db, out_dir, medians = fixture_medians())$status, "pass")
  testthat::expect_null(read_median_exclusions(base::tempfile()))
})

# ---- 2. known answers --------------------------------------------------------

testthat::test_that("known answers reproduce within 1% and pick the Trilliant file", {
  db <- validation_fixture_path()
  result <- check_known_answers(db, fixture_answers())
  testthat::expect_equal(base::nrow(result), 5)
  testthat::expect_true(base::all(result$status == "pass"))
  # the own-crawl copy of the same hospital is never a candidate
  denver_id <- duckdb_query("SELECT file_id FROM dim_file WHERE source = 'trilliant' AND hospital_name LIKE 'Denver%'",
                            database = db, read_only = TRUE)$file_id
  testthat::expect_match(result$detail[[1]], base::paste0("file_id ", denver_id, " .*1 candidate file"))
})

testthat::test_that("known answers fail on the same file version and warn on a different one", {
  db <- validation_fixture_path()
  testthat::expect_true(base::all(check_known_answers(db, fixture_answers(scale = 1.5))$status == "fail"))
  testthat::expect_true(base::all(check_known_answers(db, fixture_answers("2026-01-01", scale = 1.5))$status == "warn"))
  # right values from a different version still pass
  testthat::expect_true(base::all(check_known_answers(db, fixture_answers("2026-01-01"))$status == "pass"))

  missing <- fixture_answers() |> dplyr::mutate(ccn = "999999", hospital_name = "Nowhere")
  testthat::expect_equal(check_known_answers(db, missing)$status, "fail")
  testthat::expect_equal(check_known_answers(db, sources = "own_crawl")$status, "skip")
})

testthat::test_that("known answers load from the private file, or skip when it is absent", {
  missing <- known_answers(base::tempfile(fileext = ".csv"))
  testthat::expect_equal(base::nrow(missing), 0)
  testthat::expect_equal(check_known_answers(validation_fixture_path(), missing)$status, "skip")

  path <- base::file.path(repo_root_path(), "config", "known_answers.csv")
  testthat::skip_if_not(base::file.exists(path), "config/known_answers.csv is kept only in the private repository")
  answers <- known_answers(path)
  testthat::expect_gt(base::nrow(answers), 0)
  testthat::expect_true(base::all(answers$stat %in% base::c("median_negotiated", "max_gross", "max_cash", "n_rows", "n_payers")))
  testthat::expect_false(base::anyNA(answers$expected))
  testthat::expect_true(base::all(stringr::str_detect(answers$ccn, "^[0-9A-Z]{6}$")))
})

# ---- 3. plausibility ---------------------------------------------------------

testthat::test_that("rate plausibility passes clean data and flags gross/cash inversions", {
  clean <- check_rate_plausibility(validation_fixture_path())
  testthat::expect_true(base::all(clean$status == "pass"))

  inverted <- check_rate_plausibility(corrupted_fixture("UPDATE fact_rate SET gross = 10;"))
  testthat::expect_equal(inverted$status[inverted$check_id == "plausibility_negotiated_above_gross"], "warn")
  testthat::expect_match(inverted$detail[inverted$check_id == "plausibility_negotiated_above_gross"], "top files: file [0-9]+ \\(")
  testthat::expect_equal(inverted$status[inverted$check_id == "plausibility_cash_above_gross"], "warn")

  implausible <- check_rate_plausibility(corrupted_fixture("UPDATE fact_rate SET plausible = false, negotiated_dollar = 0;"))
  testthat::expect_equal(implausible$status[implausible$check_id == "plausibility_share_plausible"], "fail")
  testthat::expect_equal(implausible$status[implausible$check_id == "plausibility_nonpositive"], "warn")
})

testthat::test_that("files with most rates above gross are listed for exclusion", {
  clean <- files_above_gross(validation_fixture_path())
  testthat::expect_equal(base::nrow(clean), 0)
  testthat::expect_equal(check_files_above_gross(clean)$status, "pass")

  # file 1's gross becomes a $10 component charge: all 28 of its rates sit above it
  one_file <- corrupted_fixture("UPDATE fact_rate SET gross = 10 WHERE file_id = 1;")
  flagged <- files_above_gross(one_file)
  testthat::expect_equal(flagged$file_id, 1)
  testthat::expect_equal(flagged$n_rates, 28)
  testthat::expect_equal(flagged$share_above, 1)
  testthat::expect_true(base::all(base::c("mrf_file_id", "source", "hospital_name", "mrf_url", "median_ratio") %in% base::names(flagged)))
  result <- check_files_above_gross(flagged)
  testthat::expect_equal(result$status, "warn")
  testthat::expect_match(result$detail, "file 1 .* 28/28 above")

  testthat::expect_equal(base::nrow(files_above_gross(one_file, min_rates = 29L)), 0)
  # half the rows above is not "more than half"
  half <- corrupted_fixture("UPDATE fact_rate SET gross = 10 WHERE file_id = 1 AND payer_id IN (SELECT payer_id FROM dim_payer WHERE payer_name IN ('Aetna', 'Humana'));")
  testthat::expect_equal(base::nrow(files_above_gross(half)), 0)
})

testthat::test_that("self_pay payer rows are compared with the discounted cash price", {
  us <- function(code, type, price, n) {
    tibble::tibble(state = "US", concept = "x", code = code, anchor = TRUE, fee_type = "facility",
                   insurance_type = type, median_price = price, p25 = NA_real_, p75 = NA_real_, n_hospitals = n, n_rates = n)
  }
  # the CHS pattern: a mean-of-two self_pay median dominated by an OR-case line
  medians <- dplyr::bind_rows(us("58300", "self_pay", 3767.03, 71), us("58300", "self_pay_cash", 326.3, 1235),
                              us("45378", "self_pay", 1500, 40), us("45378", "self_pay_cash", 1200, 900))
  result <- check_self_pay_consistency(medians)
  testthat::expect_equal(result$status, "warn")
  testthat::expect_equal(result$metric, 1)
  testthat::expect_match(result$detail, "58300: self_pay 3,767.03 \\(71 hospitals\\) vs cash 326.30 \\(1235\\), ratio 11.54 OUT OF RANGE")

  testthat::expect_equal(check_self_pay_consistency(medians[3:4, ])$status, "pass")
  testthat::expect_equal(check_self_pay_consistency(medians[0, ])$status, "skip")
})

testthat::test_that("concept and payer ordering pass, warn when violated, skip when absent", {
  medians <- fixture_medians()
  testthat::expect_equal(check_concept_order(medians)$status, "pass")
  testthat::expect_equal(check_payer_order(medians)$status, "pass")

  swapped <- medians |>
    dplyr::mutate(median_price = dplyr::case_when(
      .data$state == "US" & .data$code == "58300" & .data$insurance_type == "commercial" ~ 99999,
      .data$state == "US" & .data$code == "45378" & .data$insurance_type == "medicaid" ~ 99999,
      TRUE ~ .data$median_price
    ))
  concept <- check_concept_order(swapped)
  testthat::expect_equal(concept$status, "warn")
  testthat::expect_match(concept$detail, "iud_insertion < colonoscopy: 99,999.00 vs .* VIOLATED")
  testthat::expect_equal(check_payer_order(swapped)$status, "warn")

  testthat::expect_equal(check_concept_order(medians[0, ])$status, "skip")
})

# ---- 4. payer type -----------------------------------------------------------

testthat::test_that("Trilliant payer labels map onto our types", {
  testthat::expect_equal(
    map_trilliant_payer_type(base::c("Commercial", "Medicare Advantage", " medicaid ", "Other", NA, "Mystery")),
    base::c("commercial", "medicare_advantage", "medicaid", "other", NA, "unmapped: mystery")
  )
})

testthat::test_that("payer-type agreement passes when labels agree and fails when they don't", {
  payers <- payer_type_table(validation_fixture_path(), test_rules())
  result <- check_payer_type_agreement(payers)
  testthat::expect_equal(result$status, "pass")
  testthat::expect_equal(result$metric, 1)
  testthat::expect_equal(check_payer_type_current(payers)$status, "pass")

  # every label Medicaid: the three plans that name their type overrule it, Aetna PPO can't
  wrong <- payer_type_table(corrupted_fixture("UPDATE dim_payer SET trilliant_payer_type = 'Medicaid';"), test_rules())
  result <- check_payer_type_agreement(wrong)
  testthat::expect_equal(result$status, "fail")
  testthat::expect_equal(result$metric, 0.75)
  testthat::expect_match(result$detail, "raw vs Trilliant labels 25.0%")

  # a classifier that types everything "other" fails outright
  broken <- payer_type_table(validation_fixture_path(), tibble::tibble(priority = 1L, payer_type = "other", pattern = ".", note = "x"))
  testthat::expect_equal(check_payer_type_agreement(broken)$metric, 0)
  confusion <- payer_type_confusion(wrong)
  testthat::expect_equal(base::sum(confusion$n_rows[confusion$current_type == "commercial" & confusion$trilliant_type == "medicaid"]), 28)
  testthat::expect_true(base::all(payer_type_disagreements(wrong)$trilliant_type == "medicaid"))

  unlabelled <- payer_type_table(corrupted_fixture("UPDATE dim_payer SET trilliant_payer_type = NULL;"), test_rules())
  testthat::expect_equal(check_payer_type_agreement(unlabelled)$status, "skip")
})

#' One row per payer_type_rules.csv change, with the string that motivated it
payer_rule_cases <- function() {
  tibble::tribble(
    ~payer_name,                         ~plan_name,       ~expected,            ~why,
    # "dual" matched inside "individual"
    "Ambetter",                          "Individual",     "exchange",           "dual needs word boundaries",
    "Cigna Individual and Family Plans", NA,               "commercial",         "dual needs word boundaries",
    "UHC Dual Complete",                 NA,               "medicare_advantage", "a real D-SNP still matches",
    "Wellcare",                          "Duals HMO",      "medicare_advantage", "plural duals",
    # "VA" inside a longer name is Virginia
    "Anthem BCBS VA",                    "PPO",            "commercial",         "VA as a state",
    "Sentara Health Plans VA",           NA,               "commercial",         "VA as a state",
    "VA",                                NA,               "tricare_va",         "bare VA",
    "VA Community Care Network",         NA,               "tricare_va",         "VA program name",
    "TriWest",                           "VA CCN",         "tricare_va",         "VA program name",
    # Medigap is paid at Medicare's rate
    "BCBS Medicare Supplement",          NA,               "medicare",           "Medigap before MA",
    "Aetna",                             "Medigap Plan G", "medicare",           "Medigap before MA",
    # Centene exchange plans named after Medicaid plans
    "Ambetter from Sunshine Health",     NA,               "exchange",           "exchange before Medicaid",
    "Ambetter from Superior HealthPlan", NA,               "exchange",           "exchange before Medicaid",
    "Superior HealthPlan",               "STAR",           "medicaid",           "the Medicaid plan itself",
    # carrier Medicaid brands
    "Aetna Better Health of Texas",      NA,               "medicaid",           "carrier Medicaid brand",
    "UnitedHealthcare Community Plan",   NA,               "medicaid",           "carrier Medicaid brand",
    "Humana Healthy Horizons",           "Florida",        "medicaid",           "carrier Medicaid brand",
    "AmeriHealth Caritas",               "Pennsylvania",   "medicaid",           "Medicaid MCO",
    "Molina Healthcare of Texas",        NA,               "medicaid",           "Molina Medicaid",
    "Molina Healthcare",                 "Medicare",       "medicare_advantage", "Molina Medicare stays MA",
    "Molina Healthcare",                 "Marketplace",    "exchange",           "Molina Marketplace stays exchange",
    "Virginia Premier",                  NA,               "medicaid",           "Medicaid MCO",
    # ConnectiCare (Molina-owned since 2025) is commercial; Connecticut Medicaid has no MCOs
    "MOLINA dba CONNECTICARE",           "MOLINA MANAGED CARE", "commercial",  "ConnectiCare before the Molina Medicaid rule",
    "ConnectiCare",                      "Medicare Advantage", "medicare_advantage", "ConnectiCare MA stays MA",
    "ConnectiCare Medicare",             NA,               "medicare_advantage", "ConnectiCare on the MA carrier list",
    "ConnectiCare",                      "Exchange",       "exchange",           "ConnectiCare exchange stays exchange",
    "Buckeye Commercial",                "Buckeye Commercial", "commercial",   "commercial product under a Medicaid brand",
    "Buckeye Health Plan",               "Ohio Medicaid",  "medicaid",           "Buckeye Medicaid unaffected",
    # pilot own-crawl strings (2026-09-12)
    "Optum VA",                          "Optum VA",       "tricare_va",         "Optum runs VA CCN regions 1-3",
    "UCARE",                             "Dually Eligible", "medicare_advantage", "dual-eligible SNP",
    "UCare Minnesota",                   "Individual and Family Plans", "commercial", "ACA individual, not dual",
    "Wellcare and Wellcare by Allwell",  "all",            "medicare_advantage", "Centene MA brand",
    "WellCare of Kentucky",              NA,               "medicaid",           "Wellcare Medicaid unaffected",
    "AR Total Care (PASSE)",             "all",            "medicaid",           "Arkansas Medicaid PASSE",
    "MDWISE HIP [6003]",                 "MDWISE EXCEL HIP PLUS [600309]", "medicaid", "Healthy Indiana Plan",
    "Health Net",                        "Salud/Enhance",  "commercial",         "Health Net commercial HMO",
    "Health Net Community Solutions",    "Medi-Cal",       "medicaid",           "Health Net Medi-Cal",
    "Health Net",                        "Seniority Plus", "medicare_advantage", "Health Net MA",
    # full Trilliant lake (2026-07-21), biggest disagreement cells
    "United Healthcare",                 "Medicare Advantage", "medicare_advantage", "Trilliant says Commercial; text says MA",
    "Humana Military",                   "TRICARE",        "tricare_va",         "Trilliant says Commercial",
    "Anthem BCBS",                       "Anthem IN Work Comp", "workers_comp",  "Trilliant says Commercial",
    "Medical Mutual Medicare",           "Medicare",       "medicare_advantage", "medi-cal matched inside medical",
    "Medi-Cal Blue Cross",               "Medicaid",       "medicaid",           "medi-cal still matches",
    "MEDICARE_PART_B_CLAIMS",            "PARTBMCR",       "medicare",           "medica matched inside medicare",
    "Traditional_Medicare",              "Outpatient",     "medicare",           "underscores around medicare",
    "Medica",                            "Choice",         "commercial",         "Medica the carrier",
    "CareSource Medicare",               "Medicare",       "medicare_advantage", "carrier list extended",
    "CareSource",                        "Ohio Medicaid",  "medicaid",           "CareSource Medicaid unaffected",
    "Harvard Pilgrim Stride Medicare",   "Medicare",       "medicare_advantage", "carrier list extended",
    "Harvard Pilgrim",                   "HMO",            "commercial",         "Harvard Pilgrim commercial",
    "Fallon",                            "MCRManaged Fallon Medicare SCO", "medicare_advantage", "carrier list extended",
    "Fallon Health",                     "Fallon DirectCare", "commercial",      "Fallon commercial",
    "Avmed_Medicare_Choice",             "MCRAVMED",       "medicare_advantage", "carrier next to medicare",
    "Humana",                            "Gold Plus HMO",  "medicare_advantage", "Humana Gold Plus is MA",
    "Molina",                            "HIX",            "exchange",           "HIX is the exchange",
    "Molina",                            "MCR",            "medicare_advantage", "MCR is Medicare",
    "Molina",                            "MGMCR",          "medicare_advantage", "managed Medicare",
    "Molina",                            "Chip KM",        "medicaid",           "Molina CHIP",
    "Molina Essential [100167]",         "Molina Essential Plan 1 and 2 [10016701]", "exchange", "NY Essential Plan",
    "Affinity Health Plan Medicaid Advantage", "Medicaid", "medicaid",           "NY Medicaid Advantage",
    "Allwell Medicaid Advantage",        "Medicaid",       "medicaid",           "Medicaid Advantage before Allwell",
    "Paramount Advantage",               NA,               "medicaid",           "Ohio Medicaid",
    "BCBS",                              "BlueAdvantageHMO", "commercial",       "BCBS Texas commercial HMO",
    "Blue Cross Blue Shield of Texas",   "Blue Advantage HMO", "commercial",     "BCBS Texas commercial HMO",
    "BLUE ADVANTAGE MEDICARE [9003]",    "HEALTHY BLUE DUAL ADVANTAGE (HMO D-SNP) [900301]", "medicare_advantage", "Louisiana Blue Advantage MA",
    "BlueAdvantage",                     "Medicare Advantage", "medicare_advantage", "Blue Advantage named as MA",
    "Blue Cross",                        "Blue Advantage MMAI", "medicare_advantage", "Illinois Medicare-Medicaid plan",
    "Kaiser",                            "Senior Advantage", "medicare_advantage", "Kaiser MA",
    # 58300 self-pay review (2026-09-13)
    "COVID19 HRSA UNINSURED TESTING AND TREATMENT FUND [1179012]", "COVID19 HRSA UNINSURED TESTING AND TREATMENT FUND [117901201]", "other", "federal program, not self-pay",
    "Uninsured",                         NA,               "self_pay",           "plain uninsured stays self-pay",
    # unchanged anchors
    "Aetna",                             "PPO",            "commercial",         "unchanged",
    "Medicare",                          NA,               "medicare",           "unchanged",
    "Humana",                            "Medicare Advantage HMO", "medicare_advantage", "unchanged"
  )
}

testthat::test_that("payer-type rule changes classify their motivating strings", {
  cases <- payer_rule_cases()
  got <- classify_payer_type(cases$payer_name, cases$plan_name, rules = test_rules())
  wrong <- cases[got != cases$expected, ] |> dplyr::mutate(got = got[got != cases$expected])
  testthat::expect_equal(base::nrow(wrong), 0, info = base::paste(wrong$payer_name, wrong$plan_name, wrong$got, collapse = "; "))
})

testthat::test_that("the R and DuckDB classifiers agree on the rule cases", {
  cases <- payer_rule_cases()
  values <- base::paste0("(", base::seq_len(base::nrow(cases)), ", ", sql_string(cases$payer_name), ", ",
                         base::ifelse(base::is.na(cases$plan_name), "NULL", sql_string(cases$plan_name)), ")", collapse = ", ")
  sql_types <- duckdb_query(base::paste0(
    "SELECT i, ", payer_type_case_sql(payer_text_sql(), test_rules()), " AS payer_type ",
    "FROM (VALUES ", values, ") AS t(i, payer_name, plan_name) ORDER BY i"
  ))$payer_type
  testthat::expect_equal(sql_types, classify_payer_type(cases$payer_name, cases$plan_name, rules = test_rules()))
})

testthat::test_that("plan text that names the type overrides a carrier-based Trilliant label", {
  testthat::expect_equal(
    explicit_payer_type(base::c("humana medicare advantage hmo", "united healthcare managed medicaid", "humana military tricare",
                                "aetna ppo", "medicare", "bcbs medicare supplement", NA)),
    base::c("medicare_advantage", "medicaid", "tricare_va", NA, "medicare", "medicare", NA)
  )
  testthat::expect_equal(comparable_payer_type(base::c("exchange", "medicaid")), base::c("commercial", "medicaid"))

  # Trilliant calls the Humana MA plan Commercial (as the lake does for UHC MA)
  mislabelled <- payer_type_table(corrupted_fixture("UPDATE dim_payer SET trilliant_payer_type = 'Commercial' WHERE payer_name = 'Humana';"), test_rules())
  result <- check_payer_type_agreement(mislabelled)
  testthat::expect_equal(result$metric, 1)
  testthat::expect_equal(result$status, "pass")
  testthat::expect_match(result$detail, "raw vs Trilliant labels 75.0%")
  testthat::expect_match(result$detail, "contradicts Trilliant on 28 rows")
  testthat::expect_equal(payer_type_disagreements(mislabelled)$reference_type, "medicare_advantage")
})

testthat::test_that("a rule change shows up as a stale dim_payer", {
  rules <- dplyr::bind_rows(
    tibble::tibble(priority = 0L, payer_type = "medicaid", pattern = "aetna", note = "test"),
    test_rules()
  )
  result <- check_payer_type_current(payer_type_table(validation_fixture_path(), rules))
  testthat::expect_equal(result$status, "warn")
  testthat::expect_match(result$detail, "1 payer/plan strings")
})

# ---- 5. cross-source ---------------------------------------------------------

testthat::test_that("cross-source agreement matches files by normalized URL", {
  db <- validation_fixture_path()
  pairs <- cross_source_pairs(db)
  testthat::expect_equal(base::nrow(pairs), 1)
  testthat::expect_true(pairs$same_version)

  result <- check_cross_source(db)
  testthat::expect_equal(result$status, "pass")
  testthat::expect_equal(result$metric, 1)

  # Trilliant keeps JSON quotes around payer names ("\"Aetna\""); the key ignores them
  quoted <- corrupted_fixture(base::c(
    base::paste(
      "INSERT INTO dim_payer (payer_id, payer_key, payer_name, plan_name, payer_type, trilliant_payer_type)",
      "SELECT payer_id + 1000, q_name || chr(31) || q_plan, q_name, q_plan, payer_type, trilliant_payer_type",
      "FROM (SELECT *, '\"' || payer_name || '\"' AS q_name, '\"' || plan_name || ' \"' AS q_plan FROM dim_payer);"
    ),
    "UPDATE fact_rate SET payer_id = payer_id + 1000 WHERE file_id = (SELECT file_id FROM dim_file WHERE source = 'trilliant' AND hospital_name LIKE 'Denver%');"
  ))
  testthat::expect_equal(check_cross_source(quoted)$metric, 1)

  drifted <- corrupted_fixture("UPDATE fact_rate SET negotiated_dollar = negotiated_dollar * 1.5 WHERE file_id = (SELECT file_id FROM dim_file WHERE source = 'own_crawl');")
  testthat::expect_equal(check_cross_source(drifted)$status, "fail")
  testthat::expect_equal(check_cross_source(db, sources = "trilliant")$status, "skip")
})

testthat::test_that("files served from one script endpoint pair only on their own query string", {
  # normalize_url_key() keeps the query of script endpoints (download.aspx?id=...)
  shared <- corrupted_fixture(base::c(
    "UPDATE dim_file SET mrf_url = 'https://x.org/download.aspx?id=2' WHERE source = 'own_crawl' OR (source = 'trilliant' AND hospital_name LIKE 'Denver%');",
    "UPDATE dim_file SET mrf_url = 'https://x.org/download.aspx?id=1' WHERE hospital_name = 'Aurora Medical';"
  ))
  pairs <- cross_source_pairs(shared)
  denver_id <- duckdb_query("SELECT file_id FROM dim_file WHERE source = 'trilliant' AND hospital_name LIKE 'Denver%'",
                            database = shared, read_only = TRUE)$file_id
  testthat::expect_equal(base::nrow(pairs), 1)
  testthat::expect_equal(pairs$tri_id, denver_id)
  testthat::expect_equal(check_cross_source(shared)$metric, 1)
})

# ---- 6. OPPS benchmark -------------------------------------------------------

testthat::test_that("Addendum B parses past its preamble and money formatting", {
  rates <- fixture_opps()
  testthat::expect_equal(rates$payment_rate[rates$code == "45378"], 950.10)
  testthat::expect_equal(rates$payment_rate[rates$code == "45380"], 1222.56)
  testthat::expect_true(base::is.na(rates$payment_rate[rates$code == "58300"]))
  testthat::expect_equal(rates$status_indicator[rates$code == "58300"], "E1")
  testthat::expect_false("HCPCS Code" %in% rates$code)
})

testthat::test_that("OPPS benchmark passes in range, warns outside, skips unpaid codes", {
  medians <- fixture_medians()
  result <- check_opps_benchmark(medians, fixture_opps())
  testthat::expect_equal(result$status[result$check_id == "benchmark_opps_45378"], "pass")
  # national medicare median = median of hospital medians 0.8 x base x (1, 1.1, 0.9)
  testthat::expect_equal(result$metric[result$check_id == "benchmark_opps_45378"], 0.8 * validation_base_rates()[["45378"]] / 950.10, tolerance = 1e-6)
  testthat::expect_equal(result$status[result$check_id == "benchmark_opps_58300"], "skip")
  testthat::expect_match(result$detail[result$check_id == "benchmark_opps_58300"], "status indicator E1")

  cheap <- fixture_opps() |> dplyr::mutate(payment_rate = dplyr::if_else(.data$code == "45378", 100, .data$payment_rate))
  testthat::expect_equal(check_opps_benchmark(medians, cheap)$status[[1]], "warn")
  testthat::expect_equal(check_opps_benchmark(medians, NULL)$status, "skip")
})

# ---- 7. raw re-check ---------------------------------------------------------

build_recheck_fixture <- function(mrf_file_id = NULL) {
  dir <- base::tempfile("recheck_db")
  base::dir.create(dir)
  mrf_path <- fixture_path("mrf", "v3_tall.csv")
  url <- base::paste0("file://", base::normalizePath(mrf_path))
  sha <- mrf_sha256(mrf_path)
  prices <- base::suppressMessages(parse_csv_mrf(mrf_path, test_codebook(), mrf_url = url, mrf_file_id = sha, retrieved_at = "2026-09-12"))
  prices$source <- "own_crawl"
  if (!base::is.null(mrf_file_id)) prices$mrf_file_id <- mrf_file_id
  prices_path <- base::file.path(dir, "prices.parquet")
  arrow::write_parquet(prices, prices_path)
  crosswalk_path <- base::file.path(dir, "crosswalk.parquet")
  arrow::write_parquet(
    tibble::tibble(mrf_file_id = prices$mrf_file_id[[1]], ccn = "060011", ccn_match_method = "mrf_url",
                   ccn_match_score = 1, ccn_conflict = FALSE, ccn_ambiguous = FALSE),
    crosswalk_path
  )
  universe <- tibble::tibble(facility_id = "060011", facility_name = "Test General", address = "x", citytown = "Denver",
                             state = "CO", zip_code = "80204", hospital_type = "Acute Care Hospitals",
                             hospital_ownership = "Government", health_sys_id = NA_character_, health_sys_name = NA_character_)
  db_path <- base::file.path(dir, "hpt.duckdb")
  base::suppressMessages(build_hpt_database(prices_path, crosswalk_path, universe, test_codebook(), db_path = db_path))
  base::list(db = db_path, state = tibble::tibble(mrf_file_id = prices$mrf_file_id[[1]], status = "ok", bytes = base::file.size(mrf_path)))
}

testthat::test_that("raw re-check re-parses a file:// MRF and compares rows exactly", {
  fixture <- build_recheck_fixture()
  testthat::expect_equal(check_raw_recheck(fixture$db, network = FALSE, state = fixture$state)$status, "skip")

  result <- base::suppressMessages(check_raw_recheck(fixture$db, network = TRUE, state = fixture$state, codebook = test_codebook()))
  testthat::expect_equal(result$status, "pass")
  testthat::expect_match(result$detail, "match")

  tampered <- base::tempfile(fileext = ".duckdb")
  base::file.copy(fixture$db, tampered)
  run_duckdb_sql("UPDATE fact_rate SET gross = gross + 1;", database = tampered)
  result <- base::suppressMessages(check_raw_recheck(tampered, network = TRUE, state = fixture$state, codebook = test_codebook()))
  testthat::expect_equal(result$status, "fail")
  testthat::expect_match(result$detail, "mismatch")

  changed <- build_recheck_fixture(mrf_file_id = base::strrep("a", 64))
  result <- base::suppressMessages(check_raw_recheck(changed$db, network = TRUE, state = changed$state, codebook = test_codebook()))
  testthat::expect_equal(result$status, "warn")
  testthat::expect_match(result$detail, "changed_upstream")
})

testthat::test_that("compare_rate_rows counts rows on each side as a multiset", {
  a <- tibble::tibble(code = "58100", payer_name = "Aetna", plan_name = NA, description = "EMB",
                      negotiated_dollar = base::c(100, 100), gross = 200, discounted_cash = NA)
  testthat::expect_true(compare_rate_rows(a, a)$identical)
  diff <- compare_rate_rows(a, a[1, ])
  testthat::expect_false(diff$identical)
  testthat::expect_equal(diff$only_db, 1)
})

# ---- 8. coverage -------------------------------------------------------------

testthat::test_that("coverage by state counts covered roster CCNs", {
  coverage <- coverage_by_state(validation_fixture_path())
  testthat::expect_equal(coverage$covered_ccns[coverage$state == "CO"], 3)
  testthat::expect_equal(coverage$covered_ccns[coverage$state == "TX"], 0)
  testthat::expect_equal(check_coverage(coverage)$status, "warn")          # 3 of 5
  testthat::expect_equal(check_coverage(coverage, pass_at = 0.5)$status, "pass")
})

testthat::test_that("rates that can't be placed in a state are counted", {
  testthat::expect_equal(check_rates_without_state(validation_fixture_path())$status, "pass")

  # an unbridged file with no license_state, and a CCN missing from the roster
  lost <- corrupted_fixture(base::c(
    "DELETE FROM bridge_file_ccn WHERE file_id = 1;",
    "UPDATE dim_file SET license_state = NULL WHERE file_id = 1;",
    "UPDATE bridge_file_ccn SET ccn = '999999' WHERE file_id = 2;",
    "UPDATE dim_file SET license_state = NULL WHERE file_id = 2;"
  ))
  result <- check_rates_without_state(lost)
  testthat::expect_equal(result$status, "warn")
  testthat::expect_match(result$detail, "in 2 files: 28 from files with no CCN and no state, 2[89] bridged to CCNs missing")

  # an unbridged file with a blank state (the MSK files, 2026-09-13): the build turns
  # '' into NULL, so it is reported as a rate with no state, never a median group
  blank <- corrupted_fixture(base::c("DELETE FROM bridge_file_ccn WHERE file_id = 1;", "UPDATE dim_file SET license_state = '   ' WHERE file_id = 1;"))
  result <- check_rates_without_state(blank)
  testthat::expect_equal(result$status, "warn")
  testthat::expect_match(result$detail, "^28 of .* with no state in 1 files: 28 from files with no CCN and no state")
  testthat::expect_match(result$detail, "0 rows in 0 files have a blank state")

  medians <- fixture_medians(blank)
  testthat::expect_false(base::any(medians$state == "", na.rm = TRUE))
  out_dir <- base::tempfile("blank_state")
  base::suppressMessages(compute_state_medians(blank, out_dir = out_dir))
  testthat::expect_equal(check_medians_current(blank, out_dir)$status, "pass")
})

testthat::test_that("thin states are listed and the headline suppresses them", {
  medians <- fixture_medians()
  thin <- thin_state_cells(medians)
  testthat::expect_equal(thin$n_states_publishable[thin$code == "45378" & thin$insurance_type == "commercial"], 1)

  states <- check_thin_states(medians, pass_states = 1L)
  testthat::expect_true(base::all(states$status[states$check_id != "coverage_states_621"] == "pass"))
  testthat::expect_equal(states$status[states$check_id == "coverage_states_621"], "skip")      # no DRG 621 in the fixture
  testthat::expect_equal(check_thin_states(medians)$status[[1]], "warn")

  testthat::expect_equal(check_headline_suppression(medians)$status, "pass")

  # the headline's MS-DRG 621 column (bariatric_ms_drg_621) is checked like the CPT columns
  with_drg <- dplyr::bind_rows(medians, medians |> dplyr::filter(.data$code == "43775") |>
                                 dplyr::mutate(code = "621", concept = "drg_bariatric", median_price = .data$median_price * 5))
  testthat::expect_true("bariatric_ms_drg_621" %in% base::names(state_median_headline(with_drg)))
  base_cells <- base::as.integer(stringr::str_match(check_headline_suppression(medians)$detail, "^([0-9]+) headline cells")[, 2])
  drg_result <- check_headline_suppression(with_drg)
  testthat::expect_equal(drg_result$status, "pass")
  testthat::expect_gt(base::as.integer(stringr::str_match(drg_result$detail, "^([0-9]+) headline cells")[, 2]), base_cells)
  testthat::expect_equal(check_thin_states(with_drg, pass_states = 1L)$status[[6]], "pass")
  # a lone hospital's median is thin under min_hospitals = 3 but publishable at 1
  lone <- medians |> dplyr::mutate(n_hospitals = dplyr::if_else(.data$state == "CO", 1L, base::as.integer(.data$n_hospitals)))
  leaky <- function(m, min_hospitals) state_median_headline(m, min_hospitals = 1L)
  result <- check_headline_suppression(lone, headline_fn = leaky)
  testthat::expect_equal(result$status, "fail")
  testthat::expect_match(result$detail, "thin cells leaked")
})

# ---- runner ------------------------------------------------------------------

testthat::test_that("run_validation writes the CSV and markdown report", {
  out_dir <- base::tempfile("validation_out")
  report <- base::suppressMessages(run_validation(
    validation_fixture_path(), out_dir = out_dir,
    opps_path = fixture_path("validation", "opps_addendum_b_sample.csv"),
    answers = fixture_answers(), rules = test_rules(), state = tibble::tibble(), codebook = test_codebook()
  ))

  testthat::expect_named(report, base::c("check_id", "category", "description", "status", "metric", "threshold", "detail"))
  testthat::expect_true(base::all(report$status %in% validation_statuses()))
  testthat::expect_false(base::any(base::duplicated(report$check_id)))
  testthat::expect_setequal(
    base::unique(report$category),
    base::c("integrity", "known_answer", "plausibility", "payer_type", "cross_source", "benchmark", "raw_recheck", "coverage", "ntsv")
  )
  testthat::expect_false(base::any(report$status == "fail"))

  testthat::expect_true(base::file.exists(base::file.path(out_dir, "validation_report.csv")))
  testthat::expect_true(base::file.exists(base::file.path(out_dir, "validation_payer_confusion.csv")))
  # written even with no flagged files, with its header, for the medians step
  above <- readr::read_csv(base::file.path(out_dir, "files_rates_above_gross.csv"), show_col_types = FALSE)
  testthat::expect_equal(base::nrow(above), 0)
  testthat::expect_true(base::all(base::c("file_id", "mrf_file_id", "n_rates", "share_above", "median_ratio") %in% base::names(above)))
  md <- base::readLines(base::file.path(out_dir, "validation_report.md"), encoding = "UTF-8")
  testthat::expect_true(base::any(stringr::str_detect(md, "^# hpt.duckdb validation")))
  testthat::expect_false(base::any(stringr::str_detect(md, "\u2014")))
})

testthat::test_that("a check that errors becomes a fail row instead of stopping the run", {
  row <- run_check_safely("boom", "integrity", "boom", function() base::stop("kaput"))
  testthat::expect_equal(row$status, "fail")
  testthat::expect_match(row$detail, "kaput")
})

testthat::test_that("the median recomputation check catches a median that drifted", {
  db <- validation_fixture_path()
  medians <- validation_medians(db)
  testthat::expect_gt(base::nrow(medians), 0)

  honest <- check_median_recomputation(db, medians, code = medians$code[[1]],
                                       payer = medians$insurance_type[[1]])
  testthat::expect_true(honest$status %in% base::c("pass", "skip"))

  if (honest$status == "pass") {
    # a published median that no longer matches the database must fail, which
    # is the whole point: the check reads the database, not the file it is
    # comparing against
    drifted <- medians
    row <- drifted$state == "US" & drifted$code == medians$code[[1]] &
      drifted$insurance_type == medians$insurance_type[[1]] & drifted$fee_type == "facility"
    if (base::any(row)) {
      drifted$median_price[row] <- drifted$median_price[row] * 1.10
      bad <- check_median_recomputation(db, drifted, code = medians$code[[1]], payer = medians$insurance_type[[1]])
      testthat::expect_equal(bad$status, "fail")
      testthat::expect_gt(bad$metric, 0.05)
    }

    # a hospital count that drifted fails too, even when the median agrees
    miscounted <- medians
    if (base::any(row)) {
      miscounted$n_hospitals[row] <- miscounted$n_hospitals[row] + 1L
      off <- check_median_recomputation(db, miscounted, code = medians$code[[1]], payer = medians$insurance_type[[1]])
      testthat::expect_equal(off$status, "fail")
    }
  }

  # a cell the build never published is skipped, not failed
  absent <- check_median_recomputation(db, medians, code = "00000", payer = "commercial")
  testthat::expect_equal(absent$status, "skip")
})
