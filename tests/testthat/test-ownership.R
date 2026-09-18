#' Ownership classification (CMS Hospital All Owners), hospital-level prices,
#' and the ownership price model. fixtures/ownership/ carries the verbatim
#' headers of the live 2026-08 Hospital All Owners and Hospital Enrollments
#' files with synthetic rows.

if (!base::exists("classify_hospital_ownership", mode = "function")) {
  base::source(base::file.path(repo_root_path(), "R", "ownership.R"))
}

ownership_fixture <- function(...) {
  fixture_path("ownership", ...)
}

ownership_roster <- function() {
  tibble::tibble(
    ccn = base::c("060011", "060024", "060030", "060040", "060050", "450097", "060060"),
    facility_name = base::c("Alpha", "Beta", "Gamma", "Delta", "Epsilon", "Zeta", "Eta"),
    state = base::c("CO", "CO", "CO", "CO", "CO", "TX", "CO"),
    hospital_type = "Acute Care Hospitals",
    hospital_ownership = base::c("Proprietary", "Proprietary", "Voluntary non-profit - Private", "Physician",
                                 "Proprietary", "Voluntary non-profit - Church", "Government - Local"),
    health_sys_id = base::c("S1", "S1", NA, NA, NA, "S2", NA),
    health_sys_name = base::c("Acme", "Acme", NA, NA, NA, "Zeta Health", NA)
  )
}

ownership_inputs <- function() {
  base::list(
    owners = load_hospital_owners(ownership_fixture("hospital_all_owners.csv")),
    enrollments = load_hospital_enrollments(ownership_fixture("hospital_enrollments.csv"))$enrollments
  )
}

testthat::test_that("All Owners loads with normalized names and upper-case owner names", {
  owners <- ownership_inputs()$owners
  testthat::expect_true(base::all(owners_required_columns() %in% base::names(owners)))
  testthat::expect_true("ACME HEALTH PARTNERS LP" %in% owners$organization_name_owner)
  testthat::expect_setequal(base::unique(owners$role_code_owner), base::c("34", "35", "37", "41", "43"))
})

testthat::test_that("Hospital All Owners release comes from the catalog, with one provenance file per download", {
  withr::local_envvar(HPT_PER_HOST_RPS = "1000")
  owners_url <- "https://data.cms.gov/sites/default/files/2026-08/aug/Hospital_All_Owners_2026.07.31.csv"
  dictionary_url <- "https://data.cms.gov/sites/default/files/2026-07/Hospital_All_Owners_Data_Dictionary.pdf"
  resources_url <- "https://data.cms.gov/data-api/v1/dataset-resources/owners-aug"
  catalog <- base::list(dataset = base::list(
    base::list(title = "Hospice All Owners", distribution = base::list()),
    base::list(title = "Hospital All Owners", distribution = base::list(
      base::list(format = "API", description = "latest", title = "Hospital All Owners : 2026-08-01",
                 temporal = "2026-08-01/2026-08-31", modified = "2026-08-19"),
      base::list(format = "CSV", title = "Hospital All Owners : 2026-08-01", temporal = "2026-08-01/2026-08-31",
                 modified = "2026-08-19", downloadURL = owners_url, resourcesAPI = resources_url)
    ))
  ))
  resources <- base::list(data = base::list(
    base::list(name = "Hospital All Owners Aug 2026", fileSize = 100, downloadURL = owners_url),
    base::list(name = "Hospital All Owners Data Dictionary (Current)", fileSize = 100, downloadURL = dictionary_url)
  ))
  path <- ownership_fixture("hospital_all_owners.csv")
  routes <- base::list()
  routes[["https://data.cms.gov/data.json"]] <- base::charToRaw(base::as.character(jsonlite::toJSON(catalog, auto_unbox = TRUE)))
  routes[[resources_url]] <- base::charToRaw(base::as.character(jsonlite::toJSON(resources, auto_unbox = TRUE)))
  routes[[owners_url]] <- base::readBin(path, "raw", base::file.size(path))
  routes[[dictionary_url]] <- base::charToRaw("%PDF-1.4")
  httr2::local_mocked_responses(function(req) {
    body <- routes[[req$url]]
    if (base::is.null(body)) {
      return(httr2::response(status_code = 404L, url = req$url, body = base::charToRaw("not found")))
    }
    type <- if (stringr::str_detect(req$url, "\\.(csv|pdf)$")) "application/octet-stream" else "application/json"
    httr2::response(status_code = 200L, url = req$url, headers = base::list("Content-Type" = type), body = body)
  })

  directory <- withr::local_tempdir()
  downloaded <- base::suppressMessages(download_hospital_owners(directory))
  testthat::expect_equal(downloaded$provenance$file_role, base::c("owners", "dictionary"))
  testthat::expect_equal(downloaded$provenance$release_title[[1]], "Hospital All Owners : 2026-08-01")
  testthat::expect_equal(downloaded$provenance$sha256[[1]], sha256_file(path))
  testthat::expect_equal(base::nrow(downloaded$owners), 9L)

  base::suppressMessages(download_hospital_owners(directory))
  testthat::expect_length(base::list.files(directory, "^hospital_all_owners_provenance_"), 1L)
})

testthat::test_that("classification separates direct, indirect, any-role, share, and fund-name PE", {
  inputs <- ownership_inputs()
  classified <- classify_hospital_ownership(inputs$owners, inputs$enrollments, ownership_roster(), extra_pe_ccns = "60060")
  row <- function(ccn) dplyr::filter(classified, .data$ccn == !!ccn)

  testthat::expect_equal(base::nrow(classified), 7)

  # "60011" lost its leading zero and the PE owner sits on the psych-unit
  # enrollment 06S011: both land on 060011
  alpha <- row("060011")
  testthat::expect_true(alpha$pe_cms)
  testthat::expect_true(alpha$pe_cms_indirect)
  testthat::expect_false(alpha$pe_cms_direct)
  testthat::expect_equal(alpha$pe_cms_max_pct, 60)
  testthat::expect_equal(alpha$ownership_group_cms_flag, "pe")
  testthat::expect_equal(alpha$pe_owners, "ACME HEALTH PARTNERS LP")

  beta <- row("060024")
  testthat::expect_true(beta$pe_cms_direct)
  testthat::expect_false(beta$pe_cms_indirect)

  # a PE-flagged security interest counts only in the any-role sensitivity
  gamma <- row("060030")
  testthat::expect_false(gamma$pe_cms)
  testthat::expect_true(gamma$pe_cms_any_role)
  testthat::expect_equal(gamma$ownership_group_cms_flag, "nonprofit")
  testthat::expect_equal(gamma$ownership_group_cms_flag_any_role, "pe")

  # a 3% PE share is PE under the primary definition, not the >= 5% one
  delta <- row("060040")
  testthat::expect_true(delta$pe_cms)
  testthat::expect_false(delta$pe_cms_5pct)
  testthat::expect_equal(delta$ownership_group_cms_flag_5pct, "for_profit_non_pe")

  # an unflagged fund vehicle is PE only under "fund_name"
  epsilon <- row("060050")
  testthat::expect_false(epsilon$pe_cms)
  testthat::expect_true(epsilon$pe_fund)
  testthat::expect_equal(epsilon$ownership_group_cms_flag, "for_profit_non_pe")
  testthat::expect_equal(epsilon$ownership_group_fund_name, "pe")

  # Vanguard (investment firm) and physician investors are never PE
  zeta <- row("450097")
  testthat::expect_false(zeta$pe_fund)
  testthat::expect_equal(zeta$ownership_group_fund_name, "nonprofit")
  testthat::expect_equal(zeta$pecos_proprietary_nonprofit, "N")

  # no enrollment: roster group only; the researcher list adds it
  eta <- row("060060")
  testthat::expect_equal(eta$ownership_group_fund_name, "government")
  testthat::expect_equal(eta$ownership_group_researcher_list, "pe")

  testthat::expect_equal(
    roster_ownership_group(base::c("Voluntary non-profit - Other", "Physician", "Tribal", "Veterans Health Administration", NA)),
    base::c("nonprofit", "for_profit_non_pe", "government", "government", "unknown")
  )

  owners_tbl <- largest_pe_owners(inputs$owners, inputs$enrollments, ownership_roster(), priced_ccns = "060011")
  testthat::expect_setequal(owners_tbl$organization_name_owner[owners_tbl$basis == "cms_flag"],
                            base::c("ACME HEALTH PARTNERS LP", "SUMMIT PE HOLDINGS LLC", "MPT OF GAMMA LLC", "SMALLSTAKE HOLDINGS LLC"))
  testthat::expect_equal(owners_tbl$organization_name_owner[owners_tbl$basis == "fund_name"], "ACME CAPITAL PARTNERS FUND II LP")
  testthat::expect_equal(owners_tbl$n_priced_ccns[owners_tbl$organization_name_owner == "ACME HEALTH PARTNERS LP"], 1)

  counts <- ownership_counts(classified, "cms_flag")
  testthat::expect_equal(base::sum(counts$n_hospitals[counts$dimension == "state"]), 7)
})

testthat::test_that("PE system list maps to CCNs by owner name, include/exclude lists, with CHSP as a cross-check", {
  inputs <- ownership_inputs()
  roster <- ownership_roster()
  systems <- load_pe_systems(ownership_fixture("pe_hospital_systems.csv"))
  mapped <- map_pe_systems(systems, owner_pe_rows(inputs$owners, inputs$enrollments), roster)

  # sys_a: owner on the unit enrollment of "60011"; 060024 is CHSP "Acme" only
  testthat::expect_equal(mapped$ccn[mapped$system_id == "sys_a" & mapped$used], "060011")
  testthat::expect_equal(mapped$match_method[mapped$system_id == "sys_a" & !mapped$used], "chsp_only")
  testthat::expect_equal(mapped$ccn[mapped$system_id == "sys_a" & !mapped$used], "060024")
  # sys_d sold Beta: excluded even though the owner still matches
  testthat::expect_false(base::any(mapped$system_id == "sys_d" & mapped$used))
  # include_ccns are normalized; excluded systems are never mapped
  testthat::expect_setequal(mapped$ccn[mapped$system_id == "sys_c" & mapped$used], base::c("060050", "060060", "060011"))
  testthat::expect_equal(mapped$match_method[mapped$system_id == "sys_c" & mapped$ccn == "060060"], "include_ccns")
  testthat::expect_false("sys_e" %in% mapped$system_id)

  assigned <- assign_pe_systems(mapped, systems)
  # strict beats creditor when a CCN matches both
  testthat::expect_equal(assigned$pe_system_id[assigned$ccn == "060011"], "sys_a")
  testthat::expect_equal(assigned$pe_system_ids[assigned$ccn == "060011"], "sys_a;sys_c")

  classified <- classify_hospital_ownership(inputs$owners, inputs$enrollments, roster, pe_systems = assigned, extra_pe_ccns = "060050")
  group <- function(ccn, definition) classified[[base::paste0("ownership_group_", definition)]][classified$ccn == ccn]
  testthat::expect_equal(group("060011", "pe_strict"), "pe")                  # strict and CMS-flagged
  testthat::expect_equal(group("060024", "pe_strict"), "cms_pe_flag")         # CMS flag, sold by its listed system
  testthat::expect_equal(group("060040", "pe_strict"), "cms_pe_flag")         # family office with a CMS PE flag
  testthat::expect_equal(group("060040", "pe_broad"), "pe")
  testthat::expect_equal(group("060050", "pe_strict"), "distressed_fund")
  testthat::expect_equal(group("060050", "pe_broad"), "distressed_fund")
  testthat::expect_equal(group("060050", "pe_broad_all"), "pe")
  testthat::expect_equal(group("060050", "researcher_list"), "pe")
  testthat::expect_equal(group("450097", "pe_broad_all"), "nonprofit")
  testthat::expect_equal(group("060030", "pe_strict"), "nonprofit")

  counts <- pe_system_counts(systems, mapped, assigned, roster, priced_ccns = "060050")
  testthat::expect_equal(counts$n_ccns[counts$system_id == "sys_c"], 3L)
  testthat::expect_equal(counts$n_include_ccns[counts$system_id == "sys_c"], 2L)
  testthat::expect_equal(counts$n_assigned[counts$system_id == "sys_c"], 2L)
  testthat::expect_equal(counts$n_priced[counts$system_id == "sys_c"], 1L)
  testthat::expect_equal(counts$n_chsp_only[counts$system_id == "sys_a"], 1L)
  testthat::expect_equal(counts$n_ccns[counts$system_id == "sys_e"], 0L)

  bad <- base::file.path(withr::local_tempdir(), "bad.csv")
  readr::write_csv(dplyr::mutate(systems, classification = dplyr::if_else(.data$system_id == "sys_a", "maybe", .data$classification)), bad, na = "")
  testthat::expect_error(load_pe_systems(bad), "Unknown classification")
})

testthat::test_that("the shipped PE system list is complete, sourced, and its patterns compile", {
  systems <- load_pe_systems(base::file.path(repo_root_path(), "config", "pe_hospital_systems.csv"))
  listed <- dplyr::filter(systems, .data$classification != "exclude")

  testthat::expect_true(base::all(base::c("lifepoint", "scionhealth", "quorum", "pipeline", "ardent") %in% systems$system_id))
  testthat::expect_true(base::all(base::nzchar(listed$source_quote) & base::nzchar(listed$source_date)))
  testthat::expect_true(base::all(listed$ambiguity[listed$classification == "ambiguous"] != ""))
  testthat::expect_false(base::any(stringr::str_detect(base::unlist(systems), base::intToUtf8(8212)), na.rm = TRUE))
  for (pattern in listed$owner_pattern[base::nzchar(listed$owner_pattern)]) {
    testthat::expect_silent(stringr::str_detect("TEST OWNER LLC", pattern))
  }
})

ownership_price_fixture <- function() {
  row <- function(file, code, concept, payer, plan, dollar, setting = "outpatient", billing = NA, cash = NA, desc = "COLONOSCOPY") {
    tibble::tibble(mrf_file_id = file, code = code, concept = concept, payer_name = payer, plan_name = plan,
                   negotiated_dollar = dollar, setting = setting, billing_class = billing, discounted_cash = cash, description = desc)
  }

  dplyr::bind_rows(
    # Alpha (060011, CO): Aetna repeats one contract on two charge lines
    row("f1", "45378", "colonoscopy", "Aetna", "PPO", 1000, cash = 1200, desc = "COLONOSCOPY DX"),
    row("f1", "45378", "colonoscopy", "Aetna", "PPO", 1200, cash = 1600, desc = "COLONOSCOPY OR"),
    row("f1", "45378", "colonoscopy", "Cigna", "OAP", 1400, cash = 1200, desc = "COLONOSCOPY DX"),
    row("f1", "45378", "colonoscopy", "Aetna", "PPO", 5000, setting = "inpatient"),
    row("f1", "45378", "colonoscopy", "Aetna", "PPO", 250, billing = "professional"),
    row("f1", "45378", "colonoscopy", "Colorado Medicaid", "", 300),
    # inpatient DRG rows stay
    row("f1", "621", "drg_bariatric", "Aetna", "PPO", 20000, setting = "inpatient", desc = "OBESITY PROCEDURES"),
    # Beta (060024, CO): the $0.01 sentinel is implausible
    row("f2", "45378", "colonoscopy", "UHC", "PPO", 2000),
    row("f2", "45378", "colonoscopy", "Aetna", "PPO", 0.01),
    # Zeta (450097, TX)
    row("f3", "45378", "colonoscopy", "Aetna", "PPO", 1500),
    # unmatched file (NM) and an excluded file (Gamma 060030)
    row("f4", "45378", "colonoscopy", "Aetna", "PPO", 9999),
    row("fx", "45378", "colonoscopy", "Aetna", "PPO", 7777)
  ) |>
    dplyr::mutate(source = "own_crawl", mrf_url = base::paste0("https://x.org/", .data$mrf_file_id, ".csv"),
                  code_system = dplyr::if_else(.data$code == "621", "MS-DRG", "CPT"), type_verified = TRUE, gross = 50000) |>
    conform_price_table()
}

build_ownership_db <- function(dir) {
  prices_path <- base::file.path(dir, "prices.parquet")
  arrow::write_parquet(ownership_price_fixture(), prices_path)
  crosswalk_path <- base::file.path(dir, "crosswalk.parquet")
  arrow::write_parquet(
    tibble::tibble(mrf_file_id = base::c("f1", "f2", "f3", "f4", "fx"), ccn = base::c("060011", "060024", "450097", NA, "060030"),
                   ccn_match_method = "mrf_url", ccn_match_score = 1, ccn_conflict = FALSE, ccn_ambiguous = FALSE,
                   state = base::c("CO", "CO", "TX", "NM", "CO")),
    crosswalk_path
  )
  universe <- ownership_roster() |>
    dplyr::transmute(facility_id = .data$ccn, .data$facility_name, address = "x", citytown = "x", .data$state, zip_code = "00000",
                     .data$hospital_type, .data$hospital_ownership, .data$health_sys_id, .data$health_sys_name)
  db_path <- base::file.path(dir, "hpt.duckdb")
  base::suppressMessages(build_hpt_database(prices_path, crosswalk_path, universe, test_codebook(), db_path = db_path))
  db_path
}

testthat::test_that("hospital prices follow the state-median rules and reproduce the state medians", {
  dir <- base::tempfile("owndb")
  base::dir.create(dir)
  db_path <- build_ownership_db(dir)

  prices <- ownership_hospital_prices(db_path, codes = base::c("45378", "621"), exclude_file_ids = "fx")
  price_of <- function(ccn, code, payer_type) {
    prices$price[prices$ccn == ccn & prices$code == code & prices$payer_type == payer_type]
  }

  # Alpha commercial: Aetna contract median(1000, 1200) = 1100, Cigna 1400 -> 1250;
  # the inpatient and professional rows are gone
  testthat::expect_equal(price_of("060011", "45378", "commercial"), 1250)
  testthat::expect_equal(prices$n_contracts[prices$ccn == "060011" & prices$code == "45378" & prices$payer_type == "commercial"], 2)
  testthat::expect_equal(price_of("060011", "45378", "medicaid"), 300)
  # cash per distinct charge line: (DX 1200, OR 1600) -> 1400
  testthat::expect_equal(price_of("060011", "45378", "self_pay_cash"), 1400)
  testthat::expect_equal(price_of("060011", "621", "commercial"), 20000)
  testthat::expect_equal(price_of("060024", "45378", "commercial"), 2000)
  # unmatched and excluded files never appear
  testthat::expect_false(base::any(base::is.na(prices$ccn)))
  testthat::expect_false("060030" %in% prices$ccn)

  medians <- compute_state_medians(db_path, out_dir = dir, exclude_file_ids = "fx") |>
    dplyr::filter(.data$fee_type == "facility", .data$state %in% base::c("CO", "TX"), .data$insurance_type %in% ownership_payer_types())
  rebuilt <- prices |>
    dplyr::inner_join(dplyr::select(ownership_roster(), "ccn", "state"), by = "ccn") |>
    dplyr::group_by(.data$state, .data$code, insurance_type = .data$payer_type) |>
    dplyr::summarise(rebuilt = stats::median(.data$price), n = dplyr::n(), .groups = "drop") |>
    dplyr::inner_join(medians, by = base::c("state", "code", "insurance_type"))
  testthat::expect_equal(base::nrow(rebuilt), base::nrow(medians))
  testthat::expect_equal(rebuilt$rebuilt, rebuilt$median_price)
  testthat::expect_equal(rebuilt$n, rebuilt$n_hospitals)
})

testthat::test_that("model frame clusters by listed PE system, then CHSP system, then hospital", {
  prices <- tibble::tibble(ccn = base::c("000001", "000002", "000003", "000004"), code = "45378", payer_type = "commercial",
                           price = base::c(100, 200, 300, 0), n_contracts = 1L, n_rows = 1L)
  classification <- tibble::tibble(
    ccn = base::c("000001", "000002", "000003", "000004"), state = "CO",
    hospital_type = base::c("Acute Care Hospitals", "Acute Care Hospitals", "Critical Access Hospitals", "Acute Care Hospitals"),
    health_sys_id = base::c("HSI1", "HSI1", NA, NA), pe_system_id = base::c("lifepoint", NA, NA, NA)
  )
  chsp <- tibble::tibble(ccn = base::c("000001", "000002"), hos_beds = base::c("50", "450"))
  frame <- ownership_model_frame(prices, classification, chsp)

  testthat::expect_equal(frame$cluster_id, base::c("pe:lifepoint", "HSI1", "ccn:000003"))   # the $0 price is dropped
  testthat::expect_equal(frame$bed_size, base::c("<100", "300+", "unknown"))
  testthat::expect_equal(frame$in_system, base::c(TRUE, TRUE, FALSE))
})

#' Synthetic analysis frame: PE hospitals price 30% above nonprofit, for-profit 10%
synthetic_frame <- function(n_pe = 12L, seed = 1L) {
  base::set.seed(seed)
  groups <- base::c(base::rep("nonprofit", 60L), base::rep("pe", n_pe), base::rep("for_profit_non_pe", 20L), base::rep("government", 2L))
  n <- base::length(groups)
  state <- base::sample(base::c("CO", "TX", "CA"), n, replace = TRUE)
  effect <- base::c(nonprofit = 0, pe = base::log(1.3), for_profit_non_pe = base::log(1.1), government = 0)
  state_effect <- base::c(CO = 0, TX = -0.2, CA = 0.3)
  log_price <- base::log(1000) + effect[groups] + state_effect[state] + stats::rnorm(n, sd = 0.02)

  tibble::tibble(
    ccn = base::sprintf("%06d", base::seq_len(n)), code = "45378", payer_type = "commercial",
    price = base::exp(base::unname(log_price)), log_price = base::unname(log_price), state = state,
    hospital_type = base::sample(base::c("Acute Care Hospitals", "Critical Access Hospitals"), n, replace = TRUE),
    in_system = base::rep(base::c(TRUE, FALSE), length.out = n), bed_size = "100-299",
    cluster_id = base::paste0("sys", (base::seq_len(n) - 1L) %/% 2L),
    ownership_group_cms_flag = groups
  )
}

testthat::test_that("the model returns adjusted % differences with CIs, n, and flags", {
  frame <- synthetic_frame()
  results <- ownership_models(frame, definitions = "cms_flag", engine = "sandwich")

  testthat::expect_equal(results$term, ownership_group_levels()[-1])
  # groups absent from the cell are reported with n = 0, not estimated
  testthat::expect_equal(results$n_group[results$term == "distressed_fund"], 0L)
  testthat::expect_true(base::all(base::c(
    "code", "payer_type", "definition", "estimate", "std_error", "ci_low", "ci_high", "p_value",
    "pct_diff", "pct_ci_low", "pct_ci_high", "n_group", "n_group_clusters", "n_hospitals", "n_clusters",
    "se_type", "n_pe", "n_pe_clusters", "low_pe_n", "few_pe_clusters", "group_clusters"
  ) %in% base::names(results)))

  pe <- dplyr::filter(results, .data$term == "pe")
  testthat::expect_lt(base::abs(pe$pct_diff - 0.3), 0.03)
  testthat::expect_true(pe$pct_ci_low < 0.3 && pe$pct_ci_high > 0.3)
  testthat::expect_equal(pe$n_group, 12L)
  testthat::expect_false(pe$low_pe_n)
  testthat::expect_match(pe$se_type, "cluster-robust")
  testthat::expect_lt(base::abs(results$pct_diff[results$term == "for_profit_non_pe"] - 0.1), 0.03)
  # two government hospitals: counted, not estimated
  government <- dplyr::filter(results, .data$term == "government")
  testthat::expect_true(base::is.na(government$estimate))
  testthat::expect_equal(government$n_group, 2L)
  testthat::expect_match(government$note, "not estimated")

  small <- ownership_models(synthetic_frame(n_pe = 5L), definitions = "cms_flag", engine = "sandwich")
  testthat::expect_true(small$low_pe_n[small$term == "pe"])

  # every PE hospital in one system: estimate kept, no clustered CI
  one_system <- synthetic_frame() |>
    dplyr::mutate(cluster_id = dplyr::if_else(.data$ownership_group_cms_flag == "pe", "pe_system", .data$cluster_id))
  single <- fit_ownership_model(one_system, engine = "sandwich") |> dplyr::filter(.data$term == "pe")
  testthat::expect_false(base::is.na(single$estimate))
  testthat::expect_true(base::is.na(single$ci_low))
  testthat::expect_equal(single$n_group_clusters, 1L)

  # base lm fallback says so
  fallback <- fit_ownership_model(frame, engine = "lm")
  testthat::expect_match(fallback$se_type[fallback$term == "pe"], "model-based")
})

testthat::test_that("raw summary and within-state comparison are computed by hand", {
  frame <- tibble::tibble(
    code = "45378", payer_type = "commercial", ccn = base::sprintf("%06d", 1:7),
    state = base::c("CO", "CO", "CO", "TX", "TX", "TX", "NM"),
    price = base::c(1000, 1400, 2000, 500, 800, 900, 3000),
    ownership_group_cms_flag = base::c("nonprofit", "nonprofit", "pe", "nonprofit", "pe", "pe", "pe")
  )

  summary_tbl <- ownership_raw_summary(frame, "cms_flag")
  testthat::expect_equal(summary_tbl$median_price[summary_tbl$ownership_group == "pe"], 1450)
  testthat::expect_equal(summary_tbl$n_hospitals[summary_tbl$ownership_group == "nonprofit"], 3L)

  ratios <- frame |>
    dplyr::mutate(pe_system_id = dplyr::if_else(.data$ownership_group_cms_flag == "pe", "sysx", NA_character_),
                  pe_system_class = "strict", pe_system_ambiguity = "") |>
    ownership_system_ratios(reference_group_col = "ownership_group_cms_flag")
  # CO: 2000 / 1200; TX: 800 / 500 and 900 / 500; NM has no nonprofit reference
  testthat::expect_equal(ratios$n_hospitals, 3L)
  testthat::expect_equal(ratios$median_pct_diff, 2000 / 1200 - 1)   # median of 1.667, 1.6, 1.8
  testthat::expect_equal(ratios$share_above_nonprofit, 1)

  within <- ownership_within_state(frame, "cms_flag", groups = "pe")
  # CO: 2000 vs 1200 (+800, +66.7%); TX: 850 vs 500 (+350, +70%); NM has no nonprofit
  testthat::expect_equal(within$n_states, 2L)
  testthat::expect_equal(within$median_diff_dollars, 575)
  testthat::expect_equal(within$median_pct_diff, (2000 / 1200 - 1 + 0.7) / 2)
  testthat::expect_equal(within$share_states_higher, 1)
})

testthat::test_that("the forest plot builds from model results", {
  testthat::expect_error(ownership_models(synthetic_frame(), definitions = "pe_broad"), "ownership_group_pe_broad")

  frame <- synthetic_frame() |>
    dplyr::mutate(
      ownership_group_pe_strict = dplyr::if_else(.data$ownership_group_cms_flag == "government", "distressed_fund", .data$ownership_group_cms_flag),
      ownership_group_pe_broad = .data$ownership_group_cms_flag
    )
  results <- ownership_models(frame, definitions = base::c("pe_strict", "pe_broad"), engine = "sandwich", B = 199L)
  plot <- plot_ownership_forest(results)
  testthat::expect_s3_class(plot, "ggplot")
  testthat::expect_silent(ggplot2::ggplot_build(plot))
})

#' Clustered data with a treatment assigned to whole clusters
wcr_data <- function(G = 40L, n = 400L, share_treated = 1 / 3, effect = 0.2, seed = 1L) {
  base::set.seed(seed)
  cl <- base::sample(G, n, replace = TRUE)
  st <- base::sample(letters[1:5], n, replace = TRUE)
  treat <- base::as.numeric(cl <= base::round(G * share_treated))
  y <- effect * treat + stats::rnorm(G)[cl] * 0.3 + stats::rnorm(n) * 0.5 + (st == "a") * 0.4
  base::list(y = y, cl = cl, X = stats::model.matrix(~ treat + st), treat = treat, st = st)
}

testthat::test_that("the wild cluster bootstrap matches sandwich CRV1 and brute-force refits", {
  d <- wcr_data()
  fit <- stats::lm(d$y ~ d$treat + d$st)
  v <- sandwich::vcovCL(fit, cluster = d$cl, type = "HC1")
  boot <- wild_cluster_bootstrap(d$y, d$X, "treat", d$cl, B = 199L, ci = FALSE)
  testthat::expect_equal(boot$estimate, base::unname(stats::coef(fit)[2]))
  testthat::expect_equal(boot$se_crv1, base::sqrt(v[2, 2]))

  # the fast cluster-sum algebra gives the same p value as refitting every draw
  G <- 25L
  small <- wcr_data(G = G, n = 200L, share_treated = 0.25, effect = 0.1, seed = 3L)
  B <- 199L
  beta0 <- 0.3
  fast <- wild_cluster_bootstrap(small$y, small$X, "treat", small$cl, B = B, ci = FALSE, seed = 11L, null = beta0)
  base::set.seed(11L)
  V <- base::matrix(webb_weights(G * B), G, B)
  g <- base::factor(small$cl)
  X <- small$X
  restricted <- stats::lm.fit(X[, -2], small$y - beta0 * X[, 2])
  full <- stats::lm.fit(X, small$y)
  c_adj <- (G / (G - 1)) * ((base::length(small$y) - 1) / (base::length(small$y) - base::ncol(X)))
  crv1 <- function(res) {
    bread <- base::solve(base::crossprod(X))
    base::sqrt(c_adj * (bread %*% base::crossprod(base::rowsum(X * res, g)) %*% bread)[2, 2])
  }
  t_obs <- (full$coefficients[2] - beta0) / crv1(full$residuals)
  t_star <- base::vapply(base::seq_len(B), function(b) {
    refit <- stats::lm.fit(X, beta0 * X[, 2] + restricted$fitted.values + restricted$residuals * V[base::as.integer(g), b])
    (refit$coefficients[2] - beta0) / crv1(refit$residuals)
  }, base::numeric(1))
  testthat::expect_equal(fast$p_value, base::mean(base::abs(t_star) >= base::abs(t_obs)))
})

testthat::test_that("with many clusters the bootstrap CI is close to the CRV1 CI, and the test has about nominal size", {
  d <- wcr_data(G = 60L, n = 600L, share_treated = 0.5)
  fit <- stats::lm(d$y ~ d$treat + d$st)
  se <- base::sqrt(sandwich::vcovCL(fit, cluster = d$cl, type = "HC1")[2, 2])
  crv1_width <- 2 * stats::qt(0.975, 59) * se
  boot <- wild_cluster_bootstrap(d$y, d$X, "treat", d$cl, B = 999L)
  testthat::expect_lt(base::abs((boot$ci_high - boot$ci_low) / crv1_width - 1), 0.15)
  testthat::expect_true(boot$ci_low < stats::coef(fit)[2] && boot$ci_high > stats::coef(fit)[2])

  # size under a true null: 200 data sets, 5% test
  rejections <- base::vapply(base::seq_len(200L), function(i) {
    null_data <- wcr_data(G = 30L, n = 240L, share_treated = 0.5, effect = 0, seed = 100L + i)
    wild_cluster_bootstrap(null_data$y, null_data$X, "treat", null_data$cl, B = 199L, ci = FALSE, seed = i)$p_value <= 0.05
  }, base::logical(1))
  testthat::expect_gt(base::mean(rejections), 0.01)
  testthat::expect_lt(base::mean(rejections), 0.10)
})

testthat::test_that("few treated clusters are exploratory and non-covered Medicare Advantage rows are not payment comparisons", {
  series <- tibble::tibble(definition = "cms_flag", term = "pe", label = "PE")
  many <- ownership_models(synthetic_frame(), definitions = "cms_flag", engine = "sandwich", wcr_series = series, B = 199L)
  pe <- dplyr::filter(many, .data$term == "pe")
  testthat::expect_equal(pe$n_group_clusters, 6L)                 # 12 PE hospitals, two per system
  testthat::expect_false(pe$exploratory)
  testthat::expect_false(base::is.na(pe$wcr_ci_low))
  testthat::expect_true(pe$pct_wcr_ci_low < 0.3 && pe$pct_wcr_ci_high > 0.3)
  testthat::expect_equal(pe$pct_wcr_ci_low, base::exp(pe$wcr_ci_low) - 1)

  few <- ownership_models(synthetic_frame(n_pe = 5L), definitions = "cms_flag", engine = "sandwich", wcr_series = series, B = 199L)
  pe_few <- dplyr::filter(few, .data$term == "pe")
  testthat::expect_true(pe_few$exploratory)                        # 3 systems < min_treated_clusters()
  testthat::expect_match(pe_few$note, "exploratory")
  # this group fails both bars, and the note says so
  testthat::expect_match(pe_few$note, "health-system clusters and fewer than 10 hospitals")

  # enough systems, too few hospitals: 8 PE hospitals, each its own system.
  # The cluster bar passes and the group is still thin evidence, which is the
  # case an audit found reported like any other row (+108% from 8 hospitals).
  thin <- synthetic_frame(n_pe = 8L)
  pe_rows <- thin$ownership_group_cms_flag == "pe"
  thin$cluster_id[pe_rows] <- base::paste0("pesys", base::seq_len(base::sum(pe_rows)))
  thin_fit <- ownership_models(thin, definitions = "cms_flag", engine = "sandwich", wcr_series = series, B = 199L)
  pe_thin <- dplyr::filter(thin_fit, .data$term == "pe")
  testthat::expect_equal(pe_thin$n_group, 8L)
  testthat::expect_gte(pe_thin$n_group_clusters, min_treated_clusters())
  testthat::expect_true(pe_thin$exploratory)
  testthat::expect_match(pe_thin$note, "fewer than 10 hospitals")
  testthat::expect_false(base::grepl("health-system clusters", pe_thin$note))

  iud_ma <- synthetic_frame() |> dplyr::mutate(code = "58300", payer_type = "medicare_advantage")
  flagged <- ownership_models(iud_ma, definitions = "cms_flag", engine = "sandwich", wcr_series = series, B = 199L)
  testthat::expect_false(base::any(flagged$payment_comparison))
  testthat::expect_match(flagged$note[flagged$term == "pe"], "not a payment comparison")

  both <- dplyr::bind_rows(many, flagged) |>
    dplyr::mutate(definition = "pe_strict")
  plot <- plot_ownership_forest(both, series = tibble::tibble(definition = "pe_strict", term = "pe", label = "PE"))
  testthat::expect_true(base::all(plot$data$code == "45378"))
})
