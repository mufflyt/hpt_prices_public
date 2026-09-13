# R/addon_economics.R is not yet in R/00_source_all.R; source it here.
if (!base::exists("addon_net_value", mode = "function")) {
  base::source(base::file.path(repo_root_path(), "R", "addon_economics.R"))
}

addon_test_params <- function() {
  load_addon_parameters(base::file.path(repo_root_path(), "config", "addon_parameters.csv"))
}

#' Tiny synthetic medians table: CO has 3+ hospitals for most codes, WY has
#' one hospital (falls back to US), and 88305 has no medicaid row anywhere.
addon_test_medians <- function() {
  row <- function(state, code, insurance_type, price, n) {
    tibble::tibble(
      state = state, concept = "x", code = code, anchor = FALSE, fee_type = "facility",
      insurance_type = insurance_type, median_price = price, p25 = 0.8 * price, p75 = 1.2 * price,
      n_hospitals = n, n_rates = 10 * n
    )
  }
  dplyr::bind_rows(
    row("US", "45378", base::c("commercial", "medicaid", "medicare"), base::c(2000, 400, 950), 50),
    row("CO", "45378", base::c("commercial", "medicaid"), base::c(1800, 350), base::c(5, 3)),
    row("WY", "45378", "commercial", 9999, 1),
    row("US", "58100", base::c("commercial", "medicaid", "medicare"), base::c(300, 80, 206), 40),
    row("CO", "58100", base::c("commercial", "medicaid"), base::c(320, 90), 4),
    row("US", "88305", base::c("commercial", "medicare"), base::c(200, 53), 30),
    row("CO", "88305", "commercial", 180, 3),
    row("US", "43775", base::c("commercial", "medicaid"), base::c(20000, 9000), 20),
    row("CO", "43775", "commercial", 22000, 3),
    row("US", "58300", base::c("commercial", "medicaid"), base::c(400, 60), 20),
    row("US", "J7298", base::c("commercial", "medicaid"), base::c(2400, 1100), 20),
    # a professional row that must be ignored
    row("CO", "45378", "commercial", 50, 9) |> dplyr::mutate(fee_type = "professional")
  )
}

addon_test_cases <- function() {
  addon_case_definitions() |> dplyr::filter(.data$variant %in% base::c("sleeve_cpt_mirena", "diagnostic_45378"))
}

testthat::test_that("parameter file is complete, sourced, and free of em dashes", {
  params <- addon_test_params()
  reused <- base::c(
    "combined_emb_added_minutes", "direct_room_cost_per_minute", "anesthesia_cost_per_minute",
    "combined_emb_anesthesia_drug_increment_cost", "coordination_cost", "emb_pathology_cost",
    "patient_time_opportunity_cost_per_visit"
  )

  testthat::expect_true(base::all(reused %in% params$parameter))
  testthat::expect_true(base::all(stringr::str_detect(params$source[params$parameter %in% reused], "471e067")))
  testthat::expect_false(base::any(base::is.na(params$provisional)))
  testthat::expect_true(base::all(base::nzchar(params$source)))
  varying <- params$distribution != "fixed"
  testthat::expect_false(base::any(base::is.na(params$low_value[varying]) | base::is.na(params$high_value[varying])))

  for (path in base::c("config/addon_parameters.csv", "R/addon_economics.R", "tests/testthat/test-addon.R")) {
    text <- base::readLines(base::file.path(repo_root_path(), path), warn = FALSE, encoding = "UTF-8")
    testthat::expect_false(base::any(stringr::str_detect(text, base::intToUtf8(8212L))), info = path)
  }
})

testthat::test_that("costs are inflated to reference-year dollars with the right index", {
  params <- addon_test_params()
  room <- params[params$parameter == "direct_room_cost_per_minute", ]
  patient <- params[params$parameter == "patient_time_opportunity_cost_per_visit", ]

  testthat::expect_equal(room$base, 20.90 * 593.781 / 435.292)
  testthat::expect_equal(patient$base, 43 * 332.813 / 218.056)
  testthat::expect_equal(params$inflation_factor[params$parameter == "combined_emb_added_minutes"], 1)
})

testthat::test_that("net value is exactly zero at the break-even minutes and utilization", {
  C_S <- 150
  R_P <- 1800
  T_P <- 55
  u <- 0.6
  m <- 0.4

  dT_star <- addon_breakeven_minutes(C_S, R_P, T_P, u, m)
  testthat::expect_equal(dT_star, C_S * T_P / (u * m * R_P))
  testthat::expect_equal(addon_net_value(C_S, R_P, dT_star, T_P, u, m), 0)

  u_star <- addon_breakeven_utilization(C_S, R_P, 5, T_P, m)
  testthat::expect_equal(addon_net_value(C_S, R_P, 5, T_P, u_star, m), 0)
  testthat::expect_gt(u_star, 1) # pays even in a fully booked room

  testthat::expect_equal(addon_room_cost_net_value(C_S, addon_breakeven_minutes_room_cost(C_S, 30), 30), 0)
})

testthat::test_that("slack capacity (u = 0) leaves the net value equal to C_S", {
  testthat::expect_equal(addon_net_value(C_S = 123.4, R_P = 5000, dT = 10, T_P = 145, u = 0, m = 0.5), 123.4)
  testthat::expect_equal(addon_breakeven_minutes(123.4, 5000, 145, 0, 0.5), Inf)
})

testthat::test_that("a negative contribution never breaks even", {
  testthat::expect_true(base::is.na(addon_breakeven_minutes(-10, 1800, 55, 0.5, 0.4)))
  testthat::expect_true(base::is.na(addon_breakeven_utilization(0, 1800, 5, 55, 0.4)))
  testthat::expect_lt(addon_net_value(-10, 1800, 0, 55, 1, 0.4), 0)
})

testthat::test_that("integer capacity floors and only loses a case once slack is used up", {
  # 480 / 55 = 8.7: 8 cases with 40 min of slack
  testthat::expect_equal(addon_cases_per_day(480, 55, 5, 0), 8)
  testthat::expect_equal(addon_cases_per_day(480, 55, 5, 1), 8) # 8 x 60 = 480 fits exactly
  testthat::expect_equal(addon_cases_per_day(480, 55, 6, 1), 7)
  testthat::expect_equal(addon_free_addons_per_day(480, 55, 5), 8)
  testthat::expect_equal(addon_free_addons_per_day(480, 145, 10), 4)
  testthat::expect_equal(addon_cases_per_day(480, 145, 10, 1), 3)
  testthat::expect_equal(addon_cases_per_day(480, 145, 20, 1), 2)

  # one day, k = 0..8 of the 8 cases get a 6-minute add-on: 8 x 55 + k x 6 <= 480
  # holds up to k = 6; at k = 7 the last case is displaced (7 x 55 + 7 x 6 = 427)
  day <- addon_day_value(480, 55, 6, R_P = 1000, R_S = 200, C_S = 100, m = 0.5)
  testthat::expect_equal(day$k, 0:8)
  testthat::expect_equal(day$primary_cases, base::c(base::rep(8, 7), 7, 7))
  testthat::expect_equal(day$primary_cases_lost, base::c(base::rep(0, 7), 1, 1))
  testthat::expect_equal(day$addons, base::c(0:7, 7))              # 8 add-ons cannot outnumber 7 cases
  testthat::expect_equal(day$contribution_change[day$k == 4], 4 * 100) # no case lost: gain = add-ons x C_S
  testthat::expect_equal(day$contribution_change[day$k == 7], 7 * 100 - 0.5 * 1000)
  testthat::expect_equal(day$revenue_change[day$k == 7], 7 * 200 - 1000)

  # bariatric: 3 cases with 45 min of slack; 20-minute IUDs displace a case at the third add-on
  bariatric <- addon_day_value(480, 145, 20, R_P = 20000, R_S = 200, C_S = -1000, m = 0.5)
  testthat::expect_equal(bariatric$primary_cases, base::c(3, 3, 3, 2))
  testthat::expect_equal(bariatric$addons, base::c(0, 1, 2, 2))
  testthat::expect_equal(addon_primary_cases_with_addons(480, 145, 10, 0:3), base::c(3, 3, 3, 3))
})

testthat::test_that("the full-rate scenario is blanked where Medicare does not cover the add-on", {
  opps <- tibble::tibble(code = base::c("58300", "J7298", "58100", "88305"), status_indicator = base::c("E1", "E1", "T", "Q1"), payment_rate = base::c(NA, NA, 206.55, 53.24))
  noncovered <- addon_medicare_noncovered_codes(opps)
  testthat::expect_setequal(noncovered, base::c("58300", "J7298"))
  testthat::expect_error(addon_medicare_noncovered_codes(NULL), "OPPS")

  testthat::expect_equal(
    addon_full_rate_applies(base::c("medicare", "medicare_advantage", "medicaid", "commercial", "medicare"),
                            base::c("58300", "58300", "58300", "58300", "58100"), noncovered),
    base::c(FALSE, FALSE, TRUE, TRUE, TRUE)
  )

  params <- addon_test_params()
  table <- addon_value_by_state(addon_test_medians(), params, cases = addon_test_cases(), insurance_types = base::c("commercial", "medicare")) |>
    addon_mask_full_rate(noncovered)
  sleeve_medicare <- table |> dplyr::filter(.data$variant == "sleeve_cpt_mirena", .data$insurance_type == "medicare")
  testthat::expect_true(base::all(base::is.na(sleeve_medicare$net_value_listed_rate)))
  sleeve_commercial <- table |> dplyr::filter(.data$variant == "sleeve_cpt_mirena", .data$insurance_type == "commercial")
  testthat::expect_false(base::any(base::is.na(sleeve_commercial$net_value_listed_rate)))
  colonoscopy_medicare <- table |> dplyr::filter(.data$variant == "diagnostic_45378", .data$insurance_type == "medicare")
  testthat::expect_false(base::any(base::is.na(colonoscopy_medicare$net_value_listed_rate)))

  # PSA-style table keyed by variant only
  psa_like <- tibble::tibble(variant = base::c("sleeve_cpt_mirena", "diagnostic_45378"), insurance_type = "medicare", prob_worth_it_listed_rate = 0.5)
  testthat::expect_equal(addon_mask_full_rate(psa_like, noncovered)$prob_worth_it_listed_rate, base::c(NA, 0.5))
})

testthat::test_that("rates fall back to the national median below min_hospitals", {
  medians <- addon_test_medians()
  grid <- tibble::tibble(state = base::c("CO", "CO", "WY", "US", "CO"), insurance_type = base::c("commercial", "medicaid", "commercial", "commercial", "medicare"))

  rates <- addon_lookup_rate(medians, grid, "45378", min_hospitals = 3L)
  testthat::expect_equal(rates$source, base::c("state", "state", "national", "national", "national"))
  testthat::expect_equal(rates$rate, base::c(1800, 350, 2000, 2000, 950)) # professional $50 row ignored

  strict <- addon_lookup_rate(medians, grid, "45378", min_hospitals = 4L)
  testthat::expect_equal(strict$rate[2], 400) # CO medicaid has 3 hospitals

  pathology <- addon_lookup_rate(medians, grid, "88305", min_hospitals = 3L, fallback_rate = 53.24)
  testthat::expect_equal(pathology$source[2], "parameter")
  testthat::expect_equal(pathology$rate[2], 53.24)

  missing <- addon_lookup_rate(medians, grid, "43644", min_hospitals = 3L)
  testthat::expect_true(base::all(missing$source == "missing" & base::is.na(missing$rate)))
})

testthat::test_that("payment fractions use insurance-specific overrides when they exist", {
  names <- addon_test_params()$parameter
  testthat::expect_equal(
    addon_pay_fraction_param(base::c("A", "A", "B", "B"), "item", base::c("medicare", "commercial", "medicare", "exchange"), names),
    base::c("pay_frac_A_item__medicare", "pay_frac_A_item", "pay_frac_B_item__medicare", "pay_frac_B_item")
  )
})

testthat::test_that("the state table reproduces a hand calculation and flags fallback", {
  params <- addon_test_params()
  values <- addon_parameter_values(params, "base")
  table <- addon_value_by_state(addon_test_medians(), params, cases = addon_test_cases(), insurance_types = base::c("commercial", "medicaid", "medicare"))

  co <- table |> dplyr::filter(.data$variant == "diagnostic_45378", .data$state == "CO", .data$insurance_type == "commercial")
  R_S <- values[["pay_frac_B_procedure"]] * 320 + values[["pay_frac_B_item"]] * 180
  cost <- base::sum(values[base::c("emb_pathology_cost", "emb_disposable_supply_cost", "combined_emb_anesthesia_drug_increment_cost", "coordination_cost")])
  expected <- (R_S - cost) - values[["utilization_B"]] * values[["combined_emb_added_minutes"]] / values[["colonoscopy_slot_minutes"]] *
    values[["contribution_margin_colonoscopy"]] * 1800
  testthat::expect_equal(co$net_value, expected)
  testthat::expect_false(co$primary_fallback)
  testthat::expect_false(co$secondary_fallback)

  wy <- table |> dplyr::filter(.data$variant == "diagnostic_45378", .data$state == "WY", .data$insurance_type == "commercial")
  testthat::expect_equal(wy$R_P, 2000)
  testthat::expect_true(wy$primary_fallback)

  medicare <- table |> dplyr::filter(.data$variant == "diagnostic_45378", .data$state == "US", .data$insurance_type == "medicare")
  testthat::expect_equal(medicare$R_S, 0.5 * 206 + 0 * 53) # OPPS: 58100 at 50%, 88305 packaged

  sleeve <- table |> dplyr::filter(.data$variant == "sleeve_cpt_mirena", .data$state == "US", .data$insurance_type == "commercial")
  testthat::expect_equal(sleeve$R_S_listed, 400 + 2400)
  testthat::expect_equal(sleeve$net_value_listed_rate, sleeve$R_S_listed - sleeve$variable_cost - sleeve$displacement_cost)
  testthat::expect_equal(sleeve$dT_star, addon_breakeven_minutes(sleeve$C_S, sleeve$R_P, sleeve$T_P, sleeve$u, sleeve$m))
})

testthat::test_that("tornado varies each used parameter one at a time", {
  params <- addon_test_params()
  rates <- addon_rates(addon_test_medians(), params, addon_test_cases(), "commercial") |> dplyr::filter(.data$state == "US")
  tornado <- addon_tornado(rates, params)
  base_row <- addon_evaluate(rates, addon_parameter_values(params, "base"))

  utilization <- tornado |> dplyr::filter(.data$variant == "sleeve_cpt_mirena", .data$parameter == "utilization_A")
  testthat::expect_equal(utilization$net_at_low, base_row$C_S[base_row$variant == "sleeve_cpt_mirena"]) # u low = 0
  testthat::expect_true(base::all(tornado$swing > 0))
  testthat::expect_false(base::any(tornado$parameter == "or_block_minutes"))
  testthat::expect_true("R_P (hospital IQR)" %in% tornado$parameter)

  labels <- addon_tornado_labels(tornado, params)
  testthat::expect_equal(base::length(labels), base::nrow(tornado))
  testthat::expect_false(base::any(base::grepl("_", labels)))              # no code names left
  added <- labels[tornado$parameter == "combined_emb_added_minutes"]
  testthat::expect_equal(added, "Added room time for the EMB (1 min to 12 min)")
  testthat::expect_equal(labels[tornado$variant == "sleeve_cpt_mirena" & tornado$parameter == "utilization_A"],
                         "Chance added minutes displace a bariatric case (0% to 75%)")
  testthat::expect_match(labels[tornado$variant == "diagnostic_45378" & tornado$parameter == "R_P (hospital IQR)"],
                         "^Colonoscopy negotiated rate \\(\\$1,600 to \\$2,400, hospital IQR\\)$")
})

testthat::test_that("PSA draws respect their bounds and are reproducible", {
  params <- addon_test_params()
  draws <- addon_draw_parameters(params, n = 2000L, seed = 1L)

  testthat::expect_equal(draws, addon_draw_parameters(params, n = 2000L, seed = 1L))
  added <- draws[, "combined_emb_added_minutes"]
  testthat::expect_true(base::all(added >= 1 & added <= 12))
  testthat::expect_equal(base::mean(draws[, "utilization_B"]), 0.5, tolerance = 0.03)
  testthat::expect_true(base::all(draws[, "pay_frac_B_item__medicare"] == 0))

  rates <- addon_rates(addon_test_medians(), params, addon_test_cases(), base::c("commercial", "medicaid")) |> dplyr::filter(.data$state == "US")
  psa <- addon_psa(rates, params, n = 500L, seed = 2L)
  testthat::expect_equal(base::nrow(psa$summary), base::nrow(rates))
  testthat::expect_true(base::all(psa$summary$prob_worth_it >= 0 & psa$summary$prob_worth_it <= 1))
  testthat::expect_equal(base::nrow(psa$draws), 500L * base::nrow(rates))
})

testthat::test_that("payer view counts the avoided office encounter", {
  params <- addon_test_params()
  values <- addon_parameter_values(params, "base")
  evaluated <- addon_rates(addon_test_medians(), params, addon_test_cases(), "commercial") |>
    dplyr::filter(.data$state == "US") |>
    addon_evaluate(values)
  system <- addon_system_perspective(evaluated, values)

  b <- system |> dplyr::filter(.data$case == "B")
  testthat::expect_equal(
    b$payer_standalone_payment,
    values[["emb_office_professional_cost"]] + values[["emb_pathology_cost"]] + values[["office_visit_em_cost"]]
  )
  a <- system |> dplyr::filter(.data$case == "A")
  testthat::expect_equal(
    a$payer_standalone_payment,
    values[["office_iud_insertion_professional_payment"]] + values[["iud_acquisition_cost_J7298"]] + values[["office_visit_em_cost"]]
  )
  testthat::expect_equal(a$patient_delay_days_per_addon, a$displaced_primary_per_100_addons / 100 * values[["delay_days_per_displaced_case_A"]])
})
