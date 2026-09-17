#' Childbirth prices: vaginal and cesarean delivery, relative to Medicare
#'
#' Facility prices for delivery are inpatient MS-DRG rates (cesarean 783-788,
#' vaginal 796-798 and 805-807); the anchors are the uncomplicated DRGs, 788
#' (cesarean without sterilization, without CC/MCC) and 807 (vaginal delivery
#' without sterilization or D&C, without CC/MCC). Physician delivery fees are
#' the CPT obstetric codes (59400-59622).
#'
#' The Medicare benchmark is the standard IPPS payment each hospital would get
#' for the DRG under the FY 2026 final rule (CMS-1833-F):
#'   operating = (labor share x wage index + nonlabor) x DRG weight
#'   capital   = capital federal rate x GAF x DRG weight, GAF = wage index^0.6848
#' with the national standardized amounts of Tables 1A/1B (labor share 66% when
#' the wage index exceeds 1, else 62%), the capital rate of Table 1D, the
#' weights of Table 5, and each hospital's Table 2 wage index (state rural
#' index from Table 3 when the CCN is not in Table 2; see R/geo_figures.R). It
#' leaves out the indirect medical education, disproportionate share,
#' uncompensated care, outlier, and quality adjustments, so it is the standard
#' federal payment, not what a particular hospital is paid. Medicare pays for
#' few births; the benchmark is a common yardstick for comparing prices across
#' payers and places, as in the colonoscopy figures.

birth_drg_anchors <- function() {
  base::c(cesarean = "788", vaginal = "807")
}

#' APR-DRG severity-1 delivery codes and the MS-DRG anchor each stands in for
#'
#' Some hospitals, mostly in states whose Medicaid programs pay by APR-DRG,
#' post inpatient prices only as APR-DRGs, so they have no MS-DRG 807 or 788
#' price at all and drop out of the delivery analysis entirely. APR-DRG 560
#' and 540 are vaginal and cesarean delivery, and severity of illness 1 is the
#' uncomplicated case, which is what MS-DRG 807 and 788 ("without CC/MCC")
#' price. Severities 2-4 have no anchor here and are not collected.
#'
#' This is an approximation, not an identity: the two groupers assign cases
#' differently, and the Medicare benchmark these prices are divided by is
#' still the MS-DRG one. So it is a coverage sensitivity, reported apart from
#' the headline, and every row it adds is labelled `price_source = "apr_drg"`.
apr_drg_anchor_map <- function() {
  base::c("560-1" = "807", "540-1" = "788")
}

#' Fall back to a hospital's APR-DRG severity-1 price where it posts no MS-DRG
#'
#' Never overwrites: a hospital with an MS-DRG price for a code and payer
#' keeps it, and the APR-DRG row is dropped rather than averaged in.
#'
#' @param prices tibble(ccn, code, payer_type, price, ...) that may carry the
#'   APR-DRG codes of `map`.
#' @return the same columns plus `price_source` ("ms_drg" or "apr_drg"), with
#'   APR-DRG rows recoded to the MS-DRG anchor they stand in for.
add_apr_drg_delivery_prices <- function(prices, map = apr_drg_anchor_map()) {
  ms <- prices |>
    dplyr::filter(!.data$code %in% base::names(map)) |>
    dplyr::mutate(price_source = "ms_drg")
  apr <- prices |>
    dplyr::filter(.data$code %in% base::names(map)) |>
    dplyr::mutate(code = base::unname(map[.data$code]), price_source = "apr_drg")
  if (base::nrow(apr) == 0L) return(ms)
  dplyr::bind_rows(
    ms,
    dplyr::anti_join(apr, dplyr::distinct(ms, .data$ccn, .data$code, .data$payer_type),
                     by = base::c("ccn", "code", "payer_type"))
  )
}

ipps_drg_source <- function() {
  base::list(
    release = "FY 2026 IPPS final rule (CMS-1833-F), Tables 1A-1E and Table 5",
    page_url = "https://www.cms.gov/medicare/payment/prospective-payment-systems/acute-inpatient-pps/fy-2026-ipps-final-rule-home-page",
    zip_urls = base::c(
      table_1 = "https://www.cms.gov/files/zip/fy2026-ipps-fr-table-1a-1e.zip",
      table_5 = "https://www.cms.gov/files/zip/fy2026-ipps-fr-table-5.zip"
    )
  )
}

#' Download and unzip IPPS Tables 1A-1E and 5; returns the directory
download_ipps_drg_tables <- function(source = ipps_drg_source(), dest_dir = hpt_path("reference", "cms_ipps_drg")) {
  for (url in source$zip_urls) {
    zip_path <- base::file.path(dest_dir, base::basename(url))
    download_public_file(url, zip_path)
    utils::unzip(zip_path, exdir = stringr::str_remove(zip_path, "\\.zip$"))
  }
  provenance_path <- base::file.path(dest_dir, "ipps_drg_provenance.csv")
  if (!base::file.exists(provenance_path)) {
    zips <- base::file.path(dest_dir, base::basename(source$zip_urls))
    write_csv_atomic(
      tibble::tibble(release = source$release, page_url = source$page_url, zip_url = base::unname(source$zip_urls),
                     zip_sha256 = base::vapply(zips, sha256_file, base::character(1), USE.NAMES = FALSE),
                     downloaded_at = utc_timestamp()),
      provenance_path
    )
  }
  dest_dir
}

find_ipps_file <- function(pattern, dir = hpt_path("reference", "cms_ipps_drg")) {
  paths <- base::list.files(dir, pattern = pattern, recursive = TRUE, full.names = TRUE)
  if (base::length(paths) == 0L) {
    base::stop("No file matching '", pattern, "' under ", dir, "; run download_ipps_drg_tables() first.")
  }
  paths[[base::which.max(base::file.mtime(paths))]]
}

#' MS-DRG weights and geometric mean length of stay (Table 5)
load_ipps_drg_weights <- function(path = find_ipps_file("Table 5\\.txt$")) {
  lines <- base::readLines(path, encoding = "latin1", warn = FALSE)
  header_idx <- base::which(stringr::str_detect(lines, "^MS-DRG\\s*\\t"))[1]
  if (base::is.na(header_idx)) {
    base::stop("No MS-DRG header row in ", path)
  }
  tbl <- readr::read_tsv(base::I(base::paste(lines[header_idx:base::length(lines)], collapse = "\n")),
                         col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE)
  base::names(tbl) <- stringr::str_squish(base::names(tbl))
  # footnote rows at the end carry no DRG number
  tbl <- tbl[stringr::str_detect(stringr::str_trim(tbl[["MS-DRG"]]), "^[0-9]{1,3}$"), ]
  tibble::tibble(
    code = stringr::str_pad(stringr::str_trim(tbl[["MS-DRG"]]), 3, pad = "0"),
    title = tbl[["MS-DRG Title"]],
    weight = base::suppressWarnings(base::as.numeric(tbl[["Weights - 10% Cap Applied"]])),
    # a few DRGs have no LOS ("N/A")
    gmlos = base::suppressWarnings(base::as.numeric(tbl[["Geometric mean LOS"]]))
  ) |>
    dplyr::filter(!base::is.na(.data$weight))
}

#' National standardized amounts and the capital rate (Tables 1A, 1B, 1D),
#' for hospitals that submit quality data and are meaningful EHR users
load_ipps_standardized_amounts <- function(path = find_ipps_file("Tables 1A - 1E\\.txt$")) {
  lines <- base::readLines(path, encoding = "latin1", warn = FALSE)
  money <- function(line) {
    base::as.numeric(stringr::str_remove_all(stringr::str_extract_all(line, "\\$[0-9,]+\\.[0-9]+")[[1]], "[$,]"))
  }
  first_money_after <- function(pattern) {
    start <- base::which(stringr::str_detect(lines, pattern))[1]
    rows <- lines[(start + 1L):base::length(lines)]
    money(rows[stringr::str_detect(rows, "\\$[0-9]")][1])
  }
  table_1a <- first_money_after("^\"?TABLE 1A")
  table_1b <- first_money_after("^\"?TABLE 1B")
  table_1d <- first_money_after("^\"?TABLE 1D")
  base::list(
    high_wage = base::c(labor = table_1a[1], nonlabor = table_1a[2]),
    low_wage = base::c(labor = table_1b[1], nonlabor = table_1b[2]),
    capital = table_1d[1]
  )
}

#' Exponent CMS uses to turn the wage index into the capital geographic
#' adjustment factor (GAF = wage index ^ 0.6848)
capital_gaf_exponent <- function() {
  0.6848
}

#' Standard IPPS payment per hospital and DRG
#'
#' @param hospitals tibble(ccn, state).
#' @param weights load_ipps_drg_weights() rows for the DRGs wanted.
#' @param amounts load_ipps_standardized_amounts().
hospital_ipps_benchmark <- function(hospitals, weights, amounts, wage_index, rural_wage_index) {
  hospitals |>
    dplyr::left_join(wage_index, by = "ccn") |>
    dplyr::left_join(rural_wage_index, by = "state") |>
    dplyr::mutate(
      wage_index_source = dplyr::if_else(base::is.na(.data$wage_index), "state_rural", "ipps_table_2"),
      wage_index = dplyr::coalesce(.data$wage_index, .data$rural_wage_index),
      labor = dplyr::if_else(.data$wage_index > 1, amounts$high_wage[["labor"]], amounts$low_wage[["labor"]]),
      nonlabor = dplyr::if_else(.data$wage_index > 1, amounts$high_wage[["nonlabor"]], amounts$low_wage[["nonlabor"]]),
      base_rate = .data$labor * .data$wage_index + .data$nonlabor + amounts$capital * .data$wage_index^capital_gaf_exponent()
    ) |>
    dplyr::select("ccn", "state", "wage_index", "wage_index_source", "base_rate") |>
    tidyr::crossing(dplyr::select(weights, "code", "weight", "gmlos")) |>
    dplyr::mutate(medicare_ipps = .data$base_rate * .data$weight)
}

# ---- which hospitals deliver babies -----------------------------------------

cms_maternal_source <- function() {
  base::list(
    dataset = "CMS Care Compare, Maternal Health - Hospital (nrdb-3fcy)",
    url = "https://data.cms.gov/provider-data/sites/default/files/resources/5a4754b088fdb10d2ae278ef215925a7_1785189972/Maternal_Health-Hospital.csv"
  )
}

#' Hospitals' labor-and-delivery status from the CMS maternal morbidity
#' structural measure (SM-7): "Yes"/"No" hospitals provide inpatient labor and
#' delivery (Yes = Birthing-Friendly), "Not Applicable" ones do not. The
#' cesarean measure (PC-02) in the same file is not yet publicly reported
#' ("Not Available" for every hospital in the 2026-07 release).
load_ld_hospitals <- function(path = hpt_path("reference", "cms_maternal", "Maternal_Health-Hospital.csv")) {
  if (!base::file.exists(path)) {
    download_public_file(cms_maternal_source()$url, path)
  }
  readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE) |>
    dplyr::filter(.data$`Measure ID` == "SM_7") |>
    dplyr::transmute(
      ccn = normalize_ccn(.data$`Facility ID`),
      sm7 = .data$Score,
      provides_ld = dplyr::case_when(
        .data$Score %in% base::c("Yes", "No") ~ TRUE,
        stringr::str_detect(.data$Score, "^Not Applicable") ~ FALSE,
        TRUE ~ NA
      ),
      birthing_friendly = .data$Score == "Yes"
    ) |>
    dplyr::distinct(.data$ccn, .keep_all = TRUE)
}

# ---- prices, ratios, premium --------------------------------------------------

#' Hospital delivery prices joined to their Medicare IPPS benchmark
#'
#' @param prices tibble(ccn, code, payer_type, price) (ownership_hospital_prices()).
#' @param benchmark hospital_ipps_benchmark() rows.
birth_price_ratios <- function(prices, hospitals, benchmark) {
  prices |>
    dplyr::inner_join(dplyr::select(benchmark, "ccn", "code", "medicare_ipps", "wage_index_source"), by = base::c("ccn", "code")) |>
    dplyr::inner_join(dplyr::distinct(hospitals, .data$ccn, .data$state), by = "ccn") |>
    dplyr::filter(!base::is.na(.data$medicare_ipps)) |>
    dplyr::mutate(ratio = .data$price / .data$medicare_ipps)
}

#' Within-hospital cesarean premium: DRG 788 price / DRG 807 price, and the
#' dollar difference, for each hospital and payer type listing both
cesarean_premium <- function(ratios, anchors = birth_drg_anchors()) {
  ratios |>
    dplyr::filter(.data$code %in% anchors) |>
    dplyr::mutate(kind = base::names(anchors)[base::match(.data$code, anchors)]) |>
    dplyr::select("ccn", "state", "payer_type", "kind", "price") |>
    tidyr::pivot_wider(names_from = "kind", values_from = "price") |>
    dplyr::filter(!base::is.na(.data$cesarean), !base::is.na(.data$vaginal)) |>
    dplyr::mutate(premium_ratio = .data$cesarean / .data$vaginal, premium_dollars = .data$cesarean - .data$vaginal)
}

#' National summary by code and payer type: median price, ratio to the
#' Medicare benchmark, interquartile ranges, and hospital counts
birth_national_summary <- function(ratios) {
  ratios |>
    dplyr::group_by(.data$code, .data$payer_type) |>
    dplyr::summarise(
      n_hospitals = dplyr::n_distinct(.data$ccn),
      median_price = stats::median(.data$price),
      p25_price = stats::quantile(.data$price, 0.25, names = FALSE),
      p75_price = stats::quantile(.data$price, 0.75, names = FALSE),
      median_medicare_ipps = stats::median(.data$medicare_ipps),
      median_ratio = stats::median(.data$ratio),
      p25_ratio = stats::quantile(.data$ratio, 0.25, names = FALSE),
      p75_ratio = stats::quantile(.data$ratio, 0.75, names = FALSE),
      .groups = "drop"
    )
}

#' State summary in the shape state_region_chart() and ratio_state_map() use
birth_state_summary <- function(ratios, min_hospitals = 5L) {
  ratios |>
    dplyr::rename(medicare_opps = "medicare_ipps") |>
    state_ratio_summary(min_hospitals = min_hospitals) |>
    dplyr::rename(medicare_ipps = "medicare_opps")
}

#' Premium summary by group (national or state), with the share of hospitals
#' where the cesarean is priced above the vaginal delivery
premium_summary <- function(premium, ...) {
  premium |>
    dplyr::group_by(.data$payer_type, ...) |>
    dplyr::summarise(
      n_hospitals = dplyr::n_distinct(.data$ccn),
      median_ratio = stats::median(.data$premium_ratio),
      p25_ratio = stats::quantile(.data$premium_ratio, 0.25, names = FALSE),
      p75_ratio = stats::quantile(.data$premium_ratio, 0.75, names = FALSE),
      median_dollars = stats::median(.data$premium_dollars),
      share_cesarean_higher = base::mean(.data$premium_ratio > 1),
      .groups = "drop"
    )
}

#' Median ratio to Medicare by hospital characteristic
birth_by_hospital_group <- function(ratios, groups) {
  ratios |>
    dplyr::inner_join(groups, by = "ccn") |>
    tidyr::pivot_longer(base::setdiff(base::names(groups), "ccn"), names_to = "dimension", values_to = "group") |>
    dplyr::filter(!base::is.na(.data$group)) |>
    dplyr::group_by(.data$code, .data$payer_type, .data$dimension, .data$group) |>
    dplyr::summarise(n_hospitals = dplyr::n_distinct(.data$ccn), median_price = stats::median(.data$price),
                     median_ratio = stats::median(.data$ratio), .groups = "drop")
}
