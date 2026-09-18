#!/usr/bin/env Rscript
#' Childbirth prices: what a commercial or Medicaid birth costs, how much it
#' varies, and whether a cesarean costs more than a vaginal delivery everywhere.
#'
#' Facility prices are the uncomplicated delivery DRGs (807 vaginal, 788
#' cesarean) at hospitals that deliver babies (CMS SM-7: not "Not
#' Applicable"), as each hospital's median across its payer/plan contracts
#' (per-diem rates converted to a stay; see per_diem_note()). The Medicare
#' benchmark is the standard FY 2026 IPPS payment at the hospital's wage
#' index (R/birth_prices.R). Physician delivery fees (CPT 59400-59622) come
#' from the state medians table.
#'
#' Reads hpt.duckdb (read-only), CMS IPPS Tables 1-5, and CMS maternal health.
#' With HPT_BIRTH_APR_DRG=true every output below is written under the
#' birth_apr_ prefix instead, so the two builds sit side by side.
#'
#' Writes to HPT_DATA_DIR/output/ (Trilliant-derived; never committed):
#'   birth_hospital_prices.csv        hospital x DRG x payer: price, Medicare IPPS, ratio
#'   birth_national_summary.csv       DRG x payer: median price and ratio, IQR, n
#'   birth_state_summary.csv          state x DRG x payer
#'   birth_cesarean_premium.csv       hospital x payer: 788 / 807 and the dollar gap
#'   birth_premium_summary.csv        national and state premium
#'   birth_by_hospital_group.csv      by hospital type, ownership, system, Birthing-Friendly
#'   birth_professional_fees.csv      national CPT delivery fees by payer
#'   birth_medicare_check.csv         hospital-listed Medicare DRG rates / the IPPS benchmark
#'   figures/birth1_delivery_ranks.{png,pdf}, birth2_cesarean_premium.{png,pdf},
#'   birth3_vaginal_maps.{png,pdf}

base::source("R/00_source_all.R")

codes <- base::unname(birth_drg_anchors())
# Coverage sensitivity, off by default: HPT_BIRTH_APR_DRG=true also reads the
# APR-DRG severity-1 delivery codes and lets them stand in for the MS-DRG
# anchor at hospitals that post no MS-DRG delivery price (apr_drg_anchor_map()
# in R/birth_prices.R). The headline numbers stay MS-DRG only.
use_apr_drg <- base::identical(base::Sys.getenv("HPT_BIRTH_APR_DRG"), "true")
price_codes <- if (use_apr_drg) base::c(codes, base::names(apr_drg_anchor_map())) else codes
# The fallback run writes its own files rather than overwriting the MS-DRG
# ones. It used to share them, so whichever run went last owned output/, and an
# impact table then compared an MS-DRG "before" against an APR-DRG "after" and
# reported a 14% fall in the Medicaid delivery price that was really 347 extra
# hospitals. Both builds now sit side by side and can be compared directly.
out_prefix <- if (use_apr_drg) "birth_apr_" else "birth_"
out_file <- function(name) base::file.path(out_dir, base::paste0(out_prefix, name))
fig_prefix <- if (use_apr_drg) "birth_apr" else "birth" 
payer_types <- base::c("commercial", "medicaid", "medicare_advantage", "self_pay_cash")
min_hospitals <- 5L
out_dir <- hpt_path("output")
fig_dir <- hpt_path("output", "figures")
base::dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
db_path <- hpt_database_path()
exclude_path <- hpt_path("output", "median_excluded_file_ids.csv")
exclude <- if (base::file.exists(exclude_path)) read_csv_chr(exclude_path)$mrf_file_id else NULL

# ---- inputs -------------------------------------------------------------------

hospitals <- duckdb_query("SELECT ccn, state, hospital_type, hospital_ownership, health_sys_name FROM dim_hospital",
                          database = db_path, read_only = TRUE)
ld <- load_ld_hospitals()
wage_dir <- download_ipps_wage_index()
wage_index <- load_hospital_wage_index(find_ipps_table("2", base::dirname(wage_dir)))
rural_wage_index <- load_state_rural_wage_index(find_ipps_table("3", base::dirname(wage_dir)))
download_ipps_drg_tables()
weights <- load_ipps_drg_weights() |> dplyr::filter(.data$code %in% codes)
amounts <- load_ipps_standardized_amounts()
benchmark <- hospital_ipps_benchmark(dplyr::distinct(hospitals, .data$ccn, .data$state), weights, amounts, wage_index, rural_wage_index)

# hospitals that do not deliver babies list delivery DRGs only as chargemaster boilerplate
# psychiatric hospitals and Rural Emergency Hospitals (no inpatient beds) do not
# deliver babies either, and are absent from the CMS maternal file
no_ld <- base::union(ld$ccn[ld$provides_ld %in% FALSE],
                     hospitals$ccn[hospitals$hospital_type %in% base::c("Psychiatric", "Rural Emergency Hospital")])
all_prices <- ownership_hospital_prices(db_path, codes = price_codes, payer_types = payer_types, exclude_file_ids = exclude) |>
  add_apr_drg_delivery_prices()
if (use_apr_drg) {
  n_apr <- dplyr::n_distinct(all_prices$ccn[all_prices$price_source == "apr_drg"])
  base::message("APR-DRG fallback on: ", n_apr, " hospitals post a delivery price only as APR-DRG severity 1")
}
prices <- dplyr::filter(all_prices, !.data$ccn %in% no_ld)
base::message("Delivery prices: ", dplyr::n_distinct(prices$ccn), " hospitals after dropping ",
              base::length(base::intersect(no_ld, all_prices$ccn)), " that do not deliver babies (CMS SM-7, psychiatric, REH)")

ratios <- birth_price_ratios(prices, hospitals, benchmark)
national <- birth_national_summary(ratios)
states <- birth_state_summary(ratios, min_hospitals = min_hospitals)
premium <- cesarean_premium(ratios)
premium_national <- premium_summary(premium)
premium_states <- premium_summary(premium, .data$state) |> dplyr::mutate(low_n = .data$n_hospitals < min_hospitals)

groups <- hospitals |>
  dplyr::left_join(dplyr::select(ld, "ccn", "birthing_friendly"), by = "ccn") |>
  dplyr::transmute(
    .data$ccn,
    hospital_type = .data$hospital_type,
    ownership = dplyr::case_when(
      stringr::str_detect(.data$hospital_ownership, "^Voluntary") ~ "nonprofit",
      stringr::str_detect(.data$hospital_ownership, "^Proprietary|^Physician") ~ "for-profit",
      stringr::str_detect(.data$hospital_ownership, "Government|Tribal|Veterans|Department of Defense") ~ "government",
      TRUE ~ "other"
    ),
    system = dplyr::if_else(base::is.na(.data$health_sys_name) | !base::nzchar(.data$health_sys_name), "independent", "in a system"),
    birthing_friendly = dplyr::case_when(.data$birthing_friendly ~ "Birthing-Friendly", !.data$birthing_friendly ~ "not designated", TRUE ~ NA_character_)
  )
by_group <- birth_by_hospital_group(ratios, groups)

professional <- arrow::read_parquet(hpt_path("output", "state_insurance_medians.parquet")) |>
  dplyr::filter(.data$state == "US", .data$fee_type == "professional",
                .data$code %in% base::c("59400", "59409", "59410", "59510", "59514", "59515"),
                .data$insurance_type %in% base::c("commercial", "medicaid", "medicare", "medicare_advantage")) |>
  dplyr::select("code", "insurance_type", "median_price", "p25", "p75", "n_hospitals")

# hospitals also list their own Medicare DRG rate; those include teaching and
# safety-net add-ons, so they should sit at or above the standard payment
medicare_check <- ownership_hospital_prices(db_path, codes = codes, payer_types = "medicare", exclude_file_ids = exclude) |>
  dplyr::inner_join(dplyr::select(benchmark, "ccn", "code", "medicare_ipps"), by = base::c("ccn", "code")) |>
  dplyr::mutate(ratio = .data$price / .data$medicare_ipps) |>
  dplyr::group_by(.data$code) |>
  dplyr::summarise(n_hospitals = dplyr::n(), median_ratio = stats::median(.data$ratio),
                   p25 = stats::quantile(.data$ratio, 0.25, names = FALSE), p75 = stats::quantile(.data$ratio, 0.75, names = FALSE),
                   .groups = "drop")

if (use_apr_drg) {
  # the count above is taken before the labour-and-delivery filter; this is the
  # number that actually reaches the medians, and it is the one to quote
  base::message("APR-DRG fallback: ", dplyr::n_distinct(ratios$ccn[ratios$price_source == "apr_drg"]),
                " of them survive the labour-and-delivery filter and enter the analysis")
}
write_csv_atomic(ratios, out_file("hospital_prices.csv"))
write_csv_atomic(national, out_file("national_summary.csv"))
write_csv_atomic(states, out_file("state_summary.csv"))
write_csv_atomic(premium, out_file("cesarean_premium.csv"))
write_csv_atomic(dplyr::bind_rows(dplyr::mutate(premium_national, state = "US"), premium_states),
                 out_file("premium_summary.csv"))
write_csv_atomic(by_group, out_file("by_hospital_group.csv"))
write_csv_atomic(professional, out_file("professional_fees.csv"))
write_csv_atomic(medicare_check, out_file("medicare_check.csv"))

base::print(national, n = 50)
base::print(premium_national)
base::print(medicare_check)

# ---- figures ------------------------------------------------------------------

caption <- base::paste(
  "Ratio = each hospital's negotiated facility rate (median of its payer/plan contracts; per-diem rates converted to a stay) /",
  "the standard FY 2026 Medicare IPPS payment for the DRG at the hospital's wage index. Hospitals without labor and delivery (CMS SM-7) excluded.",
  "Data: Trilliant Health Hospital MRF Data Directory (snapshot 2026-07-21); CMS FY 2026 IPPS Tables 1-5; CMS Care Compare maternal health.",
  sep = "\n"
)
national_ratio <- function(code, payer) national$median_ratio[national$code == code & national$payer_type == payer]

rank_page <- function(code, label) {
  state_region_chart(dplyr::filter(states, .data$code == !!code),
                     dplyr::filter(national, .data$code == !!code) |> dplyr::select("payer_type", "median_ratio"),
                     payer_types = base::c("commercial", "medicaid"), limits = base::c(0.25, 6)) +
    patchwork::plot_annotation(
      title = base::sprintf("%s (MS-DRG %s): hospital facility rates relative to Medicare, by state", label, code),
      subtitle = base::sprintf(paste0("National median: commercial %.2fx, Medicaid %.2fx. Dot: state median across hospitals. ",
                                     "Bar: 25th to 75th percentile. Hollow: fewer than %d hospitals."),
                               national_ratio(code, "commercial"), national_ratio(code, "medicaid"), min_hospitals),
      caption = caption,
      theme = ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 16),
                             plot.subtitle = ggplot2::element_text(size = 11, colour = "grey30"),
                             plot.caption = ggplot2::element_text(size = 9, colour = "grey30", hjust = 0))
    )
}
save_figure(rank_page("807", "Vaginal delivery"), base::paste0(fig_prefix, "1_vaginal_ranks"), width = 14, height = 9.5)
save_figure(rank_page("788", "Cesarean delivery"), base::paste0(fig_prefix, "1_cesarean_ranks"), width = 14, height = 9.5)

premium_chart_data <- premium_states |>
  dplyr::filter(.data$payer_type %in% base::c("commercial", "medicaid")) |>
  dplyr::transmute(.data$payer_type, .data$state, .data$n_hospitals, .data$median_ratio, .data$p25_ratio, .data$p75_ratio, .data$low_n)
premium_plot <- state_region_chart(premium_chart_data,
                                   dplyr::filter(premium_national, .data$payer_type %in% base::c("commercial", "medicaid")) |>
                                     dplyr::select("payer_type", "median_ratio"),
                                   limits = base::c(0.5, 3), one_label = "Equal", breaks = base::c(0.5, 0.75, 1, 1.5, 2, 3)) +
  patchwork::plot_annotation(
    title = "Cesarean premium: a hospital's cesarean price (DRG 788) / its vaginal delivery price (DRG 807), by state",
    subtitle = base::sprintf(paste0("Right of 'Equal': the cesarean costs more. National median: commercial %.2fx (cesarean higher at %.0f%% of hospitals), ",
                                   "Medicaid %.2fx (%.0f%%). Medicare's DRG weights imply 1.42x."),
                             premium_national$median_ratio[premium_national$payer_type == "commercial"],
                             100 * premium_national$share_cesarean_higher[premium_national$payer_type == "commercial"],
                             premium_national$median_ratio[premium_national$payer_type == "medicaid"],
                             100 * premium_national$share_cesarean_higher[premium_national$payer_type == "medicaid"]),
    caption = "Hospitals listing both DRGs for the payer type. Data: Trilliant Health Hospital MRF Data Directory (snapshot 2026-07-21).",
    theme = ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 15),
                           plot.subtitle = ggplot2::element_text(size = 11, colour = "grey30"),
                           plot.caption = ggplot2::element_text(size = 9, colour = "grey30", hjust = 0))
  )
save_figure(premium_plot, base::paste0(fig_prefix, "2_cesarean_premium"), width = 14, height = 9.5)

map_states <- dplyr::filter(states, .data$code == "807")
limits <- ratio_limits(map_states, base::c("commercial", "medicaid"))
maps <- patchwork::wrap_plots(
  ratio_state_map(dplyr::filter(map_states, .data$payer_type == "commercial"),
                  base::sprintf("Commercial (national median %.2fx)", national_ratio("807", "commercial")), limits),
  ratio_state_map(dplyr::filter(map_states, .data$payer_type == "medicaid"),
                  base::sprintf("Medicaid (national median %.2fx)", national_ratio("807", "medicaid")), limits),
  nrow = 1, guides = "collect"
) + patchwork::plot_annotation(
  title = "Vaginal delivery (MS-DRG 807) hospital facility rates relative to Medicare, by state",
  caption = base::paste0("Suppressed (hatched): fewer than ", min_hospitals, " hospitals. Grey: no data.\n", caption),
  theme = ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 13),
                         plot.caption = ggplot2::element_text(size = 7, colour = "grey30", hjust = 0))
)
save_figure(maps, base::paste0(fig_prefix, "3_vaginal_maps"), width = 12, height = 4.6)

base::message("Figures: ", fig_dir)
