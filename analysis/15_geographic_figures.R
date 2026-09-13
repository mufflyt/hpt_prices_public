#!/usr/bin/env Rscript
#' Static geographic figures for colonoscopy (CPT 45378) facility prices.
#'
#' Reads hpt.duckdb (read-only), CMS OPPS Addendum B (reference/cms_opps,
#' downloaded on first run), and the CMS FY 2026 IPPS wage index tables
#' (reference/cms_ipps_wage_index, downloaded on first run). Writes to
#' HPT_DATA_DIR/output/ (derived from Trilliant data, so never committed):
#'   geo_hospital_ratios_45378.csv      hospital x payer: price, Medicare OPPS benchmark, ratio
#'   geo_state_summary_45378.csv        state x payer: median ratio and price, IQR, n hospitals
#'   geo_spread_stats_45378.csv         p90/p10 of state medians, within-state IQR ratio, state R^2
#'   geo_system_weighted_45378.csv      state x payer: hospital- vs system-weighted state medians
#'   geo_system_weighting_stats_45378.csv  per payer: Spearman, spreads, state R^2, largest movers
#'   figures/geo1_colonoscopy_commercial_medicaid_maps.{png,pdf}
#'   figures/geo2_colonoscopy_state_ranks.{png,pdf}
#'   figures/supp_geo3_colonoscopy_medicare_advantage_map.{png,pdf}
#'   figures/supp_geo4_colonoscopy_ma_state_ranks.{png,pdf}
#'   figures/supp_geo5_colonoscopy_system_weighting.{png,pdf}
#' Methods: header of R/geo_figures.R.

base::source("R/00_source_all.R")

code <- "45378"
payer_types <- base::c("commercial", "medicaid", "medicare_advantage")
min_hospitals <- 5L
out_dir <- hpt_path("output")
fig_dir <- hpt_path("output", "figures")
base::dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
db_path <- hpt_database_path()

exclude_path <- hpt_path("output", "median_excluded_file_ids.csv")
exclude <- if (base::file.exists(exclude_path)) read_csv_chr(exclude_path)$mrf_file_id else NULL

# ---- data -----------------------------------------------------------------------

prices <- ownership_hospital_prices(db_path, codes = code, payer_types = payer_types, exclude_file_ids = exclude)
hospitals <- duckdb_query("SELECT ccn, state, hospital_type, health_sys_id, health_sys_name FROM dim_hospital",
                          database = db_path, read_only = TRUE)
opps_rates <- load_opps_rates(network = TRUE)
wage_dir <- download_ipps_wage_index()
wage_index <- load_hospital_wage_index(find_ipps_table("2", base::dirname(wage_dir)))
rural_wage_index <- load_state_rural_wage_index(find_ipps_table("3", base::dirname(wage_dir)))

ratios <- hospital_price_ratios(prices, hospitals, code, opps_rates, wage_index, rural_wage_index)
summary <- state_ratio_summary(ratios, min_hospitals = min_hospitals)
spread <- state_spread_stats(ratios, summary)
national <- ratios |>
  dplyr::group_by(.data$payer_type) |>
  dplyr::summarise(median_price = stats::median(.data$price), median_ratio = stats::median(.data$ratio),
                   n_hospitals = dplyr::n_distinct(.data$ccn), .groups = "drop")

write_csv_atomic(ratios, base::file.path(out_dir, base::paste0("geo_hospital_ratios_", code, ".csv")))
write_csv_atomic(summary, base::file.path(out_dir, base::paste0("geo_state_summary_", code, ".csv")))
write_csv_atomic(spread, base::file.path(out_dir, base::paste0("geo_spread_stats_", code, ".csv")))

# system-weighting sensitivity: one unit per health system per state
units <- hospital_system_units(hospitals)
system_summary <- state_system_weighted_summary(ratios, units, min_units = min_hospitals)
weighting <- compare_system_weighting(summary, system_summary)
weighting_stats <- system_weighting_stats(weighting, ratios, units)
write_csv_atomic(weighting, base::file.path(out_dir, base::paste0("geo_system_weighted_", code, ".csv")))
write_csv_atomic(weighting_stats, base::file.path(out_dir, base::paste0("geo_system_weighting_stats_", code, ".csv")))

base::message("Hospitals by wage-index source: ",
              base::paste(base::names(base::table(ratios$wage_index_source)), base::table(ratios$wage_index_source), sep = " ", collapse = ", "))
base::print(national)
base::print(spread)
base::message("System weighting (states with >= ", min_hospitals, " hospitals):")
base::print(base::as.data.frame(weighting_stats), digits = 3, right = FALSE)

# ---- figures --------------------------------------------------------------------

limits <- ratio_limits(summary, payer_types)
hatch_note <- base::paste0("Hatched grey: fewer than ", min_hospitals, " hospitals, estimate suppressed (shown as a hollow dot in the state ranking figure). ",
                           "Light grey: no data. Alaska and Hawaii appear in the state ranking figure.")

map_title <- function(payer) {
  national_ratio <- national$median_ratio[national$payer_type == payer]
  base::sprintf("%s (national median %.2fx)", payer_label(payer), national_ratio)
}

save_figure <- function(plot, name, width, height) {
  for (ext in base::c("png", "pdf")) {
    ggplot2::ggsave(base::file.path(fig_dir, base::paste0(name, ".", ext)), plot, width = width, height = height,
                    dpi = 300, bg = "white")
  }
}

fig1 <- patchwork::wrap_plots(
  ratio_state_map(dplyr::filter(summary, .data$payer_type == "commercial"), map_title("commercial"), limits),
  ratio_state_map(dplyr::filter(summary, .data$payer_type == "medicaid"), map_title("medicaid"), limits),
  nrow = 1, guides = "collect"
) + patchwork::plot_annotation(
  title = "Colonoscopy (CPT 45378) hospital facility rates relative to Medicare, by state",
  caption = geo_figure_caption(code, hatch_note),
  theme = ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 13),
                         plot.caption = ggplot2::element_text(size = 7, colour = "grey30", hjust = 0))
)
save_figure(fig1, "geo1_colonoscopy_commercial_medicaid_maps", width = 12, height = 4.3)

fig2 <- state_region_chart(summary, national, payer_types = base::c("commercial", "medicaid")) +
  patchwork::plot_annotation(
    title = "Colonoscopy (CPT 45378) hospital facility rates relative to Medicare, by state",
    subtitle = base::paste0("Dot: state median across hospitals. Bar: 25th to 75th percentile of hospitals. Hollow dot: fewer than 5 hospitals.\n",
                            "Solid line: Medicare OPPS payment. Dashed lines: national medians. States ordered by commercial median within each Census region."),
    caption = geo_figure_caption(code),
    theme = ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 16),
                           plot.subtitle = ggplot2::element_text(size = 11, colour = "grey30"),
                           plot.caption = ggplot2::element_text(size = 9, colour = "grey30", hjust = 0))
  )
save_figure(fig2, "geo2_colonoscopy_state_ranks", width = 14, height = 9.5)

# supplement: Medicare Advantage, which clusters at the Medicare rate (same colour scale as geo1)
for (ext in base::c("png", "pdf")) {
  base::unlink(base::file.path(fig_dir, base::paste0("geo3_colonoscopy_medicare_advantage_map.", ext)))   # renamed supp_geo3
}
supp3 <- ratio_state_map(dplyr::filter(summary, .data$payer_type == "medicare_advantage"), map_title("medicare_advantage"), limits) +
  ggplot2::labs(
    title = base::paste0("Colonoscopy (CPT 45378) facility rates relative to Medicare: ", map_title("medicare_advantage")),
    subtitle = "Same colour scale as the commercial and Medicaid maps",
    caption = geo_figure_caption(code, hatch_note)
  ) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 11, hjust = 0),
                 plot.subtitle = ggplot2::element_text(size = 8, colour = "grey30", hjust = 0),
                 plot.caption = ggplot2::element_text(size = 6.5, colour = "grey30", hjust = 0))
save_figure(supp3, "supp_geo3_colonoscopy_medicare_advantage_map", width = 8.5, height = 5)

supp4 <- state_rank_chart(summary, payer_types = "medicare_advantage", national = national) +
  ggplot2::labs(
    title = "Medicare Advantage colonoscopy (CPT 45378) facility rates by state",
    subtitle = "Dot: state median across hospitals. Bar: 25th to 75th percentile.\nGrey tick: Medicare OPPS payment. Dashed: national median.",
    caption = "Data: Trilliant Health Hospital MRF Data Directory (snapshot 2026-07-21); CMS OPPS Addendum B (July 2026);\nCMS FY 2026 IPPS Tables 2-3."
  ) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 11),
                 plot.subtitle = ggplot2::element_text(size = 8, colour = "grey30"),
                 plot.caption = ggplot2::element_text(size = 7, colour = "grey30", hjust = 0))
save_figure(supp4, "supp_geo4_colonoscopy_ma_state_ranks", width = 6, height = 12)

supp5 <- system_weighting_chart(weighting, payer_types) +
  ggplot2::labs(
    title = "Colonoscopy (CPT 45378) state medians: hospital-weighted vs one value per health system",
    subtitle = base::paste0("Each system's hospitals in a state collapse to their median before the state median; ",
                            "hospitals outside a system count as their own unit. States with ", min_hospitals, "+ hospitals.\n",
                            "Dashed: no change. Labelled: the states that move most."),
    caption = "Data: Trilliant Health Hospital MRF Data Directory (snapshot 2026-07-21); CMS OPPS Addendum B (July 2026); CMS FY 2026 IPPS Tables 2-3; AHRQ Compendium of U.S. Health Systems."
  ) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 11),
                 plot.subtitle = ggplot2::element_text(size = 8, colour = "grey30"),
                 plot.caption = ggplot2::element_text(size = 7, colour = "grey30", hjust = 0))
save_figure(supp5, "supp_geo5_colonoscopy_system_weighting", width = 11, height = 4.6)

base::message("Figures: ", fig_dir)
