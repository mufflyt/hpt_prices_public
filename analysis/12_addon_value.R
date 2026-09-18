#!/usr/bin/env Rscript
#' Is an add-on procedure worth its room time?
#'   A: IUD insertion + device at bariatric surgery
#'   B: endometrial biopsy + pathology at colonoscopy
#'
#' Reads HPT_DATA_DIR/output/state_insurance_medians.parquet (from
#' compute_state_medians()), config/addon_parameters.csv, and CMS OPPS
#' Addendum B (reference/cms_opps; Medicare coverage of the add-on codes).
#' Writes to
#' HPT_DATA_DIR/output/:
#'   addon_value_by_state.csv       base case, every variant x state x insurance type
#'   addon_tornado.csv              one-way sensitivity, national commercial and medicaid
#'   addon_day_capacity.csv         one-room-day model, k = 0..n add-on cases, national
#'   addon_psa_summary.csv          probabilistic sensitivity analysis, national
#'   addon_system_perspective.csv   payer and patient view, national
#'   addon_state_summary.csv        states where each variant is net-positive, by insurance type
#'   figures/addon_*.png            threshold, tornado, and day-capacity figures
#' Methods: docs/addon_methods.md.

base::source("R/00_source_all.R")
if (!base::exists("addon_net_value", mode = "function")) {
  base::source("R/addon_economics.R")
}
if (!base::requireNamespace("ggplot2", quietly = TRUE)) {
  base::stop("Package 'ggplot2' is required for the figures.")
}

psa_draws <- base::as.integer(base::Sys.getenv("ADDON_PSA_DRAWS", "5000"))
min_hospitals <- 3L
out_dir <- hpt_path("output")
fig_dir <- hpt_path("output", "figures")
base::dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

medians <- tibble::as_tibble(arrow::read_parquet(hpt_path("output", "state_insurance_medians.parquet")))
params <- load_addon_parameters("config/addon_parameters.csv")
values <- addon_parameter_values(params, "base")
cases <- addon_case_definitions()
# the full-standalone-rate scenario is meaningless where Medicare does not
# cover the add-on (58300 and the IUD J-codes are OPPS status E1)
noncovered_codes <- addon_medicare_noncovered_codes(load_opps_rates(network = TRUE))

# ---- 1. base case by state ---------------------------------------------------

rates <- addon_rates(medians, params, cases, min_hospitals = min_hospitals)
evaluated <- addon_evaluate(rates, values) |> addon_mask_full_rate(noncovered_codes, cases)
by_state <- addon_value_by_state(medians, params, cases, min_hospitals = min_hospitals) |>
  addon_mask_full_rate(noncovered_codes, cases)
write_csv_atomic(by_state, base::file.path(out_dir, "addon_value_by_state.csv"))

missing_primary <- by_state |>
  dplyr::filter(.data$state == "US") |>
  dplyr::group_by(.data$variant) |>
  dplyr::summarise(insurance_types_with_primary = base::sum(!base::is.na(.data$R_P)))
base::message("National insurance types with a primary price, by variant:")
base::print(missing_primary)

# national rows with a usable net value drive every summary below
national <- evaluated |> dplyr::filter(.data$state == "US", !base::is.na(.data$net_value))
# case A base = MS-DRG 621; DRG 620 and CPT 43775 are sensitivities
headline_variants <- base::c("drg621_mirena", "drg620_mirena", "sleeve_cpt_mirena", "diagnostic_45378")

state_summary <- by_state |>
  dplyr::filter(.data$state != "US", !base::is.na(.data$net_value)) |>
  dplyr::group_by(.data$case, .data$variant, .data$insurance_type) |>
  dplyr::summarise(
    states = dplyr::n(),
    states_state_primary_price = base::sum(!.data$primary_fallback),
    states_all_prices_own_state = base::sum(!.data$primary_fallback & !.data$secondary_fallback),
    states_net_positive = base::sum(.data$net_value > 0),
    states_net_positive_room_cost = base::sum(.data$net_value_room_cost > 0),
    states_net_positive_listed_rate = if (base::all(base::is.na(.data$net_value_listed_rate))) NA_integer_ else base::sum(.data$net_value_listed_rate > 0, na.rm = TRUE),
    net_value_min = base::min(.data$net_value),
    net_value_median = stats::median(.data$net_value),
    net_value_max = base::max(.data$net_value),
    .groups = "drop"
  )
write_csv_atomic(state_summary, base::file.path(out_dir, "addon_state_summary.csv"))

# ---- 2. tornado ---------------------------------------------------------------

tornado_rates <- rates |>
  dplyr::filter(
    .data$state == "US", .data$variant %in% headline_variants,
    .data$insurance_type %in% base::c("commercial", "medicaid"), !base::is.na(.data$primary_rate)
  )
tornado <- addon_tornado(tornado_rates, params) |>
  dplyr::left_join(params |> dplyr::select("parameter", "provisional"), by = "parameter")
tornado$label <- addon_tornado_labels(tornado, params)
write_csv_atomic(tornado, base::file.path(out_dir, "addon_tornado.csv"))

# ---- 3. figures ----------------------------------------------------------------

# Every payment is an HPT negotiated rate used as a payment proxy. The blue
# and green lines charge the displaced case its contribution margin and
# differ only in what the add-on is paid.
series_colors <- base::c(
  "Displaced-case margin; add-on paid its expected share of the negotiated rate (base)" = "#2a78d6",
  "Displaced-case margin; add-on paid its full negotiated rate (upper bound)" = "#1baf7a",
  "Accounting room cost per minute; add-on paid its expected share" = "#eb6834"
)
full_rate_series <- base::names(series_colors)[2]
insurance_labels <- base::c(
  commercial = "Commercial", medicare_advantage = "Medicare Advantage", medicare = "Traditional Medicare",
  medicaid = "Medicaid", exchange = "Exchange", self_pay_cash = "Cash price"
)
addon_caption <- base::paste(
  "Above zero = worth it. Payments are HPT negotiated facility rates used as a payment proxy, not claims or remittance data.",
  "Displaced-case margin = u x (dT / T_P) x contribution margin x primary negotiated rate.",
  "Room cost: Childers 2018 direct cost (staff and supplies, no overhead) treated as fully variable, so it does not depend on u.",
  sep = "\n"
)
plot_types <- base::c("commercial", "medicare_advantage", "medicare", "medicaid", "exchange", "self_pay_cash")

theme_addon <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = "#0b0b0b"),
      axis.text = ggplot2::element_text(colour = "#52514e"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#e6e5e0", linewidth = 0.3),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0),
      legend.position = "top",
      legend.title = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "#fcfcfb", colour = NA),
      plot.title.position = "plot",
      plot.caption = ggplot2::element_text(colour = "#52514e", hjust = 0)
    )
}

plot_rows <- national |>
  dplyr::filter(.data$base_variant, .data$insurance_type %in% plot_types) |>
  dplyr::mutate(insurance_type = base::factor(.data$insurance_type, levels = plot_types))

threshold_curves <- function(rows, over) {
  grid <- if (over == "minutes") base::seq(0, 30, by = 0.5) else base::seq(0, 1, by = 0.02)
  rows |>
    dplyr::select("case", "variant", "secondary_procedure", "insurance_type", "C_S", "C_S_listed", "R_P", "T_P", "dT", "u", "m", "room_cost_per_minute") |>
    tidyr::expand_grid(x = grid) |>
    dplyr::mutate(
      dT_x = if (over == "minutes") .data$x else .data$dT,
      u_x = if (over == "minutes") .data$u else .data$x,
      !!base::names(series_colors)[1] := addon_net_value(.data$C_S, .data$R_P, .data$dT_x, .data$T_P, .data$u_x, .data$m),
      !!base::names(series_colors)[3] := addon_room_cost_net_value(.data$C_S, .data$dT_x, .data$room_cost_per_minute),
      !!full_rate_series := dplyr::if_else(
        addon_full_rate_applies(.data$insurance_type, .data$secondary_procedure, noncovered_codes),
        addon_net_value(.data$C_S_listed, .data$R_P, .data$dT_x, .data$T_P, .data$u_x, .data$m),
        NA_real_
      )
    ) |>
    tidyr::pivot_longer(base::names(series_colors), names_to = "framing", values_to = "net_value") |>
    dplyr::filter(!base::is.na(.data$net_value)) |>
    dplyr::mutate(framing = base::factor(.data$framing, levels = base::names(series_colors)))
}

plot_threshold <- function(case_id, over) {
  rows <- plot_rows |> dplyr::filter(.data$case == case_id)
  if (base::nrow(rows) == 0L) {
    base::message("No national rows with a primary price for case ", case_id, "; skipping figure.")
    return(base::invisible(NULL))
  }
  curves <- threshold_curves(rows, over)
  base_x <- if (over == "minutes") rows$dT[1] else rows$u[1]
  label <- if (case_id == "A") "IUD insertion + device at bariatric surgery (MS-DRG 621 price)" else "Endometrial biopsy + pathology at colonoscopy"
  room <- if (case_id == "A") "OR time" else "endoscopy room time"
  x_label <- if (over == "minutes") "Added room minutes per add-on (dT)" else base::paste0("Probability the added minutes displace otherwise productive ", room, " (u)")
  no_full_rate <- rows |>
    dplyr::filter(!addon_full_rate_applies(.data$insurance_type, .data$secondary_procedure, noncovered_codes))
  caption <- if (base::nrow(no_full_rate) > 0L) {
    base::paste0(
      addon_caption, "\nNo full-rate line for ",
      base::paste(insurance_labels[base::intersect(base::names(insurance_labels), base::as.character(no_full_rate$insurance_type))], collapse = " or "),
      ": Medicare does not cover ", base::paste(base::unique(no_full_rate$secondary_procedure), collapse = ", "),
      " (OPPS status E1) and Medicare Advantage\nfollows Medicare coverage, so a posted rate for it is not a payment."
    )
  } else {
    addon_caption
  }

  figure <- ggplot2::ggplot(curves, ggplot2::aes(.data$x, .data$net_value, colour = .data$framing)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#52514e", linewidth = 0.4) +
    ggplot2::geom_vline(xintercept = base_x, colour = "#a3a29c", linewidth = 0.4, linetype = "dashed") +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::facet_wrap(ggplot2::vars(.data$insurance_type), scales = "free_y", labeller = ggplot2::as_labeller(insurance_labels)) +
    ggplot2::scale_colour_manual(values = series_colors, breaks = base::names(series_colors)) +
    ggplot2::scale_y_continuous(labels = scales::label_dollar()) +
    ggplot2::guides(colour = ggplot2::guide_legend(ncol = 1)) +
    ggplot2::labs(
      title = base::paste0("Case ", case_id, ": ", label),
      subtitle = "Net value to the hospital per add-on, national median negotiated facility rates. Dashed line: base case.",
      x = x_label, y = "Net value per add-on",
      caption = caption
    ) +
    theme_addon()

  path <- base::file.path(fig_dir, base::sprintf("addon_threshold_%s_%s.png", over, case_id))
  ggplot2::ggsave(path, figure, width = 10, height = 7.5, dpi = 150, bg = "#fcfcfb")
  base::message("Wrote ", path)
}

for (case_id in base::c("A", "B")) {
  plot_threshold(case_id, "minutes")
  plot_threshold(case_id, "utilization")
}

tornado_plot <- tornado |>
  dplyr::filter(.data$variant %in% base::c("drg621_mirena", "diagnostic_45378")) |>
  dplyr::group_by(.data$variant, .data$insurance_type) |>
  dplyr::slice_max(.data$swing, n = 8, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    panel = base::paste0(
      dplyr::if_else(.data$case == "A", "IUD at bariatric surgery", "EMB at colonoscopy"),
      ", ", dplyr::recode(.data$insurance_type, commercial = "commercial", medicaid = "Medicaid")
    ),
    panel = base::factor(.data$panel, levels = base::unique(.data$panel[base::order(.data$case, .data$insurance_type)])),
    # suffix keeps each panel's bars ordered by its own swing
    parameter = stats::reorder(base::paste0(.data$label, "___", .data$panel), .data$swing)
  )

if (base::nrow(tornado_plot) > 0L) {
  tornado_figure <- ggplot2::ggplot(tornado_plot) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = .data$base_net), colour = "#52514e", linewidth = 0.4) +
    ggplot2::geom_segment(
      ggplot2::aes(x = .data$net_at_low, xend = .data$net_at_high, y = .data$parameter, yend = .data$parameter),
      colour = "#2a78d6", linewidth = 3.5, lineend = "butt"
    ) +
    ggplot2::geom_point(ggplot2::aes(x = .data$net_at_low, y = .data$parameter), colour = "#0b0b0b", size = 1.4) +
    ggplot2::scale_y_discrete(labels = function(x) base::sub("___.*$", "", x)) +
    ggplot2::scale_x_continuous(labels = scales::label_dollar()) +
    ggplot2::facet_wrap(ggplot2::vars(.data$panel), scales = "free", ncol = 2) +
    ggplot2::labs(
      title = "One-way sensitivity of net value per add-on (national)",
      subtitle = base::paste(
        "Displaced-case framing, add-on paid its expected share of the negotiated rate. Bar spans the net value at the low and high",
        "value in parentheses; dot marks the low value; vertical line = base case.",
        sep = "\n"
      ),
      x = "Net value per add-on", y = NULL,
      caption = base::paste(
        "Primary prices: MS-DRG 621 (IUD at bariatric surgery) and CPT 45378 (EMB at colonoscopy).",
        "Rates are HPT negotiated facility rates used as a payment proxy; price rows move to the interhospital 25th and 75th percentiles.",
        base::sprintf(
          "\u2020 provisional: no defensible direct source (%s of the parameters drawn here). The widest bar is one of them.",
          base::sum(tornado_plot$provisional %in% TRUE)
        ),
        sep = "\n"
      )
    ) +
    theme_addon() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 8.5))
  ggplot2::ggsave(base::file.path(fig_dir, "addon_tornado.png"), tornado_figure, width = 13, height = 7.5, dpi = 150, bg = "#fcfcfb")
}

# ---- 4. integer capacity -------------------------------------------------------

high_values <- addon_parameter_values(params, "high")
day_capacity <- national |>
  dplyr::filter(.data$base_variant, .data$insurance_type %in% base::c("commercial", "medicaid")) |>
  dplyr::mutate(block_minutes = addon_lookup_values(values, .data$block_param)) |>
  purrr::pmap_dfr(function(...) {
    row <- tibble::tibble(...)
    # base added minutes, and the high end of the dT range
    dT_scenarios <- base::c(base = row$dT, high = base::unname(high_values[row$added_minutes_param]))
    purrr::imap_dfr(dT_scenarios, function(dT, scenario) {
      addon_day_value(row$block_minutes, row$T_P, dT, row$R_P, row$R_S, row$C_S, row$m) |>
        dplyr::mutate(
          case = row$case, variant = row$variant, insurance_type = row$insurance_type,
          dT_scenario = scenario, block_minutes = row$block_minutes, T_P = row$T_P, dT = dT,
          free_addons_per_day = addon_free_addons_per_day(row$block_minutes, row$T_P, dT),
          .before = 1
        )
    })
  })
write_csv_atomic(day_capacity, base::file.path(out_dir, "addon_day_capacity.csv"))

day_plot <- day_capacity |>
  dplyr::filter(.data$insurance_type == "commercial") |>
  dplyr::group_by(.data$case) |>
  dplyr::mutate(
    panel = base::sprintf(
      "%s\n%g-min %s, %g-min day: %g fit. Add-on %g min (base) or %g min (high)",
      dplyr::if_else(.data$case == "A", "IUD at bariatric surgery", "EMB at colonoscopy"),
      .data$T_P, dplyr::if_else(.data$case == "A", "cases", "colonoscopies"), .data$block_minutes, .data$n_scheduled,
      base::min(.data$dT), base::max(.data$dT)
    ),
    scenario = base::paste(.data$dT_scenario, "added minutes")
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(panel = base::factor(.data$panel, levels = base::unique(.data$panel[base::order(.data$case)])))
# label the first k at which each scenario loses a primary case
day_drops <- day_plot |>
  dplyr::group_by(.data$panel, .data$scenario) |>
  dplyr::arrange(.data$k, .by_group = TRUE) |>
  dplyr::filter(.data$primary_cases_lost > dplyr::lag(.data$primary_cases_lost, default = 0)) |>
  dplyr::mutate(
    lost = .data$primary_cases_lost - dplyr::lag(.data$primary_cases_lost, default = 0),
    note = base::sprintf("%g primary case%s displaced", .data$lost, dplyr::if_else(.data$lost == 1, "", "s"))
  ) |>
  dplyr::ungroup()

if (base::nrow(day_plot) > 0L) {
  scenario_colors <- base::c("base added minutes" = "#2a78d6", "high added minutes" = "#eb6834")
  day_figure <- ggplot2::ggplot(day_plot, ggplot2::aes(.data$k, .data$contribution_change, colour = .data$scenario)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#52514e", linewidth = 0.4) +
    ggplot2::geom_line(linewidth = 0.6, alpha = 0.6) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::geom_label(
      data = day_drops, ggplot2::aes(label = .data$note), size = 3, hjust = 1.05, vjust = 0.5,
      fill = "#fcfcfb", show.legend = FALSE
    ) +
    ggplot2::facet_wrap(ggplot2::vars(.data$panel), scales = "free") +
    ggplot2::scale_colour_manual(values = scenario_colors) +
    ggplot2::scale_y_continuous(labels = scales::label_dollar()) +
    ggplot2::scale_x_continuous(breaks = function(lim) base::seq(0, base::floor(lim[2]))) +
    ggplot2::labs(
      title = "One fully booked room day: contribution change as more of the day's cases get the add-on (national commercial)",
      subtitle = "Each point is a whole number of add-on cases. A drop marks a displaced primary case: the add-on minutes have used up the end-of-day slack.",
      x = "Scheduled primary cases that get the add-on (k)", y = "Change in day contribution",
      caption = "Rates are HPT negotiated facility rates used as a payment proxy. High added minutes = the top of the tested range."
    ) +
    theme_addon()
  ggplot2::ggsave(base::file.path(fig_dir, "addon_day_capacity.png"), day_figure, width = 11, height = 5.5, dpi = 150, bg = "#fcfcfb")
}

# ---- 5. PSA ----------------------------------------------------------------------

psa_rates <- rates |>
  dplyr::filter(.data$state == "US", .data$variant %in% headline_variants, !base::is.na(.data$primary_rate), !base::is.na(.data$procedure_rate), !base::is.na(.data$item_rate))
psa <- addon_psa(psa_rates, params, n = psa_draws)
psa$summary <- addon_mask_full_rate(psa$summary, noncovered_codes, cases)
write_csv_atomic(psa$summary, base::file.path(out_dir, "addon_psa_summary.csv"))

# ---- 6. payer and patient perspective ----------------------------------------

system_view <- addon_system_perspective(national, values)
write_csv_atomic(system_view, base::file.path(out_dir, "addon_system_perspective.csv"))

# ---- key results -------------------------------------------------------------------

base::message("\nBase case, national (net value per add-on, 2026 USD):")
national |>
  dplyr::filter(.data$variant %in% headline_variants) |>
  dplyr::select(
    "variant", "insurance_type", "R_P", "R_S_listed", "R_S", "C_S", "net_value",
    "net_value_room_cost", "net_value_listed_rate", "dT_star", "u_star", "revenue_per_minute_ratio"
  ) |>
  dplyr::mutate(dplyr::across(tidyselect::where(base::is.numeric), ~ base::round(.x, 2))) |>
  base::print(n = Inf, width = Inf)

base::message("\nPSA, probability the add-on is worth it (national):")
psa$summary |>
  dplyr::select("variant", "insurance_type", "mean_net_value", "net_value_p025", "net_value_p975", "prob_worth_it", "prob_worth_it_room_cost", "prob_worth_it_listed_rate") |>
  dplyr::mutate(dplyr::across(tidyselect::where(base::is.numeric), ~ base::round(.x, 3))) |>
  base::print(n = Inf, width = Inf)

base::message("\nStates net-positive (base variants, commercial and medicaid):")
state_summary |>
  dplyr::filter(.data$variant %in% headline_variants, .data$insurance_type %in% base::c("commercial", "medicaid")) |>
  base::print(n = Inf, width = Inf)

base::message("\nState rows with a usable net value: ", base::sum(!base::is.na(by_state$net_value) & by_state$state != "US"),
  " (", base::sum(by_state$primary_fallback & !base::is.na(by_state$net_value)), " use the national primary price)")
