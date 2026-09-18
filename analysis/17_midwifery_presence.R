#!/usr/bin/env Rscript
#' Is midwifery presence around a hospital related to its delivery prices and
#' its cesarean premium?
#'
#' Exposures (R/midwifery_link.R), per delivery hospital:
#' - CNMs/CMs within 30 miles per 1,000 births within 30 miles (tertiles
#'   across delivery hospitals, and per doubling);
#' - any CABC-accredited birth center within 30 miles;
#' - the county's CNM-attended share of births (CDC WONDER; counties of
#'   100,000+ only), as a sensitivity analysis.
#' Outcomes (from analysis/16): the hospital's commercial and Medicaid price
#' for an uncomplicated vaginal delivery (DRG 807) relative to Medicare, and
#' its cesarean premium (DRG 788 / DRG 807), on the log scale.
#' Models: log outcome ~ exposure + hospital type + system + ownership +
#' log beds + metro county + state fixed effects; 95% intervals and p values
#' from the wild cluster restricted bootstrap clustered by state
#' (wild_cluster_bootstrap() in R/ownership.R). Associations, not effects:
#' where midwives practice is not random.
#' The national AMCB-NPI linkage freeze covers all 50 states and DC, so no
#' hospital is dropped for reaching an uncovered state any more. The check
#' that used to drop them (roster_uncovered_zctas()) is still run, and must
#' come back empty.
#'
#' County NTSV cesarean rates against midwife supply are in
#' analysis/18_ntsv_midwife_supply.R.
#'
#' Writes to HPT_DATA_DIR/output/ (never committed): midwifery_presence_hospital.csv,
#' midwifery_tertile_summary.csv, midwifery_price_models.csv, and
#' figures/birth4_midwifery_presence.{png,pdf}.

base::source("R/00_source_all.R")

radius <- base::as.numeric(base::Sys.getenv("HPT_MIDWIFE_RADIUS_MILES", unset = "30"))
B <- base::as.integer(base::Sys.getenv("HPT_WCR_DRAWS", unset = "9999"))
out_dir <- hpt_path("output")
fig_dir <- hpt_path("output", "figures")
db_path <- hpt_database_path()

# ---- inputs -------------------------------------------------------------------

ratios <- readr::read_csv(base::file.path(out_dir, "birth_hospital_prices.csv"), col_types = readr::cols(ccn = "c", code = "c"), show_col_types = FALSE)
premium <- readr::read_csv(base::file.path(out_dir, "birth_cesarean_premium.csv"), col_types = readr::cols(ccn = "c"), show_col_types = FALSE)
hospitals <- duckdb_query("SELECT ccn, zip_code AS zip, state, hospital_type, hospital_ownership, health_sys_name FROM dim_hospital",
                          database = db_path, read_only = TRUE)
beds <- readr::read_csv(hpt_path("reference", "hospital_universe.csv"), col_types = readr::cols(.default = "c"), show_col_types = FALSE) |>
  dplyr::transmute(ccn = .data$facility_id, beds = base::suppressWarnings(base::as.numeric(.data$hos_beds)))

download_zcta_files()
zcta <- load_zcta_centroids()
zcta_county <- load_zcta_county()
roster <- load_midwife_roster()
national_roster_coverage(roster, strict = TRUE)
base::message("Midwife roster: ", base::nrow(roster), " active midwives, all 50 states and DC")
presence <- hospital_midwifery_presence(
  dplyr::filter(hospitals, .data$ccn %in% base::union(ratios$ccn, premium$ccn)) |> dplyr::select("ccn", "zip"),
  zcta, zcta_county, roster, load_birth_centers(), load_county_midwifery(),
  radius = radius, uncovered = roster_uncovered_zctas(zcta, zcta_county, roster)
)
base::message("Midwifery presence for ", base::nrow(presence), " delivery hospitals (radius ", radius, " miles); ",
              base::sum(!presence$roster_covered), " with a catchment reaching a state the roster does not cover (expected 0)")

covariates <- hospitals |>
  dplyr::left_join(beds, by = "ccn") |>
  dplyr::transmute(
    .data$ccn, .data$state,
    hospital_type = dplyr::if_else(.data$hospital_type == "Critical Access Hospitals", "critical access", "acute or other"),
    system = dplyr::if_else(base::is.na(.data$health_sys_name) | !base::nzchar(.data$health_sys_name), "independent", "in a system"),
    ownership = dplyr::case_when(
      stringr::str_detect(.data$hospital_ownership, "^Voluntary") ~ "nonprofit",
      stringr::str_detect(.data$hospital_ownership, "^Proprietary|^Physician") ~ "for-profit",
      TRUE ~ "government or other"
    ),
    log_beds = base::log(base::pmax(dplyr::coalesce(.data$beds, 25), 1))
  )

tertile_cuts <- stats::quantile(presence$cnm_per_1k_births, base::c(1 / 3, 2 / 3), na.rm = TRUE, names = FALSE)
analysis <- presence |>
  dplyr::mutate(
    cnm_tertile = base::cut(.data$cnm_per_1k_births, base::c(-Inf, tertile_cuts, Inf), labels = base::c("low", "middle", "high")),
    log2_cnm = base::log2(.data$cnm_per_1k_births + 0.5),
    birth_center = base::factor(dplyr::if_else(.data$bc_within > 0, "birth center nearby", "none nearby"),
                                levels = base::c("none nearby", "birth center nearby")),
    metro = dplyr::if_else(!base::is.na(.data$rucc_2023) & .data$rucc_2023 <= 3, "metro", "nonmetro")
  ) |>
  dplyr::left_join(covariates, by = "ccn")
write_csv_atomic(analysis, base::file.path(out_dir, "midwifery_presence_hospital.csv"))

outcomes <- dplyr::bind_rows(
  ratios |> dplyr::filter(.data$code == "807", .data$payer_type %in% base::c("commercial", "medicaid")) |>
    dplyr::transmute(.data$ccn, outcome = base::paste("Vaginal price / Medicare,", payer_label(.data$payer_type)), value = .data$ratio),
  premium |> dplyr::filter(.data$payer_type %in% base::c("commercial", "medicaid")) |>
    dplyr::transmute(.data$ccn, outcome = base::paste("Cesarean premium,", payer_label(.data$payer_type)), value = .data$premium_ratio)
) |>
  dplyr::filter(.data$value > 0) |>
  dplyr::inner_join(analysis, by = "ccn")

# ---- descriptive ----------------------------------------------------------------

tertile_summary <- outcomes |>
  dplyr::filter(!base::is.na(.data$cnm_tertile)) |>
  dplyr::group_by(.data$outcome, .data$cnm_tertile) |>
  dplyr::summarise(n_hospitals = dplyr::n(), median = stats::median(.data$value),
                   p25 = stats::quantile(.data$value, 0.25, names = FALSE), p75 = stats::quantile(.data$value, 0.75, names = FALSE),
                   median_cnm_per_1k = stats::median(.data$cnm_per_1k_births), .groups = "drop")
write_csv_atomic(tertile_summary, base::file.path(out_dir, "midwifery_tertile_summary.csv"))
base::print(tertile_summary, n = 40)
base::message("CNM-per-1,000-births tertile cut points: ", base::paste(base::round(tertile_cuts, 2), collapse = ", "))

# ---- adjusted models ------------------------------------------------------------

fit_terms <- function(data, rhs, terms) {
  data <- data |> dplyr::filter(dplyr::if_all(dplyr::all_of(base::all.vars(stats::as.formula(base::paste("~", rhs)))), ~ !base::is.na(.x)))
  X <- stats::model.matrix(stats::as.formula(base::paste("~", rhs, "+ state")), data = data)
  qr_X <- base::qr(X)
  X <- X[, qr_X$pivot[base::seq_len(qr_X$rank)], drop = FALSE]
  y <- base::log(data$value)
  dplyr::bind_rows(base::lapply(base::intersect(terms, base::colnames(X)), function(term) {
    boot <- wild_cluster_bootstrap(y, X, term, data$state, B = B)
    tibble::tibble(term = term, estimate = boot$estimate, pct = 100 * (base::exp(boot$estimate) - 1),
                   ci_low_pct = 100 * (base::exp(boot$ci_low) - 1), ci_high_pct = 100 * (base::exp(boot$ci_high) - 1),
                   p_value = boot$p_value, n_hospitals = base::nrow(data), n_states = boot$n_clusters)
  }))
}

adjust <- "hospital_type + system + ownership + log_beds + metro"
specs <- base::list(
  tertiles = base::list(rhs = base::paste("cnm_tertile + birth_center +", adjust),
                        terms = base::c("cnm_tertilemiddle", "cnm_tertilehigh", "birth_centerbirth center nearby")),
  per_doubling = base::list(rhs = base::paste("log2_cnm + birth_center +", adjust), terms = "log2_cnm")
)
models <- dplyr::bind_rows(base::lapply(base::unique(outcomes$outcome), function(o) {
  data <- dplyr::filter(outcomes, .data$outcome == o)
  dplyr::bind_rows(base::lapply(base::names(specs), function(s) {
    fit_terms(data, specs[[s]]$rhs, specs[[s]]$terms) |> dplyr::mutate(outcome = o, spec = s, .before = 1)
  }))
}))

# sensitivity: county CNM-attended share (large counties WONDER reports)
share <- outcomes |> dplyr::filter(.data$wonder_county_reported %in% TRUE, !base::is.na(.data$cnm_share_of_births_pct))
if (base::nrow(share) > 50) {
  models <- dplyr::bind_rows(models, dplyr::bind_rows(base::lapply(base::unique(share$outcome), function(o) {
    fit_terms(dplyr::filter(share, .data$outcome == o) |> dplyr::mutate(cnm_share_10pts = .data$cnm_share_of_births_pct / 10),
              base::paste("cnm_share_10pts +", adjust), "cnm_share_10pts") |>
      dplyr::mutate(outcome = o, spec = "county CNM birth share (per 10 points)", .before = 1)
  })))
}
write_csv_atomic(models, base::file.path(out_dir, "midwifery_price_models.csv"))
base::print(dplyr::select(models, "outcome", "spec", "term", "pct", "ci_low_pct", "ci_high_pct", "p_value", "n_hospitals"), n = 60)

# ---- figure ---------------------------------------------------------------------

plot_data <- outcomes |> dplyr::filter(!base::is.na(.data$cnm_tertile))
fig <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$cnm_tertile, y = .data$value)) +
  ggplot2::geom_boxplot(outlier.shape = NA, fill = "grey92", colour = "grey35", width = 0.55) +
  ggplot2::geom_jitter(width = 0.15, alpha = 0.25, size = 0.7, colour = "#2166ac") +
  ggplot2::facet_wrap(ggplot2::vars(.data$outcome), scales = "free_y", nrow = 1,
                      labeller = ggplot2::as_labeller(function(x) base::paste0(base::toupper(base::substr(x, 1, 1)), base::substring(x, 2)))) +
  ggplot2::scale_y_continuous(transform = "log", breaks = base::c(0.25, 0.5, 0.75, 1, 1.25, 1.5, 2, 3, 5, 10),
                              labels = scales::label_number(accuracy = 0.01, drop0trailing = TRUE, suffix = "x")) +
  ggplot2::labs(
    title = "Delivery prices and the cesarean premium by midwifery presence around the hospital",
    subtitle = base::sprintf("Tertiles of CNMs/CMs within %s miles per 1,000 births within %s miles (cut points %.2f and %.2f).",
                             radius, radius, tertile_cuts[1], tertile_cuts[2]),
    x = "Midwifery presence tertile", y = NULL,
    caption = base::paste0(
      "Prices: Trilliant Health Hospital MRF Data Directory (snapshot 2026-07-21). Midwives: AMCB-certified, NPPES practice ZIP. Births: NVSS (AHRF).\n",
      "Midwives: the national AMCB-NPI linkage freeze, all 50 states and DC, so no hospital is left out for reaching an uncovered state."
    )
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"), strip.text = ggplot2::element_text(face = "bold"),
                 plot.caption = ggplot2::element_text(size = 8, colour = "grey30", hjust = 0))
save_figure(fig, "birth4_midwifery_presence", width = 14, height = 5)
base::message("Figure: ", fig_dir)
