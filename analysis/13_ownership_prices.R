#!/usr/bin/env Rscript
#' Do private-equity (PE) owned hospitals post higher negotiated prices than
#' other hospitals for the same procedure, state, and payer type?
#'
#' Reads hpt.duckdb (read-only), CMS Hospital All Owners (downloaded on first
#' run to HPT_DATA_DIR/reference/cms_owners, with provenance), CMS Hospital
#' Enrollments (reference/cms_enrollments), the AHRQ CHSP linkage
#' (reference/ahrq), and the sourced PE system list
#' config/pe_hospital_systems.csv. Writes to HPT_DATA_DIR/output/:
#'   pe_system_ccn_map.csv              every system x CCN match (owner name, include list, CHSP-only)
#'   pe_system_counts.csv               hospitals per PE system, by method, type, and prices
#'   ownership_classification.csv       one row per roster CCN: PE markers, owners, groups
#'   ownership_pe_owners.csv            owners behind the PE markers, largest first
#'   ownership_counts.csv               hospitals by ownership group x hospital type / state
#'   ownership_hospital_prices.parquet  facility price per CCN x code x payer type, with ownership
#'   ownership_model_results.csv        adjusted % differences vs nonprofit, 95% CI, n
#'   ownership_summary.csv              raw medians and n by ownership group
#'   ownership_within_state.csv         within-state matched comparison (robustness)
#'   pe_system_price_ratios.csv         each PE system's prices vs same-state nonprofit medians
#'   figures/ownership_forest.png       forest plot of adjusted differences
#' Methods: docs/ownership_methods.md.
#'
#' Env var HPT_PE_CCN_LIST: optional CSV with a `ccn` column of hospitals the
#' researcher knows to be PE owned (e.g. from a PE deal tracker). When set,
#' the "researcher_list" definition (PE or fund, plus those CCNs) is added.

base::source("R/00_source_all.R")
if (!base::exists("classify_hospital_ownership", mode = "function")) {
  base::source("R/ownership.R")
}

out_dir <- hpt_path("output")
fig_dir <- hpt_path("output", "figures")
base::dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
db_path <- hpt_database_path()

# ---- 1. ownership data --------------------------------------------------------

owners_download <- download_hospital_owners()
owners <- owners_download$owners
enrollments_path <- latest_reference_file(hpt_path("reference", "cms_enrollments"), "^Hospital_Enrollments_.*\\.csv$")
enrollments <- load_hospital_enrollments(enrollments_path)$enrollments
base::message("Owners file: ", base::basename(owners_download$path), "; enrollments file: ", base::basename(enrollments_path))
if (!base::identical(stringr::str_extract(owners_download$path, "[0-9.]{10}(?=\\.csv$)"), stringr::str_extract(enrollments_path, "[0-9.]{10}(?=\\.csv$)"))) {
  base::warning("Hospital All Owners and Hospital Enrollments come from different releases.")
}
unlinked <- base::setdiff(base::unique(owners$enrollment_id), enrollments$enrollment_id)
base::message("Owner enrollment ids without an enrollment row: ", base::length(unlinked))

roster <- duckdb_query("SELECT * FROM dim_hospital", database = db_path, read_only = TRUE)

# ---- 2. classification ----------------------------------------------------------

extra_path <- base::Sys.getenv("HPT_PE_CCN_LIST", unset = "")
extra_pe_ccns <- if (base::nzchar(extra_path)) read_csv_chr(extra_path)$ccn else NULL
# the CMS-flag share and any-role variants select the same roster hospitals
# as cms_flag in the 2026-08 release; they stay available but are not run
definitions <- base::c("pe_strict", "pe_broad", "pe_broad_all", "cms_flag", "fund_name")
if (base::length(extra_pe_ccns) > 0L) {
  definitions <- base::c(definitions, "researcher_list")
  base::message("Researcher PE list: ", base::length(base::unique(extra_pe_ccns)), " CCNs from ", extra_path)
}

systems <- load_pe_systems("config/pe_hospital_systems.csv")
system_map <- map_pe_systems(systems, owner_pe_rows(owners, enrollments), roster)
pe_systems <- assign_pe_systems(system_map, systems)
classification <- classify_hospital_ownership(owners, enrollments, roster, pe_systems = pe_systems, extra_pe_ccns = extra_pe_ccns)
write_csv_atomic(classification, base::file.path(out_dir, "ownership_classification.csv"))
write_csv_atomic(ownership_counts(classification, definitions), base::file.path(out_dir, "ownership_counts.csv"))

base::message("Roster hospitals by PE marker:")
classification |>
  dplyr::summarise(dplyr::across(base::c("pe_cms", "pe_cms_direct", "pe_cms_indirect", "pe_cms_5pct", "pe_cms_any_role", "pe_fund"), base::sum)) |>
  base::print()
base::message("Roster group disagreements with PECOS proprietary/nonprofit (P/N):")
classification |>
  dplyr::count(.data$roster_group, .data$pecos_proprietary_nonprofit) |>
  base::print(n = 40)

# ---- 3. hospital prices ---------------------------------------------------------

exclude_path <- hpt_path("output", "median_excluded_file_ids.csv")
exclude <- if (base::file.exists(exclude_path)) read_csv_chr(exclude_path)$mrf_file_id else NULL
base::message("Excluding ", base::length(exclude), " file(s) listed in ", exclude_path)

available_codes <- duckdb_query("SELECT DISTINCT code FROM dim_code", database = db_path, read_only = TRUE)$code
missing_codes <- base::setdiff(base::names(ownership_codes()), available_codes)
if (base::length(missing_codes) > 0L) {
  base::message("Not in this database yet: ", base::paste(missing_codes, collapse = ", "))
}

prices <- ownership_hospital_prices(db_path, exclude_file_ids = exclude)
hospital_prices <- prices |>
  dplyr::inner_join(
    dplyr::select(classification, "ccn", "facility_name", "state", "hospital_type", "hospital_ownership", "health_sys_id",
                  "health_sys_name", "roster_group", dplyr::starts_with("pe_"), dplyr::starts_with("ownership_group_")),
    by = "ccn"
  )
write_parquet_atomic(hospital_prices, base::file.path(out_dir, "ownership_hospital_prices.parquet"))

owners_tbl <- largest_pe_owners(owners, enrollments, roster, priced_ccns = base::unique(prices$ccn))
write_csv_atomic(owners_tbl, base::file.path(out_dir, "ownership_pe_owners.csv"))

system_map_out <- system_map |>
  dplyr::left_join(dplyr::select(roster, "ccn", "facility_name", "state", "hospital_type", "health_sys_name"), by = "ccn") |>
  dplyr::mutate(priced = .data$ccn %in% prices$ccn)
write_csv_atomic(system_map_out, base::file.path(out_dir, "pe_system_ccn_map.csv"))
system_counts <- pe_system_counts(systems, system_map, pe_systems, roster, priced_ccns = base::unique(prices$ccn))
write_csv_atomic(system_counts, base::file.path(out_dir, "pe_system_counts.csv"))
base::message("Hospitals per PE system:")
system_counts |>
  dplyr::filter(.data$classification != "exclude") |>
  dplyr::select("system_id", "classification", "ambiguity", "n_ccns", "n_roster", "n_acute_or_cah", "n_priced", "n_in_chsp", "n_chsp_only") |>
  base::print(n = 40)

# ---- 4. models and robustness ---------------------------------------------------

chsp <- load_chsp_linkage(hpt_path("reference", "ahrq", "chsp-hospital-linkage-2023.csv"))
frame <- ownership_model_frame(prices, classification, chsp)
engine <- ownership_model_engine()
base::message("Model engine: ", engine, if (engine == "lm") " (no clustered SEs: install sandwich + lmtest)" else "")

# the forest series get a wild cluster restricted bootstrap (9,999 Webb draws,
# seeded per coefficient); the rest keep CRV1 intervals
results <- ownership_models(frame, definitions, engine = engine)
summary_tbl <- ownership_raw_summary(frame, definitions)
within_state <- ownership_within_state(frame, definitions)
write_csv_atomic(results, base::file.path(out_dir, "ownership_model_results.csv"))
write_csv_atomic(summary_tbl, base::file.path(out_dir, "ownership_summary.csv"))
write_csv_atomic(within_state, base::file.path(out_dir, "ownership_within_state.csv"))
write_csv_atomic(ownership_system_ratios(frame), base::file.path(out_dir, "pe_system_price_ratios.csv"))

forest <- plot_ownership_forest(results)
ggplot2::ggsave(base::file.path(fig_dir, "ownership_forest.png"), forest, width = 8.5, height = 11, dpi = 200, bg = "white")

# ---- 5. headline ------------------------------------------------------------------

base::message("Adjusted % difference vs nonprofit (commercial and Medicaid): CRV1 and wild cluster bootstrap 95% CIs")
results |>
  dplyr::filter(.data$term %in% base::c("pe", "for_profit_non_pe"), .data$payer_type %in% base::c("commercial", "medicaid"),
                .data$definition %in% base::c("pe_strict", "pe_broad"), .data$payment_comparison) |>
  dplyr::filter(!(.data$definition == "pe_broad" & .data$term == "for_profit_non_pe")) |>
  dplyr::transmute(
    .data$definition, .data$term, .data$code, .data$payer_type, .data$n_group, .data$n_group_clusters, .data$n_hospitals, .data$n_clusters,
    pct = base::sprintf("%+.1f%%", 100 * .data$pct_diff),
    crv1 = base::sprintf("%+.1f%% to %+.1f%%", 100 * .data$pct_ci_low, 100 * .data$pct_ci_high),
    wcr = base::sprintf("%+.1f%% to %+.1f%%", 100 * .data$pct_wcr_ci_low, 100 * .data$pct_wcr_ci_high),
    wcr_p = base::round(.data$wcr_p_value, 3),
    .data$exploratory
  ) |>
  base::print(n = 60, width = 200)
base::message("Results: ", base::file.path(out_dir, "ownership_model_results.csv"))
