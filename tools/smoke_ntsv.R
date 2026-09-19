#!/usr/bin/env Rscript
#' Smoke test of analysis/18_ntsv_midwife_supply.R on synthetic CDC WONDER exports
#'
#' Runs the whole NTSV analysis end to end before the real WONDER exports
#' exist, without touching the real data folder:
#' - builds a temporary HPT_DATA_DIR whose reference/ subfolders, hpt.duckdb,
#'   and output/birth_cesarean_premium.csv are symlinks to the real ones,
#'   except reference/cdc_wonder, which holds synthetic exports;
#' - writes every required export (and the negative-control one) in WONDER's
#'   tab-delimited layout, with a Notes block carrying the NTSV filters, for
#'   the counties of 100,000+ residents in the 2020 Census centers of
#'   population file (Connecticut and Puerto Rico left out, as in WONDER
#'   2022-2024);
#' - runs analysis/18 with a small bootstrap and prints its tables;
#' - deletes the temporary folder.
#'
#' Every birth count is random. The estimates mean nothing; the point is that
#' the reader, rates, composition shares, supply, models, and the implied
#' differential all run on real-shaped input. Needs the real data drive (or
#' HPT_REAL_DATA_DIR) with hpt.duckdb, analysis/16 outputs, and the midwifery
#' repository inputs.
#'
#' Usage (from the repository root): Rscript tools/smoke_ntsv.R

if (!base::file.exists("R/00_source_all.R")) base::stop("Run from the repository root.")
real <- base::Sys.getenv("HPT_REAL_DATA_DIR", unset = "/Volumes/MufflySamsung 1/hpt_prices")
for (need in base::c("hpt.duckdb", "output/birth_cesarean_premium.csv", "reference")) {
  if (!base::file.exists(base::file.path(real, need))) base::stop("Missing ", base::file.path(real, need), "; run analysis/09 and 16 first.")
}

smoke <- base::tempfile("ntsv_smoke_")
base::dir.create(base::file.path(smoke, "reference", "cdc_wonder"), recursive = TRUE)
base::dir.create(base::file.path(smoke, "output"))
base::on.exit(base::unlink(smoke, recursive = TRUE), add = TRUE)
for (d in base::list.files(base::file.path(real, "reference"), full.names = TRUE)) {
  if (base::basename(d) != "cdc_wonder") base::file.symlink(d, base::file.path(smoke, "reference", base::basename(d)))
}
base::file.symlink(base::file.path(real, "hpt.duckdb"), base::file.path(smoke, "hpt.duckdb"))
base::file.symlink(base::file.path(real, "output", "birth_cesarean_premium.csv"), base::file.path(smoke, "output", "birth_cesarean_premium.csv"))
base::Sys.setenv(HPT_DATA_DIR = smoke)

base::suppressMessages(base::source("R/00_source_all.R"))

# ---- synthetic exports --------------------------------------------------------------

centers_path <- download_county_population_centers()
cenpop <- readr::read_csv(centers_path, col_types = readr::cols(.default = "c"), show_col_types = FALSE)
base::names(cenpop) <- base::sub("^﻿", "", base::names(cenpop))
postal <- state_fips_postal()
counties <- tibble::tibble(fips = base::paste0(cenpop$STATEFP, cenpop$COUNTYFP),
                           name = base::paste0(cenpop$COUNAME, " County, ", postal[cenpop$STATEFP]),
                           pop = base::as.numeric(cenpop$POPULATION), st = cenpop$STATEFP) |>
  dplyr::filter(.data$pop >= 100000, !.data$st %in% base::c("09", "72"))

base::set.seed(20260914)
wonder_dir <- wonder_export_dir()
notes <- function(ntsv) {
  base::c('"---"', '"Dataset: Natality, 2016-2024 expanded"', '"Query Parameters:"',
          # exactly as CDC WONDER writes them, verified against a live D149
          # export on 2026-09-19; the request form's option label for birth
          # order is "1", and the Notes record it that way
          if (ntsv) base::c('"Live Birth Order: 1"', '"Plurality: Single"', '"Fetal Presentation: Cephalic"',
                            '"OE Gestational Age Recode 11: 37-38 weeks; 39 weeks; 40 weeks; 41 weeks; 42 weeks or more"'),
          '"Query Date: synthetic smoke test"', '"---"')
}
#' One export: `categories` is a list of category vectors (one per grouping
#' column after the geography), `probs` a geography x category matrix
write_export <- function(file, geo, columns, categories, probs, size, ntsv = TRUE, geo_label = "County of Residence") {
  blanks <- base::paste(base::rep("", base::length(columns)), collapse = "\t")
  rows <- base::unlist(base::lapply(base::seq_len(base::nrow(geo)), function(i) {
    n <- base::round(size[i])
    k <- base::as.vector(stats::rmultinom(1, n, probs[i, ]))
    cell <- base::ifelse(k > 0 & k < 10, "Suppressed", base::as.character(k))
    cats <- base::vapply(categories, function(x) base::paste0('"', x, '"', collapse = "\t"), "")
    base::c(base::sprintf('\t"%s"\t"%s"\t%s\t%s', geo$name[i], geo$fips[i], cats, cell),
            base::sprintf('"Total"\t"%s"\t"%s"\t%s\t%d', geo$name[i], geo$fips[i], blanks, n))
  }))
  header <- base::paste0('"', base::c("Notes", geo_label, base::paste(geo_label, "Code"), columns, "Births"), '"', collapse = "\t")
  base::writeLines(base::c(header, rows, notes(ntsv)), base::file.path(wonder_dir, file))
}
single <- function(x) base::lapply(x, function(v) v)
n <- base::nrow(counties)
size <- counties$pop * 0.004 * 3
ces <- stats::plogis(stats::qlogis(0.26) + stats::rnorm(n, 0, 0.2))
method <- single(base::c("Vaginal", "Cesarean", "Unknown or Not Stated"))
share_matrix <- function(v) base::matrix(v, n, base::length(v), byrow = TRUE)

write_export("ntsv_county_2022_2024.txt", counties, "Delivery Method", method, base::cbind(1 - ces - 0.001, ces, 0.001), size)
write_export("ntsv_county_2016_2019.txt", counties, "Delivery Method", method, base::cbind(1 - ces - 0.001, ces, 0.001), size * 4 / 3)
states <- counties |> dplyr::distinct(.data$st) |> dplyr::transmute(fips = .data$st, name = base::unname(postal[.data$st]))
state_ces <- stats::runif(base::nrow(states), 0.22, 0.30)
write_export("ntsv_state_2022_2024.txt", states, "Delivery Method", method, base::cbind(1 - state_ces - 0.001, state_ces, 0.001),
             base::rep(3e4, base::nrow(states)), geo_label = "State of Residence")
write_export("ntsv_county_age_2022_2024.txt", counties, "Age of Mother 9",
             single(base::c("Under 15 years", "15-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years")),
             share_matrix(base::c(0.002, 0.08, 0.2, 0.3, 0.28, 0.11, 0.028)), size)
write_export("ntsv_county_payment_2022_2024.txt", counties, "Source of Payment for Delivery",
             single(base::c("Medicaid", "Private Insurance", "Self Pay", "Other", "Unknown or Not Stated")),
             share_matrix(base::c(0.38, 0.52, 0.04, 0.04, 0.02)), size)
write_export("ntsv_county_race_2022_2024.txt", counties, base::c("Mother's Hispanic Origin", "Mother's Single Race 6"),
             base::list(base::c("Hispanic or Latino", "White"), base::c("Not Hispanic or Latino", "White"),
                        base::c("Not Hispanic or Latino", "Black or African American"), base::c("Not Hispanic or Latino", "Asian"),
                        base::c("Origin unknown or not stated", "White")),
             share_matrix(base::c(0.25, 0.5, 0.14, 0.08, 0.03)), size)
write_export("ntsv_county_bmi_2022_2024.txt", counties, "Mother's Pre-pregnancy BMI",
             single(base::c("Normal 18.5-24.9", "Overweight 25.0-29.9", "Obesity I 30.0-34.9", "Unknown or Not Stated")),
             share_matrix(base::c(0.45, 0.25, 0.27, 0.03)), size)
write_export("births_county_plurality_2022_2024.txt", counties, "Plurality", single(base::c("Single", "Twin", "Triplet or more")),
             share_matrix(base::c(0.968, 0.031, 0.001)), size * 2.5, ntsv = FALSE)
base::message("Synthetic WONDER exports for ", n, " counties in ", wonder_dir)

# ---- run analysis/18 ------------------------------------------------------------------

status <- base::system2("Rscript", "analysis/18_ntsv_midwife_supply.R",
                        env = base::c(base::paste0("HPT_DATA_DIR=", base::shQuote(smoke)), "HPT_WCR_DRAWS=199"))
if (status != 0) base::stop("analysis/18 failed on synthetic exports (exit ", status, ")")
base::message("Smoke test passed: analysis/18 ran end to end on synthetic exports.")
