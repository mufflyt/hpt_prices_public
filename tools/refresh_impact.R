#!/usr/bin/env Rscript
#' What moved in the headline numbers between two builds
#'
#' Reads a copy of `output/` taken before a rebuild and the live one after it,
#' and prints a markdown table of the numbers the documents quote. The point is
#' to notice a number that changed and was never written down, which is how
#' docs/appendix.md ended up quoting a DRG 742 median from a build two rules
#' out of date.
#'
#' Usage (from the repository root):
#'   Rscript tools/refresh_impact.R <before_dir> [after_dir]
#' after_dir defaults to HPT_DATA_DIR/output.

base::source("R/00_source_all.R")

args <- base::commandArgs(trailingOnly = TRUE)
if (base::length(args) < 1L) base::stop("Usage: Rscript tools/refresh_impact.R <before_dir> [after_dir]")
before_dir <- args[[1]]
after_dir <- if (base::length(args) > 1L) args[[2]] else hpt_path("output")

read_if <- function(dir, file) {
  path <- base::file.path(dir, file)
  if (!base::file.exists(path)) return(NULL)
  readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE)
}

num <- function(x) base::suppressWarnings(base::as.numeric(x))

#' One headline number from a file, or NA when the build has no such row
pick <- function(dir, file, filters, column) {
  tbl <- read_if(dir, file)
  if (base::is.null(tbl)) return(NA_real_)
  for (nm in base::names(filters)) {
    if (!nm %in% base::names(tbl)) return(NA_real_)
    tbl <- tbl[tbl[[nm]] == filters[[nm]], , drop = FALSE]
  }
  if (base::nrow(tbl) == 0L || !column %in% base::names(tbl)) return(NA_real_)
  num(tbl[[column]][[1]])
}

metrics <- tibble::tribble(
  ~label,                                   ~file,                          ~filters,                                                              ~column,        ~digits,
  "Colonoscopy 45378, commercial",          "state_insurance_medians.csv",  base::list(state = "US", code = "45378", insurance_type = "commercial"), "median_price", 0,
  "Colonoscopy 45378, Medicaid",            "state_insurance_medians.csv",  base::list(state = "US", code = "45378", insurance_type = "medicaid"),   "median_price", 0,
  "EMB 58100, commercial",                  "state_insurance_medians.csv",  base::list(state = "US", code = "58100", insurance_type = "commercial"), "median_price", 0,
  "IUD 58300, commercial",                  "state_insurance_medians.csv",  base::list(state = "US", code = "58300", insurance_type = "commercial"), "median_price", 0,
  "MS-DRG 621 bariatric, commercial",       "state_insurance_medians.csv",  base::list(state = "US", code = "621", insurance_type = "commercial"),   "median_price", 0,
  "MS-DRG 742 uterine, commercial",         "state_insurance_medians.csv",  base::list(state = "US", code = "742", insurance_type = "commercial"),   "median_price", 0,
  "Vaginal delivery 807, commercial",       "birth_national_summary.csv",   base::list(code = "807", payer_type = "commercial"),                     "median_price", 0,
  "Vaginal delivery 807, Medicaid",         "birth_national_summary.csv",   base::list(code = "807", payer_type = "medicaid"),                       "median_price", 0,
  "Cesarean 788, commercial",               "birth_national_summary.csv",   base::list(code = "788", payer_type = "commercial"),                     "median_price", 0,
  "Vaginal 807 ratio to Medicare, commercial", "birth_national_summary.csv", base::list(code = "807", payer_type = "commercial"),                    "median_ratio", 3,
  "Cesarean premium, commercial",           "birth_premium_summary.csv",    base::list(state = "US", payer_type = "commercial"),                     "median_ratio", 3,
  "Cesarean premium dollars, commercial",   "birth_premium_summary.csv",    base::list(state = "US", payer_type = "commercial"),                     "median_dollars", 0
)

rows <- base::lapply(base::seq_len(base::nrow(metrics)), function(i) {
  m <- metrics[i, ]
  before <- pick(before_dir, m$file, m$filters[[1]], m$column)
  after <- pick(after_dir, m$file, m$filters[[1]], m$column)
  tibble::tibble(
    metric = m$label, before = before, after = after,
    change = after - before,
    pct = dplyr::if_else(base::is.na(before) | before == 0, NA_real_, 100 * (after - before) / before),
    digits = m$digits
  )
})
impact <- dplyr::bind_rows(rows)

fmt <- function(x, digits) dplyr::if_else(base::is.na(x), "n/a", base::formatC(x, format = "f", digits = digits, big.mark = ","))

base::cat("| Output | Before | After | Change |\n|---|---|---|---|\n")
for (i in base::seq_len(base::nrow(impact))) {
  r <- impact[i, ]
  change <- if (base::is.na(r$change)) "n/a" else if (r$change == 0) "unchanged" else {
    base::sprintf("%s%s (%+.1f%%)", if (r$change > 0) "+" else "", fmt(r$change, r$digits), r$pct)
  }
  base::cat(base::sprintf("| %s | %s | %s | %s |\n", r$metric, fmt(r$before, r$digits), fmt(r$after, r$digits), change))
}

moved <- impact |> dplyr::filter(!base::is.na(.data$change), .data$change != 0)
base::cat(base::sprintf("\n%s of %s headline numbers moved.\n", base::nrow(moved), base::nrow(impact)))
if (base::nrow(moved) > 0L) {
  base::cat("Every one of these is quoted somewhere in README.md, NEWS.md, CHANGELOG.md or docs/;\n")
  base::cat("update those before the numbers and the prose disagree.\n")
}
