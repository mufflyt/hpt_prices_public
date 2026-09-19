#!/usr/bin/env Rscript
#' Check the CDC WONDER exports before running analysis/18
#'
#' Run this the moment the files land. It reads each export's own Notes block
#' and checks it against the specification: the right grouping, the right
#' years, the NTSV restrictions present where they belong and absent from the
#' negative control, and a plausible number of geographies.
#'
#' Why it is separate from analysis/18: the exports are built by hand in a web
#' form and a wrong one does not look wrong. Grouping by "Age of Mother 10"
#' instead of "Age of Mother 9", or leaving the placebo on 2022-2024, gives a
#' well-formed file with believable counts that would change the answer with
#' nothing to flag it. This reports every problem at once, in seconds, rather
#' than stopping at the first one twenty minutes into a model fit.
#'
#' Usage, from the repository root:
#'   Rscript tools/check_wonder_exports.R
#' Exits 1 when a required export is missing or wrong, so it can gate a run.

base::source("R/00_source_all.R")

dir <- wonder_export_dir()
base::message("CDC WONDER exports in ", dir)
if (!base::dir.exists(dir)) {
  base::message("That folder does not exist yet. Create it and put the ten exports there;")
  base::message("analysis/18 prints the exact query for each one.")
  base::quit(status = 1L)
}

audit <- wonder_export_audit(dir)
ok <- wonder_audit_report(audit)

out <- base::file.path(hpt_path("output"), "ntsv_wonder_audit.csv")
base::dir.create(base::dirname(out), recursive = TRUE, showWarnings = FALSE)
write_csv_atomic(audit, out)
base::message("Audit written to ", out)

if (!ok) {
  base::message("")
  base::message("The NTSV filters persist across queries within a WONDER session, so for most")
  base::message("exports only the Group Results By dropdown changes. Two exceptions: the placebo")
  base::message("export uses years 2016-2019, and the plurality export must have the NTSV")
  base::message("filters cleared, so do that one last.")
}
base::quit(status = if (ok) 0L else 1L)
