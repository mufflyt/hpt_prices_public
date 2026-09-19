#' Audit the CDC WONDER exports before the analysis reads them
#'
#' The ten exports are built by hand in a web form, one dropdown at a time,
#' and a wrong one does not look wrong. Grouping by "Age of Mother 10"
#' instead of "Age of Mother 9", or leaving the year on 2022-2024 for the
#' placebo, produces a well-formed file with plausible counts that would flow
#' through the whole analysis and change the answer.
#'
#' `analysis/18` checked only that the NTSV filters appear in the Notes, and
#' it stopped at the first problem. This audits every export against the
#' specification it is supposed to satisfy, reports all the problems at once,
#' and is cheap enough to run the moment the files land rather than twenty
#' minutes into a model fit.
#'
#' Everything here reads the Notes block that CDC writes into the export
#' itself, not the request form, so it checks what the file actually is.

#' The Notes block of a WONDER export, quotes stripped
wonder_notes <- function(path) {
  lines <- base::readLines(path, warn = FALSE)
  at <- base::which(stringr::str_detect(lines, '^"?---'))[1]
  if (base::is.na(at)) return(base::character())
  stringr::str_remove_all(lines[at:base::length(lines)], '"')
}

#' One labelled value out of the Notes block, e.g. "Group By" or "Year"
wonder_note_value <- function(notes, label) {
  hit <- base::grep(base::paste0("^", label, ":"), notes, value = TRUE)[1]
  if (base::is.na(hit)) return(NA_character_)
  stringr::str_squish(stringr::str_remove(hit, base::paste0("^", label, ":")))
}

#' What a correctly filtered export's Notes say
#'
#' Verified against a live D149 export on 2026-09-19. These are the request
#' form's option labels, which differ from the natality help page's wording;
#' taking them from the help page is what made the check reject every correct
#' export. Defined here so `analysis/18` and the preflight cannot drift.
wonder_ntsv_filter_marks <- function() {
  base::c(
    "^Live Birth Order: 1$",
    "^Plurality: Single$",
    "^Fetal Presentation: Cephalic$",
    "^OE Gestational Age Recode 11:(?=.*37-38 weeks)(?=.*39 weeks)(?=.*40 weeks)(?=.*41 weeks)(?=.*42 weeks or more)"
  )
}

#' Are the NTSV restrictions present in this Notes block?
wonder_has_ntsv_filters <- function(notes) {
  base::all(base::vapply(wonder_ntsv_filter_marks(),
                         function(m) base::any(stringr::str_detect(notes, m)), base::logical(1)))
}

#' Split a "a; b; c" Notes value into a trimmed set
wonder_split <- function(x) {
  if (base::is.na(x)) return(base::character())
  base::sort(stringr::str_squish(base::unlist(stringr::str_split(x, ";"))))
}

#' Expand a "2022-2024" specification into the years it names
wonder_expand_years <- function(spec) {
  parts <- base::as.integer(base::unlist(stringr::str_split(spec, "-")))
  base::as.character(base::seq(parts[[1]], parts[[base::length(parts)]]))
}

#' Audit every expected export
#'
#' Returns one row per export in `wonder_ntsv_exports()`, with a `problem`
#' column naming everything wrong with it, empty when it is correct. Reports
#' rather than stops, so one pass shows every file that needs redoing.
#'
#' @param dir folder holding the exports.
wonder_export_audit <- function(dir = wonder_export_dir()) {
  spec <- wonder_ntsv_exports()
  dplyr::bind_rows(base::lapply(base::seq_len(base::nrow(spec)), function(i) {
    r <- spec[i, ]
    path <- base::file.path(dir, r$file)
    row <- tibble::tibble(key = r$key, file = r$file, role = r$role, present = base::file.exists(path),
                          rows = NA_integer_, n_geo = NA_integer_, dataset = NA_character_,
                          query_date = NA_character_, sha256 = NA_character_, problem = NA_character_)
    if (!row$present) {
      row$problem <- if (r$role == "required") "missing (required)" else "missing (optional, will be skipped)"
      return(row)
    }
    row$sha256 <- sha256_file(path)
    notes <- wonder_notes(path)
    problems <- base::character()

    tbl <- base::tryCatch(read_wonder_export(path), error = function(e) e)
    if (base::inherits(tbl, "error")) {
      row$problem <- base::paste0("does not parse: ", base::conditionMessage(tbl))
      return(row)
    }
    row$rows <- base::nrow(tbl)
    geo_col <- base::grep("Code$", base::names(tbl), value = TRUE)[1]
    if (!base::is.na(geo_col)) row$n_geo <- dplyr::n_distinct(tbl[[geo_col]])
    row$dataset <- wonder_note_value(notes, "Dataset")
    row$query_date <- wonder_note_value(notes, "Query Date")

    if (base::length(notes) == 0L) {
      problems <- base::c(problems, "no Notes block (exported as TSV or CSV rather than XLS?)")
    } else {
      got_by <- wonder_split(wonder_note_value(notes, "Group By"))
      want_by <- wonder_split(r$group_by)
      if (!base::identical(got_by, want_by)) {
        problems <- base::c(problems, base::sprintf("grouped by [%s], expected [%s]",
                                                    base::paste(got_by, collapse = "; "),
                                                    base::paste(want_by, collapse = "; ")))
      }
      got_years <- wonder_split(wonder_note_value(notes, "Year"))
      want_years <- wonder_expand_years(r$years)
      if (!base::identical(got_years, base::sort(want_years))) {
        problems <- base::c(problems, base::sprintf("years [%s], expected [%s]",
                                                    base::paste(got_years, collapse = ", "),
                                                    base::paste(want_years, collapse = ", ")))
      }
      has <- wonder_has_ntsv_filters(notes)
      if (has && !r$ntsv_filters) {
        problems <- base::c(problems, "carries the NTSV filters but must not: this is the all-births negative control")
      }
      if (!has && r$ntsv_filters) {
        problems <- base::c(problems, "NTSV filters absent or incomplete (all five gestational-age categories are required)")
      }
      if (!base::is.na(row$dataset) && !stringr::str_detect(row$dataset, "expanded")) {
        problems <- base::c(problems, base::sprintf("dataset is '%s', expected the expanded natality file", row$dataset))
      }
    }
    if (row$rows == 0L) problems <- base::c(problems, "no data rows")
    # A county export reports only counties of 100,000+ residents, about 600
    # of them. An order of magnitude below that means the geography dropdown
    # was wrong, or a location filter was left set.
    if (stringr::str_detect(r$group_by, "^County") && !base::is.na(row$n_geo) && row$n_geo < 100L) {
      problems <- base::c(problems, base::sprintf("only %d counties; expected several hundred", row$n_geo))
    }
    if (stringr::str_detect(r$group_by, "^State") && !base::is.na(row$n_geo) && row$n_geo < 50L) {
      problems <- base::c(problems, base::sprintf("only %d states; expected 50 plus DC", row$n_geo))
    }
    row$problem <- if (base::length(problems)) base::paste(problems, collapse = "; ") else NA_character_
    row
  }))
}

#' Print the audit and say whether the analysis can run
#'
#' @return TRUE when every required export is present and correct.
wonder_audit_report <- function(audit = wonder_export_audit()) {
  for (i in base::seq_len(base::nrow(audit))) {
    a <- audit[i, ]
    mark <- if (base::is.na(a$problem)) "ok  " else if (a$role == "required") "FAIL" else "warn"
    base::message(base::sprintf("  %s  %-42s %s", mark, a$file,
                                if (base::is.na(a$problem)) {
                                  base::sprintf("%s rows, %s geographies", a$rows, a$n_geo)
                                } else a$problem))
  }
  bad_required <- dplyr::filter(audit, .data$role == "required", !base::is.na(.data$problem))
  ok <- base::nrow(bad_required) == 0L
  if (ok) {
    base::message("All required WONDER exports present and consistent with the specification.")
    return(TRUE)
  }
  # A file that was never made and one built from the wrong dropdown need
  # different things said about them, and the second is the dangerous case.
  absent <- dplyr::filter(bad_required, !.data$present)
  wrong <- dplyr::filter(bad_required, .data$present)
  if (base::nrow(absent)) {
    base::message(base::nrow(absent), " required export(s) not downloaded yet: ",
                  base::paste(absent$file, collapse = ", "))
  }
  if (base::nrow(wrong)) {
    base::message(base::nrow(wrong), " required export(s) present but inconsistent with the specification, ",
                  "re-export these: ", base::paste(wrong$file, collapse = ", "))
  }
  FALSE
}
