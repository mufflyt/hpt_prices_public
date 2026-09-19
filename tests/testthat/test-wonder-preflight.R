#' The preflight exists to catch a WONDER export built from the wrong
#' dropdown, so every test here builds a deliberately wrong one and checks
#' that it is refused. The shapes come from a live D149 export of the county
#' outcome taken on 2026-09-19.

wonder_fixture <- function(dir, file, group_cols, group_by_note, years, ntsv = TRUE,
                           geos = base::sprintf("%05d", 1000 + base::seq_len(200)), rows_per_geo = 2L) {
  # WONDER emits a label column and a Code column for every grouping variable,
  # then Births last: Notes / geo / geo Code / cat / cat Code / ... / Births
  header <- base::paste0('"', base::c("Notes",
                                      base::unlist(base::lapply(group_cols, function(c) base::c(c, base::paste(c, "Code")))),
                                      "Births"), '"', collapse = "\t")
  body <- base::unlist(base::lapply(geos, function(g) {
    base::vapply(base::seq_len(rows_per_geo), function(k) {
      cells <- base::c(base::sprintf('"County %s"\t"%s"', g, g),
                       base::vapply(base::seq_along(group_cols)[-1], function(j) {
                         base::sprintf('"category %d"\t"%d"', if (j == base::length(group_cols)) k else 1L,
                                       if (j == base::length(group_cols)) k else 1L)
                       }, base::character(1)))
      base::sprintf('\t%s\t%d', base::paste(cells, collapse = "\t"), base::sample(100:900, 1))
    }, base::character(1))
  }))
  notes <- base::c('"---"', '"Dataset: Natality, 2016-2024 expanded"', '"Query Parameters:"',
                   if (ntsv) base::c('"Live Birth Order: 1"', '"Plurality: Single"', '"Fetal Presentation: Cephalic"',
                                     '"OE Gestational Age Recode 11: 37-38 weeks; 39 weeks; 40 weeks; 41 weeks; 42 weeks or more"'),
                   base::sprintf('"Year: %s"', base::paste(years, collapse = "; ")),
                   base::sprintf('"Group By: %s"', group_by_note),
                   '"Query Date: fixture"', '"---"')
  base::writeLines(base::c(header, body, notes), base::file.path(dir, file))
}

#' A folder holding a correct version of every required export
correct_exports <- function() {
  dir <- base::tempfile("wonder_"); base::dir.create(dir)
  spec <- wonder_ntsv_exports()
  for (i in base::seq_len(base::nrow(spec))) {
    r <- spec[i, ]
    parts <- stringr::str_squish(base::unlist(stringr::str_split(r$group_by, ";")))
    geos <- if (stringr::str_detect(r$group_by, "^State")) base::sprintf("%02d", base::seq_len(51)) else base::sprintf("%05d", 1000 + base::seq_len(200))
    wonder_fixture(dir, r$file, parts, r$group_by, wonder_expand_years(r$years), ntsv = r$ntsv_filters, geos = geos)
  }
  dir
}

testthat::test_that("a correct set of exports passes the audit", {
  dir <- correct_exports()
  base::on.exit(base::unlink(dir, recursive = TRUE), add = TRUE)
  audit <- wonder_export_audit(dir)
  testthat::expect_equal(base::nrow(audit), base::nrow(wonder_ntsv_exports()))
  testthat::expect_true(base::all(audit$present))
  testthat::expect_true(base::all(base::is.na(audit$problem)), info = base::paste(stats::na.omit(audit$problem), collapse = " | "))
})

testthat::test_that("the audit catches an export built from the wrong dropdown", {
  dir <- correct_exports()
  base::on.exit(base::unlink(dir, recursive = TRUE), add = TRUE)
  # the classic slip: Age of Mother 10 instead of Age of Mother 9
  wonder_fixture(dir, "ntsv_county_age_2022_2024.txt",
                 base::c("County of Residence", "Age of Mother 10"),
                 "County of Residence; Age of Mother 10", base::c("2022", "2023", "2024"))
  problem <- dplyr::filter(wonder_export_audit(dir), .data$key == "age")$problem
  testthat::expect_match(problem, "grouped by")
  testthat::expect_match(problem, "Age of Mother 10")
})

testthat::test_that("the audit catches the placebo left on the wrong years", {
  dir <- correct_exports()
  base::on.exit(base::unlink(dir, recursive = TRUE), add = TRUE)
  wonder_fixture(dir, "ntsv_county_2016_2019.txt",
                 base::c("County of Residence", "Delivery Method"),
                 "County of Residence; Delivery Method", base::c("2022", "2023", "2024"))
  problem <- dplyr::filter(wonder_export_audit(dir), .data$key == "placebo")$problem
  testthat::expect_match(problem, "years")
})

testthat::test_that("the audit catches NTSV filters that are missing, partial, or wrongly present", {
  dir <- correct_exports()
  base::on.exit(base::unlink(dir, recursive = TRUE), add = TRUE)

  # no filters at all on an export that requires them
  wonder_fixture(dir, "ntsv_county_2022_2024.txt", base::c("County of Residence", "Delivery Method"),
                 "County of Residence; Delivery Method", base::c("2022", "2023", "2024"), ntsv = FALSE)
  testthat::expect_match(dplyr::filter(wonder_export_audit(dir), .data$key == "county")$problem, "NTSV filters absent")

  # the negative control must NOT carry them
  wonder_fixture(dir, "births_county_plurality_2022_2024.txt", base::c("County of Residence", "Plurality"),
                 "County of Residence; Plurality", base::c("2022", "2023", "2024"), ntsv = TRUE)
  testthat::expect_match(dplyr::filter(wonder_export_audit(dir), .data$key == "plurality")$problem,
                         "must not")

  # only early-term gestational ages: a different population that would look ordinary
  path <- base::file.path(dir, "ntsv_state_2022_2024.txt")
  lines <- base::readLines(path)
  lines <- base::sub("37-38 weeks; 39 weeks; 40 weeks; 41 weeks; 42 weeks or more", "37-38 weeks", lines, fixed = TRUE)
  base::writeLines(lines, path)
  testthat::expect_match(dplyr::filter(wonder_export_audit(dir), .data$key == "state")$problem, "NTSV filters absent")
})

testthat::test_that("the audit catches a truncated geography and a missing file", {
  dir <- correct_exports()
  base::on.exit(base::unlink(dir, recursive = TRUE), add = TRUE)
  wonder_fixture(dir, "ntsv_county_2022_2024.txt", base::c("County of Residence", "Delivery Method"),
                 "County of Residence; Delivery Method", base::c("2022", "2023", "2024"),
                 geos = base::sprintf("%05d", 1000 + base::seq_len(8)))
  testthat::expect_match(dplyr::filter(wonder_export_audit(dir), .data$key == "county")$problem, "only 8 counties")

  base::unlink(base::file.path(dir, "ntsv_county_payment_2022_2024.txt"))
  audit <- wonder_export_audit(dir)
  testthat::expect_match(dplyr::filter(audit, .data$key == "payment")$problem, "missing \\(required\\)")
  testthat::expect_false(wonder_audit_report(audit))
})

testthat::test_that("check_ntsv_falsification fails when a falsification model is significant", {
  dir <- base::tempfile("out_"); base::dir.create(dir)
  base::on.exit(base::unlink(dir, recursive = TRUE), add = TRUE)
  write_models <- function(p_placebo, p_negative) {
    readr::write_csv(tibble::tibble(
      model = base::c("primary", "placebo outcome: 2016-2019 NTSV rate", "negative control: multiple-birth share"),
      term = "log2_cnm", estimate_pp = base::c(-1.2, 0.1, 0.01),
      p_value = base::c(0.01, p_placebo, p_negative)
    ), base::file.path(dir, "ntsv_models.csv"))
  }

  testthat::expect_equal(check_ntsv_falsification(dir)$status, "skip")

  write_models(0.62, 0.88)
  testthat::expect_equal(check_ntsv_falsification(dir)$status, "pass")

  # a placebo that lights up means the headline association is confounding
  write_models(0.004, 0.71)
  failed <- check_ntsv_falsification(dir)
  testthat::expect_equal(failed$status, "fail")
  testthat::expect_match(failed$detail, "not ", fixed = TRUE)
  testthat::expect_match(failed$detail, "placebo")
})

testthat::test_that("check_wonder_exports skips when nothing is downloaded and fails on a wrong file", {
  empty <- base::tempfile("wonder_empty_"); base::dir.create(empty)
  base::on.exit(base::unlink(empty, recursive = TRUE), add = TRUE)
  testthat::expect_equal(check_wonder_exports(empty)$status, "skip")
  testthat::expect_equal(check_wonder_exports(base::file.path(empty, "nope"))$status, "skip")

  dir <- correct_exports()
  base::on.exit(base::unlink(dir, recursive = TRUE), add = TRUE)
  testthat::expect_equal(check_wonder_exports(dir)$status, "pass")

  wonder_fixture(dir, "ntsv_county_bmi_2022_2024.txt", base::c("County of Residence", "Mother's Height in Inches"),
                 "County of Residence; Mother's Height in Inches", base::c("2022", "2023", "2024"))
  testthat::expect_equal(check_wonder_exports(dir)$status, "fail")
})
