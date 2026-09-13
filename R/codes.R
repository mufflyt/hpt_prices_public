#' Target-code matching
#'
#' `config/codebook.csv` is the single source of truth for which codes count.
#' Every source (the Trilliant lake, our own CSV and JSON parsers) matches
#' through these functions so the rules can never drift apart.
#'
#' A code counts only when its value AND its declared code type agree with
#' the codebook. MRFs routinely reuse the same digits in unrelated code
#' systems: a hospital's CDM item "58100" or revenue code is not CPT 58100,
#' and APR-DRG 742 is a different grouper from MS-DRG 742. A missing type
#' (common in pre-v2 files) is kept but flagged `type_verified = FALSE`.

load_codebook <- function(path = "config/codebook.csv") {
  codebook <- read_csv_chr(path)

  required_cols <- base::c("concept", "code", "code_system", "code_family", "label")
  missing_cols <- base::setdiff(required_cols, base::names(codebook))

  if (base::length(missing_cols) > 0L) {
    base::stop("Codebook is missing: ", base::paste(missing_cols, collapse = ", "))
  }

  bad_family <- base::setdiff(codebook$code_family, base::c("procedure", "ms_drg"))

  if (base::length(bad_family) > 0L) {
    base::stop("Unknown code_family in codebook: ", base::paste(bad_family, collapse = ", "))
  }

  duplicated_codes <- codebook |>
    dplyr::count(.data$code_family, .data$code) |>
    dplyr::filter(.data$n > 1L)

  if (base::nrow(duplicated_codes) > 0L) {
    base::stop("Codebook lists a code twice: ", base::paste(duplicated_codes$code, collapse = ", "))
  }

  codebook |>
    dplyr::mutate(code = normalize_code(.data$code, .data$code_family))
}

#' Normalize a raw code value for comparison with the codebook
#'
#' Procedure codes: trimmed, uppercased, a spreadsheet-style ".0" suffix
#' dropped. MS-DRGs: leading zeros stripped then left-padded to three
#' digits, so "0742", "742", and "742.0" all become "742".
normalize_code <- function(code, family = "procedure") {
  code <- code |>
    base::as.character() |>
    stringr::str_trim() |>
    stringr::str_to_upper() |>
    stringr::str_replace("\\.0+$", "")

  family <- base::rep_len(family, base::length(code))
  is_drg <- family == "ms_drg" & stringr::str_detect(code, "^[0-9]+$")
  drg_digits <- stringr::str_replace(code[is_drg], "^0+", "")
  code[is_drg] <- stringr::str_pad(drg_digits, width = 3L, side = "left", pad = "0")

  code
}

#' Canonical form of a declared code type
normalize_code_type <- function(code_type) {
  code_type |>
    base::as.character() |>
    stringr::str_trim() |>
    stringr::str_to_upper() |>
    stringr::str_replace_all("[\\s_]+", "-") |>
    dplyr::na_if("")
}

#' Declared code types accepted as verified for each codebook family
#'
#' Shared by `code_type_fit()` (R-side matching) and the SQL built in
#' R/trilliant.R, so both engines apply identical rules.
procedure_code_types <- function() {
  base::c("CPT", "HCPCS", "CPT-HCPCS", "HCPCS-CPT", "CPT/HCPCS", "HCPCS/CPT")
}

drg_code_types <- function() {
  base::c("MS-DRG", "MSDRG")
}

#' Does a declared code type fit a codebook family?
#'
#' Returns "verified", "unverified" (type missing, or the generic "DRG"
#' that doesn't say which grouper), or "incompatible".
code_type_fit <- function(code_type, family) {
  type_norm <- normalize_code_type(code_type)
  family <- base::rep_len(family, base::length(type_norm))

  procedure_types <- procedure_code_types()
  drg_types <- drg_code_types()

  dplyr::case_when(
    base::is.na(type_norm) ~ "unverified",
    family == "procedure" & type_norm %in% procedure_types ~ "verified",
    family == "ms_drg" & type_norm %in% drg_types ~ "verified",
    family == "ms_drg" & type_norm == "DRG" ~ "unverified",
    TRUE ~ "incompatible"
  )
}

#' Check every procedure code against a CMS PFS national RVU file
#'
#' CMS publishes the physician fee schedule relative value file quarterly
#' (https://www.cms.gov/medicare/payment/fee-schedules/physician/pfs-relative-value-files;
#' the RVU26C zip holds PPRRVU2026_Jul_nonQPP.csv). A code absent from the
#' current year's file has been deleted; `active_2026` in the codebook must
#' say so. MS-DRGs are not in this file and are skipped.
#'
#' @param pprrvu_csv Path to a PPRRVU<year>_*.csv from the RVU zip.
#' @return Codebook rows with `in_pfs` and `consistent` columns.
validate_codebook_against_pfs <- function(codebook, pprrvu_csv) {
  lines <- base::readLines(pprrvu_csv, warn = FALSE)
  header_line <- base::which(stringr::str_detect(lines, "^HCPCS,MOD,DESCRIPTION"))[[1]]
  pfs <- readr::read_csv(
    pprrvu_csv,
    skip = header_line - 1L,
    col_types = readr::cols(.default = readr::col_character()),
    name_repair = "minimal",
    show_col_types = FALSE
  )
  pfs_codes <- base::unique(stringr::str_to_upper(stringr::str_trim(pfs[[1]])))

  codebook |>
    dplyr::filter(.data$code_family == "procedure") |>
    dplyr::mutate(
      in_pfs = .data$code %in% pfs_codes,
      consistent = .data$in_pfs == base::as.logical(.data$active_2026)
    )
}

#' Keep only rows whose (code, type) pair matches the codebook
#'
#' @param tbl A data frame with one code per row.
#' @param code_col,type_col Column names holding the raw code and its
#'   declared type (`type_col` may be `NULL` when the source has none).
#' @param codebook From [load_codebook()].
#' @return `tbl` restricted to matches, with `concept`, `code`, `code_system`,
#'   and `type_verified` added (`code` replaced by its normalized form).
match_target_codes <- function(tbl, code_col, type_col = NULL, codebook) {
  raw_code <- tbl[[code_col]]
  raw_type <- if (base::is.null(type_col)) base::rep(NA_character_, base::nrow(tbl)) else tbl[[type_col]]

  candidates <- base::lapply(base::unique(codebook$code_family), function(family) {
    family_codes <- codebook |> dplyr::filter(.data$code_family == family)
    code_norm <- normalize_code(raw_code, family)
    fit <- code_type_fit(raw_type, family)
    keep <- code_norm %in% family_codes$code & fit != "incompatible"

    if (!base::any(keep)) {
      return(NULL)
    }

    matched <- tbl[keep, , drop = FALSE]
    matched$code <- code_norm[keep]
    matched$code_type <- normalize_code_type(raw_type[keep])
    matched$type_verified <- fit[keep] == "verified"
    matched$code_family <- family

    dplyr::inner_join(
      matched,
      family_codes |> dplyr::select("code_family", "code", "concept", "code_system"),
      by = base::c("code_family", "code")
    )
  })

  dplyr::bind_rows(candidates)
}
