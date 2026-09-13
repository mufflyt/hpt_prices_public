#' Classify payer/plan names into insurance types
#'
#' MRF payer names are free text ("AETNA MEDICARE HMO", "Molina Healthcare of
#' Texas - STAR", "BCBS PPO"). `config/payer_type_rules.csv` holds ordered,
#' case-insensitive regex rules; the first match on "payer_name plan_name"
#' wins. Order matters: Medicare Advantage is tested before traditional
#' Medicare (so "Humana Medicare HMO" is not plain Medicare), and Medicaid
#' before commercial (so "Molina STAR" is not commercial). Anything that
#' matches no rule is "other", never silently commercial.
#'
#' The same rules drive the R classifier (tests, small tables) and the SQL
#' CASE expression used inside DuckDB for the national table, so the two
#' can't drift apart.

payer_types <- function() {
  base::c(
    "commercial", "medicare_advantage", "medicare", "medicaid", "exchange",
    "tricare_va", "workers_comp", "self_pay", "other", "unknown"
  )
}

load_payer_type_rules <- function(path = base::file.path(base::getOption("hpt_repo_root", "."), "config", "payer_type_rules.csv")) {
  rules <- read_csv_chr(path) |>
    dplyr::mutate(priority = base::as.integer(.data$priority)) |>
    dplyr::arrange(.data$priority)

  bad <- base::setdiff(rules$payer_type, payer_types())

  if (base::length(bad) > 0L) {
    base::stop("Unknown payer_type in rules: ", base::paste(bad, collapse = ", "))
  }

  rules
}

#' @return Character vector of payer types, same length as `payer_name`.
classify_payer_type <- function(payer_name, plan_name = NA_character_, rules = load_payer_type_rules()) {
  text <- base::paste(dplyr::coalesce(payer_name, ""), dplyr::coalesce(plan_name, ""))
  text <- stringr::str_squish(stringr::str_to_lower(text))
  result <- base::rep(NA_character_, base::length(text))

  for (i in base::seq_len(base::nrow(rules))) {
    hit <- base::is.na(result) & stringr::str_detect(text, stringr::regex(rules$pattern[[i]], ignore_case = TRUE))
    result[hit] <- rules$payer_type[[i]]
  }

  result[base::is.na(result)] <- "other"
  result[!base::nzchar(stringr::str_trim(text))] <- "unknown"
  result
}

#' The same classification as a DuckDB SQL CASE expression
#'
#' @param text_sql SQL expression for the lower-cased "payer plan" text.
payer_type_case_sql <- function(text_sql, rules = load_payer_type_rules()) {
  whens <- base::sprintf(
    "WHEN regexp_matches(%s, %s) THEN %s",
    text_sql,
    sql_string(rules$pattern),
    sql_string(rules$payer_type)
  )

  base::paste0(
    "CASE WHEN trim(", text_sql, ") = '' THEN 'unknown' ",
    base::paste(whens, collapse = " "),
    " ELSE 'other' END"
  )
}
