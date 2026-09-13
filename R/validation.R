#' Validation suite for hpt.duckdb
#'
#' `run_validation()` runs a set of independent checks against the analysis
#' database and returns one row per check:
#'   check_id, category, description, status (pass/warn/fail/skip), metric,
#'   threshold, detail.
#' Each check is a small function that works on any build (Trilliant only,
#' own crawl only, or both) and returns "skip" when the data it needs is
#' absent. Categories:
#'   integrity     keys resolve, no duplicate natural keys, row counts add up
#'   known_answer  per-hospital values read live from Trilliant (2026-09-12)
#'   plausibility  rate sanity, concept and payer ordering
#'   payer_type    our classifier vs Trilliant's payer labels
#'   cross_source  own crawl vs Trilliant for the same MRF URL
#'   benchmark     Medicare medians vs the CMS OPPS Addendum B payment rate
#'   raw_recheck   re-download and re-parse a sample of own-crawl files
#'   coverage      CCNs covered by state; states too thin for a median
#' Nothing here writes to the database (it is opened read-only).

validation_statuses <- function() {
  base::c("pass", "warn", "fail", "skip")
}

#' One validation result row
validation_row <- function(check_id, category, description, status,
                           metric = NA_real_, threshold = NA_character_, detail = NA_character_) {
  if (!status %in% validation_statuses()) {
    base::stop("Unknown validation status: ", status)
  }

  tibble::tibble(
    check_id = check_id, category = category, description = description, status = status,
    metric = base::as.double(metric), threshold = base::as.character(threshold),
    detail = base::as.character(detail)
  )
}

#' pass at or above `pass_at`, warn at or above `warn_at`, else fail
grade_at_least <- function(value, pass_at, warn_at = pass_at) {
  if (base::is.na(value)) return("skip")
  if (value >= pass_at) "pass" else if (value >= warn_at) "warn" else "fail"
}

#' pass at or below `pass_at`, warn at or below `warn_at`, else fail
grade_at_most <- function(value, pass_at, warn_at = pass_at) {
  if (base::is.na(value)) return("skip")
  if (value <= pass_at) "pass" else if (value <= warn_at) "warn" else "fail"
}

#' Session settings for validation queries: cap memory (the full database
#' is millions of rows on a 16 GB machine) and spill to a local temp dir,
#' since the database is opened read-only
validation_init_sql <- function() {
  base::c(
    base::sprintf("SET memory_limit = %s;", sql_string(base::Sys.getenv("HPT_DUCKDB_MEMORY_LIMIT", unset = "5GB"))),
    base::sprintf("SET temp_directory = %s;", sql_string(base::tempdir()))
  )
}

validation_query <- function(db_path, select_sql) {
  duckdb_query(select_sql, database = db_path, read_only = TRUE, init = validation_init_sql())
}

#' Sources with at least one file in the database
#' SQL for a file's display name: the MRF's hospital_name, else the roster
#' name of a CCN it is bridged to (Trilliant files carry no hospital_name)
file_label_sql <- function(file_alias = "f") {
  base::sprintf(
    "coalesce(%1$s.hospital_name, (SELECT min(h.facility_name) FROM bridge_file_ccn b JOIN dim_hospital h USING (ccn) WHERE b.file_id = %1$s.file_id))",
    file_alias
  )
}

validation_sources <- function(db_path) {
  base::as.character(validation_query(db_path, "SELECT DISTINCT CAST(source AS VARCHAR) AS source FROM dim_file")$source)
}

fmt_num <- function(x, digits = 2) {
  base::formatC(x, format = "f", digits = digits, big.mark = ",")
}

fmt_pct <- function(x, digits = 1) {
  base::paste0(base::formatC(100 * x, format = "f", digits = digits), "%")
}

#' Run a check, turning an error into a "fail" row so one broken check
#' never hides the others
run_check_safely <- function(check_id, category, description, fn) {
  base::tryCatch(fn(), error = function(e) {
    validation_row(check_id, category, description, "fail",
                   detail = base::paste("check errored:", base::conditionMessage(e)))
  })
}

#' Median price table the headline uses (R/state_medians.R), recomputed from
#' the database so every downstream check sees the same numbers
validation_medians <- function(db_path, exclude_file_ids = NULL) {
  medians <- validation_query(db_path, state_medians_sql(exclude_file_ids)) |>
    dplyr::mutate(code = base::as.character(.data$code))
  base::attr(medians, "excluded_file_ids") <- exclude_file_ids
  medians
}

#' mrf_file_ids the medians step left out (analysis/11 writes
#' median_excluded_file_ids.csv next to the medians); NULL when absent
read_median_exclusions <- function(out_dir) {
  path <- base::file.path(out_dir, "median_excluded_file_ids.csv")
  if (!base::file.exists(path)) {
    return(NULL)
  }
  ids <- read_csv_chr(path)$mrf_file_id
  base::sort(base::unique(ids[!base::is.na(ids) & base::nzchar(ids)]))
}

national_median <- function(medians, code_value, insurance, fee = "facility") {
  hit <- medians |>
    dplyr::filter(.data$state == "US", .data$code == code_value,
                  .data$insurance_type == insurance, .data$fee_type == fee)
  if (base::nrow(hit) == 0L) NA_real_ else hit$median_price[[1]]
}

# ---- 1. Referential integrity ------------------------------------------------

#' Every fact_rate key resolves to its dimension
check_fact_keys <- function(db_path) {
  counts <- validation_query(db_path, base::paste(
    "SELECT",
    "(SELECT count(*) FROM fact_rate r WHERE NOT EXISTS (SELECT 1 FROM dim_file f WHERE f.file_id = r.file_id)) AS orphan_file,",
    "(SELECT count(*) FROM fact_rate r WHERE NOT EXISTS (SELECT 1 FROM dim_code c WHERE c.code_id = r.code_id)) AS orphan_code,",
    "(SELECT count(*) FROM fact_rate r WHERE r.payer_id IS NOT NULL AND NOT EXISTS (SELECT 1 FROM dim_payer p WHERE p.payer_id = r.payer_id)) AS orphan_payer,",
    "(SELECT count(*) FROM fact_rate WHERE payer_id IS NULL AND negotiated_dollar IS NOT NULL) AS rate_without_payer,",
    "(SELECT count(*) FROM fact_rate WHERE payer_id IS NULL) AS no_payer_rows"
  ))
  orphans <- counts$orphan_file + counts$orphan_code + counts$orphan_payer

  validation_row(
    "integrity_fact_keys", "integrity", "fact_rate file_id, code_id, payer_id resolve to dim tables",
    if (orphans > 0) "fail" else if (counts$rate_without_payer > 0) "warn" else "pass",
    metric = orphans, threshold = "0 orphan keys",
    detail = base::sprintf(
      "orphans: file %s, code %s, payer %s; %s gross/cash-only rows with no payer (expected), %s rows with a negotiated rate but no payer",
      counts$orphan_file, counts$orphan_code, counts$orphan_payer, counts$no_payer_rows, counts$rate_without_payer
    )
  )
}

#' No duplicate surrogate or natural keys in the dimension tables
check_dim_keys <- function(db_path) {
  dups <- validation_query(db_path, base::paste(
    "SELECT",
    "(SELECT count(*) - count(DISTINCT code_id) FROM dim_code) AS code_id,",
    "(SELECT count(*) - count(DISTINCT (code, CAST(concept AS VARCHAR))) FROM dim_code) AS code_natural,",
    "(SELECT count(*) - count(DISTINCT ccn) FROM dim_hospital) AS ccn,",
    "(SELECT count(*) - count(DISTINCT file_id) FROM dim_file) AS file_id,",
    "(SELECT count(*) - count(DISTINCT mrf_file_id) FROM dim_file) AS mrf_file_id,",
    "(SELECT count(*) - count(DISTINCT payer_id) FROM dim_payer) AS payer_id,",
    "(SELECT count(*) - count(DISTINCT (coalesce(payer_name, chr(1)), coalesce(plan_name, chr(1)))) FROM dim_payer) AS payer_natural"
  ))
  values <- base::unlist(dups[1, ])
  bad <- values[values > 0]

  validation_row(
    "integrity_dim_keys", "integrity", "No duplicate surrogate or natural keys in dim_code, dim_hospital, dim_file, dim_payer",
    if (base::length(bad) > 0) "fail" else "pass",
    metric = base::sum(values), threshold = "0 duplicates",
    detail = if (base::length(bad) > 0) {
      base::paste("duplicates:", base::paste(base::names(bad), bad, sep = " = ", collapse = "; "),
                  "(a duplicate ccn repeats rates in v_hospital_rate)")
    } else {
      "no duplicates"
    }
  )
}

#' bridge_file_ccn points at real hospitals and files, once per pair
check_bridge <- function(db_path) {
  counts <- validation_query(db_path, base::paste(
    "SELECT count(*) AS n,",
    "count(*) FILTER (WHERE NOT EXISTS (SELECT 1 FROM dim_hospital h WHERE h.ccn = b.ccn)) AS ccn_missing,",
    "count(*) FILTER (WHERE NOT EXISTS (SELECT 1 FROM dim_file f WHERE f.file_id = b.file_id)) AS file_missing,",
    "count(*) - count(DISTINCT (file_id, ccn)) AS dup_pairs,",
    "count(DISTINCT ccn) FILTER (WHERE NOT EXISTS (SELECT 1 FROM dim_hospital h WHERE h.ccn = b.ccn)) AS distinct_ccn_missing",
    "FROM bridge_file_ccn b"
  ))
  rows <- validation_query(db_path, base::paste(
    "WITH n AS (SELECT file_id, count(*) AS n FROM fact_rate GROUP BY 1)",
    "SELECT",
    "(SELECT coalesce(sum((d.copies - 1) * n.n), 0) FROM (SELECT file_id, ccn, count(*) AS copies FROM bridge_file_ccn GROUP BY ALL HAVING count(*) > 1) d JOIN n USING (file_id)) AS repeated_rows,",
    "(SELECT coalesce(sum(n.n), 0) FROM (SELECT DISTINCT file_id FROM bridge_file_ccn b WHERE NOT EXISTS (SELECT 1 FROM dim_hospital h WHERE h.ccn = b.ccn)) m JOIN n USING (file_id)) AS unrostered_rows"
  ))
  missing <- counts$ccn_missing + counts$file_missing

  validation_row(
    "integrity_bridge", "integrity", "bridge_file_ccn CCNs exist in dim_hospital and files in dim_file",
    if (missing > 0) "fail" else if (counts$dup_pairs > 0) "warn" else "pass",
    metric = missing, threshold = "0 missing",
    detail = base::sprintf(
      "%s bridge rows; %s rows (%s distinct CCNs, %s fact rows) point at CCNs not in dim_hospital; %s files not in dim_file; %s duplicate (file, ccn) pairs repeat %s rows in v_hospital_rate",
      counts$n, counts$ccn_missing, counts$distinct_ccn_missing, rows$unrostered_rows, counts$file_missing,
      counts$dup_pairs, rows$repeated_rows
    )
  )
}

#' fact_rate row count equals the staged rows recorded per file
check_row_counts <- function(db_path) {
  counts <- validation_query(db_path, base::paste(
    "SELECT CAST(f.source AS VARCHAR) AS source, sum(f.n_rows) AS staged, sum(coalesce(l.n, 0)) AS loaded",
    "FROM dim_file f LEFT JOIN (SELECT file_id, count(*) AS n FROM fact_rate GROUP BY 1) l USING (file_id)",
    "GROUP BY 1 ORDER BY 1"
  ))
  total_loaded <- validation_query(db_path, "SELECT count(*) AS n FROM fact_rate")$n
  gap <- base::sum(counts$staged) - total_loaded

  validation_row(
    "integrity_row_counts", "integrity", "count(fact_rate) equals sum(dim_file.n_rows)",
    if (gap == 0) "pass" else "fail",
    metric = gap, threshold = "difference 0",
    detail = base::paste0(
      "fact_rate ", total_loaded, " vs staged ", base::sum(counts$staged), "; ",
      base::paste0(counts$source, ": staged ", counts$staged, ", loaded ", counts$loaded, collapse = "; "),
      if (gap != 0) " (rows whose code/concept is not in dim_code are dropped at load)" else ""
    )
  )
}

#' File ids look like real content hashes (md5 or sha256) and each MRF URL
#' maps to one file per source
check_file_ids <- function(db_path) {
  ids <- validation_query(db_path, base::paste(
    "SELECT CAST(source AS VARCHAR) AS source, count(*) AS n,",
    "count(*) FILTER (WHERE NOT regexp_full_match(coalesce(mrf_file_id, ''), '[0-9a-fA-F]{32}|[0-9a-fA-F]{64}')) AS malformed,",
    "string_agg(DISTINCT mrf_file_id, ', ') FILTER (WHERE NOT regexp_full_match(coalesce(mrf_file_id, ''), '[0-9a-fA-F]{32}|[0-9a-fA-F]{64}')) AS examples",
    "FROM dim_file GROUP BY 1 ORDER BY 1"
  ))
  dup_urls <- validation_query(db_path, base::paste(
    "SELECT CAST(source AS VARCHAR) AS source, count(*) AS urls, sum(n_files) AS files FROM (",
    "  SELECT source, mrf_url, count(*) AS n_files FROM dim_file WHERE mrf_url IS NOT NULL GROUP BY ALL HAVING count(*) > 1",
    ") GROUP BY 1 ORDER BY 1"
  ))
  malformed <- base::sum(ids$malformed)
  examples <- stringr::str_trunc(dplyr::coalesce(ids$examples, ""), 80)

  validation_row(
    "integrity_file_ids", "integrity", "mrf_file_id is an md5/sha256 hash; one file per MRF URL per source",
    if (malformed > 0) "fail" else if (base::nrow(dup_urls) > 0) "warn" else "pass",
    metric = malformed, threshold = "0 malformed ids",
    detail = base::paste0(
      base::paste0(ids$source, ": ", ids$malformed, " of ", ids$n, " ids malformed",
                   base::ifelse(ids$malformed > 0, base::paste0(" (", examples, ")"), ""), collapse = "; "),
      if (base::nrow(dup_urls) > 0) {
        base::paste0("; URLs with several file ids: ",
                     base::paste0(dup_urls$source, " ", dup_urls$urls, " URLs / ", dup_urls$files, " files", collapse = ", "))
      } else {
        ""
      }
    )
  )
}

#' The saved state medians match a fresh computation from the database,
#' leaving out the same files the medians step excluded
#' (median_excluded_file_ids.csv)
#'
#' @param medians Recomputed medians to reuse when they were built with the
#'   same exclusions; otherwise they are recomputed here.
check_medians_current <- function(db_path, out_dir, medians = NULL) {
  path <- base::file.path(out_dir, "state_insurance_medians.csv")
  description <- "output/state_insurance_medians.csv matches the database"

  if (!base::file.exists(path)) {
    return(validation_row("integrity_medians_current", "integrity", description, "skip",
                          detail = base::paste("no file at", path)))
  }

  excluded <- read_median_exclusions(out_dir)
  if (base::is.null(medians) || !base::identical(base::sort(base::attr(medians, "excluded_file_ids")), excluded)) {
    medians <- validation_medians(db_path, excluded)
  }

  saved <- readr::read_csv(path, show_col_types = FALSE, col_types = readr::cols(code = "c", state = "c", .default = readr::col_guess()))
  keys <- base::c("state", "code", "fee_type", "insurance_type")
  # the CSV writes NA as "", so a blank state reads back as NA: compare them as one
  blank_to_na <- function(tbl) dplyr::mutate(tbl, state = dplyr::na_if(.data$state, ""))
  joined <- dplyr::full_join(
    blank_to_na(saved) |> dplyr::select(dplyr::all_of(keys), saved = "median_price", saved_n = "n_hospitals"),
    blank_to_na(medians) |> dplyr::select(dplyr::all_of(keys), fresh = "median_price", fresh_n = "n_hospitals"),
    by = keys
  )
  unmatched <- base::sum(base::is.na(joined$saved) != base::is.na(joined$fresh))
  rel_diff <- base::abs(joined$saved - joined$fresh) / base::pmax(base::abs(joined$fresh), 1e-9)
  changed <- base::sum(rel_diff > 1e-6 | joined$saved_n != joined$fresh_n, na.rm = TRUE)

  validation_row(
    "integrity_medians_current", "integrity", description,
    if (unmatched + changed == 0) "pass" else "warn",
    metric = unmatched + changed, threshold = "0 differing cells",
    detail = base::sprintf("%s saved rows vs %s recomputed excluding %s files (median_excluded_file_ids.csv); %s cells only on one side, %s cells differ (re-run compute_state_medians())",
                           base::nrow(saved), base::nrow(medians), base::length(excluded), unmatched, changed)
  )
}

# ---- 2. Known answers from live Trilliant queries ----------------------------

#' Known per-hospital values, read from config/known_answers.csv
#'
#' The values were read from Trilliant's per-hospital parsed databases on
#' 2026-09-12. They are Trilliant-derived rates, so the file lives only in the
#' private repository (tools/export_public.sh leaves it out); without it the
#' known-answer check is skipped. Columns: hospital, ccn, npi, hospital_name,
#' trilliant_id, mrf_file_id (the lake's mrf_content_hash), last_updated_on,
#' code, stat, expected. `median_negotiated` is the median of
#' standard_charge_dollar over every row of the file for that code (not the
#' three-stage median); `n_rows` and `n_payers` count rows and distinct payer
#' names for the code.
known_answers <- function(path = base::file.path(base::getOption("hpt_repo_root", "."), "config", "known_answers.csv")) {
  columns <- base::c("hospital", "ccn", "npi", "hospital_name", "trilliant_id", "mrf_file_id",
                     "last_updated_on", "code", "stat", "expected")
  if (!base::file.exists(path)) {
    empty <- tibble::as_tibble(stats::setNames(base::rep(base::list(base::character()), base::length(columns)), columns))
    empty$expected <- base::numeric()
    return(empty)
  }
  answers <- readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE)
  require_columns(answers, columns, "config/known_answers.csv")
  answers$expected <- base::as.numeric(answers$expected)
  answers[, columns]
}

#' Per-file statistics for the files that could be each known hospital
known_answer_candidates <- function(db_path, answers) {
  if (!"mrf_file_id" %in% base::names(answers)) {
    answers$mrf_file_id <- NA_character_
  }
  hospitals <- answers |> dplyr::distinct(.data$hospital, .data$ccn, .data$npi, .data$hospital_name, .data$mrf_file_id)
  sql_or_null <- function(x) base::ifelse(base::is.na(x), "CAST(NULL AS VARCHAR)", sql_string(x))
  match_sql <- base::paste0(
    "(", sql_string(hospitals$hospital), ", ", sql_string(hospitals$ccn), ", ", sql_or_null(hospitals$npi), ", ",
    sql_string(stringr::str_to_lower(hospitals$hospital_name)), ", ", sql_or_null(hospitals$mrf_file_id), ")",
    collapse = ", "
  )

  validation_query(db_path, base::paste0(
    "WITH k(hospital, ccn, npi, hospital_name, mrf_file_id) AS (VALUES ", match_sql, "),\n",
    "cand AS (\n",
    "  SELECT k.hospital, f.file_id, f.hospital_name AS file_hospital_name, CAST(f.last_updated_on AS VARCHAR) AS file_last_updated,\n",
    "         coalesce(bool_or(f.mrf_file_id = k.mrf_file_id), false) AS id_match,\n",
    "         coalesce(bool_or(b.ccn = k.ccn), false) AS ccn_match,\n",
    "         coalesce(bool_or(k.npi IS NOT NULL AND contains(coalesce(f.type_2_npi, ''), k.npi)), false) AS npi_match,\n",
    "         coalesce(bool_or(lower(trim(f.hospital_name)) = k.hospital_name), false) AS name_match\n",
    "  FROM k JOIN dim_file f ON f.source = 'trilliant'\n",
    "  LEFT JOIN bridge_file_ccn b ON b.file_id = f.file_id\n",
    "  WHERE f.mrf_file_id = k.mrf_file_id OR b.ccn = k.ccn\n",
    "     OR (k.npi IS NOT NULL AND contains(coalesce(f.type_2_npi, ''), k.npi))\n",
    "     OR lower(trim(f.hospital_name)) = k.hospital_name\n",
    "  GROUP BY ALL\n",
    "),\n",
    "per_code AS (\n",
    "  SELECT c.hospital, c.file_id, d.code, median(r.negotiated_dollar) AS median_negotiated, max(r.gross) AS max_gross,\n",
    "         max(r.discounted_cash) AS max_cash, CAST(count(*) AS DOUBLE) AS n_rows,\n",
    "         CAST(count(DISTINCT p.payer_name) AS DOUBLE) AS n_payers\n",
    "  FROM cand c JOIN fact_rate r ON r.file_id = c.file_id JOIN dim_code d ON d.code_id = r.code_id\n",
    "  LEFT JOIN dim_payer p ON p.payer_id = r.payer_id\n",
    "  WHERE d.code IN (", sql_string_list(base::unique(answers$code)), ")\n",
    "  GROUP BY ALL\n",
    ")\n",
    "SELECT c.*, s.code, s.median_negotiated, s.max_gross, s.max_cash, s.n_rows, s.n_payers,\n",
    "       sum(s.n_rows) OVER (PARTITION BY c.hospital, c.file_id) AS file_rows\n",
    "FROM cand c JOIN per_code s USING (hospital, file_id)"
  ))
}

#' Reproduce known per-hospital values within 1%
#'
#' The file for each hospital is the Trilliant file whose last_updated_on
#' matches the live query, then the one matching NPI/name/CCN, then the
#' largest. A miss on a file whose last_updated_on differs is a "warn" (the
#' lake snapshot is 2026-07-21, the live databases 2026-07-18).
check_known_answers <- function(db_path, answers = known_answers(), sources = validation_sources(db_path), tolerance = 0.01) {
  description <- "Known per-hospital values (live Trilliant queries, 2026-09-12)"

  if (!"trilliant" %in% sources) {
    return(validation_row("known_answer", "known_answer", description, "skip", detail = "no 'trilliant' source in this database"))
  }
  if (base::nrow(answers) == 0L) {
    return(validation_row("known_answer", "known_answer", description, "skip",
                          detail = "no config/known_answers.csv (kept only in the private repository)"))
  }

  candidates <- known_answer_candidates(db_path, answers)
  rows <- base::list()

  for (hospital_label in base::unique(answers$hospital)) {
    expected <- answers |> dplyr::filter(.data$hospital == hospital_label)
    slug <- stringr::str_replace_all(stringr::str_to_lower(stringr::word(hospital_label, 1, 2)), "[^a-z0-9]+", "_")
    files <- candidates |>
      dplyr::filter(.data$hospital == hospital_label) |>
      dplyr::distinct(.data$file_id, .data$file_hospital_name, .data$file_last_updated, .data$id_match,
                      .data$ccn_match, .data$npi_match, .data$name_match, .data$file_rows) |>
      dplyr::mutate(date_match = dplyr::coalesce(.data$file_last_updated == expected$last_updated_on[[1]], FALSE)) |>
      dplyr::arrange(dplyr::desc(.data$id_match), dplyr::desc(.data$date_match), dplyr::desc(.data$npi_match),
                     dplyr::desc(.data$name_match), dplyr::desc(.data$ccn_match), dplyr::desc(.data$file_rows))

    if (base::nrow(files) == 0L) {
      rows[[base::length(rows) + 1L]] <- validation_row(
        base::paste0("known_answer_", slug), "known_answer", base::paste(description, "-", hospital_label), "fail",
        threshold = "file present",
        detail = base::sprintf("no Trilliant file matched CCN %s / NPI %s / name '%s'",
                               expected$ccn[[1]], expected$npi[[1]], expected$hospital_name[[1]])
      )
      next
    }

    chosen <- files[1, ]
    file_stats <- candidates |> dplyr::filter(.data$hospital == hospital_label, .data$file_id == chosen$file_id)
    file_note <- base::sprintf("file_id %s ('%s', matched by %s, last_updated_on %s vs live %s; %s candidate file(s))",
                               chosen$file_id, chosen$file_hospital_name,
                               base::paste(base::c("mrf_file_id", "ccn", "npi", "name")[base::c(chosen$id_match, chosen$ccn_match, chosen$npi_match, chosen$name_match)], collapse = "+"),
                               chosen$file_last_updated %||% "NA", expected$last_updated_on[[1]], base::nrow(files))

    for (i in base::seq_len(base::nrow(expected))) {
      target <- expected[i, ]
      hit <- file_stats[file_stats$code == target$code, ]
      ours <- if (base::nrow(hit) == 0L) NA_real_ else base::as.double(hit[[target$stat]][[1]])
      rel <- base::abs(ours - target$expected) / target$expected
      status <- if (!base::is.na(rel) && rel <= tolerance) "pass" else if (!chosen$date_match) "warn" else "fail"

      rows[[base::length(rows) + 1L]] <- validation_row(
        base::paste0("known_answer_", slug, "_", target$code, "_", target$stat), "known_answer",
        base::sprintf("%s %s %s = %s", hospital_label, target$code, target$stat, fmt_num(target$expected)),
        status, metric = rel, threshold = base::paste0("relative difference <= ", fmt_pct(tolerance, 0)),
        detail = base::paste0("ours ", if (base::is.na(ours)) "missing" else fmt_num(ours), "; ", file_note)
      )
    }
  }

  dplyr::bind_rows(rows)
}

# ---- 3. Plausibility ---------------------------------------------------------

#' Rate-level sanity: plausible share, negotiated > gross, cash > gross,
#' zero or negative amounts
check_rate_plausibility <- function(db_path, max_share_above_gross = 0.05) {
  by_source <- validation_query(db_path, base::paste(
    "SELECT CAST(f.source AS VARCHAR) AS source,",
    "count(r.negotiated_dollar) AS n_dollar,",
    "count(*) FILTER (WHERE r.plausible) AS n_plausible,",
    "count(*) FILTER (WHERE r.plausible AND r.gross > 1) AS n_with_gross,",
    "count(*) FILTER (WHERE r.plausible AND r.gross > 1 AND r.negotiated_dollar > r.gross + 0.01) AS n_above_gross,",
    "count(*) FILTER (WHERE r.negotiated_dollar <= 0) AS neg_nonpos,",
    "count(*) FILTER (WHERE r.gross <= 0) AS gross_nonpos,",
    "count(*) FILTER (WHERE r.discounted_cash <= 0) AS cash_nonpos",
    "FROM fact_rate r JOIN dim_file f USING (file_id) GROUP BY 1 ORDER BY 1"
  ))
  top_above <- validation_query(db_path, base::paste(
    "SELECT file_id, ", file_label_sql(), " AS hospital_name, n_above, ratio FROM (",
    "  SELECT r.file_id, count(*) AS n_above, median(r.negotiated_dollar / r.gross) AS ratio",
    "  FROM fact_rate r WHERE r.plausible AND r.gross > 1 AND r.negotiated_dollar > r.gross + 0.01",
    "  GROUP BY 1 ORDER BY n_above DESC LIMIT 3",
    ") t JOIN dim_file f USING (file_id) ORDER BY n_above DESC"
  ))
  # cash and gross repeat on every payer row of a charge line: dedupe lines first
  cash <- validation_query(db_path, base::paste(
    "SELECT count(*) AS n_lines, count(*) FILTER (WHERE discounted_cash > gross + 0.01) AS n_cash_above_gross FROM (",
    "  SELECT DISTINCT file_id, code_id, description, setting, billing_class, gross, discounted_cash",
    "  FROM fact_rate WHERE gross > 1 AND discounted_cash > 1)"
  ))
  total <- function(column) base::sum(by_source[[column]])
  per_source <- function(num, den) base::paste0(by_source$source, " ", fmt_pct(by_source[[num]] / by_source[[den]]), collapse = ", ")

  share_plausible <- total("n_plausible") / total("n_dollar")
  share_above <- total("n_above_gross") / total("n_with_gross")
  share_cash_above <- cash$n_cash_above_gross / cash$n_lines
  nonpos <- total("neg_nonpos") + total("gross_nonpos") + total("cash_nonpos")
  n_values <- total("n_dollar")

  dplyr::bind_rows(
    validation_row(
      "plausibility_share_plausible", "plausibility", "Share of negotiated dollar rates flagged plausible at load",
      grade_at_least(share_plausible, 0.90, 0.75), metric = share_plausible, threshold = "pass >= 90%, warn >= 75%",
      detail = base::paste0(total("n_plausible"), " of ", n_values, " dollar rates; by source: ", per_source("n_plausible", "n_dollar"))
    ),
    validation_row(
      "plausibility_negotiated_above_gross", "plausibility", "Share of plausible negotiated rates above the line's gross charge",
      grade_at_most(share_above, max_share_above_gross, Inf), metric = share_above,
      threshold = base::paste0("warn above ", fmt_pct(max_share_above_gross, 0)),
      detail = base::paste0(
        total("n_above_gross"), " of ", total("n_with_gross"), " rates with a gross charge; by source: ",
        per_source("n_above_gross", "n_with_gross"),
        if (base::nrow(top_above) > 0) {
          base::paste0("; top files: ", base::paste0("file ", top_above$file_id, " (", top_above$hospital_name, ") ",
                                                    top_above$n_above, " rates, median ", fmt_num(top_above$ratio), "x gross",
                                                    collapse = ", "))
        } else {
          ""
        }
      )
    ),
    validation_row(
      "plausibility_cash_above_gross", "plausibility", "Share of charge lines whose discounted cash price exceeds gross",
      grade_at_most(share_cash_above, 0.05, Inf), metric = share_cash_above, threshold = "warn above 5%",
      detail = base::sprintf("%s of %s distinct charge lines with both prices", cash$n_cash_above_gross, cash$n_lines)
    ),
    validation_row(
      "plausibility_nonpositive", "plausibility", "Zero or negative negotiated, gross, or cash amounts",
      grade_at_most(nonpos / base::max(n_values, 1), 0.01, Inf), metric = nonpos, threshold = "warn above 1% of dollar rates",
      detail = base::sprintf("negotiated <= 0: %s; gross <= 0: %s; cash <= 0: %s (kept but excluded from medians)",
                             total("neg_nonpos"), total("gross_nonpos"), total("cash_nonpos"))
    )
  )
}

#' Files where most plausible negotiated rates exceed the line's gross charge
#'
#' Usually a gross column that is a per-unit or component charge while the
#' negotiated rate is a case rate. Written to files_rates_above_gross.csv so
#' the medians step can exclude these files.
#'
#' @param max_share Flag files whose share above gross is greater than this.
#' @param min_rates Ignore files with fewer rates carrying a gross charge.
files_above_gross <- function(db_path, max_share = 0.5, min_rates = 5L) {
  validation_query(db_path, base::paste0(
    "WITH per_file AS (\n",
    "  SELECT f.file_id, f.mrf_file_id, CAST(f.source AS VARCHAR) AS source, f.hospital_name, f.mrf_url,\n",
    "         count(*) AS n_rates, count(*) FILTER (WHERE r.negotiated_dollar > r.gross + 0.01) AS n_above,\n",
    "         median(r.negotiated_dollar / r.gross) AS median_ratio\n",
    "  FROM fact_rate r JOIN dim_file f USING (file_id)\n",
    "  WHERE r.plausible AND r.gross > 1\n",
    "  GROUP BY ALL\n",
    ")\n",
    "SELECT p.file_id, p.mrf_file_id, p.source, ", file_label_sql("p"), " AS hospital_name,\n",
    "       (SELECT string_agg(DISTINCT b.ccn, ';') FROM bridge_file_ccn b WHERE b.file_id = p.file_id) AS ccns,\n",
    "       p.mrf_url, p.n_rates, p.n_above, CAST(p.n_above AS DOUBLE) / p.n_rates AS share_above, p.median_ratio\n",
    "FROM per_file p WHERE p.n_rates >= ", base::as.integer(min_rates), " AND CAST(p.n_above AS DOUBLE) / p.n_rates > ", max_share, "\n",
    "ORDER BY p.n_above DESC"
  ))
}

check_files_above_gross <- function(flagged, max_share = 0.5, min_rates = 5L) {
  validation_row(
    "plausibility_files_above_gross", "plausibility",
    base::sprintf("Files where more than %s of plausible negotiated rates exceed gross", fmt_pct(max_share, 0)),
    if (base::nrow(flagged) == 0L) "pass" else "warn",
    metric = base::nrow(flagged), threshold = base::sprintf("0 files (files with >= %s rates carrying a gross charge)", min_rates),
    detail = if (base::nrow(flagged) == 0L) {
      "none"
    } else {
      base::paste0(
        base::sum(flagged$n_above), " rates in ", base::nrow(flagged), " files (listed in files_rates_above_gross.csv): ",
        base::paste0("file ", utils::head(flagged$file_id, 5), " (", utils::head(flagged$hospital_name, 5), ") ",
                     utils::head(flagged$n_above, 5), "/", utils::head(flagged$n_rates, 5), " above, median ",
                     fmt_num(utils::head(flagged$median_ratio, 5)), "x gross", collapse = ", "),
        if (base::nrow(flagged) > 5L) ", ..." else ""
      )
    }
  )
}

#' National commercial facility medians should order IUD < colonoscopy <
#' bariatric surgery and EMB < colonoscopy (anchor codes)
check_concept_order <- function(medians) {
  pairs <- tibble::tibble(
    lower = base::c("58300", "58100", "45378", "45378"),
    higher = base::c("45378", "45378", "43775", "43644"),
    label = base::c("iud_insertion < colonoscopy", "emb < colonoscopy", "colonoscopy < sleeve", "colonoscopy < bypass")
  )
  ordering_row(
    pairs, function(code) national_median(medians, code, "commercial"),
    "plausibility_concept_order", "National commercial facility medians: iud_insertion < colonoscopy < bariatric_surgery; emb < colonoscopy"
  )
}

#' National 45378 facility medians should order Medicaid <= Medicare <= commercial
check_payer_order <- function(medians, code = "45378") {
  pairs <- tibble::tibble(
    lower = base::c("medicaid", "medicare"), higher = base::c("medicare", "commercial"),
    label = base::c("medicaid <= medicare", "medicare <= commercial")
  )
  ordering_row(
    pairs, function(insurance) national_median(medians, code, insurance),
    "plausibility_payer_order", base::paste0("National ", code, " facility medians: medicaid <= medicare <= commercial"),
    strict = FALSE
  )
}

#' Payer rows typed self_pay should price near the discounted cash column
#'
#' Hospitals list only one or two "Self Pay" payer rows per code, so a code
#' that sits on both a clinic line and an OR-case line (some files list 58300
#' at clinic and at operating-room gross charges two orders of magnitude
#' apart) gives a median of two that is really their mean. The discounted-cash price (self_pay_cash) is the robust one.
check_self_pay_consistency <- function(medians, low = 0.5, high = 2) {
  rows <- base::lapply(headline_codes(), function(code) {
    self_pay <- national_median(medians, code, "self_pay")
    cash <- national_median(medians, code, "self_pay_cash")
    hit <- medians |> dplyr::filter(.data$state == "US", .data$code == !!code, .data$fee_type == "facility",
                                    .data$insurance_type %in% base::c("self_pay", "self_pay_cash"))
    n_of <- function(type) base::sum(hit$n_hospitals[hit$insurance_type == type])
    tibble::tibble(code = code, self_pay = self_pay, cash = cash, ratio = self_pay / cash,
                   n_self_pay = n_of("self_pay"), n_cash = n_of("self_pay_cash"))
  }) |>
    dplyr::bind_rows() |>
    dplyr::filter(!base::is.na(.data$ratio))

  if (base::nrow(rows) == 0L) {
    return(validation_row("plausibility_self_pay_vs_cash", "plausibility",
                          "National self_pay payer-row median vs discounted cash (self_pay_cash)", "skip",
                          detail = "no headline code has both medians"))
  }

  off <- rows |> dplyr::filter(.data$ratio < low | .data$ratio > high)
  validation_row(
    "plausibility_self_pay_vs_cash", "plausibility", "National self_pay payer-row median vs discounted cash (self_pay_cash)",
    if (base::nrow(off) == 0L) "pass" else "warn", metric = base::nrow(off),
    threshold = base::sprintf("ratio within %s to %s for every headline code", low, high),
    detail = base::paste0(
      base::paste0(rows$code, ": self_pay ", fmt_num(rows$self_pay), " (", rows$n_self_pay, " hospitals) vs cash ",
                   fmt_num(rows$cash), " (", rows$n_cash, "), ratio ", fmt_num(rows$ratio),
                   base::ifelse(rows$ratio < low | rows$ratio > high, " OUT OF RANGE", ""), collapse = "; "),
      if (base::nrow(off) > 0L) "; report self_pay_cash as the self-pay price" else ""
    )
  )
}

ordering_row <- function(pairs, value_of, check_id, description, strict = TRUE) {
  pairs$lower_value <- base::vapply(pairs$lower, value_of, base::numeric(1))
  pairs$higher_value <- base::vapply(pairs$higher, value_of, base::numeric(1))
  evaluable <- pairs |> dplyr::filter(!base::is.na(.data$lower_value), !base::is.na(.data$higher_value))

  if (base::nrow(evaluable) == 0L) {
    return(validation_row(check_id, "plausibility", description, "skip", detail = "no pair has both medians"))
  }

  evaluable$ok <- if (strict) evaluable$lower_value < evaluable$higher_value else evaluable$lower_value <= evaluable$higher_value
  missing <- pairs$label[!pairs$label %in% evaluable$label]

  validation_row(
    check_id, "plausibility", description,
    if (base::all(evaluable$ok)) "pass" else "warn",
    metric = base::sum(!evaluable$ok), threshold = "0 violated pairs",
    detail = base::paste0(
      base::paste0(evaluable$label, ": ", fmt_num(evaluable$lower_value), " vs ", fmt_num(evaluable$higher_value),
                   base::ifelse(evaluable$ok, "", " VIOLATED"), collapse = "; "),
      if (base::length(missing) > 0) base::paste0("; not evaluable: ", base::paste(missing, collapse = ", ")) else ""
    )
  )
}

# ---- 4. Payer-type classifier vs Trilliant labels ----------------------------

#' The lower-cased "payer plan" text dim_payer is classified on (must match
#' the inline expression in build_hpt_database(), R/duckdb_store.R)
payer_text_sql <- function() {
  "lower(regexp_replace(trim(coalesce(payer_name, '') || ' ' || coalesce(plan_name, '')), '\\s+', ' ', 'g'))"
}

#' Map Trilliant's payer-type labels onto ours
#'
#' Labels seen in the lake are mapped explicitly; anything else keeps its
#' text as "unmapped: <label>" so a new label shows up in the confusion
#' matrix instead of silently counting as a disagreement.
map_trilliant_payer_type <- function(label) {
  key <- stringr::str_squish(stringr::str_to_lower(label))
  mapping <- base::c(
    "commercial" = "commercial",
    "medicare" = "medicare",
    "medicaid" = "medicaid",
    "managed medicaid" = "medicaid",
    "medicare advantage" = "medicare_advantage",
    "exchange" = "exchange",
    "marketplace" = "exchange",
    "tricare" = "tricare_va",
    "va" = "tricare_va",
    "workers comp" = "workers_comp",
    "workers compensation" = "workers_comp",
    "self pay" = "self_pay",
    "other" = "other",
    "unknown" = "unknown"
  )
  mapped <- base::unname(mapping[key])
  dplyr::case_when(
    base::is.na(key) ~ NA_character_,
    !base::is.na(mapped) ~ mapped,
    TRUE ~ base::paste0("unmapped: ", key)
  )
}

main_payer_types <- function() {
  base::c("commercial", "medicare", "medicare_advantage", "medicaid")
}

#' The type a payer/plan string states outright, or NA
#'
#' Only unambiguous product words count (a plan literally named "Medicare
#' Advantage", "Managed Medicaid", "TRICARE", "Workers Comp", "Medigap").
#' Trilliant labels most lines by carrier, so "United Healthcare / Medicare
#' Advantage" is Commercial there; where the text says otherwise, the text
#' is the reference. These patterns are deliberately narrower than
#' config/payer_type_rules.csv (no carrier names, no brand lists).
explicit_payer_type <- function(text) {
  patterns <- base::c(
    tricare_va = "tricare|champva|\\bva ccn\\b|va community care",
    workers_comp = "worker'?s'? ?comp|work comp",
    medicare = "medicare supplement|medigap|traditional medicare|medicare part [ab]\\b|^medicare$",
    medicare_advantage = "medicare advantage|medicare adv\\b|\\bd-?snp\\b|managed medicare|medicare managed care|medicare (hmo|ppo)\\b",
    medicaid = "medicaid|tenncare|\\bmedi-cal\\b|\\bchip\\b|\\bstar ?plus\\b"
  )
  text <- stringr::str_squish(stringr::str_remove_all(stringr::str_to_lower(dplyr::coalesce(text, "")), "\""))
  result <- base::rep(NA_character_, base::length(text))

  for (type in base::names(patterns)) {
    hit <- base::is.na(result) & stringr::str_detect(text, patterns[[type]])
    result[hit] <- type
  }

  result
}

#' Our type as compared with Trilliant's: they have no exchange label, and
#' ACA marketplace plans are commercial insurance
comparable_payer_type <- function(type) {
  dplyr::if_else(type == "exchange", "commercial", type)
}

#' dim_payer with fact_rate row counts, the stored type, and the type under
#' the current rules
payer_type_table <- function(db_path, rules = load_payer_type_rules()) {
  validation_query(db_path, base::paste0(
    "SELECT p.payer_id, p.payer_name, p.plan_name, ", payer_text_sql(), " AS payer_text, CAST(p.payer_type AS VARCHAR) AS stored_type, ",
    payer_type_case_sql(payer_text_sql(), rules), " AS current_type, p.trilliant_payer_type, coalesce(n.n_rows, 0) AS n_rows ",
    "FROM dim_payer p LEFT JOIN (SELECT payer_id, count(*) AS n_rows FROM fact_rate GROUP BY 1) n USING (payer_id)"
  )) |>
    dplyr::mutate(
      trilliant_type = map_trilliant_payer_type(.data$trilliant_payer_type),
      explicit_type = explicit_payer_type(.data$payer_text),
      # the adjudicated reference: what the text says outright, else Trilliant's label
      reference_type = dplyr::if_else(!base::is.na(.data$trilliant_type) & !base::is.na(.data$explicit_type),
                                      .data$explicit_type, .data$trilliant_type)
    )
}

#' Row-weighted confusion matrix, ours (current rules) x Trilliant's
payer_type_confusion <- function(payers) {
  payers |>
    dplyr::filter(!base::is.na(.data$trilliant_type)) |>
    dplyr::group_by(.data$current_type, .data$trilliant_type) |>
    dplyr::summarise(n_rows = base::sum(.data$n_rows), n_payers = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$n_rows))
}

#' The payer/plan strings behind the biggest disagreement cells
payer_type_disagreements <- function(payers, n = 200L) {
  payers |>
    dplyr::filter(!base::is.na(.data$trilliant_type), comparable_payer_type(.data$current_type) != .data$trilliant_type) |>
    dplyr::arrange(dplyr::desc(.data$n_rows)) |>
    dplyr::select("current_type", "trilliant_type", "explicit_type", "reference_type", "trilliant_payer_type",
                  "payer_name", "plan_name", "n_rows") |>
    utils::head(n)
}

#' Agreement between our payer_type and Trilliant's, weighted by fact_rate rows
check_payer_type_agreement <- function(payers, pass_at = 0.90, warn_at = 0.80) {
  description <- "Row-weighted agreement with Trilliant payer labels on the four main types"
  labelled <- payers |> dplyr::filter(!base::is.na(.data$trilliant_type))

  if (base::nrow(labelled) == 0L) {
    return(validation_row("payer_type_agreement", "payer_type", description, "skip",
                          detail = "no dim_payer row carries a trilliant_payer_type"))
  }

  labelled <- labelled |> dplyr::mutate(ours = comparable_payer_type(.data$current_type))
  agreement <- function(tbl, reference) base::sum(tbl$n_rows[tbl$ours == tbl[[reference]]]) / base::sum(tbl$n_rows)
  raw_main <- labelled |> dplyr::filter(.data$trilliant_type %in% main_payer_types())
  adjudicated_main <- labelled |> dplyr::filter(.data$reference_type %in% main_payer_types())
  agree_raw <- agreement(raw_main, "trilliant_type")
  agree_main <- agreement(adjudicated_main, "reference_type")
  agree_all <- agreement(labelled, "trilliant_type")
  overruled <- raw_main |> dplyr::filter(.data$reference_type != .data$trilliant_type)
  per_type <- adjudicated_main |>
    dplyr::group_by(.data$reference_type) |>
    dplyr::summarise(recall = base::sum(.data$n_rows[.data$ours == .data$reference_type]) / base::sum(.data$n_rows),
                     rows = base::sum(.data$n_rows), .groups = "drop")
  worst <- labelled |>
    dplyr::filter(.data$ours != .data$trilliant_type) |>
    dplyr::count(current_type = .data$ours, trilliant_type = .data$trilliant_type, wt = .data$n_rows, name = "n_rows", sort = TRUE) |>
    utils::head(3)
  unmapped <- base::unique(labelled$trilliant_payer_type[stringr::str_starts(labelled$trilliant_type, "unmapped")])
  # Trilliant's catch-all: what our rules make of it
  theirs_other <- labelled |>
    dplyr::filter(.data$trilliant_type == "other") |>
    dplyr::count(.data$current_type, wt = .data$n_rows, sort = TRUE) |>
    dplyr::mutate(share = .data$n / base::sum(.data$n)) |>
    utils::head(4)

  validation_row(
    "payer_type_agreement", "payer_type", description,
    grade_at_least(agree_main, pass_at, warn_at), metric = agree_main,
    threshold = base::paste0("adjudicated agreement: pass >= ", fmt_pct(pass_at, 0), ", warn >= ", fmt_pct(warn_at, 0)),
    detail = base::paste0(
      "adjudicated (plan text overrides Trilliant where it names the type; exchange counts as commercial) ",
      fmt_pct(agree_main), "; raw vs Trilliant labels ", fmt_pct(agree_raw), " on the four main types, ",
      fmt_pct(agree_all), " on all labels; plan text contradicts Trilliant on ", base::sum(overruled$n_rows), " rows (",
      fmt_pct(base::sum(overruled$n_rows) / base::sum(raw_main$n_rows)), " of main-type rows); recall by reference type: ",
      base::paste0(per_type$reference_type, " ", fmt_pct(per_type$recall), collapse = ", "),
      "; biggest disagreements (ours/theirs rows): ",
      base::paste0(worst$current_type, "/", worst$trilliant_type, " ", worst$n_rows, collapse = ", "),
      if (base::length(unmapped) > 0) base::paste0("; unmapped labels: ", base::paste(unmapped, collapse = ", ")) else "",
      if (base::nrow(theirs_other) > 0) {
        base::paste0("; Trilliant 'Other' (", fmt_pct(base::sum(labelled$n_rows[labelled$trilliant_type == "other"]) / base::sum(labelled$n_rows)),
                     " of labelled rows) is ours: ", base::paste0(theirs_other$current_type, " ", fmt_pct(theirs_other$share), collapse = ", "))
      } else {
        ""
      }
    )
  )
}

#' dim_payer.payer_type was built with the current rules
check_payer_type_current <- function(payers) {
  stale <- payers |> dplyr::filter(.data$stored_type != .data$current_type)

  validation_row(
    "payer_type_current", "payer_type", "dim_payer.payer_type matches config/payer_type_rules.csv",
    if (base::nrow(stale) == 0L) "pass" else "warn",
    metric = base::sum(stale$n_rows), threshold = "0 rows",
    detail = if (base::nrow(stale) == 0L) {
      "stored types match the current rules"
    } else {
      base::sprintf("%s payer/plan strings (%s fact rows) would change type; rebuild hpt.duckdb to apply the current rules",
                    base::nrow(stale), base::sum(stale$n_rows))
    }
  )
}

# ---- 5. Cross-source agreement -----------------------------------------------

#' Pairs of files (own crawl, Trilliant) with the same normalized MRF URL
#'
#' normalize_url_key() keeps the query string of script endpoints
#' ("download.aspx?pi=..."), so files served from one script stay apart.
cross_source_pairs <- function(db_path) {
  files <- validation_query(db_path, "SELECT file_id, CAST(source AS VARCHAR) AS source, mrf_url, CAST(last_updated_on AS VARCHAR) AS last_updated_on FROM dim_file") |>
    dplyr::mutate(url_key = normalize_url_key(.data$mrf_url)) |>
    dplyr::filter(!base::is.na(.data$url_key))

  dplyr::inner_join(
    files |> dplyr::filter(.data$source == "own_crawl") |> dplyr::select("url_key", own_id = "file_id", own_updated = "last_updated_on"),
    files |> dplyr::filter(.data$source == "trilliant") |> dplyr::select("url_key", tri_id = "file_id", tri_updated = "last_updated_on"),
    by = "url_key", relationship = "many-to-many"
  ) |>
    dplyr::mutate(same_version = dplyr::coalesce(.data$own_updated == .data$tri_updated, FALSE))
}

#' Payer or plan text as a comparison key: Trilliant keeps the JSON quotes
#' around names from JSON MRFs ("\"Aetna\""), which the own-crawl parser drops
payer_key_sql <- function(column) {
  base::sprintf("lower(regexp_replace(trim(regexp_replace(trim(coalesce(%s, '')), '^\"+|\"+$', '', 'g')), '\\s+', ' ', 'g'))", column)
}

#' For each own-crawl rate, the closest Trilliant rate for the same code and
#' payer/plan text in the paired file
cross_source_matches <- function(db_path, pairs) {
  values <- base::paste0("(", pairs$own_id, ", ", pairs$tri_id, ")", collapse = ", ")
  side <- function(id_column) {
    base::paste0(
      "SELECT p.own_id, p.tri_id, r.code_id, ", payer_key_sql("py.payer_name"), " AS payer, ",
      payer_key_sql("py.plan_name"), " AS plan, r.negotiated_dollar AS v ",
      "FROM pairs p JOIN fact_rate r ON r.file_id = p.", id_column, " LEFT JOIN dim_payer py ON py.payer_id = r.payer_id ",
      "WHERE r.negotiated_dollar IS NOT NULL"
    )
  }

  validation_query(db_path, base::paste0(
    "WITH pairs(own_id, tri_id) AS (VALUES ", values, "),\n",
    "own AS (SELECT row_number() OVER () AS rid, * FROM (", side("own_id"), ")),\n",
    "tri AS (", side("tri_id"), "),\n",
    "best AS (\n",
    "  SELECT o.rid, o.own_id, o.tri_id, o.v, min(abs(t.v - o.v)) AS diff\n",
    "  FROM own o LEFT JOIN tri t ON t.own_id = o.own_id AND t.tri_id = o.tri_id AND t.code_id = o.code_id\n",
    "   AND t.payer = o.payer AND t.plan = o.plan\n",
    "  GROUP BY ALL\n",
    ")\n",
    "SELECT own_id, tri_id, count(*) AS n_rates, count(diff) AS n_keyed,\n",
    "       count(*) FILTER (WHERE diff <= 0.01) AS n_exact, count(*) FILTER (WHERE diff <= 0.01 * v) AS n_within_1pct\n",
    "FROM best GROUP BY ALL ORDER BY own_id, tri_id"
  ))
}

#' Own crawl vs Trilliant negotiated rates for files present in both
check_cross_source <- function(db_path, sources = validation_sources(db_path), pass_at = 0.95, warn_at = 0.80) {
  description <- "Own crawl vs Trilliant rates for the same MRF URL (code x payer/plan)"

  if (!base::all(base::c("own_crawl", "trilliant") %in% sources)) {
    return(validation_row("cross_source_agreement", "cross_source", description, "skip",
                          detail = base::paste("needs both sources; database has:", base::paste(sources, collapse = ", "))))
  }

  pairs <- cross_source_pairs(db_path)

  if (base::nrow(pairs) == 0L) {
    return(validation_row("cross_source_agreement", "cross_source", description, "skip",
                          detail = "no own-crawl file shares a normalized MRF URL with a Trilliant file"))
  }

  matches <- cross_source_matches(db_path, pairs) |>
    dplyr::left_join(pairs |> dplyr::select("own_id", "tri_id", "same_version"), by = c("own_id", "tri_id"))
  share <- function(tbl, column) base::sum(tbl[[column]]) / base::max(base::sum(tbl$n_rates), 1)
  same <- matches |> dplyr::filter(.data$same_version)
  graded <- if (base::nrow(same) > 0L) same else matches

  validation_row(
    "cross_source_agreement", "cross_source", description,
    grade_at_least(share(graded, "n_within_1pct"), pass_at, warn_at), metric = share(graded, "n_within_1pct"),
    threshold = base::paste0("share within 1% on same-version pairs: pass >= ", fmt_pct(pass_at, 0), ", warn >= ", fmt_pct(warn_at, 0)),
    detail = base::sprintf(
      "%s file pairs (%s with equal last_updated_on); same-version: %s rates, within $0.01 %s, within 1%% %s; all pairs: %s rates, key found %s, within $0.01 %s, within 1%% %s",
      base::nrow(pairs), base::sum(pairs$same_version), base::sum(same$n_rates),
      fmt_pct(share(same, "n_exact")), fmt_pct(share(same, "n_within_1pct")),
      base::sum(matches$n_rates), fmt_pct(share(matches, "n_keyed")),
      fmt_pct(share(matches, "n_exact")), fmt_pct(share(matches, "n_within_1pct"))
    )
  )
}

# ---- 6. External benchmark: CMS OPPS Addendum B ------------------------------

#' The CMS OPPS Addendum B release used as the Medicare facility benchmark
#'
#' Addendum B lists the national unadjusted OPPS payment rate per HCPCS
#' code. CMS serves the zip behind an AMA click-through (CPT descriptors are
#' AMA-copyrighted); only payment rates are used here.
opps_addendum_b_source <- function() {
  base::list(
    release = "July 2026",
    page_url = "https://www.cms.gov/medicare/payment/prospective-payment-systems/hospital-outpatient-pps/quarterly-addenda-updates/july-2026-addendum-b",
    zip_url = "https://www.cms.gov/files/zip/july-2026-opps-addendum-b.zip"
  )
}

opps_benchmark_codes <- function() {
  base::c("45378", "45380", "45385", "58100", "58300", "88305", "G0121")
}

#' Download and unzip Addendum B; returns the CSV path
download_opps_addendum_b <- function(source = opps_addendum_b_source(), dest_dir = hpt_path("reference", "cms_opps")) {
  zip_path <- base::file.path(dest_dir, base::basename(source$zip_url))
  download_public_file(source$zip_url, zip_path)
  unzip_dir <- stringr::str_remove(zip_path, "\\.zip$")
  utils::unzip(zip_path, exdir = unzip_dir)

  csv_path <- find_opps_addendum_b(unzip_dir)
  if (base::is.null(csv_path)) {
    base::stop("No CSV inside ", zip_path)
  }

  write_csv_atomic(
    tibble::tibble(release = source$release, page_url = source$page_url, zip_url = source$zip_url,
                   zip_sha256 = sha256_file(zip_path), csv_path = csv_path, downloaded_at = utc_timestamp()),
    base::paste0(unzip_dir, "_provenance.csv")
  )
  csv_path
}

#' Most recent Addendum B CSV under a directory, or NULL
find_opps_addendum_b <- function(dir = hpt_path("reference", "cms_opps")) {
  paths <- base::list.files(dir, pattern = "[Aa]ddendum B.*\\.csv$", recursive = TRUE, full.names = TRUE)
  if (base::length(paths) == 0L) NULL else paths[[base::which.max(base::file.mtime(paths))]]
}

#' Parse Addendum B: one row per HCPCS code with its payment rate
#'
#' The CSV opens with a few title and copyright lines (Windows-1252) before
#' the "HCPCS Code" header; payment rates are text like "$1,222.56 ".
parse_opps_addendum_b <- function(path) {
  lines <- readr::read_lines(path, locale = readr::locale(encoding = "windows-1252"))
  header_idx <- base::which(stringr::str_detect(lines, "^\"?HCPCS Code\"?,"))[1]

  if (base::is.na(header_idx)) {
    base::stop("No 'HCPCS Code' header in ", path)
  }

  tbl <- readr::read_csv(
    base::I(base::paste(lines[header_idx:base::length(lines)], collapse = "\n")),
    col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE, name_repair = "minimal"
  )
  base::names(tbl) <- stringr::str_squish(base::names(tbl))

  tibble::tibble(
    code = stringr::str_to_upper(stringr::str_squish(tbl[["HCPCS Code"]])),
    short_descriptor = tbl[["Short Descriptor"]],
    status_indicator = stringr::str_squish(tbl[["SI"]]),
    apc = stringr::str_squish(tbl[["APC"]]),
    payment_rate = parse_money(tbl[["Payment Rate"]])
  ) |>
    dplyr::filter(!base::is.na(.data$code), base::nzchar(.data$code))
}

#' OPPS rates from a given CSV, the cached download, or (network = TRUE) a
#' fresh download; NULL when none is available
load_opps_rates <- function(opps_path = NULL, network = FALSE) {
  path <- opps_path %||% find_opps_addendum_b()

  if (base::is.null(path) && network) {
    path <- download_opps_addendum_b()
  }

  if (base::is.null(path)) {
    return(NULL)
  }

  rates <- parse_opps_addendum_b(path)
  base::attr(rates, "source_path") <- path
  rates
}

#' National Medicare facility median vs the OPPS national payment rate
check_opps_benchmark <- function(medians, opps_rates, codes = opps_benchmark_codes(), low = 0.5, high = 2) {
  description <- "National 'medicare' facility median / CMS OPPS Addendum B payment rate"
  threshold <- base::sprintf("ratio within %s to %s", low, high)

  if (base::is.null(opps_rates)) {
    return(validation_row("benchmark_opps", "benchmark", description, "skip",
                          detail = "no Addendum B file cached; run with network = TRUE once (downloads to reference/cms_opps)"))
  }

  source_note <- base::basename(base::attr(opps_rates, "source_path") %||% "Addendum B")

  rows <- base::lapply(codes, function(code) {
    rate <- opps_rates[opps_rates$code == code, ]
    ours <- national_median(medians, code, "medicare")
    id <- base::paste0("benchmark_opps_", code)
    label <- base::paste(description, code)

    if (base::nrow(rate) == 0L || base::is.na(rate$payment_rate[[1]])) {
      si <- if (base::nrow(rate) == 0L) "code absent" else base::paste0("status indicator ", rate$status_indicator[[1]])
      return(validation_row(id, "benchmark", label, "skip", threshold = threshold,
                            detail = base::paste0("no OPPS payment rate (", si, "; not separately paid under OPPS); ours ",
                                                  if (base::is.na(ours)) "missing" else fmt_num(ours))))
    }

    if (base::is.na(ours)) {
      return(validation_row(id, "benchmark", label, "skip", threshold = threshold,
                            detail = base::paste0("no national medicare facility median; OPPS ", fmt_num(rate$payment_rate[[1]]))))
    }

    ratio <- ours / rate$payment_rate[[1]]
    validation_row(
      id, "benchmark", label, if (ratio >= low && ratio <= high) "pass" else "warn",
      metric = ratio, threshold = threshold,
      detail = base::sprintf("ours %s vs OPPS %s (SI %s, APC %s; %s)", fmt_num(ours), fmt_num(rate$payment_rate[[1]]),
                             rate$status_indicator[[1]], rate$apc[[1]], source_note)
    )
  })

  dplyr::bind_rows(rows)
}

# ---- 7. Random raw re-check (own crawl, network opt-in) ----------------------

#' Rows as a sorted multiset of text keys (money rounded to cents)
rate_row_keys <- function(tbl) {
  money <- function(x) base::ifelse(base::is.na(x), "NA", base::sprintf("%.2f", x))
  base::sort(base::paste(tbl$code, dplyr::coalesce(tbl$payer_name, "<NA>"), dplyr::coalesce(tbl$plan_name, "<NA>"),
                         dplyr::coalesce(tbl$description, "<NA>"), money(tbl$negotiated_dollar), money(tbl$gross),
                         money(tbl$discounted_cash), sep = " | "))
}

#' Compare database rows with freshly parsed rows for the same codes
compare_rate_rows <- function(db_rows, parsed_rows) {
  db_keys <- rate_row_keys(db_rows)
  parsed_keys <- rate_row_keys(parsed_rows)
  db_counts <- base::table(db_keys)
  parsed_counts <- base::table(parsed_keys)
  keys <- base::union(base::names(db_counts), base::names(parsed_counts))
  lookup <- function(counts) { out <- base::as.integer(counts[keys]); out[base::is.na(out)] <- 0L; out }
  diff <- lookup(db_counts) - lookup(parsed_counts)

  base::list(identical = base::all(diff == 0L), n_db = base::length(db_keys), n_parsed = base::length(parsed_keys),
             only_db = base::sum(base::pmax(diff, 0L)), only_parsed = base::sum(base::pmax(-diff, 0L)))
}

#' Re-download, re-parse, and compare one own-crawl file
recheck_one_file <- function(db_path, file, codebook, n_codes) {
  work_dir <- base::tempfile("recheck_")
  base::on.exit(base::unlink(work_dir, recursive = TRUE), add = TRUE)

  downloads <- download_mrf(file$mrf_url, dest_dir = work_dir, overwrite = TRUE)
  ok_files <- downloads |> dplyr::filter(base::is.na(.data$error), !base::is.na(.data$local_path))

  if (base::nrow(ok_files) == 0L) {
    return(base::list(outcome = "download_failed", note = downloads$error[[1]] %||% "download failed"))
  }

  if (!base::identical(ok_files$sha256[[1]], file$mrf_file_id)) {
    return(base::list(outcome = "changed_upstream", note = "file hash differs from the extracted file"))
  }

  codes <- validation_query(db_path, base::sprintf(
    "SELECT DISTINCT d.code FROM fact_rate r JOIN dim_code d USING (code_id) WHERE r.file_id = %s ORDER BY 1", file$file_id
  ))$code
  codes <- if (base::length(codes) > n_codes) base::sample(codes, n_codes) else codes

  parsed <- dplyr::bind_rows(base::lapply(base::seq_len(base::nrow(ok_files)), function(i) {
    part <- ok_files[i, ]
    parser <- if (base::identical(part$format, "json")) parse_json_mrf else parse_csv_mrf
    parser(part$local_path, codebook, mrf_url = file$mrf_url, mrf_file_id = ok_files$sha256[[1]], retrieved_at = part$retrieved_at)
  }))
  db_rows <- validation_query(db_path, base::sprintf(
    base::paste("SELECT d.code, p.payer_name, p.plan_name, r.description, r.negotiated_dollar, r.gross, r.discounted_cash",
                "FROM fact_rate r JOIN dim_code d USING (code_id) LEFT JOIN dim_payer p USING (payer_id)",
                "WHERE r.file_id = %s AND d.code IN (%s)"),
    file$file_id, sql_string_list(codes)
  ))
  compared <- compare_rate_rows(db_rows, parsed |> dplyr::filter(.data$code %in% codes))

  base::list(
    outcome = if (compared$identical) "match" else "mismatch",
    note = base::sprintf("codes %s: %s db rows, %s re-parsed, %s only in db, %s only re-parsed",
                         base::paste(codes, collapse = "/"), compared$n_db, compared$n_parsed, compared$only_db, compared$only_parsed)
  )
}

#' Sample own-crawl files under `max_bytes`, re-download, re-parse, and
#' confirm fact_rate matches exactly for a few codes. Raw files are deleted.
check_raw_recheck <- function(db_path, network = FALSE, n_files = 5L, n_codes = 2L, max_bytes = 30e6,
                              state = read_gap_extract_state(),
                              codebook = load_codebook(base::file.path(base::getOption("hpt_repo_root", "."), "config", "codebook.csv")),
                              sources = validation_sources(db_path), seed = 20260912L) {
  description <- "Re-download and re-parse sampled own-crawl files; fact_rate rows match exactly"

  if (!network) {
    return(validation_row("raw_recheck", "raw_recheck", description, "skip", detail = "network = FALSE (opt in with run_validation(network = TRUE))"))
  }

  if (!"own_crawl" %in% sources) {
    return(validation_row("raw_recheck", "raw_recheck", description, "skip", detail = "no 'own_crawl' source in this database"))
  }

  own <- validation_query(db_path, "SELECT file_id, mrf_file_id, mrf_url FROM dim_file WHERE source = 'own_crawl'")
  eligible <- own |>
    dplyr::inner_join(
      state |> dplyr::filter(.data$status == "ok", .data$bytes < max_bytes) |> dplyr::distinct(.data$mrf_file_id, .data$bytes),
      by = "mrf_file_id"
    )

  if (base::nrow(eligible) == 0L) {
    return(validation_row("raw_recheck", "raw_recheck", description, "skip",
                          detail = base::sprintf("no own-crawl file under %s MB with an 'ok' gap_extract_state row", max_bytes / 1e6)))
  }

  base::set.seed(seed)
  sampled <- eligible[base::sample(base::nrow(eligible), base::min(n_files, base::nrow(eligible))), ]
  results <- base::lapply(base::seq_len(base::nrow(sampled)), function(i) {
    base::tryCatch(recheck_one_file(db_path, sampled[i, ], codebook, n_codes),
                   error = function(e) base::list(outcome = "error", note = base::conditionMessage(e)))
  })
  outcomes <- base::vapply(results, function(r) r$outcome, base::character(1))
  notes <- base::vapply(results, function(r) r$note, base::character(1))

  validation_row(
    "raw_recheck", "raw_recheck", description,
    if (base::any(outcomes %in% base::c("mismatch", "error"))) "fail" else if (base::all(outcomes == "match")) "pass" else "warn",
    metric = base::sum(outcomes == "match") / base::length(outcomes), threshold = "all sampled files match",
    detail = base::paste0("file ", sampled$file_id, " ", outcomes, " (", notes, ")", collapse = "; ")
  )
}

# ---- 8. Coverage -------------------------------------------------------------

#' CCNs with at least one rate, by state, vs the CMS roster
coverage_by_state <- function(db_path) {
  validation_query(db_path, base::paste(
    "WITH covered AS (SELECT DISTINCT b.ccn FROM bridge_file_ccn b WHERE EXISTS (SELECT 1 FROM fact_rate r WHERE r.file_id = b.file_id))",
    "SELECT h.state, count(DISTINCT h.ccn) AS roster_ccns, count(DISTINCT c.ccn) AS covered_ccns",
    "FROM dim_hospital h LEFT JOIN covered c ON c.ccn = h.ccn GROUP BY 1 ORDER BY 1"
  )) |>
    dplyr::mutate(share = .data$covered_ccns / .data$roster_ccns)
}

check_coverage <- function(coverage, pass_at = 0.70) {
  overall <- base::sum(coverage$covered_ccns) / base::max(base::sum(coverage$roster_ccns), 1)
  lowest <- coverage |> dplyr::filter(.data$roster_ccns >= 5) |> dplyr::arrange(.data$share) |> utils::head(5)

  validation_row(
    "coverage_ccn_share", "coverage", "Share of CMS roster CCNs with at least one rate",
    grade_at_least(overall, pass_at, 0), metric = overall, threshold = base::paste0("warn below ", fmt_pct(pass_at, 0)),
    detail = base::sprintf("%s of %s CCNs in %s states; %s states with none; lowest: %s",
                           base::sum(coverage$covered_ccns), base::sum(coverage$roster_ccns), base::nrow(coverage),
                           base::sum(coverage$covered_ccns == 0),
                           base::paste0(lowest$state, " ", fmt_pct(lowest$share, 0), collapse = ", "))
  )
}

#' Rates with no state never reach a median (state or national): files with
#' no CCN and no state, or CCNs missing from the roster. A blank state ('')
#' is worse: the medians' `state IS NOT NULL` lets it through as a state of
#' its own, and into the national row.
check_rates_without_state <- function(db_path, max_share = 0.01) {
  counts <- validation_query(db_path, base::paste(
    "SELECT count(*) AS n, count(*) FILTER (WHERE state IS NULL) AS no_state,",
    "count(DISTINCT file_id) FILTER (WHERE state IS NULL) AS files,",
    "count(*) FILTER (WHERE state IS NULL AND ccn IS NULL) AS unbridged,",
    "count(*) FILTER (WHERE state IS NULL AND ccn IS NOT NULL) AS unrostered,",
    "count(*) FILTER (WHERE trim(state) = '') AS blank,",
    "count(DISTINCT file_id) FILTER (WHERE trim(state) = '') AS blank_files",
    "FROM v_hospital_rate"
  ))
  share <- (counts$no_state + counts$blank) / base::max(counts$n, 1)

  validation_row(
    "coverage_rates_without_state", "coverage", "Rates with no state (dropped from every median) or a blank state",
    if (counts$blank > 0) "warn" else grade_at_most(share, max_share, Inf), metric = share,
    threshold = base::paste0("warn above ", fmt_pct(max_share, 0), " or on any blank state"),
    detail = base::sprintf(
      "%s of %s v_hospital_rate rows with no state in %s files: %s from files with no CCN and no state, %s bridged to CCNs missing from dim_hospital; %s rows in %s files have a blank state ('') and form their own median group",
      counts$no_state, counts$n, counts$files, counts$unbridged, counts$unrostered, counts$blank, counts$blank_files
    )
  )
}

#' Codes in state_median_headline() (MS-DRG 621 is the inpatient bariatric
#' facility payment)
headline_codes <- function() {
  base::c("45378", "58100", "58300", "43775", "43644", "621")
}

#' State x insurance cells for the headline codes with fewer than
#' `min_hospitals` hospitals (suppressed in the headline)
thin_state_cells <- function(medians, min_hospitals = 3L) {
  medians |>
    dplyr::filter(.data$code %in% headline_codes(), .data$fee_type == "facility", .data$state != "US") |>
    dplyr::group_by(.data$code, .data$insurance_type) |>
    dplyr::summarise(
      n_states_publishable = base::sum(.data$n_hospitals >= min_hospitals),
      n_states_thin = base::sum(.data$n_hospitals < min_hospitals),
      thin_states = base::paste(base::sort(.data$state[.data$n_hospitals < min_hospitals]), collapse = " "),
      .groups = "drop"
    )
}

#' One row per headline code: states with a publishable commercial median
check_thin_states <- function(medians, min_hospitals = 3L, pass_states = 40L) {
  thin <- thin_state_cells(medians, min_hospitals)

  dplyr::bind_rows(base::lapply(headline_codes(), function(code) {
    cells <- thin |> dplyr::filter(.data$code == !!code)
    commercial <- cells |> dplyr::filter(.data$insurance_type == "commercial")
    n_ok <- if (base::nrow(commercial) == 0L) 0L else commercial$n_states_publishable[[1]]

    validation_row(
      base::paste0("coverage_states_", code), "coverage",
      base::sprintf("States with >= %s hospitals for a commercial %s facility median", min_hospitals, code),
      if (base::nrow(cells) == 0L) "skip" else if (n_ok >= pass_states) "pass" else "warn",
      metric = n_ok, threshold = base::paste0("warn below ", pass_states, " states"),
      detail = if (base::nrow(cells) == 0L) {
        "no facility medians for this code"
      } else {
        base::paste0("suppressed (<", min_hospitals, " hospitals): ",
                     base::paste0(cells$insurance_type, " ", cells$n_states_thin, " states", collapse = ", "),
                     "; commercial thin: ", dplyr::coalesce(commercial$thin_states[1], ""))
      }
    )
  }))
}

#' state_median_headline() blanks every cell below min_hospitals and keeps
#' every other one
check_headline_suppression <- function(medians, min_hospitals = 3L, headline_fn = state_median_headline) {
  description <- "state_median_headline() suppresses cells with too few hospitals"
  headline <- headline_fn(medians, min_hospitals = min_hospitals)
  long <- headline |>
    tidyr::pivot_longer(-dplyr::all_of(base::c("state", "insurance_type")), names_to = "procedure", values_to = "value") |>
    # headline columns end in their code: colonoscopy_45378, bariatric_ms_drg_621
    dplyr::mutate(code = stringr::str_extract(.data$procedure, "[^_]+$"))
  joined <- long |>
    dplyr::inner_join(
      medians |> dplyr::filter(.data$fee_type == "facility") |> dplyr::select("state", "insurance_type", "code", "median_price", "n_hospitals"),
      by = base::c("state", "insurance_type", "code")
    )

  if (base::nrow(joined) == 0L) {
    return(validation_row("coverage_headline_suppression", "coverage", description, "skip", detail = "no headline cells"))
  }

  leaked <- base::sum(joined$n_hospitals < min_hospitals & !base::is.na(joined$value))
  dropped <- base::sum(joined$n_hospitals >= min_hospitals & (base::is.na(joined$value) | joined$value != joined$median_price))

  validation_row(
    "coverage_headline_suppression", "coverage", description,
    if (leaked + dropped == 0) "pass" else "fail",
    metric = leaked + dropped, threshold = "0 leaked or dropped cells",
    detail = base::sprintf("%s headline cells: %s published, %s suppressed; %s thin cells leaked, %s publishable cells dropped",
                           base::nrow(joined), base::sum(!base::is.na(joined$value)), base::sum(base::is.na(joined$value)),
                           leaked, dropped)
  )
}

# ---- Runner and report -------------------------------------------------------

#' Run every check and write the report
#'
#' @param db_path hpt.duckdb to validate (opened read-only).
#' @param out_dir Where validation_report.csv/.md and the supporting tables
#'   go; also where state_insurance_medians.csv is compared from.
#' @param network Opt in to network use: the raw MRF re-check and, when no
#'   Addendum B is cached, its download.
#' @param opps_path Addendum B CSV to use instead of the cached download.
#' @param answers Known per-hospital values (known_answers()).
#' @param rules Payer-type rules to evaluate (current config by default).
#' @return Tibble of checks (check_id, category, description, status,
#'   metric, threshold, detail).
run_validation <- function(db_path = hpt_database_path(), out_dir = hpt_path("output"), network = FALSE,
                           opps_path = NULL, answers = known_answers(), rules = load_payer_type_rules(),
                           state = read_gap_extract_state(), codebook = NULL) {
  if (!base::file.exists(db_path)) {
    base::stop("Database not found: ", db_path)
  }

  base::dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  sources <- validation_sources(db_path)
  base::message("Validating ", db_path, " (sources: ", base::paste(sources, collapse = ", "), ")")
  codebook <- codebook %||% load_codebook(base::file.path(base::getOption("hpt_repo_root", "."), "config", "codebook.csv"))

  # every median-based check sees the published medians: same excluded files
  excluded <- read_median_exclusions(out_dir)
  if (!base::is.null(excluded)) {
    base::message("Medians exclude ", base::length(excluded), " file(s) listed in median_excluded_file_ids.csv")
  }
  medians <- base::tryCatch(validation_medians(db_path, excluded), error = function(e) {
    base::message("Could not compute state medians: ", base::conditionMessage(e))
    NULL
  })
  payers <- base::tryCatch(payer_type_table(db_path, rules), error = function(e) {
    base::message("Could not read dim_payer: ", base::conditionMessage(e))
    NULL
  })
  coverage <- base::tryCatch(coverage_by_state(db_path), error = function(e) NULL)
  opps_rates <- base::tryCatch(load_opps_rates(opps_path, network), error = function(e) {
    base::message("Could not load OPPS Addendum B: ", base::conditionMessage(e))
    NULL
  })
  above_gross <- base::tryCatch(files_above_gross(db_path), error = function(e) {
    base::message("Could not list files above gross: ", base::conditionMessage(e))
    NULL
  })
  needs <- function(x, what) if (base::is.null(x)) base::stop(what, " unavailable") else x

  checks <- base::list(
    base::list("integrity_fact_keys", "integrity", function() check_fact_keys(db_path)),
    base::list("integrity_dim_keys", "integrity", function() check_dim_keys(db_path)),
    base::list("integrity_bridge", "integrity", function() check_bridge(db_path)),
    base::list("integrity_row_counts", "integrity", function() check_row_counts(db_path)),
    base::list("integrity_file_ids", "integrity", function() check_file_ids(db_path)),
    base::list("integrity_medians_current", "integrity", function() check_medians_current(db_path, out_dir, medians)),
    base::list("known_answer", "known_answer", function() check_known_answers(db_path, answers, sources)),
    base::list("plausibility_rates", "plausibility", function() check_rate_plausibility(db_path)),
    base::list("plausibility_files_above_gross", "plausibility", function() check_files_above_gross(needs(above_gross, "files above gross"))),
    base::list("plausibility_concept_order", "plausibility", function() check_concept_order(needs(medians, "state medians"))),
    base::list("plausibility_payer_order", "plausibility", function() check_payer_order(needs(medians, "state medians"))),
    base::list("plausibility_self_pay_vs_cash", "plausibility", function() check_self_pay_consistency(needs(medians, "state medians"))),
    base::list("payer_type_agreement", "payer_type", function() check_payer_type_agreement(needs(payers, "dim_payer"))),
    base::list("payer_type_current", "payer_type", function() check_payer_type_current(needs(payers, "dim_payer"))),
    base::list("cross_source_agreement", "cross_source", function() check_cross_source(db_path, sources)),
    base::list("benchmark_opps", "benchmark", function() check_opps_benchmark(needs(medians, "state medians"), opps_rates)),
    base::list("raw_recheck", "raw_recheck", function() check_raw_recheck(db_path, network, state = state, codebook = codebook, sources = sources)),
    base::list("coverage_ccn_share", "coverage", function() check_coverage(needs(coverage, "coverage by state"))),
    base::list("coverage_rates_without_state", "coverage", function() check_rates_without_state(db_path)),
    base::list("coverage_states", "coverage", function() check_thin_states(needs(medians, "state medians"))),
    base::list("coverage_headline_suppression", "coverage", function() check_headline_suppression(needs(medians, "state medians")))
  )

  report <- dplyr::bind_rows(base::lapply(checks, function(check) {
    run_check_safely(check[[1]], check[[2]], check[[1]], check[[3]])
  }))

  write_csv_atomic(report, base::file.path(out_dir, "validation_report.csv"))
  base::writeLines(validation_markdown(report, db_path, sources), base::file.path(out_dir, "validation_report.md"))

  if (!base::is.null(payers) && base::any(!base::is.na(payers$trilliant_type))) {
    write_csv_atomic(payer_type_confusion(payers), base::file.path(out_dir, "validation_payer_confusion.csv"))
    write_csv_atomic(payer_type_disagreements(payers), base::file.path(out_dir, "validation_payer_disagreements.csv"))
  }
  if (!base::is.null(above_gross)) {
    # written even when empty, so the medians step can rely on it existing
    write_csv_atomic(above_gross, base::file.path(out_dir, "files_rates_above_gross.csv"))
  }
  if (!base::is.null(coverage)) {
    write_csv_atomic(coverage, base::file.path(out_dir, "validation_coverage_by_state.csv"))
  }
  if (!base::is.null(medians)) {
    write_csv_atomic(thin_state_cells(medians), base::file.path(out_dir, "validation_thin_states.csv"))
  }

  counts <- base::table(base::factor(report$status, levels = validation_statuses()))
  base::message("Validation: ", base::paste(base::names(counts), counts, sep = " ", collapse = ", "),
                ". Report: ", base::file.path(out_dir, "validation_report.md"))
  report
}

md_cell <- function(x) {
  x <- dplyr::coalesce(base::as.character(x), "")
  stringr::str_replace_all(stringr::str_replace_all(x, "\\|", "/"), "[\r\n]+", " ")
}

md_metric <- function(x) {
  base::ifelse(base::is.na(x), "", base::formatC(x, format = "g", digits = 4))
}

#' Short markdown summary of a validation report
validation_markdown <- function(report, db_path, sources) {
  counts <- base::table(base::factor(report$status, levels = validation_statuses()))
  table_lines <- function(tbl) {
    base::c(
      "| check | status | metric | threshold | detail |",
      "|---|---|---|---|---|",
      base::sprintf("| %s | %s | %s | %s | %s |", md_cell(tbl$check_id), tbl$status, md_metric(tbl$metric),
                    md_cell(tbl$threshold), md_cell(tbl$detail))
    )
  }
  flagged <- report |>
    dplyr::filter(.data$status %in% base::c("fail", "warn")) |>
    dplyr::arrange(base::match(.data$status, base::c("fail", "warn")))

  base::c(
    "# hpt.duckdb validation",
    "",
    base::paste0("- Database: `", db_path, "`"),
    base::paste0("- Sources: ", base::paste(sources, collapse = ", ")),
    base::paste0("- Run at: ", utc_timestamp()),
    base::paste0("- Checks: ", base::nrow(report), " (", base::paste(counts, base::names(counts), collapse = ", "), ")"),
    "",
    "## Failures and warnings",
    "",
    if (base::nrow(flagged) == 0L) "None." else table_lines(flagged),
    "",
    "## All checks",
    "",
    table_lines(report)
  )
}
