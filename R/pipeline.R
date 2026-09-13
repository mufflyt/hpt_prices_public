#' Orchestration shared by the analysis/ scripts
#'
#' Each helper is resumable: it records per-item status under
#' HPT_DATA_DIR/state/ and skips items already done.

#' Cap for pilot runs (HPT_MAX_ITEMS); Inf when unset
max_items <- function() {
  value <- base::Sys.getenv("HPT_MAX_ITEMS", unset = "")
  if (!base::nzchar(value)) Inf else base::as.numeric(value)
}

take_max_items <- function(x) {
  n <- max_items()
  if (base::is.finite(n) && base::length(x) > n) {
    base::message("HPT_MAX_ITEMS = ", n, ": taking the first ", n, " of ", base::length(x), ".")
    x <- x[base::seq_len(n)]
  }
  x
}

#' Reference inputs (CMS roster, enrollments, CHSP, tracker), downloaded once
#' and reused from HPT_DATA_DIR/reference on later runs
load_reference_inputs <- function(refresh = FALSE) {
  or_null <- function(expr) base::tryCatch(expr, error = function(e) NULL)

  roster_path <- or_null(latest_reference_file(hpt_path("reference", "cms_hospitals"), "^hospital_general_information_[0-9_]+\\.csv$"))
  roster <- if (refresh || base::is.null(roster_path)) download_cms_hospital_frame() else load_cms_hospital_frame(roster_path)

  enrollments_cache <- hpt_path("reference", "npi_ccn_crosswalk.parquet")
  if (refresh || !base::file.exists(enrollments_cache)) {
    enrollments <- download_hospital_enrollments()
    npi_xwalk <- npi_ccn_crosswalk(enrollments$enrollments, enrollments$additional_npis)
    write_parquet_atomic(npi_xwalk, enrollments_cache)
  }
  npi_xwalk <- tibble::as_tibble(arrow::read_parquet(enrollments_cache))

  chsp_path <- hpt_path("reference", "ahrq", "chsp-hospital-linkage-2023.csv")
  if (!base::file.exists(chsp_path)) {
    import_chsp_linkage()
  }
  universe <- build_hospital_universe(roster, load_chsp_linkage(chsp_path))

  tracker_dir <- if (refresh) NULL else or_null(latest_tracker_snapshot())
  tracker_dir <- tracker_dir %||% download_tracker_snapshot()
  tracker <- load_tracker(tracker_dir)

  base::list(universe = universe, npi_xwalk = npi_xwalk, tracker = tracker)
}

own_crawl_prices_dir <- function() {
  hpt_path("prices", "source=own_crawl")
}

#' File-level facility records from our own parsed MRFs (one per file),
#' in the shape match_facilities_to_ccn() expects
own_crawl_facilities <- function() {
  files <- base::list.files(own_crawl_prices_dir(), pattern = "\\.parquet$", full.names = TRUE)

  if (base::length(files) == 0L) {
    return(NULL)
  }

  # When one URL yields several files (a system zip with a CSV per hospital),
  # the URL says nothing about which hospital each file is, so blank it and
  # let the NPI and name/address tiers match each member file.
  duckdb_query(base::sprintf(
    base::paste(
      "SELECT 'own:' || mrf_file_id AS facility_key, 'own_crawl' AS source,",
      "CASE WHEN count(*) OVER (PARTITION BY min(mrf_url)) > 1 THEN NULL ELSE min(mrf_url) END AS mrf_url,",
      "mrf_file_id, min(hospital_name) AS hospital_name, min(location_name) AS location_name,",
      "CAST(NULL AS VARCHAR) AS address, CAST(NULL AS VARCHAR) AS city, min(license_state) AS state,",
      "min(license_number) AS license_number, min(license_state) AS license_state, min(type_2_npi) AS type_2_npi",
      "FROM read_parquet(%s, union_by_name = true) GROUP BY mrf_file_id"
    ),
    sql_string(base::file.path(own_crawl_prices_dir(), "*.parquet"))
  ))
}

#' Run the CCN crosswalk over every facility record we have (Trilliant and
#' own crawl) and write crosswalk, file->CCN map, and coverage tables
run_ccn_crosswalk <- function(reference) {
  # every facility field is an identifier or text; coerce so the two
  # sources stack even when one side has an all-NULL (typed) column
  as_chr <- function(tbl) if (base::is.null(tbl)) NULL else dplyr::mutate(tbl, dplyr::across(dplyr::everything(), base::as.character))
  facilities <- dplyr::bind_rows(
    if (base::file.exists(hpt_path("prices", "source=trilliant", "facilities.parquet"))) {
      as_chr(tibble::as_tibble(arrow::read_parquet(hpt_path("prices", "source=trilliant", "facilities.parquet"))))
    },
    as_chr(own_crawl_facilities())
  )

  if (base::nrow(facilities) == 0L) {
    base::stop("No facility records yet: run analysis/01_trilliant_extract.R (and/or the gap crawl) first.")
  }

  base::message("Matching ", scales::comma(base::nrow(facilities)), " facility records to CCNs.")
  crosswalk <- match_facilities_to_ccn(
    facilities,
    tracker_manifest = reference$tracker$manifest,
    npi_xwalk = reference$npi_xwalk,
    universe = reference$universe
  )

  write_parquet_atomic(crosswalk, hpt_path("crosswalk", "facility_ccn.parquet"))

  file_ccn <- crosswalk |>
    dplyr::filter(!base::is.na(.data$ccn), !.data$ccn_ambiguous) |>
    dplyr::distinct(.data$mrf_file_id, .data$ccn, .data$ccn_match_method)
  write_parquet_atomic(file_ccn, hpt_path("crosswalk", "file_ccn.parquet"))

  coverage <- coverage_report(crosswalk, reference$universe)
  for (name in base::names(coverage)) {
    write_csv_atomic(coverage[[name]], hpt_path("crosswalk", base::paste0("coverage_", name, ".csv")))
  }

  base::list(crosswalk = crosswalk, file_ccn = file_ccn, coverage = coverage)
}

#' Own-crawl files that duplicate a Trilliant file
#'
#' When both sources hold the same MRF (same URL key), keep one copy so a
#' hospital's contracts aren't counted twice: prefer the newer
#' last_updated_on, and Trilliant on ties or unknown dates. Returns the
#' mrf_file_ids to exclude from the database build.
cross_source_duplicate_file_ids <- function() {
  trilliant_glob <- hpt_path("prices", "source=trilliant", "prices", "**", "*.parquet")
  own_glob <- base::file.path(own_crawl_prices_dir(), "*.parquet")

  if (base::length(base::Sys.glob(trilliant_glob)) == 0L || base::length(base::Sys.glob(own_glob)) == 0L) {
    return(base::character())
  }

  files_of <- function(glob) {
    duckdb_query(base::sprintf(
      "SELECT mrf_file_id, min(mrf_url) AS mrf_url, max(last_updated_on) AS last_updated_on FROM read_parquet(%s, hive_partitioning = true, union_by_name = true) GROUP BY mrf_file_id",
      sql_string(glob)
    )) |>
      dplyr::mutate(url_key = normalize_url_key(.data$mrf_url))
  }

  trilliant <- files_of(trilliant_glob)
  own <- files_of(own_glob)

  pairs <- dplyr::inner_join(own, trilliant, by = "url_key", suffix = base::c("_own", "_tri"), relationship = "many-to-many")
  own_newer <- !base::is.na(pairs$last_updated_on_own) & !base::is.na(pairs$last_updated_on_tri) &
    pairs$last_updated_on_own > pairs$last_updated_on_tri

  drop_ids <- base::unique(base::c(pairs$mrf_file_id_own[!own_newer], pairs$mrf_file_id_tri[own_newer]))
  base::message("Cross-source duplicates: ", base::length(drop_ids), " file(s) excluded (",
                base::sum(!own_newer), " own-crawl, ", base::sum(own_newer), " Trilliant).")
  drop_ids
}

#' CCNs with at least one matched MRF so far
covered_ccns <- function() {
  path <- hpt_path("crosswalk", "file_ccn.parquet")
  if (!base::file.exists(path)) {
    return(base::character())
  }
  base::unique(arrow::read_parquet(path)$ccn)
}

#' MRF URL keys already extracted by any source (so the gap crawl never
#' re-downloads a file Trilliant already parsed)
extracted_url_keys <- function() {
  keys <- base::character()
  trilliant_facilities <- hpt_path("prices", "source=trilliant", "facilities.parquet")

  if (base::file.exists(trilliant_facilities)) {
    keys <- base::c(keys, normalize_url_key(arrow::read_parquet(trilliant_facilities)$mrf_url))
  }

  state <- read_gap_extract_state()
  keys <- base::c(keys, normalize_url_key(state$url[state$status == "ok"]))
  base::unique(stats::na.omit(keys))
}

gap_extract_state_path <- function() {
  hpt_path("state", "gap_extract_state.csv")
}

read_gap_extract_state <- function() {
  path <- gap_extract_state_path()
  if (!base::file.exists(path)) {
    return(tibble::tibble(url = base::character(), stage = base::character(), status = base::character(),
                          mrf_file_id = base::character(), format = base::character(), bytes = base::double(),
                          n_rows = base::integer(), seconds = base::double(), error = base::character(),
                          extracted_at = base::character()))
  }
  read_csv_chr(path) |>
    dplyr::mutate(bytes = base::as.double(.data$bytes), n_rows = base::as.integer(.data$n_rows),
                  seconds = base::as.double(.data$seconds))
}

#' Download, parse, and store MRFs one at a time, deleting each raw file
#' after extraction (HPT_KEEP_RAW=true keeps it). Disk use stays bounded by
#' the largest single file.
#'
#' @param urls MRF URLs.
#' @param stage Label recorded in the state file ("tracker_c1", "txt_c2", ...).
extract_gap_mrfs <- function(urls, codebook, stage) {
  state <- read_gap_extract_state()
  urls <- base::unique(stats::na.omit(urls))
  # "blocked" (401/403) is final: we never work around a site's refusal.
  done <- state$url[state$status %in% base::c("ok", "no_target_codes", "blocked")]
  todo <- take_max_items(base::setdiff(urls, done))
  keep_raw <- base::tolower(base::Sys.getenv("HPT_KEEP_RAW", unset = "false")) %in% base::c("1", "true", "yes")
  out_dir <- own_crawl_prices_dir()
  base::dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  base::message(stage, ": ", base::length(urls), " MRF URLs, ", base::length(base::intersect(urls, done)),
                " already extracted, ", base::length(todo), " to process.")

  for (url in todo) {
    started <- base::Sys.time()
    result <- base::tryCatch({
      downloads <- download_mrf(url)
      ok_files <- downloads |> dplyr::filter(base::is.na(.data$error), !base::is.na(.data$local_path))

      if (base::nrow(ok_files) == 0L) {
        base::stop(downloads$error[[1]] %||% "download failed")
      }

      parts <- base::lapply(base::seq_len(base::nrow(ok_files)), function(i) {
        file <- ok_files[i, ]
        parser <- if (base::identical(file$format, "json")) parse_json_mrf else parse_csv_mrf
        parser(file$local_path, codebook, mrf_url = url, mrf_file_id = file$sha256, retrieved_at = file$retrieved_at)
      })
      prices <- dplyr::bind_rows(parts)

      # A file id must be a full sha256. A 2-character id (a raw digest byte
      # recycled across rows) once merged three hospitals' rates together.
      bad_ids <- base::setdiff(base::unique(prices$mrf_file_id), NA_character_)
      bad_ids <- bad_ids[!stringr::str_detect(bad_ids, "^[0-9a-f]{64}$")]
      if (base::length(bad_ids) > 0L || !stringr::str_detect(ok_files$sha256[[1]], "^[0-9a-f]{64}$")) {
        base::stop("Refusing to store prices with malformed file ids: ", base::paste(utils::head(bad_ids, 3), collapse = ", "))
      }

      if (base::nrow(prices) > 0L) {
        write_parquet_atomic(prices, base::file.path(out_dir, base::paste0(ok_files$sha256[[1]], ".parquet")))
      }

      if (!keep_raw) {
        base::unlink(ok_files$local_path)
      }

      tibble::tibble(status = if (base::nrow(prices) > 0L) "ok" else "no_target_codes",
                     mrf_file_id = ok_files$sha256[[1]], format = ok_files$format[[1]],
                     bytes = base::sum(ok_files$bytes), n_rows = base::nrow(prices), error = NA_character_)
    }, error = function(e) {
      message_text <- base::conditionMessage(e)
      blocked <- stringr::str_detect(message_text, "returned error: 40[13]\\b|HTTP 40[13]\\b|status 40[13]\\b")
      tibble::tibble(status = if (blocked) "blocked" else "failed", mrf_file_id = NA_character_,
                     format = NA_character_, bytes = NA_real_, n_rows = 0L, error = message_text)
    })

    row <- dplyr::bind_cols(
      tibble::tibble(url = url, stage = stage),
      result,
      tibble::tibble(seconds = base::as.numeric(base::difftime(base::Sys.time(), started, units = "secs")),
                     extracted_at = utc_timestamp())
    )
    state <- dplyr::bind_rows(state |> dplyr::filter(.data$url != !!url), row)
    write_csv_atomic(state, gap_extract_state_path())
    base::message("  [", row$status, "] ", url, " (", row$n_rows, " rows, ", base::round(row$seconds), " s)")
  }

  state |> dplyr::filter(.data$url %in% urls)
}
