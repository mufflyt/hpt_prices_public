#' cms-hpt-tracker snapshots: a CCN -> MRF URL bridge that needs checking
#'
#' github.com/anthonyisnotadev/cms-hpt-tracker (AGPL-3.0) publishes an audit
#' of every CMS hospital's cms-hpt.txt and MRF. We use ONLY its published
#' CSVs, as data; none of its code is copied or run. Each snapshot is pinned
#' to a commit SHA so a rerun can fetch exactly the same files.
#'
#' Its CCN assignments are mostly name-based (in the 2026-09 snapshot:
#' match_method "name" 2853 rows, "exact-mrf-url-corpus" 363,
#' "llm-adjudicated" 281, "global-name" 202, ...), and 59 MRF URLs are
#' assigned to more than one CCN, so R/ccn_match.R treats the manifest as a
#' bridge to verify, not as ground truth.

tracker_repo <- function() {
  "anthonyisnotadev/cms-hpt-tracker"
}

tracker_files <- function() {
  base::c("manifest.csv", "compliance.csv", "gaps.csv")
}

#' Current commit SHA of a branch of the tracker repository
#'
#' Uses GITHUB_TOKEN when set (the unauthenticated API allows 60 calls/hour).
resolve_tracker_sha <- function(branch = "main") {
  api_url <- base::paste0("https://api.github.com/repos/", tracker_repo(), "/commits/", branch)
  base::message("Resolving tracker commit: ", api_url)

  payload <- fetch_public_json(api_url, timeout_s = 30, bearer_token = base::Sys.getenv("GITHUB_TOKEN"))
  sha <- payload$sha %||% NA_character_

  if (base::is.na(sha) || !stringr::str_detect(sha, "^[0-9a-f]{40}$")) {
    base::stop("GitHub API returned no commit SHA for ", tracker_repo(), "@", branch)
  }

  sha
}

tracker_file_url <- function(ref, file) {
  base::paste0("https://raw.githubusercontent.com/", tracker_repo(), "/", ref, "/data/hpt-audit/", file)
}

#' Download manifest.csv, compliance.csv, and gaps.csv at one commit
#'
#' Files land in `<dest_dir>/<sha>/` next to `tracker_provenance.csv`
#' (sha, file, url, bytes, sha256, downloaded_at), so snapshots never
#' overwrite each other.
#'
#' @param dest_dir Parent directory for snapshots.
#' @param ref Commit SHA (or other git ref) to fetch; NULL resolves the
#'   current head of `main` through the GitHub API.
#' @return The snapshot directory, for [load_tracker()].
download_tracker_snapshot <- function(dest_dir = hpt_path("reference", "tracker"), ref = NULL) {
  sha <- ref %||% resolve_tracker_sha()
  snapshot_dir <- base::file.path(dest_dir, sha)

  if (!base::dir.exists(snapshot_dir)) {
    base::dir.create(snapshot_dir, recursive = TRUE)
  }

  provenance_tbl <- purrr::map_dfr(
    tracker_files(),
    function(file) {
      url <- tracker_file_url(sha, file)
      local_path <- download_public_file(url, base::file.path(snapshot_dir, file), overwrite = TRUE, timeout_s = 300)

      tibble::tibble(
        sha = sha,
        file = file,
        url = url,
        bytes = base::file.size(local_path),
        sha256 = sha256_file(local_path),
        downloaded_at = utc_timestamp()
      )
    }
  )

  write_csv_atomic(provenance_tbl, base::file.path(snapshot_dir, "tracker_provenance.csv"))
  base::message("Saved tracker snapshot ", sha, " to ", snapshot_dir)

  base::invisible(snapshot_dir)
}

#' Most recently downloaded tracker snapshot directory under `dest_dir`
latest_tracker_snapshot <- function(dest_dir = hpt_path("reference", "tracker")) {
  provenance_paths <- base::list.files(dest_dir, pattern = "^tracker_provenance\\.csv$", recursive = TRUE, full.names = TRUE)

  if (base::length(provenance_paths) == 0L) {
    base::stop("No tracker snapshot under ", dest_dir, "; run download_tracker_snapshot() first.")
  }

  downloaded_at <- base::vapply(
    provenance_paths,
    function(path) base::max(read_csv_chr(path)$downloaded_at),
    base::character(1)
  )

  base::dirname(provenance_paths[[base::order(downloaded_at, decreasing = TRUE)[[1]]]])
}

#' Load a tracker snapshot with every column (CCN included) as character
#'
#' @return list(manifest, compliance, gaps).
load_tracker <- function(dir) {
  paths <- base::file.path(dir, tracker_files())
  missing_paths <- paths[!base::file.exists(paths)]

  if (base::length(missing_paths) > 0L) {
    base::stop("Tracker snapshot is missing: ", base::paste(missing_paths, collapse = ", "))
  }

  tables <- purrr::map(paths, read_csv_chr)
  base::names(tables) <- base::c("manifest", "compliance", "gaps")

  require_columns(tables$manifest, base::c("ccn", "hospital_name", "city", "state", "location_name", "mrf_url"), "Tracker manifest")
  require_columns(tables$compliance, base::c("ccn", "finding", "assessable"), "Tracker compliance")
  require_columns(tables$gaps, base::c("ccn", "seeded_domain", "pointer_status"), "Tracker gaps")

  tables
}

#' Split the manifest's extra_mrf_urls cell into URLs
#'
#' The field is undocumented and empty in every row of the 2026-09
#' snapshot, so accept the plausible encodings: a JSON array, or URLs
#' separated by ";", "|", whitespace, or a comma that starts a new URL.
#'
#' @return A list of character vectors, one per input element.
parse_extra_mrf_urls <- function(x) {
  purrr::map(
    x,
    function(cell) {
      if (base::is.na(cell) || !base::nzchar(stringr::str_trim(cell))) {
        return(base::character())
      }

      parts <- stringr::str_split(cell, "[\\s;|\"\\[\\]]+|,(?=\\s*\"?https?://)")[[1]]
      parts <- stringr::str_trim(parts)

      parts[stringr::str_detect(parts, stringr::regex("^https?://", ignore_case = TRUE))]
    }
  )
}

#' Long table of every tracker MRF URL with its CCN
#'
#' One row per CCN x URL, primary `mrf_url` and each `extra_mrf_urls` entry,
#' with `url_key` from [normalize_url_key()] for exact matching.
tracker_mrf_urls <- function(manifest) {
  require_columns(manifest, base::c("ccn", "mrf_url"), "Tracker manifest")

  if (!"extra_mrf_urls" %in% base::names(manifest)) {
    manifest$extra_mrf_urls <- NA_character_
  }

  if (!"match_method" %in% base::names(manifest)) {
    manifest$match_method <- NA_character_
  }

  primary_tbl <- manifest |>
    dplyr::transmute(
      ccn = .data$ccn,
      mrf_url = .data$mrf_url,
      url_role = "primary",
      tracker_match_method = .data$match_method
    )

  extra_tbl <- manifest |>
    dplyr::transmute(
      ccn = .data$ccn,
      mrf_url = parse_extra_mrf_urls(.data$extra_mrf_urls),
      url_role = "extra",
      tracker_match_method = .data$match_method
    ) |>
    tidyr::unnest_longer("mrf_url", keep_empty = FALSE) |>
    dplyr::mutate(mrf_url = base::as.character(.data$mrf_url))

  dplyr::bind_rows(primary_tbl, extra_tbl) |>
    dplyr::mutate(url_key = normalize_url_key(.data$mrf_url)) |>
    dplyr::filter(!base::is.na(.data$ccn), !base::is.na(.data$url_key)) |>
    dplyr::distinct(.data$ccn, .data$url_key, .keep_all = TRUE)
}
