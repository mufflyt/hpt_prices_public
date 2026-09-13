#' Download hospital machine-readable files (MRFs)
#'
#' MRFs range from a few MB to tens of GB, so downloads are resumable: bytes
#' land in `<file>.part` (curl resumes from its current size on the next
#' attempt) and the file is renamed to its final name only once the transfer
#' completes. `.zip` and `.gz` payloads are unpacked, and the format (csv or
#' json) is sniffed from the content rather than trusted from the URL.
#' A failed URL never throws: the error is recorded in the returned row.
#'
#' Pattern follows private_equity R/fetch_mup_phy_prov_svc.R
#' (curl::multi_download with resume, a minimum-size check, a manifest).

#' Empty download result, used for the state file and zero-row returns
mrf_download_template <- function() {
  tibble::tibble(
    url = base::character(),
    local_path = base::character(),
    format = base::character(),
    bytes = base::double(),
    sha256 = base::character(),
    http_status = base::integer(),
    retrieved_at = base::character(),
    error = base::character(),
    note = base::character()
  )
}

mrf_download_row <- function(url,
                             local_path = NA_character_,
                             format = NA_character_,
                             http_status = NA_integer_,
                             retrieved_at = utc_timestamp(),
                             error = NA_character_,
                             note = NA_character_) {
  has_file <- !base::is.na(local_path) && base::file.exists(local_path)

  tibble::tibble(
    url = url,
    local_path = local_path,
    format = format,
    bytes = if (has_file) base::as.double(base::file.size(local_path)) else NA_real_,
    sha256 = if (has_file) mrf_sha256(local_path) else NA_character_,
    http_status = base::as.integer(http_status),
    retrieved_at = retrieved_at,
    error = error,
    note = note
  )
}

#' Does a curl error message describe a TLS/certificate failure?
is_tls_error <- function(error_text) {
  !base::is.na(error_text) &
    stringr::str_detect(error_text, stringr::regex("ssl|tls|certificate", ignore_case = TRUE))
}

#' sha256 of a file as a plain string (sha256_file() returns an openssl "hash")
mrf_sha256 <- function(path) {
  base::as.character(base::unclass(sha256_file(path)))
}

#' Extension implied by an MRF URL path (".csv", ".json", ".csv.gz", ".zip", ...)
mrf_url_extension <- function(url) {
  path <- stringr::str_to_lower(stringr::str_replace(url, "[?#].*$", ""))
  ext <- stringr::str_extract(path, "(\\.(csv|json|txt))?\\.(csv|json|zip|gz|txt)$")
  dplyr::if_else(base::is.na(ext), "", ext)
}

#' A short, collision-safe local file stem for a URL
#'
#' Many hospitals name every file `standardcharges.csv`, so the stem leads
#' with a hash of the full URL and keeps a readable slice of the basename.
mrf_file_stem <- function(url) {
  path <- stringr::str_replace(url, "[?#].*$", "")
  readable <- base::basename(path) |>
    utils::URLdecode() |>
    stringr::str_replace("(\\.(csv|json|txt))?\\.(csv|json|zip|gz|txt)$", "") |>
    stringr::str_replace_all("[^A-Za-z0-9._-]+", "_") |>
    stringr::str_sub(1L, 60L)

  url_hash <- stringr::str_sub(base::as.character(openssl::md5(url)), 1L, 12L)
  dplyr::if_else(base::nzchar(readable), base::paste0(url_hash, "_", readable), url_hash)
}

#' Percent-encode characters curl rejects (MRF URLs often contain spaces)
mrf_encode_url <- function(url) {
  url |>
    stringr::str_trim() |>
    stringr::str_replace_all(" ", "%20") |>
    stringr::str_replace_all("\\[", "%5B") |>
    stringr::str_replace_all("\\]", "%5D")
}

#' Classify the first bytes of a file
#'
#' @param bytes Raw vector (the first few KB of a file or response).
#' @return List with `compression` ("gzip", "zip", "none") and `format`
#'   ("json", "csv", "html", "empty"). The format is only meaningful when
#'   `compression` is "none".
sniff_mrf_bytes <- function(bytes) {
  if (base::length(bytes) == 0L) {
    return(base::list(compression = "none", format = "empty"))
  }

  if (base::length(bytes) >= 2L && bytes[[1]] == base::as.raw(0x1f) && bytes[[2]] == base::as.raw(0x8b)) {
    return(base::list(compression = "gzip", format = NA_character_))
  }

  if (base::length(bytes) >= 4L && base::identical(bytes[1:4], base::as.raw(base::c(0x50, 0x4b, 0x03, 0x04)))) {
    return(base::list(compression = "zip", format = NA_character_))
  }

  if (base::length(bytes) >= 3L && base::identical(bytes[1:3], base::as.raw(base::c(0xef, 0xbb, 0xbf)))) {
    bytes <- bytes[-(1:3)]
  }

  # skip whitespace, NUL (UTF-16), and the UTF-16 BOM bytes
  ignorable <- base::as.raw(base::c(0x00, 0x09, 0x0a, 0x0d, 0x20, 0xfe, 0xff))
  significant <- bytes[!bytes %in% ignorable]

  if (base::length(significant) == 0L) {
    return(base::list(compression = "none", format = "empty"))
  }

  first_char <- base::rawToChar(significant[1])

  format <- dplyr::case_when(
    first_char %in% base::c("{", "[") ~ "json",
    first_char == "<" ~ "html",
    TRUE ~ "csv"
  )

  base::list(compression = "none", format = format)
}

sniff_mrf_file <- function(path, n_bytes = 4096L) {
  sniff_mrf_bytes(base::readBin(path, what = "raw", n = n_bytes))
}

#' Unpack a .gz or .zip download into plain csv/json files
#'
#' @return Character vector of unpacked file paths (not yet renamed).
unpack_mrf_archive <- function(path, compression, work_dir) {
  base::dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)

  if (compression == "gzip") {
    out_path <- base::file.path(work_dir, "unpacked")
    status <- base::system2("gzip", base::c("-dc", base::shQuote(path)), stdout = out_path, stderr = FALSE)

    if (!base::identical(base::as.integer(status), 0L)) {
      base::stop("gzip could not decompress ", path)
    }

    return(out_path)
  }

  entries <- utils::unzip(path, list = TRUE)
  entries <- entries[!stringr::str_detect(entries$Name, "/$") & entries$Length > 0, , drop = FALSE]
  entries <- entries[!stringr::str_detect(entries$Name, "(^|/)(__MACOSX|\\._)"), , drop = FALSE]
  data_entries <- entries[stringr::str_detect(stringr::str_to_lower(entries$Name), "\\.(csv|json|txt)$"), , drop = FALSE]

  if (base::nrow(data_entries) == 0L) {
    data_entries <- entries
  }

  if (base::nrow(data_entries) == 0L) {
    base::stop("Zip archive has no files: ", path)
  }

  utils::unzip(path, files = data_entries$Name, exdir = work_dir, junkpaths = FALSE)
  base::file.path(work_dir, data_entries$Name)
}

#' Existing finished files for a stem (lets a re-run skip completed downloads)
mrf_existing_outputs <- function(dest_dir, file_stem) {
  candidates <- base::list.files(dest_dir, full.names = TRUE)
  candidate_names <- base::basename(candidates)
  is_output <- stringr::str_starts(candidate_names, stringr::fixed(file_stem)) &
    stringr::str_detect(
      stringr::str_sub(candidate_names, base::nchar(file_stem) + 1L),
      "^(__[^/]*)?\\.(csv|json)$"
    )

  candidates[is_output]
}

#' Fetch one URL into a .part file, resuming and retrying transient failures
#'
#' @return List with `ok`, `http_status`, `error`.
mrf_fetch_part <- function(url, part_path, max_tries = 3L) {
  last <- base::list(ok = FALSE, http_status = NA_integer_, error = "not attempted")

  for (attempt in base::seq_len(max_tries)) {
    result <- base::tryCatch(
      curl::multi_download(
        urls = mrf_encode_url(url),
        destfiles = part_path,
        resume = TRUE,
        progress = FALSE,
        useragent = hpt_user_agent(),
        followlocation = TRUE,
        failonerror = TRUE,
        connecttimeout = 60,
        low_speed_limit = 1024,
        low_speed_time = 300
      ),
      error = function(e) {
        tibble::tibble(
          success = FALSE, status_code = NA_real_, resumefrom = 0,
          error = base::conditionMessage(e), headers = base::list(base::character())
        )
      }
    )

    status <- base::as.integer(result$status_code[[1]])
    resumed_from <- result$resumefrom[[1]]
    error_text <- result$error[[1]]
    last <- base::list(ok = base::isTRUE(result$success[[1]]), http_status = status, error = error_text)

    # file:// URLs report status 0; http(s) must be 2xx
    if (last$ok && (base::is.na(status) || status == 0L || (status >= 200L && status < 300L))) {
      last$error <- NA_character_
      return(last)
    }

    # Asked to resume a file that is already complete
    if (!base::is.na(status) && status == 416L && resumed_from > 0) {
      content_range <- result$headers[[1]][stringr::str_detect(result$headers[[1]], stringr::regex("^content-range:", ignore_case = TRUE))]
      total_size <- base::suppressWarnings(base::as.numeric(stringr::str_match(content_range, "/([0-9]+)")[, 2]))

      if (base::length(total_size) > 0L && !base::is.na(total_size[[1]]) && total_size[[1]] == resumed_from) {
        return(base::list(ok = TRUE, http_status = 206L, error = NA_character_))
      }

      base::unlink(part_path)
      next
    }

    # Server refuses byte ranges: start over from zero
    if (resumed_from > 0 && !base::is.na(error_text) &&
        stringr::str_detect(error_text, stringr::regex("range|resume", ignore_case = TRUE))) {
      base::message("  server will not resume; restarting ", url)
      base::unlink(part_path)
      next
    }

    # a server that omits its intermediate certificate fails the same way every time
    permanent <- (!base::is.na(status) && status >= 400L && status < 500L && !status %in% base::c(408L, 429L)) ||
      base::isTRUE(is_tls_error(error_text))

    if (permanent || attempt == max_tries) {
      break
    }

    base::Sys.sleep(base::min(60, 2^attempt))
  }

  last$ok <- FALSE
  last
}

#' Download one MRF
#'
#' @param url MRF URL (http, https, or file://).
#' @param dest_dir Directory for finished files (default `<HPT_DATA_DIR>/mrf_raw`).
#' @param file_stem Base name for the local file(s); defaults to a hash of the
#'   URL plus a readable slice of its basename.
#' @param min_bytes Smaller downloads are treated as failures (error pages).
#' @param keep_archive Keep the original .zip/.gz after unpacking.
#' @param overwrite Re-download even when finished files for `file_stem` exist.
#' @param http_fallback When an https URL fails TLS verification (typically a
#'   server that omits its intermediate certificate), retry once over plain
#'   http. Verification is never disabled; a file fetched this way carries a
#'   `note`. 45 CFR 180.50(d)(3)(iv) requires MRFs to be downloadable by
#'   automated means.
#' @return Tibble with one row per data file (a zip can hold several):
#'   url, local_path, format, bytes, sha256, http_status, retrieved_at,
#'   error (TLS failures start with "tls_fail: "), note.
download_mrf <- function(url,
                         dest_dir = hpt_path("mrf_raw"),
                         file_stem = NULL,
                         min_bytes = 512,
                         keep_archive = FALSE,
                         overwrite = FALSE,
                         max_tries = 3L,
                         http_fallback = TRUE) {
  file_stem <- file_stem %||% mrf_file_stem(url)
  base::dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  existing <- mrf_existing_outputs(dest_dir, file_stem)

  if (!overwrite && base::length(existing) > 0L) {
    base::message("Already downloaded: ", base::paste(base::basename(existing), collapse = ", "))
    rows <- base::lapply(existing, function(path) {
      mrf_download_row(
        url,
        local_path = path,
        format = stringr::str_extract(path, "(csv|json)$"),
        retrieved_at = base::format(base::file.mtime(path), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      )
    })
    return(dplyr::bind_rows(rows))
  }

  base::tryCatch(
    mrf_download_one(url, dest_dir, file_stem, min_bytes, keep_archive, max_tries, http_fallback),
    error = function(e) {
      mrf_download_row(url, error = base::conditionMessage(e))
    }
  )
}

mrf_download_one <- function(url, dest_dir, file_stem, min_bytes, keep_archive, max_tries, http_fallback = TRUE) {
  download_path <- base::file.path(dest_dir, base::paste0(file_stem, mrf_url_extension(url), ".download"))
  part_path <- base::paste0(download_path, ".part")

  base::message("Downloading MRF: ", url)
  started <- base::Sys.time()
  fetched <- mrf_fetch_part(url, part_path, max_tries = max_tries)
  note <- NA_character_
  tls_failed <- !fetched$ok && base::isTRUE(is_tls_error(fetched$error))

  if (tls_failed && http_fallback && stringr::str_detect(url, stringr::regex("^https://", ignore_case = TRUE))) {
    http_url <- stringr::str_replace(url, stringr::regex("^https://", ignore_case = TRUE), "http://")
    base::message("  TLS verification failed; retrying once over ", http_url)
    base::unlink(part_path)
    fetched_http <- mrf_fetch_part(http_url, part_path, max_tries = 1L)

    if (fetched_http$ok) {
      note <- base::paste0("tls_fail over https (", stringr::str_squish(fetched$error), "); fetched over http://")
      fetched <- fetched_http
    }
  }

  retrieved_at <- utc_timestamp()

  if (!fetched$ok) {
    # keep a partial body for the next resume; nothing useful after a 4xx or TLS failure
    if (tls_failed || (!base::is.na(fetched$http_status) && fetched$http_status >= 400L && fetched$http_status < 500L)) {
      base::unlink(part_path)
    }

    error_text <- stringr::str_squish(dplyr::coalesce(fetched$error, "download failed"))
    if (tls_failed) {
      error_text <- base::paste0("tls_fail: ", error_text, if (http_fallback) " (http:// retry also failed)" else "")
    }

    return(mrf_download_row(url, http_status = fetched$http_status, retrieved_at = retrieved_at, error = error_text))
  }

  part_size <- base::file.size(part_path)

  if (base::is.na(part_size) || part_size < min_bytes) {
    base::unlink(part_path)
    return(mrf_download_row(url, http_status = fetched$http_status, retrieved_at = retrieved_at, note = note,
                            error = base::paste0("download smaller than ", min_bytes, " bytes (", part_size %||% 0, ")")))
  }

  base::file.rename(part_path, download_path)
  base::message(
    "  ", scales::comma(part_size), " bytes in ",
    base::round(base::as.numeric(base::difftime(base::Sys.time(), started, units = "secs")), 1), " s"
  )

  sniffed <- sniff_mrf_file(download_path)

  if (sniffed$compression == "none") {
    unpacked <- download_path
  } else {
    work_dir <- base::file.path(dest_dir, base::paste0(file_stem, ".unpacking"))
    base::on.exit(base::unlink(work_dir, recursive = TRUE), add = TRUE)
    unpacked <- unpack_mrf_archive(download_path, sniffed$compression, work_dir)
  }

  rows <- base::lapply(base::seq_along(unpacked), function(i) {
    path <- unpacked[[i]]
    format <- sniff_mrf_file(path)$format

    if (!format %in% base::c("csv", "json")) {
      return(mrf_download_row(url, http_status = fetched$http_status, retrieved_at = retrieved_at, note = note,
                              error = base::paste0("content is ", format, ", not an MRF")))
    }

    suffix <- if (base::length(unpacked) == 1L) {
      ""
    } else {
      entry <- base::basename(path) |>
        stringr::str_replace("\\.[A-Za-z0-9]+$", "") |>
        stringr::str_replace_all("[^A-Za-z0-9._-]+", "_") |>
        stringr::str_sub(1L, 60L)
      base::paste0("__", i, "_", entry)
    }

    final_path <- base::file.path(dest_dir, base::paste0(file_stem, suffix, ".", format))
    moved <- base::file.rename(path, final_path)

    if (!moved) {
      moved <- base::file.copy(path, final_path, overwrite = TRUE)
    }

    if (!moved) {
      return(mrf_download_row(url, http_status = fetched$http_status, retrieved_at = retrieved_at, note = note,
                              error = base::paste0("could not move ", path, " to ", final_path)))
    }

    mrf_download_row(url, local_path = final_path, format = format,
                     http_status = fetched$http_status, retrieved_at = retrieved_at, note = note)
  })

  if (sniffed$compression != "none" && !keep_archive) {
    base::unlink(download_path)
  }

  if (sniffed$compression == "none" && base::file.exists(download_path)) {
    # non-MRF content (an HTML error page, an empty body) is not kept
    base::unlink(download_path)
  }

  dplyr::bind_rows(rows)
}

#' Download many MRFs, resumable against a state CSV
#'
#' URLs already recorded in `state_path` without an error are skipped, so an
#' interrupted batch picks up where it stopped; failed URLs are retried on
#' the next run unless `retry_failed = FALSE`. The state file is rewritten
#' atomically after every URL.
#'
#' @param urls Character vector of MRF URLs.
#' @param dest_dir,min_bytes,keep_archive,http_fallback Passed to [download_mrf()].
#' @param state_path CSV recording one row per downloaded file.
#' @param file_stems Optional stems, parallel to `urls`.
#' @return The state rows for `urls`.
download_mrf_batch <- function(urls,
                               dest_dir = hpt_path("mrf_raw"),
                               state_path = hpt_path("state", "mrf_download_state.csv"),
                               file_stems = NULL,
                               min_bytes = 512,
                               keep_archive = FALSE,
                               retry_failed = TRUE,
                               http_fallback = TRUE) {
  file_stems <- file_stems %||% mrf_file_stem(urls)
  keep <- !base::is.na(urls) & base::nzchar(urls) & !base::duplicated(urls)
  urls <- urls[keep]
  file_stems <- file_stems[keep]
  base::dir.create(base::dirname(state_path), recursive = TRUE, showWarnings = FALSE)

  state <- read_mrf_download_state(state_path)
  recorded <- state |>
    dplyr::group_by(.data$url) |>
    dplyr::summarise(done = base::all(base::is.na(.data$error)), .groups = "drop")
  skip_urls <- if (retry_failed) recorded$url[recorded$done] else recorded$url
  todo <- base::which(!urls %in% skip_urls)

  base::message(
    "MRF batch: ", scales::comma(base::length(urls)), " URLs, ",
    scales::comma(base::length(urls) - base::length(todo)), " already recorded, ",
    scales::comma(base::length(todo)), " to download."
  )

  for (i in todo) {
    rows <- download_mrf(urls[[i]], dest_dir = dest_dir, file_stem = file_stems[[i]],
                         min_bytes = min_bytes, keep_archive = keep_archive, http_fallback = http_fallback)
    state <- dplyr::bind_rows(state |> dplyr::filter(.data$url != urls[[i]]), rows)
    write_csv_atomic(state, state_path)
  }

  state |> dplyr::filter(.data$url %in% urls)
}

read_mrf_download_state <- function(state_path) {
  if (!base::file.exists(state_path)) {
    return(mrf_download_template())
  }

  read_csv_chr(state_path) |>
    dplyr::mutate(
      bytes = base::as.double(.data$bytes),
      http_status = base::as.integer(.data$http_status)
    )
}
