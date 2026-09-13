`%||%` <- function(x, y) {
  if (base::is.null(x) || base::length(x) == 0L) {
    return(y)
  }

  x
}

# ported from emb_colonoscopy R/meps_download.R @ 471e067
sha256_file <- function(path) {
  if (!base::file.exists(path)) {
    base::stop("Cannot hash missing file: ", path)
  }

  connection <- base::file(path, open = "rb")
  base::on.exit(base::close(connection), add = TRUE)

  # as.character() turns openssl's raw digest into one hex string but keeps
  # the "hash" class, which bind_rows() can't combine with plain character
  # columns read back from state files; as.vector(unclass()) drops it.
  hex <- base::as.character(openssl::sha256(connection))
  base::as.vector(base::unclass(hex))
}

utc_timestamp <- function() {
  base::format(base::Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

#' Write a table atomically: to a temp file first, then rename.
#'
#' An interrupted write can never leave a partial file that a later resumed
#' run would mistake for finished output.
write_csv_atomic <- function(tbl, path) {
  temp_path <- base::paste0(path, ".tmp")
  readr::write_csv(tbl, temp_path, na = "")
  base::file.rename(temp_path, path)
  base::invisible(path)
}

write_parquet_atomic <- function(tbl, path) {
  temp_path <- base::paste0(path, ".tmp")
  arrow::write_parquet(tbl, temp_path)
  base::file.rename(temp_path, path)
  base::invisible(path)
}

#' Repair text that is not valid UTF-8 by reading it as Windows-1252
#'
#' Public files (AHRQ CHSP, CMS, hospital MRFs) mix in Windows-1252 bytes
#' such as 0x96 (en dash). DuckDB rejects those in Parquet, so repair every
#' character column before handing a table to DuckDB.
repair_utf8 <- function(tbl) {
  for (column in base::names(tbl)) {
    x <- tbl[[column]]

    if (base::is.character(x)) {
      bad <- !base::is.na(x) & !base::validUTF8(x)

      if (base::any(bad)) {
        x[bad] <- base::iconv(x[bad], from = "WINDOWS-1252", to = "UTF-8", sub = "?")
        tbl[[column]] <- x
      }
    }
  }

  tbl
}

read_csv_chr <- function(path) {
  readr::read_csv(
    path,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character()),
    na = base::c("", "NA")
  )
}

#' Normalize a URL for exact matching across sources
#'
#' Two datasets that record "the same" MRF URL often differ in scheme,
#' host case, a trailing slash, or percent-encoded spaces. The path is kept
#' case-sensitive because many servers treat it that way. The query string
#' is dropped when the path names a data file (".csv", ".json", ".zip",
#' ".gz", ".xlsx", ".txt"; a query there is a cache-buster) or when it is a
#' signed/expiring-URL query (S3 X-Amz-*, Azure SAS sig/se/sv,
#' Signature/Expires/token). It is KEPT for script endpoints, where it
#' identifies the file: 128 different hospitalpricedisclosure.com/
#' download.aspx?pi=... files would otherwise collapse to one key.
normalize_url_key <- function(url) {
  url <- stringr::str_trim(url)
  url <- stringr::str_replace(url, stringr::regex("^https?://", ignore_case = TRUE), "")
  url <- stringr::str_replace(url, "#.*$", "")
  signed <- stringr::str_detect(url, stringr::regex("[?&](x-amz-[a-z-]+|sig|se|sv|sp|st|spr|signature|expires|token|key-pair-id|policy)=", ignore_case = TRUE))
  data_file_path <- stringr::str_detect(url, stringr::regex("\\.(csv|json|zip|gz|xlsx?|txt)/?(\\?|$)", ignore_case = TRUE))
  url <- dplyr::if_else(signed | data_file_path, stringr::str_replace(url, "\\?.*$", ""), url)
  url <- stringr::str_replace_all(url, "%20", " ")
  url <- stringr::str_replace(url, "/+(\\?|$)", "\\1")

  host <- stringr::str_to_lower(stringr::str_extract(url, "^[^/]+"))
  path <- stringr::str_replace(url, "^[^/]+", "")
  host <- stringr::str_replace(host, "^www\\.", "")

  dplyr::if_else(base::is.na(url) | !base::nzchar(url), NA_character_, base::paste0(host, path))
}
