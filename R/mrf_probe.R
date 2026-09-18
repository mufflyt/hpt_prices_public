#' Probe MRF URLs without downloading them
#'
#' A HEAD request (redirects followed) gives status, size, type, and change
#' markers; a Range request for the first ~64 KB gives the declared CMS
#' template version, hospital name, locations, license, type 2 NPIs, and
#' format. Gzip and zip payloads are inflated from the prefix alone. Servers
#' that ignore Range are read as a stream and closed after the prefix, so a
#' probe never pulls a whole multi-GB file.

#' First bytes of a local file or URL
#'
#' Generalized from emb_colonoscopy's read_hpt_prefix(): reads bytes rather
#' than lines (so gzip/zip prefixes can be inflated), sends a Range header,
#' and returns the response status and headers along with the bytes.
#'
#' @return List: bytes (raw), http_status, headers (named list), final_url,
#'   error.
# ported from emb_colonoscopy R/hpt_prices.R @ 471e067
read_mrf_prefix <- function(path_or_url, n_bytes = 65536L, timeout_s = 60) {
  if (base::file.exists(path_or_url)) {
    return(base::list(
      bytes = base::readBin(path_or_url, what = "raw", n = n_bytes),
      http_status = NA_integer_, headers = base::list(), final_url = path_or_url, error = NA_character_
    ))
  }

  base::tryCatch(
    {
      request_obj <- hpt_request(mrf_encode_url(path_or_url), timeout_s = timeout_s) |>
        httr2::req_headers(Range = base::paste0("bytes=0-", n_bytes - 1L))

      response <- httr2::req_perform_connection(request_obj)
      base::on.exit(base::close(response), add = TRUE)

      chunks <- base::list()
      received <- 0L

      while (received < n_bytes && !httr2::resp_stream_is_complete(response)) {
        chunk <- httr2::resp_stream_raw(response, kb = base::ceiling((n_bytes - received) / 1024))
        if (base::length(chunk) == 0L) {
          break
        }
        chunks[[base::length(chunks) + 1L]] <- chunk
        received <- received + base::length(chunk)
      }

      bytes <- base::do.call(base::c, chunks) %||% base::raw()

      base::list(
        bytes = utils::head(bytes, n_bytes),
        http_status = httr2::resp_status(response),
        headers = httr2::resp_headers(response),
        final_url = httr2::resp_url(response),
        error = NA_character_
      )
    },
    error = function(e) {
      base::list(bytes = base::raw(), http_status = NA_integer_, headers = base::list(),
                 final_url = path_or_url, error = base::conditionMessage(e))
    }
  )
}

#' Inflate a truncated gzip stream (as much as the bytes allow)
inflate_gzip_prefix <- function(bytes, n_bytes = 65536L) {
  connection <- base::gzcon(base::rawConnection(bytes))
  base::on.exit(base::close(connection), add = TRUE)
  base::tryCatch(base::readBin(connection, what = "raw", n = n_bytes), error = function(e) base::raw())
}

#' Inflate the first entry of a truncated zip archive
#'
#' A zip local-file header is followed by raw deflate data; wrapping that in
#' a minimal gzip header lets gzcon() inflate it without the rest of the
#' archive.
#'
#' @return List: bytes (inflated prefix), entry_name.
inflate_zip_prefix <- function(bytes, n_bytes = 65536L) {
  read_le <- function(offset, size) {
    base::sum(base::as.integer(bytes[offset + base::seq_len(size)]) * 256^(base::seq_len(size) - 1L))
  }

  if (base::length(bytes) < 30L) {
    return(base::list(bytes = base::raw(), entry_name = NA_character_))
  }

  method <- read_le(8L, 2L)
  name_length <- read_le(26L, 2L)
  extra_length <- read_le(28L, 2L)
  data_start <- 30L + name_length + extra_length
  entry_name <- base::rawToChar(bytes[30L + base::seq_len(name_length)])

  if (data_start >= base::length(bytes)) {
    return(base::list(bytes = base::raw(), entry_name = entry_name))
  }

  payload <- bytes[(data_start + 1L):base::length(bytes)]
  compressed_size <- read_le(18L, 4L)

  if (method == 0) {
    return(base::list(bytes = utils::head(payload, n_bytes), entry_name = entry_name))
  }

  if (method != 8) {
    return(base::list(bytes = base::raw(), entry_name = entry_name))
  }

  gzip_header <- base::as.raw(base::c(0x1f, 0x8b, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff))

  # the whole entry is in the prefix: end it with the CRC-32 and size from
  # the zip header, which is exactly the gzip trailer
  if (compressed_size > 0 && compressed_size <= base::length(payload)) {
    payload <- base::c(payload[base::seq_len(compressed_size)], bytes[14L + base::seq_len(4L)], bytes[22L + base::seq_len(4L)])
  }

  base::list(bytes = inflate_gzip_prefix(base::c(gzip_header, payload), n_bytes), entry_name = entry_name)
}

#' Metadata from the first bytes of an MRF (plain, gzip, or zip)
#'
#' @return List: compression, format, layout (csv only), and the metadata
#'   fields of [read_csv_mrf_metadata()].
mrf_metadata_from_prefix <- function(bytes) {
  empty_meta <- base::list(
    hospital_name = NA_character_, last_updated_on = NA_character_, version = NA_character_,
    location_name = NA_character_, hospital_address = NA_character_,
    license_number = NA_character_, license_state = NA_character_, type_2_npi = NA_character_
  )
  sniffed <- sniff_mrf_bytes(bytes)
  compression <- sniffed$compression

  if (compression == "gzip") {
    bytes <- inflate_gzip_prefix(bytes)
  } else if (compression == "zip") {
    bytes <- inflate_zip_prefix(bytes)$bytes
  }

  format <- sniff_mrf_bytes(bytes)$format
  out <- base::c(base::list(compression = compression, format = format, layout = NA_character_), empty_meta)

  if (!format %in% base::c("csv", "json")) {
    return(out)
  }

  text <- mrf_decode_bytes(bytes)

  if (format == "json") {
    meta <- parse_json_mrf_prefix(mrf_clean_text_lines(text))
    out[base::names(empty_meta)] <- meta[base::names(empty_meta)]
    return(out)
  }

  # drop the last (probably truncated) line of the prefix
  lines <- stringr::str_split(text, "\n")[[1]]
  lines <- mrf_clean_text_lines(utils::head(lines, base::max(1L, base::length(lines) - 1L)))
  meta <- base::tryCatch(parse_csv_mrf_header_lines(utils::head(lines, 20L)), error = function(e) NULL)

  if (!base::is.null(meta)) {
    out[base::names(empty_meta)] <- meta[base::names(empty_meta)]
    out$layout <- csv_mrf_layout(stringr::str_to_lower(normalize_hpt_names(meta$charge_columns)))
  }

  out
}

mrf_header_value <- function(headers, name) {
  if (base::length(headers) == 0L) {
    return(NA_character_)
  }

  hit <- base::which(stringr::str_to_lower(base::names(headers)) == name)
  if (base::length(hit) == 0L) NA_character_ else base::as.character(headers[[hit[[1]]]])
}
