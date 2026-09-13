#' Discover machine-readable price files through cms-hpt.txt
#'
#' 45 CFR 180.50(d)(6) requires a `cms-hpt.txt` file at the root of the
#' public website that hosts a hospital's MRF, "without regard to page
#' structure". That site may be the hospital's, its health system's, or a
#' vendor's, and redirects from /cms-hpt.txt are allowed (we follow them and
#' record the final URL). Each entry has five `key: value` lines
#' (location-name, source-page-url, mrf-url, contact-name, contact-email) and
#' entries are separated by a blank line. The file carries no CCN, NPI, or
#' EIN, so linking entries to hospitals happens downstream.
#'
#' Real files break the format in predictable ways (CRLF, BOM, key case,
#' missing blank lines, HTML "soft 404" pages served with 200 OK), so the
#' parser and the status classes below are deliberately forgiving.

hpt_txt_keys <- function() {
  base::c("location-name", "source-page-url", "mrf-url", "contact-name", "contact-email")
}

#' Statuses that are final for `max_age_days`; the rest are retried next run
hpt_txt_terminal_statuses <- function() {
  base::c("ok", "soft_404_html", "not_found", "blocked", "dns_fail", "tls_fail", "empty")
}

#' Transport failures worth a second try over plain http://
hpt_transport_failures <- function() {
  base::c("tls_fail", "network_error", "timeout")
}

# ported from emb_colonoscopy R/hpt_hospital_discovery.R @ 471e067
# (vectorized; also lowercases and drops userinfo, port, and trailing dot)
normalize_hpt_domain <- function(domain) {
  normalized <- domain |>
    base::as.character() |>
    stringr::str_trim() |>
    stringr::str_replace(stringr::regex("^[a-z][a-z0-9+.-]*://", ignore_case = TRUE), "") |>
    stringr::str_replace("[/?#\\\\].*$", "") |>
    stringr::str_replace("^.*@", "") |>
    stringr::str_replace(":[0-9]*$", "") |>
    stringr::str_replace("\\.+$", "") |>
    stringr::str_to_lower()

  valid <- stringr::str_detect(normalized, "^[a-z0-9]([a-z0-9-]*[a-z0-9])?(\\.[a-z0-9]([a-z0-9-]*[a-z0-9])?)+$")
  dplyr::if_else(!base::is.na(valid) & valid, normalized, NA_character_)
}

# ported from emb_colonoscopy R/hpt_hospital_discovery.R @ 471e067
# (vectorized; returns NA unless the value is an http(s) URL)
extract_url_hostname <- function(url) {
  url <- stringr::str_trim(base::as.character(url))
  has_scheme <- stringr::str_detect(url, stringr::regex("^https?://", ignore_case = TRUE))

  dplyr::if_else(!base::is.na(has_scheme) & has_scheme, normalize_hpt_domain(url), NA_character_)
}

#' Decode a cms-hpt.txt body (raw bytes or text) to one UTF-8 string
#'
#' Handles UTF-8 and UTF-16 byte order marks, stray NUL bytes (UTF-16 saved
#' without a BOM), and Latin-1 files.
decode_hpt_body <- function(body) {
  if (base::is.null(body) || base::length(body) == 0L) {
    return("")
  }

  if (base::is.character(body)) {
    text <- base::paste(body[!base::is.na(body)], collapse = "\n")
  } else if (base::length(body) >= 2L && body[[1]] == base::as.raw(0xff) && body[[2]] == base::as.raw(0xfe)) {
    text <- base::iconv(base::list(body[-(1:2)]), from = "UTF-16LE", to = "UTF-8")
  } else if (base::length(body) >= 2L && body[[1]] == base::as.raw(0xfe) && body[[2]] == base::as.raw(0xff)) {
    text <- base::iconv(base::list(body[-(1:2)]), from = "UTF-16BE", to = "UTF-8")
  } else {
    text <- base::rawToChar(body[body != base::as.raw(0)])
  }

  if (base::is.na(text)) {
    return("")
  }

  if (!base::validUTF8(text)) {
    text <- base::iconv(text, from = "latin1", to = "UTF-8")
  }

  base::Encoding(text) <- "UTF-8"
  stringr::str_remove(text, "^\ufeff")
}

#' Map loosely written key names ("Location Name", "MRF_URL") to CMS keys
hpt_canonical_key <- function(key) {
  squashed <- stringr::str_to_lower(stringr::str_remove_all(key, "[\\s_-]"))

  dplyr::case_when(
    squashed == "locationname" ~ "location-name",
    squashed == "sourcepageurl" ~ "source-page-url",
    squashed == "mrfurl" ~ "mrf-url",
    squashed == "contactname" ~ "contact-name",
    squashed == "contactemail" ~ "contact-email",
    TRUE ~ NA_character_
  )
}

empty_hpt_locations <- function() {
  tibble::tibble(
    location_name = base::character(),
    source_page_url = base::character(),
    mrf_url = base::character(),
    contact_name = base::character(),
    contact_email = base::character()
  )
}

#' Parse cms-hpt.txt content into one row per location entry
#'
#' A new entry starts when a key repeats within the current entry, or when
#' `location-name` follows a blank line. So a missing blank line between
#' entries still splits correctly, and blank lines inside one entry do not.
#' A non-key line directly under a key line continues that key's value
#' (a wrapped URL). When an entry lists a second `mrf-url` with no blank
#' line and no new `location-name`, the extra entry inherits the missing
#' fields from its neighbor.
#'
#' @param hpt_text Character (one string or lines) or raw bytes.
# ported from emb_colonoscopy R/hpt_hospital_discovery.R @ 471e067
# (rewritten for CRLF, BOM, key case, missing separators, wrapped values)
parse_cms_hpt_text <- function(hpt_text) {
  text <- decode_hpt_body(hpt_text)
  lines <- stringr::str_split(text, "\r\n|\r|\n")[[1]]
  lines <- lines |>
    stringr::str_remove_all("[\ufeff\u200b\u200c\u200d\u2060]") |>
    stringr::str_replace_all("[\u00a0\t]", " ")

  key_pattern <- stringr::regex(
    "^\\s*[\"']?\\s*(location[\\s_-]*name|source[\\s_-]*page[\\s_-]*url|mrf[\\s_-]*url|contact[\\s_-]*name|contact[\\s_-]*e[\\s_-]*mail)\\s*[\"']?\\s*:\\s*(.*?)\\s*$",
    ignore_case = TRUE
  )
  matched <- stringr::str_match(lines, key_pattern)
  line_key <- hpt_canonical_key(matched[, 2])
  line_value <- matched[, 3]
  is_blank <- !base::nzchar(stringr::str_trim(lines))

  n_lines <- base::length(lines)
  entry_id <- base::integer(n_lines)
  chained <- base::logical(0)
  current_id <- 0L
  seen_keys <- base::character(0)
  after_blank <- TRUE
  last_key <- NA_character_

  for (i in base::seq_len(n_lines)) {
    if (is_blank[[i]]) {
      after_blank <- TRUE
      last_key <- NA_character_
      next
    }

    key <- line_key[[i]]

    if (base::is.na(key)) {
      if (!base::is.na(last_key) && current_id > 0L) {
        line_key[[i]] <- last_key
        line_value[[i]] <- stringr::str_trim(lines[[i]])
        entry_id[[i]] <- current_id
      }
      next
    }

    starts_entry <- current_id == 0L ||
      key %in% seen_keys ||
      (after_blank && key == "location-name")

    if (starts_entry) {
      current_id <- current_id + 1L
      chained[[current_id]] <- current_id > 1L && !after_blank && key != "location-name"
      seen_keys <- base::character(0)
    }

    seen_keys <- base::c(seen_keys, key)
    entry_id[[i]] <- current_id
    last_key <- key
    after_blank <- FALSE
  }

  if (current_id == 0L) {
    return(empty_hpt_locations())
  }

  pairs_tbl <- tibble::tibble(entry_id = entry_id, key = line_key, value = line_value) |>
    dplyr::filter(.data$entry_id > 0L, !base::is.na(.data$key)) |>
    dplyr::mutate(
      value = stringr::str_trim(stringr::str_remove_all(.data$value, "^[\"'<]+|[\"'>]+$"))
    ) |>
    dplyr::group_by(.data$entry_id, .data$key) |>
    dplyr::summarise(
      value = base::paste(
        .data$value[base::nzchar(.data$value)],
        collapse = base::ifelse(stringr::str_ends(.data$key[[1]], "url"), "", " ")
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(value = dplyr::na_if(.data$value, ""))

  wide_tbl <- pairs_tbl |>
    tidyr::pivot_wider(names_from = "key", values_from = "value")

  for (key in hpt_txt_keys()) {
    if (!key %in% base::names(wide_tbl)) {
      wide_tbl[[key]] <- NA_character_
    }
  }

  wide_tbl |>
    dplyr::arrange(.data$entry_id) |>
    dplyr::mutate(chain_id = base::cumsum(!chained[.data$entry_id])) |>
    dplyr::group_by(.data$chain_id) |>
    tidyr::fill(
      "location-name", "source-page-url", "contact-name", "contact-email",
      .direction = "downup"
    ) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      location_name = .data$`location-name`,
      source_page_url = .data$`source-page-url`,
      mrf_url = .data$`mrf-url`,
      contact_name = .data$`contact-name`,
      contact_email = .data$`contact-email`
    ) |>
    dplyr::filter(!(base::is.na(.data$location_name) & base::is.na(.data$mrf_url)))
}

#' Is this body a cms-hpt.txt file rather than an HTML page?
#'
#' TRUE only when the body has an `mrf-url:` key (any case) and no HTML
#' markup. `content_type` is accepted for the record but never decides: a
#' text/html page without an mrf-url key already fails the key test, and a
#' real pointer file mislabeled text/html (common) should still pass.
looks_like_hpt_txt <- function(body, content_type = NA_character_) {
  if (base::is.raw(body)) {
    body <- decode_hpt_body(body)
  } else if (base::is.list(body)) {
    body <- base::vapply(body, decode_hpt_body, base::character(1))
  }
  body[base::is.na(body)] <- ""

  has_mrf_key <- stringr::str_detect(
    body,
    stringr::regex("(^|[\\r\\n])[\\s\"'\ufeff]*mrf[\\s_-]*url[\\s\"']*:", ignore_case = TRUE)
  )
  has_markup <- stringr::str_detect(body, stringr::regex("<html|<!doctype|<body|<head", ignore_case = TRUE))

  has_mrf_key & !has_markup
}

is_html_content_type <- function(content_type) {
  !base::is.na(content_type) &
    stringr::str_detect(content_type, stringr::regex("text/html|application/xhtml", ignore_case = TRUE))
}

#' Build the polite request for one cms-hpt.txt URL
#'
#' A real pointer file is a few KB (a vendor file listing hundreds of
#' hospitals is still well under 1 MB), so a size cap protects against a
#' server that answers /cms-hpt.txt with something huge.
hpt_txt_request <- function(url, timeout_s = 30) {
  hpt_request(url, timeout_s = timeout_s, max_tries = 2) |>
    httr2::req_options(maxfilesize = 10 * 1024^2)
}

#' Classify one cms-hpt.txt response or transport error
#'
#' @return List with status, http_status, final_url, body (raw), bytes,
#'   n_locations, and error_message.
classify_hpt_txt_response <- function(resp_or_error) {
  if (!base::inherits(resp_or_error, "httr2_response")) {
    return(base::list(
      status = classify_http_outcome(resp_or_error),
      http_status = NA_integer_,
      final_url = NA_character_,
      body = base::raw(0),
      bytes = 0,
      n_locations = 0L,
      error_message = stringr::str_squish(base::conditionMessage(resp_or_error))
    ))
  }

  http_class <- classify_http_outcome(resp_or_error)
  body_raw <- base::tryCatch(httr2::resp_body_raw(resp_or_error), error = function(error_condition) base::raw(0))
  content_type <- base::tryCatch(httr2::resp_content_type(resp_or_error), error = function(error_condition) NA_character_)
  body_text <- decode_hpt_body(body_raw)
  n_locations <- 0L

  if (http_class != "ok") {
    status <- http_class
    error_message <- base::paste0("HTTP ", httr2::resp_status(resp_or_error))
  } else if (!base::nzchar(stringr::str_trim(body_text))) {
    status <- "empty"
    error_message <- "empty body"
  } else if (looks_like_hpt_txt(body_text, content_type)) {
    n_locations <- base::nrow(parse_cms_hpt_text(body_text))
    status <- if (n_locations > 0L) "ok" else "empty"
    error_message <- if (n_locations > 0L) NA_character_ else "mrf-url key present but no parsable entries"
  } else if (is_html_content_type(content_type) ||
    stringr::str_detect(body_text, stringr::regex("<html|<!doctype|<body|<head", ignore_case = TRUE))) {
    status <- "soft_404_html"
    error_message <- "HTML page instead of cms-hpt.txt"
  } else {
    status <- "empty"
    error_message <- "no mrf-url key"
  }

  base::list(
    status = status,
    http_status = httr2::resp_status(resp_or_error),
    final_url = httr2::resp_url(resp_or_error),
    body = body_raw,
    bytes = base::length(body_raw),
    n_locations = n_locations,
    error_message = error_message
  )
}

hpt_txt_cache_path <- function(cache_dir, domain) {
  base::file.path(cache_dir, base::paste0(stringr::str_replace_all(domain, "[^a-z0-9.-]", "_"), ".txt"))
}

write_raw_atomic <- function(bytes, path) {
  temp_path <- base::paste0(path, ".tmp")
  base::writeBin(bytes, temp_path)
  base::file.rename(temp_path, path)
  base::invisible(path)
}

empty_hpt_txt_results <- function() {
  tibble::tibble(
    domain = base::character(),
    attempted_url = base::character(),
    final_url = base::character(),
    http_status = base::integer(),
    status = base::character(),
    n_locations = base::integer(),
    bytes = base::numeric(),
    fetched_at = base::character(),
    error_message = base::character()
  )
}

perform_hpt_txt_round <- function(urls, timeout_s, max_active) {
  requests <- base::lapply(urls, hpt_txt_request, timeout_s = timeout_s)
  responses <- hpt_perform_parallel(requests, max_active = max_active)
  base::lapply(responses, classify_hpt_txt_response)
}

#' Fetch /cms-hpt.txt for many domains in parallel
#'
#' Tries https:// first and falls back to http:// only for TLS, timeout, or
#' connection failures. Redirects are followed and the final URL recorded.
#' Bodies of anything that is not HTML are cached to `cache_dir/<domain>.txt`
#' so parsing can be redone offline.
#'
#' @param domains Hostnames or URLs; normalized and deduplicated.
#' @param cache_dir Directory for raw TXT bodies.
#' @return One row per domain: domain, attempted_url, final_url,
#'   http_status, status, n_locations, bytes, fetched_at, error_message.
fetch_hpt_txt_batch <- function(domains,
                                cache_dir = hpt_path("txt_cache"),
                                timeout_s = 30,
                                max_active = NULL) {
  domains <- base::unique(normalize_hpt_domain(domains))
  domains <- domains[!base::is.na(domains)]

  if (base::length(domains) == 0L) {
    return(empty_hpt_txt_results())
  }

  if (!base::dir.exists(cache_dir)) {
    base::dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  base::message("Fetching cms-hpt.txt for ", scales::comma(base::length(domains)), " domains.")
  started_at <- base::Sys.time()

  attempted_urls <- base::paste0("https://", domains, "/cms-hpt.txt")
  results <- perform_hpt_txt_round(attempted_urls, timeout_s, max_active)

  retry_index <- base::which(base::vapply(results, function(result) result$status, base::character(1)) %in% hpt_transport_failures())

  if (base::length(retry_index) > 0L) {
    base::message("Retrying ", base::length(retry_index), " domains over http://.")
    http_urls <- base::paste0("http://", domains[retry_index], "/cms-hpt.txt")
    http_results <- perform_hpt_txt_round(http_urls, timeout_s, max_active)

    for (j in base::seq_along(retry_index)) {
      if (!http_results[[j]]$status %in% hpt_transport_failures()) {
        results[[retry_index[[j]]]] <- http_results[[j]]
        attempted_urls[[retry_index[[j]]]] <- http_urls[[j]]
      }
    }
  }

  fetched_at <- utc_timestamp()

  for (i in base::seq_along(domains)) {
    if (results[[i]]$status %in% base::c("ok", "empty") && results[[i]]$bytes > 0) {
      write_raw_atomic(results[[i]]$body, hpt_txt_cache_path(cache_dir, domains[[i]]))
    }
  }

  results_tbl <- tibble::tibble(
    domain = domains,
    attempted_url = attempted_urls,
    final_url = base::vapply(results, function(result) result$final_url, base::character(1)),
    http_status = base::vapply(results, function(result) base::as.integer(result$http_status), base::integer(1)),
    status = base::vapply(results, function(result) result$status, base::character(1)),
    n_locations = base::vapply(results, function(result) base::as.integer(result$n_locations), base::integer(1)),
    bytes = base::vapply(results, function(result) base::as.numeric(result$bytes), base::numeric(1)),
    fetched_at = fetched_at,
    error_message = base::vapply(results, function(result) result$error_message, base::character(1))
  )

  elapsed_s <- base::as.numeric(base::difftime(base::Sys.time(), started_at, units = "secs"))
  base::message(
    "Fetched ", base::nrow(results_tbl), " domains in ", base::round(elapsed_s, 1), " s (",
    base::round(base::nrow(results_tbl) / base::max(elapsed_s, 0.001), 2), " domains/s); ",
    base::sum(results_tbl$status == "ok"), " ok."
  )

  results_tbl
}

read_hpt_txt_state <- function(state_path) {
  if (!base::file.exists(state_path)) {
    return(empty_hpt_txt_results())
  }

  read_csv_chr(state_path) |>
    dplyr::mutate(
      http_status = base::as.integer(.data$http_status),
      n_locations = base::as.integer(.data$n_locations),
      bytes = base::as.numeric(.data$bytes)
    )
}

#' Replace every state row for the keys in `new_tbl`, keep the rest
upsert_state <- function(state_tbl, new_tbl, key = "domain") {
  state_tbl |>
    dplyr::filter(!.data[[key]] %in% new_tbl[[key]]) |>
    dplyr::bind_rows(new_tbl) |>
    dplyr::arrange(.data[[key]])
}

#' Domains in a state table whose last result is final and recent enough
fresh_state_keys <- function(state_tbl, terminal_statuses, max_age_days, key = "domain") {
  if (base::nrow(state_tbl) == 0L) {
    return(base::character(0))
  }

  fetched_time <- base::as.POSIXct(state_tbl$fetched_at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  age_days <- base::as.numeric(base::difftime(base::Sys.time(), fetched_time, units = "days"))
  is_fresh <- state_tbl$status %in% terminal_statuses & !base::is.na(age_days) & age_days <= max_age_days

  base::unique(state_tbl[[key]][is_fresh])
}

#' Resumable cms-hpt.txt crawl
#'
#' Skips domains whose state row has a terminal status fetched within
#' `max_age_days`; timeouts, network errors, and other HTTP errors are
#' retried on the next run. Results are upserted into `state_path` after
#' every chunk (atomic write), so an interrupted crawl loses at most one
#' chunk. With `prefer_apex`, a www. domain whose apex is also requested is
#' only fetched when the apex did not return a usable file.
#'
#' @return State rows for the requested domains.
crawl_hpt_txt <- function(domains,
                          state_path = hpt_path("txt_state.csv"),
                          cache_dir = hpt_path("txt_cache"),
                          max_age_days = 30,
                          chunk_size = 200L,
                          prefer_apex = TRUE,
                          timeout_s = 30) {
  domains <- base::unique(normalize_hpt_domain(domains))
  domains <- domains[!base::is.na(domains)]
  state_tbl <- read_hpt_txt_state(state_path)

  fresh <- fresh_state_keys(state_tbl, hpt_txt_terminal_statuses(), max_age_days)
  due <- base::setdiff(domains, fresh)
  base::message(
    "cms-hpt.txt crawl: ", scales::comma(base::length(domains)), " domains, ",
    scales::comma(base::length(domains) - base::length(due)), " fresh in state, ",
    scales::comma(base::length(due)), " to fetch."
  )

  is_www <- stringr::str_starts(due, "www\\.")
  deferred <- if (prefer_apex) due[is_www & stringr::str_remove(due, "^www\\.") %in% domains] else base::character(0)
  passes <- base::list(base::setdiff(due, deferred), deferred)

  for (pass_index in base::seq_along(passes)) {
    pass_domains <- passes[[pass_index]]

    if (pass_index == 2L && base::length(pass_domains) > 0L) {
      apex_ok <- state_tbl$domain[state_tbl$status == "ok"]
      pass_domains <- pass_domains[!stringr::str_remove(pass_domains, "^www\\.") %in% apex_ok]
    }

    chunks <- base::split(pass_domains, base::ceiling(base::seq_along(pass_domains) / chunk_size))

    for (chunk in chunks) {
      new_tbl <- fetch_hpt_txt_batch(chunk, cache_dir = cache_dir, timeout_s = timeout_s)
      state_tbl <- upsert_state(state_tbl, new_tbl)
      write_csv_atomic(state_tbl, state_path)
    }
  }

  state_tbl |>
    dplyr::filter(.data$domain %in% domains)
}

#' All parsed locations for state rows with status "ok", read from cache
#'
#' @return tibble(domain, final_url, location_name, source_page_url,
#'   mrf_url, contact_name, contact_email, fetched_at).
collect_hpt_txt_locations <- function(state_tbl, cache_dir = hpt_path("txt_cache")) {
  ok_tbl <- state_tbl |>
    dplyr::filter(.data$status == "ok")

  location_tbls <- base::lapply(base::seq_len(base::nrow(ok_tbl)), function(i) {
    cache_file <- hpt_txt_cache_path(cache_dir, ok_tbl$domain[[i]])

    if (!base::file.exists(cache_file)) {
      base::message("Missing cached cms-hpt.txt for ", ok_tbl$domain[[i]])
      return(NULL)
    }

    parse_cms_hpt_text(base::readBin(cache_file, what = "raw", n = base::file.size(cache_file))) |>
      dplyr::mutate(
        domain = ok_tbl$domain[[i]],
        final_url = ok_tbl$final_url[[i]],
        fetched_at = ok_tbl$fetched_at[[i]],
        .before = 1L
      )
  })

  dplyr::bind_rows(
    tibble::tibble(domain = base::character(), final_url = base::character()),
    empty_hpt_locations(),
    tibble::tibble(fetched_at = base::character()),
    location_tbls
  ) |>
    dplyr::relocate("fetched_at", .after = dplyr::last_col())
}

#' Site identity for deduplication: host without a leading "www."
hpt_site_key <- function(domain) {
  stringr::str_remove(normalize_hpt_domain(domain), "^www\\.")
}

#' Hosts named in parsed locations that have not been fetched yet
#'
#' A vendor or health-system cms-hpt.txt often points at MRFs on other
#' hosts; those hosts (and their www./apex/parent variants) may carry their
#' own cms-hpt.txt listing many more hospitals. A candidate counts as known
#' when its www-stripped form matches a known domain's.
snowball_hosts <- function(locations_tbl, known_domains) {
  urls <- base::c(locations_tbl$mrf_url, locations_tbl$source_page_url)
  hosts <- base::unique(extract_url_hostname(urls))
  hosts <- hosts[!base::is.na(hosts)]

  if (base::length(hosts) == 0L) {
    return(base::character(0))
  }

  candidates <- domain_variants(hosts)
  candidates <- candidates[!is_junk_hpt_host(candidates)]
  known_keys <- hpt_site_key(known_domains)

  base::sort(candidates[!hpt_site_key(candidates) %in% known_keys])
}

#' Crawl seeds, then repeatedly crawl hosts found in their cms-hpt.txt files
#'
#' @return List: `state` (one row per fetched domain with the round it was
#'   crawled in), `locations` (all parsed entries), and `snowball` (each new
#'   host with the round it was queued in and a domain that referenced it).
crawl_hpt_txt_snowball <- function(seed_domains,
                                   state_path = hpt_path("txt_state.csv"),
                                   cache_dir = hpt_path("txt_cache"),
                                   max_rounds = 3L,
                                   max_age_days = 30,
                                   timeout_s = 30) {
  queue <- base::unique(normalize_hpt_domain(seed_domains))
  queue <- queue[!base::is.na(queue)]
  known <- queue
  round_index <- 0L
  state_rows <- base::list()
  snowball_rows <- base::list()

  while (base::length(queue) > 0L) {
    base::message("Snowball round ", round_index, ": ", base::length(queue), " domains.")
    round_state <- crawl_hpt_txt(
      queue,
      state_path = state_path,
      cache_dir = cache_dir,
      max_age_days = max_age_days,
      timeout_s = timeout_s
    )
    state_rows[[base::length(state_rows) + 1L]] <- round_state |>
      dplyr::mutate(round = round_index)

    if (round_index >= max_rounds) {
      break
    }

    round_locations <- collect_hpt_txt_locations(round_state, cache_dir = cache_dir)
    state_domains <- read_hpt_txt_state(state_path)$domain
    new_hosts <- snowball_hosts(round_locations, base::c(known, state_domains))

    if (base::length(new_hosts) > 0L) {
      snowball_rows[[base::length(snowball_rows) + 1L]] <- snowball_provenance(round_locations, new_hosts) |>
        dplyr::mutate(round = round_index + 1L)
    }

    known <- base::union(known, new_hosts)
    queue <- new_hosts
    round_index <- round_index + 1L
  }

  state_tbl <- dplyr::bind_rows(state_rows)

  base::list(
    state = state_tbl,
    locations = collect_hpt_txt_locations(state_tbl, cache_dir = cache_dir),
    snowball = dplyr::bind_rows(
      tibble::tibble(host = base::character(), found_via = base::character(), round = base::integer()),
      snowball_rows
    )
  )
}

#' For each snowballed host, one domain whose cms-hpt.txt referenced it
snowball_provenance <- function(locations_tbl, new_hosts) {
  referenced <- dplyr::bind_rows(
    tibble::tibble(found_via = locations_tbl$domain, url = locations_tbl$mrf_url),
    tibble::tibble(found_via = locations_tbl$domain, url = locations_tbl$source_page_url)
  ) |>
    dplyr::mutate(url_host = extract_url_hostname(.data$url)) |>
    dplyr::filter(!base::is.na(.data$url_host)) |>
    dplyr::distinct(.data$found_via, .data$url_host)

  variant_tbl <- domain_variant_table(referenced$url_host) |>
    dplyr::rename(url_host = "original", host = "variant")

  variant_tbl |>
    dplyr::inner_join(referenced, by = "url_host", relationship = "many-to-many") |>
    dplyr::filter(.data$host %in% new_hosts) |>
    dplyr::distinct(.data$host, .keep_all = TRUE) |>
    dplyr::select("host", "found_via")
}
