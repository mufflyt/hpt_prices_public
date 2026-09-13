#' Polite HTTP for crawling hospital and vendor sites
#'
#' 45 CFR 180.50(d)(3)(iv) requires MRFs and cms-hpt.txt to be accessible
#' "to automated searches and direct file downloads", so fetching them is
#' what the rule intends. We still crawl politely:
#' - one project user agent, no personal email (set HPT_USER_AGENT to add a
#'   contact if you want sites to be able to reach you);
#' - at most `HPT_PER_HOST_RPS` requests per second to any one host
#'   (httr2::req_throttle keyed by host, so parallel requests to different
#'   hosts don't slow each other down);
#' - retries only on 429 / 5xx / network errors, honoring Retry-After;
#' - 401/403 are recorded as "blocked" and never retried or worked around.

hpt_user_agent <- function() {
  base::Sys.getenv("HPT_USER_AGENT", unset = "hpt-prices-research/0.1")
}

url_host <- function(url) {
  stringr::str_to_lower(
    stringr::str_match(url, stringr::regex("^https?://([^/:?#]+)", ignore_case = TRUE))[, 2]
  )
}

#' Build a throttled, retrying request
#'
#' @param url Target URL.
#' @param timeout_s Whole-request timeout in seconds.
#' @param max_tries Total attempts for transient failures.
hpt_request <- function(url, timeout_s = 30, max_tries = 3) {
  per_host_rps <- base::as.numeric(base::Sys.getenv("HPT_PER_HOST_RPS", unset = "1"))
  host <- url_host(url) %||% "unknown-host"

  httr2::request(url) |>
    httr2::req_user_agent(hpt_user_agent()) |>
    httr2::req_timeout(timeout_s) |>
    httr2::req_throttle(capacity = 1, fill_time_s = 1 / per_host_rps, realm = host) |>
    httr2::req_retry(
      max_tries = max_tries,
      is_transient = function(resp) httr2::resp_status(resp) %in% base::c(429L, 500L, 502L, 503L, 504L),
      backoff = function(attempt) base::min(60, 2^attempt)
    ) |>
    httr2::req_error(is_error = function(resp) FALSE)
}

#' Classify the outcome of a request (response or error condition)
#'
#' @return One of "ok", "not_found", "blocked", "http_error", "timeout",
#'   "dns_fail", "tls_fail", "network_error".
classify_http_outcome <- function(resp_or_error) {
  if (base::inherits(resp_or_error, "httr2_response")) {
    status <- httr2::resp_status(resp_or_error)

    return(dplyr::case_when(
      status >= 200L & status < 300L ~ "ok",
      status %in% base::c(404L, 410L) ~ "not_found",
      status %in% base::c(401L, 403L) ~ "blocked",
      TRUE ~ "http_error"
    ))
  }

  message_text <- base::conditionMessage(resp_or_error)

  dplyr::case_when(
    stringr::str_detect(message_text, stringr::regex("timeout|timed out", ignore_case = TRUE)) ~ "timeout",
    stringr::str_detect(message_text, stringr::regex("resolve|name or service|getaddrinfo", ignore_case = TRUE)) ~ "dns_fail",
    stringr::str_detect(message_text, stringr::regex("ssl|tls|certificate", ignore_case = TRUE)) ~ "tls_fail",
    TRUE ~ "network_error"
  )
}

#' Perform many requests in parallel, never erroring on individual failures
#'
#' @param requests List of httr2 requests.
#' @param max_active Concurrent connections across all hosts.
#' @return List (same order) of `httr2_response` objects or error conditions.
hpt_perform_parallel <- function(requests, max_active = NULL) {
  if (base::length(requests) == 0L) {
    return(base::list())
  }

  max_active <- max_active %||% base::as.integer(base::Sys.getenv("HPT_MAX_ACTIVE", unset = "16"))

  # httr2 1.2.2's queue consults the throttle only for the next pending
  # request, so a run of same-host requests stalls every other host behind
  # it. Submit round-robin across hosts, then restore the caller's order.
  submit_order <- interleave_by_host(base::vapply(requests, function(req) req$url, base::character(1)))

  responses <- httr2::req_perform_parallel(
    requests[submit_order],
    on_error = "continue",
    progress = base::interactive(),
    max_active = max_active
  )

  responses[base::order(submit_order)]
}

#' Order indices so consecutive requests hit different hosts where possible
#'
#' Returns a permutation of `seq_along(urls)`: first request to every host,
#' then the second to every host, and so on (hosts in first-seen order).
interleave_by_host <- function(urls) {
  hosts <- url_host(urls)
  hosts[base::is.na(hosts)] <- "unknown-host"
  rank_within_host <- stats::ave(base::seq_along(hosts), hosts, FUN = base::seq_along)
  host_order <- base::match(hosts, base::unique(hosts))

  base::order(rank_within_host, host_order)
}
