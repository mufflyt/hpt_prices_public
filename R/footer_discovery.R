#' Fallback MRF discovery through the "Price Transparency" footer link
#'
#' CMS requires every page, homepage included, to carry a footer link
#' labeled "Price Transparency" that goes straight to the page linking the
#' MRF. When /cms-hpt.txt is missing or broken we follow that link: fetch the
#' homepage, find <a> elements whose text matches "price transparency"
#' (footer links first), follow up to two of them, and collect links that
#' look like MRFs (`<ein>_<name>_standardcharges.csv|json`, or any .csv,
#' .json, .zip). Unlike the pointer file, which the rule says must be open
#' to automated search, this is an ordinary HTML crawl, so it honors
#' robots.txt for our user agent (or `*`). Pages that build their links in
#' JavaScript look empty to this crawler.

.hpt_robots_cache <- base::new.env(parent = base::emptyenv())

reset_robots_cache <- function() {
  base::rm(list = base::ls(.hpt_robots_cache), envir = .hpt_robots_cache)
  base::invisible(NULL)
}

#' Product token of our user agent ("hpt-prices-research/0.1" -> "hpt-prices-research")
hpt_robots_agent_token <- function() {
  stringr::str_to_lower(stringr::str_extract(hpt_user_agent(), "^[^/\\s]+"))
}

hpt_footer_terminal_statuses <- function() {
  base::c(
    "ok", "no_mrf_link", "no_price_link", "not_html", "robots_disallowed",
    "price_page_robots_disallowed", "not_found", "blocked", "dns_fail", "tls_fail",
    "price_page_not_found", "price_page_blocked", "price_page_dns_fail", "price_page_tls_fail",
    "price_page_not_html"
  )
}

#' Rules from robots.txt that apply to `agent_token`
#'
#' Groups naming our token win; otherwise `*` groups apply (RFC 9309). No
#' applicable group means everything is allowed.
#'
#' @return tibble(type = "allow"/"disallow", pattern).
parse_robots_txt <- function(robots_text, agent_token = hpt_robots_agent_token()) {
  lines <- stringr::str_split(decode_hpt_body(robots_text), "\r\n|\r|\n")[[1]]
  lines <- stringr::str_trim(stringr::str_remove(lines, "#.*$"))
  matched <- stringr::str_match(lines, "^([A-Za-z-]+)\\s*:\\s*(.*)$")
  field <- stringr::str_to_lower(matched[, 2])
  value <- stringr::str_trim(matched[, 3])

  groups <- base::list()
  current_agents <- base::character(0)
  current_rules <- base::list()
  in_rules <- FALSE

  close_group <- function() {
    if (base::length(current_agents) > 0L) {
      groups[[base::length(groups) + 1L]] <<- base::list(
        agents = current_agents,
        rules = dplyr::bind_rows(tibble::tibble(type = base::character(), pattern = base::character()), current_rules)
      )
    }
  }

  for (i in base::seq_along(lines)) {
    if (base::is.na(field[[i]])) {
      next
    }

    if (field[[i]] == "user-agent") {
      if (in_rules) {
        close_group()
        current_agents <- base::character(0)
        current_rules <- base::list()
        in_rules <- FALSE
      }
      current_agents <- base::c(current_agents, stringr::str_to_lower(value[[i]]))
    } else if (field[[i]] %in% base::c("allow", "disallow") && base::length(current_agents) > 0L) {
      in_rules <- TRUE
      current_rules[[base::length(current_rules) + 1L]] <- tibble::tibble(type = field[[i]], pattern = value[[i]])
    }
  }

  close_group()

  own_groups <- base::Filter(function(group) {
    base::any(group$agents != "*" & base::vapply(group$agents, function(agent) {
      stringr::str_detect(agent_token, stringr::fixed(agent))
    }, base::logical(1)))
  }, groups)
  star_groups <- base::Filter(function(group) "*" %in% group$agents, groups)
  applicable <- if (base::length(own_groups) > 0L) own_groups else star_groups

  dplyr::bind_rows(
    tibble::tibble(type = base::character(), pattern = base::character()),
    base::lapply(applicable, function(group) group$rules)
  ) |>
    dplyr::filter(base::nzchar(.data$pattern))
}

#' Translate a robots.txt path pattern (`*` wildcard, `$` end anchor) to a regex
robots_pattern_regex <- function(pattern) {
  anchored_end <- stringr::str_ends(pattern, stringr::fixed("$"))
  escaped <- pattern |>
    stringr::str_remove("\\$$") |>
    stringr::str_replace_all("([.+?^(){}|\\[\\]\\\\$])", "\\\\\\1") |>
    stringr::str_replace_all("\\*", ".*")

  base::paste0("^", escaped, base::ifelse(anchored_end, "$", ""))
}

#' Is `path` allowed under robots rules? Longest match wins, ties allow.
robots_path_allowed <- function(rules, path) {
  if (base::is.null(rules) || base::nrow(rules) == 0L) {
    return(TRUE)
  }

  if (base::is.na(path) || !base::nzchar(path)) {
    path <- "/"
  }

  matches <- stringr::str_detect(path, robots_pattern_regex(rules$pattern))

  if (!base::any(matches)) {
    return(TRUE)
  }

  match_length <- base::nchar(rules$pattern)
  best <- base::max(match_length[matches])
  best_types <- rules$type[matches & match_length == best]

  "allow" %in% best_types
}

#' Robots decision for a fetch result (RFC 9309 section 2.3.1)
#'
#' 2xx text is parsed; an HTML soft 404 or a 4xx means no rules; a 5xx or
#' an unreachable server means "assume everything is disallowed".
robots_rules_from_response <- function(resp_or_error) {
  disallow_all <- tibble::tibble(type = "disallow", pattern = "/")
  allow_all <- tibble::tibble(type = base::character(), pattern = base::character())

  if (!base::inherits(resp_or_error, "httr2_response")) {
    return(disallow_all)
  }

  status <- httr2::resp_status(resp_or_error)

  if (status >= 500L) {
    return(disallow_all)
  }

  if (status >= 400L) {
    return(allow_all)
  }

  body_text <- decode_hpt_body(base::tryCatch(httr2::resp_body_raw(resp_or_error), error = function(error_condition) base::raw(0)))

  if (stringr::str_detect(body_text, stringr::regex("<html|<!doctype", ignore_case = TRUE))) {
    return(allow_all)
  }

  parse_robots_txt(body_text)
}

#' Fetch robots.txt once per host (cached for the R session)
#'
#' A host whose robots.txt cannot be reached at all is recorded with its
#' transport failure (dns_fail, tls_fail, ...) so the crawl reports that
#' instead of a misleading "robots_disallowed".
ensure_robots <- function(hosts, timeout_s = 20) {
  hosts <- base::unique(hosts[!base::is.na(hosts)])
  missing_hosts <- hosts[!base::vapply(hosts, base::exists, base::logical(1), envir = .hpt_robots_cache, inherits = FALSE)]

  if (base::length(missing_hosts) == 0L) {
    return(base::invisible(NULL))
  }

  responses <- hpt_perform_parallel(base::lapply(
    base::paste0("https://", missing_hosts, "/robots.txt"),
    hpt_request, timeout_s = timeout_s, max_tries = 1
  ))
  retry_index <- base::which(base::vapply(responses, function(resp) {
    !base::inherits(resp, "httr2_response") && classify_http_outcome(resp) %in% hpt_transport_failures()
  }, base::logical(1)))

  if (base::length(retry_index) > 0L) {
    http_responses <- hpt_perform_parallel(base::lapply(
      base::paste0("http://", missing_hosts[retry_index], "/robots.txt"),
      hpt_request, timeout_s = timeout_s, max_tries = 1
    ))
    responses[retry_index] <- http_responses
  }

  for (i in base::seq_along(missing_hosts)) {
    fetch_status <- if (base::inherits(responses[[i]], "httr2_response")) "ok" else classify_http_outcome(responses[[i]])
    base::assign(
      missing_hosts[[i]],
      base::list(rules = robots_rules_from_response(responses[[i]]), fetch_status = fetch_status),
      envir = .hpt_robots_cache
    )
  }

  base::invisible(NULL)
}

robots_url_allowed <- function(url) {
  host <- url_host(url)

  if (base::is.na(host) || !base::exists(host, envir = .hpt_robots_cache, inherits = FALSE)) {
    return(FALSE)
  }

  path <- stringr::str_replace(url, stringr::regex("^https?://[^/?#]+", ignore_case = TRUE), "")
  path <- stringr::str_remove(path, "#.*$")
  robots_path_allowed(base::get(host, envir = .hpt_robots_cache)$rules, path)
}

#' "ok" if robots.txt got any HTTP response, else the transport failure class
robots_fetch_status <- function(host) {
  if (base::is.na(host) || !base::exists(host, envir = .hpt_robots_cache, inherits = FALSE)) {
    return(NA_character_)
  }

  base::get(host, envir = .hpt_robots_cache)$fetch_status
}

#' Does a link look like an MRF?
#'
#' CMS naming (`_standardcharges.csv|json`, optionally zipped) anywhere in
#' the URL, or a path ending in .csv/.json/.zip.
is_mrf_link <- function(url) {
  path <- stringr::str_remove(url, "[?#].*$")

  stringr::str_detect(url, stringr::regex("_standardcharges\\.(csv|json)(\\.(zip|gz))?", ignore_case = TRUE)) |
    stringr::str_detect(path, stringr::regex("\\.(csv|json|zip)$", ignore_case = TRUE))
}

#' Anchors in a parsed page with absolute hrefs, text, and footer flag
page_anchors <- function(doc, base_url) {
  anchors <- xml2::xml_find_all(doc, "//a[@href]")

  if (base::length(anchors) == 0L) {
    return(tibble::tibble(url = base::character(), link_text = base::character(), in_footer = base::logical()))
  }

  base_href <- xml2::xml_attr(xml2::xml_find_first(doc, "//base[@href]"), "href")
  base_url <- if (!base::is.na(base_href)) xml2::url_absolute(base_href, base_url) else base_url

  footer_xpath <- base::paste0(
    "boolean(ancestor::footer or ancestor::*[@role='contentinfo'] or ",
    "ancestor::*[contains(translate(@id, 'FOTER', 'foter'), 'footer')] or ",
    "ancestor::*[contains(translate(@class, 'FOTER', 'foter'), 'footer')])"
  )
  visible_text <- stringr::str_squish(rvest::html_text2(anchors))
  label_text <- dplyr::coalesce(xml2::xml_attr(anchors, "aria-label"), xml2::xml_attr(anchors, "title"))
  href <- stringr::str_trim(xml2::xml_attr(anchors, "href"))

  tibble::tibble(
    url = xml2::url_absolute(href, base_url),
    link_text = dplyr::if_else(base::nzchar(visible_text), visible_text, dplyr::coalesce(label_text, "")),
    in_footer = base::vapply(anchors, function(anchor) xml2::xml_find_lgl(anchor, footer_xpath), base::logical(1))
  ) |>
    dplyr::filter(
      !base::is.na(.data$url),
      stringr::str_detect(.data$url, stringr::regex("^https?://", ignore_case = TRUE))
    )
}

#' "Price Transparency" links on a page, footer links first
#'
#' Falls back to "standard charges" / "chargemaster" wording only when no
#' price transparency link exists.
extract_price_transparency_links <- function(doc, base_url) {
  anchors <- page_anchors(doc, base_url)
  primary <- stringr::regex("pric(e|ing)\\s*transparency", ignore_case = TRUE)
  secondary <- stringr::regex("standard\\s*charges|chargemaster", ignore_case = TRUE)

  matched <- anchors |>
    dplyr::filter(stringr::str_detect(.data$link_text, primary))

  if (base::nrow(matched) == 0L) {
    matched <- anchors |>
      dplyr::filter(stringr::str_detect(.data$link_text, secondary))
  }

  matched |>
    dplyr::mutate(url = stringr::str_remove(.data$url, "#.*$")) |>
    dplyr::arrange(dplyr::desc(.data$in_footer)) |>
    dplyr::distinct(.data$url, .keep_all = TRUE)
}

#' MRF-looking links on a page
extract_mrf_links <- function(doc, base_url) {
  page_anchors(doc, base_url) |>
    dplyr::filter(is_mrf_link(.data$url)) |>
    dplyr::distinct(.data$url, .keep_all = TRUE) |>
    dplyr::transmute(
      mrf_url = .data$url,
      mrf_link_text = .data$link_text,
      matches_cms_naming = stringr::str_detect(.data$url, stringr::regex("_standardcharges\\.(csv|json)", ignore_case = TRUE))
    )
}

#' Fetch HTML pages in parallel and parse them
#'
#' @return List (same order) of lists: status, http_status, final_url, doc,
#'   error_message.
fetch_html_pages <- function(urls, timeout_s = 30, http_fallback = FALSE) {
  if (base::length(urls) == 0L) {
    return(base::list())
  }

  fetch_round <- function(round_urls) {
    responses <- hpt_perform_parallel(base::lapply(round_urls, function(url) {
      hpt_request(url, timeout_s = timeout_s, max_tries = 2) |>
        httr2::req_options(maxfilesize = 20 * 1024^2)
    }))
    base::lapply(responses, classify_html_response)
  }

  results <- fetch_round(urls)

  if (http_fallback) {
    retry_index <- base::which(base::vapply(results, function(result) result$status, base::character(1)) %in% hpt_transport_failures())

    if (base::length(retry_index) > 0L) {
      http_urls <- stringr::str_replace(urls[retry_index], stringr::regex("^https://", ignore_case = TRUE), "http://")
      http_results <- fetch_round(http_urls)

      for (j in base::seq_along(retry_index)) {
        if (!http_results[[j]]$status %in% hpt_transport_failures()) {
          results[[retry_index[[j]]]] <- http_results[[j]]
        }
      }
    }
  }

  results
}

classify_html_response <- function(resp_or_error) {
  if (!base::inherits(resp_or_error, "httr2_response")) {
    return(base::list(
      status = classify_http_outcome(resp_or_error),
      http_status = NA_integer_,
      final_url = NA_character_,
      doc = NULL,
      error_message = stringr::str_squish(base::conditionMessage(resp_or_error))
    ))
  }

  status <- classify_http_outcome(resp_or_error)
  final_url <- httr2::resp_url(resp_or_error)
  doc <- NULL

  if (status == "ok") {
    body_raw <- base::tryCatch(httr2::resp_body_raw(resp_or_error), error = function(error_condition) base::raw(0))
    content_type <- base::tryCatch(httr2::resp_content_type(resp_or_error), error = function(error_condition) NA_character_)
    looks_html <- is_html_content_type(content_type) ||
      stringr::str_detect(decode_hpt_body(utils::head(body_raw, 2048L)), stringr::regex("<html|<!doctype|<a\\s", ignore_case = TRUE))

    if (base::length(body_raw) > 0L && looks_html) {
      doc <- base::tryCatch(xml2::read_html(body_raw), error = function(error_condition) NULL)
    }

    if (base::is.null(doc)) {
      status <- "not_html"
    }
  }

  base::list(
    status = status,
    http_status = httr2::resp_status(resp_or_error),
    final_url = final_url,
    doc = doc,
    error_message = if (status %in% base::c("ok")) NA_character_ else base::paste0("HTTP ", httr2::resp_status(resp_or_error))
  )
}

empty_footer_results <- function() {
  tibble::tibble(
    domain = base::character(),
    homepage_url = base::character(),
    price_page_url = base::character(),
    mrf_url = base::character(),
    link_text = base::character(),
    status = base::character(),
    in_footer = base::logical(),
    mrf_link_text = base::character(),
    matches_cms_naming = base::logical(),
    fetched_at = base::character()
  )
}

#' Footer-link discovery for many domains, in parallel rounds
#'
#' Rounds: robots.txt for homepage hosts; homepages; robots.txt for price
#' page hosts; price pages. MRF links on the homepage itself count too.
#'
#' @return One row per MRF link found, or one row per domain with the
#'   failure status: domain, homepage_url, price_page_url, mrf_url,
#'   link_text (the "Price Transparency" link text), status, in_footer,
#'   mrf_link_text, matches_cms_naming, fetched_at.
discover_via_footer_batch <- function(domains, max_price_links = 2L, timeout_s = 30) {
  domains <- base::unique(normalize_hpt_domain(domains))
  domains <- domains[!base::is.na(domains)]

  if (base::length(domains) == 0L) {
    return(empty_footer_results())
  }

  base::message("Footer discovery for ", base::length(domains), " domains.")
  ensure_robots(domains, timeout_s = timeout_s)
  homepage_urls <- base::paste0("https://", domains, "/")
  robots_status <- base::vapply(domains, robots_fetch_status, base::character(1), USE.NAMES = FALSE)
  home_allowed <- base::vapply(homepage_urls, robots_url_allowed, base::logical(1), USE.NAMES = FALSE)
  home_fetch <- home_allowed & robots_status == "ok"
  home_results <- base::vector("list", base::length(domains))
  home_results[home_fetch] <- fetch_html_pages(homepage_urls[home_fetch], timeout_s = timeout_s, http_fallback = TRUE)

  price_links <- base::lapply(base::seq_along(domains), function(i) {
    result <- home_results[[i]]

    if (base::is.null(result) || base::is.null(result$doc)) {
      return(NULL)
    }

    extract_price_transparency_links(result$doc, result$final_url) |>
      utils::head(max_price_links)
  })

  page_urls <- base::unique(base::unlist(base::lapply(price_links, function(links) {
    if (base::is.null(links)) base::character(0) else links$url[!is_mrf_link(links$url)]
  })))
  ensure_robots(url_host(page_urls), timeout_s = timeout_s)
  page_allowed <- base::vapply(page_urls, robots_url_allowed, base::logical(1), USE.NAMES = FALSE)
  page_results <- base::vector("list", base::length(page_urls))
  page_results[page_allowed] <- fetch_html_pages(page_urls[page_allowed], timeout_s = timeout_s)
  base::names(page_results) <- page_urls

  fetched_at <- utc_timestamp()

  rows <- base::lapply(base::seq_along(domains), function(i) {
    base_row <- tibble::tibble(domain = domains[[i]], homepage_url = homepage_urls[[i]], fetched_at = fetched_at)

    if (robots_status[[i]] != "ok") {
      return(dplyr::mutate(base_row, status = robots_status[[i]]))
    }

    if (!home_allowed[[i]]) {
      return(dplyr::mutate(base_row, status = "robots_disallowed"))
    }

    home <- home_results[[i]]
    base_row$homepage_url <- home$final_url %||% homepage_urls[[i]]

    if (home$status != "ok") {
      return(dplyr::mutate(base_row, status = home$status))
    }

    links <- price_links[[i]]
    home_mrfs <- extract_mrf_links(home$doc, home$final_url) |>
      dplyr::mutate(price_page_url = home$final_url, link_text = NA_character_, in_footer = NA)

    link_mrfs <- base::lapply(base::seq_len(base::nrow(links)), function(j) {
      link_url <- links$url[[j]]

      if (is_mrf_link(link_url)) {
        mrf_tbl <- tibble::tibble(
          mrf_url = link_url,
          mrf_link_text = links$link_text[[j]],
          matches_cms_naming = stringr::str_detect(link_url, stringr::regex("_standardcharges\\.(csv|json)", ignore_case = TRUE))
        )
      } else if (!base::is.null(page_results[[link_url]]$doc)) {
        mrf_tbl <- extract_mrf_links(page_results[[link_url]]$doc, page_results[[link_url]]$final_url)
      } else {
        return(NULL)
      }

      dplyr::mutate(mrf_tbl, price_page_url = link_url, link_text = links$link_text[[j]], in_footer = links$in_footer[[j]])
    })

    found <- dplyr::bind_rows(home_mrfs, link_mrfs) |>
      dplyr::distinct(.data$mrf_url, .keep_all = TRUE)

    if (base::nrow(found) > 0L) {
      return(dplyr::bind_cols(base_row[base::rep(1L, base::nrow(found)), ], found) |> dplyr::mutate(status = "ok"))
    }

    if (base::nrow(links) == 0L) {
      return(dplyr::mutate(base_row, status = "no_price_link"))
    }

    first_link <- links[1L, ]
    page_statuses <- base::vapply(links$url, function(link_url) {
      if (!base::isTRUE(page_allowed[base::match(link_url, page_urls)])) {
        return("price_page_robots_disallowed")
      }
      page <- page_results[[link_url]]
      if (page$status == "ok") "no_mrf_link" else base::paste0("price_page_", page$status)
    }, base::character(1), USE.NAMES = FALSE)
    status <- if ("no_mrf_link" %in% page_statuses) "no_mrf_link" else page_statuses[[1]]

    dplyr::mutate(
      base_row,
      price_page_url = first_link$url,
      link_text = first_link$link_text,
      in_footer = first_link$in_footer,
      status = status
    )
  })

  results_tbl <- dplyr::bind_rows(empty_footer_results(), rows) |>
    dplyr::relocate("fetched_at", .after = dplyr::last_col())

  base::message(
    "Footer discovery: ", dplyr::n_distinct(results_tbl$domain[results_tbl$status == "ok"]),
    " of ", base::length(domains), " domains yielded MRF links."
  )

  results_tbl
}

#' Footer-link discovery for one domain
discover_via_footer <- function(domain, max_price_links = 2L, timeout_s = 30) {
  discover_via_footer_batch(domain[[1]], max_price_links = max_price_links, timeout_s = timeout_s) |>
    dplyr::select("domain", "homepage_url", "price_page_url", "mrf_url", "link_text", "status", dplyr::everything())
}

read_footer_state <- function(state_path) {
  if (!base::file.exists(state_path)) {
    return(empty_footer_results())
  }

  read_csv_chr(state_path) |>
    dplyr::mutate(
      in_footer = base::as.logical(.data$in_footer),
      matches_cms_naming = base::as.logical(.data$matches_cms_naming)
    )
}

#' Resumable footer-link crawl (same resume rules as crawl_hpt_txt())
crawl_footer <- function(domains,
                         state_path = hpt_path("footer_state.csv"),
                         max_age_days = 30,
                         chunk_size = 50L,
                         max_price_links = 2L) {
  domains <- base::unique(normalize_hpt_domain(domains))
  domains <- domains[!base::is.na(domains)]
  state_tbl <- read_footer_state(state_path)

  due <- base::setdiff(domains, fresh_state_keys(state_tbl, hpt_footer_terminal_statuses(), max_age_days))
  base::message("Footer crawl: ", base::length(domains), " domains, ", base::length(due), " to fetch.")
  chunks <- base::split(due, base::ceiling(base::seq_along(due) / chunk_size))

  for (chunk in chunks) {
    new_tbl <- discover_via_footer_batch(chunk, max_price_links = max_price_links)
    state_tbl <- upsert_state(state_tbl, new_tbl)
    write_csv_atomic(state_tbl, state_path)
  }

  state_tbl |>
    dplyr::filter(.data$domain %in% domains)
}
