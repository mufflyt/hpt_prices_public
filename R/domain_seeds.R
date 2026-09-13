#' Candidate website domains per CCN, for the cms-hpt.txt crawl
#'
#' Nothing in cms-hpt.txt names a CCN, so we crawl every domain any source
#' associates with a hospital and link entries back downstream. Sources:
#' - cms-hpt-tracker manifest.csv and gaps.csv;
#' - TPAFS machine_readable_links.csv (page, supplemental, and MRF hosts:
#'   all three, since the MRF host is often a vendor whose root carries the
#'   pointer file);
#' - DoltHub hospital-price-transparency-v3 and transparency-in-pricing
#'   (optional; public SQL API, 1000 rows per query);
#' - HIFLD 2020 hospitals WEBSITE, matched to CCNs by name and location.
#'
#' Every seed_domains_from_*() returns tibble(ccn, domain, seed_source,
#' match_score); build_domain_seeds() combines them, adds apex/www/parent
#' variants, and drops hosts that cannot carry a hospital's cms-hpt.txt.

hifld_hospitals_layer_url <- function() {
  "https://services5.arcgis.com/HDRa0B57OVrv2E1q/arcgis/rest/services/HIFLD_2020_Hospitals/FeatureServer/0"
}

dolthub_api_url <- function(owner, repo, branch = "main") {
  base::paste0("https://www.dolthub.com/api/v1alpha1/", owner, "/", repo, "/", branch)
}

empty_domain_seeds <- function() {
  tibble::tibble(
    ccn = base::character(),
    domain = base::character(),
    seed_source = base::character(),
    match_score = base::numeric()
  )
}

#' Accept a data frame or a CSV path
as_input_tbl <- function(x) {
  if (base::is.null(x)) {
    return(NULL)
  }

  if (base::is.character(x) && base::length(x) == 1L) {
    return(read_csv_chr(x))
  }

  tibble::as_tibble(x) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), base::as.character))
}

#' Multi-tenant platforms: a subdomain is a tenant, the parent is not theirs
hpt_platform_suffixes <- function() {
  base::c(
    "azurewebsites.net", "azureedge.net", "cloudfront.net", "amazonaws.com",
    "windows.net", "googleapis.com", "googleusercontent.com", "appspot.com",
    "web.app", "firebaseapp.com", "github.io", "githubusercontent.com",
    "herokuapp.com", "netlify.app", "vercel.app", "pages.dev", "wixsite.com",
    "squarespace.com", "wordpress.com", "blogspot.com", "sharepoint.com",
    "myshopify.com", "weebly.com", "godaddysites.com", "hubspotpagebuilder.com",
    "box.com", "dropboxusercontent.com", "cdn-website.com"
  )
}

#' Hosts that cannot carry a hospital's cms-hpt.txt at their root
#'
#' File-sharing, cloud-storage, and CDN endpoints (and cms.gov itself) show
#' up as MRF hosts, but their roots belong to the platform. Vendor hosts that
#' serve many hospitals are kept on purpose.
is_junk_hpt_host <- function(host) {
  host <- normalize_hpt_domain(host)

  exact_junk <- base::c(
    "google.com", "www.google.com", "docs.google.com", "drive.google.com",
    "sites.google.com", "storage.googleapis.com", "dropbox.com", "www.dropbox.com",
    "dl.dropboxusercontent.com", "github.com", "www.github.com",
    "raw.githubusercontent.com", "s3.amazonaws.com", "cms.gov", "www.cms.gov",
    "hhs.gov", "www.hhs.gov", "onedrive.live.com", "1drv.ms", "bit.ly",
    "tinyurl.com", "facebook.com", "www.facebook.com", "twitter.com", "x.com",
    "linkedin.com", "www.linkedin.com", "youtube.com", "www.youtube.com",
    "instagram.com", "www.instagram.com", "box.com", "app.box.com",
    "archive.org", "web.archive.org", "dolthub.com", "www.dolthub.com"
  )
  junk_pattern <- base::paste0(
    "(\\.blob\\.core\\.windows\\.net|\\.core\\.windows\\.net|\\.cloudfront\\.net|",
    "\\.amazonaws\\.com|\\.googleapis\\.com|\\.googleusercontent\\.com|",
    "\\.sharepoint\\.com|\\.azureedge\\.net|\\.box\\.com|\\.dropboxusercontent\\.com|",
    "\\.githubusercontent\\.com|\\.cdn-website\\.com)$"
  )

  base::is.na(host) |
    host %in% exact_junk |
    stringr::str_detect(host, junk_pattern) |
    stringr::str_detect(host, "^[0-9.]+$") |
    !stringr::str_detect(host, "\\.")
}

#' Registrable parent of a host ("files.hospital.org" -> "hospital.org")
#'
#' NA when the host already is registrable or sits on a multi-tenant
#' platform. US hospital domains are almost all two-label .org/.com/.net/
#' .edu/.gov; a few common two-level suffixes are handled explicitly.
registrable_parent <- function(host) {
  host <- normalize_hpt_domain(host)
  two_level_suffixes <- base::c("co.us", "com.au", "co.uk", "org.uk", "nhs.uk", "com.pr", "org.pr")
  platform_pattern <- base::paste0(
    "(^|\\.)(", base::paste(stringr::str_replace_all(hpt_platform_suffixes(), "\\.", "\\\\."), collapse = "|"), ")$"
  )

  n_labels <- stringr::str_count(host, "\\.") + 1L
  last_two <- stringr::str_extract(host, "[^.]+\\.[^.]+$")
  is_two_level <- last_two %in% two_level_suffixes
  parent <- dplyr::if_else(is_two_level, stringr::str_extract(host, "[^.]+\\.[^.]+\\.[^.]+$"), last_two)
  keep_labels <- dplyr::if_else(is_two_level, 3L, 2L)
  eligible <- !base::is.na(host) & n_labels > keep_labels & !stringr::str_detect(host, platform_pattern)

  dplyr::if_else(!base::is.na(eligible) & eligible, parent, NA_character_)
}

#' One row per (original, variant): the host, www-stripped, www-added, and
#' registrable-parent forms
#'
#' `www.` is only added to two-label apexes ("hospital.org"), never to
#' subdomains ("apps.vendor.com").
domain_variant_table <- function(domain, include_parent = TRUE) {
  original <- base::unique(base::as.character(domain))
  host <- normalize_hpt_domain(original)
  apex <- stringr::str_remove(host, "^www\\.")
  with_www <- dplyr::if_else(stringr::str_count(apex, "\\.") == 1L, base::paste0("www.", apex), NA_character_)
  parent <- if (include_parent) registrable_parent(apex) else base::rep(NA_character_, base::length(apex))
  parent_www <- dplyr::if_else(!base::is.na(parent), base::paste0("www.", parent), NA_character_)

  tibble::tibble(original, host, apex, with_www, parent, parent_www) |>
    tidyr::pivot_longer(-"original", values_to = "variant") |>
    dplyr::filter(!base::is.na(.data$variant)) |>
    dplyr::distinct(.data$original, .data$variant)
}

#' All variants of the given domains (see domain_variant_table())
#'
#' @return Unique character vector across all inputs.
domain_variants <- function(domain, include_parent = TRUE) {
  base::unique(domain_variant_table(domain, include_parent = include_parent)$variant)
}

#' Long seed rows from a table of URL columns
url_columns_to_seeds <- function(tbl, ccn_col, url_cols) {
  if (base::is.null(tbl) || base::nrow(tbl) == 0L) {
    return(empty_domain_seeds())
  }

  missing_cols <- base::setdiff(base::c(ccn_col, base::unname(url_cols)), base::names(tbl))

  if (base::length(missing_cols) > 0L) {
    base::stop("Seed table is missing columns: ", base::paste(missing_cols, collapse = ", "))
  }

  seed_tbls <- base::lapply(base::names(url_cols), function(seed_source) {
    tibble::tibble(
      ccn = normalize_ccn(tbl[[ccn_col]]),
      domain = normalize_hpt_domain(tbl[[url_cols[[seed_source]]]]),
      seed_source = seed_source,
      match_score = NA_real_
    )
  })

  dplyr::bind_rows(seed_tbls) |>
    dplyr::filter(!base::is.na(.data$ccn), !base::is.na(.data$domain)) |>
    dplyr::distinct()
}

#' Seeds from cms-hpt-tracker manifest.csv and gaps.csv
#'
#' @param manifest Data frame or path (ccn, domain, pointer_url, mrf_url,
#'   source_page_url).
#' @param gaps Data frame or path (ccn, seeded_domain, resolved_domain).
seed_domains_from_tracker <- function(manifest = NULL, gaps = NULL) {
  manifest_tbl <- as_input_tbl(manifest)
  gaps_tbl <- as_input_tbl(gaps)

  dplyr::bind_rows(
    url_columns_to_seeds(manifest_tbl, "ccn", base::c(
      tracker_manifest_domain = "domain",
      tracker_manifest_pointer = "pointer_url",
      tracker_manifest_mrf = "mrf_url",
      tracker_manifest_source_page = "source_page_url"
    )),
    url_columns_to_seeds(gaps_tbl, "ccn", base::c(
      tracker_gaps_seeded = "seeded_domain",
      tracker_gaps_resolved = "resolved_domain"
    ))
  )
}

#' Seeds from TPAFS machine_readable_links.csv, keeping all three hosts
#'
#' @param links Data frame or path (ccn, machine_readable_page,
#'   supplemental_url, machine_readable_url).
# ported from emb_colonoscopy R/hpt_hospital_discovery.R @ 471e067
# (build_hpt_domain_hints; now long, one row per host, instead of coalescing
# to the first non-missing host)
seed_domains_from_tpafs <- function(links) {
  url_columns_to_seeds(as_input_tbl(links), "ccn", base::c(
    tpafs_page = "machine_readable_page",
    tpafs_supplemental = "supplemental_url",
    tpafs_mrf = "machine_readable_url"
  ))
}

#' Run one SQL query against DoltHub's public API
dolthub_query <- function(owner, repo, sql, branch = "main") {
  response <- hpt_request(dolthub_api_url(owner, repo, branch), timeout_s = 120) |>
    httr2::req_url_query(q = sql) |>
    httr2::req_perform()

  if (httr2::resp_status(response) != 200L) {
    base::stop("DoltHub returned HTTP ", httr2::resp_status(response), " for: ", sql)
  }

  payload <- httr2::resp_body_json(response, simplifyVector = FALSE)

  if (!payload$query_execution_status %in% base::c("Success", "RowLimit")) {
    base::stop("DoltHub query failed (", payload$query_execution_message, "): ", sql)
  }

  rows <- base::lapply(payload$rows, function(row) {
    base::lapply(row, function(value) if (base::is.null(value)) NA_character_ else base::as.character(value))
  })

  dplyr::bind_rows(rows)
}

#' Page through a DoltHub table after checking the columns exist
#'
#' @param columns Named character: output name = DoltHub column.
fetch_dolthub_table <- function(owner, repo, table, columns, order_col,
                                where = NULL, branch = "main",
                                page_size = 1000L, max_pages = 100L) {
  described <- dolthub_query(owner, repo, base::paste0("DESCRIBE `", table, "`"), branch = branch)
  missing_cols <- base::setdiff(base::unname(columns), described$Field)

  if (base::length(missing_cols) > 0L) {
    base::stop(
      "DoltHub ", owner, "/", repo, ".", table, " no longer has columns: ",
      base::paste(missing_cols, collapse = ", ")
    )
  }

  select_sql <- base::paste0(
    "SELECT ",
    base::paste0("`", base::unname(columns), "` AS `", base::names(columns), "`", collapse = ", "),
    " FROM `", table, "`",
    if (!base::is.null(where)) base::paste0(" WHERE ", where) else "",
    " ORDER BY `", order_col, "`"
  )

  pages <- base::list()

  for (page_index in base::seq_len(max_pages)) {
    offset <- (page_index - 1L) * page_size
    page_tbl <- dolthub_query(
      owner, repo,
      base::paste0(select_sql, " LIMIT ", page_size, " OFFSET ", offset),
      branch = branch
    )
    pages[[page_index]] <- page_tbl
    base::message("DoltHub ", repo, ".", table, ": ", offset + base::nrow(page_tbl), " rows.")

    if (base::nrow(page_tbl) < page_size) {
      break
    }
  }

  dplyr::bind_rows(pages)
}

#' Download the DoltHub URL tables used for seeding (optional source)
#'
#' Cached as CSV under `cache_dir`; pass `refresh = TRUE` to re-download.
#' Returns NULL for a table whose download fails, with a message.
#'
#' @return List with `v3` (ccn, homepage_url, chargemaster_url) and `tip`
#'   (ccn, transparency_page, mrf_url).
fetch_dolthub_seed_tables <- function(cache_dir = hpt_path("seeds"), refresh = FALSE) {
  specs <- base::list(
    v3 = base::list(
      owner = "dolthub", repo = "hospital-price-transparency-v3", table = "hospitals",
      columns = base::c(ccn = "cms_certification_num", homepage_url = "homepage_url", chargemaster_url = "chargemaster_url"),
      order_col = "cms_certification_num",
      where = "homepage_url IS NOT NULL OR chargemaster_url IS NOT NULL"
    ),
    tip = base::list(
      owner = "dolthub", repo = "transparency-in-pricing", table = "hospital",
      columns = base::c(ccn = "id", transparency_page = "transparency_page", mrf_url = "mrf_url"),
      order_col = "id",
      where = "transparency_page IS NOT NULL OR mrf_url IS NOT NULL"
    )
  )

  base::lapply(specs, function(spec) {
    cache_path <- base::file.path(cache_dir, base::paste0("dolthub_", spec$repo, "_", spec$table, ".csv"))

    if (base::file.exists(cache_path) && !refresh) {
      return(read_csv_chr(cache_path))
    }

    downloaded <- base::tryCatch(
      fetch_dolthub_table(spec$owner, spec$repo, spec$table, spec$columns, spec$order_col, where = spec$where),
      error = function(error_condition) {
        base::message("DoltHub download failed: ", base::conditionMessage(error_condition))
        NULL
      }
    )

    if (!base::is.null(downloaded)) {
      write_csv_atomic(downloaded, cache_path)
    }

    downloaded
  })
}

#' Seeds from DoltHub tables returned by fetch_dolthub_seed_tables()
seed_domains_from_dolthub <- function(dolthub_tables) {
  if (base::is.null(dolthub_tables)) {
    return(empty_domain_seeds())
  }

  dplyr::bind_rows(
    url_columns_to_seeds(as_input_tbl(dolthub_tables$v3), "ccn", base::c(
      dolthub_v3_homepage = "homepage_url",
      dolthub_v3_chargemaster = "chargemaster_url"
    )),
    url_columns_to_seeds(as_input_tbl(dolthub_tables$tip), "ccn", base::c(
      dolthub_tip_transparency_page = "transparency_page",
      dolthub_tip_mrf = "mrf_url"
    ))
  )
}

#' Download HIFLD 2020 hospitals (name, address, website; no CCN)
#'
#' Public ArcGIS FeatureServer, 2000 records per page. Cached as CSV.
fetch_hifld_hospitals <- function(layer_url = hifld_hospitals_layer_url(),
                                  cache_path = hpt_path("seeds", "hifld_2020_hospitals.csv"),
                                  refresh = FALSE,
                                  page_size = 2000L) {
  if (base::file.exists(cache_path) && !refresh) {
    return(read_csv_chr(cache_path))
  }

  pages <- base::list()
  offset <- 0L

  repeat {
    response <- hpt_request(base::paste0(layer_url, "/query"), timeout_s = 120) |>
      httr2::req_url_query(
        where = "1=1",
        outFields = "OBJECTID,NAME,ALT_NAME,ADDRESS,CITY,STATE,ZIP,WEBSITE,STATUS,TYPE",
        returnGeometry = "false",
        orderByFields = "OBJECTID",
        resultOffset = offset,
        resultRecordCount = page_size,
        f = "json"
      ) |>
      httr2::req_perform()

    payload <- httr2::resp_body_json(response, simplifyVector = FALSE)

    if (!base::is.null(payload$error)) {
      base::stop("HIFLD query failed: ", payload$error$message)
    }

    page_tbl <- dplyr::bind_rows(base::lapply(payload$features, function(feature) {
      base::lapply(feature$attributes, function(value) if (base::is.null(value)) NA_character_ else base::as.character(value))
    }))
    pages[[base::length(pages) + 1L]] <- page_tbl
    offset <- offset + base::nrow(page_tbl)
    base::message("HIFLD hospitals: ", offset, " rows.")

    if (base::nrow(page_tbl) == 0L || !base::isTRUE(payload$exceededTransferLimit)) {
      break
    }
  }

  hifld_tbl <- dplyr::bind_rows(pages)
  write_csv_atomic(hifld_tbl, cache_path)
  hifld_tbl
}

#' Match HIFLD hospitals to CCNs with the project's CCN matcher
#'
#' HIFLD rows are facility records like any other (name, alternate name,
#' street, city, state), so they go through [match_facilities_to_ccn()] in
#' R/ccn_match.R: same-state candidates scored on name, street, and city,
#' kept only above `min_score` with a `min_margin` lead over the runner-up.
#' No MRF URL or NPI is available, so the tracker and NPI tiers are skipped
#' (NULL) and only its name/address tier can fire.
#'
#' @param roster_tbl CMS roster: facility_id, facility_name, address,
#'   citytown, state (zip_code optional).
#' @return tibble(ccn, website, match_score, hifld_name).
match_hifld_to_roster <- function(hifld_tbl, roster_tbl, min_score = 0.6, min_margin = 0.15) {
  hifld_input <- as_input_tbl(hifld_tbl)

  if (!"ALT_NAME" %in% base::names(hifld_input)) {
    hifld_input$ALT_NAME <- NA_character_
  }

  facilities <- hifld_input |>
    dplyr::filter(!base::is.na(.data$WEBSITE), !stringr::str_detect(.data$WEBSITE, "^\\s*(NOT AVAILABLE)?\\s*$")) |>
    dplyr::transmute(
      facility_key = base::paste0("hifld_", dplyr::row_number()),
      source = "hifld",
      mrf_url = NA_character_,
      hospital_name = .data$NAME,
      location_name = dplyr::na_if(.data$ALT_NAME, "NOT AVAILABLE"),
      address = .data$ADDRESS,
      city = .data$CITY,
      state = stringr::str_to_upper(stringr::str_trim(.data$STATE)),
      license_number = NA_character_,
      license_state = NA_character_,
      type_2_npi = NA_character_,
      website = .data$WEBSITE
    )

  universe <- as_input_tbl(roster_tbl) |>
    dplyr::mutate(
      facility_id = normalize_ccn(.data$facility_id),
      state = stringr::str_to_upper(stringr::str_trim(.data$state))
    )
  matched <- match_facilities_to_ccn(
    facilities,
    tracker_manifest = NULL,
    npi_xwalk = NULL,
    universe = universe,
    min_score = min_score,
    min_margin = min_margin
  )

  matched |>
    dplyr::filter(!base::is.na(.data$ccn), !.data$ccn_ambiguous) |>
    dplyr::arrange(dplyr::desc(.data$ccn_match_score)) |>
    dplyr::distinct(.data$ccn, .data$website, .keep_all = TRUE) |>
    dplyr::transmute(
      ccn = .data$ccn,
      website = .data$website,
      match_score = base::round(.data$ccn_match_score, 3),
      hifld_name = .data$hospital_name
    )
}

#' Seeds from HIFLD WEBSITE via confident roster matches
seed_domains_from_hifld <- function(hifld_tbl, roster_tbl, min_score = 0.6) {
  if (base::is.null(hifld_tbl) || base::is.null(roster_tbl)) {
    return(empty_domain_seeds())
  }

  matched <- match_hifld_to_roster(hifld_tbl, roster_tbl, min_score = min_score)
  base::message("HIFLD matched ", base::nrow(matched), " roster hospitals with a website.")

  matched |>
    dplyr::transmute(
      ccn = .data$ccn,
      domain = normalize_hpt_domain(.data$website),
      seed_source = "hifld_website",
      match_score = .data$match_score
    ) |>
    dplyr::filter(!base::is.na(.data$domain))
}

#' Long table of candidate domains for a set of CCNs
#'
#' @param ccns CCNs to keep (NULL keeps every CCN the sources mention).
#' @param tracker_manifest,tracker_gaps,tpafs_links Data frames or paths.
#' @param dolthub_tables Output of fetch_dolthub_seed_tables(), or NULL.
#' @param hifld_tbl Output of fetch_hifld_hospitals(), or NULL.
#' @param roster_tbl CMS roster used to match HIFLD rows to CCNs.
#' @return Distinct tibble(ccn, domain, seed_source, match_score, is_variant).
build_domain_seeds <- function(ccns = NULL,
                               tracker_manifest = NULL,
                               tracker_gaps = NULL,
                               tpafs_links = NULL,
                               dolthub_tables = NULL,
                               hifld_tbl = NULL,
                               roster_tbl = NULL,
                               add_variants = TRUE) {
  seeds <- dplyr::bind_rows(
    empty_domain_seeds(),
    seed_domains_from_tracker(tracker_manifest, tracker_gaps),
    if (!base::is.null(tpafs_links)) seed_domains_from_tpafs(tpafs_links),
    seed_domains_from_dolthub(dolthub_tables),
    seed_domains_from_hifld(hifld_tbl, roster_tbl)
  )

  if (!base::is.null(ccns)) {
    seeds <- seeds |>
      dplyr::filter(.data$ccn %in% normalize_ccn(ccns))
  }

  seeds <- seeds |>
    dplyr::mutate(is_variant = FALSE)

  if (add_variants && base::nrow(seeds) > 0L) {
    variant_tbl <- domain_variant_table(seeds$domain)

    seeds <- seeds |>
      dplyr::inner_join(variant_tbl, by = c("domain" = "original"), relationship = "many-to-many") |>
      dplyr::mutate(is_variant = .data$variant != .data$domain, domain = .data$variant) |>
      dplyr::select(-"variant")
  }

  result <- seeds |>
    dplyr::filter(!is_junk_hpt_host(.data$domain)) |>
    dplyr::group_by(.data$ccn, .data$domain, .data$seed_source) |>
    dplyr::summarise(
      match_score = if (base::all(base::is.na(.data$match_score))) NA_real_ else base::max(.data$match_score, na.rm = TRUE),
      is_variant = base::all(.data$is_variant),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$ccn, .data$domain, .data$seed_source)

  base::message(
    "Domain seeds: ", scales::comma(base::nrow(result)), " rows, ",
    scales::comma(dplyr::n_distinct(result$ccn)), " CCNs, ",
    scales::comma(dplyr::n_distinct(result$domain)), " distinct domains."
  )

  result
}
