#' Offline tests for cms-hpt.txt discovery, domain seeding, and the footer
#' fallback. Anything that fetches runs under httr2 mocked responses, so no
#' request leaves the machine.

discovery_fixture <- function(name) {
  fixture_path("discovery", name)
}

read_fixture_text <- function(name) {
  path <- discovery_fixture(name)
  base::readChar(path, nchars = base::file.size(path), useBytes = TRUE)
}

read_fixture_raw <- function(name) {
  path <- discovery_fixture(name)
  base::readBin(path, what = "raw", n = base::file.size(path))
}

#' Mock that serves `routes` (url -> list(status, body, type, final_url))
#' and logs every requested URL into `log_env$urls`. Unknown URLs get a 404.
route_mock <- function(routes, log_env = NULL) {
  function(req) {
    if (!base::is.null(log_env)) {
      log_env$urls <- base::c(log_env$urls, req$url)
    }

    route <- routes[[req$url]]

    if (base::is.null(route)) {
      return(httr2::response(
        status_code = 404L,
        url = req$url,
        headers = base::list("content-type" = "text/html"),
        body = base::charToRaw("<html><body>Not found</body></html>")
      ))
    }

    body <- route$body %||% ""

    httr2::response(
      status_code = route$status %||% 200L,
      url = route$final_url %||% req$url,
      headers = base::list("content-type" = route$type %||% "text/plain"),
      body = if (base::is.raw(body)) body else base::charToRaw(body)
    )
  }
}

local_fast_http <- function(env = parent.frame()) {
  withr::local_envvar(HPT_PER_HOST_RPS = "1000", HPT_MAX_ACTIVE = "4", .local_envir = env)
}

# ---- parsing ---------------------------------------------------------------

testthat::test_that("CMS two-location example parses into two entries", {
  parsed <- parse_cms_hpt_text(read_fixture_text("cms_example_two_locations.txt"))

  testthat::expect_equal(base::nrow(parsed), 2L)
  testthat::expect_named(parsed, base::c("location_name", "source_page_url", "mrf_url", "contact_name", "contact_email"))
  testthat::expect_equal(parsed$location_name, base::c("Example Hospital East", "Example Hospital West"))
  testthat::expect_equal(parsed$source_page_url, base::rep("https://example.com/price-transparency", 2L))
  testthat::expect_equal(
    parsed$mrf_url[[2]],
    "https://example.com/price-transparency/987654321_Example-Hospital-West_standardcharges.json"
  )
  testthat::expect_equal(parsed$contact_name, base::c("Jon Snow", "Jane Doe"))
  testthat::expect_equal(parsed$contact_email, base::c("jsnow@example.com", "jdoe@example2.com"))
})

testthat::test_that("CRLF, BOM, uppercase keys, and stray whitespace are tolerated", {
  parsed <- parse_cms_hpt_text(read_fixture_raw("crlf_bom_uppercase.txt"))

  testthat::expect_equal(base::nrow(parsed), 2L)
  testthat::expect_equal(parsed$location_name, base::c("Sample Hospital", "Sample Standalone Emergency Department"))
  testthat::expect_equal(
    parsed$mrf_url,
    base::rep("https://vendor.com/hospital-price-transparency-files/links/101010101_Sample_standardcharges.csv", 2L)
  )
  testthat::expect_equal(parsed$contact_email, base::rep("MRFteam@sample.com", 2L))
  testthat::expect_false(base::any(stringr::str_detect(parsed$location_name, "\ufeff|\r")))
})

testthat::test_that("a new location-name starts an entry even without a blank line", {
  parsed <- parse_cms_hpt_text(read_fixture_text("missing_blank_lines.txt"))

  testthat::expect_equal(parsed$location_name, base::c("North Campus", "South Campus", "Rural Clinic"))
  testthat::expect_equal(parsed$mrf_url[[3]], "https://files.health.example.org/333333333_Rural-Clinic_standardcharges.csv")
  testthat::expect_true(base::is.na(parsed$source_page_url[[3]]))
  testthat::expect_true(base::is.na(parsed$contact_email[[3]]))
})

testthat::test_that("wrapped values, inner blank lines, and a second mrf-url are handled", {
  text <- base::paste(
    "location-name: Sample Hospital",
    "",
    "source-page-url:",
    "https://vendor.com/links/samplehospital.aspx",
    "mrf-url: https://vendor.com/links",
    "/101010101_Sample_standardcharges.csv",
    "mrf-url: https://vendor.com/links/101010101_Sample_standardcharges.json",
    "contact-name: MRF Department",
    "contact-email: MRFteam@sample.com",
    sep = "\n"
  )
  parsed <- parse_cms_hpt_text(text)

  testthat::expect_equal(base::nrow(parsed), 2L)
  testthat::expect_equal(parsed$source_page_url[[1]], "https://vendor.com/links/samplehospital.aspx")
  testthat::expect_equal(parsed$mrf_url, base::c(
    "https://vendor.com/links/101010101_Sample_standardcharges.csv",
    "https://vendor.com/links/101010101_Sample_standardcharges.json"
  ))
  testthat::expect_equal(parsed$location_name, base::rep("Sample Hospital", 2L))
  testthat::expect_equal(parsed$contact_email, base::rep("MRFteam@sample.com", 2L))
})

testthat::test_that("empty or garbage text parses to zero rows", {
  testthat::expect_equal(base::nrow(parse_cms_hpt_text("")), 0L)
  testthat::expect_equal(base::nrow(parse_cms_hpt_text(base::raw(0))), 0L)
  testthat::expect_equal(base::nrow(parse_cms_hpt_text("hello world\nnothing: here")), 0L)
})

testthat::test_that("looks_like_hpt_txt rejects HTML soft 404s", {
  soft_404 <- read_fixture_text("soft_404.html")
  pointer <- read_fixture_text("cms_example_two_locations.txt")

  testthat::expect_true(looks_like_hpt_txt(pointer, "text/plain"))
  testthat::expect_false(looks_like_hpt_txt(soft_404, "text/html"))
  testthat::expect_false(looks_like_hpt_txt(base::paste0("<html><body><pre>", pointer, "</pre></body></html>"), "text/html"))
  testthat::expect_true(looks_like_hpt_txt(pointer, "text/html"))
  testthat::expect_true(looks_like_hpt_txt(read_fixture_raw("crlf_bom_uppercase.txt")))
  testthat::expect_false(looks_like_hpt_txt(""))
})

# ---- fetching --------------------------------------------------------------

testthat::test_that("fetch_hpt_txt_batch classifies outcomes and caches bodies", {
  local_fast_http()
  cache_dir <- withr::local_tempdir()
  pointer <- read_fixture_text("cms_example_two_locations.txt")
  routes <- base::list(
    "https://example.com/cms-hpt.txt" = base::list(body = pointer),
    "https://soft.example.org/cms-hpt.txt" = base::list(body = read_fixture_text("soft_404.html"), type = "text/html"),
    "https://locked.example.org/cms-hpt.txt" = base::list(status = 403L, body = "Forbidden"),
    "https://moved.example.org/cms-hpt.txt" = base::list(body = pointer, final_url = "https://www.moved.example.org/files/cms-hpt.txt"),
    "https://blank.example.org/cms-hpt.txt" = base::list(body = "  \n")
  )
  httr2::local_mocked_responses(route_mock(routes))

  results <- fetch_hpt_txt_batch(
    base::c("https://Example.com/", "soft.example.org", "gone.example.org", "locked.example.org", "moved.example.org", "blank.example.org", "example.com"),
    cache_dir = cache_dir
  )
  status_of <- stats::setNames(results$status, results$domain)

  testthat::expect_equal(base::nrow(results), 6L)
  testthat::expect_equal(base::unname(status_of[["example.com"]]), "ok")
  testthat::expect_equal(base::unname(status_of[["soft.example.org"]]), "soft_404_html")
  testthat::expect_equal(base::unname(status_of[["gone.example.org"]]), "not_found")
  testthat::expect_equal(base::unname(status_of[["locked.example.org"]]), "blocked")
  testthat::expect_equal(base::unname(status_of[["moved.example.org"]]), "ok")
  testthat::expect_equal(base::unname(status_of[["blank.example.org"]]), "empty")
  testthat::expect_equal(results$final_url[results$domain == "moved.example.org"], "https://www.moved.example.org/files/cms-hpt.txt")
  testthat::expect_equal(results$n_locations[results$domain == "example.com"], 2L)
  testthat::expect_equal(results$http_status[results$domain == "locked.example.org"], 403L)
  testthat::expect_true(base::file.exists(base::file.path(cache_dir, "example.com.txt")))
  testthat::expect_false(base::file.exists(base::file.path(cache_dir, "soft.example.org.txt")))

  locations <- collect_hpt_txt_locations(results, cache_dir = cache_dir)
  testthat::expect_named(locations, base::c(
    "domain", "final_url", "location_name", "source_page_url", "mrf_url", "contact_name", "contact_email", "fetched_at"
  ))
  testthat::expect_equal(base::nrow(locations), 4L)
})

testthat::test_that("transport failures are classified from the curl message", {
  dns_error <- base::simpleError("Failed to perform HTTP request. Could not resolve host: nowhere.invalid")
  tls_error <- base::simpleError("SSL certificate problem: certificate has expired")

  testthat::expect_equal(classify_hpt_txt_response(dns_error)$status, "dns_fail")
  testthat::expect_equal(classify_hpt_txt_response(tls_error)$status, "tls_fail")
})

testthat::test_that("crawl_hpt_txt resumes: fresh terminal rows are skipped, stale and transient rows refetched", {
  local_fast_http()
  cache_dir <- withr::local_tempdir()
  state_path <- base::file.path(withr::local_tempdir(), "txt_state.csv")
  log_env <- base::new.env()
  routes <- base::list("https://example.com/cms-hpt.txt" = base::list(body = read_fixture_text("cms_example_two_locations.txt")))
  httr2::local_mocked_responses(route_mock(routes, log_env))

  first <- crawl_hpt_txt(base::c("example.com", "gone.example.org"), state_path = state_path, cache_dir = cache_dir)
  testthat::expect_equal(base::sort(first$status), base::c("not_found", "ok"))
  testthat::expect_equal(base::length(log_env$urls), 2L)

  log_env$urls <- NULL
  crawl_hpt_txt(base::c("example.com", "gone.example.org"), state_path = state_path, cache_dir = cache_dir)
  testthat::expect_length(log_env$urls, 0L)

  state_tbl <- read_hpt_txt_state(state_path)
  state_tbl$fetched_at[state_tbl$domain == "example.com"] <- "2000-01-01T00:00:00Z"
  state_tbl <- dplyr::bind_rows(state_tbl, tibble::tibble(domain = "slow.example.org", status = "timeout", fetched_at = utc_timestamp()))
  write_csv_atomic(state_tbl, state_path)

  log_env$urls <- NULL
  crawl_hpt_txt(base::c("example.com", "gone.example.org", "slow.example.org"), state_path = state_path, cache_dir = cache_dir)
  testthat::expect_setequal(log_env$urls, base::c("https://example.com/cms-hpt.txt", "https://slow.example.org/cms-hpt.txt"))
  testthat::expect_equal(base::nrow(read_hpt_txt_state(state_path)), 3L)
})

testthat::test_that("prefer_apex skips the www variant when the apex already works", {
  local_fast_http()
  log_env <- base::new.env()
  routes <- base::list("https://example.com/cms-hpt.txt" = base::list(body = read_fixture_text("cms_example_two_locations.txt")))
  httr2::local_mocked_responses(route_mock(routes, log_env))

  crawl_hpt_txt(
    base::c("example.com", "www.example.com", "gone.example.org", "www.gone.example.org"),
    state_path = base::file.path(withr::local_tempdir(), "state.csv"),
    cache_dir = withr::local_tempdir()
  )

  testthat::expect_false("https://www.example.com/cms-hpt.txt" %in% log_env$urls)
  testthat::expect_true("https://www.gone.example.org/cms-hpt.txt" %in% log_env$urls)
})

# ---- snowball --------------------------------------------------------------

testthat::test_that("snowball_hosts finds a vendor host named in mrf-url and skips junk and known sites", {
  locations <- parse_cms_hpt_text(read_fixture_text("vendor_pointer.txt"))
  hosts <- snowball_hosts(locations, known_domains = base::c("mvhospital.org"))

  testthat::expect_true("apps.pricevendor.com" %in% hosts)
  testthat::expect_true(base::all(base::c("pricevendor.com", "www.pricevendor.com") %in% hosts))
  testthat::expect_false(base::any(stringr::str_detect(hosts, "blob\\.core\\.windows\\.net")))
  testthat::expect_false(base::any(hosts %in% base::c("mvhospital.org", "www.mvhospital.org")))
})

testthat::test_that("crawl_hpt_txt_snowball crawls hosts discovered in earlier rounds", {
  local_fast_http()
  log_env <- base::new.env()
  vendor_file <- base::paste(
    "location-name: Lakeside Hospital",
    "source-page-url: https://apps.pricevendor.com/lakeside",
    "mrf-url: https://apps.pricevendor.com/mrf/777777777_Lakeside-Hospital_standardcharges.json",
    "contact-name: Vendor Support",
    "contact-email: support@pricevendor.com",
    sep = "\n"
  )
  routes <- base::list(
    "https://mvhospital.org/cms-hpt.txt" = base::list(body = read_fixture_text("vendor_pointer.txt")),
    "https://apps.pricevendor.com/cms-hpt.txt" = base::list(body = vendor_file)
  )
  httr2::local_mocked_responses(route_mock(routes, log_env))

  result <- crawl_hpt_txt_snowball(
    "mvhospital.org",
    state_path = base::file.path(withr::local_tempdir(), "state.csv"),
    cache_dir = withr::local_tempdir(),
    max_rounds = 2L
  )

  testthat::expect_true("apps.pricevendor.com" %in% result$snowball$host)
  testthat::expect_equal(result$snowball$found_via[result$snowball$host == "apps.pricevendor.com"], "mvhospital.org")
  testthat::expect_true("Lakeside Hospital" %in% result$locations$location_name)
  testthat::expect_equal(base::max(result$state$round), 1L)
})

# ---- domain seeds ----------------------------------------------------------

testthat::test_that("domain_variants adds apex, www, and registrable parent forms", {
  testthat::expect_setequal(domain_variants("https://www.Hospital.org/path?x=1"), base::c("www.hospital.org", "hospital.org"))
  testthat::expect_setequal(domain_variants("hospital.org"), base::c("hospital.org", "www.hospital.org"))
  testthat::expect_setequal(
    domain_variants("apps.pricevendor.com"),
    base::c("apps.pricevendor.com", "pricevendor.com", "www.pricevendor.com")
  )
  testthat::expect_equal(domain_variants("mrfprices.azurewebsites.net"), "mrfprices.azurewebsites.net")
  testthat::expect_equal(domain_variants(base::c(NA, "", "not a domain")), base::character(0))
})

testthat::test_that("junk hosts are dropped but vendor hosts are kept", {
  junk <- base::c(
    "docs.google.com", "drive.google.com", "www.dropbox.com", "github.com", "s3.amazonaws.com",
    "mybucket.s3.us-east-1.amazonaws.com", "use2webtechstgpricelist.blob.core.windows.net",
    "d1234abcd.cloudfront.net", "www.cms.gov", "10.0.0.1", "localhost"
  )
  kept <- base::c("apps.para-hcfs.com", "hospital.org", "www.southeasthealth.org", "mrfprices.azurewebsites.net")

  testthat::expect_true(base::all(is_junk_hpt_host(junk)))
  testthat::expect_false(base::any(is_junk_hpt_host(kept)))
})

testthat::test_that("TPAFS seeding keeps all three hosts per CCN", {
  links <- tibble::tibble(
    ccn = base::c("10001", "390231"),
    machine_readable_page = base::c("https://www.southeasthealth.org/price-transparency/", "https://www.abingtonhealth.org/app/login.aspx"),
    supplemental_url = base::c("https://apps.para-hcfs.com/PTT/FinalLinks/Southeast.aspx", NA),
    machine_readable_url = base::c(
      "https://files.southeasthealth.org/636004476_Southeast_standardcharges.csv",
      "https://use2webtechstgpricelist.blob.core.windows.net/pricelist/23-1352152_standardcharges.csv"
    )
  )
  seeds <- build_domain_seeds(ccns = base::c("010001", "390231"), tpafs_links = links)

  southeast <- seeds |> dplyr::filter(.data$ccn == "010001")
  testthat::expect_setequal(base::unique(southeast$seed_source), base::c("tpafs_page", "tpafs_supplemental", "tpafs_mrf"))
  testthat::expect_true(base::all(
    base::c("www.southeasthealth.org", "southeasthealth.org", "apps.para-hcfs.com", "para-hcfs.com", "files.southeasthealth.org") %in% southeast$domain
  ))
  testthat::expect_false(base::any(stringr::str_detect(seeds$domain, "windows\\.net")))
  testthat::expect_true(base::all(base::c("www.abingtonhealth.org", "abingtonhealth.org") %in% seeds$domain[seeds$ccn == "390231"]))
  testthat::expect_equal(base::nrow(seeds), base::nrow(dplyr::distinct(seeds, .data$ccn, .data$domain, .data$seed_source)))
  testthat::expect_false(seeds$is_variant[seeds$ccn == "010001" & seeds$domain == "files.southeasthealth.org"][[1]])
})

testthat::test_that("tracker manifest and gaps seed domains and filter to the requested CCNs", {
  manifest <- tibble::tibble(
    ccn = "010001", domain = "southeasthealth.org", pointer_url = "https://southeasthealth.org/cms-hpt.txt",
    mrf_url = "https://www.southeasthealth.org/wp-content/uploads/636004476_standardcharges.csv",
    source_page_url = "https://www.southeasthealth.org/financial-information-price-transparency/"
  )
  gaps <- tibble::tibble(
    ccn = base::c("010019", "010024"), seeded_domain = base::c("huntsvillehospital.org", NA),
    resolved_domain = base::c("helenkellerhospital.org", NA)
  )
  seeds <- build_domain_seeds(ccns = base::c("010001", "010019"), tracker_manifest = manifest, tracker_gaps = gaps)

  testthat::expect_setequal(base::unique(seeds$ccn), base::c("010001", "010019"))
  testthat::expect_true(base::all(base::c("huntsvillehospital.org", "helenkellerhospital.org") %in% seeds$domain))
  testthat::expect_true("tracker_gaps_resolved" %in% seeds$seed_source)
})

testthat::test_that("HIFLD rows match CCNs only when name and location agree", {
  roster <- tibble::tibble(
    facility_id = base::c("050001", "050002", "050003"),
    facility_name = base::c("CENTRAL VALLEY GENERAL HOSPITAL", "ST MARY MEDICAL CENTER", "RIVERSIDE COMMUNITY HOSPITAL"),
    address = base::c("1025 NORTH DOUTY STREET", "18300 US HIGHWAY 18", "4445 MAGNOLIA AVE"),
    citytown = base::c("HANFORD", "APPLE VALLEY", "RIVERSIDE"),
    state = base::c("CA", "CA", "CA"),
    zip_code = base::c("93230", "92307", "92501")
  )
  hifld <- tibble::tibble(
    NAME = base::c("CENTRAL VALLEY GENERAL HOSPITAL", "ST. MARY MEDICAL CENTER", "KAISER RIVERSIDE", "RIVERSIDE COMMUNITY HOSPITAL"),
    ADDRESS = base::c("1025 N DOUTY ST", "18300 HIGHWAY 18", "10800 MAGNOLIA AVE", "9999 OTHER ST"),
    CITY = base::c("HANFORD", "APPLE VALLEY", "RIVERSIDE", "SACRAMENTO"),
    STATE = base::c("CA", "CA", "CA", "CA"),
    ZIP = base::c("93230", "92307", "92505", "95814"),
    WEBSITE = base::c("http://www.hanfordhealth.com", "https://www.stmaryapplevalley.org", "https://kp.org", "https://www.rchc.org")
  )
  matched <- match_hifld_to_roster(hifld, roster)

  testthat::expect_setequal(matched$ccn, base::c("050001", "050002"))
  testthat::expect_true(base::all(matched$match_score >= 0.6))

  seeds <- build_domain_seeds(ccns = roster$facility_id, hifld_tbl = hifld, roster_tbl = roster)
  testthat::expect_true(base::all(seeds$seed_source == "hifld_website"))
  testthat::expect_true(base::all(base::c("www.hanfordhealth.com", "hanfordhealth.com") %in% seeds$domain))
  testthat::expect_false("rchc.org" %in% seeds$domain)
})

# ---- footer fallback -------------------------------------------------------

testthat::test_that("footer Price Transparency link is found first and MRF links are extracted", {
  homepage <- xml2::read_html(discovery_fixture("homepage_footer.html"))
  links <- extract_price_transparency_links(homepage, "https://www.mvhospital.org/")

  testthat::expect_equal(links$url[[1]], "https://www.mvhospital.org/billing/price-transparency/")
  testthat::expect_true(links$in_footer[[1]])
  testthat::expect_equal(links$link_text[[1]], "Price Transparency")
  testthat::expect_false(links$in_footer[[2]])

  price_page <- xml2::read_html(discovery_fixture("price_page.html"))
  mrfs <- extract_mrf_links(price_page, "https://www.mvhospital.org/billing/price-transparency/")

  testthat::expect_setequal(mrfs$mrf_url, base::c(
    "https://www.mvhospital.org/files/555555555_Mountain-View-Hospital_standardcharges.json",
    "https://cdn.mvhospital.org/files/555555555_Mountain-View-Hospital_standardcharges.csv?v=3"
  ))
  testthat::expect_true(base::all(mrfs$matches_cms_naming))
})

testthat::test_that("discover_via_footer follows the footer link to the MRF links", {
  local_fast_http()
  reset_robots_cache()
  withr::defer(reset_robots_cache())
  routes <- base::list(
    "https://mvhospital.org/robots.txt" = base::list(body = "User-agent: *\nDisallow: /admin/\n"),
    "https://mvhospital.org/" = base::list(
      body = read_fixture_text("homepage_footer.html"), type = "text/html; charset=utf-8",
      final_url = "https://www.mvhospital.org/"
    ),
    "https://www.mvhospital.org/robots.txt" = base::list(body = "User-agent: *\nDisallow: /admin/\n"),
    "https://www.mvhospital.org/billing/price-transparency/" = base::list(body = read_fixture_text("price_page.html"), type = "text/html"),
    "https://www.mvhospital.org/blog/price-transparency-explained" = base::list(body = "<html><body><p>Blog</p></body></html>", type = "text/html")
  )
  httr2::local_mocked_responses(route_mock(routes))

  found <- discover_via_footer("mvhospital.org")

  testthat::expect_equal(base::unique(found$status), "ok")
  testthat::expect_equal(base::nrow(found), 2L)
  testthat::expect_equal(base::unique(found$price_page_url), "https://www.mvhospital.org/billing/price-transparency/")
  testthat::expect_equal(base::unique(found$link_text), "Price Transparency")
  testthat::expect_equal(base::unique(found$homepage_url), "https://www.mvhospital.org/")
  testthat::expect_true(base::all(found$in_footer))
})

testthat::test_that("robots.txt Disallow is honored for the homepage and for price pages", {
  local_fast_http()
  reset_robots_cache()
  withr::defer(reset_robots_cache())
  log_env <- base::new.env()
  routes <- base::list(
    "https://closed.example.org/robots.txt" = base::list(body = "User-agent: *\nDisallow: /\n"),
    "https://closed.example.org/" = base::list(body = read_fixture_text("homepage_footer.html"), type = "text/html"),
    "https://partial.example.org/robots.txt" = base::list(body = base::paste(
      "User-agent: *", "Disallow: /", "", "User-agent: hpt-prices-research", "Disallow: /billing/", sep = "\n"
    )),
    "https://partial.example.org/" = base::list(body = read_fixture_text("homepage_footer.html"), type = "text/html"),
    "https://partial.example.org/blog/price-transparency-explained" = base::list(body = "<html><body></body></html>", type = "text/html")
  )
  httr2::local_mocked_responses(route_mock(routes, log_env))

  found <- discover_via_footer_batch(base::c("closed.example.org", "partial.example.org"))
  status_of <- stats::setNames(found$status, found$domain)

  testthat::expect_equal(base::unname(status_of[["closed.example.org"]]), "robots_disallowed")
  testthat::expect_false("https://closed.example.org/" %in% log_env$urls)
  testthat::expect_true("https://partial.example.org/" %in% log_env$urls)
  testthat::expect_false("https://partial.example.org/billing/price-transparency/" %in% log_env$urls)
  testthat::expect_equal(base::unname(status_of[["partial.example.org"]]), "no_mrf_link")
})

testthat::test_that("robots rules: own agent group wins, longest match wins, wildcards work", {
  rules <- parse_robots_txt(base::paste(
    "User-agent: *",
    "Disallow: /",
    "",
    "User-agent: hpt-prices-research",
    "Disallow: /private/",
    "Allow: /private/prices/",
    "Disallow: /*.pdf$",
    sep = "\n"
  ))

  testthat::expect_true(robots_path_allowed(rules, "/"))
  testthat::expect_false(robots_path_allowed(rules, "/private/staff"))
  testthat::expect_true(robots_path_allowed(rules, "/private/prices/list.html"))
  testthat::expect_false(robots_path_allowed(rules, "/files/chargemaster.pdf"))
  testthat::expect_true(robots_path_allowed(rules, "/files/chargemaster.pdf?download=1"))

  star_only <- parse_robots_txt("User-agent: *\nDisallow: /billing/\n\nUser-agent: Googlebot\nDisallow: /\n")
  testthat::expect_false(robots_path_allowed(star_only, "/billing/price-transparency/"))
  testthat::expect_true(robots_path_allowed(star_only, "/"))
  testthat::expect_true(robots_path_allowed(parse_robots_txt(""), "/anything"))
})
