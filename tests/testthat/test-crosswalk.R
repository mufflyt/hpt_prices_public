#' Hospital universe, tracker bridge, and CCN matching. Fixtures under
#' fixtures/crosswalk/ carry the verbatim headers of the live CMS, tracker,
#' and AHRQ files (captured 2026-09-12), with synthetic rows. Anything that
#' touches the network runs against httr2 mocked responses.

crosswalk_fixture <- function(...) {
  fixture_path("crosswalk", ...)
}

crosswalk_inputs <- function() {
  roster <- load_cms_hospital_frame(crosswalk_fixture("hospital_general_information.csv"))
  chsp <- load_chsp_linkage(crosswalk_fixture("chsp_hospital_linkage.csv"))
  enrollment <- load_hospital_enrollments(
    crosswalk_fixture("hospital_enrollments.csv"),
    crosswalk_fixture("hospital_additional_npis.csv")
  )

  base::list(
    universe = base::suppressMessages(build_hospital_universe(roster, chsp)),
    npi_xwalk = npi_ccn_crosswalk(enrollment$enrollments, enrollment$additional_npis),
    manifest = load_tracker(crosswalk_fixture())$manifest
  )
}

facility_record <- function(facility_key, mrf_url = NA_character_, hospital_name = NA_character_,
                            location_name = NA_character_, address = NA_character_,
                            city = NA_character_, state = NA_character_,
                            type_2_npi = NA_character_) {
  tibble::tibble(
    facility_key = facility_key,
    source = "own_crawl",
    mrf_url = mrf_url,
    hospital_name = hospital_name,
    location_name = location_name,
    address = address,
    city = city,
    state = state,
    license_number = NA_character_,
    license_state = NA_character_,
    type_2_npi = type_2_npi
  )
}

run_match <- function(facilities) {
  inputs <- crosswalk_inputs()

  base::suppressMessages(
    match_facilities_to_ccn(facilities, inputs$manifest, inputs$npi_xwalk, inputs$universe)
  )
}

#' Route mocked requests by exact URL; anything unrouted is a 404
mock_router <- function(routes) {
  function(req) {
    body <- routes[[req$url]]

    if (base::is.null(body)) {
      return(httr2::response(status_code = 404L, url = req$url, body = base::charToRaw("not found")))
    }

    content_type <- if (stringr::str_detect(req$url, "\\.csv$")) "text/csv" else "application/json"

    httr2::response(
      status_code = 200L,
      url = req$url,
      headers = base::list("Content-Type" = content_type),
      body = if (base::is.raw(body)) body else base::charToRaw(body)
    )
  }
}

fixture_bytes <- function(...) {
  path <- crosswalk_fixture(...)
  base::readBin(path, "raw", base::file.size(path))
}

system_mrf_url <- "https://mrf.examplehealth.org/84-0000000_example-health_standardcharges.json"
trinity_mrf_url <- "https://hpt.trinity-health.org/043398280_the-mercy-hospital-inc_standardcharges.json"

testthat::test_that("CMS City/Town header normalizes to citytown and CCNs keep leading zeros", {
  roster <- load_cms_hospital_frame(crosswalk_fixture("hospital_general_information.csv"))

  testthat::expect_true(base::all(base::c("facility_id", "citytown", "zip_code", "hospital_type") %in% base::names(roster)))
  testthat::expect_false("city_town" %in% base::names(roster))
  testthat::expect_equal(roster$facility_id[[1]], "060011")
  testthat::expect_equal(roster$zip_code[roster$facility_id == "220066"], "01104")
})

testthat::test_that("normalize_ccn restores stripped zeros and drops location suffixes", {
  testthat::expect_equal(
    normalize_ccn(base::c("60011", "060011", "22007401", "220083A", "330125001", "7003301", "01014F", "06S011", " 450097 ", NA, "")),
    base::c("060011", "060011", "220074", "220083", "330125", "070033", "01014F", "06S011", "450097", NA, NA)
  )
  testthat::expect_equal(
    unit_parent_ccn(base::c("06S011", "04Z304", "45U097", "17M320", "050W12", "060011", "01014F")),
    base::c("060011", "041304", "450097", "171320", "050W12", "060011", "01014F")
  )
})

testthat::test_that("download_cms_hospital_frame saves the roster atomically with provenance", {
  withr::local_envvar(HPT_PER_HOST_RPS = "1000")
  csv_url <- "https://data.cms.gov/provider-data/sites/default/files/resources/abc/Hospital_General_Information.csv"
  metadata <- jsonlite::toJSON(
    base::list(
      title = "Hospital General Information",
      modified = "2026-07-22",
      distribution = base::list(base::list(mediaType = "text/csv", downloadURL = csv_url))
    ),
    auto_unbox = TRUE
  )
  routes <- base::list()
  routes[["https://data.cms.gov/provider-data/api/1/metastore/schemas/dataset/items/xubh-q36u"]] <- base::as.character(metadata)
  routes[[csv_url]] <- fixture_bytes("hospital_general_information.csv")
  httr2::local_mocked_responses(mock_router(routes))

  directory <- withr::local_tempdir()
  roster <- base::suppressMessages(download_cms_hospital_frame(directory))

  testthat::expect_equal(base::nrow(roster), 8L)
  testthat::expect_true("citytown" %in% base::names(roster))

  provenance <- read_csv_chr(base::list.files(directory, "provenance", full.names = TRUE))
  testthat::expect_equal(provenance$modified, "2026-07-22")
  testthat::expect_equal(provenance$sha256, base::as.vector(sha256_file(provenance$local_path)))
  testthat::expect_length(base::list.files(directory, "\\.part$"), 0L)
})

testthat::test_that("download_public_file never leaves a partial file behind", {
  withr::local_envvar(HPT_PER_HOST_RPS = "1000")
  httr2::local_mocked_responses(mock_router(base::list()))
  destination <- base::file.path(withr::local_tempdir(), "missing.csv")

  testthat::expect_error(
    base::suppressMessages(download_public_file("https://example.org/missing.csv", destination)),
    "HTTP 404"
  )
  testthat::expect_false(base::file.exists(destination))
  testthat::expect_false(base::file.exists(base::paste0(destination, ".part")))
})

testthat::test_that("Hospital Enrollments release is resolved through the catalog, not a fixed UUID", {
  withr::local_envvar(HPT_PER_HOST_RPS = "1000")
  enrollment_url <- "https://data.cms.gov/sites/default/files/2026-08/aug/Hospital_Enrollments_2026.07.31.csv"
  additional_url <- "https://data.cms.gov/sites/default/files/2026-08/Hospital_Additional_NPIs_2026.07.31.csv"
  resources_url <- "https://data.cms.gov/data-api/v1/dataset-resources/aug-uuid"
  catalog <- base::list(dataset = base::list(
    base::list(title = "Hospice Enrollments", distribution = base::list()),
    base::list(title = "Hospital Enrollments", distribution = base::list(
      base::list(format = "API", description = "latest", title = "Hospital Enrollments : 2026-08-01",
                 temporal = "2026-08-01/2026-08-31", modified = "2026-08-17",
                 resourcesAPI = "https://data.cms.gov/data-api/v1/dataset-resources/latest-uuid"),
      base::list(format = "CSV", mediaType = "text/csv", title = "Hospital Enrollments : 2026-08-01",
                 temporal = "2026-08-01/2026-08-31", modified = "2026-08-17",
                 downloadURL = enrollment_url, resourcesAPI = resources_url),
      base::list(format = "CSV", mediaType = "text/csv", title = "Hospital Enrollments : 2026-07-01",
                 temporal = "2026-07-01/2026-07-31", modified = "2026-07-17",
                 downloadURL = "https://data.cms.gov/sites/default/files/2026-07/jul/Hospital_Enrollments_2026.07.17.csv",
                 resourcesAPI = "https://data.cms.gov/data-api/v1/dataset-resources/jul-uuid")
    ))
  ))
  resources <- base::list(data = base::list(
    base::list(name = "Hospital Enrollments Aug 2026", fileSize = 100, downloadURL = enrollment_url),
    base::list(name = "Hospital Enrollments Data Dictionary (Current)", fileSize = 100,
               downloadURL = "https://data.cms.gov/sites/default/files/2026-07/Hospital_Enrollments_Data_Dictionary.pdf"),
    base::list(name = "Hospital Additional NPIs Aug 2026", fileSize = 100, downloadURL = additional_url)
  ))
  routes <- base::list()
  routes[["https://data.cms.gov/data.json"]] <- base::as.character(jsonlite::toJSON(catalog, auto_unbox = TRUE))
  routes[[resources_url]] <- base::as.character(jsonlite::toJSON(resources, auto_unbox = TRUE))
  routes[[enrollment_url]] <- fixture_bytes("hospital_enrollments.csv")
  routes[[additional_url]] <- fixture_bytes("hospital_additional_npis.csv")
  httr2::local_mocked_responses(mock_router(routes))

  directory <- withr::local_tempdir()
  enrollment <- base::suppressMessages(download_hospital_enrollments(directory))

  testthat::expect_equal(enrollment$provenance$file_role, base::c("enrollments", "additional_npis"))
  testthat::expect_equal(enrollment$provenance$download_url, base::c(enrollment_url, additional_url))
  testthat::expect_equal(enrollment$provenance$temporal[[1]], "2026-08-01/2026-08-31")
  testthat::expect_true(base::all(base::c("doing_business_as_name", "practice_location_type", "cah_or_hospital_ccn") %in% base::names(enrollment$enrollments)))
  testthat::expect_equal(base::names(enrollment$additional_npis), base::c("enrollment_id", "npi"))
})

testthat::test_that("NPI crosswalk collapses units to the parent CCN and adds additional NPIs", {
  xwalk <- crosswalk_inputs()$npi_xwalk

  denver <- dplyr::filter(xwalk, .data$ccn == "060011")
  testthat::expect_setequal(denver$npi, base::c("1689624686", "1000000009"))
  testthat::expect_equal(denver$enrollment_ccns[denver$npi == "1689624686"], "060011;06S011")
  testthat::expect_equal(denver$source[denver$npi == "1000000009"], "additional_npi")
  testthat::expect_equal(xwalk$ccn[xwalk$npi == "1174576698"], "450097")
  testthat::expect_equal(xwalk$ccn[xwalk$npi == "1000000001"], "220066")
  testthat::expect_true(base::all(base::nchar(xwalk$ccn) == 6L))
  testthat::expect_equal(base::anyDuplicated(xwalk[base::c("npi", "ccn")]), 0L)
})

testthat::test_that("hospital universe joins CHSP systems and keeps every roster CCN", {
  universe <- crosswalk_inputs()$universe

  testthat::expect_equal(base::nrow(universe), 8L)
  testthat::expect_equal(universe$health_sys_name[universe$facility_id == "450097"], "HCA Healthcare")
  testthat::expect_true(base::is.na(universe$health_sys_id[universe$facility_id == "060023"]))
  testthat::expect_equal(base::sum(universe$chsp_linked), 3L)
})

testthat::test_that("download_tracker_snapshot pins files to the resolved commit SHA", {
  withr::local_envvar(HPT_PER_HOST_RPS = "1000", GITHUB_TOKEN = "")
  sha <- "27c08db25896d765845f9ff6827da7f96ee66b04"
  routes <- base::list()
  routes[["https://api.github.com/repos/anthonyisnotadev/cms-hpt-tracker/commits/main"]] <- base::paste0('{"sha": "', sha, '"}')

  for (file in tracker_files()) {
    routes[[tracker_file_url(sha, file)]] <- fixture_bytes(file)
  }

  httr2::local_mocked_responses(mock_router(routes))

  snapshot_dir <- base::suppressMessages(download_tracker_snapshot(withr::local_tempdir()))
  provenance <- read_csv_chr(base::file.path(snapshot_dir, "tracker_provenance.csv"))
  tracker <- load_tracker(snapshot_dir)

  testthat::expect_equal(base::basename(snapshot_dir), sha)
  testthat::expect_equal(provenance$file, tracker_files())
  testthat::expect_true(base::all(provenance$sha == sha))
  testthat::expect_equal(provenance$sha256[[1]], base::as.vector(sha256_file(base::file.path(snapshot_dir, "manifest.csv"))))
  testthat::expect_equal(tracker$manifest$ccn[[1]], "060011")
  testthat::expect_true("02013F" %in% tracker$compliance$ccn)
})

testthat::test_that("tracker URL bridge ignores scheme, www, query, and matches extra MRF URLs", {
  testthat::expect_equal(
    parse_extra_mrf_urls(base::c(NA, "", "https://a.org/1.csv;https://a.org/2.csv", "[\"https://a.org/x,y.csv\",\"https://a.org/3.csv\"]")),
    base::list(base::character(), base::character(), base::c("https://a.org/1.csv", "https://a.org/2.csv"), base::c("https://a.org/x,y.csv", "https://a.org/3.csv"))
  )

  crosswalk <- run_match(dplyr::bind_rows(
    facility_record("denver", mrf_url = "http://denverhealth.org/-/media/files/84-6000613_denver-health-medical-center_standardcharges.csv", hospital_name = "Denver Health", state = "CO"),
    facility_record("pslmc_part2", mrf_url = "https://www.healthonecares.com/mrf/pslmc_part2.csv?sig=abc", hospital_name = "Presbyterian/St. Luke's")
  ))

  testthat::expect_equal(crosswalk$ccn, base::c("060011", "060014"))
  testthat::expect_equal(crosswalk$ccn_match_method, base::c("mrf_url", "mrf_url"))
  testthat::expect_equal(crosswalk$tracker_match_method, base::c("name", "exact-mrf-url-corpus"))
  testthat::expect_false(base::any(crosswalk$ccn_ambiguous))
})

testthat::test_that("system-wide MRF is disambiguated by name, state narrows candidates, and ties stay ambiguous", {
  crosswalk <- run_match(dplyr::bind_rows(
    facility_record("by_name", mrf_url = system_mrf_url, hospital_name = "Example Health", location_name = "Presbyterian St. Luke's Medical Center", city = "Denver", state = "CO"),
    facility_record("tied", mrf_url = system_mrf_url, hospital_name = "Example Health", city = "Denver", state = "CO"),
    facility_record("by_state", mrf_url = trinity_mrf_url, hospital_name = "Mercy Hospital", state = "MA")
  ))

  by_name <- dplyr::filter(crosswalk, .data$facility_key == "by_name")
  testthat::expect_equal(by_name$ccn, "060014")
  testthat::expect_false(by_name$ccn_ambiguous)
  testthat::expect_gt(by_name$ccn_match_score, 0.9)

  tied <- dplyr::filter(crosswalk, .data$facility_key == "tied")
  testthat::expect_setequal(tied$ccn, base::c("060011", "060014"))
  testthat::expect_true(base::all(tied$ccn_ambiguous))
  testthat::expect_true(base::all(tied$ccn_match_method == "mrf_url"))

  testthat::expect_equal(crosswalk$ccn[crosswalk$facility_key == "by_state"], "220066")
})

testthat::test_that("NPI tier reads type_2_npi in every format", {
  crosswalk <- run_match(dplyr::bind_rows(
    facility_record("hca", hospital_name = "HCA Houston Healthcare Southeast", state = "TX", type_2_npi = "[1174576698]"),
    facility_record("denver_two_npis", hospital_name = "Denver Health", type_2_npi = "1000000009; 1689624686"),
    facility_record("empty", type_2_npi = "[]")
  ))

  testthat::expect_equal(crosswalk$ccn, base::c("450097", "060011", NA))
  testthat::expect_equal(crosswalk$ccn_match_method, base::c("npi", "npi", NA))
  testthat::expect_equal(crosswalk$ccn_match_score[[1]], 1)
  testthat::expect_true(base::is.na(crosswalk$ccn_match_score[[3]]))
})

testthat::test_that("URL CCN that disagrees with the NPI CCN is kept but flagged", {
  crosswalk <- run_match(dplyr::bind_rows(
    facility_record("conflict", mrf_url = "https://healthonecares.com/mrf/pslmc_standardcharges.json", hospital_name = "Presbyterian St Lukes", type_2_npi = "1689624686"),
    facility_record("agree", mrf_url = "https://www.denverhealth.org/-/media/files/84-6000613_denver-health-medical-center_standardcharges.csv", hospital_name = "Denver Health", type_2_npi = "[1689624686]"),
    facility_record("tie_break", mrf_url = system_mrf_url, hospital_name = "Example Health", type_2_npi = "1720038946")
  ))

  conflict <- dplyr::filter(crosswalk, .data$facility_key == "conflict")
  testthat::expect_equal(conflict$ccn, "060014")
  testthat::expect_true(conflict$ccn_conflict)
  testthat::expect_equal(conflict$npi_ccns, "060011")

  testthat::expect_false(crosswalk$ccn_conflict[crosswalk$facility_key == "agree"])

  tie_break <- dplyr::filter(crosswalk, .data$facility_key == "tie_break")
  testthat::expect_equal(tie_break$ccn, "060014")
  testthat::expect_equal(tie_break$ccn_match_method, "mrf_url+npi")
  testthat::expect_false(tie_break$ccn_conflict)
})

testthat::test_that("name_address tier accepts a clear match and rejects an ambiguous one", {
  crosswalk <- run_match(dplyr::bind_rows(
    facility_record("clear", hospital_name = "Denver Health Medical Center", address = "777 Bannock St, Denver, CO 80204", city = "Denver", state = "CO"),
    facility_record("ambiguous", hospital_name = "St. Mary's Medical Center", state = "CO"),
    facility_record("city_breaks_tie", hospital_name = "St. Mary's Medical Center", city = "Pueblo", state = "CO"),
    facility_record("wrong_state", hospital_name = "Denver Health Medical Center", state = "TX"),
    facility_record("one_word_name", hospital_name = "Mercy Hospital", city = "Moundridge", state = "KS")
  ))

  testthat::expect_equal(crosswalk$ccn, base::c("060011", NA, "060999", NA, NA))
  testthat::expect_equal(crosswalk$ccn_match_method, base::c("name_address", NA, "name_address", NA, NA))
  testthat::expect_gt(crosswalk$ccn_match_score[[1]], 0.9)
  testthat::expect_true(base::all(base::nchar(stats::na.omit(crosswalk$ccn)) == 6L))
})

testthat::test_that("NULL tracker or NPI crosswalk skips that tier; minimal tables work", {
  inputs <- crosswalk_inputs()
  facilities <- facility_record(
    "denver",
    mrf_url = "https://www.denverhealth.org/-/media/files/84-6000613_denver-health-medical-center_standardcharges.csv",
    hospital_name = "Denver Health Medical Center",
    address = "777 Bannock St",
    state = "CO",
    type_2_npi = "[1689624686]"
  )
  run <- function(...) base::suppressMessages(match_facilities_to_ccn(facilities, ..., universe = inputs$universe))

  neither <- run(tracker_manifest = NULL, npi_xwalk = NULL)
  testthat::expect_equal(neither$ccn, "060011")
  testthat::expect_equal(neither$ccn_match_method, "name_address")
  testthat::expect_true(base::is.na(neither$npi_ccns))

  npi_only <- run(tracker_manifest = NULL, npi_xwalk = inputs$npi_xwalk)
  testthat::expect_equal(npi_only$ccn_match_method, "npi")

  url_only <- run(tracker_manifest = inputs$manifest, npi_xwalk = NULL)
  testthat::expect_equal(url_only$ccn_match_method, "mrf_url")
  testthat::expect_false(url_only$ccn_conflict)

  minimal <- run(
    tracker_manifest = dplyr::select(inputs$manifest, "ccn", "mrf_url"),
    npi_xwalk = tibble::tibble(npi = "1689624686", ccn = "060014")
  )
  testthat::expect_equal(minimal$ccn, "060011")
  testthat::expect_true(minimal$ccn_conflict)
  testthat::expect_equal(minimal$npi_ccns, "060014")

  testthat::expect_error(
    run(tracker_manifest = NULL, npi_xwalk = tibble::tibble(npi = "1689624686")),
    "NPI crosswalk is missing: ccn"
  )
})

testthat::test_that("coverage_report counts covered CCNs by state, type, and system", {
  universe <- crosswalk_inputs()$universe
  crosswalk <- tibble::tibble(
    facility_key = base::c("a", "b", "c", "c", "d", "e"),
    ccn = base::c("060011", "060011", "060014", "060023", NA, "999999"),
    ccn_ambiguous = base::c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE)
  )

  report <- coverage_report(crosswalk, universe)

  testthat::expect_equal(report$overall$n_ccn, 8L)
  testthat::expect_equal(report$overall$n_covered, 1L)
  testthat::expect_equal(report$overall$n_covered_incl_ambiguous, 3L)
  testthat::expect_equal(report$overall$n_ccns_outside_universe, 1L)

  colorado <- dplyr::filter(report$by_state, .data$state == "CO")
  testthat::expect_equal(base::c(colorado$n_ccn, colorado$n_covered, colorado$n_covered_incl_ambiguous), base::c(4L, 1L, 3L))

  acute <- dplyr::filter(report$by_hospital_type, .data$hospital_type == "Acute Care Hospitals")
  testthat::expect_equal(base::c(acute$n_ccn, acute$n_covered), base::c(6L, 1L))

  hca <- dplyr::filter(report$by_health_system, .data$health_sys_name %in% "HCA Healthcare")
  testthat::expect_equal(base::c(hca$n_ccn, hca$n_covered, hca$n_covered_incl_ambiguous), base::c(2L, 0L, 1L))
})

# ---- name, address and licence normalization ---------------------------------------

testthat::test_that("hospital names normalize and drop the words every hospital shares", {
  testthat::expect_equal(normalize_hospital_name("St. Mary's Hospital & Medical Center"), "ST MARY S HOSPITAL MEDICAL CENTER")
  # the stop words are what make two unrelated hospitals look alike
  testthat::expect_equal(hospital_name_tokens("The Medical Center of Aurora"), base::c("AURORA"))
  testthat::expect_setequal(hospital_name_tokens("Denver Health Medical Center"), base::c("DENVER"))
  # an empty or missing name yields no tokens, so two unknown names never match
  testthat::expect_length(hospital_name_tokens(NA_character_), 0)
  testthat::expect_length(hospital_name_tokens("   "), 0)
})

testthat::test_that("street addresses and cities normalize to their abbreviations", {
  testthat::expect_equal(normalize_street_address("777 Bannock Street"), "777 BANNOCK ST")
  testthat::expect_equal(normalize_street_address("1234 North Washington Avenue, Suite 200"),
                         "1234 N WASHINGTON AVE STE 200")
  testthat::expect_equal(normalize_city("Saint Louis"), "ST LOUIS")
  testthat::expect_equal(normalize_city("Fort Collins"), "FT COLLINS")
  testthat::expect_equal(normalize_city("Mount Pleasant"), "MT PLEASANT")
})

testthat::test_that("similarity reweights around whatever the file actually carries", {
  # all three present: 0.5 name, 0.35 address, 0.15 city
  testthat::expect_equal(combine_similarity(1, 1, 1), 1)
  testthat::expect_equal(combine_similarity(1, 0, 0), 0.5)
  # a missing address leaves the denominator, so a perfect name and city is 1,
  # not 0.65 -- a file without an address must not be penalised for it
  testthat::expect_equal(combine_similarity(1, NA, 1), 1)
  testthat::expect_equal(combine_similarity(1, NA, 0), 0.5 / 0.65)
  testthat::expect_equal(combine_similarity(0.8, NA, NA), 0.8)
})

testthat::test_that("NPI lists and licence keys are read the way the sources write them", {
  testthat::expect_equal(parse_npi_list("1234567890;9876543210")[[1]], base::c("1234567890", "9876543210"))
  testthat::expect_equal(parse_npi_list("NPI 1234567890 (primary)")[[1]], "1234567890")
  # 9 and 11 digit runs are not NPIs and must not be half-matched
  testthat::expect_length(parse_npi_list("123456789")[[1]], 0)
  testthat::expect_length(parse_npi_list("12345678901")[[1]], 0)
  testthat::expect_length(parse_npi_list(NA)[[1]], 0)

  testthat::expect_equal(normalize_license_key(base::c("010-444", "0010444", "10444")), base::rep("10444", 3))
  testthat::expect_true(base::is.na(normalize_license_key("")))
  testthat::expect_true(base::is.na(normalize_license_key(NA_character_)))
})

# ---- blocking before name scoring ---------------------------------------------------

testthat::test_that("ZIPs are read from a roster column or the tail of an address", {
  testthat::expect_equal(zip_key(base::c("80204", "80204-1234")), base::c("80204", "80204"))
  testthat::expect_equal(zip_key("777 Bannock St, Denver, CO 80204"), "80204")
  testthat::expect_equal(zip_key("1 Main St, Town, CO 80204-1234"), "80204")
  # a five-digit run that is not at the end is a street number, not a ZIP
  testthat::expect_true(base::is.na(zip_key("12345 Sunset Boulevard, Los Angeles, CA")))
  testthat::expect_true(base::is.na(zip_key(base::c("no zip", NA))[[1]]))
})

testthat::test_that("candidates narrow to the ZIP, then the city, then the state", {
  profiles <- base::list(
    profile = tibble::tibble(ccn = base::c("A", "B", "C", "D")),
    zip_key = base::c("80204", "80218", NA, "80204"),
    city_key = base::c("DENVER", "DENVER", "PUEBLO", "AURORA")
  )
  all_ccns <- base::c("A", "B", "C", "D")

  # a shared ZIP wins, and a candidate with no ZIP is not dropped by it
  zip_hit <- block_ccn_candidates(base::list(.zip_key = "80204", .city_key = "DENVER"), all_ccns, profiles)
  testthat::expect_equal(zip_hit$key, "zip")
  testthat::expect_setequal(zip_hit$ccns, base::c("A", "D"))

  # no ZIP agrees: fall back to the city
  city_hit <- block_ccn_candidates(base::list(.zip_key = "99999", .city_key = "DENVER"), all_ccns, profiles)
  testthat::expect_equal(city_hit$key, "city")
  testthat::expect_setequal(city_hit$ccns, base::c("A", "B"))

  # nothing agrees, or the facility has neither key: the state set stands
  none <- block_ccn_candidates(base::list(.zip_key = "99999", .city_key = "NOWHERE"), all_ccns, profiles)
  testthat::expect_equal(none$key, "state")
  testthat::expect_setequal(none$ccns, all_ccns)
  testthat::expect_equal(block_ccn_candidates(base::list(), all_ccns, profiles)$key, "state")
})

testthat::test_that("a name-only match needs a higher score than one with an address or a ZIP", {
  inputs <- crosswalk_inputs()
  run <- function(f) base::suppressMessages(match_facilities_to_ccn(f, tracker_manifest = NULL, npi_xwalk = NULL, universe = inputs$universe))

  # exact street address, no city or ZIP in the file: the address carries it,
  # so the 0.71 score stands
  with_address <- run(facility_record("a", hospital_name = "Denver Health Medical Center",
                                      address = "777 Bannock St", state = "CO"))
  testthat::expect_equal(with_address$ccn, "060011")
  testthat::expect_equal(with_address$ccn_block_key, "state")

  # the same name with no address at all is name-only evidence and is refused
  name_only <- run(facility_record("b", hospital_name = "Denver Health Medical Center", state = "CO"))
  testthat::expect_true(base::is.na(name_only$ccn))

  # a ZIP in the address narrows the field and is recorded
  with_zip <- run(facility_record("c", hospital_name = "Denver Health Medical Center",
                                  address = "777 Bannock St, Denver, CO 80204", city = "Denver", state = "CO"))
  testthat::expect_equal(with_zip$ccn, "060011")
  testthat::expect_equal(with_zip$ccn_block_key, "zip")
})
