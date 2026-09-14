#' Midwifery presence around hospitals (R/midwifery_link.R).

testthat::test_that("haversine distances match known city pairs", {
  # Denver to Boulder is about 24 miles; a point to itself is 0
  testthat::expect_equal(haversine_miles(39.7392, -104.9903, 40.0150, -105.2705), 24.2, tolerance = 0.02)
  testthat::expect_equal(haversine_miles(39.7, -105, 39.7, -105), 0)
})

testthat::test_that("presence counts midwives, births, and birth centers within the radius", {
  zcta <- tibble::tibble(zip = base::c("80204", "80302", "81501"), lat = base::c(39.7392, 40.0150, 39.0639),
                         lon = base::c(-104.9903, -105.2705, -108.5506))
  zcta_county <- tibble::tibble(zip = base::c("80204", "80302", "81501"), county_fips = base::c("08031", "08013", "08077"))
  # two midwives in Denver, one in Boulder (24 mi), one in Grand Junction (~200 mi)
  midwives <- tibble::tibble(npi = base::as.character(1:4), zip = base::c("80204", "80204", "80302", "81501"), certification = "CNM")
  birth_centers <- tibble::tibble(bc_id = "B1", facility_name = "x", state = "CO", zip = "80302")
  counties <- tibble::tibble(county_fips = base::c("08031", "08013", "08077"), lat = zcta$lat, lon = zcta$lon,
                             births = base::c(8000, 2000, 1500), n_cnm_cm = NA_real_, cnm_share_of_births_pct = NA_real_,
                             wonder_county_reported = FALSE, rucc_2023 = base::c(1L, 2L, 3L))
  p <- hospital_midwifery_presence(tibble::tibble(ccn = "060011", zip = "80204"), zcta, zcta_county, midwives,
                                   birth_centers, counties, radius = 30)
  testthat::expect_equal(p$cnm_within, 3)
  testthat::expect_equal(p$births_within, 10000)
  testthat::expect_equal(p$cnm_per_1k_births, 0.3)
  testthat::expect_equal(p$bc_within, 1)
  testthat::expect_equal(p$county_fips, "08031")
})

testthat::test_that("birth center ZIPs come from the end of the address, not a street number", {
  path <- base::tempfile(fileext = ".csv")
  readr::write_csv(tibble::tibble(bc_id = base::c("C1", "C2"), facility_name = "x", state = "MN",
                                  full_address = base::c("16802 145th Avenue, P.O. Box 116, Milaca, MN 56353", "117 Gillis Avenue NE, Brainerd, MN 56401"),
                                  city = "x", zip_code = base::c("16802", "56401")), path)
  testthat::expect_equal(load_birth_centers(path)$zip, base::c("56353", "56401"))
})
