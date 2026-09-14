#!/usr/bin/env Rscript
#' Build the CCN-crosswalk fixtures (tests/testthat/fixtures/crosswalk/*).
#'
#' Each fixture's header row is read verbatim from the real public file, so the
#' fixtures cannot drift from the live column names: CMS Hospital General
#' Information, CMS Hospital Enrollments and Additional NPIs, the
#' cms-hpt-tracker manifest/compliance/gaps CSVs, and the AHRQ Compendium
#' hospital linkage. The rows are a handful of hand-written cases (a
#' dropped-zero CCN, a psychiatric unit CCN, a shared system MRF, same-name
#' "Mercy" hospitals in three states, a federal hospital, a hospital without a
#' CCN). Names, addresses, CCNs, and NPIs are public CMS roster facts; nothing
#' comes from hospital price files.
#'
#' Usage (from the repository root):
#'   Rscript tools/make_crosswalk_fixtures.R [out_dir]
#' Header sources are the downloads under HPT_DATA_DIR/reference (made by
#' analysis/02_ccn_crosswalk.R): cms_hospitals/, cms_enrollments/,
#' tracker/<commit>/, and ahrq/. out_dir defaults to a new temporary
#' directory; point it at tests/testthat/fixtures/crosswalk only to regenerate
#' on purpose, after diffing. Output is byte-identical to the committed
#' fixtures (checked 2026-09-13 against the 2026-07-31 CMS releases and tracker
#' commit 27c08db).

base::source("config/paths.R")

args <- base::commandArgs(trailingOnly = TRUE)
out <- if (base::length(args) >= 1L) args[[1]] else base::tempfile("crosswalk_fixtures_")
base::dir.create(out, recursive = TRUE, showWarnings = FALSE)

header_of <- function(path) {
  base::names(readr::read_csv(path, n_max = 0, show_col_types = FALSE, col_types = readr::cols(.default = "c")))
}

#' Newest file under HPT_DATA_DIR/reference/<dir> matching a pattern
reference_file <- function(dir, pattern) {
  paths <- base::list.files(hpt_path("reference", dir), pattern = pattern, recursive = TRUE, full.names = TRUE)
  paths <- paths[!base::grepl("provenance", base::basename(paths))]
  if (base::length(paths) == 0L) {
    base::stop("No ", pattern, " under ", hpt_path("reference", dir), "; run analysis/02_ccn_crosswalk.R first.")
  }
  paths[[base::which.max(base::file.mtime(paths))]]
}

fill_rows <- function(header, rows) {
  tbl <- tibble::as_tibble(stats::setNames(base::rep(base::list(character()), length(header)), header))
  for (row in rows) {
    base::stopifnot(base::all(base::names(row) %in% header))
    new <- base::as.list(stats::setNames(base::rep(NA_character_, length(header)), header))
    new[base::names(row)] <- row
    tbl <- dplyr::bind_rows(tbl, tibble::as_tibble(new))
  }
  tbl
}

write_fixture <- function(tbl, name) {
  readr::write_csv(tbl, base::file.path(out, name), na = "")
}

# CMS Hospital General Information (verbatim "City/Town", "ZIP Code" headers)
hgi <- fill_rows(header_of(reference_file("cms_hospitals", "^hospital_general_information_.*\\.csv$")), base::list(
  base::list("Facility ID" = "060011", "Facility Name" = "DENVER HEALTH & HOSPITAL AUTHORITY", "Address" = "777 BANNOCK STREET", "City/Town" = "DENVER", "State" = "CO", "ZIP Code" = "80204", "Hospital Type" = "Acute Care Hospitals", "Hospital Ownership" = "Government - Hospital District or Authority"),
  base::list("Facility ID" = "060014", "Facility Name" = "PRESBYTERIAN ST LUKES MEDICAL CENTER", "Address" = "1719 EAST 19TH AVENUE", "City/Town" = "DENVER", "State" = "CO", "ZIP Code" = "80218", "Hospital Type" = "Acute Care Hospitals", "Hospital Ownership" = "Proprietary"),
  base::list("Facility ID" = "060023", "Facility Name" = "ST MARYS MEDICAL CENTER", "Address" = "2635 NORTH 7TH STREET", "City/Town" = "GRAND JUNCTION", "State" = "CO", "ZIP Code" = "81501", "Hospital Type" = "Acute Care Hospitals", "Hospital Ownership" = "Voluntary non-profit - Church"),
  base::list("Facility ID" = "060999", "Facility Name" = "ST MARYS MEDICAL CENTER", "Address" = "100 MAIN STREET", "City/Town" = "PUEBLO", "State" = "CO", "ZIP Code" = "81003", "Hospital Type" = "Critical Access Hospitals", "Hospital Ownership" = "Voluntary non-profit - Private"),
  base::list("Facility ID" = "450097", "Facility Name" = "HCA HOUSTON HEALTHCARE SOUTHEAST", "Address" = "4000 SPENCER HIGHWAY", "City/Town" = "PASADENA", "State" = "TX", "ZIP Code" = "77504", "Hospital Type" = "Acute Care Hospitals", "Hospital Ownership" = "Proprietary"),
  base::list("Facility ID" = "170780", "Facility Name" = "MERCY HOSPITAL, INC", "Address" = "218 EAST PACK STREET", "City/Town" = "MOUNDRIDGE", "State" = "KS", "ZIP Code" = "67107", "Hospital Type" = "Critical Access Hospitals", "Hospital Ownership" = "Voluntary non-profit - Private"),
  base::list("Facility ID" = "210008", "Facility Name" = "MERCY MEDICAL CENTER INC", "Address" = "345 ST PAUL PLACE", "City/Town" = "BALTIMORE", "State" = "MD", "ZIP Code" = "21202", "Hospital Type" = "Acute Care Hospitals", "Hospital Ownership" = "Voluntary non-profit - Private"),
  base::list("Facility ID" = "220066", "Facility Name" = "MERCY MEDICAL CTR", "Address" = "271 CAREW STREET", "City/Town" = "SPRINGFIELD", "State" = "MA", "ZIP Code" = "01104", "Hospital Type" = "Acute Care Hospitals", "Hospital Ownership" = "Voluntary non-profit - Church")
))
write_fixture(hgi, "hospital_general_information.csv")

# CMS Hospital Enrollments: CCNs exactly as the live file stores them
enr_rows <- base::list(
  base::list("ENROLLMENT ID" = "O20071120000696", "ENROLLMENT STATE" = "CO", "PROVIDER TYPE TEXT" = "PART A PROVIDER - HOSPITAL", "NPI" = "1689624686", "MULTIPLE NPI FLAG" = "Y", "CCN" = "60011", "ASSOCIATE ID" = "4688583578", "ORGANIZATION NAME" = "DENVER HEALTH AND HOSPITAL AUTHORITY", "DOING BUSINESS AS NAME" = "DENVER HEALTH MEDICAL CENTER", "CITY" = "DENVER", "STATE" = "CO", "ZIP CODE" = "802044507", "PRACTICE LOCATION TYPE" = "MAIN/PRIMARY HOSPITAL LOCATION"),
  base::list("ENROLLMENT ID" = "O20071120000697", "ENROLLMENT STATE" = "CO", "PROVIDER TYPE TEXT" = "PART A PROVIDER - HOSPITAL", "NPI" = "1689624686", "MULTIPLE NPI FLAG" = "N", "CCN" = "06S011", "ASSOCIATE ID" = "4688583578", "ORGANIZATION NAME" = "DENVER HEALTH AND HOSPITAL AUTHORITY", "CITY" = "DENVER", "STATE" = "CO", "PRACTICE LOCATION TYPE" = "HOSPITAL PSYCHIATRIC UNIT"),
  base::list("ENROLLMENT ID" = "O20030204000016", "ENROLLMENT STATE" = "CO", "PROVIDER TYPE TEXT" = "PART A PROVIDER - HOSPITAL", "NPI" = "1720038946", "MULTIPLE NPI FLAG" = "N", "CCN" = "60014", "ASSOCIATE ID" = "143139857", "ORGANIZATION NAME" = "HCA HEALTHONE LLC", "DOING BUSINESS AS NAME" = "HCA HEALTHONE PRESBYTERIAN ST. LUKE'S HOSPITAL", "CITY" = "DENVER", "STATE" = "CO", "PRACTICE LOCATION TYPE" = "MAIN/PRIMARY HOSPITAL LOCATION"),
  base::list("ENROLLMENT ID" = "O20031118000301", "ENROLLMENT STATE" = "TX", "PROVIDER TYPE TEXT" = "PART A PROVIDER - HOSPITAL", "NPI" = "1174576698", "MULTIPLE NPI FLAG" = "N", "CCN" = "450097", "ASSOCIATE ID" = "3173437753", "ORGANIZATION NAME" = "CHCA BAYSHORE LP", "DOING BUSINESS AS NAME" = "HCA HOUSTON HEALTHCARE SOUTHEAST", "CITY" = "PASADENA", "STATE" = "TX", "PRACTICE LOCATION TYPE" = "MAIN/PRIMARY HOSPITAL LOCATION"),
  base::list("ENROLLMENT ID" = "O20990101000001", "ENROLLMENT STATE" = "MA", "PROVIDER TYPE TEXT" = "PART A PROVIDER - HOSPITAL", "NPI" = "1000000001", "MULTIPLE NPI FLAG" = "N", "CCN" = "22006601", "ASSOCIATE ID" = "1000000101", "ORGANIZATION NAME" = "MERCY MEDICAL CENTER", "CITY" = "SPRINGFIELD", "STATE" = "MA", "PRACTICE LOCATION TYPE" = "OTHER HOSPITAL PRACTICE LOCATION"),
  base::list("ENROLLMENT ID" = "O20990101000002", "ENROLLMENT STATE" = "MD", "PROVIDER TYPE TEXT" = "PART A PROVIDER - HOSPITAL", "NPI" = "1000000002", "MULTIPLE NPI FLAG" = "N", "CCN" = "210008", "ASSOCIATE ID" = "1000000102", "ORGANIZATION NAME" = "MERCY MEDICAL CENTER INC", "CITY" = "BALTIMORE", "STATE" = "MD", "PRACTICE LOCATION TYPE" = "MAIN/PRIMARY HOSPITAL LOCATION")
)
enr <- fill_rows(header_of(reference_file("cms_enrollments", "^Hospital_Enrollments_.*\\.csv$")), enr_rows)
write_fixture(enr, "hospital_enrollments.csv")

addl <- fill_rows(header_of(reference_file("cms_enrollments", "^Hospital_Additional_NPIs_.*\\.csv$")), base::list(
  base::list("ENROLLMENT ID" = "O20071120000696", "NPI" = "1000000009")
))
write_fixture(addl, "hospital_additional_npis.csv")

# cms-hpt-tracker manifest, compliance, gaps
manifest_header <- header_of(reference_file("tracker", "^manifest\\.csv$"))
trinity <- "https://hpt.trinity-health.org/043398280_the-mercy-hospital-inc_standardcharges.json"
system_url <- "https://mrf.examplehealth.org/84-0000000_example-health_standardcharges.json"
manifest <- fill_rows(manifest_header, base::list(
  base::list(ccn = "060011", hospital_name = "DENVER HEALTH & HOSPITAL AUTHORITY", city = "DENVER", state = "CO", type = "Acute Care Hospitals", location_name = "Denver Health Medical Center", mrf_url = "https://www.denverhealth.org/-/media/files/84-6000613_denver-health-medical-center_standardcharges.csv?v=3", match_method = "name"),
  base::list(ccn = "060014", hospital_name = "PRESBYTERIAN ST LUKES MEDICAL CENTER", city = "DENVER", state = "CO", type = "Acute Care Hospitals", location_name = "Presbyterian St. Luke's Medical Center", mrf_url = "https://healthonecares.com/mrf/pslmc_standardcharges.json", extra_mrf_urls = "https://healthonecares.com/mrf/pslmc_part1.csv;https://healthonecares.com/mrf/pslmc_part2.csv", match_method = "exact-mrf-url-corpus"),
  base::list(ccn = "060011", hospital_name = "DENVER HEALTH & HOSPITAL AUTHORITY", city = "DENVER", state = "CO", type = "Acute Care Hospitals", location_name = "Example Health", mrf_url = system_url, match_method = "name"),
  base::list(ccn = "060014", hospital_name = "PRESBYTERIAN ST LUKES MEDICAL CENTER", city = "DENVER", state = "CO", type = "Acute Care Hospitals", location_name = "Example Health", mrf_url = system_url, match_method = "name"),
  base::list(ccn = "170780", hospital_name = "MERCY HOSPITAL, INC", city = "MOUNDRIDGE", state = "KS", type = "Critical Access Hospitals", location_name = "Mercy Hospital", mrf_url = trinity, match_method = "global-name"),
  base::list(ccn = "210008", hospital_name = "MERCY MEDICAL CENTER INC", city = "BALTIMORE", state = "MD", type = "Acute Care Hospitals", location_name = "Mercy Hospital", mrf_url = trinity, match_method = "llm-adjudicated"),
  base::list(ccn = "220066", hospital_name = "MERCY MEDICAL CTR", city = "SPRINGFIELD", state = "MA", type = "Acute Care Hospitals", location_name = "Mercy Hospital", mrf_url = trinity, match_method = "name")
))
write_fixture(manifest, "manifest.csv")

compliance <- fill_rows(header_of(reference_file("tracker", "^compliance\\.csv$")), base::list(
  base::list(ccn = "060011", hospital_name = "DENVER HEALTH & HOSPITAL AUTHORITY", city = "DENVER", state = "CO", type = "Acute Care Hospitals", finding = "compliant-observed", assessable = "yes"),
  base::list(ccn = "02013F", hospital_name = "673rd Medical Group (Joint Base Elmendorf-Richardson)", city = "JBER", state = "AK", type = "Acute Care - Department of Defense", finding = "not-applicable-federal", assessable = "no")
))
write_fixture(compliance, "compliance.csv")

gaps <- fill_rows(header_of(reference_file("tracker", "^gaps\\.csv$")), base::list(
  base::list(ccn = "010012", hospital_name = "DEKALB REGIONAL MEDICAL CENTER", address = "200 MED CENTER DRIVE", city = "FORT PAYNE", state = "AL", zip = "35968", type = "Acute Care Hospitals", seeded_domain = "dekalbregional.com", pointer_status = "ok", remediation = "name-match-review")
))
write_fixture(gaps, "gaps.csv")

# AHRQ CHSP 2023 hospital linkage
chsp <- fill_rows(header_of(reference_file("ahrq", "^chsp-hospital-linkage-2023\\.csv$")), base::list(
  base::list(compendium_hospital_id = "CHSP90000001", ccn = "060011", hospital_name = "Denver Health Medical Center", hospital_state = "CO", acutehosp_flag = "1", health_sys_id = "HSI90000001", health_sys_name = "Denver Health", hos_beds = "525"),
  base::list(compendium_hospital_id = "CHSP90000002", ccn = "060014", hospital_name = "Presbyterian/St. Luke's Medical Center", hospital_state = "CO", acutehosp_flag = "1", health_sys_id = "HSI90000002", health_sys_name = "HCA Healthcare", hos_beds = "680"),
  base::list(compendium_hospital_id = "CHSP90000003", ccn = "450097", hospital_name = "HCA Houston Healthcare Southeast", hospital_state = "TX", acutehosp_flag = "1", health_sys_id = "HSI90000002", health_sys_name = "HCA Healthcare", hos_beds = "193"),
  base::list(compendium_hospital_id = "CHSP90000004", ccn = NA_character_, hospital_name = "No CCN Hospital", hospital_state = "TX", acutehosp_flag = "0")
))
write_fixture(chsp, "chsp_hospital_linkage.csv")

base::message("Crosswalk fixtures written to ", out)
base::print(base::list.files(out))
