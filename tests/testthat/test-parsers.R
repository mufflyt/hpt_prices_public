#' MRF download, probe, and parser tests. Every fixture goes through the real
#' read paths: CSVs through the DuckDB CLI, JSON through jq --stream.

mrf_fixture <- function(name) {
  fixture_path("mrf", name)
}

price_row <- function(tbl, ...) {
  out <- dplyr::filter(tbl, ...)
  testthat::expect_equal(base::nrow(out), 1L)
  out
}

testthat::test_that("CSV metadata rows give license, type 2 NPIs, version, and dates", {
  tall_meta <- read_csv_mrf_metadata(mrf_fixture("v3_tall.csv"))
  testthat::expect_equal(tall_meta$hospital_name, "Test General Hospital")
  testthat::expect_equal(tall_meta$version, "3.0.0")
  testthat::expect_equal(tall_meta$license_number, "12345")
  testthat::expect_equal(tall_meta$license_state, "CO")
  testthat::expect_equal(tall_meta$type_2_npi, "1234567890;1098765432")
  testthat::expect_equal(tall_meta$location_name, "Test General Hospital; Test General North")
  testthat::expect_equal(tall_meta$charge_header_line, 3L)

  messy_meta <- read_csv_mrf_metadata(mrf_fixture("messy_tall.csv"))
  testthat::expect_equal(messy_meta$hospital_name, "Messy Regional Medical Center")
  testthat::expect_equal(messy_meta$license_state, "TX")
  testthat::expect_equal(messy_meta$type_2_npi, "1212121212;3434343434")
  testthat::expect_equal(messy_meta$last_updated_on, "2026-03-02")
  testthat::expect_equal(messy_meta$charge_header_line, 4L)

  v2_meta <- read_csv_mrf_metadata(mrf_fixture("v22_tall.csv"))
  testthat::expect_equal(v2_meta$version, "2.2.0")
  testthat::expect_equal(v2_meta$location_name, "Legacy Community Hospital")
  testthat::expect_equal(v2_meta$license_number, "CA-998877")
  testthat::expect_true(base::is.na(v2_meta$type_2_npi))
  testthat::expect_equal(v2_meta$last_updated_on, "2025-07-01")
})

testthat::test_that("v3 tall CSV: type gate, multi-code lines, DRG padding, gross-only lines", {
  prices <- parse_csv_mrf(mrf_fixture("v3_tall.csv"), test_codebook(), mrf_url = "https://example.org/tall.csv")

  testthat::expect_named(prices, base::names(canonical_price_columns()))
  testthat::expect_equal(base::nrow(prices), 6L)
  testthat::expect_true(base::all(prices$source == "own_crawl"))
  testthat::expect_true(base::all(prices$type_2_npi == "1234567890;1098765432"))
  testthat::expect_true(base::all(prices$license_state == "CO"))
  testthat::expect_equal(prices$mrf_file_id[[1]], mrf_sha256(mrf_fixture("v3_tall.csv")))

  # CDM "58100" on code|1 rejected, CPT 58100 on code|2 of the same line kept;
  # the CDM + RC line is rejected entirely
  emb <- prices |> dplyr::filter(.data$code == "58100")
  testthat::expect_equal(base::nrow(emb), 2L)
  testthat::expect_true(base::all(emb$code_type == "CPT" & emb$type_verified))
  testthat::expect_false(base::any(prices$description == "Chargemaster biopsy tray"))
  aetna <- price_row(emb, .data$payer_name == "Aetna")
  testthat::expect_equal(aetna$negotiated_dollar, 450.25)
  testthat::expect_equal(aetna$gross, 1200)
  testthat::expect_equal(aetna$discounted_cash, 600)
  testthat::expect_equal(base::c(aetna$median_amount, aetna$p10, aetna$p90), base::c(440, 300, 520))
  testthat::expect_equal(aetna$count, "25")
  testthat::expect_equal(base::c(aetna$min, aetna$max), base::c(380, 610))
  testthat::expect_equal(price_row(emb, .data$payer_name == "Blue Cross Blue Shield")$count, "1 through 10")

  # two target codes on one charge line both yield rows
  colonoscopy <- prices |> dplyr::filter(.data$description == "Colonoscopy diagnostic")
  testthat::expect_setequal(colonoscopy$code, base::c("45378", "G0121"))
  testthat::expect_true(base::all(colonoscopy$negotiated_dollar == 900))

  # APR-DRG 742 rejected; MS-DRG "0742" matched as 742
  drg <- price_row(prices, .data$concept == "drg_uterine_nonmalignant")
  testthat::expect_equal(drg$code, "742")
  testthat::expect_equal(drg$code_type, "MS-DRG")
  testthat::expect_equal(drg$negotiated_percentage, 55)
  testthat::expect_equal(drg$median_amount, 16500)
  testthat::expect_equal(drg$notes, "Percent of billed charges, capped")
  testthat::expect_false(base::any(stringr::str_detect(prices$description, "APR grouper")))

  # gross/cash-only line kept with NA payer
  iud <- price_row(prices, .data$code == "58300")
  testthat::expect_true(base::is.na(iud$payer_name))
  testthat::expect_equal(base::c(iud$gross, iud$discounted_cash), base::c(500, 300))

  testthat::expect_false(base::any(prices$code == "99213"))
})

testthat::test_that("v3 wide CSV unpivots to one row per payer/plan with the right numbers", {
  prices <- parse_csv_mrf(mrf_fixture("v3_wide.csv"), test_codebook())

  testthat::expect_equal(base::nrow(prices), 7L)
  testthat::expect_true(base::all(prices$license_state == "TX"))
  testthat::expect_true(base::all(prices$type_2_npi == "1111111111"))

  emb <- prices |> dplyr::filter(.data$code == "58100")
  testthat::expect_equal(base::nrow(emb), 2L)
  bcbs <- price_row(emb, .data$payer_name == "Blue Cross Blue Shield")
  testthat::expect_equal(bcbs$plan_name, "PPO Select")
  testthat::expect_equal(bcbs$negotiated_dollar, 410)
  testthat::expect_equal(base::c(bcbs$median_amount, bcbs$p10, bcbs$p90), base::c(405, 380, 430))
  testthat::expect_equal(bcbs$methodology, "fee schedule")
  uhc <- price_row(emb, .data$payer_name == "United Healthcare")
  testthat::expect_equal(uhc$plan_name, "Choice Plus")
  testthat::expect_equal(uhc$negotiated_dollar, 395)
  testthat::expect_equal(uhc$count, "30")

  # two codes x two payers on one line; a percentage-only payer keeps its stats
  colonoscopy <- prices |> dplyr::filter(.data$description == "Colonoscopy with biopsy")
  testthat::expect_equal(base::nrow(colonoscopy), 4L)
  testthat::expect_setequal(colonoscopy$code, base::c("45378", "45380"))
  uhc_pct <- price_row(colonoscopy, .data$code == "45380", .data$payer_name == "United Healthcare")
  testthat::expect_true(base::is.na(uhc_pct$negotiated_dollar))
  testthat::expect_equal(uhc_pct$negotiated_percentage, 60)
  testthat::expect_equal(uhc_pct$median_amount, 950)
  testthat::expect_equal(uhc_pct$count, "1 through 10")
  testthat::expect_equal(uhc_pct$notes, "Paid at 60 percent of billed charges")

  iud <- price_row(prices, .data$code == "58300")
  testthat::expect_true(base::is.na(iud$payer_name))
  testthat::expect_equal(iud$gross, 500)
})

testthat::test_that("v2.2 tall CSV keeps billing_class and estimated_amount", {
  prices <- parse_csv_mrf(mrf_fixture("v22_tall.csv"), test_codebook())

  testthat::expect_equal(base::nrow(prices), 4L)
  testthat::expect_true(base::all(prices$file_version == "2.2.0"))
  snare <- prices |> dplyr::filter(.data$code == "45385")
  testthat::expect_setequal(snare$billing_class, base::c("facility", "professional"))
  testthat::expect_equal(price_row(snare, .data$billing_class == "professional")$estimated_amount, 275)
  drg <- price_row(prices, .data$code == "743")
  testthat::expect_equal(drg$negotiated_percentage, 70)
  testthat::expect_equal(drg$estimated_amount, 15400)
  testthat::expect_equal(drg$payer_name, "Kaiser")
})

testthat::test_that("messy CSV (BOM, CRLF, spaced pipes, blank row, $ amounts, stray quote) parses", {
  prices <- parse_csv_mrf(mrf_fixture("messy_tall.csv"), test_codebook())

  testthat::expect_equal(base::nrow(prices), 3L)
  testthat::expect_equal(base::attr(prices, "parse_log")$rejected_lines, 0)

  emb <- price_row(prices, .data$code == "58100")
  testthat::expect_equal(emb$code_type, "CPT")
  testthat::expect_equal(emb$gross, 1250)
  testthat::expect_equal(emb$negotiated_dollar, 512.40)
  testthat::expect_equal(emb$description, "Biopsy, endometrium")

  colonoscopy <- price_row(prices, .data$code == "45378")
  testthat::expect_equal(colonoscopy$notes, "Line one\r\nline two")

  testthat::expect_equal(price_row(prices, .data$concept == "drg_uterine_nonmalignant")$code, "742")
})

testthat::test_that("a stray Windows-1252 byte is repaired, counted, and loses no rows", {
  prices <- parse_csv_mrf(mrf_fixture("invalid_utf8_tall.csv"), test_codebook())
  log <- base::attr(prices, "parse_log")

  testthat::expect_equal(log$encoding, "invalid")
  testthat::expect_equal(log$invalid_bytes, 1)
  testthat::expect_equal(base::nrow(prices), 2L)
  emb <- price_row(prices, .data$code == "58100")
  testthat::expect_equal(emb$description, "Endometrial biopsy, physician\u2019s office")
  testthat::expect_equal(emb$negotiated_dollar, 450.25)
})

testthat::test_that("invalid UTF-8 past the first MB triggers a repaired re-read", {
  tall_bytes <- base::readBin(mrf_fixture("v3_tall.csv"), "raw", 1e6)
  noise_row <- base::charToRaw("Office visit,99213,CPT,,,,outpatient,,,200,100,Aetna,Aetna PPO,95,,,,,,,fee schedule,95,95,\n")
  # a target row with a Windows-1252 em dash (0x97) beyond the 1 MB sniff window
  bad_row <- base::c(
    base::charToRaw("Endometrial biopsy "), base::as.raw(0x97),
    base::charToRaw(" office,58100,CPT,,,,outpatient,,,999,499,Cigna,Open Access Plus,321,,,,,,,fee schedule,321,321,\n")
  )
  late_path <- base::tempfile(fileext = ".csv")
  base::writeBin(base::c(tall_bytes, base::rep(noise_row, 12000L), bad_row), late_path)

  late <- parse_csv_mrf(late_path, test_codebook())
  testthat::expect_equal(base::attr(late, "parse_log")$encoding, "invalid")
  testthat::expect_equal(base::attr(late, "parse_log")$invalid_bytes, 1)
  testthat::expect_equal(base::nrow(late), 7L)
  cigna <- price_row(late, .data$payer_name == "Cigna", .data$code == "58100")
  testthat::expect_equal(cigna$negotiated_dollar, 321)
  testthat::expect_equal(cigna$description, "Endometrial biopsy \u2014 office")
})

testthat::test_that("UTF-16 CSVs (LE with BOM, BOM-less LE, BE) are transcoded", {
  prices <- parse_csv_mrf(mrf_fixture("utf16le_tall.csv"), test_codebook())
  testthat::expect_equal(base::attr(prices, "parse_log")$encoding, "utf-16le")
  testthat::expect_equal(base::nrow(prices), 2L)
  testthat::expect_equal(price_row(prices, .data$code == "45378")$negotiated_dollar, 900)
  testthat::expect_true(base::all(prices$type_2_npi == "1234567890;1098765432"))

  utf16le <- base::readBin(mrf_fixture("utf16le_tall.csv"), "raw", 1e6)
  no_bom_path <- base::tempfile(fileext = ".csv")
  base::writeBin(utf16le[-(1:2)], no_bom_path)
  testthat::expect_equal(mrf_file_encoding(no_bom_path), "utf-16le")
  testthat::expect_equal(read_csv_mrf_metadata(no_bom_path)$license_state, "CO")
  testthat::expect_equal(base::nrow(parse_csv_mrf(no_bom_path, test_codebook())), 2L)

  # swap byte pairs: little-endian to big-endian (BOM-less)
  body <- utf16le[-(1:2)]
  swapped <- body[base::as.vector(base::rbind(base::seq(2L, base::length(body), 2L), base::seq(1L, base::length(body), 2L)))]
  be_path <- base::tempfile(fileext = ".csv")
  base::writeBin(swapped, be_path)
  testthat::expect_equal(mrf_file_encoding(be_path), "utf-16be")
  testthat::expect_equal(base::nrow(parse_csv_mrf(be_path, test_codebook())), 2L)
})

testthat::test_that("TLS failures are recognized from curl's messages", {
  testthat::expect_true(is_tls_error("SSL peer certificate or SSH remote key was not OK [www.mclaren.org]: SSL certificate problem: unable to get local issuer certificate"))
  testthat::expect_false(is_tls_error("The requested URL returned error: 404"))
  testthat::expect_false(is_tls_error(NA_character_))
})

testthat::test_that("v3 JSON streams through jq with the same gating and flattening", {
  prices <- parse_json_mrf(mrf_fixture("v3.json"), test_codebook(), mrf_url = "https://example.org/v3.json")

  testthat::expect_named(prices, base::names(canonical_price_columns()))
  testthat::expect_equal(base::nrow(prices), 6L)
  testthat::expect_true(base::all(prices$hospital_name == "Test General Hospital"))
  testthat::expect_true(base::all(prices$type_2_npi == "1234567890;1098765432"))
  testthat::expect_true(base::all(prices$license_number == "12345" & prices$license_state == "CO"))
  testthat::expect_true(base::all(prices$location_name == "Test General Hospital; Test General North"))
  testthat::expect_true(base::all(prices$file_version == "3.0.0"))

  # CDM entry rejected, CPT entry of the same item kept: 2 payers + 1 payer-less charge
  emb <- prices |> dplyr::filter(.data$code == "58100")
  testthat::expect_equal(base::nrow(emb), 3L)
  testthat::expect_true(base::all(emb$code_type == "CPT"))
  aetna <- price_row(emb, .data$payer_name == "Aetna")
  testthat::expect_equal(aetna$negotiated_dollar, 450.25)
  testthat::expect_equal(base::c(aetna$median_amount, aetna$p10, aetna$p90), base::c(440, 300, 520))
  testthat::expect_equal(aetna$modifiers, "50|59")
  testthat::expect_equal(price_row(emb, .data$payer_name == "Blue Cross Blue Shield")$negotiated_percentage, 60)
  cash_only <- price_row(emb, base::is.na(.data$payer_name))
  testthat::expect_equal(base::c(cash_only$gross, cash_only$discounted_cash), base::c(1500, 700))
  testthat::expect_equal(cash_only$setting, "inpatient")

  testthat::expect_setequal(prices$code[prices$concept == "colonoscopy"], base::c("45378", "G0121"))

  drg <- price_row(prices, .data$concept == "drg_uterine_nonmalignant")
  testthat::expect_equal(drg$code, "742")
  testthat::expect_equal(drg$negotiated_dollar, 18000)
  testthat::expect_false(base::any(stringr::str_detect(prices$description, "APR grouper")))
})

testthat::test_that("v2 JSON: metadata after the charge array, numeric codes, v2 fields", {
  prices <- parse_json_mrf(mrf_fixture("v2.json"), test_codebook())

  testthat::expect_equal(base::nrow(prices), 2L)
  testthat::expect_true(base::all(prices$file_version == "2.2.0"))
  testthat::expect_true(base::all(prices$last_updated_on == "2025-07-01"))
  testthat::expect_true(base::all(prices$location_name == "Legacy Community Hospital"))
  testthat::expect_true(base::all(prices$license_state == "CA"))

  biopsy <- price_row(prices, .data$code == "45380")
  testthat::expect_equal(biopsy$estimated_amount, 1100)
  testthat::expect_equal(biopsy$billing_class, "facility")
  testthat::expect_true(base::is.na(price_row(prices, .data$code == "58100")$payer_name))
})

testthat::test_that("JSON stream mode (two jq --stream passes) matches memory mode", {
  for (fixture in base::c("v3.json", "v2.json")) {
    in_memory <- parse_json_mrf(mrf_fixture(fixture), test_codebook(), mode = "memory")
    streamed <- parse_json_mrf(mrf_fixture(fixture), test_codebook(), mode = "stream")
    testthat::expect_equal(base::attr(streamed, "parse_log")$layout, "stream")
    base::attr(streamed, "parse_log") <- NULL
    base::attr(in_memory, "parse_log") <- NULL
    testthat::expect_equal(
      dplyr::arrange(streamed, .data$code, .data$setting, .data$payer_name),
      dplyr::arrange(in_memory, .data$code, .data$setting, .data$payer_name)
    )
  }
})

testthat::test_that("JSON prefix metadata is read cheaply from the first bytes", {
  meta <- read_json_mrf_metadata(mrf_fixture("v3.json"))
  testthat::expect_equal(meta$hospital_name, "Test General Hospital")
  testthat::expect_equal(meta$license_state, "CO")
  testthat::expect_equal(meta$type_2_npi, "1234567890;1098765432")
  testthat::expect_equal(meta$attestation, "TRUE")
})

testthat::test_that("probe reads metadata from plain, gzip, and zip prefixes", {
  csv_bytes <- base::readBin(mrf_fixture("v3_tall.csv"), "raw", 65536L)
  plain <- mrf_metadata_from_prefix(csv_bytes)
  testthat::expect_equal(plain$format, "csv")
  testthat::expect_equal(plain$layout, "tall")
  testthat::expect_equal(plain$type_2_npi, "1234567890;1098765432")

  gz_path <- base::tempfile(fileext = ".csv.gz")
  connection <- base::gzfile(gz_path, "wb")
  base::writeBin(csv_bytes, connection)
  base::close(connection)
  gz_bytes <- base::readBin(gz_path, "raw", 65536L)
  gz <- mrf_metadata_from_prefix(utils::head(gz_bytes, base::length(gz_bytes) - 20L))
  testthat::expect_equal(gz$compression, "gzip")
  testthat::expect_equal(gz$format, "csv")
  testthat::expect_equal(gz$hospital_name, "Test General Hospital")

  zip_path <- base::tempfile(fileext = ".zip")
  utils::zip(zip_path, mrf_fixture("v3.json"), flags = "-j -q")
  zipped <- mrf_metadata_from_prefix(base::readBin(zip_path, "raw", 65536L))
  testthat::expect_equal(zipped$compression, "zip")
  testthat::expect_equal(zipped$format, "json")
  testthat::expect_equal(zipped$version, "3.0.0")

  local_probe <- read_mrf_prefix(mrf_fixture("v3_wide.csv"), n_bytes = 1024L)
  testthat::expect_equal(base::length(local_probe$bytes), 1024L)
})

testthat::test_that("download_mrf unpacks gz and zip, sniffs format, and the batch resumes", {
  dest_dir <- base::file.path(base::tempdir(), "mrf_download_test")
  base::unlink(dest_dir, recursive = TRUE)

  gz_path <- base::file.path(base::tempdir(), "hospital_standardcharges.gz")
  connection <- base::gzfile(gz_path, "wb")
  base::writeBin(base::readBin(mrf_fixture("v3.json"), "raw", 1e6), connection)
  base::close(connection)
  gz_url <- base::paste0("file://", base::normalizePath(gz_path))

  gz_row <- download_mrf(gz_url, dest_dir = dest_dir)
  testthat::expect_equal(base::nrow(gz_row), 1L)
  testthat::expect_true(base::is.na(gz_row$error))
  testthat::expect_equal(gz_row$format, "json")
  testthat::expect_equal(gz_row$sha256, mrf_sha256(mrf_fixture("v3.json")))
  testthat::expect_true(stringr::str_ends(gz_row$local_path, "\\.json"))
  testthat::expect_false(base::any(stringr::str_detect(base::list.files(dest_dir), "\\.(part|download)$")))

  zip_path <- base::file.path(base::tempdir(), "two_files.zip")
  base::unlink(zip_path)
  utils::zip(zip_path, base::c(mrf_fixture("v3_tall.csv"), mrf_fixture("v2.json")), flags = "-j -q")
  zip_url <- base::paste0("file://", base::normalizePath(zip_path))
  missing_url <- base::paste0("file://", base::normalizePath(base::tempdir()), "/does_not_exist.csv")
  state_path <- base::file.path(dest_dir, "state.csv")

  batch <- download_mrf_batch(base::c(zip_url, missing_url), dest_dir = dest_dir, state_path = state_path)
  zip_rows <- batch |> dplyr::filter(.data$url == zip_url)
  testthat::expect_equal(base::nrow(zip_rows), 2L)
  testthat::expect_setequal(zip_rows$format, base::c("csv", "json"))
  testthat::expect_false(base::is.na(batch$error[batch$url == missing_url]))

  # a re-run skips the finished URL and records the same state
  rerun <- download_mrf_batch(base::c(zip_url, missing_url), dest_dir = dest_dir, state_path = state_path, retry_failed = FALSE)
  testthat::expect_equal(base::nrow(rerun), 3L)
  testthat::expect_equal(base::sort(rerun$sha256[rerun$url == zip_url]), base::sort(zip_rows$sha256))

  # a parser accepts the downloaded file directly
  csv_path <- zip_rows$local_path[zip_rows$format == "csv"]
  testthat::expect_equal(base::nrow(parse_csv_mrf(csv_path, test_codebook())), 6L)
})

testthat::test_that("sniffing classifies content, not extensions", {
  testthat::expect_equal(sniff_mrf_bytes(base::charToRaw("﻿  {\"a\":1}"))$format, "json")
  testthat::expect_equal(sniff_mrf_bytes(base::charToRaw("<!DOCTYPE html><html>"))$format, "html")
  testthat::expect_equal(sniff_mrf_bytes(base::charToRaw("hospital_name,version\n"))$format, "csv")
  testthat::expect_equal(sniff_mrf_bytes(base::raw())$format, "empty")
})

testthat::test_that("wide column names parse by position with spaced payer names", {
  parsed <- parse_hpt_wide_columns(base::c(
    "standard_charge | Blue Cross Blue Shield | PPO Select | negotiated_dollar",
    "median_amount|Blue Cross Blue Shield|PPO Select",
    "estimated_amount|Humana|Gold",
    "standard_charge|gross",
    "description",
    "standard_charge|[payer_AETNA MAN MEDICARE]|[plan_MANAGED MEDICARE]|negotiated_dollar"
  ))

  testthat::expect_equal(parsed$col_index, base::c(1L, 2L, 3L, 6L))
  testthat::expect_equal(parsed$payer_name, base::c("Blue Cross Blue Shield", "Blue Cross Blue Shield", "Humana", "AETNA MAN MEDICARE"))
  testthat::expect_equal(parsed$plan_name, base::c("PPO Select", "PPO Select", "Gold", "MANAGED MEDICARE"))
  testthat::expect_equal(parsed$metric, base::c("negotiated_dollar", "median_amount", "estimated_amount", "negotiated_dollar"))
})

# ---- CSV MRF text handling ---------------------------------------------------------

testthat::test_that("the charge-table header is found wherever the hospital hid it", {
  # CMS lets the charge table start after the two general-data rows, and files
  # pad that with blank or preamble lines
  lines <- base::c("Hospital Name,Last Updated", "Denver Health,2026-01-02", "", "notes about this file",
                   "description,code|1,code|1|type,payer_name", "COLONOSCOPY,45378,CPT,Aetna")
  testthat::expect_equal(detect_hpt_charge_header(lines), 5L)

  # a byte-order mark, upper case, and spaces around the pipe must not hide it
  testthat::expect_equal(detect_hpt_charge_header(base::c("x", "﻿DESCRIPTION, CODE | 1 ,PAYER")), 2L)
  # "description" first is enough even with no code column
  testthat::expect_equal(detect_hpt_charge_header(base::c("junk", "description,setting,payer_name")), 2L)
  # a file with no charge table fails loudly rather than guessing row 1
  testthat::expect_error(detect_hpt_charge_header(base::c("a,b", "1,2")), "Could not find")
})

testthat::test_that("CSV lines split on quoting, not on commas", {
  fields <- split_csv_line('"BIOPSY, ENDOMETRIAL",58100,"He said ""no""",')
  testthat::expect_equal(fields[1:3], base::c("BIOPSY, ENDOMETRIAL", "58100", 'He said "no"'))
  testthat::expect_length(split_csv_line("a,b,c"), 3)
})

testthat::test_that("MRF dates normalize across formats and refuse nonsense", {
  testthat::expect_equal(normalize_mrf_date(base::c("2026-01-02", "1/2/2026", "01-02-2026", "2026/01/02")),
                         base::rep("2026-01-02", 4))
  # A two-digit year must land in this century on every platform. Linux's %Y
  # accepts "26" as year 26 where macOS refuses it, and the year guard used to
  # be a STRING comparison against "1990" ("26" > "1990" is TRUE), so the same
  # file dated 1/2/26 became "26-01-02" in CI and "2026-01-02" locally.
  testthat::expect_equal(normalize_mrf_date("1/2/26"), "2026-01-02")
  # a year outside the plausible range is left as written rather than turned
  # into a date nobody meant
  testthat::expect_equal(normalize_mrf_date("1/2/1899"), "1/2/1899")
  testthat::expect_equal(normalize_mrf_date("not a date"), "not a date")
})

testthat::test_that("multi-value fields split on pipe or semicolon, and NPIs are extracted", {
  testthat::expect_equal(split_mrf_multi("a | b;c"), base::c("a", "b", "c"))
  testthat::expect_equal(split_mrf_multi(NA_character_), base::character())
  testthat::expect_equal(split_mrf_multi("   "), base::character())

  testthat::expect_equal(format_type_2_npi(base::c("1234567890", "NPI: 1234567890", "9876543210")),
                         "1234567890;9876543210")
  # nothing NPI-shaped: the text is kept rather than dropped silently
  testthat::expect_equal(format_type_2_npi(base::c("pending", NA)), "pending")
  testthat::expect_true(base::is.na(format_type_2_npi(base::c(NA_character_, ""))))
})

testthat::test_that("blanks become NA, percentages parse, and notes combine in order", {
  testthat::expect_equal(mrf_blank_to_na(base::c("x", "", "   ", NA)), base::c("x", NA, NA, NA))
  testthat::expect_equal(parse_percentage(base::c("45%", "45", "")), base::c(45, 45, NA))
  testthat::expect_equal(combine_mrf_notes("payer note", "generic note"), "payer note || generic note")
  testthat::expect_equal(combine_mrf_notes(NA, "generic note"), "generic note")
  testthat::expect_equal(combine_mrf_notes("payer note", ""), "payer note")
  testthat::expect_true(base::is.na(combine_mrf_notes("", NA)))
})

testthat::test_that("UTF-16 and embedded nulls are decoded, not mangled", {
  utf16 <- base::c(base::as.raw(base::c(0xff, 0xfe)), base::writeBin("hi", base::raw(), size = 1)[0],
                   base::as.raw(base::c(0x68, 0x00, 0x69, 0x00)))
  testthat::expect_equal(mrf_bytes_encoding(utf16), "utf-16le")
  # the byte-order mark survives decoding as a character and is stripped later,
  # by detect_hpt_charge_header(); decoding does not silently reshape the text
  testthat::expect_equal(mrf_decode_bytes(utf16), "\ufeffhi")
  testthat::expect_equal(detect_hpt_charge_header(base::c(base::paste0(mrf_decode_bytes(utf16), "description,code|1"))), 1L)

  plain <- base::charToRaw("description,code|1")
  testthat::expect_false(mrf_bytes_encoding(plain) %in% base::c("utf-16le", "utf-16be"))
  testthat::expect_equal(mrf_decode_bytes(plain), "description,code|1")
  # a stray null byte would end the string early in rawToChar
  testthat::expect_equal(mrf_decode_bytes(base::c(base::charToRaw("ab"), base::as.raw(0), base::charToRaw("cd"))), "abcd")
})

# ---- JSON MRF helpers --------------------------------------------------------------

testthat::test_that("the jq code lookup carries both families, with DRGs unpadded", {
  codebook <- tibble::tibble(
    code = base::c("45378", "58100", "0742", "788"),
    code_family = base::c("procedure", "procedure", "ms_drg", "ms_drg")
  )
  lookup <- jsonlite::fromJSON(base::as.character(json_mrf_code_lookup(codebook)))
  testthat::expect_setequal(base::names(lookup$procedure), base::c("45378", "58100"))
  # jq sees the DRG as the file writes it, without our left padding
  testthat::expect_setequal(base::names(lookup$drg), base::c("742", "788"))
  testthat::expect_true(base::all(base::unlist(lookup$procedure)))

  # a codebook with no DRGs yields an empty object, not a JSON array, so the
  # jq program's lookup stays an object
  only_cpt <- json_mrf_code_lookup(codebook[codebook$code_family == "procedure", ])
  testthat::expect_match(base::as.character(only_cpt), '"drg":\\{\\}')
})

testthat::test_that("header values are read from a JSON prefix that stops mid-file", {
  # the first bytes of a large MRF: the metadata is complete, the array is not
  prefix <- '{"hospital_name": "Denver Health", "last_updated_on": "2026-01-02", "version": "3.0.0",
              "hospital_address": ["777 Bannock St"], "standard_charge_information": [{"description": "COLON'
  # values come back parsed, not as raw JSON text
  testthat::expect_equal(json_prefix_value(prefix, "hospital_name"), "Denver Health")
  testthat::expect_equal(json_prefix_value(prefix, "version"), "3.0.0")
  testthat::expect_equal(json_prefix_value(prefix, "hospital_address", "array"), "777 Bannock St")
  # a key the prefix never reached returns NULL: absent, not guessed and not NA
  testthat::expect_null(json_prefix_value(prefix, "license_number"))
  # the truncated charge array must not be read as a value
  testthat::expect_null(json_prefix_value(prefix, "standard_charge_information", "array"))
})
