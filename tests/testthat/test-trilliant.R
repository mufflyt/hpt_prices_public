#' Builds a tiny DuckDB shaped like Trilliant's parsed schema (column names
#' copied from a live per-hospital database on 2026-09-12), then runs the
#' real extract SQL through the DuckDB CLI.

build_trilliant_fixture <- function(path) {
  run_duckdb_sql(base::c(
    "CREATE TABLE hospitals (
       hospital_id BIGINT, hospital_name VARCHAR, location_name VARCHAR,
       hospital_address VARCHAR[], license_number VARCHAR, hospital_state VARCHAR,
       type_2_npi VARCHAR[], version VARCHAR, last_updated_on DATE,
       mrf_url VARCHAR, md5 VARCHAR, data_downloaded VARCHAR,
       enriched_hospital_city VARCHAR);",
    "INSERT INTO hospitals VALUES
       (1, 'Alpha Health', 'Alpha Main', ['1 Main St, Town, CO'], '010444', 'CO', ['1111111111'], '3.0.0', DATE '2026-04-30',
        'https://alpha.org/84-1_alpha_standardcharges.csv', 'aaa', '2026-07-18', 'Town'),
       (2, 'Alpha Health', 'Alpha ER', ['9 Side St, Town, CO'], '010444', 'CO', ['1111111111'], '3.0.0', DATE '2026-04-30',
        'https://alpha.org/84-1_alpha_standardcharges.csv', 'aaa', '2026-07-19', 'Town'),
       (3, 'Beta Hospital', 'Beta', [], '77', 'TX', [], '2.2.0', DATE '2025-12-01',
        'https://beta.org/99-2_beta_standardcharges.json', 'bbb', '2026-07-18', 'City');",
    "CREATE TABLE standard_charge_details (
       detail_id BIGINT, charge_seq INTEGER, payer_seq INTEGER, hospital_id BIGINT,
       description VARCHAR, gross_charge DOUBLE, discounted_cash DOUBLE, minimum DOUBLE, maximum DOUBLE,
       setting VARCHAR, billing_class VARCHAR, cpt VARCHAR, hcpcs VARCHAR, ms_drg VARCHAR, rc VARCHAR, cdm VARCHAR,
       other_code1 VARCHAR, other_code1_type VARCHAR, other_code2 VARCHAR, other_code2_type VARCHAR,
       payer_name VARCHAR, plan_name VARCHAR, additional_payer_notes VARCHAR, additional_generic_notes VARCHAR,
       standard_charge_dollar DOUBLE, standard_charge_percentage DOUBLE, standard_charge_algorithm VARCHAR,
       estimated_amount DOUBLE, methodology VARCHAR, median_amount DOUBLE, percentile_10th DOUBLE,
       percentile_90th DOUBLE, count VARCHAR, enriched_payer_type VARCHAR);",
    # Hospitals 1 and 2 share file 'aaa': identical rows that must collapse.
    "INSERT INTO standard_charge_details
     SELECT h.id * 100 + r.n, r.cs, r.ps, h.id, r.descr, r.gross, r.cash, NULL, NULL, 'outpatient', NULL,
            r.cpt, r.hcpcs, r.ms_drg, NULL, r.cdm, r.oc1, r.oc1t, NULL, NULL,
            r.payer, 'PPO', NULL, NULL, r.dollar, NULL, NULL, NULL, 'fee schedule', r.median, NULL, NULL, NULL, 'Commercial'
     FROM (VALUES (1), (2)) AS h(id),
          (VALUES
            (1, 1, 1, 'EMB',            330.0, 115.0, '58100', NULL,    NULL,  NULL,   NULL,   NULL,      'Aetna',  200.0,   NULL),
            (2, 2, 1, 'COLONOSCOPY',    1700.0, 600.0, '45378', 'G0121', NULL, NULL,   NULL,   NULL,      'Aetna',  1200.0,  NULL),
            (3, 3, 1, 'CDM COLLISION',  50.0,  NULL,  NULL,    NULL,    NULL,  '58100', NULL,  NULL,      'Aetna',  10.0,    NULL),
            (4, 4, 1, 'DRG 742 VIA OC', NULL,  NULL,  NULL,    NULL,    NULL,  NULL,   '0742', 'MS-DRG',  'Cigna',  31000.0, 30000.0),
            (5, 5, 1, 'APR DRG',        NULL,  NULL,  NULL,    NULL,    NULL,  NULL,   '742',  'APR-DRG', 'Cigna',  9999.0,  NULL),
            (6, 6, 1, 'DRG 743',        NULL,  NULL,  NULL,    NULL,    '743', NULL,   NULL,   NULL,      'United', 25173.0, NULL),
            (7, 7, 1, 'APR VAGINAL SOI1', NULL, NULL,  NULL,    NULL,    NULL,  NULL,   '5601', 'APR-DRG', 'Cigna',  6100.0,  NULL),
            (8, 8, 1, 'APR CESAREAN SOI4', NULL, NULL, NULL,    NULL,    NULL,  NULL,   '540-4', 'APR-DRG','Cigna',  19000.0, NULL),
            (9, 9, 1, 'UNTYPED 560-1',   NULL,  NULL,  NULL,    NULL,    NULL,  NULL,   '560-1', NULL,     'Cigna',  5900.0,  NULL),
            (10, 10, 1, 'MS-DRG 560-1',  NULL,  NULL,  NULL,    NULL,    NULL,  NULL,   '560-1', 'MS-DRG', 'Cigna',  5800.0,  NULL)
          ) AS r(n, cs, ps, descr, gross, cash, cpt, hcpcs, ms_drg, cdm, oc1, oc1t, payer, dollar, median);",
    # Hospital 3: a gross/cash-only IUD insertion line with no payer.
    "INSERT INTO standard_charge_details (detail_id, charge_seq, payer_seq, hospital_id, description, gross_charge, discounted_cash, cpt)
     VALUES (301, 1, NULL, 3, 'IUD INSERTION', 331.65, 116.08, '58300');"
  ), database = path)

  path
}

testthat::test_that("Trilliant extract gates code types, splits multi-code lines, and collapses shared files", {
  fixture_db <- build_trilliant_fixture(base::tempfile(fileext = ".duckdb"))
  out_dir <- base::tempfile("trilliant_out")

  result <- extract_trilliant(fixture_db, test_codebook(), out_dir = out_dir)
  prices <- read_trilliant_prices(result$prices_dir)

  testthat::expect_equal(result$n_price_rows, 7)
  testthat::expect_setequal(prices$code, base::c("58100", "45378", "G0121", "742", "743", "58300", "560-1"))

  # CDM "58100" and APR-DRG 742 were rejected; only the CPT 58100 row survives.
  testthat::expect_equal(prices$negotiated_dollar[prices$code == "58100"], 200)
  testthat::expect_equal(prices$negotiated_dollar[prices$code == "742"], 31000)
  testthat::expect_true(prices$type_verified[prices$code == "742"])
  testthat::expect_equal(prices$median_amount[prices$code == "742"], 30000)

  # An APR-DRG line is kept only when its declared type names the APR grouper,
  # and only at the severity the codebook lists. "5601" is 560 severity 1.
  apr <- prices[prices$code == "560-1", ]
  testthat::expect_equal(apr$concept, "apr_drg_vaginal_delivery")
  testthat::expect_equal(apr$negotiated_dollar, 6100)
  testthat::expect_true(apr$type_verified)
  # severity 4 is a different product and is not in the codebook; an untyped
  # 560-1 could be any grouper; a 560-1 declared MS-DRG is a different grouper
  testthat::expect_false(base::any(prices$description %in% base::c("APR CESAREAN SOI4", "UNTYPED 560-1", "MS-DRG 560-1")))

  # One colonoscopy line carrying both 45378 and G0121 yields a row per code.
  testthat::expect_equal(prices$description[prices$code == "G0121"], "COLONOSCOPY")
  testthat::expect_equal(prices$concept[prices$code == "G0121"], "colonoscopy")

  # File 'aaa' shared by two facilities contributes each row once, with the
  # earliest retrieval date.
  testthat::expect_equal(base::sum(prices$mrf_file_id == "aaa"), 6)
  testthat::expect_equal(base::unique(prices$retrieved_at[prices$mrf_file_id == "aaa"]), "2026-07-18")

  # Gross/cash-only line keeps NA payer.
  iud <- prices[prices$code == "58300", ]
  testthat::expect_true(base::is.na(iud$payer_name))
  testthat::expect_equal(iud$gross, 331.65)
  testthat::expect_equal(iud$file_version, "2.2.0")

  facilities <- tibble::as_tibble(arrow::read_parquet(result$facilities_path))
  testthat::expect_equal(base::nrow(facilities), 3)
  testthat::expect_equal(facilities$type_2_npi[facilities$facility_key == "trilliant:1"], "1111111111")
  testthat::expect_equal(facilities$address[facilities$facility_key == "trilliant:2"], "9 Side St, Town, CO")
})

testthat::test_that("lake layout: internal_id + run_date keys, versions hash, identity NPI, one read per shared file", {
  db <- base::tempfile(fileext = ".duckdb")
  run_duckdb_sql(base::c(
    "CREATE TABLE current_hospitals (internal_id UUID, canonical_id VARCHAR, hospital_state VARCHAR, run_date DATE, hospital_id BIGINT,
       hospital_name VARCHAR, hospital_address VARCHAR[], location_name VARCHAR, last_updated_on DATE, version VARCHAR,
       license_number VARCHAR, type_2_npi VARCHAR[], enriched_hospital_address VARCHAR, enriched_hospital_city VARCHAR);",
    "INSERT INTO current_hospitals VALUES
       ('00000000-0000-0000-0000-000000000001', 'TX-1', 'TX', DATE '2026-07-18', 1, 'HCA SE', [], 'HCA SE Main', DATE '2026-03-01', '3.0.0', '349', ['1174576698'], '4000 Spencer Hwy', 'Pasadena'),
       ('00000000-0000-0000-0000-000000000002', 'TX-1', 'TX', DATE '2026-07-18', 1, 'HCA SE', [], 'HCA ER Fairmont', DATE '2026-03-01', '3.0.0', '349', [], 'Not provided', 'Not provided');",
    "CREATE TABLE hospital_versions (internal_id UUID, run_date DATE, mrf_url VARCHAR, mrf_content_hash VARCHAR, mrf_data_downloaded VARCHAR, mrf_schema_version VARCHAR);",
    "INSERT INTO hospital_versions VALUES
       ('00000000-0000-0000-0000-000000000001', DATE '2026-07-18', 'https://x.org/62-1_HCA_standardcharges.json', 'hashA', '2026-07-18T01:00', '3.x'),
       ('00000000-0000-0000-0000-000000000002', DATE '2026-07-18', 'https://x.org/62-1_HCA_standardcharges.json', 'hashA', '2026-07-18T01:00', '3.x');",
    "CREATE TABLE hospital_identity (internal_id UUID, npi VARCHAR, license_state VARCHAR, license_number VARCHAR);",
    "INSERT INTO hospital_identity VALUES ('00000000-0000-0000-0000-000000000002', '1154375129', 'TX', '349');",
    "CREATE TABLE current_charge_details (internal_id UUID, run_date DATE, hospital_id BIGINT, charge_seq INTEGER, payer_seq INTEGER,
       description VARCHAR, gross_charge DOUBLE, discounted_cash DOUBLE, setting VARCHAR, billing_class VARCHAR,
       cpt VARCHAR, hcpcs VARCHAR, ms_drg VARCHAR, other_code1 VARCHAR, other_code1_type VARCHAR,
       payer_name VARCHAR, plan_name VARCHAR, standard_charge_dollar DOUBLE, methodology VARCHAR, enriched_payer_type VARCHAR);",
    # identical rows repeated for both facilities, as the lake does for shared files
    "INSERT INTO current_charge_details
     SELECT f.id, DATE '2026-07-18', 1, r.cs, r.ps, r.descr, NULL, NULL, 'outpatient', 'facility', r.cpt, NULL, r.drg, NULL, NULL, r.payer, 'PPO', r.dollar, 'fee schedule', 'Commercial'
     FROM (VALUES ('00000000-0000-0000-0000-000000000001'::UUID), ('00000000-0000-0000-0000-000000000002'::UUID)) AS f(id),
          (VALUES (1, 1, 'COLONOSCOPY', '45378', NULL, 'Aetna', 2400.0), (2, 1, 'DRG 742', NULL, '742', 'Cigna', 4400.0), (3, 1, 'OFFICE VISIT', '99213', NULL, 'Aetna', 100.0))
          AS r(cs, ps, descr, cpt, drg, payer, dollar);"
  ), database = db)

  result <- extract_trilliant(db, test_codebook(), out_dir = base::tempfile("lake_out"))
  prices <- read_trilliant_prices(result$prices_dir)
  facilities <- tibble::as_tibble(arrow::read_parquet(result$facilities_path))

  testthat::expect_equal(result$n_price_rows, 2)                       # shared file read once, 99213 not in codebook
  testthat::expect_setequal(prices$code, base::c("45378", "742"))
  testthat::expect_true(base::all(prices$mrf_file_id == "hashA"))
  testthat::expect_equal(prices$negotiated_dollar[prices$code == "45378"], 2400)
  testthat::expect_equal(base::nrow(facilities), 2)
  testthat::expect_true(base::all(facilities$mrf_file_id == "hashA"))
  er <- facilities[facilities$location_name == "HCA ER Fairmont", ]
  testthat::expect_true(base::is.na(er$address))                       # 'Not provided' becomes NA
  testthat::expect_equal(er$type_2_npi, "1154375129")                  # NPI from hospital_identity
  testthat::expect_equal(facilities$type_2_npi[facilities$location_name == "HCA SE Main"], "1174576698")
})

# ---- resumable stage 1 -------------------------------------------------------------

fake_lake <- function(n_files = 3L, deletes = 0L, inlined = 0L, drop_file = FALSE) {
  dir <- base::tempfile("lake")
  data_dir <- base::file.path(dir, "data", "main", "standard_charge_details")
  base::dir.create(data_dir, recursive = TRUE)
  names <- base::sprintf("ducklake-%02d.parquet", base::seq_len(n_files))
  for (i in base::seq_len(n_files)) {
    readr::write_csv(tibble::tibble(x = i), base::file.path(data_dir, base::paste0(names[[i]], ".csv")))
    base::file.rename(base::file.path(data_dir, base::paste0(names[[i]], ".csv")), base::file.path(data_dir, names[[i]]))
  }
  if (drop_file) base::unlink(base::file.path(data_dir, names[[1]]))

  run_duckdb_sql(base::c(
    "CREATE TABLE ducklake_table (table_id BIGINT, table_name VARCHAR);",
    "INSERT INTO ducklake_table VALUES (7, 'standard_charge_details'), (5, 'hospitals');",
    "CREATE TABLE ducklake_data_file (data_file_id BIGINT, table_id BIGINT, path VARCHAR, path_is_relative BOOLEAN, record_count BIGINT, file_size_bytes BIGINT);",
    base::sprintf("INSERT INTO ducklake_data_file VALUES %s;", base::paste(base::sprintf(
      "(%d, 7, '%s', true, %d, %d)", base::seq_len(n_files), names, 1000L * base::seq_len(n_files), 99L), collapse = ", ")),
    "CREATE TABLE ducklake_delete_file (data_file_id BIGINT, table_id BIGINT);",
    if (deletes > 0L) "INSERT INTO ducklake_delete_file VALUES (1, 7);",
    # DuckLake registers an inlined table for every table, empty or not
    "CREATE TABLE ducklake_inlined_data_tables (table_id BIGINT, table_name VARCHAR, schema_version BIGINT);",
    "INSERT INTO ducklake_inlined_data_tables VALUES (7, 'ducklake_inlined_data_7_7', 7);",
    "CREATE TABLE ducklake_inlined_data_7_7 (x BIGINT);",
    if (inlined > 0L) "INSERT INTO ducklake_inlined_data_7_7 VALUES (1);"
  ), database = base::file.path(dir, "metadata.ducklake"))
  dir
}

testthat::test_that("the lake's data files are listed, and never when reading them would be incomplete", {
  files <- trilliant_lake_data_files(fake_lake(3L), "standard_charge_details")
  testthat::expect_equal(base::nrow(files), 3)
  testthat::expect_equal(files$n_rows, base::c(1000, 2000, 3000))
  testthat::expect_true(base::all(base::file.exists(files$path)))

  # a table with deleted rows, or rows inlined in the metadata, cannot be read
  # from its parquet files alone: the caller must fall back to the full scan
  testthat::expect_null(trilliant_lake_data_files(fake_lake(3L, deletes = 1L), "standard_charge_details"))
  testthat::expect_null(trilliant_lake_data_files(fake_lake(3L, inlined = 1L), "standard_charge_details"))
  # an EMPTY inlined table is registered for every DuckLake table and must not
  # be read as "this table has inlined rows" -- that rejected the real lake
  testthat::expect_equal(base::nrow(trilliant_lake_data_files(fake_lake(2L), "standard_charge_details")), 2)
  # a file the metadata names but the disk does not have
  testthat::expect_null(trilliant_lake_data_files(fake_lake(3L, drop_file = TRUE), "standard_charge_details"))
  # a lake directory with no metadata at all
  testthat::expect_null(trilliant_lake_data_files(base::tempdir(), "standard_charge_details"))
})

testthat::test_that("batches cover every file exactly once", {
  files <- tibble::tibble(data_file_id = 1:7, path = base::letters[1:7], n_rows = 1, bytes = 1)
  batches <- stage_file_batches(files, 3)
  testthat::expect_equal(base::length(batches), 3)
  testthat::expect_equal(base::vapply(batches, base::nrow, base::integer(1), USE.NAMES = FALSE), base::c(3L, 3L, 1L))
  testthat::expect_equal(base::sort(base::unlist(base::lapply(batches, function(b) b$data_file_id), use.names = FALSE)), 1:7)
  testthat::expect_equal(base::length(stage_file_batches(files, 100)), 1)
})

testthat::test_that("an interrupted stage resumes at the first unfinished batch", {
  parts <- base::tempfile("parts")
  base::dir.create(parts)
  manifest <- base::file.path(parts, "_manifest.csv")
  files <- tibble::tibble(data_file_id = 1:6, path = base::paste0(base::letters[1:6], ".parquet"), n_rows = 10, bytes = 1)

  testthat::expect_equal(base::nrow(read_stage_manifest(manifest)), 0)

  # batches 1 and 2 (files 1-4) already written before the interruption
  readr::write_csv(tibble::tibble(part = "part_000000000001.parquet", data_file_id = 1:4, n_rows = 7,
                                  written_at = utc_timestamp()), manifest)

  ran <- base::character()
  real_sql <- base::get("run_duckdb_sql", envir = base::globalenv())
  real_query <- base::get("duckdb_query", envir = base::globalenv())
  real_stage <- base::get("trilliant_stage_sql", envir = base::globalenv())
  # the SQL itself is covered by the fixture extract above; here only which
  # files each batch reads matters
  base::assign("trilliant_stage_sql", function(schema_tbl, codebook, details_relation = NULL) {
    base::paste("SELECT 1 FROM", details_relation)
  }, envir = base::globalenv())
  base::assign("run_duckdb_sql", function(sql, ...) {
    ran <<- base::c(ran, sql[[base::length(sql)]])
    invisible(NULL)
  }, envir = base::globalenv())
  base::assign("duckdb_query", function(...) tibble::tibble(n = 3), envir = base::globalenv())
  withr::defer({
    base::assign("run_duckdb_sql", real_sql, envir = base::globalenv())
    base::assign("duckdb_query", real_query, envir = base::globalenv())
    base::assign("trilliant_stage_sql", real_stage, envir = base::globalenv())
  })

  glob <- stage_in_batches(files, schema_tbl = NULL, codebook = NULL, connection = base::list(database = "x", init = NULL),
                           settings = base::character(), parts_dir = parts, manifest_path = manifest, batch_size = 2)

  # only the unfinished batch (files 5 and 6) is scanned
  testthat::expect_length(ran, 1)
  testthat::expect_true(base::grepl("e.parquet", ran[[1]], fixed = TRUE))
  testthat::expect_true(base::grepl("f.parquet", ran[[1]], fixed = TRUE))
  testthat::expect_false(base::grepl("a.parquet", ran[[1]], fixed = TRUE))
  # the manifest now covers every file, and stage 2 reads the parts as one relation
  testthat::expect_setequal(read_stage_manifest(manifest)$data_file_id, 1:6)
  testthat::expect_equal(base::basename(glob), "part_*.parquet")
})
