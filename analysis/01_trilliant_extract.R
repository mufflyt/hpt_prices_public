#!/usr/bin/env Rscript
#' Part A: extract target-code prices from the Trilliant Health MRF lake.
#'
#' Prerequisite (manual, once per snapshot): log in to Oria
#' (oria.trillianthealth.com, free account), choose "Full Data Download",
#' and save mrf_lake_<snapshot>.zip to
#'   <HPT_DATA_DIR>/trilliant/<snapshot>/
#' This script unzips it (if not already unzipped) and runs the extract.
#'
#' Env vars: HPT_TRILLIANT_SNAPSHOT (default 20260721), HPT_DATA_DIR,
#' HPT_REUSE_STAGE=true (skip the lake scan; codebook codes must be unchanged).

base::source("R/00_source_all.R")

snapshot <- base::Sys.getenv("HPT_TRILLIANT_SNAPSHOT", unset = "20260721")
snapshot_dir <- hpt_path("trilliant", snapshot)
lake_dir <- base::file.path(snapshot_dir, "lake")
zip_path <- base::file.path(snapshot_dir, base::paste0("mrf_lake_", snapshot, ".zip"))

if (!base::file.exists(base::file.path(lake_dir, "catalog.duckdb"))) {
  if (!base::file.exists(zip_path)) {
    base::stop(
      "Neither an unzipped lake nor the archive was found in ", snapshot_dir,
      ". Download it from Oria first (see the header of this script)."
    )
  }

  base::message("Unzipping ", zip_path, " (about 80 GB; this takes a while).")
  status <- base::system2("unzip", base::c("-q", "-o", base::shQuote(zip_path), "-d", base::shQuote(snapshot_dir)))

  if (!base::identical(status, 0L)) {
    base::stop("unzip failed with status ", status)
  }

  if (!base::file.exists(base::file.path(lake_dir, "catalog.duckdb"))) {
    found <- base::list.files(snapshot_dir, pattern = "^catalog\\.duckdb$", recursive = TRUE, full.names = TRUE)
    base::stop("Unzipped, but no lake/catalog.duckdb. catalog.duckdb found at: ", base::paste(found, collapse = ", "))
  }
}

codebook <- load_codebook("config/codebook.csv")
connection <- trilliant_connection(lake_dir)

schema_tbl <- trilliant_schema(connection)
write_csv_atomic(schema_tbl, hpt_path("trilliant", snapshot, "schema_columns.csv"))
base::message("Lake schema written: ", hpt_path("trilliant", snapshot, "schema_columns.csv"))

reuse_stage <- base::tolower(base::Sys.getenv("HPT_REUSE_STAGE", unset = "false")) %in% base::c("1", "true", "yes")
result <- extract_trilliant(lake_dir, codebook, out_dir = hpt_path("prices", "source=trilliant"), reuse_stage = reuse_stage)

provenance <- tibble::tibble(
  source = "Trilliant Health Hospital MRF Data Directory (oria-data.trillianthealth.com)",
  snapshot = snapshot,
  archive = zip_path,
  archive_bytes = if (base::file.exists(zip_path)) base::file.size(zip_path) else NA_real_,
  lake_dir = lake_dir,
  n_price_rows = result$n_price_rows,
  n_facilities = result$n_facilities,
  codebook_sha256 = sha256_file("config/codebook.csv"),
  duckdb_cli = duckdb_cli_version(),
  extracted_at = utc_timestamp(),
  terms = "Trilliant ToS 2.2(b) attribution required; 2.3(i),(iii) no redistribution of derived rate data"
)
write_csv_atomic(provenance, hpt_path("prices", "source=trilliant", "provenance.csv"))

base::message("Done. Prices: ", result$prices_dir, " | Facilities: ", result$facilities_path)
