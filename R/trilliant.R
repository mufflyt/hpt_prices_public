#' Part A: extract target-code prices from the Trilliant Health MRF lake
#'
#' Trilliant's free "Hospital MRF Data Download" is a DuckLake (format 1.0,
#' DuckDB >= 1.5) of every hospital MRF they could download and parse. Use
#' only the consolidated download (their sanctioned bulk route; their terms
#' forbid automated scraping of the per-hospital pages, 2.3(xii)), never
#' redistribute the extracted rates (2.3(i), 2.3(iii)), and cite Trilliant
#' (2.2(b)).
#'
#' Layout after unzipping (from the download page, 2026-09-12):
#'   lake/catalog.duckdb      convenience views: current_hospitals, current_charges
#'   lake/metadata.ducklake   DuckLake catalog, attached as `lake`
#'   lake/data/main/<table>/  parquet managed by DuckLake: hospitals,
#'     hospital_versions, hospital_identity, current_hospital_versions,
#'     standard_charges, standard_charge_details, modifier_charges,
#'     modifier_charge_details
#'
#' Column names were verified on per-hospital parsed databases (2026-09-12).
#' The lake's exact column set is resolved at runtime: `trilliant_schema()`
#' lists it, and the extract stops with the list of missing columns rather
#' than guessing.

#' SQL that attaches the DuckLake catalog read-only (run with catalog.duckdb
#' opened read-only as the main database)
trilliant_init_sql <- function(lake_dir) {
  base::c(
    "INSTALL ducklake;",
    "LOAD ducklake;",
    base::sprintf(
      "ATTACH %s AS lake (DATA_PATH %s, OVERRIDE_DATA_PATH true, READ_ONLY);",
      sql_string(base::paste0("ducklake:", base::file.path(lake_dir, "metadata.ducklake"))),
      sql_string(base::file.path(lake_dir, "data"))
    )
  )
}

#' Connection spec for duckdb_query()/run_duckdb_sql()
#'
#' Accepts either an unzipped lake directory (catalog.duckdb +
#' metadata.ducklake) or a plain .duckdb file with the same tables (used by
#' the tests and for per-hospital databases).
trilliant_connection <- function(source_path) {
  if (base::dir.exists(source_path)) {
    catalog_path <- base::file.path(source_path, "catalog.duckdb")

    if (!base::file.exists(catalog_path)) {
      base::stop("Not a Trilliant lake directory (no catalog.duckdb): ", source_path)
    }

    return(base::list(database = catalog_path, init = trilliant_init_sql(source_path)))
  }

  if (!base::file.exists(source_path)) {
    base::stop("Trilliant source not found: ", source_path)
  }

  base::list(database = source_path, init = NULL)
}

trilliant_query <- function(connection, select_sql) {
  duckdb_query(select_sql, database = connection$database, read_only = TRUE, init = connection$init)
}

#' Every table/view and column visible in the lake (main catalog and `lake`)
trilliant_schema <- function(connection) {
  trilliant_query(
    connection,
    base::paste(
      "SELECT database_name, schema_name, table_name, column_name, data_type",
      "FROM duckdb_columns()",
      "WHERE database_name NOT IN ('system', 'temp')",
      "  AND schema_name NOT IN ('information_schema', 'pg_catalog')"
    )
  ) |>
    dplyr::mutate(
      relation = base::sprintf(
        "\"%s\".\"%s\".\"%s\"",
        .data$database_name, .data$schema_name, .data$table_name
      )
    )
}

#' Pick the relation to read from: the first candidate table name that exists
#' and has every required column
trilliant_pick_relation <- function(schema_tbl, table_candidates, required_cols) {
  for (table in table_candidates) {
    for (relation in base::unique(schema_tbl$relation[schema_tbl$table_name == table])) {
      cols <- schema_tbl$column_name[schema_tbl$relation == relation]

      if (base::all(required_cols %in% cols)) {
        return(base::list(relation = relation, columns = cols))
      }
    }
  }

  base::stop(
    "No Trilliant relation among {", base::paste(table_candidates, collapse = ", "),
    "} has all required columns: ", base::paste(required_cols, collapse = ", "),
    ". Inspect trilliant_schema() and update the candidates in R/trilliant.R."
  )
}

#' First available column from a list of candidates, as a SQL expression
#' (NULL literal when none exist)
col_expr <- function(available, candidates, alias_prefix, cast = "VARCHAR") {
  hit <- base::intersect(candidates, available)

  if (base::length(hit) == 0L) {
    return(base::sprintf("CAST(NULL AS %s)", cast))
  }

  base::sprintf("%s.\"%s\"", alias_prefix, hit[[1]])
}

#' Array or scalar column -> semicolon-joined string
list_to_text_expr <- function(expr) {
  base::sprintf(
    "CASE WHEN typeof(%1$s) LIKE '%%[]' THEN array_to_string(CAST(%1$s AS VARCHAR[]), ';') ELSE CAST(%1$s AS VARCHAR) END",
    expr
  )
}

sql_normalized_type <- function(expr) {
  base::sprintf("NULLIF(upper(regexp_replace(trim(CAST(%s AS VARCHAR)), '[\\s_]+', '-', 'g')), '')", expr)
}

sql_procedure_code <- function(expr) {
  base::sprintf("upper(regexp_replace(trim(CAST(%s AS VARCHAR)), '\\.0+$', ''))", expr)
}

sql_drg_code <- function(expr) {
  inner <- base::sprintf("regexp_replace(trim(CAST(%s AS VARCHAR)), '\\.0+$', '')", expr)
  base::sprintf(
    "CASE WHEN regexp_full_match(%1$s, '[0-9]+') THEN lpad(ltrim(%1$s, '0'), 3, '0') ELSE upper(%1$s) END",
    inner
  )
}

#' Canonical APR-DRG code in SQL: "NNN-S", or "NNN" with no severity
#'
#' Mirrors normalize_apr_drg_code() in R/codes.R. Both engines must agree, so
#' a change here needs the same change there (tests/testthat/test-codes.R
#' compares them on the same inputs).
sql_apr_drg_code <- function(expr) {
  cleaned <- base::sprintf(
    "regexp_replace(regexp_replace(upper(trim(CAST(%s AS VARCHAR))), '\\bSOI\\b|\\bSEVERITY\\b|\\bAPR[- ]?DRG\\b', '', 'g'), '[\\s._]+', '-', 'g')",
    expr
  )
  trimmed <- base::sprintf("regexp_replace(regexp_replace(%s, '^-+', ''), '-+$', '')", cleaned)
  base::sprintf(
    base::paste0(
      "CASE WHEN regexp_full_match(%1$s, '[0-9]{1,3}-[1-4]') ",
      "THEN lpad(regexp_extract(%1$s, '^([0-9]{1,3})', 1), 3, '0') || '-' || regexp_extract(%1$s, '-([1-4])$', 1) ",
      "WHEN regexp_full_match(%1$s, '[0-9]{3}[1-4]') ",
      "THEN substr(%1$s, 1, 3) || '-' || substr(%1$s, 4, 1) ",
      "WHEN regexp_full_match(%1$s, '[0-9]{1,3}') THEN lpad(%1$s, 3, '0') ",
      "ELSE %1$s END"
    ),
    trimmed
  )
}

#' SQL VALUES table of the codebook
codebook_values_sql <- function(codebook) {
  rows <- base::sprintf(
    "(%s, %s, %s, %s)",
    sql_string(codebook$code_family), sql_string(codebook$code),
    sql_string(codebook$concept), sql_string(codebook$code_system)
  )

  base::sprintf(
    "(VALUES %s) AS cb(code_family, code, concept, code_system)",
    base::paste(rows, collapse = ", ")
  )
}

#' Resolve the lake's relations and the SQL that joins them
#'
#' Verified against the 2026-07-21 lake: payer-level rates are in the
#' `current_charge_details` view (not `current_charges`, which only holds
#' per-service summaries); facilities are `current_hospitals`, keyed by
#' `internal_id` + `run_date` (`hospital_id` is 1 everywhere); the file URL
#' and content hash are in `lake.hospital_versions`; the facility NPI and
#' license state are in `lake.hospital_identity`. Older per-hospital parsed
#' databases (and the test fixture) key on `hospital_id` and keep mrf_url/md5
#' on the hospitals table; both layouts resolve here.
trilliant_relations <- function(schema_tbl) {
  details <- trilliant_pick_relation(
    schema_tbl,
    base::c("current_charge_details", "standard_charge_details"),
    base::c("payer_name", "plan_name", "cpt", "hcpcs", "ms_drg")
  )
  hospitals <- trilliant_pick_relation(
    schema_tbl,
    base::c("current_hospitals", "hospitals"),
    base::c("hospital_name")
  )

  shared <- base::intersect(details$columns, hospitals$columns)
  key <- base::intersect(base::c("internal_id", "hospital_version_id", "hospital_id"), shared)

  if (base::length(key) == 0L) {
    base::stop("Cannot join Trilliant details to hospitals: no shared internal_id/hospital_version_id/hospital_id column.")
  }

  optional <- function(table, required) {
    base::tryCatch(trilliant_pick_relation(schema_tbl, table, required), error = function(e) NULL)
  }

  base::list(
    details = details,
    hospitals = hospitals,
    key = key[[1]],
    by_run_date = "run_date" %in% shared,
    versions = optional("hospital_versions", base::c("internal_id", "run_date", "mrf_url")),
    identity = optional("hospital_identity", base::c("internal_id", "npi"))
  )
}

#' One row per facility with its file-level fields (the `fac` CTE body)
trilliant_facility_cte <- function(rel) {
  h <- function(candidates, cast = "VARCHAR") col_expr(rel$hospitals$columns, candidates, "h", cast)
  use_versions <- !base::is.null(rel$versions) && rel$key == "internal_id"
  use_identity <- !base::is.null(rel$identity) && rel$key == "internal_id"
  v <- function(candidates, cast = "VARCHAR") {
    if (use_versions) col_expr(rel$versions$columns, candidates, "v", cast) else base::sprintf("CAST(NULL AS %s)", cast)
  }
  i <- function(candidates, cast = "VARCHAR") {
    if (use_identity) col_expr(rel$identity$columns, candidates, "i", cast) else base::sprintf("CAST(NULL AS %s)", cast)
  }
  not_provided <- function(expr) base::sprintf("NULLIF(NULLIF(trim(%s), 'Not provided'), '')", expr)

  base::paste0(
    "SELECT\n",
    "    h.\"", rel$key, "\" AS fkey,\n",
    "    'trilliant:' || CAST(h.\"", rel$key, "\" AS VARCHAR) AS facility_key,\n",
    "    coalesce(", v("mrf_content_hash"), ", ", h(base::c("md5", "mrf_md5", "file_md5")), ", ",
    v("mrf_url"), ", ", h(base::c("mrf_url", "source_url")), ", 'hospital:' || CAST(h.\"", rel$key, "\" AS VARCHAR)) AS mrf_file_id,\n",
    "    coalesce(", v("mrf_url"), ", ", h(base::c("mrf_url", "source_url")), ") AS mrf_url,\n",
    "    coalesce(", h("version"), ", ", v("mrf_schema_version"), ") AS file_version,\n",
    "    CAST(", h("last_updated_on"), " AS VARCHAR) AS last_updated_on,\n",
    "    coalesce(CAST(", v("mrf_data_downloaded"), " AS VARCHAR), CAST(", h(base::c("data_downloaded", "downloaded_at", "downloaded")),
    " AS VARCHAR), CAST(", h("run_date"), " AS VARCHAR)) AS retrieved_at,\n",
    "    ", h(base::c("hospital_name", "mrf_hospital_name")), " AS hospital_name,\n",
    "    ", h(base::c("location_name", "hpt_hospital_name", "hospital_location")), " AS location_name,\n",
    "    ", not_provided(list_to_text_expr(h(base::c("enriched_hospital_address", "hospital_address")))), " AS address,\n",
    "    ", not_provided(h(base::c("enriched_hospital_city", "hospital_city"))), " AS city,\n",
    "    ", h(base::c("hospital_state", "state")), " AS state,\n",
    "    coalesce(", h("license_number"), ", ", i("license_number"), ") AS license_number,\n",
    "    coalesce(", i("license_state"), ", ", h(base::c("license_state", "hospital_state")), ") AS license_state,\n",
    "    NULLIF(concat_ws(';', NULLIF(", list_to_text_expr(h("type_2_npi")), ", ''), ", i("npi"), "), '') AS type_2_npi\n",
    "  FROM ", rel$hospitals$relation, " AS h\n",
    if (use_versions) base::paste0("  LEFT JOIN ", rel$versions$relation, " AS v ON v.internal_id = h.internal_id AND v.run_date = h.run_date\n"),
    if (use_identity) base::paste0("  LEFT JOIN ", rel$identity$relation, " AS i ON i.internal_id = h.internal_id\n")
  )
}

#' Stage 1 of the extract: stream the lake once, keeping only charge lines
#' that carry a target code, read through one representative facility per
#' file. A filter plus a small hash join, so memory stays bounded however
#' large the lake is; the heavier matching and de-duplication then run on
#' this much smaller staged table.
#' @param details_relation SQL relation to read charge lines from, replacing
#'   the lake table. The batched stage passes a `read_parquet([...])` over one
#'   batch of the table's own data files; everything else is unchanged.
trilliant_stage_sql <- function(schema_tbl, codebook, details_relation = NULL) {
  rel <- trilliant_relations(schema_tbl)
  details_relation <- details_relation %||% rel$details$relation
  procedure_codes <- codebook$code[codebook$code_family == "procedure"]
  drg_codes <- codebook$code[codebook$code_family == "ms_drg"]
  # APR-DRGs live only in other_code1/2, never in the typed ms_drg column:
  # that column's name declares the grouper, and it is not this one.
  apr_codes <- codebook$code[codebook$code_family == "apr_drg"]
  other_cols <- base::intersect(base::c("other_code1", "other_code2"), rel$details$columns)

  prefilter <- base::c(
    base::sprintf("%s IN (%s)", sql_procedure_code("d.cpt"), sql_string_list(procedure_codes)),
    base::sprintf("%s IN (%s)", sql_procedure_code("d.hcpcs"), sql_string_list(procedure_codes)),
    base::sprintf("%s IN (%s)", sql_drg_code("d.ms_drg"), sql_string_list(drg_codes)),
    base::unlist(base::lapply(other_cols, function(col) {
      base::c(
        base::sprintf("%s IN (%s)", sql_procedure_code(base::paste0("d.", col)), sql_string_list(procedure_codes)),
        base::sprintf("%s IN (%s)", sql_drg_code(base::paste0("d.", col)), sql_string_list(drg_codes)),
        if (base::length(apr_codes)) {
          base::sprintf("%s IN (%s)", sql_apr_drg_code(base::paste0("d.", col)), sql_string_list(apr_codes))
        }
      )
    }))
  )

  base::paste0(
    "WITH fac AS (\n  ", trilliant_facility_cte(rel), "),\n",
    "rep AS (\n",
    "  SELECT mrf_file_id, arg_min(fkey, facility_key) AS fkey, min(mrf_url) AS mrf_url, min(file_version) AS file_version,\n",
    "         min(last_updated_on) AS last_updated_on, min(retrieved_at) AS retrieved_at\n",
    "  FROM fac GROUP BY mrf_file_id\n",
    ")\n",
    "SELECT r.mrf_file_id, r.mrf_url AS f_mrf_url, r.file_version AS f_version,\n",
    "       r.last_updated_on AS f_last_updated_on, r.retrieved_at AS f_retrieved_at, d.* EXCLUDE (", rel$key, ")\n",
    "FROM ", details_relation, " AS d\n",
    "JOIN rep AS r ON d.\"", rel$key, "\" = r.fkey\n",
    "WHERE ", base::paste(prefilter, collapse = "\n   OR ")
  )
}


# ---- resumable stage 1 ------------------------------------------------------------

#' The parquet files behind one DuckLake table, when reading them is complete
#'
#' A DuckLake table is a set of parquet files listed in `ducklake_data_file`,
#' plus two things that would make reading those files alone WRONG: delete
#' files (rows logically removed) and inlined data (small rows kept in the
#' metadata database rather than in parquet). This returns NULL rather than a
#' partial answer when either exists, and the caller falls back to scanning the
#' table through DuckLake itself.
#'
#' Reading the files directly is what makes stage 1 resumable: the scan can
#' stop after any file and pick up there, instead of losing hours of work.
#'
#' @param lake_dir the unzipped lake directory (holding metadata.ducklake).
#' @param table_name the DuckLake table, e.g. "standard_charge_details".
#' @return tibble(data_file_id, path, n_rows, bytes) ordered by id, or NULL.
trilliant_lake_data_files <- function(lake_dir, table_name) {
  metadata <- base::file.path(lake_dir, "metadata.ducklake")
  if (!base::file.exists(metadata)) return(NULL)

  ask <- function(sql) base::tryCatch(duckdb_query(sql, database = metadata, read_only = TRUE), error = function(e) NULL)

  ids <- ask(base::sprintf("SELECT table_id FROM ducklake_table WHERE table_name = %s", sql_string(table_name)))
  if (base::is.null(ids) || base::nrow(ids) != 1L) return(NULL)
  table_id <- ids$table_id[[1]]

  deletes <- ask(base::sprintf("SELECT count(*) AS n FROM ducklake_delete_file WHERE table_id = %s", table_id))
  if (base::is.null(deletes) || deletes$n[[1]] > 0) return(NULL)

  # DuckLake registers an inlined-data table for EVERY table, empty or not, so
  # the question is whether that table holds rows, not whether it is named.
  inlined_name <- ask(base::sprintf(
    "SELECT table_name FROM ducklake_inlined_data_tables WHERE table_id = %s", table_id))
  if (!base::is.null(inlined_name) && base::nrow(inlined_name) > 0L) {
    for (nm in inlined_name$table_name) {
      rows <- ask(base::sprintf("SELECT count(*) AS n FROM %s", nm))
      if (base::is.null(rows) || rows$n[[1]] > 0) return(NULL)
    }
  }

  files <- ask(base::sprintf(
    base::paste0(
      "SELECT f.data_file_id, f.path, f.path_is_relative, f.record_count AS n_rows, f.file_size_bytes AS bytes ",
      "FROM ducklake_data_file f JOIN ducklake_table t USING (table_id) ",
      "WHERE t.table_name = %s ORDER BY f.data_file_id"
    ), sql_string(table_name)))
  if (base::is.null(files) || base::nrow(files) == 0L) return(NULL)

  absolute <- base::ifelse(
    files$path_is_relative %in% base::c(TRUE, "true", 1L),
    base::file.path(lake_dir, "data", "main", table_name, files$path),
    files$path
  )
  if (!base::all(base::file.exists(absolute))) return(NULL)

  tibble::tibble(data_file_id = files$data_file_id, path = absolute,
                 n_rows = base::as.numeric(files$n_rows), bytes = base::as.numeric(files$bytes))
}

#' Which stage batches are already written, from the manifest
#'
#' The manifest is appended only after a batch's parquet part is closed, so a
#' part interrupted mid-write is simply redone: its name is derived from the
#' batch's first data file, so the rerun overwrites it.
read_stage_manifest <- function(manifest_path) {
  if (!base::file.exists(manifest_path)) {
    return(tibble::tibble(part = base::character(), data_file_id = base::numeric(),
                          n_rows = base::numeric(), written_at = base::character()))
  }
  readr::read_csv(manifest_path, col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE) |>
    dplyr::mutate(data_file_id = base::as.numeric(.data$data_file_id), n_rows = base::as.numeric(.data$n_rows))
}

#' Split data files into batches that each become one stage part
stage_file_batches <- function(files, batch_size) {
  batch_size <- base::max(1L, base::as.integer(batch_size))
  base::split(files, base::ceiling(base::seq_len(base::nrow(files)) / batch_size))
}

#' Build the extraction SQL
#'
#' One row per (file, charge line, payer/plan, target code). Codes are read
#' from Trilliant's typed columns (cpt, hcpcs, ms_drg; the type is implied
#' by the column) and from other_code1/2, where the declared `_type` must
#' pass the same gate as R/codes.R. cdm/rc/ndc/icd are never matched.
#'
#' Facilities that share one MRF reference identical charge rows (lake
#' README), so rates are read through ONE representative facility per file
#' (`rep`), which is exact and roughly halves the scan. Every source
#' charge-line row is kept.
trilliant_extract_sql <- function(schema_tbl, codebook, stage_path = NULL) {
  rel <- trilliant_relations(schema_tbl)
  details <- rel$details
  d <- function(candidates, cast = "VARCHAR") col_expr(details$columns, candidates, "d", cast)

  procedure_codes <- codebook$code[codebook$code_family == "procedure"]
  drg_codes <- codebook$code[codebook$code_family == "ms_drg"]
  # APR-DRGs live only in other_code1/2, never in the typed ms_drg column:
  # that column's name declares the grouper, and it is not this one.
  apr_codes <- codebook$code[codebook$code_family == "apr_drg"]
  other_cols <- base::intersect(base::c("other_code1", "other_code2"), details$columns)

  prefilter <- base::c(
    base::sprintf("%s IN (%s)", sql_procedure_code("d.cpt"), sql_string_list(procedure_codes)),
    base::sprintf("%s IN (%s)", sql_procedure_code("d.hcpcs"), sql_string_list(procedure_codes)),
    base::sprintf("%s IN (%s)", sql_drg_code("d.ms_drg"), sql_string_list(drg_codes)),
    base::unlist(base::lapply(other_cols, function(col) {
      base::c(
        base::sprintf("%s IN (%s)", sql_procedure_code(base::paste0("d.", col)), sql_string_list(procedure_codes)),
        base::sprintf("%s IN (%s)", sql_drg_code(base::paste0("d.", col)), sql_string_list(drg_codes)),
        if (base::length(apr_codes)) {
          base::sprintf("%s IN (%s)", sql_apr_drg_code(base::paste0("d.", col)), sql_string_list(apr_codes))
        }
      )
    }))
  )

  code_branches <- base::c(
    base::sprintf("SELECT row_key, 'procedure' AS code_family, %s AS code, 'CPT' AS code_type FROM filtered WHERE cpt IS NOT NULL", sql_procedure_code("cpt")),
    base::sprintf("SELECT row_key, 'procedure', %s, 'HCPCS' FROM filtered WHERE hcpcs IS NOT NULL", sql_procedure_code("hcpcs")),
    base::sprintf("SELECT row_key, 'ms_drg', %s, 'MS-DRG' FROM filtered WHERE ms_drg IS NOT NULL", sql_drg_code("ms_drg")),
    base::unlist(base::lapply(other_cols, function(col) {
      type_expr <- sql_normalized_type(base::paste0(col, "_type"))
      base::c(
        base::sprintf("SELECT row_key, 'procedure', %s, %s FROM filtered WHERE %s IS NOT NULL", sql_procedure_code(col), type_expr, col),
        base::sprintf("SELECT row_key, 'ms_drg', %s, %s FROM filtered WHERE %s IS NOT NULL", sql_drg_code(col), type_expr, col),
        if (base::length(apr_codes)) {
          base::sprintf("SELECT row_key, 'apr_drg', %s, %s FROM filtered WHERE %s IS NOT NULL", sql_apr_drg_code(col), type_expr, col)
        }
      )
    }))
  )

  fit_sql <- base::sprintf(
    base::paste(
      # the APR arm comes first for the same reason as in code_type_fit():
      # an untyped three-digit code is never an APR-DRG
      "CASE WHEN c.code_family = 'apr_drg' AND c.code_type IN (%s) THEN 'verified'",
      "WHEN c.code_family = 'apr_drg' THEN 'incompatible'",
      "WHEN c.code_type IS NULL THEN 'unverified'",
      "WHEN c.code_family = 'procedure' AND c.code_type IN (%s) THEN 'verified'",
      "WHEN c.code_family = 'ms_drg' AND c.code_type IN (%s) THEN 'verified'",
      "WHEN c.code_family = 'ms_drg' AND c.code_type = 'DRG' THEN 'unverified'",
      "ELSE 'incompatible' END"
    ),
    sql_string_list(apr_drg_code_types()),
    sql_string_list(procedure_code_types()),
    sql_string_list(drg_code_types())
  )

  stage_relation <- if (base::is.null(stage_path)) {
    base::paste0("(", trilliant_stage_sql(schema_tbl, codebook), ")")
  } else {
    base::sprintf("read_parquet(%s)", sql_string(stage_path))
  }

  base::paste0(
    "WITH ",
    "filtered AS (\n",
    "  SELECT row_number() OVER () AS row_key, * FROM ", stage_relation, "\n",
    "),\n",
    "codes AS (\n  ", base::paste(code_branches, collapse = "\n  UNION ALL "), "\n),\n",
    "matched AS (\n",
    "  SELECT DISTINCT c.row_key, c.code, c.code_type, cb.concept, cb.code_system,\n",
    "         (", fit_sql, ") = 'verified' AS type_verified\n",
    "  FROM codes AS c\n",
    "  JOIN ", codebook_values_sql(codebook), " ON cb.code_family = c.code_family AND cb.code = c.code\n",
    "  WHERE (", fit_sql, ") <> 'incompatible'\n",
    ")\n",
    # No DISTINCT here: shared files are already read once (through `rep`),
    # and distinct source charge lines can carry identical payer/plan/rate
    # values (HCA lists MS-DRG 742 on 69 lines); collapsing them changed a
    # hospital's pooled median. State medians neutralize repeats instead.
    "SELECT\n",
    "  'trilliant' AS source, d.f_mrf_url AS mrf_url, d.mrf_file_id, d.f_version AS file_version,\n",
    "  d.f_last_updated_on AS last_updated_on, d.f_retrieved_at AS retrieved_at,\n",
    "  m.concept, m.code, m.code_system, m.code_type, m.type_verified,\n",
    "  ", d("description"), " AS description,\n",
    "  ", d("modifiers"), " AS modifiers,\n",
    "  ", d(base::c("setting", "enriched_setting")), " AS setting,\n",
    "  ", d(base::c("billing_class", "enriched_billing_class")), " AS billing_class,\n",
    "  ", d("gross_charge", "DOUBLE"), " AS gross,\n",
    "  ", d("discounted_cash", "DOUBLE"), " AS discounted_cash,\n",
    "  ", d("minimum", "DOUBLE"), " AS min,\n",
    "  ", d("maximum", "DOUBLE"), " AS max,\n",
    "  d.payer_name, d.plan_name,\n",
    "  ", d(base::c("enriched_payer_type", "payer_type")), " AS payer_type,\n",
    "  ", d("standard_charge_dollar", "DOUBLE"), " AS negotiated_dollar,\n",
    "  ", d("standard_charge_percentage", "DOUBLE"), " AS negotiated_percentage,\n",
    "  ", d("standard_charge_algorithm"), " AS negotiated_algorithm,\n",
    "  ", d(base::c("methodology", "enriched_methodology")), " AS methodology,\n",
    "  ", d("median_amount", "DOUBLE"), " AS median_amount,\n",
    "  ", d(base::c("percentile_10th", "10th_percentile"), "DOUBLE"), " AS p10,\n",
    "  ", d(base::c("percentile_90th", "90th_percentile"), "DOUBLE"), " AS p90,\n",
    "  CAST(", d("count"), " AS VARCHAR) AS count,\n",
    "  ", d("estimated_amount", "DOUBLE"), " AS estimated_amount,\n",
    "  concat_ws(' | ', ", d("additional_generic_notes"), ", ", d("additional_payer_notes"), ") AS notes,\n",
    "  ", d("charge_seq"), " AS charge_seq, ", d("payer_seq"), " AS payer_seq\n",
    "FROM matched AS m\n",
    "JOIN filtered AS d ON d.row_key = m.row_key"
  ) |>
    (\(inner) base::paste0("SELECT * EXCLUDE (charge_seq, payer_seq) FROM (", inner, ")"))()
}

#' Facility-level records (one per Trilliant facility) for the CCN crosswalk
#' in R/ccn_match.R, each carrying its file's mrf_file_id
trilliant_facilities_sql <- function(schema_tbl) {
  rel <- trilliant_relations(schema_tbl)
  base::paste0(
    "WITH fac AS (\n  ", trilliant_facility_cte(rel), ")\n",
    "SELECT DISTINCT facility_key, 'trilliant' AS source, mrf_url, mrf_file_id, hospital_name, location_name,\n",
    "       address, city, state, license_number, license_state, type_2_npi\n",
    "FROM fac"
  )
}

#' The charge-details table's own name, as the lake spells it
trilliant_table_name <- function(schema_tbl) {
  relation <- trilliant_relations(schema_tbl)$details$relation
  base::gsub('"', "", utils::tail(base::strsplit(relation, ".", fixed = TRUE)[[1]], 1L))
}

#' Stage 1, one part per batch of lake files, resuming where it stopped
#'
#' Each part is written whole or not at all: the manifest is appended only
#' after DuckDB closes the file, and a part's name comes from its batch's first
#' data file, so an interrupted batch is rewritten rather than half-read next
#' time. The scan also reports files done and rows kept as it goes, which the
#' single scan could not: its only progress signal was a staging file's size.
#'
#' @return a glob the matching stage reads as one relation.
stage_in_batches <- function(data_files, schema_tbl, codebook, connection, settings,
                             parts_dir, manifest_path, batch_size) {
  base::dir.create(parts_dir, recursive = TRUE, showWarnings = FALSE)
  done <- read_stage_manifest(manifest_path)
  batches <- stage_file_batches(data_files, batch_size)
  total_rows <- base::sum(data_files$n_rows, na.rm = TRUE)

  base::message(base::sprintf(
    "Stage 1: %s lake files (%.1f billion rows) in %s batches of %s; resumable, %s file(s) already done.",
    base::nrow(data_files), total_rows / 1e9, base::length(batches), batch_size,
    base::sum(data_files$data_file_id %in% done$data_file_id)
  ))

  for (i in base::seq_along(batches)) {
    batch <- batches[[i]]
    if (base::all(batch$data_file_id %in% done$data_file_id)) next

    part <- base::file.path(parts_dir, base::sprintf("part_%012.0f.parquet", batch$data_file_id[[1]]))
    relation <- base::sprintf("read_parquet([%s])", base::paste(sql_string(batch$path), collapse = ", "))
    started <- base::Sys.time()
    run_duckdb_sql(
      base::c(settings, base::sprintf("COPY (%s) TO %s (FORMAT parquet);",
                                      trilliant_stage_sql(schema_tbl, codebook, details_relation = relation),
                                      sql_string(part))),
      database = connection$database, read_only = TRUE, init = connection$init
    )
    kept <- duckdb_query(base::sprintf("SELECT count(*) AS n FROM read_parquet(%s)", sql_string(part)),
                         database = connection$database, read_only = TRUE)$n[[1]]

    readr::write_csv(
      tibble::tibble(part = base::basename(part), data_file_id = batch$data_file_id,
                     n_rows = kept, written_at = utc_timestamp()),
      manifest_path, append = base::file.exists(manifest_path)
    )
    done <- read_stage_manifest(manifest_path)
    base::message(base::sprintf(
      "  batch %s/%s: %s file(s), %s target lines kept (%.1f min); %.0f%% of the lake scanned",
      i, base::length(batches), base::nrow(batch), base::format(kept, big.mark = ","),
      base::as.numeric(base::difftime(base::Sys.time(), started, units = "mins")),
      100 * base::sum(data_files$n_rows[data_files$data_file_id %in% done$data_file_id], na.rm = TRUE) / total_rows
    ))
  }

  base::file.path(parts_dir, "part_*.parquet")
}

#' Run the Part A extract
#'
#' @param source_path Unzipped lake directory, or a .duckdb file with the
#'   same tables.
#' @param codebook From [load_codebook()].
#' @param out_dir Output directory (default `HPT_DATA_DIR/prices/source=trilliant`).
#' @param reuse_stage Reuse an existing stage-1 file (the 55-minute lake
#'   scan) and rerun only the matching step. Only valid if the codebook's
#'   codes have not changed since the stage was written.
#' @return list(prices_dir, facilities_path, n_price_rows, n_facilities).
#' @param stage_batch_files How many lake data files each stage-1 part covers
#'   (`HPT_STAGE_BATCH`, default 20). Stage 1 writes one part per batch and
#'   records it in a manifest, so an interrupted scan resumes at the next
#'   unfinished batch instead of starting over. 0 restores the single scan.
extract_trilliant <- function(source_path, codebook, out_dir = NULL, reuse_stage = FALSE,
                              stage_batch_files = base::as.integer(base::Sys.getenv("HPT_STAGE_BATCH", unset = "20"))) {
  require_duckdb_cli("1.5.0")
  out_dir <- out_dir %||% hpt_path("prices", "source=trilliant")
  connection <- trilliant_connection(source_path)

  base::message("Reading Trilliant schema: ", source_path)
  schema_tbl <- trilliant_schema(connection)

  facilities_path <- base::file.path(out_dir, "facilities.parquet")
  prices_dir <- base::file.path(out_dir, "prices")
  base::dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  base::unlink(prices_dir, recursive = TRUE)

  stage_path <- base::file.path(out_dir, "stage_target_lines.parquet")
  spill_dir <- hpt_path("duckdb_tmp")
  settings <- base::c(
    "SET preserve_insertion_order = false;",
    base::sprintf("SET memory_limit = %s;", sql_string(base::Sys.getenv("HPT_DUCKDB_MEMORY", unset = "5GB"))),
    base::sprintf("SET threads = %s;", base::Sys.getenv("HPT_DUCKDB_THREADS", unset = "3")),
    base::sprintf("SET temp_directory = %s;", sql_string(spill_dir))
  )

  parts_dir <- base::file.path(out_dir, "stage_parts")
  manifest_path <- base::file.path(parts_dir, "_manifest.csv")
  data_files <- if (base::is.na(stage_batch_files) || stage_batch_files < 1L) {
    NULL
  } else {
    trilliant_lake_data_files(source_path, trilliant_table_name(schema_tbl))
  }

  if (reuse_stage && base::file.exists(stage_path)) {
    base::message("Stage 1: reusing ", stage_path, " (reuse_stage = TRUE).")
    stage_source <- stage_path
  } else if (!base::is.null(data_files)) {
    stage_source <- stage_in_batches(
      data_files = data_files, schema_tbl = schema_tbl, codebook = codebook, connection = connection,
      settings = settings, parts_dir = parts_dir, manifest_path = manifest_path, batch_size = stage_batch_files
    )
  } else {
    base::message("Stage 1: streaming the lake for target-code lines (one scan; not resumable).")
    run_duckdb_sql(
      base::c(settings, base::sprintf("COPY (%s) TO %s (FORMAT parquet);", trilliant_stage_sql(schema_tbl, codebook), sql_string(stage_path))),
      database = connection$database, read_only = TRUE, init = connection$init
    )
    stage_source <- stage_path
  }

  base::message("Stage 2: matching codes and collapsing shared files.")
  run_duckdb_sql(
    base::c(
      settings,
      base::sprintf(
        "COPY (%s) TO %s (FORMAT parquet, PARTITION_BY (concept));",
        trilliant_extract_sql(schema_tbl, codebook, stage_path = stage_source),
        sql_string(prices_dir)
      ),
      base::sprintf(
        "COPY (%s) TO %s (FORMAT parquet);",
        trilliant_facilities_sql(schema_tbl),
        sql_string(facilities_path)
      )
    ),
    database = connection$database,
    read_only = TRUE,
    init = connection$init
  )

  counts <- duckdb_query(base::sprintf(
    "SELECT (SELECT count(*) FROM read_parquet(%s)) AS n_price_rows, (SELECT count(*) FROM read_parquet(%s)) AS n_facilities",
    sql_string(base::file.path(prices_dir, "**", "*.parquet")),
    sql_string(facilities_path)
  ))

  base::message(
    "Trilliant extract: ", scales::comma(counts$n_price_rows), " price rows, ",
    scales::comma(counts$n_facilities), " facility records."
  )

  base::list(
    prices_dir = prices_dir,
    facilities_path = facilities_path,
    n_price_rows = counts$n_price_rows,
    n_facilities = counts$n_facilities
  )
}

#' Read extracted Trilliant prices back as a canonical tibble (optionally
#' only some concepts, pushed down to the Parquet partitions)
read_trilliant_prices <- function(prices_dir, concepts = NULL) {
  dataset <- arrow::open_dataset(prices_dir, partitioning = arrow::hive_partition(concept = arrow::utf8()))

  if (!base::is.null(concepts)) {
    dataset <- dplyr::filter(dataset, .data$concept %in% concepts)
  }

  conform_price_table(dplyr::collect(dataset))
}
