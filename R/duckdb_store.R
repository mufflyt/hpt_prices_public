#' Build the analysis database (HPT_DATA_DIR/hpt.duckdb)
#'
#' A star schema, written by the DuckDB CLI in v1.4.0 storage format so the
#' installed R duckdb package (1.4.4) can open it directly with DBI.
#'
#'   dim_code         one row per codebook code (code_id SMALLINT)
#'   dim_hospital     one row per CMS CCN (CMS roster + AHRQ health system)
#'   dim_file         one row per MRF file (file_id INTEGER), with provenance
#'   bridge_file_ccn  which CCNs each file covers (from the CCN crosswalk)
#'   dim_payer        one row per distinct payer/plan text, with payer_type
#'   fact_rate        one row per file x charge line x code x payer/plan
#'   ref_code_gross   typical facility / professional gross per code, the
#'                    blank-billing-class cutoff, and case-line thresholds
#'
#' Space and speed choices:
#' - integer surrogate keys; low-cardinality text as ENUM types (setting,
#'   billing class, methodology, payer type, concept, source), which DuckDB
#'   stores as 1-byte codes and compares without string work;
#' - fact_rate is written sorted by (code_id, file_id), so each row group's
#'   min/max zone map lets DuckDB skip everything outside the codes a query
#'   filters on;
#' - payer/plan text lives once in dim_payer instead of on every rate row;
#' - the implausible-value rule is applied once at load time as a
#'   `plausible` flag, so every downstream query uses the same definition;
#' - likewise the fee type (`fee_type`, inferred from gross where the billing
#'   class is blank) and the operating-room case-line rule for outpatient
#'   procedures (`case_line`); thresholds in ref_code_gross, rules at
#'   case_line_multiple().

hpt_database_path <- function() {
  hpt_path("hpt.duckdb")
}

#' Strip literal surrounding double quotes and blank to NULL
unquote_sql <- function(column) {
  base::sprintf("NULLIF(trim(regexp_replace(CAST(%s AS VARCHAR), '^\\s*\"+|\"+\\s*$', '', 'g')), '')", column)
}

enum_sql <- function(values) {
  base::paste0("ENUM (", sql_string_list(values), ")")
}

#' Values allowed for the ENUM columns (anything else loads as 'unknown')
hpt_enum_values <- function() {
  base::list(
    setting = base::c("outpatient", "inpatient", "both", "unknown"),
    billing_class = base::c("facility", "professional", "both", "unknown"),
    methodology = base::c("case rate", "fee schedule", "percent of total billed charges", "per diem", "other", "unknown"),
    source = base::c("trilliant", "own_crawl"),
    payer_type = payer_types()
  )
}

#' Normalize a free-text enum-like SQL column to one of `values`
enum_normalize_sql <- function(expr, values, aliases = NULL) {
  cleaned <- base::sprintf("lower(trim(CAST(%s AS VARCHAR)))", expr)
  alias_whens <- if (base::length(aliases) > 0L) {
    base::paste(base::sprintf("WHEN %s = %s THEN %s", cleaned, sql_string(base::names(aliases)), sql_string(aliases)), collapse = " ")
  } else {
    ""
  }

  base::sprintf(
    "CASE %s WHEN %s IN (%s) THEN %s ELSE 'unknown' END",
    alias_whens, cleaned, sql_string_list(values), cleaned
  )
}

#' Plausibility rule for a negotiated dollar amount, applied at load
#'
#' Drops placeholders and unit errors seen in MRFs: amounts of $1 or less
#' ("0.01" sentinels), 9-filled values (999999.99), and amounts above
#' $2,000,000. Rows are kept and flagged, not deleted.
plausible_rate_sql <- function(expr) {
  base::sprintf(
    "(%1$s IS NOT NULL AND %1$s > 1 AND %1$s < 2000000 AND NOT regexp_matches(CAST(CAST(%1$s AS DECIMAL(18,2)) AS VARCHAR), '^9{5,}(\\.9+)?$'))",
    expr
  )
}

#' Typical gross charge per code and fee type (table ref_code_gross)
#'
#' For each code, the typical facility and professional gross charge is the
#' median across files of each file's lowest gross for that code, taken only
#' from rows whose billing class says so explicitly (outpatient settings).
#' Two load-time rules use it.
#'
#' Fee type. Most files leave billing class blank, and a blank used to count
#' as facility, which let professional fees into facility medians ("45378 -
#' PF COLONOSCOPY", "HOSPITALIST BP 45378", or plain "DIAGNOSTIC COLONOSCOPY"
#' priced at physician-fee level, a fraction of the OPPS facility rate). A blank-class row
#' with a gross charge below the geometric midpoint of the two typical gross
#' charges is now professional (`fee_type_inferred`). This is done only when
#' each typical rests on at least `fee_type_min_files` files and the facility
#' typical is at least `fee_type_min_separation()` times the professional one.
#' Validated against the Medicare Advantage rate on the same blank-class lines
#' (a line is professional when its MA rate is below the geometric midpoint
#' of the national MA facility and professional rates): the gross rule
#' classified more lines consistently than "blank = facility" for the
#' colonoscopy codes and D&C, but not for EMB, whose facility and
#' professional gross charges separate by less than 2.5x, hence the floor
#' (EMB and IUD blank rows stay facility). The rule also moves some true
#' facility lines to professional, so analyses drop inferred professional
#' rows from facility fees but do not count them as professional fees either
#' (rate_row_filter_sql()). Numbers: docs/appendix.md, section E2, in the
#' private repository.
#'
#' Case line. Some hospitals list an office procedure twice: a clinic line
#' and an operating-room case line carrying the same CPT code (some files list
#' 58300 at a clinic gross charge and again as "INSERT INTRAUTERINE DEVICE" at
#' a gross charge two orders of magnitude higher). A charge line of an
#' outpatient procedure whose gross
#' exceeds `case_line_multiple()` times the typical gross for its fee type is
#' flagged `case_line`. Why 10x: the 58300 facility gross distribution
#' (2026-07-21 snapshot) has a clinic mode and a separate mode of OR lines,
#' and 10x the typical gross falls in the trough between them (docs/appendix.md,
#' section E3, in the private repository). A within-file rule (5x the file's lowest line) was rejected because it
#' flagged ordinary colonoscopy lines whose negotiated rates were no higher
#' than the rest.
#'
#' Rows are kept and flagged, not deleted.
case_line_multiple <- function() {
  10
}

fee_type_min_separation <- function() {
  2.5
}

#' Per-diem rates for inpatient DRGs
#'
#' A per-diem contract prices one day of the stay, so a hospital whose
#' contracts are per diem looked several times cheaper than one paid by case
#' rate for the same DRG. Per-diem rows are a small share of MS-DRG rows but
#' appear in a sizable share of files, so they move hospital medians. At
#' load, a per-diem rate for an MS-DRG code becomes a stay price,
#' `case_dollar` = rate x the DRG's geometric mean length of stay (CMS IPPS
#' Table 5; 2.0 days for DRG 807, 2.9 for DRG 788), and `per_diem_converted`
#' marks it. The listed rate stays in `negotiated_dollar`; medians, payer
#' ratios, and ownership prices use `case_dollar`, which equals
#' `negotiated_dollar` for every other row.
#'
#' Following Turquoise Health's delivery-price method (pricepoints,
#' 2025_04_delivery_costs), a "per diem" rate at or above
#' `per_diem_case_multiple()` (3) times Medicare's national per-day payment
#' for the DRG (standard IPPS payment at wage index 1 / length of stay) is
#' taken as a stay price mislabeled per diem: it is not multiplied, and
#' `per_diem_as_case` marks it.
per_diem_case_multiple <- function() {
  3
}

per_diem_note <- function() {
  "per-diem MS-DRG rates x CMS Table 5 geometric mean LOS"
}

#' USPS state and territory codes a file's state may take
valid_state_codes <- function() {
  base::c(datasets::state.abb, "DC", "PR", "VI", "GU", "AS", "MP")
}

#' A facility's two-letter state, validated
#'
#' Trilliant's hospital_state is parsed from the address and sometimes holds
#' a street token instead ("PO" from "PO Box 99", "NW" and "SE" from street
#' quadrants). Only USPS codes are kept; otherwise the code before the ZIP
#' at the end of the address ("..., KS, 67547") is used; otherwise NULL.
clean_state_sql <- function(state_col, address_col = NULL) {
  valid <- sql_string_list(valid_state_codes())
  from_state <- base::sprintf("CASE WHEN upper(trim(%1$s)) IN (%2$s) THEN upper(trim(%1$s)) END", state_col, valid)
  if (base::is.null(address_col)) {
    return(from_state)
  }
  from_address <- base::sprintf(
    "CASE WHEN regexp_extract(upper(%1$s), '(^|[^A-Z])([A-Z]{2})[ ,]+[0-9]{5}(-[0-9]{4})?\\s*$', 2) IN (%2$s) THEN regexp_extract(upper(%1$s), '(^|[^A-Z])([A-Z]{2})[ ,]+[0-9]{5}(-[0-9]{4})?\\s*$', 2) END",
    address_col, valid
  )
  base::paste0("coalesce(", from_state, ", ", from_address, ")")
}

#' Build or rebuild the database
#'
#' @param price_globs Parquet globs of canonical price rows (all sources).
#' @param crosswalk_path Parquet from run_ccn_crosswalk() (facility_ccn.parquet).
#' @param universe Hospital universe tibble (build_hospital_universe()).
#' @param codebook From load_codebook().
#' @param db_path Output database path.
#' @param dry_run Return the SQL statements instead of running them (for
#'   profiling).
#' @param exclude_file_ids mrf_file_id values to leave out, e.g. own-crawl
#'   files that duplicate a Trilliant file (cross_source_duplicate_file_ids()).
#' @param fee_type_min_files Files each typical gross needs before blank
#'   billing classes are inferred from it (see case_line_multiple()).
#' @param drg_los tibble(code, gmlos): geometric mean length of stay per
#'   MS-DRG (CMS IPPS Table 5, load_ipps_drg_weights()). Per-diem rates for
#'   MS-DRG codes are converted to a stay price with it (see per_diem_note()).
#'   NULL leaves per-diem rates as listed.
build_hpt_database <- function(price_globs, crosswalk_path, universe, codebook,
                               db_path = hpt_database_path(),
                               payer_rules = load_payer_type_rules(),
                               exclude_file_ids = NULL,
                               fee_type_min_files = 20L,
                               drg_los = NULL,
                               dry_run = FALSE) {
  require_duckdb_cli("1.5.0")
  enums <- hpt_enum_values()
  build_path <- base::paste0(db_path, ".building")
  base::unlink(build_path)

  # dry runs keep their inputs on the data drive: R deletes tempdir() on exit
  input_dir <- if (dry_run) hpt_path("build_inputs") else base::tempdir()
  universe_path <- base::tempfile(tmpdir = input_dir, fileext = ".parquet")
  codebook_path <- base::tempfile(tmpdir = input_dir, fileext = ".parquet")
  los_path <- base::tempfile(tmpdir = input_dir, fileext = ".parquet")
  if (!dry_run) {
    base::on.exit(base::unlink(base::c(universe_path, codebook_path, los_path)), add = TRUE)
  }
  arrow::write_parquet(repair_utf8(universe), universe_path)
  arrow::write_parquet(repair_utf8(codebook), codebook_path)
  drg_los <- drg_los %||% tibble::tibble(code = base::character(), gmlos = base::numeric())
  if (!"medicare_per_day" %in% base::names(drg_los)) drg_los$medicare_per_day <- NA_real_
  arrow::write_parquet(dplyr::select(drg_los, "code", "gmlos", "medicare_per_day"), los_path)

  prices_sql <- base::paste(
    base::sprintf("SELECT * FROM read_parquet(%s, hive_partitioning = true, union_by_name = true)", sql_string(price_globs)),
    collapse = " UNION ALL BY NAME "
  )
  crosswalk_cols <- arrow::open_dataset(crosswalk_path)$schema$names
  stg_setting <- enum_normalize_sql("s.setting", enums$setting)
  stg_methodology <- enum_normalize_sql("s.methodology", enums$methodology, base::c("percent of total billed charge" = "percent of total billed charges", "percentage of total billed charges" = "percent of total billed charges", "fee-schedule" = "fee schedule"))
  per_diem_convert <- base::paste0(
    "(", stg_methodology, " = 'per diem' AND los.gmlos IS NOT NULL ",
    "AND (los.medicare_per_day IS NULL OR s.negotiated_dollar < ", per_diem_case_multiple(), " * los.medicare_per_day))"
  )
  stg_billing_class <- enum_normalize_sql("s.billing_class", enums$billing_class, base::c(institutional = "facility"))
  # explicit billing class first; a blank one is professional only when its
  # gross sits below the code's facility/professional cutoff
  stg_fee_type <- base::paste0(
    "CASE WHEN ", stg_billing_class, " = 'professional' THEN 'professional' ",
    "WHEN ", stg_billing_class, " IN ('facility', 'both') THEN 'facility' ",
    "WHEN s.gross > 1 AND s.gross < g.professional_gross_cutoff THEN 'professional' ELSE 'facility' END"
  )
  payer_text <-"lower(regexp_replace(trim(coalesce(payer_name, '') || ' ' || coalesce(plan_name, '')), '\\s+', ' ', 'g'))"

  sql <- base::c(
    base::sprintf("ATTACH %s AS hpt (STORAGE_VERSION 'v1.4.0');", sql_string(build_path)),
    "USE hpt;",
    "SET preserve_insertion_order = false;",
    base::sprintf("CREATE TYPE setting_t AS %s;", enum_sql(enums$setting)),
    base::sprintf("CREATE TYPE billing_class_t AS %s;", enum_sql(enums$billing_class)),
    base::sprintf("CREATE TYPE methodology_t AS %s;", enum_sql(enums$methodology)),
    base::sprintf("CREATE TYPE source_t AS %s;", enum_sql(enums$source)),
    base::sprintf("CREATE TYPE payer_type_t AS %s;", enum_sql(enums$payer_type)),
    base::sprintf("CREATE TYPE fee_type_t AS %s;", enum_sql(base::c("facility", "professional"))),
    base::sprintf("CREATE TYPE concept_t AS %s;", enum_sql(base::sort(base::unique(codebook$concept)))),

    # staging (temporary; dropped at the end)
    # Trilliant keeps literal JSON quotes around some strings ("Aetna"); strip
    # them so one payer/plan is one dim_payer row and anchored rules match.
    base::sprintf(
      "CREATE TEMP TABLE stg0 AS SELECT * REPLACE (%s AS payer_name, %s AS plan_name, %s AS description) FROM (%s)%s;",
      unquote_sql("payer_name"), unquote_sql("plan_name"), unquote_sql("description"), prices_sql,
      if (base::length(exclude_file_ids) > 0L) base::paste0(" WHERE mrf_file_id NOT IN (", sql_string_list(exclude_file_ids), ")") else ""
    ),
    # One hashable payer key. Joining dim_payer on two IS NOT DISTINCT FROM
    # columns plus an OR forced a nested-loop join (34 minutes for 7.2M rows);
    # an equality join on this key is a hash join (seconds).
    base::paste0(
      "CREATE TEMP TABLE stg AS SELECT *, CASE WHEN payer_name IS NULL AND plan_name IS NULL THEN NULL ",
      "ELSE coalesce(payer_name, '') || chr(31) || coalesce(plan_name, '') END AS payer_key FROM stg0;"
    ),
    "DROP TABLE stg0;",

    # dim_code
    base::paste0(
      "CREATE TABLE dim_code AS SELECT CAST(row_number() OVER (ORDER BY concept, code) AS SMALLINT) AS code_id, ",
      "code, code_system, CAST(concept AS concept_t) AS concept, label, CAST(active_2026 AS BOOLEAN) AS active_2026, ",
      "CAST(coalesce(anchor, 'FALSE') AS BOOLEAN) AS anchor, code_family ",
      "FROM read_parquet(", sql_string(codebook_path), ");"
    ),

    # ref_drg_los: geometric mean length of stay per MS-DRG, for per-diem rates
    base::paste0(
      "CREATE TABLE ref_drg_los AS SELECT lpad(CAST(code AS VARCHAR), 3, '0') AS code, CAST(gmlos AS DOUBLE) AS gmlos, ",
      "CAST(medicare_per_day AS DOUBLE) AS medicare_per_day ",
      "FROM read_parquet(", sql_string(los_path), ") WHERE gmlos > 0;"
    ),

    # ref_code_gross: typical facility and professional gross per code (from
    # explicitly classed rows), the blank-class fee-type cutoff, and the
    # case-line thresholds for outpatient procedures
    base::paste0(
      "CREATE TABLE ref_code_gross AS WITH typ AS (",
      "SELECT c.code_id, c.concept, m.fee_type, median(m.min_gross) AS typical_gross, count(*) AS n_files ",
      "FROM (SELECT s.mrf_file_id, s.code, s.concept, ", stg_billing_class, " AS fee_type, min(s.gross) AS min_gross ",
      "FROM stg AS s WHERE s.gross > 1 AND ", stg_billing_class, " IN ('facility', 'professional') ",
      "AND ", stg_setting, " <> 'inpatient' GROUP BY ALL) AS m ",
      "JOIN dim_code AS c ON c.code = m.code AND c.concept = CAST(m.concept AS concept_t) GROUP BY ALL), ",
      "wide AS (SELECT code_id, any_value(concept) AS concept, ",
      "max(typical_gross) FILTER (WHERE fee_type = 'facility') AS facility_typical_gross, ",
      "max(n_files) FILTER (WHERE fee_type = 'facility') AS facility_n_files, ",
      "max(typical_gross) FILTER (WHERE fee_type = 'professional') AS professional_typical_gross, ",
      "max(n_files) FILTER (WHERE fee_type = 'professional') AS professional_n_files FROM typ GROUP BY code_id) ",
      "SELECT code_id, facility_typical_gross, facility_n_files, professional_typical_gross, professional_n_files, ",
      "CASE WHEN facility_n_files >= ", base::as.integer(fee_type_min_files), " AND professional_n_files >= ", base::as.integer(fee_type_min_files),
      " AND facility_typical_gross >= ", fee_type_min_separation(), " * professional_typical_gross ",
      "THEN sqrt(facility_typical_gross * professional_typical_gross) END AS professional_gross_cutoff, ",
      "CASE WHEN CAST(concept AS VARCHAR) IN (", sql_string_list(outpatient_concepts()), ") THEN ", case_line_multiple(), " * facility_typical_gross END AS facility_case_line_gross, ",
      "CASE WHEN CAST(concept AS VARCHAR) IN (", sql_string_list(outpatient_concepts()), ") THEN ", case_line_multiple(), " * professional_typical_gross END AS professional_case_line_gross ",
      "FROM wide;"
    ),

    # dim_hospital
    base::paste0(
      "CREATE TABLE dim_hospital AS SELECT DISTINCT facility_id AS ccn, facility_name, address, citytown AS city, state, zip_code, ",
      "hospital_type, hospital_ownership, CAST(health_sys_id AS VARCHAR) AS health_sys_id, health_sys_name ",
      "FROM read_parquet(", sql_string(universe_path), ");"
    ),

    # dim_file: one row per MRF file id
    base::paste0(
      "CREATE TABLE dim_file AS SELECT CAST(row_number() OVER (ORDER BY mrf_file_id) AS INTEGER) AS file_id, * FROM (",
      "SELECT mrf_file_id, CAST(min(source) AS source_t) AS source, min(mrf_url) AS mrf_url, min(file_version) AS file_version, ",
      "TRY_CAST(min(last_updated_on) AS DATE) AS last_updated_on, min(retrieved_at) AS retrieved_at, ",
      "min(hospital_name) AS hospital_name, min(location_name) AS location_name, min(license_number) AS license_number, ",
      "upper(min(license_state)) AS license_state, min(type_2_npi) AS type_2_npi, count(*) AS n_rows ",
      "FROM stg GROUP BY mrf_file_id) ",
      # the state of the facilities that publish the file (Trilliant rows carry no
      # license state); used when a file matched no CCN
      base::paste0(
        "LEFT JOIN (SELECT mrf_file_id, ",
        if ("state" %in% crosswalk_cols) base::paste0("mode(", clean_state_sql("state", if ("address" %in% crosswalk_cols) "address"), ")") else "NULL::VARCHAR", " AS file_state, ",
        if ("hospital_name" %in% crosswalk_cols) "mode(hospital_name)" else "NULL::VARCHAR", " AS xw_hospital_name, ",
        if ("type_2_npi" %in% crosswalk_cols) "string_agg(DISTINCT type_2_npi, ';')" else "NULL::VARCHAR", " AS xw_type_2_npi ",
        "FROM read_parquet(", sql_string(crosswalk_path), ") GROUP BY mrf_file_id) USING (mrf_file_id);"
      )
    ),

    # Trilliant price rows carry no hospital name or NPI; take them from the
    # facilities that publish the file
    base::paste0(
      "UPDATE dim_file SET hospital_name = coalesce(hospital_name, xw_hospital_name), type_2_npi = coalesce(type_2_npi, xw_type_2_npi);",
      "ALTER TABLE dim_file DROP COLUMN xw_hospital_name;",
      "ALTER TABLE dim_file DROP COLUMN xw_type_2_npi;"
    ),
    # bridge_file_ccn: unambiguous crosswalk matches only
    base::paste0(
      # exactly one row per (file, CCN): several facilities sharing a file often
      # match the same CCN by different methods, and duplicate bridge rows would
      # repeat that file's rates in v_hospital_rate. CCNs outside the CMS
      # roster (dim_hospital) are dropped.
      "CREATE TABLE bridge_file_ccn AS SELECT f.file_id, x.ccn, ",
      "arg_max(x.ccn_match_method, coalesce(x.ccn_match_score, 0)) AS ccn_match_method, max(x.ccn_match_score) AS ccn_match_score, ",
      "bool_or(coalesce(x.ccn_conflict, false)) AS ccn_conflict ",
      "FROM read_parquet(", sql_string(crosswalk_path), ") AS x JOIN dim_file AS f USING (mrf_file_id) ",
      "JOIN dim_hospital AS h ON h.ccn = x.ccn ",
      "WHERE x.ccn IS NOT NULL AND NOT coalesce(x.ccn_ambiguous, false) GROUP BY f.file_id, x.ccn;"
    ),

    # dim_payer, classified once per distinct payer/plan text
    base::paste0(
      "CREATE TABLE dim_payer AS SELECT CAST(row_number() OVER (ORDER BY payer_name, plan_name) AS INTEGER) AS payer_id, payer_key, payer_name, plan_name, ",
      "CAST(", payer_type_case_sql("payer_text", payer_rules), " AS payer_type_t) AS payer_type, trilliant_payer_type ",
      "FROM (SELECT payer_key, min(payer_name) AS payer_name, min(plan_name) AS plan_name, min(", payer_text, ") AS payer_text, ",
      "min(payer_type) AS trilliant_payer_type FROM stg WHERE payer_key IS NOT NULL GROUP BY payer_key);"
    ),

    # fact_rate, sorted for zone-map pruning
    base::paste0(
      "CREATE TABLE fact_rate AS SELECT ",
      "f.file_id, c.code_id, p.payer_id, ",
      "CAST(", stg_setting, " AS setting_t) AS setting, ",
      "CAST(", stg_billing_class, " AS billing_class_t) AS billing_class, ",
      "CAST(", stg_methodology, " AS methodology_t) AS methodology, ",
      "s.type_verified, s.gross, s.discounted_cash, s.min, s.max, s.negotiated_dollar, s.negotiated_percentage, ",
      "s.negotiated_algorithm, s.median_amount, s.p10, s.p90, s.count, s.estimated_amount, s.description, s.modifiers, s.notes, ",
      plausible_rate_sql("s.negotiated_dollar"), " AS plausible, ",
      "CAST(", stg_fee_type, " AS fee_type_t) AS fee_type, ",
      "coalesce(", stg_billing_class, " NOT IN ('facility', 'professional', 'both') AND s.gross > 1 AND g.professional_gross_cutoff IS NOT NULL, false) AS fee_type_inferred, ",
      "coalesce(s.gross > CASE WHEN ", stg_fee_type, " = 'professional' THEN g.professional_case_line_gross ELSE g.facility_case_line_gross END, false) AS case_line, ",
      "coalesce(", per_diem_convert, ", false) AS per_diem_converted, ",
      "coalesce(", stg_methodology, " = 'per diem' AND los.gmlos IS NOT NULL AND NOT (", per_diem_convert, "), false) AS per_diem_as_case, ",
      "CASE WHEN ", per_diem_convert, " THEN s.negotiated_dollar * los.gmlos ELSE s.negotiated_dollar END AS case_dollar ",
      "FROM stg AS s ",
      "JOIN dim_file AS f USING (mrf_file_id) ",
      "JOIN dim_code AS c ON c.code = s.code AND c.concept = CAST(s.concept AS concept_t) ",
      "LEFT JOIN dim_payer AS p ON p.payer_key = s.payer_key ",
      "LEFT JOIN ref_code_gross AS g ON g.code_id = c.code_id ",
      "LEFT JOIN ref_drg_los AS los ON c.code_system = 'MS-DRG' AND los.code = c.code ",
      "ORDER BY c.code_id, f.file_id;"
    ),

    # denormalized convenience views
    base::paste0(
      "CREATE VIEW v_rate AS SELECT r.*, c.code, c.concept, c.anchor, c.code_system, p.payer_name, p.plan_name, p.payer_type, ",
      "p.trilliant_payer_type, f.source, f.mrf_file_id, f.mrf_url, f.hospital_name, f.license_state, f.file_state ",
      "FROM fact_rate r JOIN dim_code c USING (code_id) JOIN dim_file f USING (file_id) LEFT JOIN dim_payer p USING (payer_id);"
    ),
    # one row per hospital (CCN) a rate applies to; unmatched files count as their own unit
    base::paste0(
      "CREATE VIEW v_hospital_rate AS SELECT v.*, coalesce(b.ccn, 'file:' || v.file_id) AS unit_id, b.ccn, ",
      "coalesce(NULLIF(trim(h.state), ''), NULLIF(trim(v.file_state), ''), NULLIF(trim(v.license_state), '')) AS state, h.hospital_type, h.health_sys_name ",
      "FROM v_rate v LEFT JOIN bridge_file_ccn b USING (file_id) LEFT JOIN dim_hospital h ON h.ccn = b.ccn;"
    ),

    "DROP TABLE stg;",
    "CHECKPOINT;"
  )

  if (dry_run) {
    return(sql)
  }

  base::message("Building ", db_path, " from ", base::length(price_globs), " price source(s).")
  run_duckdb_sql(sql)
  base::file.rename(build_path, db_path)

  counts <- duckdb_query(
    base::paste(
      "SELECT (SELECT count(*) FROM fact_rate) AS fact_rows, (SELECT count(*) FROM dim_file) AS files,",
      "(SELECT count(*) FROM dim_payer) AS payers, (SELECT count(DISTINCT ccn) FROM bridge_file_ccn) AS ccns,",
      "(SELECT avg(plausible::INT) FROM fact_rate WHERE negotiated_dollar IS NOT NULL) AS share_plausible"
    ),
    database = db_path, read_only = TRUE
  )
  base::message(
    "hpt.duckdb: ", scales::comma(counts$fact_rows), " rates, ", scales::comma(counts$files), " files, ",
    scales::comma(counts$payers), " payer/plans, ", scales::comma(counts$ccns), " CCNs; ",
    base::round(100 * counts$share_plausible, 1), "% of dollar rates plausible."
  )

  base::invisible(db_path)
}
