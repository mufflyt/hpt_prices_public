#' The canonical long price table
#'
#' Every source (the Trilliant lake in R/trilliant.R, our own CSV and JSON
#' parsers in R/parse_csv.R and R/parse_json.R) must return exactly these
#' columns so the outputs can be stacked. One row per
#' MRF file x charge line x target code x setting x payer x plan. Rows with no
#' payer (gross/cash-only lines) keep payer_name/plan_name as NA.

canonical_price_columns <- function() {
  base::c(
    # provenance
    source = "character",               # "trilliant" | "own_crawl"
    mrf_url = "character",
    mrf_file_id = "character",          # md5 (Trilliant) or sha256 (own crawl) of the file
    file_version = "character",         # CMS template version declared in the file
    last_updated_on = "character",      # as declared in the file (ISO date when parseable)
    retrieved_at = "character",         # when the file was downloaded
    hospital_name = "character",        # as written in the MRF
    location_name = "character",
    license_number = "character",
    license_state = "character",
    type_2_npi = "character",           # semicolon-joined when the file lists several
    # the charge line
    concept = "character",
    code = "character",
    code_system = "character",          # from the codebook: CPT | HCPCS | MS-DRG
    code_type = "character",            # as declared in the file
    type_verified = "logical",
    description = "character",
    modifiers = "character",
    setting = "character",
    billing_class = "character",        # v2 only
    gross = "double",
    discounted_cash = "double",
    min = "double",
    max = "double",
    # the payer rate
    payer_name = "character",
    plan_name = "character",
    payer_type = "character",           # Trilliant enrichment; NA for own crawl
    negotiated_dollar = "double",
    negotiated_percentage = "double",
    negotiated_algorithm = "character",
    methodology = "character",
    median_amount = "double",           # v3 allowed-amount statistics
    p10 = "double",
    p90 = "double",
    count = "character",                # v3: "0", "1 through 10", or an integer
    estimated_amount = "double",        # v2 only
    notes = "character"
  )
}

#' Coerce a data frame to the canonical schema (adds missing columns as NA,
#' drops extras, fixes types, orders columns)
conform_price_table <- function(tbl) {
  spec <- canonical_price_columns()

  for (column in base::names(spec)) {
    if (!column %in% base::names(tbl)) {
      tbl[[column]] <- NA
    }

    tbl[[column]] <- switch(
      spec[[column]],
      character = base::as.character(tbl[[column]]),
      double = suppressWarnings(base::as.numeric(tbl[[column]])),
      logical = base::as.logical(tbl[[column]])
    )
  }

  tibble::as_tibble(tbl[base::names(spec)])
}

#' Parse a money-like string ("$1,234.50", " 1234 ") to numeric
parse_money <- function(x) {
  cleaned <- stringr::str_replace_all(base::as.character(x), "[$,\\s]", "")
  suppressWarnings(base::as.numeric(cleaned))
}
