#' Parse CMS hospital price-transparency CSV MRFs (tall and wide, v2.x and v3)
#'
#' Layout (CMS v3.0 data dictionary): row 1 is the general-data header,
#' row 2 its values, row 3 the charge header. Real files add a BOM, CRLF,
#' spaced pipes ("code | 1"), blank rows, and extra metadata rows, so the
#' charge header is found by scanning the first lines instead of assuming
#' row 3.
#'
#' The charge table can be tens of GB, so it is never loaded into R: the
#' DuckDB CLI scans the file with a WHERE clause over every `code|N` column
#' and returns only candidate rows. R then unpivots the code slots, applies
#' the code-type gate ([match_target_codes()]), and for wide files pivots
#' the payer/plan columns to long.

# ported from emb_colonoscopy R/hpt_prices.R @ 471e067
normalize_hpt_names <- function(column_names) {
  column_names |>
    stringr::str_replace("^\\ufeff", "") |>
    stringr::str_trim() |>
    stringr::str_replace_all("\\s*\\|\\s*", "|")
}

#' Classify the text encoding of bytes from the top of an MRF
#'
#' @return "utf-16le" or "utf-16be" (by BOM, or BOM-less by where the NUL
#'   bytes of ASCII text fall), "utf-8" (valid), or "invalid" (UTF-8 with
#'   stray bytes, usually Windows-1252 curly quotes and dashes).
mrf_bytes_encoding <- function(bytes) {
  n_bytes <- base::length(bytes)

  if (n_bytes >= 2L && base::identical(bytes[1:2], base::as.raw(base::c(0xff, 0xfe)))) {
    return("utf-16le")
  }

  if (n_bytes >= 2L && base::identical(bytes[1:2], base::as.raw(base::c(0xfe, 0xff)))) {
    return("utf-16be")
  }

  if (n_bytes >= 64L) {
    sample <- bytes[base::seq_len(base::min(n_bytes, 4096L) %/% 2L * 2L)]
    nul_odd <- base::mean(sample[base::seq(2L, base::length(sample), by = 2L)] == base::as.raw(0))
    nul_even <- base::mean(sample[base::seq(1L, base::length(sample), by = 2L)] == base::as.raw(0))

    if (nul_odd > 0.4 && nul_even < 0.05) {
      return("utf-16le")
    }

    if (nul_even > 0.4 && nul_odd < 0.05) {
      return("utf-16be")
    }
  }

  # a multi-byte character cut at the buffer end is not an encoding error
  bytes <- bytes[bytes != base::as.raw(0)]
  if (base::length(bytes) > 4L) {
    bytes <- bytes[base::seq_len(base::length(bytes) - 4L)]
  }

  if (base::isTRUE(base::validUTF8(base::rawToChar(bytes)))) "utf-8" else "invalid"
}

#' Decode raw bytes from the top of an MRF to UTF-8 text
mrf_decode_bytes <- function(bytes) {
  encoding <- mrf_bytes_encoding(bytes)

  if (encoding %in% base::c("utf-16le", "utf-16be")) {
    # an odd trailing byte cannot be decoded
    bytes <- bytes[base::seq_len(base::length(bytes) %/% 2L * 2L)]
    text <- base::iconv(base::list(bytes), from = stringr::str_to_upper(encoding), to = "UTF-8", sub = "")
    return(dplyr::coalesce(text, ""))
  }

  base::rawToChar(bytes[bytes != base::as.raw(0)])
}

#' Read the first lines of a local MRF as UTF-8 text (BOM stripped)
#'
#' Reads raw bytes (so a UTF-16 file decodes correctly) and drops a final
#' line cut off by the byte limit.
read_mrf_lines <- function(path, n_lines = 20L, n_bytes = 1048576L) {
  bytes <- base::readBin(path, what = "raw", n = n_bytes)
  lines <- stringr::str_split(mrf_decode_bytes(bytes), "\n")[[1]]

  if (base::length(bytes) == n_bytes && base::length(lines) > 1L) {
    lines <- lines[-base::length(lines)]
  }

  mrf_clean_text_lines(utils::head(lines, n_lines))
}

#' Strip BOM and CR, and repair non-UTF-8 text (assumed Latin-1)
mrf_clean_text_lines <- function(lines) {
  lines <- base::iconv(lines, from = "UTF-8", to = "UTF-8", sub = NA)
  bad <- base::is.na(lines)

  if (base::any(bad)) {
    lines[bad] <- base::iconv(lines[bad], from = "latin1", to = "UTF-8")
  }

  if (base::length(lines) > 0L) {
    lines[[1]] <- stringr::str_replace(lines[[1]], "^\\ufeff", "")
  }

  stringr::str_replace(lines, "\r$", "")
}

#' Find the charge-table header among the first lines of a CSV MRF
#'
#' Generalized from emb_colonoscopy's detect_hpt_charge_header(): the header
#' is the first line that (after normalizing " | " to "|") has a `code|1`
#' field, or failing that starts with `description`.
detect_hpt_charge_header <- function(lines) {
  cleaned <- lines |>
    stringr::str_replace("^\\ufeff", "") |>
    stringr::str_replace_all("\\s*\\|\\s*", "|") |>
    stringr::str_to_lower() |>
    stringr::str_trim()

  has_code_1 <- stringr::str_detect(cleaned, "(^|,)\\s*\"?code\\|1\"?\\s*(,|$)")
  starts_description <- stringr::str_detect(cleaned, "^\"?description\"?\\s*,")
  matches <- base::which(has_code_1 | starts_description)

  if (base::length(matches) == 0L) {
    base::stop("Could not find the CMS HPT charge-table header in the first ", base::length(lines), " lines.")
  }

  matches[[1]]
}

#' Split CSV text lines into fields (quote-aware)
split_csv_line <- function(line) {
  parsed <- readr::read_csv(
    base::I(base::paste0(line, "\n")),
    col_names = FALSE,
    col_types = readr::cols(.default = readr::col_character()),
    na = base::character(),
    trim_ws = TRUE,
    progress = FALSE,
    show_col_types = FALSE
  )

  if (base::nrow(parsed) == 0L) {
    return(base::character())
  }

  base::unname(base::unlist(parsed[1, ], use.names = FALSE))
}

#' Normalize a declared date to ISO (YYYY-MM-DD) when it parses, else keep it
normalize_mrf_date <- function(x) {
  x <- stringr::str_trim(base::as.character(x))
  out <- x
  done <- base::rep(FALSE, base::length(x))

  for (fmt in base::c("%Y-%m-%d", "%m/%d/%Y", "%m-%d-%Y", "%Y/%m/%d", "%m/%d/%y")) {
    todo <- base::which(!done & !base::is.na(x))

    if (base::length(todo) == 0L) {
      break
    }

    parsed <- base::as.Date(x[todo], format = fmt)
    ok <- !base::is.na(parsed) & base::format(parsed, "%Y") > "1990"
    out[todo[ok]] <- base::format(parsed[ok], "%Y-%m-%d")
    done[todo[ok]] <- TRUE
  }

  out
}

#' Split a multi-valued metadata cell ("A|B", "A;B") into trimmed values
split_mrf_multi <- function(x) {
  if (base::is.na(x) || !base::nzchar(stringr::str_trim(x))) {
    return(base::character())
  }

  values <- stringr::str_trim(stringr::str_split(x, "\\s*[|;]\\s*")[[1]])
  values[base::nzchar(values)]
}

#' Canonical type_2_npi: every 10-digit NPI, semicolon-joined
format_type_2_npi <- function(values) {
  values <- values[!base::is.na(values)]
  npis <- base::unique(base::unlist(stringr::str_extract_all(values, "[0-9]{10}")))

  if (base::length(npis) > 0L) {
    return(base::paste(npis, collapse = ";"))
  }

  values <- stringr::str_trim(values)
  values <- values[base::nzchar(values)]
  if (base::length(values) == 0L) NA_character_ else base::paste(values, collapse = ";")
}

#' Parse the general-data rows (1 and 2) of a CSV MRF from its first lines
#'
#' @param lines Text lines from the top of the file (BOM/CR stripped).
#' @return Named list: hospital_name, last_updated_on, version,
#'   location_name, hospital_address, license_number, license_state,
#'   type_2_npi, attestation, attester_name, charge_header_line,
#'   charge_columns (raw charge-header field names).
parse_csv_mrf_header_lines <- function(lines) {
  header_line <- detect_hpt_charge_header(lines)
  charge_columns <- split_csv_line(lines[[header_line]])

  meta_lines <- lines[base::seq_len(header_line - 1L)]
  meta_lines <- meta_lines[stringr::str_detect(meta_lines, "[^,\\s]")]

  meta <- base::list(
    hospital_name = NA_character_, last_updated_on = NA_character_, version = NA_character_,
    location_name = NA_character_, hospital_address = NA_character_,
    license_number = NA_character_, license_state = NA_character_, type_2_npi = NA_character_,
    attestation = NA_character_, attester_name = NA_character_,
    charge_header_line = header_line, charge_columns = charge_columns
  )

  if (base::length(meta_lines) < 2L) {
    return(meta)
  }

  meta_names <- split_csv_line(meta_lines[[1]])
  meta_values <- split_csv_line(meta_lines[[2]])
  meta_values <- base::c(meta_values, base::rep("", base::max(0L, base::length(meta_names) - base::length(meta_values))))
  names_norm <- stringr::str_to_lower(normalize_hpt_names(meta_names))

  value_of <- function(pattern) {
    hit <- base::which(stringr::str_detect(names_norm, pattern))
    if (base::length(hit) == 0L) {
      return(NA_character_)
    }
    value <- stringr::str_trim(meta_values[[hit[[1]]]])
    if (base::nzchar(value)) value else NA_character_
  }

  license_idx <- base::which(stringr::str_detect(names_norm, "^license_number"))
  license_state <- NA_character_

  if (base::length(license_idx) > 0L) {
    license_state <- stringr::str_to_upper(stringr::str_match(names_norm[[license_idx[[1]]]], "^license_number\\|([a-z]{2})$")[, 2])
  }

  location <- value_of("^(location_name|hospital_location)$")

  meta$hospital_name <- value_of("^hospital_name$")
  meta$last_updated_on <- normalize_mrf_date(value_of("^last_updated_on$"))
  meta$version <- value_of("^version$")
  meta$location_name <- if (base::is.na(location)) NA_character_ else base::paste(split_mrf_multi(location), collapse = "; ")
  address <- value_of("^hospital_address$")
  meta$hospital_address <- if (base::is.na(address)) NA_character_ else base::paste(split_mrf_multi(address), collapse = "; ")
  meta$license_number <- value_of("^license_number")
  meta$license_state <- license_state
  meta$type_2_npi <- format_type_2_npi(value_of("^type_2_npi$"))
  meta$attestation <- value_of("^to the best of its knowledge")
  meta$attester_name <- value_of("^attester_name$")

  meta
}

#' General data elements of a local CSV MRF (rows 1 and 2)
#'
#' @return See [parse_csv_mrf_header_lines()].
read_csv_mrf_metadata <- function(path) {
  # very wide files can have a multi-MB charge header: widen the window
  for (n_bytes in base::c(1048576L, 16777216L, 134217728L)) {
    lines <- read_mrf_lines(path, n_lines = 20L, n_bytes = n_bytes)
    header_found <- base::tryCatch({
      detect_hpt_charge_header(lines)
      TRUE
    }, error = function(e) FALSE)

    if (header_found || base::file.size(path) <= n_bytes) {
      break
    }
  }

  parse_csv_mrf_header_lines(lines)
}

#' Tall, wide, or unknown, from normalized (lowercase) charge-header names
csv_mrf_layout <- function(names_norm) {
  if (base::all(base::c("payer_name", "plan_name") %in% names_norm)) {
    return("tall")
  }

  wide_pattern <- "^(standard_charge\\|.+\\|(negotiated_dollar|negotiated_percentage|negotiated_algorithm|methodology)|(median_amount|10th_percentile|90th_percentile|count|estimated_amount|additional_payer_notes)\\|.+)$"

  if (base::any(stringr::str_detect(names_norm, wide_pattern))) {
    return("wide")
  }

  "unknown"
}

#' Parse wide-format payer/plan column names
#'
#' Generalized from emb_colonoscopy's parse_hpt_wide_column(): covers all
#' nine v3 payer-specific elements plus v2 `estimated_amount`, reads the
#' metric from the right and the element prefix from the left, and keeps
#' payer/plan text as written (payer names can contain spaces).
#'
#' @return Tibble: col_index, payer_name, plan_name, metric (canonical name).
parse_hpt_wide_columns <- function(column_names) {
  normalized <- normalize_hpt_names(column_names)

  rows <- base::lapply(base::seq_along(normalized), function(i) {
    parts <- stringr::str_split(normalized[[i]], stringr::fixed("|"))[[1]]
    parts_lower <- stringr::str_to_lower(parts)
    n_parts <- base::length(parts)

    if (n_parts < 2L) {
      return(NULL)
    }

    metric <- NA_character_
    middle <- base::character()

    suffix_metrics <- base::c(
      negotiated_dollar = "negotiated_dollar", negotiated_percentage = "negotiated_percentage",
      negotiated_algorithm = "negotiated_algorithm", methodology = "methodology"
    )
    prefix_metrics <- base::c(
      median_amount = "median_amount", `10th_percentile` = "p10", `90th_percentile` = "p90",
      count = "count", estimated_amount = "estimated_amount", additional_payer_notes = "payer_notes"
    )

    if (parts_lower[[1]] == "standard_charge" && n_parts >= 3L && parts_lower[[n_parts]] %in% base::names(suffix_metrics)) {
      metric <- suffix_metrics[[parts_lower[[n_parts]]]]
      middle <- parts[2:(n_parts - 1L)]
    } else if (parts_lower[[1]] %in% base::names(prefix_metrics)) {
      metric <- prefix_metrics[[parts_lower[[1]]]]
      middle <- parts[2:n_parts]
    }

    if (base::is.na(metric) || base::length(middle) == 0L) {
      return(NULL)
    }

    # unreplaced template brackets ("[payer_Aetna]", "[plan_name_PPO]")
    middle <- middle |>
      stringr::str_replace("^\\[(?:payer_name_|payer_|plan_name_|plan_)?(.*)\\]$", "\\1") |>
      stringr::str_trim()

    # payer|plan normally; payer only when the plan is omitted; with extra
    # pipes the first piece is the payer and the rest the plan
    tibble::tibble(
      col_index = i,
      payer_name = middle[[1]],
      plan_name = if (base::length(middle) >= 2L) base::paste(middle[-1], collapse = "|") else NA_character_,
      metric = metric
    )
  })

  out <- dplyr::bind_rows(rows)

  if (base::nrow(out) == 0L) {
    return(tibble::tibble(col_index = base::integer(), payer_name = base::character(),
                          plan_name = base::character(), metric = base::character()))
  }

  out
}

#' SQL predicate: does a VARCHAR column hold one of the target codes?
#'
#' Mirrors normalize_code(): trim, uppercase, drop a ".0" suffix; DRG codes
#' also compare after stripping leading zeros. It is a liberal pre-filter;
#' the code-type gate runs afterwards in R.
target_code_sql_predicate <- function(column_sql, codebook) {
  procedure_codes <- codebook$code[codebook$code_family == "procedure"]
  drg_codes <- base::unique(stringr::str_replace(codebook$code[codebook$code_family == "ms_drg"], "^0+", ""))
  normalized <- base::sprintf(
    "regexp_replace(upper(trim(%s, ' ' || chr(9) || chr(160))), '\\.0+$', '')",
    column_sql
  )

  clauses <- base::c(
    if (base::length(procedure_codes) > 0L) base::sprintf("%s IN (%s)", normalized, sql_string_list(procedure_codes)),
    if (base::length(drg_codes) > 0L) base::sprintf("ltrim(%s, '0') IN (%s)", normalized, sql_string_list(drg_codes))
  )

  base::paste0("(", base::paste(clauses, collapse = " OR "), ")")
}

sql_identifier <- function(x) {
  base::paste0("\"", stringr::str_replace_all(x, "\"", "\"\""), "\"")
}

#' Run a DuckDB CLI script whose input CSV arrives on stdin from a command
#'
#' Like [run_duckdb_sql()] but with `<input_command> | duckdb`, so a file
#' can be transcoded on the fly without a second copy on disk.
run_duckdb_sql_piped <- function(sql, input_command) {
  script_path <- base::tempfile(fileext = ".sql")
  base::on.exit(base::unlink(script_path), add = TRUE)
  base::writeLines(sql, script_path)

  command <- base::paste(input_command, "|", base::shQuote(duckdb_bin()), "-bail -f", base::shQuote(script_path))
  output <- base::suppressWarnings(
    base::system2("bash", base::c("-c", base::shQuote(command)), stdout = TRUE, stderr = TRUE)
  )
  status <- base::attr(output, "status") %||% 0L

  if (!base::identical(base::as.integer(status), 0L) ||
      base::any(stringr::str_detect(output, "^(Error|.*Error:)"))) {
    base::stop("DuckDB CLI failed:\n", base::paste(utils::tail(output, 20), collapse = "\n"))
  }

  base::invisible(output)
}

#' Perl filter: keep valid UTF-8, decode each stray byte as Windows-1252
#'
#' Lines without high bytes pass straight through. Prints
#' `hpt_invalid_bytes=<n>` to stderr at the end.
mrf_cp1252_repair_script <- function() {
  r"---(
use strict;
use warnings;
use Encode qw(decode encode);
binmode STDIN;
binmode STDOUT;
my $n = 0;
my $valid = qr/(?:[\x00-\x7F]|[\xC2-\xDF][\x80-\xBF]|\xE0[\xA0-\xBF][\x80-\xBF]|[\xE1-\xEC\xEE\xEF][\x80-\xBF]{2}|\xED[\x80-\x9F][\x80-\xBF]|\xF0[\x90-\xBF][\x80-\xBF]{2}|[\xF1-\xF3][\x80-\xBF]{3}|\xF4[\x80-\x8F][\x80-\xBF]{2})/;
while (my $line = <STDIN>) {
  if ($line =~ /[\x80-\xFF]/) {
    $line =~ s/\G((?>$valid*))([\x80-\xFF])/do { my ($ok, $bad) = ($1, $2); $n++; $ok . encode("UTF-8", decode("cp1252", $bad)) }/ge;
  }
  print $line;
}
print STDERR "hpt_invalid_bytes=$n\n";
)---"
}

#' Shell command that writes a file to stdout as UTF-8
#'
#' @param encoding From [mrf_bytes_encoding()]: "invalid" repairs stray
#'   bytes as Windows-1252 with perl (falls back to dropping them with
#'   `iconv -c` when perl is missing); "utf-16le"/"utf-16be" convert with
#'   iconv.
#' @param script_path Where to write the perl script (caller deletes it).
mrf_transcode_command <- function(path, encoding, script_path) {
  quoted_path <- base::shQuote(base::normalizePath(path))

  if (encoding %in% base::c("utf-16le", "utf-16be")) {
    return(base::paste("iconv -c -f", stringr::str_to_upper(encoding), "-t UTF-8", quoted_path))
  }

  if (base::nzchar(base::Sys.which("perl"))) {
    base::writeLines(mrf_cp1252_repair_script(), script_path)
    return(base::paste("perl", base::shQuote(script_path), "<", quoted_path))
  }

  base::paste("iconv -c -f UTF-8 -t UTF-8", quoted_path)
}

#' Filter a CSV MRF to candidate rows inside DuckDB
#'
#' Columns are declared explicitly (c1..cN, all VARCHAR) so DuckDB never has
#' to sniff a dialect from a file whose first rows are metadata. The file is
#' read from its first line (`skip = 0`): the metadata rows and the charge
#' header can never satisfy the code predicate, and this avoids depending
#' on how quoted newlines or blank lines in the metadata rows are counted.
#' Malformed lines are skipped and counted (`store_rejects`). The scan is
#' single-threaded because DuckDB's parallel reader refuses `null_padding`
#' once it meets a quoted newline (common in real notes columns); measured
#' cost is about 30% on a 33 MB file.
#'
#' @param encoding From [mrf_bytes_encoding()]. Valid UTF-8 is read in
#'   place; anything else is transcoded on the fly and piped in on stdin.
#' @return List: `rows` (tibble c1..cN), `rejects` (error_type, n),
#'   `invalid_bytes` (stray bytes repaired; 0 for valid UTF-8, NA when
#'   unknown).
filter_csv_mrf_duckdb <- function(path, n_columns, code_positions, codebook, encoding = "utf-8") {
  column_names <- base::paste0("c", base::seq_len(n_columns))
  columns_sql <- base::paste0(
    "{", base::paste(base::sprintf("'%s': 'VARCHAR'", column_names), collapse = ", "), "}"
  )
  where_sql <- base::paste(
    base::vapply(column_names[code_positions], function(column) target_code_sql_predicate(column, codebook), base::character(1)),
    collapse = "\n   OR "
  )

  rows_path <- base::tempfile(fileext = ".parquet")
  rejects_path <- base::tempfile(fileext = ".parquet")
  script_path <- base::tempfile(fileext = ".pl")
  base::on.exit(base::unlink(base::c(rows_path, rejects_path, script_path)), add = TRUE)

  direct <- encoding == "utf-8"
  source_path <- if (direct) base::normalizePath(path) else "/dev/stdin"

  sql <- base::sprintf(
    "SET preserve_insertion_order = false;
CREATE TEMP TABLE candidates AS
SELECT * FROM read_csv(%s,
  auto_detect = false, header = false, skip = 0, columns = %s,
  delim = ',', quote = '\"', escape = '\"', comment = '',
  strict_mode = false, null_padding = true, store_rejects = true, parallel = false,
  max_line_size = 33554432, buffer_size = 67108864)
WHERE %s;
COPY candidates TO %s (FORMAT parquet);
COPY (SELECT error_type::VARCHAR AS error_type, count(*) AS n FROM reject_errors GROUP BY 1) TO %s (FORMAT parquet);",
    sql_string(source_path), columns_sql, where_sql, sql_string(rows_path), sql_string(rejects_path)
  )

  invalid_bytes <- 0

  if (direct) {
    run_duckdb_sql(sql)
  } else {
    output <- run_duckdb_sql_piped(sql, mrf_transcode_command(path, encoding, script_path))
    counted <- stringr::str_match(output, "^hpt_invalid_bytes=([0-9]+)$")[, 2]
    counted <- counted[!base::is.na(counted)]
    invalid_bytes <- if (base::length(counted) > 0L) base::as.numeric(counted[[1]]) else NA_real_
  }

  base::list(
    rows = tibble::as_tibble(arrow::read_parquet(rows_path)),
    rejects = tibble::as_tibble(arrow::read_parquet(rejects_path)),
    invalid_bytes = invalid_bytes
  )
}

#' Encoding of a CSV MRF from its first MB (see [mrf_bytes_encoding()])
mrf_file_encoding <- function(path, n_bytes = 1048576L) {
  mrf_bytes_encoding(base::readBin(path, what = "raw", n = n_bytes))
}

#' Column values by normalized header name (NA when the column is absent)
mrf_column <- function(tbl, names_norm, name) {
  hit <- base::which(names_norm == name)

  if (base::length(hit) == 0L) {
    return(base::rep(NA_character_, base::nrow(tbl)))
  }

  mrf_blank_to_na(tbl[[hit[[1]]]])
}

mrf_blank_to_na <- function(x) {
  x <- stringr::str_trim(base::as.character(x))
  dplyr::if_else(base::is.na(x) | !base::nzchar(x), NA_character_, x)
}

parse_percentage <- function(x) {
  parse_money(stringr::str_replace_all(base::as.character(x), "%", ""))
}

#' Join generic and payer-specific notes into one `notes` value
combine_mrf_notes <- function(payer_notes, generic_notes) {
  payer_notes <- mrf_blank_to_na(payer_notes)
  generic_notes <- mrf_blank_to_na(generic_notes)

  dplyr::case_when(
    !base::is.na(payer_notes) & !base::is.na(generic_notes) ~ base::paste(payer_notes, generic_notes, sep = " || "),
    !base::is.na(payer_notes) ~ payer_notes,
    TRUE ~ generic_notes
  )
}

#' One row per (candidate row, code slot) with the raw code and declared type
unpivot_csv_code_slots <- function(rows, names_norm) {
  code_positions <- base::which(stringr::str_detect(names_norm, "^code(\\|[0-9]+)?$"))

  slots <- base::lapply(code_positions, function(position) {
    type_position <- base::which(names_norm == base::paste0(names_norm[[position]], "|type"))
    raw_type <- if (base::length(type_position) > 0L) rows[[type_position[[1]]]] else NA_character_

    tibble::tibble(
      .row_id = rows$.row_id,
      code_slot = position,
      raw_code = mrf_blank_to_na(rows[[position]]),
      raw_type = mrf_blank_to_na(raw_type)
    )
  })

  dplyr::bind_rows(slots) |>
    dplyr::filter(!base::is.na(.data$raw_code))
}

#' Row-level (non-payer) fields of candidate rows
csv_mrf_line_fields <- function(rows, names_norm) {
  tibble::tibble(
    .row_id = rows$.row_id,
    description = mrf_column(rows, names_norm, "description"),
    modifiers = mrf_column(rows, names_norm, "modifiers"),
    setting = mrf_column(rows, names_norm, "setting"),
    billing_class = mrf_column(rows, names_norm, "billing_class"),
    gross = mrf_column(rows, names_norm, "standard_charge|gross"),
    discounted_cash = mrf_column(rows, names_norm, "standard_charge|discounted_cash"),
    min = mrf_column(rows, names_norm, "standard_charge|min"),
    max = mrf_column(rows, names_norm, "standard_charge|max"),
    generic_notes = mrf_column(rows, names_norm, "additional_generic_notes")
  )
}

#' Payer rates of a tall CSV: already one row per payer/plan
csv_mrf_tall_rates <- function(rows, names_norm) {
  tibble::tibble(
    .row_id = rows$.row_id,
    payer_name = mrf_column(rows, names_norm, "payer_name"),
    plan_name = mrf_column(rows, names_norm, "plan_name"),
    negotiated_dollar = mrf_column(rows, names_norm, "standard_charge|negotiated_dollar"),
    negotiated_percentage = mrf_column(rows, names_norm, "standard_charge|negotiated_percentage"),
    negotiated_algorithm = mrf_column(rows, names_norm, "standard_charge|negotiated_algorithm"),
    methodology = mrf_column(rows, names_norm, "standard_charge|methodology"),
    median_amount = mrf_column(rows, names_norm, "median_amount"),
    p10 = mrf_column(rows, names_norm, "10th_percentile"),
    p90 = mrf_column(rows, names_norm, "90th_percentile"),
    count = mrf_column(rows, names_norm, "count"),
    estimated_amount = mrf_column(rows, names_norm, "estimated_amount"),
    payer_notes = NA_character_
  )
}

#' Payer rates of a wide CSV, pivoted to one row per row x payer x plan
#'
#' A payer/plan pair is kept for a row only when it carries a rate or an
#' allowed-amount statistic; rows with no payer data at all yield no rate
#' rows here (the caller keeps them with NA payer).
csv_mrf_wide_rates <- function(rows, column_names) {
  wide_columns <- parse_hpt_wide_columns(column_names)
  metrics <- base::c(
    "negotiated_dollar", "negotiated_percentage", "negotiated_algorithm", "methodology",
    "median_amount", "p10", "p90", "count", "estimated_amount", "payer_notes"
  )
  empty <- tibble::as_tibble(stats::setNames(
    base::c(base::list(base::integer()), base::rep(base::list(base::character()), base::length(metrics) + 2L)),
    base::c(".row_id", "payer_name", "plan_name", metrics)
  ))

  if (base::nrow(wide_columns) == 0L || base::nrow(rows) == 0L) {
    return(empty)
  }

  values <- rows[, wide_columns$col_index, drop = FALSE]
  base::names(values) <- base::as.character(wide_columns$col_index)
  values$.row_id <- rows$.row_id

  long <- values |>
    tidyr::pivot_longer(-dplyr::all_of(".row_id"), names_to = "col_index", values_to = "value") |>
    dplyr::mutate(value = mrf_blank_to_na(.data$value), col_index = base::as.integer(.data$col_index)) |>
    dplyr::filter(!base::is.na(.data$value)) |>
    dplyr::inner_join(wide_columns, by = "col_index")

  if (base::nrow(long) == 0L) {
    return(empty)
  }

  rates <- long |>
    dplyr::select(".row_id", "payer_name", "plan_name", "metric", "value") |>
    tidyr::pivot_wider(names_from = "metric", values_from = "value", values_fn = dplyr::first)

  for (metric in base::setdiff(metrics, base::names(rates))) {
    rates[[metric]] <- NA_character_
  }

  rates |>
    dplyr::filter(
      !base::is.na(.data$negotiated_dollar) | !base::is.na(.data$negotiated_percentage) |
        !base::is.na(.data$negotiated_algorithm) | !base::is.na(.data$median_amount) |
        !base::is.na(.data$p10) | !base::is.na(.data$p90) | !base::is.na(.data$estimated_amount)
    ) |>
    dplyr::select(".row_id", "payer_name", "plan_name", dplyr::all_of(metrics))
}

#' Zero-row canonical price table
empty_price_table <- function() {
  spec <- canonical_price_columns()
  tibble::as_tibble(base::lapply(spec, function(type) base::vector(type, 0L)))
}

#' Attach file-level provenance and coerce to the canonical schema
finalize_mrf_prices <- function(prices, meta, mrf_url, mrf_file_id, retrieved_at) {
  prices |>
    dplyr::mutate(
      source = "own_crawl",
      mrf_url = mrf_url,
      mrf_file_id = mrf_file_id,
      file_version = meta$version,
      last_updated_on = meta$last_updated_on,
      retrieved_at = retrieved_at,
      hospital_name = meta$hospital_name,
      location_name = meta$location_name,
      license_number = meta$license_number,
      license_state = meta$license_state,
      type_2_npi = meta$type_2_npi
    ) |>
    conform_price_table()
}

#' Parse a CSV MRF into canonical price rows for the codebook's codes
#'
#' @param path Local CSV file.
#' @param codebook From [load_codebook()].
#' @param mrf_url Source URL, recorded on every row.
#' @param mrf_file_id File identifier; defaults to the file's sha256.
#' @param retrieved_at Download timestamp, recorded on every row.
#' @return [conform_price_table()] output (source = "own_crawl") with a
#'   `parse_log` attribute (layout, candidate rows, rejected lines).
parse_csv_mrf <- function(path,
                          codebook,
                          mrf_url = NA_character_,
                          mrf_file_id = NULL,
                          retrieved_at = NA_character_) {
  started <- base::Sys.time()
  base::message("Parsing CSV MRF: ", path)

  meta <- read_csv_mrf_metadata(path)
  column_names <- meta$charge_columns
  names_norm <- stringr::str_to_lower(normalize_hpt_names(column_names))
  layout <- csv_mrf_layout(names_norm)
  code_positions <- base::which(stringr::str_detect(names_norm, "^code(\\|[0-9]+)?$"))

  if (base::length(code_positions) == 0L) {
    base::stop("CSV MRF has no code columns: ", path)
  }

  if (layout == "unknown") {
    base::stop("Could not identify CMS HPT tall or wide layout: ", path)
  }

  encoding <- mrf_file_encoding(path)
  filtered <- base::tryCatch(
    filter_csv_mrf_duckdb(path, base::length(column_names), code_positions, codebook, encoding),
    error = function(e) {
      # some invalid bytes (e.g. inside a quoted field) abort the scan instead
      # of being rejected line by line
      if (encoding == "utf-8" && stringr::str_detect(base::conditionMessage(e), stringr::regex("encoding|unicode", ignore_case = TRUE))) {
        return(base::list(rows = NULL, rejects = tibble::tibble(error_type = "INVALID ENCODING (scan aborted)", n = NA_real_)))
      }
      base::stop(e)
    }
  )

  # invalid UTF-8 past the first MB shows up as rejected lines or an aborted
  # scan: re-read with the stray bytes repaired
  if (encoding == "utf-8" && base::any(stringr::str_detect(filtered$rejects$error_type, stringr::regex("encoding", ignore_case = TRUE)))) {
    base::message("  invalid UTF-8 found past the first MB; re-reading with stray bytes repaired")
    encoding <- "invalid"
    filtered <- filter_csv_mrf_duckdb(path, base::length(column_names), code_positions, codebook, encoding)
  }

  if (encoding != "utf-8") {
    base::message("  transcoded from ", encoding, "; ", filtered$invalid_bytes, " stray bytes read as Windows-1252")
  }

  rows <- filtered$rows
  rows$.row_id <- base::seq_len(base::nrow(rows))
  n_rejected <- base::sum(filtered$rejects$n)

  base::message(
    "  ", layout, " layout, ", base::length(code_positions), " code columns, ",
    scales::comma(base::nrow(rows)), " candidate rows, ", scales::comma(n_rejected), " rejected lines",
    if (n_rejected > 0) base::paste0(" (", base::paste(filtered$rejects$error_type, filtered$rejects$n, sep = ": ", collapse = ", "), ")") else ""
  )

  codes <- unpivot_csv_code_slots(rows, names_norm)
  matched <- match_target_codes(codes, "raw_code", "raw_type", codebook)

  if (base::nrow(matched) == 0L) {
    matched <- tibble::tibble(
      .row_id = base::integer(), concept = base::character(), code = base::character(),
      code_system = base::character(), code_type = base::character(), type_verified = base::logical()
    )
  } else {
    matched <- matched |>
      dplyr::arrange(.data$.row_id, dplyr::desc(.data$type_verified), .data$code_slot) |>
      dplyr::distinct(.data$.row_id, .data$code_system, .data$code, .keep_all = TRUE) |>
      dplyr::select(".row_id", "concept", "code", "code_system", "code_type", "type_verified")
  }

  line_fields <- csv_mrf_line_fields(rows, names_norm)
  rates <- if (layout == "tall") csv_mrf_tall_rates(rows, names_norm) else csv_mrf_wide_rates(rows, column_names)

  if (layout == "wide") {
    # rows with no payer data keep one row with NA payer (gross/cash only)
    no_rates <- tibble::tibble(.row_id = base::setdiff(rows$.row_id, rates$.row_id))
    rates <- dplyr::bind_rows(rates, no_rates)
  }

  prices <- matched |>
    dplyr::inner_join(line_fields, by = ".row_id") |>
    dplyr::inner_join(rates, by = ".row_id", relationship = "many-to-many")

  prices <- prices |>
    dplyr::mutate(
      gross = parse_money(.data$gross),
      discounted_cash = parse_money(.data$discounted_cash),
      min = parse_money(.data$min),
      max = parse_money(.data$max),
      negotiated_dollar = parse_money(.data$negotiated_dollar),
      negotiated_percentage = parse_percentage(.data$negotiated_percentage),
      median_amount = parse_money(.data$median_amount),
      p10 = parse_money(.data$p10),
      p90 = parse_money(.data$p90),
      estimated_amount = parse_money(.data$estimated_amount),
      notes = combine_mrf_notes(.data$payer_notes, .data$generic_notes)
    )

  out <- finalize_mrf_prices(
    prices, meta, mrf_url,
    mrf_file_id = mrf_file_id %||% mrf_sha256(path),
    retrieved_at = retrieved_at
  )

  base::message(
    "  ", scales::comma(base::nrow(out)), " price rows for ",
    base::length(base::unique(out$concept)), " concepts in ",
    base::round(base::as.numeric(base::difftime(base::Sys.time(), started, units = "secs")), 1), " s"
  )

  base::attr(out, "parse_log") <- tibble::tibble(
    path = path,
    format = "csv",
    layout = layout,
    encoding = encoding,
    invalid_bytes = filtered$invalid_bytes,
    candidate_rows = base::nrow(rows),
    rejected_lines = n_rejected,
    price_rows = base::nrow(out)
  )

  out
}
