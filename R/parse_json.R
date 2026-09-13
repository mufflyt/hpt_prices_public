#' Parse CMS hospital price-transparency JSON MRFs (v2.x and v3)
#'
#' A JSON MRF is one top-level object whose `standard_charge_information`
#' array can hold GBs of items. jq does the extraction and writes matching
#' charge items, flattened to one line per matching code x standard_charges
#' element x payers_information element, to a temporary NDJSON file that R
#' reads with yyjsonr. Top-level metadata comes out of the same jq run as
#' path/value events, wherever it sits in the file.
#'
#' Two jq modes (measured on a 200 MB synthetic v3 file, 127k items):
#' - "memory": plain `jq` over the whole document, about 17 s and 1.2 GB
#'   resident (roughly 6x the file size). Used for files up to
#'   `HPT_JQ_MEMORY_MAX_MB` (default 750).
#' - "stream": `jq --stream`, flat memory (about 2 MB resident). A single
#'   `fromstream()` pass rebuilds every item and ran at 0.7 MB/s, so this
#'   mode makes two passes instead: pass 1 lists (item index, code) pairs,
#'   R picks the items holding a codebook code, and pass 2 rebuilds only
#'   those items. About 2 MB/s, i.e. roughly 40 min for a 5 GB file.
#'
#' v3 names: location_name, attestation, type_2_npi, median_amount,
#' 10th_percentile, 90th_percentile, count. v2 names: hospital_location,
#' affirmation, billing_class, estimated_amount. Both are read.

jq_bin <- function() {
  base::Sys.getenv("HPT_JQ_BIN", unset = "jq")
}

#' jq definitions shared by both modes
#'
#' `$codes[0]` is `{"procedure": {"45378": true, ...}, "drg": {"742": true}}`
#' (DRG keys without leading zeros). The code test is a liberal pre-filter
#' (exact match first, normalized match second); the code-type gate runs
#' afterwards in R.
json_mrf_jq_defs <- function() {
  '
def str: if . == null then "" elif type == "string" then . else tojson end;
def each: if type == "array" then .[] elif type == "object" then . else empty end;
def joined: if . == null then "" elif type == "array" then map(str) | join("|") else str end;
def or_empty_obj: if length == 0 then [{}] else . end;
def norm_code: gsub("^[\\\\s\\u00a0]+|[\\\\s\\u00a0]+$"; "") | ascii_upcase | sub("\\\\.0+$"; "");
def is_target: tostring as $c
  | if (($codes[0].procedure[$c] // false) or ($codes[0].drg[$c] // false)) then true
    else ($c | norm_code) as $n
      | ($codes[0].procedure[$n] // false) or ($codes[0].drg[($n | sub("^0+"; ""))] // false)
    end;
def meta_row($path; $value): {kind: "meta", path: ($path | map(tostring) | join("/")), value: ($value | str)};
def price_rows: . as $item
  | [($item.code_information // null) | each | select(type == "object" and .code != null) | select(.code | is_target)] as $hits
  | select(($hits | length) > 0)
  | $hits[] as $ci
  | ([($item.standard_charges // null) | each | select(type == "object")] | or_empty_obj)[] as $sc
  | ([($sc.payers_information // null) | each | select(type == "object")] | or_empty_obj)[] as $p
  | {
      kind: "price",
      description: ($item.description | str),
      code: ($ci.code | str),
      code_type: ($ci.type | str),
      setting: ($sc.setting | str),
      billing_class: (($sc.billing_class // $item.billing_class) | str),
      modifiers: ($sc.modifier_code | joined),
      gross: ($sc.gross_charge | str),
      discounted_cash: ($sc.discounted_cash | str),
      min: ($sc.minimum | str),
      max: ($sc.maximum | str),
      generic_notes: ($sc.additional_generic_notes | str),
      payer_name: ($p.payer_name | str),
      plan_name: ($p.plan_name | str),
      negotiated_dollar: ($p.standard_charge_dollar | str),
      negotiated_percentage: ($p.standard_charge_percentage | str),
      negotiated_algorithm: ($p.standard_charge_algorithm | str),
      methodology: ($p.methodology | str),
      median_amount: ($p.median_amount | str),
      p10: ($p["10th_percentile"] | str),
      p90: ($p["90th_percentile"] | str),
      count: ($p.count | str),
      estimated_amount: ($p.estimated_amount | str),
      payer_notes: ($p.additional_payer_notes | str)
    };
'
}

#' jq program: whole document in memory
json_mrf_jq_memory_program <- function() {
  base::paste0(json_mrf_jq_defs(), '
if type != "object" then empty else
  . as $root
  | (del(.standard_charge_information, .modifier_information) | paths(scalars) as $p | meta_row($p; $root | getpath($p))),
    ($root.standard_charge_information // null | each | select(type == "object") | price_rows)
end
')
}

#' jq program, streaming pass 1: (item index, code) for every code entry
json_mrf_jq_index_program <- function() {
  '
inputs
| select(length == 2 and .[0][0] == "standard_charge_information" and .[0][2] == "code_information"
         and (.[0][4] == "code" or .[0][3] == "code"))
| [.[0][1], (.[1] | tostring)] | @tsv
'
}

#' jq program, streaming pass 2: rebuild only flagged items, plus metadata
#'
#' `$flags[0]` is an array with 1 at the index of every item to rebuild.
json_mrf_jq_stream_program <- function() {
  base::paste0(json_mrf_jq_defs(), '
fromstream(
  inputs
  | if .[0][0] == "standard_charge_information" then
      (if (.[0] | length) > 2 and $flags[0][.[0][1]] == 1 then .[0] |= .[2:] else empty end)
    elif .[0][0] == "modifier_information" then empty
    elif length == 2 then [[], {"__meta__": .}]
    else empty end
)
| select(type == "object")
| if has("__meta__") then meta_row(.__meta__[0]; .__meta__[1]) else price_rows end
')
}

#' Codebook codes as the jq lookup object
json_mrf_code_lookup <- function(codebook) {
  procedure_codes <- codebook$code[codebook$code_family == "procedure"]
  drg_codes <- base::unique(stringr::str_replace(codebook$code[codebook$code_family == "ms_drg"], "^0+", ""))
  as_set <- function(codes) {
    if (base::length(codes) == 0L) {
      return(stats::setNames(base::list(), base::character()))
    }
    stats::setNames(base::as.list(base::rep(TRUE, base::length(codes))), codes)
  }

  jsonlite::toJSON(base::list(procedure = as_set(procedure_codes), drg = as_set(drg_codes)), auto_unbox = TRUE)
}

#' Run jq with a program and --slurpfile inputs; stdout goes to `out_path`
#' @param input_command Optional shell command whose stdout replaces `path`
#'   as jq's input (used to transcode UTF-16 files on the fly).
run_jq <- function(program, path, out_path, slurp = base::list(), args = base::character(), input_command = NULL) {
  program_path <- base::tempfile(fileext = ".jq")
  stderr_path <- base::tempfile(fileext = ".txt")
  slurp_paths <- base::vapply(base::names(slurp), function(name) base::tempfile(fileext = ".json"), base::character(1))
  base::on.exit(base::unlink(base::c(program_path, stderr_path, slurp_paths)), add = TRUE)

  base::writeLines(program, program_path)
  slurp_args <- base::character()

  for (name in base::names(slurp)) {
    base::writeLines(slurp[[name]], slurp_paths[[name]])
    slurp_args <- base::c(slurp_args, "--slurpfile", name, base::shQuote(slurp_paths[[name]]))
  }

  jq_args <- base::c(args, slurp_args, "-f", base::shQuote(program_path))

  status <- if (base::is.null(input_command)) {
    base::system2(jq_bin(), base::c(jq_args, base::shQuote(path)), stdout = out_path, stderr = stderr_path)
  } else {
    command <- base::paste(input_command, "|", base::shQuote(jq_bin()), base::paste(jq_args, collapse = " "))
    base::system2("bash", base::c("-c", base::shQuote(command)), stdout = out_path, stderr = stderr_path)
  }

  if (!base::identical(base::as.integer(status), 0L)) {
    stderr_text <- base::readLines(stderr_path, warn = FALSE)
    base::stop("jq failed on ", path, ":\n", base::paste(utils::tail(stderr_text, 10), collapse = "\n"))
  }

  base::invisible(out_path)
}

#' Indices of charge items holding a codebook code (streaming pass 1)
#'
#' @return List: `items` (0-based item indices to rebuild), `n_items`.
json_mrf_target_items <- function(path, codebook, input_command = NULL) {
  index_path <- base::tempfile(fileext = ".tsv")
  base::on.exit(base::unlink(index_path), add = TRUE)
  run_jq(json_mrf_jq_index_program(), path, index_path, args = base::c("-n", "-r", "--stream"),
         input_command = input_command)

  if (base::file.size(index_path) == 0) {
    return(base::list(items = base::integer(), n_items = 0L))
  }

  index <- readr::read_tsv(
    index_path,
    col_names = base::c("item", "code"),
    col_types = readr::cols(item = readr::col_integer(), code = readr::col_character()),
    quote = "", na = base::character(), progress = FALSE
  )

  distinct_codes <- base::unique(index$code)
  procedure_codes <- codebook$code[codebook$code_family == "procedure"]
  drg_codes <- codebook$code[codebook$code_family == "ms_drg"]
  is_candidate <- normalize_code(distinct_codes, "procedure") %in% procedure_codes |
    normalize_code(distinct_codes, "ms_drg") %in% drg_codes

  base::list(
    items = base::sort(base::unique(index$item[index$code %in% distinct_codes[is_candidate]])),
    n_items = base::max(index$item, na.rm = TRUE) + 1L
  )
}

#' Run the jq extraction; returns the path of the NDJSON output
#'
#' @param mode "memory", "stream", or "auto" (memory up to
#'   `HPT_JQ_MEMORY_MAX_MB`, else stream).
run_json_mrf_jq <- function(path, codebook, mode = "auto", out_path = base::tempfile(fileext = ".ndjson")) {
  if (mode == "auto") {
    max_mb <- base::as.numeric(base::Sys.getenv("HPT_JQ_MEMORY_MAX_MB", unset = "750"))
    mode <- if (base::file.size(path) <= max_mb * 1024^2) "memory" else "stream"
  }

  codes_json <- json_mrf_code_lookup(codebook)

  # jq reads UTF-8 (and tolerates stray bytes); UTF-16 is converted on the fly
  encoding <- mrf_file_encoding(path)
  input_command <- if (encoding %in% base::c("utf-16le", "utf-16be")) {
    base::paste("iconv -c -f", stringr::str_to_upper(encoding), "-t UTF-8", base::shQuote(base::normalizePath(path)))
  }

  if (mode == "memory") {
    run_jq(json_mrf_jq_memory_program(), path, out_path, slurp = base::list(codes = codes_json), args = "-c",
           input_command = input_command)
    return(base::list(path = out_path, mode = mode, target_items = NA_integer_))
  }

  targets <- json_mrf_target_items(path, codebook, input_command)
  base::message("  stream pass 1: ", scales::comma(base::length(targets$items)), " of ",
                scales::comma(targets$n_items), " charge items hold a target code")

  if (base::length(targets$items) == 0L) {
    base::file.create(out_path)
    return(base::list(path = out_path, mode = mode, target_items = 0L))
  }

  flags <- base::integer(targets$n_items)
  flags[targets$items + 1L] <- 1L
  flags_json <- base::paste0("[", base::paste(flags, collapse = ","), "]")

  run_jq(
    json_mrf_jq_stream_program(), path, out_path,
    slurp = base::list(codes = codes_json, flags = flags_json),
    args = base::c("-c", "-n", "--stream"),
    input_command = input_command
  )

  base::list(path = out_path, mode = mode, target_items = base::length(targets$items))
}

#' Collapse jq metadata path events into the canonical metadata list
json_mrf_metadata_from_events <- function(meta_events) {
  value_at <- function(pattern) {
    values <- meta_events$value[stringr::str_detect(meta_events$path, pattern)]
    values <- stringr::str_trim(values[!base::is.na(values)])
    values[base::nzchar(values)]
  }
  first_or_na <- function(values) if (base::length(values) == 0L) NA_character_ else values[[1]]
  join_or_na <- function(values, sep) if (base::length(values) == 0L) NA_character_ else base::paste(values, collapse = sep)

  base::list(
    hospital_name = first_or_na(value_at("^hospital_name$")),
    last_updated_on = normalize_mrf_date(first_or_na(value_at("^last_updated_on$"))),
    version = first_or_na(value_at("^version$")),
    location_name = join_or_na(value_at("^(location_name|hospital_location)(/[0-9]+)?$"), "; "),
    hospital_address = join_or_na(value_at("^hospital_address(/[0-9]+)?$"), "; "),
    license_number = first_or_na(value_at("^license_information/license_number$")),
    license_state = stringr::str_to_upper(first_or_na(value_at("^license_information/state$"))),
    type_2_npi = format_type_2_npi(value_at("^type_2_npi(/[0-9]+)?$")),
    attestation = first_or_na(value_at("^(attestation/confirm_attestation|affirmation/confirm_affirmation)$")),
    attester_name = first_or_na(value_at("^attestation/attester_name$"))
  )
}

#' Pull one top-level JSON value out of the start of a file by key
#'
#' Only for keys that never occur inside charge items (hospital_name,
#' version, license_information, ...), so a regex over a truncated prefix
#' is safe. Returns NULL when the key is absent or its value is cut off.
json_prefix_value <- function(text, key, shape = base::c("scalar", "array", "object")) {
  shape <- base::match.arg(shape)
  key_pattern <- base::paste0("\"", key, "\"\\s*:\\s*")
  patterns <- base::switch(
    shape,
    scalar = base::c(
      base::paste0(key_pattern, "(\"(?:[^\"\\\\]|\\\\.)*\")"),
      base::paste0(key_pattern, "(-?[0-9.]+|true|false)")
    ),
    # a single string is accepted where the schema wants an array
    array = base::c(
      base::paste0(key_pattern, "(\\[[^\\[\\]]*\\])"),
      base::paste0(key_pattern, "(\"(?:[^\"\\\\]|\\\\.)*\")")
    ),
    object = base::paste0(key_pattern, "(\\{[^{}]*\\})")
  )

  for (pattern in patterns) {
    match <- stringr::str_match(text, pattern)[, 2]

    if (!base::is.na(match)) {
      return(base::tryCatch(jsonlite::fromJSON(match, simplifyVector = TRUE), error = function(e) NULL))
    }
  }

  NULL
}

#' Top-level metadata from the first bytes of a JSON MRF (cheap, no jq)
#'
#' @param text The first ~64 KB of the file as text.
#' @return Named list like [read_csv_mrf_metadata()]; fields that come after
#'   the charge array in the file are NA.
parse_json_mrf_prefix <- function(text) {
  as_text <- function(value, sep = "; ") {
    if (base::is.null(value) || base::length(value) == 0L) {
      return(NA_character_)
    }
    value <- stringr::str_trim(base::as.character(base::unlist(value)))
    value <- value[!base::is.na(value) & base::nzchar(value)]
    if (base::length(value) == 0L) NA_character_ else base::paste(value, collapse = sep)
  }

  license <- json_prefix_value(text, "license_information", "object")
  attestation <- json_prefix_value(text, "attestation", "object") %||% json_prefix_value(text, "affirmation", "object")
  location <- json_prefix_value(text, "location_name", "array") %||% json_prefix_value(text, "hospital_location", "array")

  base::list(
    hospital_name = as_text(json_prefix_value(text, "hospital_name")),
    last_updated_on = normalize_mrf_date(as_text(json_prefix_value(text, "last_updated_on"))),
    version = as_text(json_prefix_value(text, "version")),
    location_name = as_text(location),
    hospital_address = as_text(json_prefix_value(text, "hospital_address", "array")),
    license_number = as_text(if (base::is.list(license)) license$license_number else NULL),
    license_state = stringr::str_to_upper(as_text(if (base::is.list(license)) license$state else NULL)),
    type_2_npi = format_type_2_npi(base::as.character(base::unlist(json_prefix_value(text, "type_2_npi", "array")))),
    attestation = as_text(if (base::is.list(attestation)) (attestation$confirm_attestation %||% attestation$confirm_affirmation) else NULL),
    attester_name = as_text(if (base::is.list(attestation)) attestation$attester_name else NULL)
  )
}

#' Cheap top-level metadata of a local JSON MRF (first 64 KB only)
read_json_mrf_metadata <- function(path, n_bytes = 65536L) {
  bytes <- base::readBin(path, what = "raw", n = n_bytes)
  parse_json_mrf_prefix(mrf_clean_text_lines(mrf_decode_bytes(bytes)))
}

#' Parse a JSON MRF into canonical price rows for the codebook's codes
#'
#' @param path Local JSON file.
#' @param codebook From [load_codebook()].
#' @param mrf_url Source URL, recorded on every row.
#' @param mrf_file_id File identifier; defaults to the file's sha256.
#' @param retrieved_at Download timestamp, recorded on every row.
#' @param mode jq mode: "auto", "memory", or "stream" (see file header).
#' @return [conform_price_table()] output (source = "own_crawl") with a
#'   `parse_log` attribute.
parse_json_mrf <- function(path,
                           codebook,
                           mrf_url = NA_character_,
                           mrf_file_id = NULL,
                           retrieved_at = NA_character_,
                           mode = "auto") {
  started <- base::Sys.time()
  base::message("Parsing JSON MRF: ", path)

  extraction <- run_json_mrf_jq(path, codebook, mode = mode)
  ndjson_path <- extraction$path
  base::on.exit(base::unlink(ndjson_path), add = TRUE)

  records <- if (base::file.size(ndjson_path) > 0) {
    tibble::as_tibble(yyjsonr::read_ndjson_file(ndjson_path, nprobe = -1))
  } else {
    tibble::tibble(kind = base::character())
  }

  meta_events <- records |> dplyr::filter(.data$kind == "meta")
  meta_events <- tibble::tibble(
    path = base::as.character(meta_events$path %||% base::character()),
    value = base::as.character(meta_events$value %||% base::character())
  )
  meta <- json_mrf_metadata_from_events(meta_events)

  # a stream run with no target items skips pass 2, so read the prefix instead
  if (base::nrow(meta_events) == 0L) {
    meta <- read_json_mrf_metadata(path)
  }

  if (base::is.na(meta$hospital_name) && base::is.na(meta$version)) {
    base::warning("No top-level metadata found in ", path, " (is the top level an array rather than a CMS object?)")
  }

  price_fields <- base::c(
    "description", "code", "code_type", "setting", "billing_class", "modifiers",
    "gross", "discounted_cash", "min", "max", "generic_notes", "payer_name", "plan_name",
    "negotiated_dollar", "negotiated_percentage", "negotiated_algorithm", "methodology",
    "median_amount", "p10", "p90", "count", "estimated_amount", "payer_notes"
  )
  candidates <- records |> dplyr::filter(.data$kind == "price")

  for (field in price_fields) {
    candidates[[field]] <- if (field %in% base::names(candidates)) mrf_blank_to_na(candidates[[field]]) else base::rep(NA_character_, base::nrow(candidates))
  }

  candidates <- candidates |> dplyr::rename(raw_code = "code", raw_type = "code_type")
  matched <- match_target_codes(candidates, "raw_code", "raw_type", codebook)

  if (base::nrow(matched) == 0L) {
    prices <- empty_price_table()
  } else {
    prices <- matched |>
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
  }

  out <- finalize_mrf_prices(
    prices, meta, mrf_url,
    mrf_file_id = mrf_file_id %||% mrf_sha256(path),
    retrieved_at = retrieved_at
  )

  base::message(
    "  jq ", extraction$mode, " mode, ", scales::comma(base::nrow(candidates)), " candidate rows, ",
    scales::comma(base::nrow(out)), " price rows for ",
    base::length(base::unique(out$concept)), " concepts in ",
    base::round(base::as.numeric(base::difftime(base::Sys.time(), started, units = "secs")), 1), " s"
  )

  base::attr(out, "parse_log") <- tibble::tibble(
    path = path,
    format = "json",
    layout = extraction$mode,
    encoding = NA_character_,
    invalid_bytes = NA_real_,
    candidate_rows = base::nrow(candidates),
    rejected_lines = NA_real_,
    price_rows = base::nrow(out)
  )

  out
}
