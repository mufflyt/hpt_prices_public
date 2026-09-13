#' Run SQL through the DuckDB command-line binary
#'
#' The Trilliant lake is DuckLake format 1.0, which needs DuckDB >= 1.5. The
#' installed R `duckdb` package is 1.4.4, so lake queries run through the
#' CLI (`duckdb` >= 1.5 on PATH, or HPT_DUCKDB_BIN) and write their results
#' to Parquet, which R then reads with arrow. Local CSV/JSON parsing that
#' doesn't need DuckLake also uses the CLI, so there is one DuckDB version
#' in play everywhere.

duckdb_bin <- function() {
  base::Sys.getenv("HPT_DUCKDB_BIN", unset = "duckdb")
}

duckdb_cli_version <- function() {
  out <- base::system2(duckdb_bin(), "--version", stdout = TRUE, stderr = TRUE)
  stringr::str_match(out[[1]], "v?([0-9]+\\.[0-9]+\\.[0-9]+)")[, 2]
}

require_duckdb_cli <- function(min_version = "1.5.0") {
  version <- duckdb_cli_version()

  if (base::is.na(version) || utils::compareVersion(version, min_version) < 0) {
    base::stop(
      "DuckDB CLI >= ", min_version, " is required (found ", version %||% "none",
      "). Install with `brew install duckdb` or set HPT_DUCKDB_BIN."
    )
  }

  base::invisible(version)
}

#' Quote a string as a SQL literal
sql_string <- function(x) {
  base::paste0("'", stringr::str_replace_all(x, "'", "''"), "'")
}

sql_string_list <- function(x) {
  base::paste(sql_string(x), collapse = ", ")
}

#' Execute a SQL script with the DuckDB CLI
#'
#' @param sql SQL text (multiple statements allowed).
#' @param database Database file to open (default in-memory).
#' @param read_only Open the database read-only.
#' @param init Optional SQL run before `sql` (e.g. ATTACH statements).
#' @return Invisibly, the CLI's stdout lines. Errors if DuckDB exits non-zero.
run_duckdb_sql <- function(sql, database = NULL, read_only = FALSE, init = NULL) {
  script_path <- base::tempfile(fileext = ".sql")
  base::on.exit(base::unlink(script_path), add = TRUE)
  base::writeLines(base::c(init, sql), script_path)

  args <- base::c(
    if (read_only) "-readonly",
    "-bail",
    if (!base::is.null(database)) base::shQuote(database),
    "-f", base::shQuote(script_path)
  )

  output <- base::suppressWarnings(
    base::system2(duckdb_bin(), args, stdout = TRUE, stderr = TRUE)
  )
  status <- base::attr(output, "status") %||% 0L

  if (!base::identical(base::as.integer(status), 0L) ||
      base::any(stringr::str_detect(output, "^(Error|.*Error:)"))) {
    base::stop("DuckDB CLI failed:\n", base::paste(utils::tail(output, 20), collapse = "\n"))
  }

  base::invisible(output)
}

#' Run a SELECT and return the result as a tibble (via a temp Parquet file)
duckdb_query <- function(select_sql, database = NULL, read_only = FALSE, init = NULL) {
  out_path <- base::tempfile(fileext = ".parquet")
  base::on.exit(base::unlink(out_path), add = TRUE)

  run_duckdb_sql(
    base::sprintf("COPY (%s) TO %s (FORMAT parquet);", select_sql, sql_string(out_path)),
    database = database,
    read_only = read_only,
    init = init
  )

  tibble::as_tibble(arrow::read_parquet(out_path))
}
