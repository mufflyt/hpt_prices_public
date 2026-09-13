#' Resolve where large data lives
#'
#' Hospital price data is too large for the local disk (about 80 GB for the
#' Trilliant lake archive alone, plus gap-crawl MRFs), so everything lives
#' under `HPT_DATA_DIR`, which defaults to the external drive. The pattern
#' follows `~/researchpaths/R/volumes.R::resolve_volume()`: fail fast with a
#' clear message instead of silently writing somewhere unexpected.
#'
#' Set `HPT_DATA_DIR` to override (tests point it at a temp directory).

#' The external drive's volume name is "MufflySamsung", but macOS mounts it at
#' "/Volumes/MufflySamsung 1" whenever a stale folder already occupies
#' "/Volumes/MufflySamsung" (true on 2026-09-12). A stale folder lives on the
#' boot disk, so writing there would silently fill it. Pick whichever
#' candidate is a real mount point (a different device from "/").
hpt_default_data_dir <- function() {
  candidates <- base::c("/Volumes/MufflySamsung", "/Volumes/MufflySamsung 1")

  for (candidate in candidates) {
    if (!base::dir.exists(candidate)) {
      next
    }

    # `df -P` reports the mount point that actually holds the path; it works
    # even where macOS privacy settings block listing the directory.
    df_lines <- base::suppressWarnings(
      base::system2("df", base::c("-P", base::shQuote(candidate)), stdout = TRUE, stderr = FALSE)
    )
    mounted_on <- stringr::str_match(utils::tail(df_lines, 1L), "\\s(/.*)$")[, 2]

    if (base::identical(mounted_on, candidate)) {
      return(base::file.path(candidate, "hpt_prices"))
    }
  }

  base::stop(
    "External drive 'MufflySamsung' is not mounted (checked: ",
    base::paste(candidates, collapse = ", "),
    "). Mount it or set HPT_DATA_DIR."
  )
}

hpt_data_dir <- function(must_exist = TRUE) {
  data_dir <- base::Sys.getenv("HPT_DATA_DIR", unset = hpt_default_data_dir())

  if (must_exist && !base::dir.exists(data_dir)) {
    parent_dir <- base::dirname(data_dir)

    if (!base::dir.exists(parent_dir)) {
      base::stop(
        "HPT data directory's parent is not available: ", parent_dir,
        "\nIs the external drive mounted? Set HPT_DATA_DIR to override."
      )
    }

    created <- base::dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

    if (!created) {
      base::stop(
        "Could not create HPT data directory: ", data_dir,
        "\nOn macOS, grant the terminal access to Removable Volumes ",
        "(System Settings > Privacy & Security > Files and Folders)."
      )
    }
  }

  data_dir
}

#' Path to a subdirectory of the data directory, created on demand
hpt_path <- function(...) {
  path <- base::file.path(hpt_data_dir(), ...)
  parent <- if (base::grepl("\\.[A-Za-z0-9]+$", path)) base::dirname(path) else path

  if (!base::dir.exists(parent)) {
    base::dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  }

  path
}
