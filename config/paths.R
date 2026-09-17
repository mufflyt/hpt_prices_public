#' Resolve where large data lives
#'
#' Hospital price data is too large for the local disk (about 80 GB for the
#' Trilliant lake archive alone, plus gap-crawl MRFs), so everything lives
#' under `HPT_DATA_DIR`, which defaults to the external drive. The pattern
#' follows `~/researchpaths/R/volumes.R::resolve_volume()`: fail fast with a
#' clear message instead of silently writing somewhere unexpected.
#'
#' Set `HPT_DATA_DIR` to override (tests point it at a temp directory).

#' The external drive's volume name is "MufflySamsung", but macOS appends a
#' number whenever a stale folder already occupies the name: it mounted at
#' "/Volumes/MufflySamsung 1" on 2026-09-12 and at "/Volumes/MufflySamsung 3"
#' on 2026-09-17, by which point "MufflySamsung", " 1" and " 2" were all
#' stale, empty folders on the boot disk. So the candidates are globbed rather
#' than listed: a hardcoded list goes stale the next time the drive is
#' remounted, and the failure is a confusing "not mounted" while the drive sits
#' there mounted.
#'
#' A stale folder lives on the boot disk, where writing 80 GB of lake would
#' silently fill it, so a candidate counts only if it is a real mount point (df
#' reports the candidate itself, not "/"). Among real mount points, one already
#' holding `hpt_prices` wins, so a second, empty copy of the drive cannot
#' quietly become the destination.
hpt_default_data_dir <- function() {
  candidates <- hpt_volume_candidates()

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

#' Candidate mount paths for the data drive, best first
#'
#' Globbed so a remount under a different number is still found, sorted so the
#' order is stable, and with any path already holding `hpt_prices` first.
#' Whether a candidate is a real mount point is checked by the caller.
#'
#' @param pattern glob for the volume; the default matches every number macOS
#'   may append.
hpt_volume_candidates <- function(pattern = "/Volumes/MufflySamsung*") {
  found <- base::sort(base::Sys.glob(pattern))
  has_data <- base::vapply(found, function(p) base::dir.exists(base::file.path(p, "hpt_prices")),
                           base::logical(1), USE.NAMES = FALSE)
  base::c(found[has_data], found[!has_data])
}

hpt_data_dir <- function(must_exist = TRUE) {
  # Sys.getenv() always evaluates `unset`, so the drive check must not sit there:
  # it would fail on any machine without the drive even when HPT_DATA_DIR is set
  data_dir <- base::Sys.getenv("HPT_DATA_DIR", unset = "")
  if (!base::nzchar(data_dir)) {
    data_dir <- hpt_default_data_dir()
  }

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
