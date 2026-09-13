#' The hospital universe: every CMS hospital, keyed by CCN
#'
#' Three public sources, each saved under `hpt_path("reference")` with a
#' provenance CSV so a later run can prove which release it used:
#' - CMS Hospital General Information (provider-data dataset xubh-q36u), the
#'   roster of ~5,400 hospitals of every type. No type filtering here.
#' - CMS Hospital Enrollments (data.cms.gov), which ties each enrolled
#'   hospital's NPI to its CCN, plus the "Hospital Additional NPIs" file.
#' - AHRQ Compendium of US Health Systems 2023 hospital linkage, for health
#'   system membership.
#'
#' CCNs are always 6-character strings. Leading zeros matter: "060011" is
#' Denver Health, and Hospital Enrollments has already lost that zero
#' upstream (see [normalize_ccn()]).

# ported from emb_colonoscopy R/hpt_hospital_discovery.R @ 471e067
normalize_public_names <- function(column_names) {
  normalized <- column_names |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", "_") |>
    stringr::str_replace_all("^_+|_+$", "")

  base::make.unique(normalized, sep = "_")
}

#' Apply column-name normalization to a raw CMS hospital frame
#'
#' CMS's real "City/Town" header normalizes to "city_town" (the "/" becomes
#' an underscore, same as the space in "Facility ID"), not "citytown". The
#' emb_colonoscopy repo found this only with a live download, because its
#' synthetic fixtures shared the code's wrong assumption. Downstream code
#' expects "citytown", so rename at this ingestion boundary.
#'
#' @param hospital_tbl A tibble/data.frame with raw CMS column headers.
#' @return `hospital_tbl` with normalized, `citytown`-corrected names.
# ported from emb_colonoscopy R/hpt_hospital_discovery.R @ 471e067
normalize_cms_hospital_frame_names <- function(hospital_tbl) {
  base::names(hospital_tbl) <- normalize_public_names(base::names(hospital_tbl))

  if ("city_town" %in% base::names(hospital_tbl) &&
      !"citytown" %in% base::names(hospital_tbl)) {
    hospital_tbl <- dplyr::rename(hospital_tbl, citytown = "city_town")
  }

  hospital_tbl
}

#' Stop with a clear message when a table lacks required columns
#'
#' Lists the columns actually found, so a renamed upstream header is
#' diagnosable from the error alone.
require_columns <- function(tbl, required_cols, what) {
  missing_cols <- base::setdiff(required_cols, base::names(tbl))

  if (base::length(missing_cols) > 0L) {
    base::stop(
      what, " is missing: ", base::paste(missing_cols, collapse = ", "),
      "\nColumns found: ", base::paste(base::names(tbl), collapse = ", ")
    )
  }

  base::invisible(tbl)
}

#' GET a public JSON document through the polite project client
#'
#' @param url URL to fetch.
#' @param timeout_s Whole-request timeout in seconds.
#' @param bearer_token Optional token (used only for the GitHub API).
fetch_public_json <- function(url, timeout_s = 60, bearer_token = NULL) {
  request_obj <- hpt_request(url, timeout_s = timeout_s, max_tries = 4)

  if (!base::is.null(bearer_token) && base::nzchar(bearer_token)) {
    request_obj <- httr2::req_auth_bearer_token(request_obj, bearer_token)
  }

  response <- httr2::req_perform(request_obj)
  status <- httr2::resp_status(response)

  if (status < 200L || status >= 300L) {
    base::stop("HTTP ", status, " from ", url)
  }

  httr2::resp_body_json(response, simplifyVector = FALSE)
}

#' Download a public file atomically
#'
#' Streams to `<destination>.part` and renames only after a 2xx response,
#' so an interrupted or failed download never looks finished.
#'
#' @param url File URL.
#' @param destination Final local path.
#' @param overwrite Re-download when `destination` already exists.
#' @param timeout_s Whole-request timeout in seconds.
# ported from emb_colonoscopy R/meps_download.R @ 471e067
download_public_file <- function(url,
                                 destination,
                                 overwrite = FALSE,
                                 timeout_s = 600) {
  base::message("Downloading public file: ", url)
  base::message("Destination: ", destination)

  if (base::file.exists(destination) && !overwrite) {
    base::message("Using existing file: ", destination)
    return(destination)
  }

  parent_dir <- base::dirname(destination)

  if (!base::dir.exists(parent_dir)) {
    base::dir.create(parent_dir, recursive = TRUE)
  }

  part_path <- base::paste0(destination, ".part")
  base::unlink(part_path)
  base::on.exit(base::unlink(part_path), add = TRUE)

  response <- hpt_request(url, timeout_s = timeout_s, max_tries = 4) |>
    httr2::req_perform(path = part_path)

  status <- httr2::resp_status(response)

  if (status < 200L || status >= 300L) {
    base::stop("Download failed with HTTP ", status, ": ", url)
  }

  # httr2 mocked responses keep the body in memory instead of writing `path`
  if (!base::file.exists(part_path)) {
    base::writeBin(httr2::resp_body_raw(response), part_path)
  }

  if (base::file.size(part_path) == 0L) {
    base::stop("Download was empty: ", url)
  }

  if (!base::file.rename(part_path, destination)) {
    base::stop("Could not move ", part_path, " to ", destination)
  }

  base::message("Downloaded: ", destination)

  destination
}

file_stamp <- function() {
  base::format(base::Sys.time(), "%Y%m%d_%H%M%S", tz = "UTC")
}

#' Most recently modified file in a directory matching a pattern
latest_reference_file <- function(directory, pattern) {
  paths <- base::list.files(directory, pattern = pattern, full.names = TRUE)

  if (base::length(paths) == 0L) {
    base::stop("No file matching '", pattern, "' in ", directory)
  }

  paths[[base::which.max(base::file.mtime(paths))]]
}

# ---- CMS Hospital General Information ---------------------------------------

# ported from emb_colonoscopy R/hpt_hospital_discovery.R @ 471e067
cms_provider_dataset_metadata <- function(identifier = "xubh-q36u") {
  metadata_url <- base::paste0(
    "https://data.cms.gov/provider-data/api/1/",
    "metastore/schemas/dataset/items/",
    identifier
  )

  base::message("Reading CMS provider metadata: ", metadata_url)

  fetch_public_json(metadata_url, timeout_s = 60)
}

# ported from emb_colonoscopy R/hpt_hospital_discovery.R @ 471e067
cms_csv_distribution_url <- function(metadata) {
  distributions <- metadata$distribution

  if (base::length(distributions) == 0L) {
    base::stop("CMS metadata contains no distributions.")
  }

  csv_distributions <- purrr::keep(
    distributions,
    function(item) {
      media_type <- item$mediaType %||% ""
      download_url <- item$downloadURL %||% ""

      base::identical(media_type, "text/csv") ||
        stringr::str_detect(download_url, stringr::regex("\\.csv($|\\?)", ignore_case = TRUE))
    }
  )

  if (base::length(csv_distributions) != 1L) {
    base::stop("Expected one CMS CSV distribution; found ", base::length(csv_distributions), ".")
  }

  csv_distributions[[1]]$downloadURL
}

cms_hospital_frame_columns <- function() {
  base::c(
    "facility_id", "facility_name", "address", "citytown", "state",
    "zip_code", "hospital_type", "hospital_ownership"
  )
}

#' Read a saved Hospital General Information CSV with normalized names
load_cms_hospital_frame <- function(path) {
  hospital_tbl <- read_csv_chr(path) |>
    normalize_cms_hospital_frame_names()

  require_columns(hospital_tbl, cms_hospital_frame_columns(), "CMS hospital frame")

  hospital_tbl
}

#' Download CMS Hospital General Information (every hospital type)
#'
#' @param directory Where the CSV and its provenance CSV are saved.
#' @param identifier CMS provider-data dataset id.
#' @return The roster with normalized column names (`citytown`, `zip_code`).
# ported from emb_colonoscopy R/hpt_hospital_discovery.R @ 471e067
download_cms_hospital_frame <- function(directory = hpt_path("reference", "cms_hospitals"),
                                        identifier = "xubh-q36u") {
  base::message("Starting CMS Hospital General Information download.")

  metadata <- cms_provider_dataset_metadata(identifier)
  csv_url <- cms_csv_distribution_url(metadata)
  stamp <- file_stamp()

  csv_path <- base::file.path(directory, base::paste0("hospital_general_information_", stamp, ".csv"))
  download_public_file(url = csv_url, destination = csv_path, overwrite = TRUE)

  hospital_tbl <- load_cms_hospital_frame(csv_path)

  provenance_tbl <- tibble::tibble(
    identifier = identifier,
    title = metadata$title %||% NA_character_,
    modified = metadata$modified %||% NA_character_,
    download_url = csv_url,
    local_path = csv_path,
    sha256 = sha256_file(csv_path),
    downloaded_at = utc_timestamp()
  )

  provenance_path <- base::file.path(
    directory,
    base::paste0("hospital_general_information_provenance_", stamp, ".csv")
  )
  write_csv_atomic(provenance_tbl, provenance_path)

  base::message("Saved CMS hospital frame (", base::nrow(hospital_tbl), " rows): ", csv_path)
  base::message("Saved CMS provenance: ", provenance_path)

  hospital_tbl
}

# ---- CMS Hospital Enrollments ------------------------------------------------

#' Normalize a CMS Certification Number to its 6-character form
#'
#' Hospital Enrollments stores CCN as a number upstream (the CSV and the
#' data-api JSON alike, checked live 2026-09-12), so states 01-09 lose their
#' leading zero: "60011" is Denver Health, 060011. A few practice-location
#' rows also append a location suffix ("22007401", "220083A", "330125001"),
#' and a suffixed CCN can lose its zero too ("7003301" is 070033 + "01").
#' Five- and seven-digit values are zero-padded, then anything longer than
#' six characters is truncated to the CCN itself. Six-character values
#' (including federal "01014F" and unit "06S011" forms) pass through. A
#' trailing ".0" from a spreadsheet's numeric read is removed first.
normalize_ccn <- function(x) {
  x <- stringr::str_to_upper(stringr::str_trim(base::as.character(x)))
  x <- stringr::str_remove(x, "\\.0+$")
  x <- dplyr::if_else(stringr::str_detect(x, "^[0-9]{5}$|^[0-9]{7}$"), base::paste0("0", x), x)
  x <- dplyr::if_else(!base::is.na(x) & base::nchar(x) > 6L, stringr::str_sub(x, 1L, 6L), x)

  dplyr::if_else(base::is.na(x) | !base::nzchar(x), NA_character_, x)
}

#' Is a CCN a hospital sub-unit (psych/rehab unit or swing beds)?
is_unit_ccn <- function(ccn) {
  !base::is.na(ccn) & stringr::str_detect(ccn, "^[0-9]{2}[MRSTUWYZ][0-9]{3}$")
}

#' Parent hospital CCN of a sub-unit CCN
#'
#' A unit CCN replaces the first digit of the parent's 4-digit sequence with
#' a letter: S/T/U are psych/rehab/swing-bed units of short-term hospitals
#' (0001-0879), M/R/Z of critical access hospitals (1300-1399), W swing beds
#' of long-term hospitals (2000-2299), Y of rehab hospitals (3025-3099). So
#' 06S011 -> 060011 and 04Z304 -> 041304. Checked live against Hospital
#' Enrollments: the derived parent exists for 99.3% of unit rows. Non-unit
#' CCNs are returned unchanged.
unit_parent_ccn <- function(ccn) {
  first_digit <- base::c(S = "0", T = "0", U = "0", M = "1", R = "1", Z = "1", W = "2", Y = "3")
  unit <- is_unit_ccn(ccn)
  letter <- stringr::str_sub(ccn, 3L, 3L)

  dplyr::if_else(
    unit,
    base::paste0(stringr::str_sub(ccn, 1L, 2L), base::unname(first_digit[letter]), stringr::str_sub(ccn, 4L, 6L)),
    ccn
  )
}

cms_data_catalog <- function(catalog_url = "https://data.cms.gov/data.json") {
  base::message("Reading CMS Open Data catalog: ", catalog_url)

  fetch_public_json(catalog_url, timeout_s = 120)
}

#' One dataset from the data.cms.gov data.json catalog, by exact title
cms_catalog_dataset <- function(catalog, title) {
  matches <- purrr::keep(
    catalog$dataset,
    function(item) base::identical(stringr::str_to_lower(item$title %||% ""), stringr::str_to_lower(title))
  )

  if (base::length(matches) != 1L) {
    base::stop("Expected one CMS catalog dataset titled '", title, "'; found ", base::length(matches), ".")
  }

  matches[[1]]
}

#' The current release of a data.cms.gov dataset
#'
#' Each monthly release appears in the catalog as an API distribution and a
#' CSV distribution sharing a title and temporal range; the current one's
#' API distribution carries description "latest". Falls back to the latest
#' temporal range when no distribution is marked. Resolving through the
#' catalog avoids hard-coding a release UUID that rots every month.
#'
#' @return list(title, temporal, modified, csv_url, resources_api).
cms_current_release <- function(dataset) {
  distribution_tbl <- purrr::map_dfr(
    dataset$distribution,
    function(item) {
      tibble::tibble(
        format = item$format %||% NA_character_,
        description = item$description %||% NA_character_,
        title = item$title %||% NA_character_,
        temporal = item$temporal %||% NA_character_,
        modified = item$modified %||% NA_character_,
        download_url = item$downloadURL %||% NA_character_,
        resources_api = item$resourcesAPI %||% NA_character_
      )
    }
  )

  if (base::nrow(distribution_tbl) == 0L) {
    base::stop("CMS catalog dataset has no distributions: ", dataset$title %||% "")
  }

  latest_tbl <- dplyr::filter(distribution_tbl, stringr::str_to_lower(.data$description) %in% "latest")
  current_temporal <- if (base::nrow(latest_tbl) == 1L) {
    latest_tbl$temporal
  } else {
    # ISO "YYYY-MM-DD/YYYY-MM-DD" ranges sort chronologically as text
    base::max(distribution_tbl$temporal, na.rm = TRUE)
  }

  release_tbl <- dplyr::filter(distribution_tbl, .data$temporal %in% current_temporal)
  csv_tbl <- dplyr::filter(
    release_tbl,
    .data$format %in% "CSV" |
      stringr::str_detect(.data$download_url, stringr::regex("\\.csv$", ignore_case = TRUE))
  )

  if (base::nrow(csv_tbl) != 1L) {
    base::stop("Expected one CSV distribution for release ", current_temporal, "; found ", base::nrow(csv_tbl), ".")
  }

  base::list(
    title = csv_tbl$title,
    temporal = current_temporal,
    modified = csv_tbl$modified,
    csv_url = csv_tbl$download_url,
    resources_api = csv_tbl$resources_api
  )
}

#' Files attached to a data.cms.gov release (data file, dictionary, extras)
cms_dataset_resources <- function(resources_api) {
  if (base::is.na(resources_api) || !base::nzchar(resources_api)) {
    return(tibble::tibble(name = base::character(), file_size = base::numeric(), download_url = base::character()))
  }

  payload <- fetch_public_json(resources_api, timeout_s = 60)

  purrr::map_dfr(
    payload$data,
    function(item) {
      tibble::tibble(
        name = item$name %||% NA_character_,
        file_size = base::as.numeric(item$fileSize %||% NA_real_),
        download_url = item$downloadURL %||% NA_character_
      )
    }
  )
}

#' Pick the enrollment CSV and the Additional NPIs CSV from a release
#'
#' @return list(enrollments = URL, additional_npis = URL or NA).
select_enrollment_resources <- function(resources, fallback_csv_url) {
  csv_resources <- dplyr::filter(
    resources,
    stringr::str_detect(.data$download_url, stringr::regex("\\.csv$", ignore_case = TRUE))
  )
  enrollment_url <- csv_resources$download_url[
    stringr::str_detect(csv_resources$name, stringr::regex("^hospital enrollments", ignore_case = TRUE))
  ]
  additional_url <- csv_resources$download_url[
    stringr::str_detect(csv_resources$name, stringr::regex("additional npis", ignore_case = TRUE))
  ]

  if (base::length(additional_url) == 0L) {
    base::message("No 'Additional NPIs' file listed for this release; primary NPIs only.")
  }

  base::list(
    enrollments = if (base::length(enrollment_url) == 1L) enrollment_url else fallback_csv_url,
    additional_npis = if (base::length(additional_url) == 1L) additional_url else NA_character_
  )
}

enrollment_required_columns <- function() {
  base::c(
    "enrollment_id", "npi", "ccn", "organization_name", "doing_business_as_name",
    "state", "practice_location_type", "associate_id"
  )
}

#' Read saved Hospital Enrollments files with normalized names
#'
#' Raw headers are upper case with spaces ("ENROLLMENT ID", "DOING BUSINESS
#' AS NAME", "CAH OR HOSPITAL CCN"); the Additional NPIs file has only
#' "ENROLLMENT ID","NPI". Both verified against the live 2026-08 release.
#'
#' @return list(enrollments, additional_npis); additional_npis is NULL when
#'   no path is given.
load_hospital_enrollments <- function(enrollments_path, additional_npis_path = NULL) {
  enrollments <- read_csv_chr(enrollments_path)
  base::names(enrollments) <- normalize_public_names(base::names(enrollments))
  require_columns(enrollments, enrollment_required_columns(), "Hospital Enrollments")

  additional_npis <- NULL

  if (!base::is.null(additional_npis_path) && !base::is.na(additional_npis_path)) {
    additional_npis <- read_csv_chr(additional_npis_path)
    base::names(additional_npis) <- normalize_public_names(base::names(additional_npis))
    require_columns(additional_npis, base::c("enrollment_id", "npi"), "Hospital Additional NPIs")
  }

  base::list(enrollments = enrollments, additional_npis = additional_npis)
}

#' Download the current CMS Hospital Enrollments release
#'
#' Resolves the release through the data.cms.gov catalog, downloads the
#' enrollment CSV and the "Hospital Additional NPIs" CSV, and writes a
#' provenance CSV.
#'
#' @return list(enrollments, additional_npis, provenance).
download_hospital_enrollments <- function(directory = hpt_path("reference", "cms_enrollments"),
                                          catalog_url = "https://data.cms.gov/data.json",
                                          dataset_title = "Hospital Enrollments") {
  catalog <- cms_data_catalog(catalog_url)
  dataset <- cms_catalog_dataset(catalog, dataset_title)
  release <- cms_current_release(dataset)

  base::message("Current ", dataset_title, " release: ", release$title, " (modified ", release$modified, ")")

  resources <- cms_dataset_resources(release$resources_api)
  urls <- select_enrollment_resources(resources, fallback_csv_url = release$csv_url)

  enrollments_path <- base::file.path(directory, base::basename(urls$enrollments))
  download_public_file(urls$enrollments, enrollments_path, overwrite = TRUE)

  additional_path <- NA_character_

  if (!base::is.na(urls$additional_npis)) {
    additional_path <- base::file.path(directory, base::basename(urls$additional_npis))
    download_public_file(urls$additional_npis, additional_path, overwrite = TRUE)
  }

  loaded <- load_hospital_enrollments(enrollments_path, additional_path)
  saved_paths <- base::c(enrollments = enrollments_path, additional_npis = additional_path)
  saved_paths <- saved_paths[!base::is.na(saved_paths)]

  provenance_tbl <- tibble::tibble(
    dataset_title = dataset_title,
    release_title = release$title,
    temporal = release$temporal,
    modified = release$modified,
    file_role = base::names(saved_paths),
    download_url = base::unname(base::unlist(urls[base::names(saved_paths)])),
    local_path = base::unname(saved_paths),
    rows = base::c(base::nrow(loaded$enrollments), base::nrow(loaded$additional_npis))[base::seq_along(saved_paths)],
    sha256 = base::vapply(saved_paths, sha256_file, base::character(1), USE.NAMES = FALSE),
    downloaded_at = utc_timestamp()
  )

  write_csv_atomic(
    provenance_tbl,
    base::file.path(directory, base::paste0("hospital_enrollments_provenance_", file_stamp(), ".csv"))
  )

  base::message(
    "Hospital Enrollments: ", base::nrow(loaded$enrollments), " rows; Additional NPIs: ",
    base::nrow(loaded$additional_npis) %||% 0L, " rows."
  )

  base::c(loaded, base::list(provenance = provenance_tbl))
}

#' Every NPI-CCN pair in CMS Hospital Enrollments
#'
#' Unit CCNs (psych/rehab units, swing beds) are collapsed to their parent
#' hospital's CCN, since the roster lists hospitals, and a unit usually
#' shares the parent's NPI anyway. Additional NPIs inherit the CCN of their
#' enrollment. One row per NPI x CCN; `source` is "enrollment" when the NPI
#' is an enrollment's primary NPI, else "additional_npi"; `enrollment_ccns`
#' lists the normalized enrollment CCNs that collapsed into the pair.
#'
#' @param enrollments Normalized enrollment tibble ([load_hospital_enrollments()]).
#' @param additional_npis Normalized Additional NPIs tibble, or NULL.
npi_ccn_crosswalk <- function(enrollments, additional_npis = NULL) {
  require_columns(enrollments, enrollment_required_columns(), "Hospital Enrollments")

  enrollment_tbl <- enrollments |>
    dplyr::transmute(
      enrollment_id = .data$enrollment_id,
      npi = stringr::str_trim(.data$npi),
      enrollment_ccn = normalize_ccn(.data$ccn),
      ccn = unit_parent_ccn(.data$enrollment_ccn),
      organization_name = .data$organization_name,
      doing_business_as_name = .data$doing_business_as_name,
      state = .data$state
    )

  pair_tbl <- dplyr::mutate(enrollment_tbl, source = "enrollment")

  if (!base::is.null(additional_npis)) {
    additional_tbl <- additional_npis |>
      dplyr::transmute(enrollment_id = .data$enrollment_id, npi = stringr::str_trim(.data$npi)) |>
      dplyr::inner_join(dplyr::select(enrollment_tbl, -"npi"), by = "enrollment_id", relationship = "many-to-many") |>
      dplyr::mutate(source = "additional_npi")

    pair_tbl <- dplyr::bind_rows(pair_tbl, additional_tbl)
  }

  pair_tbl |>
    dplyr::filter(stringr::str_detect(.data$npi, "^[0-9]{10}$"), !base::is.na(.data$ccn)) |>
    dplyr::arrange(.data$npi, .data$ccn, .data$source != "enrollment", .data$enrollment_ccn) |>
    dplyr::group_by(.data$npi, .data$ccn) |>
    dplyr::summarise(
      organization_name = dplyr::first(.data$organization_name),
      doing_business_as_name = dplyr::first(.data$doing_business_as_name[!base::is.na(.data$doing_business_as_name)]),
      state = dplyr::first(.data$state),
      source = dplyr::first(.data$source),
      enrollment_ccns = base::paste(base::sort(base::unique(.data$enrollment_ccn)), collapse = ";"),
      .groups = "drop"
    )
}

# ---- AHRQ Compendium of US Health Systems ------------------------------------

chsp_default_source <- function() {
  "/Users/tylermuffly/consolidation/data/raw/ahrq/chsp-hospital-linkage-2023.csv"
}

#' Copy the AHRQ CHSP 2023 hospital linkage into the reference directory
#'
#' Runs read the copy, never the sibling repository, and the provenance row
#' records where it came from and its hash.
#'
#' @return Path to the copied file.
import_chsp_linkage <- function(source_path = chsp_default_source(),
                                directory = hpt_path("reference", "ahrq")) {
  if (!base::file.exists(source_path)) {
    base::stop("CHSP linkage file not found: ", source_path)
  }

  if (!base::dir.exists(directory)) {
    base::dir.create(directory, recursive = TRUE)
  }

  local_path <- base::file.path(directory, base::basename(source_path))
  temp_path <- base::paste0(local_path, ".tmp")
  base::file.copy(source_path, temp_path, overwrite = TRUE)
  base::file.rename(temp_path, local_path)

  provenance_tbl <- tibble::tibble(
    dataset = "AHRQ Compendium of US Health Systems, 2023 hospital linkage",
    source_url = "https://www.ahrq.gov/chsp/data-resources/compendium-2023.html",
    copied_from = source_path,
    local_path = local_path,
    sha256 = sha256_file(local_path),
    copied_at = utc_timestamp()
  )
  write_csv_atomic(provenance_tbl, base::file.path(directory, "chsp_hospital_linkage_provenance.csv"))

  base::message("Copied CHSP linkage: ", local_path)

  local_path
}

#' Load the AHRQ CHSP hospital linkage, one row per CCN
#'
#' Keeps every column (beds, discharges, teaching flags, ownership, revenue,
#' corporate parent) and normalizes `ccn`. Rows without a CCN (124 in the
#' 2023 file) are dropped because nothing can join to them.
load_chsp_linkage <- function(path = hpt_path("reference", "ahrq", "chsp-hospital-linkage-2023.csv")) {
  chsp_tbl <- read_csv_chr(path)
  require_columns(chsp_tbl, base::c("ccn", "health_sys_id", "health_sys_name"), "CHSP hospital linkage")

  chsp_tbl <- chsp_tbl |>
    dplyr::mutate(ccn = normalize_ccn(.data$ccn)) |>
    dplyr::filter(!base::is.na(.data$ccn))

  duplicated_ccns <- base::unique(chsp_tbl$ccn[base::duplicated(chsp_tbl$ccn)])

  if (base::length(duplicated_ccns) > 0L) {
    base::warning("CHSP linkage lists ", base::length(duplicated_ccns), " CCNs twice; keeping the first row of each.")
    chsp_tbl <- dplyr::distinct(chsp_tbl, .data$ccn, .keep_all = TRUE)
  }

  chsp_tbl
}

#' One row per CMS hospital with health-system membership
#'
#' @param roster Normalized Hospital General Information tibble.
#' @param chsp [load_chsp_linkage()] output, or NULL.
#' @return tibble(facility_id, facility_name, address, citytown, state,
#'   zip_code, hospital_type, hospital_ownership, health_sys_id,
#'   health_sys_name, hos_beds, chsp_linked).
build_hospital_universe <- function(roster, chsp = NULL) {
  require_columns(roster, cms_hospital_frame_columns(), "CMS hospital roster")

  universe <- roster |>
    dplyr::transmute(
      facility_id = normalize_ccn(.data$facility_id),
      facility_name = .data$facility_name,
      address = .data$address,
      citytown = .data$citytown,
      state = .data$state,
      zip_code = .data$zip_code,
      hospital_type = .data$hospital_type,
      hospital_ownership = .data$hospital_ownership
    )

  duplicated_ids <- base::unique(universe$facility_id[base::duplicated(universe$facility_id)])

  if (base::length(duplicated_ids) > 0L) {
    base::stop("CMS roster repeats CCNs: ", base::paste(utils::head(duplicated_ids, 10L), collapse = ", "))
  }

  if (base::is.null(chsp)) {
    return(dplyr::mutate(
      universe,
      health_sys_id = NA_character_, health_sys_name = NA_character_,
      hos_beds = NA_character_, chsp_linked = FALSE
    ))
  }

  chsp_cols <- dplyr::select(chsp, "ccn", "health_sys_id", "health_sys_name", dplyr::any_of("hos_beds"))

  universe <- universe |>
    dplyr::left_join(chsp_cols, by = base::c(facility_id = "ccn")) |>
    dplyr::mutate(chsp_linked = .data$facility_id %in% chsp$ccn)

  if (!"hos_beds" %in% base::names(universe)) {
    universe$hos_beds <- NA_character_
  }

  base::message(
    "Hospital universe: ", base::nrow(universe), " CCNs; ",
    base::sum(universe$chsp_linked), " linked to CHSP; ",
    base::sum(!base::is.na(universe$health_sys_id)), " in a health system."
  )

  universe
}
