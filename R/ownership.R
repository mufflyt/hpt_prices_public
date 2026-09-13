#' Hospital ownership and negotiated prices: do private-equity (PE) owned
#' hospitals post higher prices than other hospitals?
#'
#' Ownership comes from CMS "Hospital All Owners" (data.cms.gov), one row per
#' enrollment x owner, linked to CCNs through CMS Hospital Enrollments
#' (ENROLLMENT ID -> CCN). Every hospital in the CMS roster (dim_hospital)
#' gets one ownership group per PE definition (pe_definitions()):
#'   pe                 PE owned under that definition;
#'   cms_pe_flag        an owner flagged PRIVATE EQUITY COMPANY holds an
#'                      ownership interest, but the system list does not
#'                      count the hospital as PE (system-list definitions);
#'   distressed_fund    owned by creditor or distressed-debt funds
#'                      (system-list definitions);
#'   for_profit_non_pe  roster ownership "Proprietary" or "Physician";
#'   nonprofit          roster ownership "Voluntary non-profit - ...";
#'   government         any "Government - ...", VHA, DoD, or Tribal.
#' The system-list definitions come from config/pe_hospital_systems.csv, a
#' sourced, dated list of PE-owned hospital systems mapped to CCNs through
#' the owner names in Hospital All Owners (map_pe_systems()).
#'
#' Hospital prices follow R/state_medians.R exactly (plausible negotiated
#' dollars, facility fees, outpatient procedures without explicitly inpatient
#' rows, contract median then hospital median; discounted cash per charge
#' line). The model regresses log(hospital price) on ownership group with
#' state fixed effects and hospital covariates, clustered by health system,
#' one model per PE definition. Intervals for the plotted groups come from a
#' wild cluster restricted bootstrap (wild_cluster_bootstrap()); groups drawn
#' from fewer than min_treated_clusters() health systems are exploratory.

# ---- CMS Hospital All Owners -------------------------------------------------

#' Columns the classification needs (normalized from the live 2026-08 headers,
#' e.g. "PRIVATE EQUITY COMPANY - OWNER" -> private_equity_company_owner)
owners_required_columns <- function() {
  base::c(
    "enrollment_id", "organization_name", "associate_id_owner", "type_owner",
    "role_code_owner", "role_text_owner", "organization_name_owner",
    "percentage_ownership", "private_equity_company_owner"
  )
}

#' Y/N organization-type flags reported alongside the PE flag (kept when present)
owners_flag_columns <- function() {
  base::c(
    "private_equity_company_owner", "investment_firm_owner", "holding_company_owner",
    "corporation_owner", "for_profit_owner", "non_profit_owner", "reit_owner",
    "created_for_acquisition_owner", "owned_by_another_org_or_ind_owner"
  )
}

#' Read a saved Hospital All Owners CSV with normalized names
#'
#' Owner names carry stray Windows-1252 bytes ("CHILDREN\xbfS"), repaired
#' here so regex matching works. Role codes are zero-padded to two digits.
load_hospital_owners <- function(path) {
  owners <- read_csv_chr(path)
  base::names(owners) <- normalize_public_names(base::names(owners))
  require_columns(owners, owners_required_columns(), "Hospital All Owners")

  owners |>
    repair_utf8() |>
    dplyr::mutate(
      role_code_owner = stringr::str_pad(stringr::str_trim(.data$role_code_owner), 2L, pad = "0"),
      organization_name_owner = stringr::str_squish(stringr::str_to_upper(.data$organization_name_owner))
    )
}

#' Download the current CMS Hospital All Owners release
#'
#' Resolves the release through the data.cms.gov catalog, as
#' download_hospital_enrollments() does, saves the CSV and the data
#' dictionary, and writes a provenance CSV (URL, release, sha256). The
#' file name carries the release date, so an existing copy is reused.
#'
#' @return list(owners, provenance, path).
download_hospital_owners <- function(directory = hpt_path("reference", "cms_owners"),
                                     catalog_url = "https://data.cms.gov/data.json",
                                     dataset_title = "Hospital All Owners") {
  catalog <- cms_data_catalog(catalog_url)
  dataset <- cms_catalog_dataset(catalog, dataset_title)
  release <- cms_current_release(dataset)

  base::message("Current ", dataset_title, " release: ", release$title, " (modified ", release$modified, ")")

  resources <- cms_dataset_resources(release$resources_api)
  is_csv <- stringr::str_detect(resources$download_url %||% base::character(), stringr::regex("\\.csv$", ignore_case = TRUE))
  csv_url <- resources$download_url[is_csv & stringr::str_detect(resources$name, stringr::regex("^hospital all owners", ignore_case = TRUE))]
  csv_url <- if (base::length(csv_url) == 1L) csv_url else release$csv_url
  dictionary_url <- resources$download_url[stringr::str_detect(resources$name, stringr::regex("dictionary", ignore_case = TRUE))]

  urls <- base::c(owners = csv_url, dictionary = if (base::length(dictionary_url) == 1L) dictionary_url else NA_character_)
  urls <- urls[!base::is.na(urls)]
  paths <- base::file.path(directory, base::basename(urls))
  base::names(paths) <- base::names(urls)

  for (role in base::names(urls)) {
    download_public_file(urls[[role]], paths[[role]], overwrite = FALSE)
  }

  owners <- load_hospital_owners(paths[["owners"]])

  provenance_tbl <- tibble::tibble(
    dataset_title = dataset_title,
    release_title = release$title,
    temporal = release$temporal,
    modified = release$modified,
    file_role = base::names(paths),
    download_url = base::unname(urls),
    local_path = base::unname(paths),
    rows = base::ifelse(base::names(paths) == "owners", base::nrow(owners), NA_integer_),
    sha256 = base::vapply(paths, sha256_file, base::character(1), USE.NAMES = FALSE),
    downloaded_at = utc_timestamp()
  )
  # one provenance file per distinct download, not one per run
  recorded <- base::list.files(directory, pattern = "^hospital_all_owners_provenance_.*\\.csv$", full.names = TRUE) |>
    purrr::map(read_csv_chr) |>
    purrr::map(function(tbl) tbl$sha256) |>
    base::unlist()
  if (!provenance_tbl$sha256[[1]] %in% recorded) {
    write_csv_atomic(provenance_tbl, base::file.path(directory, base::paste0("hospital_all_owners_provenance_", file_stamp(), ".csv")))
  }

  base::message("Hospital All Owners: ", base::nrow(owners), " rows, ", dplyr::n_distinct(owners$enrollment_id), " enrollments.")

  base::list(owners = owners, provenance = provenance_tbl, path = paths[["owners"]])
}

# ---- classification ----------------------------------------------------------

#' Owner role codes (data dictionary, "Owner Role Code Reference Table")
#'
#' Ownership roles are direct (01, 34), indirect (35), and partnership
#' interests (03, 38, 39). Mortgage/security interests (36, 37), managerial
#' control (43), and "other" (44) count only in the any-role sensitivity.
owner_role_codes <- function() {
  base::list(
    direct = base::c("01", "34"),
    indirect = "35",
    partnership = base::c("03", "38", "39"),
    ownership = base::c("01", "03", "34", "35", "38", "39")
  )
}

#' Owner names that mark a private investment fund
#'
#' The CMS PE flag is self-reported and sparse (41 of 9,161 enrollments in the
#' 2026-08 release), so the "PE or fund" sensitivity also counts ownership
#' interests held by owners whose legal name marks a fund vehicle (private
#' equity, equity/capital partners, fund, AIV, co-invest), plus sponsors
#' found by scanning the 2026-08 owner names whose vehicles lack those words.
#' Physician "... INVESTORS LLC" vehicles and public asset managers (Vanguard,
#' BlackRock) deliberately do not match. Review the matches in
#' ownership_pe_owners.csv before relying on this definition.
pe_fund_name_pattern <- function() {
  base::paste(
    base::c(
      "PRIVATE EQUITY", "EQUITY PARTNERS", "CAPITAL PARTNERS", "\\bFUNDS?\\b", "\\bAIV\\b", "CO-?INVEST",
      "^GENERAL ATLANTIC", "^DAVIDSON KEMPNER", "^DK (LDOI|DISTRESSED|QUINCY)", "^DKLDOI", "^GOLDENTREE", "^GTAM ",
      "^WELSH,? CARSON", "^TPG VII", "^BAIN CAPITAL", "^ONE EQUITY", "^DEERFIELD", "^OAK HC/FT", "^BW CLEARSKY"
    ),
    collapse = "|"
  )
}

#' Map CMS roster ownership text to a non-PE ownership group
roster_ownership_group <- function(hospital_ownership) {
  text <- stringr::str_to_lower(hospital_ownership)

  dplyr::case_when(
    stringr::str_detect(text, "non-profit|nonprofit") ~ "nonprofit",
    stringr::str_detect(text, "^proprietary|^physician") ~ "for_profit_non_pe",
    stringr::str_detect(text, "government|veterans|defense|tribal") ~ "government",
    TRUE ~ "unknown"
  )
}

#' Ownership group levels, reference (nonprofit) first
ownership_group_levels <- function() {
  base::c("nonprofit", "pe", "cms_pe_flag", "distressed_fund", "for_profit_non_pe", "government")
}

#' PE definitions (ownership_group_for() holds the rules)
pe_definitions <- function() {
  tibble::tribble(
    ~definition,           ~label,
    "pe_strict",           "PE system list, strict (clear PE fund control as of 2026-07)",
    "pe_broad",            "PE system list, strict plus ambiguous (family office, public company with PE stake, JV, undisclosed stake)",
    "pe_broad_all",        "PE system list, every listed system including creditor-owned, plus the CMS flag",
    "cms_flag",            "CMS PE flag, ownership interest",
    "cms_flag_5pct",       "CMS PE flag, PE share >= 5%",
    "cms_flag_any_role",   "CMS PE flag, any role",
    "fund_name",           "CMS PE flag or fund-named owner (mostly distressed-debt funds)",
    "researcher_list",     "pe_strict plus a researcher-supplied CCN list"
  )
}

#' The ownership group of every hospital under one PE definition
#'
#' System-list definitions keep hospitals with a CMS PE flag and hospitals
#' owned by creditor or distressed-debt funds as their own groups, so they
#' never mix into "pe" or the roster groups.
ownership_group_for <- function(classified, definition) {
  roster <- classified$roster_group
  strict <- classified$pe_system_class %in% "strict"
  creditor <- classified$pe_system_class %in% "ambiguous" & classified$pe_system_ambiguity %in% "creditor"
  broad <- strict | (classified$pe_system_class %in% "ambiguous" & !creditor)
  cms <- classified$pe_cms

  switch(
    definition,
    pe_strict = dplyr::case_when(strict ~ "pe", cms ~ "cms_pe_flag", creditor ~ "distressed_fund", TRUE ~ roster),
    pe_broad = dplyr::case_when(broad ~ "pe", cms ~ "cms_pe_flag", creditor ~ "distressed_fund", TRUE ~ roster),
    pe_broad_all = dplyr::if_else(broad | creditor | cms, "pe", roster),
    cms_flag = dplyr::if_else(cms, "pe", roster),
    cms_flag_5pct = dplyr::if_else(classified$pe_cms_5pct, "pe", roster),
    cms_flag_any_role = dplyr::if_else(classified$pe_cms_any_role, "pe", roster),
    fund_name = dplyr::if_else(classified$pe_fund, "pe", roster),
    researcher_list = dplyr::case_when(strict | classified$pe_list ~ "pe", cms ~ "cms_pe_flag", creditor ~ "distressed_fund", TRUE ~ roster),
    base::stop("Unknown PE definition: ", definition)
  )
}

#' ENROLLMENT ID -> CCN, one row per enrollment
#'
#' CMS stores CCN as a number, so "60011" is Denver Health 060011
#' (normalize_ccn()); unit CCNs (06S011) collapse to their parent hospital.
enrollment_ccn_map <- function(enrollments) {
  require_columns(enrollments, base::c("enrollment_id", "ccn"), "Hospital Enrollments")

  enrollments |>
    dplyr::transmute(
      enrollment_id = .data$enrollment_id,
      enrollment_ccn = normalize_ccn(.data$ccn),
      ccn = unit_parent_ccn(.data$enrollment_ccn),
      proprietary_nonprofit = if ("proprietary_nonprofit" %in% base::names(enrollments)) .data$proprietary_nonprofit else NA_character_
    ) |>
    dplyr::filter(!base::is.na(.data$ccn)) |>
    dplyr::distinct(.data$enrollment_id, .keep_all = TRUE)
}

#' Organizational owner rows with their CCN and PE markers
owner_pe_rows <- function(owners, enrollments, fund_pattern = pe_fund_name_pattern()) {
  roles <- owner_role_codes()
  flags <- base::intersect(owners_flag_columns(), base::names(owners))

  owners |>
    dplyr::filter(.data$type_owner %in% "O") |>
    dplyr::select(dplyr::all_of(base::c(owners_required_columns(), flags))) |>
    dplyr::inner_join(enrollment_ccn_map(enrollments), by = "enrollment_id") |>
    dplyr::mutate(
      pct = base::suppressWarnings(base::as.numeric(.data$percentage_ownership)),
      ownership_role = .data$role_code_owner %in% roles$ownership,
      pe_flag = .data$private_equity_company_owner %in% "Y",
      fund_name = !base::is.na(.data$organization_name_owner) &
        stringr::str_detect(.data$organization_name_owner, fund_pattern)
    )
}

#' One row per roster CCN with its PE markers and ownership groups
#'
#' @param owners [load_hospital_owners()] output.
#' @param enrollments Normalized Hospital Enrollments ([load_hospital_enrollments()]$enrollments).
#' @param roster dim_hospital (ccn, facility_name, state, hospital_type,
#'   hospital_ownership, health_sys_id, health_sys_name).
#' @param pe_systems Optional [assign_pe_systems()] output (one row per CCN
#'   with pe_system_id, pe_system_class, pe_system_ambiguity, ...). Without
#'   it, the system-list definitions reduce to the CMS flag and roster groups.
#' @param extra_pe_ccns Optional CCNs the researcher knows to be PE owned
#'   (e.g. from a PE deal tracker); they count as PE only under the
#'   "researcher_list" definition, which otherwise equals "pe_strict".
#' @return tibble with pe_cms, pe_cms_direct, pe_cms_indirect, pe_cms_max_pct,
#'   pe_cms_5pct, pe_cms_any_role, pe_fund, pe_list, the PE system columns,
#'   the owners behind each, the roster group, and one
#'   ownership_group_<definition> column per pe_definitions() row.
classify_hospital_ownership <- function(owners, enrollments, roster, fund_pattern = pe_fund_name_pattern(),
                                        pe_systems = NULL, extra_pe_ccns = NULL) {
  require_columns(roster, base::c("ccn", "state", "hospital_type", "hospital_ownership"), "Hospital roster")
  roles <- owner_role_codes()
  rows <- owner_pe_rows(owners, enrollments, fund_pattern)

  collapse_names <- function(x) {
    x <- base::sort(base::unique(x[!base::is.na(x)]))
    if (base::length(x) == 0L) NA_character_ else base::paste(x, collapse = "; ")
  }

  per_ccn <- rows |>
    dplyr::group_by(.data$ccn) |>
    dplyr::summarise(
      pe_cms = base::any(.data$pe_flag & .data$ownership_role),
      pe_cms_direct = base::any(.data$pe_flag & .data$role_code_owner %in% roles$direct),
      pe_cms_indirect = base::any(.data$pe_flag & .data$role_code_owner %in% roles$indirect),
      pe_cms_max_pct = base::suppressWarnings(base::max(.data$pct[.data$pe_flag & .data$ownership_role], na.rm = TRUE)),
      pe_cms_any_role = base::any(.data$pe_flag),
      pe_fund = base::any((.data$pe_flag | .data$fund_name) & .data$ownership_role),
      pe_owners = collapse_names(.data$organization_name_owner[.data$pe_flag]),
      pe_roles = collapse_names(.data$role_code_owner[.data$pe_flag]),
      fund_owners = collapse_names(.data$organization_name_owner[.data$fund_name & .data$ownership_role & !.data$pe_flag]),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      pe_cms_max_pct = dplyr::if_else(base::is.finite(.data$pe_cms_max_pct), .data$pe_cms_max_pct, NA_real_),
      pe_cms_5pct = !base::is.na(.data$pe_cms_max_pct) & .data$pe_cms_max_pct >= 5
    )

  pecos <- enrollment_ccn_map(enrollments) |>
    dplyr::group_by(.data$ccn) |>
    dplyr::summarise(pecos_proprietary_nonprofit = collapse_names(.data$proprietary_nonprofit), .groups = "drop")

  logical_cols <- base::c("pe_cms", "pe_cms_direct", "pe_cms_indirect", "pe_cms_5pct", "pe_cms_any_role", "pe_fund")

  classified <- roster |>
    dplyr::select(dplyr::any_of(base::c("ccn", "facility_name", "state", "hospital_type", "hospital_ownership", "health_sys_id", "health_sys_name"))) |>
    dplyr::mutate(ccn = normalize_ccn(.data$ccn), roster_group = roster_ownership_group(.data$hospital_ownership)) |>
    dplyr::left_join(per_ccn, by = "ccn") |>
    dplyr::left_join(pecos, by = "ccn") |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(logical_cols), function(x) dplyr::coalesce(x, FALSE)),
      pe_list = .data$ccn %in% normalize_ccn(extra_pe_ccns)
    )

  if (base::is.null(pe_systems)) {
    pe_systems <- tibble::tibble(
      ccn = base::character(), pe_system_id = base::character(), pe_system_name = base::character(),
      pe_system_class = base::character(), pe_system_ambiguity = base::character(), pe_system_owner = base::character(),
      pe_system_max_pct = base::numeric(), pe_system_ids = base::character()
    )
  }
  classified <- dplyr::left_join(classified, pe_systems, by = "ccn")

  for (definition in pe_definitions()$definition) {
    classified[[base::paste0("ownership_group_", definition)]] <- ownership_group_for(classified, definition)
  }

  classified
}

# ---- PE hospital systems (config/pe_hospital_systems.csv) --------------------

pe_systems_required_columns <- function() {
  base::c(
    "system_id", "system_name", "owner", "owner_type", "classification", "ambiguity", "acquired",
    "status_2026_07", "owner_pattern", "chsp_name", "include_ccns", "exclude_ccns",
    "source_url", "source_publisher", "source_date", "source_quote"
  )
}

#' Read the sourced list of PE-owned hospital systems
#'
#' One row per system: owner, owner type, classification as of 2026-07
#' ("strict", "ambiguous", or "exclude"; `ambiguity` says why, and
#' "creditor" marks creditor or distressed-debt ownership), dates, and at
#' least one citable source with a verbatim quote. `owner_pattern` is a
#' regex on Hospital All Owners owner names; `include_ccns` and
#' `exclude_ccns` are ";"-separated.
load_pe_systems <- function(path = base::file.path("config", "pe_hospital_systems.csv")) {
  systems <- read_csv_chr(path)
  require_columns(systems, pe_systems_required_columns(), "PE hospital systems list")
  systems <- dplyr::mutate(systems, dplyr::across(dplyr::everything(), function(x) dplyr::coalesce(x, "")))

  bad_class <- base::setdiff(systems$classification, base::c("strict", "ambiguous", "exclude"))
  if (base::length(bad_class) > 0L) {
    base::stop("Unknown classification in PE systems list: ", base::paste(bad_class, collapse = ", "))
  }
  if (base::anyDuplicated(systems$system_id) > 0L) {
    base::stop("PE systems list repeats a system_id.")
  }
  if (base::any(systems$classification != "exclude" & !base::nzchar(systems$source_url))) {
    base::stop("Every strict or ambiguous PE system needs a source_url.")
  }

  systems
}

split_ccns <- function(x) {
  x <- stringr::str_split(x %||% "", "\\s*;\\s*")[[1]]
  normalize_ccn(x[base::nzchar(x)])
}

#' Map PE systems to CCNs
#'
#' A CCN belongs to a system when an ownership-role owner in Hospital All
#' Owners matches the system's `owner_pattern` (this follows each CCN's
#' current owners), or when it is listed in `include_ccns`; `exclude_ccns`
#' removes hospitals the owner has since sold. CHSP 2023 health-system
#' membership (`chsp_name`) is reported alongside as a cross-check, and
#' CHSP-only CCNs are returned with `used = FALSE`: CHSP predates sales and
#' lists hospitals a system only manages.
#'
#' @param owner_rows [owner_pe_rows()] output.
#' @param roster dim_hospital.
#' @return tibble(system_id, ccn, match_method, max_pct, in_chsp, used).
map_pe_systems <- function(systems, owner_rows, roster) {
  mapped <- systems |>
    dplyr::filter(.data$classification != "exclude") |>
    base::split(~system_id) |>
    purrr::map_dfr(function(system) {
      owner_hits <- if (base::nzchar(system$owner_pattern)) {
        owner_rows |>
          dplyr::filter(.data$ownership_role, stringr::str_detect(.data$organization_name_owner, system$owner_pattern)) |>
          dplyr::group_by(.data$ccn) |>
          dplyr::summarise(max_pct = base::suppressWarnings(base::max(.data$pct, na.rm = TRUE)), .groups = "drop") |>
          dplyr::mutate(match_method = "owner_name", max_pct = dplyr::if_else(base::is.finite(.data$max_pct), .data$max_pct, NA_real_))
      } else {
        tibble::tibble(ccn = base::character(), max_pct = base::numeric(), match_method = base::character())
      }
      manual <- tibble::tibble(ccn = base::setdiff(split_ccns(system$include_ccns), owner_hits$ccn), max_pct = NA_real_, match_method = "include_ccns")
      chsp_names <- stringr::str_split(system$chsp_name, "\\s*;\\s*")[[1]]
      chsp_ccns <- roster$ccn[roster$health_sys_name %in% chsp_names[base::nzchar(chsp_names)]]
      hits <- dplyr::bind_rows(owner_hits, manual) |>
        dplyr::filter(!.data$ccn %in% split_ccns(system$exclude_ccns)) |>
        dplyr::mutate(used = TRUE)
      chsp_only <- tibble::tibble(ccn = base::setdiff(chsp_ccns, hits$ccn), max_pct = NA_real_, match_method = "chsp_only", used = FALSE)

      dplyr::bind_rows(hits, chsp_only) |>
        dplyr::mutate(system_id = system$system_id, in_chsp = .data$ccn %in% chsp_ccns, .before = 1)
    })

  if (base::nrow(mapped) == 0L) {
    return(tibble::tibble(system_id = base::character(), ccn = base::character(), max_pct = base::numeric(),
                          match_method = base::character(), used = base::logical(), in_chsp = base::logical()))
  }
  mapped
}

#' One PE system per CCN
#'
#' When a CCN matches several systems, strict beats ambiguous and ambiguous
#' beats creditor ownership (e.g. a Lifepoint hospital with a minority
#' creditor-fund stake stays Lifepoint). All matches are kept in
#' `pe_system_ids`.
assign_pe_systems <- function(mapped, systems) {
  mapped |>
    dplyr::filter(.data$used) |>
    dplyr::inner_join(dplyr::select(systems, "system_id", "system_name", "classification", "ambiguity", "owner"), by = "system_id") |>
    dplyr::mutate(
      rank = dplyr::case_when(.data$classification == "strict" ~ 1L, .data$ambiguity != "creditor" ~ 2L, TRUE ~ 3L),
      order = base::match(.data$system_id, systems$system_id)
    ) |>
    dplyr::arrange(.data$ccn, .data$rank, .data$order) |>
    dplyr::group_by(.data$ccn) |>
    dplyr::summarise(
      pe_system_id = dplyr::first(.data$system_id),
      pe_system_name = dplyr::first(.data$system_name),
      pe_system_class = dplyr::first(.data$classification),
      pe_system_ambiguity = dplyr::first(.data$ambiguity),
      pe_system_owner = dplyr::first(.data$owner),
      pe_system_max_pct = dplyr::first(.data$max_pct),
      pe_system_ids = base::paste(.data$system_id, collapse = ";"),
      .groups = "drop"
    )
}

#' Hospitals per PE system, by match method, roster type, and prices
#'
#' @param priced_ccns CCNs with at least one price in the model sample.
pe_system_counts <- function(systems, mapped, assigned, roster, priced_ccns = NULL) {
  roster <- dplyr::mutate(roster, ccn = normalize_ccn(.data$ccn))
  general <- roster$ccn[roster$hospital_type %in% base::c("Acute Care Hospitals", "Critical Access Hospitals")]

  counts <- mapped |>
    dplyr::group_by(.data$system_id) |>
    dplyr::summarise(
      n_ccns = base::sum(.data$used),
      n_owner_name = base::sum(.data$used & .data$match_method == "owner_name"),
      n_include_ccns = base::sum(.data$match_method == "include_ccns"),
      n_roster = base::sum(.data$used & .data$ccn %in% roster$ccn),
      n_acute_or_cah = base::sum(.data$used & .data$ccn %in% general),
      n_assigned = base::sum(.data$used & .data$ccn %in% assigned$ccn[assigned$pe_system_id == dplyr::first(.data$system_id)]),
      n_priced = base::sum(.data$used & .data$ccn %in% base::intersect(general, priced_ccns)),
      n_in_chsp = base::sum(.data$used & .data$in_chsp),
      n_chsp_only = base::sum(.data$match_method == "chsp_only"),
      n_below_50pct = base::sum(.data$used & !base::is.na(.data$max_pct) & .data$max_pct < 50),
      .groups = "drop"
    )

  systems |>
    dplyr::select("system_id", "system_name", "owner", "classification", "ambiguity", "acquired", "status_2026_07") |>
    dplyr::left_join(counts, by = "system_id") |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("n_"), function(x) dplyr::coalesce(x, 0L)))
}

#' Owners behind the PE markers, largest first
#'
#' @param roster Optional dim_hospital, to count roster hospitals.
#' @param priced_ccns Optional CCNs with at least one price in the analysis.
#' @return One row per owner name x basis ("cms_flag" or "fund_name").
largest_pe_owners <- function(owners, enrollments, roster = NULL, priced_ccns = NULL, fund_pattern = pe_fund_name_pattern()) {
  rows <- owner_pe_rows(owners, enrollments, fund_pattern) |>
    dplyr::filter(.data$pe_flag | (.data$fund_name & .data$ownership_role)) |>
    dplyr::mutate(basis = dplyr::if_else(.data$pe_flag, "cms_flag", "fund_name"))
  roster_ccns <- if (base::is.null(roster)) base::character() else normalize_ccn(roster$ccn)
  roster_names <- if (base::is.null(roster)) NULL else stats::setNames(roster$facility_name, normalize_ccn(roster$ccn))

  rows |>
    dplyr::group_by(.data$organization_name_owner, .data$basis) |>
    dplyr::summarise(
      n_enrollments = dplyr::n_distinct(.data$enrollment_id),
      n_ccns = dplyr::n_distinct(.data$ccn),
      n_roster_ccns = dplyr::n_distinct(.data$ccn[.data$ccn %in% roster_ccns]),
      n_priced_ccns = dplyr::n_distinct(.data$ccn[.data$ccn %in% priced_ccns]),
      roles = base::paste(base::sort(base::unique(.data$role_code_owner)), collapse = ";"),
      pct_min = base::suppressWarnings(base::min(.data$pct, na.rm = TRUE)),
      pct_max = base::suppressWarnings(base::max(.data$pct, na.rm = TRUE)),
      example_hospitals = base::paste(utils::head(base::unique(stats::na.omit(
        if (base::is.null(roster_names)) .data$ccn else base::unname(roster_names[.data$ccn])
      )), 3L), collapse = " | "),
      .groups = "drop"
    ) |>
    dplyr::mutate(dplyr::across(base::c("pct_min", "pct_max"), function(x) dplyr::if_else(base::is.finite(x), x, NA_real_))) |>
    dplyr::arrange(.data$basis, dplyr::desc(.data$n_roster_ccns), dplyr::desc(.data$n_ccns))
}

#' Hospital counts by ownership group, by hospital type and by state
#'
#' @return tibble(definition, dimension, value, ownership_group, n_hospitals).
ownership_counts <- function(classification, definitions = pe_definitions()$definition) {
  purrr::map_dfr(definitions, function(definition) {
    group_col <- base::paste0("ownership_group_", definition)
    by_dim <- function(dimension) {
      classification |>
        dplyr::count(value = .data[[dimension]], ownership_group = .data[[group_col]], name = "n_hospitals") |>
        dplyr::mutate(definition = definition, dimension = dimension, .before = 1)
    }
    dplyr::bind_rows(by_dim("hospital_type"), by_dim("state"))
  })
}

# ---- hospital prices ---------------------------------------------------------

#' Codes and payer types the ownership analysis compares
ownership_codes <- function() {
  base::c(
    "45378" = "Colonoscopy (45378)", "58100" = "Endometrial biopsy (58100)", "58300" = "IUD insertion (58300)",
    "58120" = "D&C (58120)", "58558" = "Hysteroscopy with biopsy (58558)",
    "43775" = "Sleeve gastrectomy (43775)", "621" = "Bariatric MS-DRG 621"
  )
}

ownership_payer_types <- function() {
  base::c("commercial", "medicare_advantage", "medicaid", "exchange", "self_pay_cash")
}

#' SQL for one facility price per CCN x code x payer type
#'
#' The same rules as state_medians_sql(): plausible negotiated dollars;
#' facility fees only (the stored fee_type); rate_row_filter_sql() drops
#' rows that price a different product; a payer/plan contract's median first,
#' then the median across contracts; "self_pay_cash" is the median
#' discounted cash price over distinct charge lines. Only CCN-matched rates
#' count, since ownership needs a CCN.
ownership_price_sql <- function(codes = base::names(ownership_codes()),
                                payer_types = ownership_payer_types(),
                                exclude_file_ids = NULL) {
  negotiated_types <- base::setdiff(payer_types, "self_pay_cash")
  exclude_sql <- if (base::length(exclude_file_ids) > 0L) {
    base::paste0(" AND mrf_file_id NOT IN (", sql_string_list(exclude_file_ids), ")")
  } else {
    ""
  }

  negotiated_sql <- if (base::length(negotiated_types) > 0L) {
    base::paste0(
      "  SELECT ccn, code, payer_type, median(plan_median) AS price, count(*) AS n_contracts, sum(n_rows) AS n_rows\n",
      "  FROM (SELECT ccn, code, payer_type, payer_name, plan_name, median(negotiated_dollar) AS plan_median, count(*) AS n_rows\n",
      "        FROM base WHERE plausible AND payer_type IN (", sql_string_list(negotiated_types), ") GROUP BY ALL)\n",
      "  GROUP BY ALL\n"
    )
  }
  cash_sql <- if ("self_pay_cash" %in% payer_types) {
    base::paste0(
      "  SELECT ccn, code, 'self_pay_cash' AS payer_type, median(discounted_cash) AS price, count(*) AS n_contracts, count(*) AS n_rows\n",
      "  FROM (SELECT DISTINCT ccn, code, description, discounted_cash FROM base\n",
      "        WHERE discounted_cash > 1 AND discounted_cash < 2000000)\n",
      "  GROUP BY ALL\n"
    )
  }

  base::paste0(
    "WITH base AS (\n",
    "  SELECT ccn, code, CAST(payer_type AS VARCHAR) AS payer_type, payer_name, plan_name,\n",
    "         negotiated_dollar, plausible, discounted_cash, description\n",
    "  FROM v_hospital_rate\n",
    "  WHERE ccn IS NOT NULL AND state IS NOT NULL AND code IN (", sql_string_list(codes), ")\n",
    "    AND fee_type = 'facility'\n",
    "    AND ", rate_row_filter_sql(), exclude_sql, "\n",
    ")\n",
    base::paste(base::c(negotiated_sql, cash_sql), collapse = "  UNION ALL\n"),
    "ORDER BY code, payer_type, ccn"
  )
}

#' Facility price per CCN x code x payer type from hpt.duckdb (read-only)
#'
#' @param exclude_file_ids mrf_file_id values to leave out
#'   (output/median_excluded_file_ids.csv).
#' @return tibble(ccn, code, payer_type, price, n_contracts, n_rows).
ownership_hospital_prices <- function(db_path = hpt_database_path(),
                                      codes = base::names(ownership_codes()),
                                      payer_types = ownership_payer_types(),
                                      exclude_file_ids = NULL) {
  prices <- duckdb_query(
    ownership_price_sql(codes, payer_types, exclude_file_ids),
    database = db_path, read_only = TRUE, init = "SET memory_limit = '4GB';"
  )
  base::attr(prices, "excluded_file_ids") <- exclude_file_ids
  prices
}

# ---- model -------------------------------------------------------------------

#' Bed-size band from the AHRQ CHSP linkage (hos_beds)
bed_size_band <- function(beds) {
  beds <- base::suppressWarnings(base::as.numeric(beds))

  dplyr::case_when(
    base::is.na(beds) ~ "unknown",
    beds < 100 ~ "<100",
    beds < 300 ~ "100-299",
    TRUE ~ "300+"
  )
}

#' Analysis frame: one row per CCN x code x payer type with covariates
#'
#' @param prices [ownership_hospital_prices()] output.
#' @param classification [classify_hospital_ownership()] output.
#' @param chsp Optional [load_chsp_linkage()] output, for bed size.
#' @param hospital_types Roster hospital types kept (general hospitals).
ownership_model_frame <- function(prices, classification, chsp = NULL,
                                  hospital_types = base::c("Acute Care Hospitals", "Critical Access Hospitals")) {
  beds <- if (!base::is.null(chsp) && "hos_beds" %in% base::names(chsp)) {
    dplyr::select(chsp, "ccn", "hos_beds")
  } else {
    tibble::tibble(ccn = base::character(), hos_beds = base::character())
  }

  frame <- prices |>
    dplyr::filter(!base::is.na(.data$price), .data$price > 0) |>
    dplyr::inner_join(classification, by = "ccn") |>
    dplyr::filter(.data$hospital_type %in% hospital_types) |>
    dplyr::left_join(beds, by = "ccn")
  if (!"pe_system_id" %in% base::names(frame)) {
    frame$pe_system_id <- NA_character_
  }

  dplyr::mutate(
    frame,
    log_price = base::log(.data$price),
    in_system = !base::is.na(.data$health_sys_id) | !base::is.na(.data$pe_system_id),
    bed_size = bed_size_band(.data$hos_beds),
    # a listed PE system (current owner) first, then the CHSP 2023 system;
    # hospitals in neither are their own cluster
    cluster_id = dplyr::coalesce(
      dplyr::if_else(base::is.na(.data$pe_system_id), NA_character_, base::paste0("pe:", .data$pe_system_id)),
      .data$health_sys_id, base::paste0("ccn:", .data$ccn)
    )
  )
}

#' Which estimator is available: fixest, else lm + sandwich/lmtest, else lm
ownership_model_engine <- function() {
  if (base::requireNamespace("fixest", quietly = TRUE)) {
    return("fixest")
  }
  if (base::requireNamespace("sandwich", quietly = TRUE) && base::requireNamespace("lmtest", quietly = TRUE)) {
    return("sandwich")
  }
  "lm"
}

#' Fit log(price) ~ ownership group + state FE + covariates for one cell
#'
#' Covariates with a single level in the cell are dropped. Cluster-robust
#' SEs (CR1 / HC1) by `cluster_id`; t intervals with G - 1 degrees of
#' freedom. Groups with fewer than `min_group` hospitals are not estimated.
#' When every hospital of a group sits in one cluster, its clustered SE is
#' degenerate (the group's residuals sum to zero inside that cluster), so
#' the point estimate is kept and the CI and p value are left NA.
#'
#' The groups in one model are mutually exclusive (a hospital is PE,
#' CMS-flagged, distressed-fund, for-profit, government, or nonprofit), and
#' each PE definition is its own model (ownership_models()), so the nested
#' strict and broad PE indicators never share a regression.
#'
#' For the groups in `wcr_terms` with at least two clusters, a wild cluster
#' restricted bootstrap (wild_cluster_bootstrap()) adds wcr_p_value and a
#' test-inversion CI (wcr_ci_low, wcr_ci_high); ci_low/ci_high stay the
#' CRV1 t intervals for comparison.
#'
#' @param cell Rows of [ownership_model_frame()] for one code x payer type.
#' @param group_col Ownership-group column to use.
#' @param engine "fixest", "sandwich", or "lm" (model-based SEs, no clustering).
#' @param wcr_terms Groups to bootstrap (e.g. "pe").
#' @param B Bootstrap draws.
#' @return tibble(term, estimate, std_error, ci_low, ci_high, p_value,
#'   wcr_p_value, wcr_ci_low, wcr_ci_high, pct_diff, pct_ci_low,
#'   pct_ci_high, pct_wcr_ci_low, pct_wcr_ci_high, n_group,
#'   n_group_clusters, n_hospitals, n_clusters, se_type, covariates, note),
#'   one row per non-reference group.
fit_ownership_model <- function(cell, group_col = "ownership_group_cms_flag",
                                engine = ownership_model_engine(), min_group = 3L,
                                wcr_terms = base::character(), B = 9999L) {
  levels <- ownership_group_levels()
  cell <- cell |>
    dplyr::mutate(group = .data[[group_col]]) |>
    dplyr::filter(.data$group %in% levels)
  n_by_group <- base::table(base::factor(cell$group, levels = levels))
  fit_levels <- levels[levels == "nonprofit" | n_by_group >= min_group]
  fit_cell <- dplyr::filter(cell, .data$group %in% fit_levels)
  fit_cell$group <- base::factor(fit_cell$group, levels = fit_levels)

  covariates <- base::c("hospital_type", "in_system", "bed_size")
  covariates <- covariates[base::vapply(covariates, function(v) dplyr::n_distinct(fit_cell[[v]]) > 1L, base::logical(1))]
  n_clusters <- dplyr::n_distinct(fit_cell$cluster_id)
  clusters_by_group <- base::vapply(levels[-1], function(level) dplyr::n_distinct(cell$cluster_id[cell$group == level]), base::integer(1))
  # which clusters make up each group, largest first ("pe:lifepoint 35; ...")
  cluster_mix <- base::vapply(levels[-1], function(level) {
    counts <- base::sort(base::table(cell$cluster_id[cell$group == level]), decreasing = TRUE)
    top <- utils::head(counts, 3L)
    base::paste(base::c(base::paste(base::names(top), top), if (base::length(counts) > 3L) base::paste0("+", base::length(counts) - 3L, " more")), collapse = "; ")
  }, base::character(1))

  empty <- tibble::tibble(
    term = levels[-1], estimate = NA_real_, std_error = NA_real_, ci_low = NA_real_, ci_high = NA_real_,
    p_value = NA_real_, wcr_p_value = NA_real_, wcr_ci_low = NA_real_, wcr_ci_high = NA_real_, n_group = base::as.integer(n_by_group[levels[-1]]), n_group_clusters = base::unname(clusters_by_group),
    group_clusters = base::unname(cluster_mix),
    n_hospitals = base::nrow(cell),
    n_clusters = n_clusters, se_type = NA_character_, covariates = base::paste(covariates, collapse = "+"),
    note = dplyr::if_else(base::as.integer(n_by_group[levels[-1]]) < min_group, base::paste0("fewer than ", min_group, " hospitals; not estimated"), NA_character_)
  )

  if (n_by_group[["nonprofit"]] < min_group || base::length(fit_levels) < 2L || n_clusters < 3L) {
    empty$note <- dplyr::coalesce(empty$note, "too few nonprofit hospitals or clusters; not estimated")
    return(add_pct_columns(empty))
  }

  rhs <- base::paste(base::c("group", covariates), collapse = " + ")

  if (engine == "fixest") {
    fit <- fixest::feols(stats::as.formula(base::paste("log_price ~", rhs, "| state")), data = fit_cell, cluster = ~cluster_id)
    b <- stats::coef(fit)
    v <- stats::vcov(fit)
    dof <- n_clusters - 1L
    se_type <- "cluster-robust by health system (fixest)"
  } else {
    fit <- stats::lm(stats::as.formula(base::paste("log_price ~", rhs, "+ state")), data = fit_cell)
    b <- stats::coef(fit)
    b <- b[!base::is.na(b)]

    if (engine == "sandwich") {
      v <- sandwich::vcovCL(fit, cluster = fit_cell$cluster_id, type = "HC1")
      dof <- n_clusters - 1L
      se_type <- "cluster-robust by health system (sandwich HC1)"
    } else {
      v <- stats::vcov(fit)
      dof <- fit$df.residual
      se_type <- "model-based (sandwich not installed; no clustering)"
    }
  }

  terms <- base::paste0("group", fit_levels[-1])
  terms <- terms[terms %in% base::names(b)]
  estimate <- base::unname(b[terms])
  std_error <- base::sqrt(base::diag(v)[terms])
  t_crit <- stats::qt(0.975, dof)

  estimated <- tibble::tibble(
    term = base::sub("^group", "", terms),
    estimate = estimate,
    std_error = base::unname(std_error),
    ci_low = estimate - t_crit * std_error,
    ci_high = estimate + t_crit * std_error,
    wcr_p_value = NA_real_, wcr_ci_low = NA_real_, wcr_ci_high = NA_real_,
    p_value = 2 * stats::pt(-base::abs(estimate / std_error), dof),
    se_type = se_type
  )

  # wild cluster restricted bootstrap for the requested groups (lm matrix;
  # the fixest path refits it)
  boot_terms <- base::intersect(wcr_terms, estimated$term)
  boot_terms <- boot_terms[clusters_by_group[boot_terms] >= 2L]
  if (base::length(boot_terms) > 0L) {
    lm_fit <- if (engine == "fixest") stats::lm(stats::as.formula(base::paste("log_price ~", rhs, "+ state")), data = fit_cell) else fit
    X <- stats::model.matrix(lm_fit)[, !base::is.na(stats::coef(lm_fit)), drop = FALSE]
    if (base::nrow(X) != base::nrow(fit_cell)) {
      base::stop("Model matrix and cell rows differ; the bootstrap needs complete rows.")
    }
    for (term in boot_terms) {
      boot <- wild_cluster_bootstrap(fit_cell$log_price, X, base::paste0("group", term), fit_cell$cluster_id, B = B)
      estimated$wcr_p_value[estimated$term == term] <- boot$p_value
      estimated$wcr_ci_low[estimated$term == term] <- boot$ci_low
      estimated$wcr_ci_high[estimated$term == term] <- boot$ci_high
    }
  }

  empty |>
    dplyr::select(-"estimate", -"std_error", -"ci_low", -"ci_high", -"p_value", -"wcr_p_value", -"wcr_ci_low", -"wcr_ci_high", -"se_type") |>
    dplyr::left_join(estimated, by = "term") |>
    dplyr::mutate(
      n_hospitals = base::nrow(fit_cell),
      single_cluster = engine != "lm" & !base::is.na(.data$estimate) & .data$n_group_clusters < 2L,
      dplyr::across(base::c("ci_low", "ci_high", "p_value", "wcr_p_value", "wcr_ci_low", "wcr_ci_high"), function(x) dplyr::if_else(.data$single_cluster, NA_real_, x)),
      note = dplyr::if_else(.data$single_cluster, "all hospitals in this group share one health system: no valid clustered CI", .data$note)
    ) |>
    dplyr::select(-"single_cluster") |>
    dplyr::relocate("estimate", "std_error", "ci_low", "ci_high", "p_value", "wcr_p_value", "wcr_ci_low", "wcr_ci_high", .after = "term") |>
    add_pct_columns()
}

#' Adjusted % difference: exp(beta) - 1, with its CI
#'
#' The model is in log(price), so the percentage difference is
#' 100 x (exp(beta) - 1), and each CI limit is exponentiated on its own
#' (never 100 x beta, which is badly wrong at +100% or more). Intervals are
#' therefore asymmetric on the % scale and symmetric on the log axis the
#' forest plot uses.
add_pct_columns <- function(tbl) {
  tbl <- dplyr::mutate(
    tbl,
    pct_diff = base::exp(.data$estimate) - 1,
    pct_ci_low = base::exp(.data$ci_low) - 1,
    pct_ci_high = base::exp(.data$ci_high) - 1
  )
  if ("wcr_ci_low" %in% base::names(tbl)) {
    tbl <- dplyr::mutate(tbl, pct_wcr_ci_low = base::exp(.data$wcr_ci_low) - 1, pct_wcr_ci_high = base::exp(.data$wcr_ci_high) - 1)
  }
  tbl
}

# ---- small-cluster inference -------------------------------------------------

#' Treated clusters an ownership group needs before its interval is shown
#'
#' Wild cluster bootstrap tests are unreliable when very few clusters are
#' treated: the restricted version under-rejects and the unrestricted one
#' over-rejects (MacKinnon and Webb 2017, J Appl Econometrics 32:233-254;
#' 2018, Econometrics Journal 21:114-135). Strict PE here is essentially
#' Lifepoint and ScionHealth, so a group drawn from fewer than 5 health
#' systems is reported as an exploratory point estimate without an
#' interval, whatever its number of hospitals.
min_treated_clusters <- function() {
  5L
}

#' Codes traditional Medicare does not pay for, so Medicare and Medicare
#' Advantage rates for them are hospital-listed numbers, not payments
#' (58300: OPPS status E1, PFS status N; MA follows Medicare coverage)
medicare_noncovered_codes <- function() {
  "58300"
}

#' Webb six-point weights, the recommended wild bootstrap weights when there
#' are few clusters (Webb 2023, Can J Econ 56:1011-1037)
webb_weights <- function(n) {
  base::sample(base::c(-base::sqrt(1.5), -1, -base::sqrt(0.5), base::sqrt(0.5), 1, base::sqrt(1.5)), n, replace = TRUE)
}

#' Wild cluster restricted (WCR) bootstrap for one OLS coefficient
#'
#' Tests beta = beta0 with the null imposed (restricted residuals), Webb
#' weights drawn per cluster, and CRV1 t statistics (small-sample factor
#' G/(G-1) x (N-1)/(N-K), as sandwich::vcovCL(type = "HC1")). The CI is the
#' set of beta0 the test does not reject, found by test inversion.
#'
#' Fast algorithm (Roodman, Nielsen, MacKinnon and Webb 2019, Stata J
#' 19:4-60): with the other regressors Z partialled out (x~ = M_Z x,
#' y~ = M_Z y), the bootstrap coefficient and every cluster's CRV1 score are
#' linear in the weights and in beta0. Per cluster g, with a_g = x~_g'y~_g,
#' d_g = x~_g'x~_g, Q_g = Z_g'x~_g and S_g = Z_g'y~_g, the score of draw v is
#' P0 v - beta0 P1 v, where P0 and P1 are built once. The sums of squared
#' scores then reduce to three numbers per draw, so each beta0 costs O(B)
#' and the inversion grid is cheap.
#'
#' @param y Response vector.
#' @param X Full-rank model matrix (with intercept and fixed-effect dummies).
#' @param term Column of X to test.
#' @param cluster Cluster id per row.
#' @param B Bootstrap draws.
#' @param level Confidence level.
#' @param ci Also invert the test for a CI.
#' @param seed RNG seed (set here, so each coefficient's draws are reproducible).
#' @param null beta0 for the reported p value.
#' @return list(estimate, se_crv1, p_value, ci_low, ci_high, B, n_clusters).
#'   An interval side that never rejects within 60 CRV1 standard errors is
#'   returned as -Inf / Inf (unbounded).
wild_cluster_bootstrap <- function(y, X, term, cluster, B = 9999L, level = 0.95, ci = TRUE,
                                   seed = 20260913L, chunk = 1000L, null = 0) {
  j <- base::match(term, base::colnames(X))
  if (base::is.na(j)) {
    base::stop("Term not in the model matrix: ", term)
  }
  x <- X[, j]
  Z <- X[, -j, drop = FALSE]
  qz <- base::qr(Z)
  x_t <- base::qr.resid(qz, x)
  y_t <- base::qr.resid(qz, y)
  D <- base::sum(x_t^2)
  estimate <- base::sum(x_t * y_t) / D

  g <- base::factor(cluster)
  G <- base::nlevels(g)
  N <- base::length(y)
  c_adj <- (G / (G - 1)) * ((N - 1) / (N - base::ncol(X)))
  score <- base::rowsum(x_t * (y_t - estimate * x_t), g)[, 1]
  se_crv1 <- base::sqrt(c_adj * base::sum(score^2)) / D

  a_y <- base::rowsum(x_t * y_t, g)[, 1]
  d <- base::rowsum(x_t^2, g)[, 1]
  Q <- base::rowsum(Z * x_t, g)
  S <- base::rowsum(Z * y_t, g)
  QW <- Q %*% base::chol2inv(base::chol(base::crossprod(Z)))

  base::set.seed(seed)
  av_y <- av_d <- s00 <- s01 <- s11 <- base::numeric(0)
  for (start in base::seq(1L, B, by = chunk)) {
    b <- base::min(chunk, B - start + 1L)
    V <- base::matrix(webb_weights(G * b), G, b)
    avy <- base::colSums(a_y * V)
    avd <- base::colSums(d * V)
    P0 <- a_y * V - QW %*% (base::crossprod(S, V)) - base::outer(d, avy) / D
    P1 <- d * V - QW %*% (base::crossprod(Q, V)) - base::outer(d, avd) / D
    av_y <- base::c(av_y, avy)
    av_d <- base::c(av_d, avd)
    s00 <- base::c(s00, base::colSums(P0^2))
    s01 <- base::c(s01, base::colSums(P0 * P1))
    s11 <- base::c(s11, base::colSums(P1^2))
  }

  p_at <- function(beta0) {
    t_obs <- (estimate - beta0) / se_crv1
    t_star <- (av_y - beta0 * av_d) / base::sqrt(c_adj * base::pmax(s00 - 2 * beta0 * s01 + beta0^2 * s11, 0))
    base::mean(base::abs(t_star) >= base::abs(t_obs), na.rm = TRUE)
  }

  alpha <- 1 - level
  bound <- function(direction) {
    # walk out from the estimate until the test rejects, then bisect
    inside <- estimate
    for (k in base::c(base::seq(0.25, 12, by = 0.25), base::seq(13, 60, by = 1))) {
      candidate <- estimate + direction * k * se_crv1
      if (p_at(candidate) <= alpha) {
        outside <- candidate
        for (i in base::seq_len(40L)) {
          mid <- (inside + outside) / 2
          if (p_at(mid) > alpha) inside <- mid else outside <- mid
        }
        return((inside + outside) / 2)
      }
      inside <- candidate
    }
    direction * Inf
  }

  base::list(
    estimate = estimate, se_crv1 = se_crv1, p_value = p_at(null),
    ci_low = if (ci) bound(-1) else NA_real_, ci_high = if (ci) bound(1) else NA_real_,
    B = B, n_clusters = G
  )
}

#' The (definition, group) series the forest plot draws and the bootstrap
#' covers: PE strict, PE broad, and non-PE for-profit (from the strict model)
ownership_forest_series <- function() {
  tibble::tribble(
    ~definition,  ~term,               ~label,
    "pe_strict",  "pe",                "PE, strict",
    "pe_broad",   "pe",                "PE, broad",
    "pe_strict",  "for_profit_non_pe", "For-profit, not PE"
  )
}

#' Fit every code x payer type x PE definition
#'
#' Each definition is a separate model. The groups in `wcr_series` get a
#' wild cluster restricted bootstrap (B draws); the others keep CRV1
#' intervals only.
#'
#' @return One row per cell x non-reference group, with `low_pe_n` TRUE when
#'   the cell has fewer than `min_pe_flag` PE hospitals, `few_pe_clusters`
#'   TRUE when they come from fewer than `min_pe_clusters` clusters,
#'   `exploratory` TRUE for any group drawn from fewer than
#'   min_treated_clusters() clusters (point estimate only), and
#'   `payment_comparison` FALSE for Medicare and Medicare Advantage rates of
#'   codes Medicare does not cover (medicare_noncovered_codes()).
ownership_models <- function(frame, definitions = pe_definitions()$definition,
                             engine = ownership_model_engine(), min_pe_flag = 10L,
                             min_pe_clusters = min_treated_clusters(),
                             wcr_series = ownership_forest_series(), B = 9999L) {
  cells <- dplyr::distinct(frame, .data$code, .data$payer_type)

  purrr::pmap_dfr(cells, function(code, payer_type) {
    cell <- dplyr::filter(frame, .data$code == !!code, .data$payer_type == !!payer_type)

    purrr::map_dfr(definitions, function(definition) {
      wcr_terms <- wcr_series$term[wcr_series$definition == definition]
      fit_ownership_model(cell, base::paste0("ownership_group_", definition), engine = engine, wcr_terms = wcr_terms, B = B) |>
        dplyr::mutate(code = code, payer_type = payer_type, definition = definition, .before = 1)
    })
  }) |>
    dplyr::group_by(.data$code, .data$payer_type, .data$definition) |>
    dplyr::mutate(
      n_pe = .data$n_group[.data$term == "pe"],
      n_pe_clusters = .data$n_group_clusters[.data$term == "pe"],
      low_pe_n = .data$n_pe < min_pe_flag,
      few_pe_clusters = .data$n_pe_clusters < min_pe_clusters
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      exploratory = !base::is.na(.data$estimate) & .data$n_group_clusters < min_treated_clusters(),
      payment_comparison = !(.data$code %in% medicare_noncovered_codes() & .data$payer_type %in% base::c("medicare", "medicare_advantage")),
      note = dplyr::case_when(
        !.data$payment_comparison ~ dplyr::if_else(base::is.na(.data$note), "", base::paste0(.data$note, "; ")) |>
          base::paste0("not a payment comparison: Medicare does not cover this code, so Medicare Advantage rates are hospital-listed numbers"),
        .data$exploratory ~ dplyr::if_else(base::is.na(.data$note), "", base::paste0(.data$note, "; ")) |>
          base::paste0("exploratory: fewer than ", min_treated_clusters(), " health-system clusters in the group"),
        TRUE ~ .data$note
      )
    ) |>
    dplyr::arrange(.data$definition, .data$code, .data$payer_type, base::match(.data$term, ownership_group_levels()))
}

#' Raw medians and hospital counts by ownership group
ownership_raw_summary <- function(frame, definitions = pe_definitions()$definition) {
  purrr::map_dfr(definitions, function(definition) {
    frame |>
      dplyr::mutate(ownership_group = .data[[base::paste0("ownership_group_", definition)]]) |>
      dplyr::filter(.data$ownership_group %in% ownership_group_levels()) |>
      dplyr::group_by(.data$code, .data$payer_type, .data$ownership_group) |>
      dplyr::summarise(
        n_hospitals = dplyr::n(),
        n_states = dplyr::n_distinct(.data$state),
        median_price = stats::median(.data$price),
        p25 = stats::quantile(.data$price, 0.25, names = FALSE),
        p75 = stats::quantile(.data$price, 0.75, names = FALSE),
        .groups = "drop"
      ) |>
      dplyr::mutate(definition = definition, .before = 1)
  }) |>
    dplyr::arrange(.data$definition, .data$code, .data$payer_type, base::match(.data$ownership_group, ownership_group_levels()))
}

#' Within-state matched comparison against nonprofit hospitals
#'
#' In each state with at least one hospital of `group` and one nonprofit
#' hospital, take (median group price - median nonprofit price) and the
#' ratio - 1; report the median of those across states.
ownership_within_state <- function(frame, definitions = pe_definitions()$definition,
                                   groups = ownership_group_levels()[-1]) {
  purrr::map_dfr(definitions, function(definition) {
    by_state <- frame |>
      dplyr::mutate(ownership_group = .data[[base::paste0("ownership_group_", definition)]]) |>
      dplyr::group_by(.data$code, .data$payer_type, .data$state, .data$ownership_group) |>
      dplyr::summarise(median_price = stats::median(.data$price), n = dplyr::n(), .groups = "drop")
    nonprofit <- by_state |>
      dplyr::filter(.data$ownership_group == "nonprofit") |>
      dplyr::select("code", "payer_type", "state", np_price = "median_price", np_n = "n")

    by_state |>
      dplyr::filter(.data$ownership_group %in% groups) |>
      dplyr::inner_join(nonprofit, by = base::c("code", "payer_type", "state")) |>
      dplyr::group_by(.data$code, .data$payer_type, term = .data$ownership_group) |>
      dplyr::summarise(
        n_states = dplyr::n(),
        n_group_hospitals = base::sum(.data$n),
        n_nonprofit_hospitals = base::sum(.data$np_n),
        median_diff_dollars = stats::median(.data$median_price - .data$np_price),
        median_pct_diff = stats::median(.data$median_price / .data$np_price - 1),
        share_states_higher = base::mean(.data$median_price > .data$np_price),
        .groups = "drop"
      ) |>
      dplyr::mutate(definition = definition, .before = 1)
  })
}

#' Each listed PE system's prices relative to nonprofit hospitals in the same state
#'
#' For every hospital in a listed system (pe_system_id), price / median
#' nonprofit price in its state (same code and payer type); then the median
#' and IQR of that ratio - 1 across the system's hospitals. A descriptive
#' check that needs no clustered inference and shows which systems drive
#' the pooled PE estimate.
ownership_system_ratios <- function(frame, reference_group_col = "ownership_group_pe_strict") {
  nonprofit <- frame |>
    dplyr::filter(.data[[reference_group_col]] == "nonprofit") |>
    dplyr::group_by(.data$code, .data$payer_type, .data$state) |>
    dplyr::summarise(np_price = stats::median(.data$price), np_n = dplyr::n(), .groups = "drop")

  frame |>
    dplyr::filter(!base::is.na(.data$pe_system_id)) |>
    dplyr::inner_join(nonprofit, by = base::c("code", "payer_type", "state")) |>
    dplyr::mutate(ratio = .data$price / .data$np_price) |>
    dplyr::group_by(.data$pe_system_id, .data$pe_system_class, .data$pe_system_ambiguity, .data$code, .data$payer_type) |>
    dplyr::summarise(
      n_hospitals = dplyr::n(),
      n_states = dplyr::n_distinct(.data$state),
      median_pct_diff = stats::median(.data$ratio) - 1,
      p25_pct_diff = stats::quantile(.data$ratio, 0.25, names = FALSE) - 1,
      p75_pct_diff = stats::quantile(.data$ratio, 0.75, names = FALSE) - 1,
      share_above_nonprofit = base::mean(.data$ratio > 1),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$code, .data$payer_type, dplyr::desc(.data$n_hospitals))
}

#' Forest plot of adjusted % differences vs nonprofit hospitals
#'
#' Rows are codes, facets are payer types. One series per (definition,
#' group) pair in `series`, dodged side by side; by default PE strict, PE
#' broad, and non-PE for-profit. Intervals are the 95% wild cluster
#' restricted bootstrap CIs; exploratory groups (fewer than
#' min_treated_clusters() health systems) are hollow points without an
#' interval. The column at the right edge of each panel gives each group's
#' count of health-system clusters. Rows that are not payment comparisons are left out.
plot_ownership_forest <- function(results, series = ownership_forest_series()) {
  plot_tbl <- results |>
    dplyr::inner_join(series, by = base::c("definition", "term")) |>
    dplyr::filter(!base::is.na(.data$estimate), .data$payment_comparison) |>
    dplyr::mutate(
      series = base::factor(.data$label, levels = series$label),
      procedure = base::factor(base::unname(ownership_codes()[.data$code]), levels = base::rev(base::unname(ownership_codes()))),
      payer = base::factor(.data$payer_type, levels = ownership_payer_types(),
                           labels = base::c("Commercial", "Medicare Advantage", "Medicaid", "Exchange", "Cash price")),
      # an unbounded bootstrap side is drawn to the panel edge
      lo = dplyr::if_else(.data$exploratory, NA_real_, base::pmax(1 + .data$pct_wcr_ci_low, 0.15)),
      hi = dplyr::if_else(.data$exploratory, NA_real_, base::pmin(1 + .data$pct_wcr_ci_high, 8)),
      cluster_label = base::as.character(.data$n_group_clusters),
      # one count column per series at the right edge (log axis)
      label_x = base::c(9, 13, 18.5)[base::as.integer(.data$series)]
    )

  # reference palette slots in fixed order (blue, orange, aqua, yellow), validated
  colors <- stats::setNames(base::c("#2a78d6", "#eb6834", "#1baf7a", "#eda100")[base::seq_len(base::nrow(series))], series$label)
  dodge <- ggplot2::position_dodge(width = 0.6)

  # the axis is the price ratio on a log scale (symmetric for a log model),
  # labelled as % difference
  ggplot2::ggplot(plot_tbl, ggplot2::aes(x = 1 + .data$pct_diff, y = .data$procedure, color = .data$series)) +
    ggplot2::geom_vline(xintercept = 1, color = "#8a8983", linewidth = 0.4) +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = .data$lo, xmax = .data$hi), width = 0, linewidth = 0.6, orientation = "y", position = dodge, na.rm = TRUE) +
    ggplot2::geom_point(ggplot2::aes(shape = .data$exploratory), size = 2.2, stroke = 0.8, fill = "white", position = dodge) +
    # health-system cluster counts, one column at the right edge of each panel
    ggplot2::geom_text(ggplot2::aes(x = .data$label_x, label = .data$cluster_label), position = dodge, size = 2.4, hjust = 0.5, show.legend = FALSE) +
    ggplot2::scale_shape_manual(
      values = base::c(`FALSE` = 16, `TRUE` = 21),
      labels = base::c(`FALSE` = base::paste0(min_treated_clusters(), "+ health systems: 95% wild cluster bootstrap CI"),
                       `TRUE` = base::paste0("Fewer than ", min_treated_clusters(), " systems: exploratory, no interval")),
      name = NULL
    ) +
    ggplot2::scale_color_manual(values = colors, name = NULL, drop = FALSE) +
    ggplot2::scale_x_log10(
      breaks = base::c(0.25, 0.5, 1, 2, 4),
      labels = function(ratio) scales::label_percent(style_positive = "plus")(ratio - 1)
    ) +
    ggplot2::coord_cartesian(xlim = base::c(0.15, 21)) +
    ggplot2::guides(color = ggplot2::guide_legend(order = 1), shape = ggplot2::guide_legend(order = 2)) +
    ggplot2::facet_wrap(~payer, ncol = 1L, scales = "free_y") +
    ggplot2::labs(
      x = "Adjusted difference vs nonprofit hospitals, 100 x (exp(b) - 1), log axis", y = NULL,
      title = "Negotiated facility prices by hospital ownership",
      subtitle = "log(price) ~ ownership + state fixed effects + hospital type, system membership, bed size",
      caption = base::paste(
        "Strict and broad PE come from separate models (strict is nested in broad); \"For-profit, not PE\" is from the strict model.",
        "Intervals: wild cluster restricted bootstrap, Webb weights, 9,999 draws, clustered by health system; limits exponentiated separately.",
        "Right-hand columns: health-system clusters in each group, in series colours.",
        "Strict PE is mostly Lifepoint and ScionHealth (Apollo): descriptive, not causal.",
        "IUD insertion under Medicare Advantage is omitted: Medicare does not cover 58300, so those rates are not payments.",
        sep = "\n"
      )
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      legend.position = "top", legend.box = "vertical", panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(), strip.text = ggplot2::element_text(face = "bold", hjust = 0),
      plot.background = ggplot2::element_rect(fill = "white", color = NA)
    )
}
