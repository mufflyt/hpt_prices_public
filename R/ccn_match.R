#' Match MRF-level facility records to CCNs
#'
#' MRFs identify their hospital by name, location, license, and type 2
#' NPI, never by CCN, so every price row reaches the CCN-keyed universe
#' through [match_facilities_to_ccn()]. Tiers, first success wins:
#' 1. `mrf_url`: the MRF URL appears in the cms-hpt-tracker manifest.
#' 2. `npi`: a declared type 2 NPI is in CMS Hospital Enrollments.
#' 3. `license`: license number + state, only when the caller supplies a
#'    license crosswalk. No free national license -> CCN source exists: the
#'    CMS roster and enrollment files carry no license numbers, and NPPES
#'    taxonomy license fields are sparse and inconsistent for hospitals
#'    (checked 2026-09-12: none for HCA Houston Healthcare Southeast, "0900"
#'    for Denver Health).
#' 4. `name_address`: name, street, and city similarity against roster
#'    hospitals in the same state, accepted only above a threshold and with
#'    a clear margin over the runner-up, and never on a one-word name
#'    without a street address.
#'
#' `ccn_match_score` is always the name/address/city similarity between the
#' facility record and the CCN it received (0-1), so a low score flags a
#' tier-1 or tier-2 match whose own names disagree with the roster.

# ported from emb_colonoscopy R/hpt_hospital_discovery.R @ 471e067
normalize_hospital_name <- function(name) {
  name |>
    stringr::str_to_upper() |>
    stringr::str_replace_all("[^A-Z0-9 ]", " ") |>
    stringr::str_squish()
}

hospital_name_stop_words <- function() {
  base::c("HOSPITAL", "MEDICAL", "CENTER", "CENTRE", "HEALTH", "SYSTEM", "THE", "OF")
}

# ported from emb_colonoscopy R/hpt_hospital_discovery.R @ 471e067
# (drops NA/empty tokens so two missing names never score 1)
hospital_name_tokens <- function(name) {
  tokens <- normalize_hospital_name(name) |>
    stringr::str_split("\\s+") |>
    base::unlist()

  tokens <- tokens[!base::is.na(tokens) & base::nzchar(tokens)]

  base::setdiff(tokens, hospital_name_stop_words())
}

#' Name tokens for matching, vectorized
#'
#' The [hospital_name_tokens()] rules after expanding the roster's
#' abbreviations ("REG", "CTR", "MEM"), folding SAINT to ST and CHILDRENS to
#' CHILDREN, removing apostrophes ("MARY'S" -> "MARYS"), and dropping legal
#' suffixes and connectives ("INC", "FOR", "AT").
#'
#' @return A list of token vectors, one per element of `name`.
match_name_tokens <- function(name) {
  abbreviations <- base::c(
    "\\bCTR\\b" = "CENTER", "\\bHOSP\\b" = "HOSPITAL", "\\bMED\\b" = "MEDICAL",
    "\\bREG\\b" = "REGIONAL", "\\bMEM\\b" = "MEMORIAL", "\\bCMTY\\b" = "COMMUNITY",
    "\\bHLTH\\b" = "HEALTH", "\\bUNIV\\b" = "UNIVERSITY", "\\bGEN\\b" = "GENERAL",
    "\\bSAINT\\b" = "ST", "\\bMOUNT\\b" = "MT", "\\bCHILDRENS\\b" = "CHILDREN",
    "\\bORTHOPAEDIC\\b" = "ORTHOPEDIC", "\\bORTHO\\b" = "ORTHOPEDIC", "\\bHOSPITALS\\b" = "HOSPITAL"
  )
  legal_words <- base::c(
    "INC", "LLC", "LP", "LLP", "CORP", "CORPORATION", "CO", "COMPANY", "PC", "PLLC", "DBA",
    "AND", "FOR", "AT", "A", "AN", "IN"
  )
  drop_words <- base::c(hospital_name_stop_words(), legal_words)

  expanded <- base::as.character(name) |>
    stringr::str_remove_all("['’]") |>
    normalize_hospital_name() |>
    stringr::str_replace_all(abbreviations)

  purrr::map(stringr::str_split(expanded, " "), function(tokens) {
    base::setdiff(tokens[!base::is.na(tokens) & base::nzchar(tokens)], drop_words)
  })
}

#' Upper-case street address with standard USPS suffix abbreviations
normalize_street_address <- function(address) {
  suffixes <- base::c(
    STREET = "ST", AVENUE = "AVE", ROAD = "RD", DRIVE = "DR", BOULEVARD = "BLVD",
    HIGHWAY = "HWY", PARKWAY = "PKWY", LANE = "LN", COURT = "CT", PLACE = "PL",
    CIRCLE = "CIR", FREEWAY = "FWY", EXPRESSWAY = "EXPY", TERRACE = "TER",
    SQUARE = "SQ", SUITE = "STE", ROUTE = "RT", NORTHEAST = "NE", NORTHWEST = "NW",
    SOUTHEAST = "SE", SOUTHWEST = "SW", NORTH = "N", SOUTH = "S", EAST = "E", WEST = "W"
  )
  patterns <- stats::setNames(suffixes, base::paste0("\\b", base::names(suffixes), "\\b"))

  normalize_hospital_name(address) |>
    stringr::str_replace_all(patterns)
}

#' Unique street-address tokens, vectorized (character() when missing)
address_tokens <- function(address) {
  purrr::map(stringr::str_split(normalize_street_address(address), " "), function(tokens) {
    base::unique(tokens[!base::is.na(tokens) & base::nzchar(tokens)])
  })
}

normalize_city <- function(city) {
  normalize_hospital_name(city) |>
    stringr::str_replace_all(base::c("\\bSAINT\\b" = "ST", "\\bFORT\\b" = "FT", "\\bMOUNT\\b" = "MT"))
}

#' The five-digit ZIP a record carries, wherever it sits
#'
#' The roster has a zip_code column; an MRF facility has only a free-text
#' address, where the ZIP is the last five-digit run ("777 Bannock St,
#' Denver, CO 80204", ZIP+4 truncated to five). Returns NA when there is
#' none, and NA never blocks anything: a missing ZIP cannot contradict one.
zip_key <- function(x) {
  x <- base::as.character(x)
  bare <- stringr::str_match(stringr::str_trim(x), "^([0-9]{5})(?:-[0-9]{4})?$")[, 2]
  trailing <- stringr::str_match(x, "([0-9]{5})(?:-[0-9]{4})?\\s*$")[, 2]
  out <- dplyr::coalesce(bare, trailing)
  dplyr::if_else(base::is.na(out) | !base::nzchar(out), NA_character_, out)
}

#' Weighted similarity over whichever components are available
#'
#' Name 0.5, street address 0.35, city 0.15; a missing address or city
#' drops out of both numerator and denominator.
combine_similarity <- function(name_score, address_score, city_score) {
  has_address <- !base::is.na(address_score)
  has_city <- !base::is.na(city_score)

  numerator <- 0.5 * name_score +
    0.35 * dplyr::coalesce(address_score, 0) +
    0.15 * dplyr::coalesce(city_score, 0)
  denominator <- 0.5 + 0.35 * has_address + 0.15 * has_city

  numerator / denominator
}

#' Everything the matcher knows about each CCN, indexed for fast scoring
#'
#' `profile` has one row per CCN: roster rows first, then CCNs only the
#' tracker or enrollments know. Each CCN's names ("aliases") are the roster
#' name plus enrollment legal and doing-business-as names, which often match
#' an MRF's hospital_name better than the roster's abbreviations ("DENVER
#' HEALTH MEDICAL CENTER" vs "DENVER HEALTH & HOSPITAL AUTHORITY"). Tracker
#' location names are not used as aliases because the tracker derived them
#' from the name matching we are checking.
#'
#' Inverted indexes (token -> alias ids, token -> profile rows) let
#' [score_ccn_candidates()] count token overlaps against every CCN at once.
ccn_match_profiles <- function(universe, npi_xwalk = NULL, tracker_manifest = NULL) {
  universe <- add_missing_columns(universe, "zip_code")
  profile <- universe |>
    dplyr::transmute(
      ccn = .data$facility_id,
      state = .data$state,
      address = .data$address,
      citytown = .data$citytown,
      zip = .data$zip_code,
      name = .data$facility_name,
      in_universe = TRUE
    )

  if (!base::is.null(tracker_manifest)) {
    tracker_profile <- tracker_manifest |>
      dplyr::filter(!.data$ccn %in% profile$ccn) |>
      dplyr::distinct(.data$ccn, .keep_all = TRUE) |>
      dplyr::transmute(
        ccn = .data$ccn, state = .data$state, address = NA_character_,
        citytown = .data$city, zip = NA_character_, name = .data$hospital_name, in_universe = FALSE
      )
    profile <- dplyr::bind_rows(profile, tracker_profile)
  }

  alias_tbl <- dplyr::select(profile, "ccn", "name")

  if (!base::is.null(npi_xwalk)) {
    xwalk_profile <- npi_xwalk |>
      dplyr::filter(!.data$ccn %in% profile$ccn) |>
      dplyr::distinct(.data$ccn, .keep_all = TRUE) |>
      dplyr::transmute(
        ccn = .data$ccn, state = .data$state, address = NA_character_,
        citytown = NA_character_, zip = NA_character_,
        name = dplyr::coalesce(.data$doing_business_as_name, .data$organization_name),
        in_universe = FALSE
      )
    profile <- dplyr::bind_rows(profile, xwalk_profile)

    alias_tbl <- dplyr::bind_rows(
      alias_tbl,
      dplyr::transmute(npi_xwalk, ccn = .data$ccn, name = .data$organization_name),
      dplyr::transmute(npi_xwalk, ccn = .data$ccn, name = .data$doing_business_as_name)
    )
  }

  alias_tbl <- alias_tbl |>
    dplyr::filter(!base::is.na(.data$name), .data$ccn %in% profile$ccn) |>
    dplyr::distinct()
  alias_tokens <- match_name_tokens(alias_tbl$name)
  alias_profile_row <- base::match(alias_tbl$ccn, profile$ccn)

  street_tokens <- address_tokens(profile$address)
  first_tokens <- base::vapply(street_tokens, function(tokens) if (base::length(tokens) > 0L) tokens[[1]] else NA_character_, base::character(1))

  base::list(
    profile = profile,
    city_key = normalize_city(profile$citytown),
    zip_key = zip_key(profile$zip),
    address_n = base::lengths(street_tokens),
    house_number = dplyr::if_else(stringr::str_detect(first_tokens, "^[0-9]+$"), first_tokens, NA_character_),
    alias_profile_row = alias_profile_row,
    alias_n = base::lengths(alias_tokens),
    name_index = base::split(
      base::rep(base::seq_along(alias_tokens), base::lengths(alias_tokens)),
      base::unlist(alias_tokens)
    ),
    address_index = base::split(
      base::rep(base::seq_along(street_tokens), base::lengths(street_tokens)),
      base::unlist(street_tokens)
    )
  )
}

#' Tokenized name, address, and city for each facility record
#'
#' @return `facilities` plus list columns `.name_tokens` (one entry per
#'   facility: list of hospital_name and location_name token vectors) and
#'   `.address_tokens`, and `.city_key`.
facility_match_features <- function(facilities) {
  facilities$.name_tokens <- purrr::map2(
    match_name_tokens(facilities$hospital_name),
    match_name_tokens(facilities$location_name),
    base::list
  )
  facilities$.address_tokens <- address_tokens(facilities$address)
  facilities$.city_key <- normalize_city(facilities$city)
  facilities$.zip_key <- zip_key(facilities$address)

  facilities
}

#' Similarity of one facility record to each candidate CCN
#'
#' Name: best Jaccard over the facility's hospital_name/location_name and
#' each of the CCN's aliases. Address: share of the CCN's street tokens found
#' in the facility address (containment, so a facility address that also
#' carries city/state/ZIP still scores 1), and 0 when the house numbers
#' differ. City: exact match of normalized city. See [combine_similarity()].
#'
#' @param facility One row of [facility_match_features()] output, as a list.
#' @param candidate_ccns Character vector of CCNs.
#' @param profiles [ccn_match_profiles()] output.
score_ccn_candidates <- function(facility, candidate_ccns, profiles) {
  n_profile <- base::nrow(profiles$profile)
  n_alias <- base::length(profiles$alias_n)
  candidate_rows <- base::match(candidate_ccns, profiles$profile$ccn)

  best_alias <- base::numeric(n_alias)

  for (tokens in facility$.name_tokens) {
    if (base::length(tokens) == 0L) {
      next
    }

    hits <- base::c(base::integer(), base::unlist(profiles$name_index[tokens], use.names = FALSE))
    overlap <- base::tabulate(hits, nbins = n_alias)
    best_alias <- base::pmax(best_alias, overlap / (base::length(tokens) + profiles$alias_n - overlap))
  }

  name_by_row <- base::numeric(n_profile)
  scored <- base::which(best_alias > 0)

  if (base::length(scored) > 0L) {
    scored <- scored[base::order(best_alias[scored], decreasing = TRUE)]
    first_per_row <- !base::duplicated(profiles$alias_profile_row[scored])
    name_by_row[profiles$alias_profile_row[scored][first_per_row]] <- best_alias[scored][first_per_row]
  }

  name_score <- dplyr::coalesce(name_by_row[candidate_rows], 0)

  facility_address <- facility$.address_tokens

  address_score <- if (base::length(facility_address) == 0L) {
    base::rep(NA_real_, base::length(candidate_ccns))
  } else {
    hits <- base::c(base::integer(), base::unlist(profiles$address_index[facility_address], use.names = FALSE))
    containment <- base::tabulate(hits, nbins = n_profile) / profiles$address_n
    containment[profiles$address_n == 0L] <- NA_real_
    facility_numbers <- facility_address[stringr::str_detect(facility_address, "^[0-9]+$")]

    if (base::length(facility_numbers) > 0L) {
      containment[!base::is.na(profiles$house_number) & !profiles$house_number %in% facility_numbers &
                    profiles$address_n > 0L] <- 0
    }

    containment[candidate_rows]
  }

  facility_city <- facility$.city_key %||% NA_character_
  candidate_cities <- profiles$city_key[candidate_rows]
  city_score <- if (base::is.na(facility_city) || !base::nzchar(facility_city)) {
    base::rep(NA_real_, base::length(candidate_ccns))
  } else {
    dplyr::if_else(base::is.na(candidate_cities), NA_real_, base::as.numeric(candidate_cities == facility_city))
  }

  combine_similarity(name_score, address_score, city_score)
}

#' Narrow same-state candidates to those a hard key agrees with
#'
#' State alone is a weak block: it leaves every hospital in Texas competing on
#' name similarity, and the name tier already accepts scores as low as 0.6.
#' So before scoring, candidates are cut to those sharing the facility's ZIP,
#' or failing that its city.
#'
#' Two rules keep this from losing true matches:
#' - a candidate whose key is UNKNOWN is never dropped, because a missing ZIP
#'   cannot contradict one (tracker- and NPI-only profiles carry no ZIP);
#' - if no candidate agrees on any key, the state-wide set is returned with
#'   `key = "state"`, and the caller demands a higher score for it.
#'
#' Phone number would be a better block than either, and is not available:
#' the CMS roster carries one, an MRF does not, so there is nothing to
#' compare against. The same is true of any identifier absent from the file.
#'
#' @return list(ccns, key) where key is "zip", "city" or "state".
block_ccn_candidates <- function(facility, candidate_ccns, profiles) {
  rows <- base::match(candidate_ccns, profiles$profile$ccn)

  agree_on <- function(facility_key, candidate_keys) {
    if (base::is.na(facility_key) || !base::nzchar(facility_key)) return(NULL)
    hit <- !base::is.na(candidate_keys) & candidate_keys == facility_key
    if (base::any(hit)) candidate_ccns[hit] else NULL
  }

  by_zip <- agree_on(facility$.zip_key %||% NA_character_, profiles$zip_key[rows])
  if (!base::is.null(by_zip)) return(base::list(ccns = by_zip, key = "zip"))

  by_city <- agree_on(facility$.city_key %||% NA_character_, profiles$city_key[rows])
  if (!base::is.null(by_city)) return(base::list(ccns = by_city, key = "city"))

  base::list(ccns = candidate_ccns, key = "state")
}

facility_state_of <- function(facility) {
  state <- facility$state %||% NA_character_

  if (base::is.na(state)) {
    state <- facility$license_state %||% NA_character_
  }

  state
}

#' Choose among several CCNs tied to the same evidence (URL or NPI)
#'
#' Narrows to candidates in the facility's state, then its city, whenever
#' that leaves at least one; then takes the best similarity if it beats the
#' runner-up by `min_margin` and reaches `min_pick_score`. Otherwise every
#' remaining candidate is returned with `ambiguous = TRUE`.
#'
#' @return list(ccn, score, ambiguous).
pick_among_candidates <- function(facility, ccns, profiles, min_margin, min_pick_score) {
  ccns <- base::unique(ccns)
  candidate_rows <- base::match(ccns, profiles$profile$ccn)

  if (base::length(ccns) > 1L) {
    facility_state <- facility_state_of(facility)
    keep <- !base::is.na(facility_state) & profiles$profile$state[candidate_rows] %in% facility_state

    if (base::any(keep)) {
      ccns <- ccns[keep]
      candidate_rows <- candidate_rows[keep]
    }
  }

  if (base::length(ccns) > 1L) {
    facility_city <- facility$.city_key %||% NA_character_
    keep <- !base::is.na(facility_city) & profiles$city_key[candidate_rows] %in% facility_city

    if (base::any(keep)) {
      ccns <- ccns[keep]
    }
  }

  scores <- score_ccn_candidates(facility, ccns, profiles)

  if (base::length(ccns) == 1L) {
    return(base::list(ccn = ccns, score = scores, ambiguous = FALSE))
  }

  ordered <- base::order(scores, decreasing = TRUE)
  top_score <- scores[[ordered[[1]]]]
  runner_up <- scores[[ordered[[2]]]]

  if (top_score >= min_pick_score && top_score - runner_up >= min_margin) {
    return(base::list(ccn = ccns[[ordered[[1]]]], score = top_score, ambiguous = FALSE))
  }

  base::list(ccn = ccns, score = scores, ambiguous = TRUE)
}

#' Every 10-digit NPI in a type_2_npi cell
#'
#' Handles "1174576698", "123...;456...", JSON-ish "[1174576698]" and "[]".
parse_npi_list <- function(x) {
  stringr::str_extract_all(dplyr::coalesce(base::as.character(x), ""), "(?<![0-9])[0-9]{10}(?![0-9])")
}

normalize_license_key <- function(license_number) {
  key <- stringr::str_remove_all(stringr::str_to_upper(license_number), "[^A-Z0-9]")
  key <- stringr::str_remove(key, "^0+(?=[0-9A-Z])")

  dplyr::if_else(base::is.na(key) | !base::nzchar(key), NA_character_, key)
}

#' How much more name similarity a name-only match needs
#'
#' Applies when the state is all that agrees and the file gives no street
#' address, so nothing but the name is holding the match up. At the default
#' min_score of 0.6 that match needs 0.75. The value is a judgement, not a
#' measurement: it is the gap between "these two names look alike" and "these
#' two names look alike and nothing else about them agrees".
unblocked_name_penalty <- function() {
  0.15
}

#' Add absent optional columns as NA character
add_missing_columns <- function(tbl, columns) {
  for (column in base::setdiff(columns, base::names(tbl))) {
    tbl[[column]] <- NA_character_
  }

  tbl
}

facility_match_columns <- function() {
  base::c(
    "facility_key", "source", "mrf_url", "hospital_name", "location_name", "address",
    "city", "state", "license_number", "license_state", "type_2_npi"
  )
}

#' Resolve one facility through the tiers
#'
#' @return list(ccn, score, method, ambiguous, conflict, npi_ccns).
resolve_facility_ccn <- function(facility, url_ccns, npi_ccns, license_ccns, state_ccns,
                                 profiles, min_score, min_margin) {
  npi_text <- if (base::length(npi_ccns) > 0L) base::paste(base::sort(npi_ccns), collapse = ";") else NA_character_
  min_pick_score <- min_score / 2

  result <- function(picked, method, conflict = FALSE, block_key = NA_character_) {
    base::c(picked, base::list(method = method, conflict = conflict, npi_ccns = npi_text, block_key = block_key))
  }

  if (base::length(url_ccns) > 0L) {
    picked <- pick_among_candidates(facility, url_ccns, profiles, min_margin, min_pick_score)
    method <- "mrf_url"

    if (picked$ambiguous && base::length(npi_ccns) > 0L) {
      overlap <- base::intersect(picked$ccn, npi_ccns)

      if (base::length(overlap) == 1L) {
        picked <- base::list(ccn = overlap, score = picked$score[picked$ccn == overlap], ambiguous = FALSE)
        method <- "mrf_url+npi"
      }
    }

    conflict <- base::length(npi_ccns) > 0L && !base::any(picked$ccn %in% npi_ccns)

    return(result(picked, method, conflict))
  }

  if (base::length(npi_ccns) > 0L) {
    return(result(pick_among_candidates(facility, npi_ccns, profiles, min_margin, min_pick_score), "npi"))
  }

  if (base::length(license_ccns) > 0L) {
    return(result(pick_among_candidates(facility, license_ccns, profiles, min_margin, min_pick_score), "license"))
  }

  unmatched <- base::list(ccn = NA_character_, score = NA_real_, ambiguous = FALSE)
  facility_state <- facility_state_of(facility)
  candidates <- if (base::is.na(facility_state)) base::character() else state_ccns[[facility_state]] %||% base::character()

  # A one-word name ("Fremont Medical Center" -> FREMONT) fits several
  # hospitals in a city; without a street address it is not evidence enough.
  name_words <- base::max(0L, base::lengths(facility$.name_tokens))

  if (name_words < 2L && base::length(facility$.address_tokens) == 0L) {
    return(result(unmatched, NA_character_))
  }

  if (base::length(candidates) == 0L) {
    return(result(unmatched, NA_character_))
  }

  blocked <- block_ccn_candidates(facility, candidates, profiles)
  candidates <- blocked$ccns

  scores <- score_ccn_candidates(facility, candidates, profiles)
  ordered <- base::order(scores, decreasing = TRUE)
  top_score <- scores[[ordered[[1]]]]
  runner_up <- if (base::length(scores) > 1L) scores[[ordered[[2]]]] else 0

  # When nothing but the state agrees AND the file carries no street address,
  # the name alone is the whole match: demand more of it. A file with an
  # address is not penalised, because the address is real evidence -- it is
  # scored by containment and zeroed outright when house numbers differ, and
  # a genuine match whose name is generic ("Denver Health Medical Center"
  # against a roster alias) scores 0.71 on an exact street match alone.
  name_only <- blocked$key == "state" && base::length(facility$.address_tokens) == 0L
  floor_score <- if (name_only) min_score + unblocked_name_penalty() else min_score

  if (top_score >= floor_score && top_score - runner_up >= min_margin) {
    picked <- base::list(ccn = candidates[[ordered[[1]]]], score = top_score, ambiguous = FALSE)
    return(result(picked, "name_address", block_key = blocked$key))
  }

  result(unmatched, NA_character_)
}

#' Match MRF-level facility records to CMS Certification Numbers
#'
#' @param facilities tibble with [facility_match_columns()]: facility_key
#'   (unique), source, mrf_url, hospital_name, location_name, address, city,
#'   state, license_number, license_state, type_2_npi (one or several NPIs,
#'   ";"-separated or JSON-ish "[...]").
#' @param tracker_manifest `load_tracker()$manifest` (needs ccn, mrf_url),
#'   or NULL to skip the mrf_url tier.
#' @param npi_xwalk [npi_ccn_crosswalk()] output (needs npi, ccn), or NULL to
#'   skip the npi tier.
#' @param universe [build_hospital_universe()] output.
#' @param license_xwalk Optional tibble(license_number, license_state, ccn).
#' @param min_score Minimum similarity for the name_address tier (half of it
#'   is the floor for choosing among URL/NPI candidates).
#' @param min_margin Required lead of the best candidate over the runner-up.
#' @return `facilities` with ccn, ccn_match_method ("mrf_url",
#'   "mrf_url+npi", "npi", "license", "name_address", or NA),
#'   ccn_match_score, ccn_block_key (which key narrowed the name tier's
#'   candidates: "zip", "city" or "state"), ccn_ambiguous, ccn_conflict
#'   (tier-1 CCN not among the
#'   NPI-derived CCNs; the tier-1 CCN is kept), npi_ccns, and
#'   tracker_match_method. One row per facility_key x ccn; unmatched
#'   facilities keep one row with ccn = NA.
match_facilities_to_ccn <- function(facilities,
                                    tracker_manifest = NULL,
                                    npi_xwalk = NULL,
                                    universe,
                                    license_xwalk = NULL,
                                    min_score = 0.6,
                                    min_margin = 0.15) {
  require_columns(facilities, facility_match_columns(), "Facility records")

  if (base::anyDuplicated(facilities$facility_key) > 0L) {
    base::stop("facility_key must be unique in `facilities`.")
  }

  facilities <- dplyr::mutate(facilities, .row = base::seq_len(dplyr::n()))

  if (!base::is.null(tracker_manifest)) {
    require_columns(tracker_manifest, base::c("ccn", "mrf_url"), "Tracker manifest")
    tracker_manifest <- add_missing_columns(tracker_manifest, base::c("state", "city", "hospital_name"))
  }

  if (!base::is.null(npi_xwalk)) {
    require_columns(npi_xwalk, base::c("npi", "ccn"), "NPI crosswalk")
    npi_xwalk <- add_missing_columns(npi_xwalk, base::c("state", "organization_name", "doing_business_as_name"))
  }

  profiles <- ccn_match_profiles(universe, npi_xwalk, tracker_manifest)

  url_candidates <- tibble::tibble(.row = base::integer(), ccn = base::character(), tracker_match_method = base::character())

  if (!base::is.null(tracker_manifest)) {
    url_candidates <- facilities |>
      dplyr::transmute(.row = .data$.row, url_key = normalize_url_key(.data$mrf_url)) |>
      dplyr::filter(!base::is.na(.data$url_key)) |>
      dplyr::inner_join(
        dplyr::select(tracker_mrf_urls(tracker_manifest), "url_key", "ccn", "tracker_match_method"),
        by = "url_key",
        relationship = "many-to-many"
      ) |>
      dplyr::distinct(.data$.row, .data$ccn, .keep_all = TRUE)
  }

  npi_candidates <- tibble::tibble(.row = base::integer(), ccn = base::character())

  if (!base::is.null(npi_xwalk)) {
    npi_candidates <- facilities |>
      dplyr::transmute(.row = .data$.row, npi = parse_npi_list(.data$type_2_npi)) |>
      tidyr::unnest_longer("npi", keep_empty = FALSE) |>
      dplyr::inner_join(dplyr::select(npi_xwalk, "npi", "ccn"), by = "npi", relationship = "many-to-many") |>
      dplyr::distinct(.data$.row, .data$ccn)
  }

  license_candidates <- tibble::tibble(.row = base::integer(), ccn = base::character())

  if (!base::is.null(license_xwalk)) {
    require_columns(license_xwalk, base::c("license_number", "license_state", "ccn"), "License crosswalk")

    license_candidates <- facilities |>
      dplyr::transmute(
        .row = .data$.row,
        license_key = normalize_license_key(.data$license_number),
        license_state = stringr::str_to_upper(dplyr::coalesce(.data$license_state, .data$state))
      ) |>
      dplyr::filter(!base::is.na(.data$license_key)) |>
      dplyr::inner_join(
        dplyr::transmute(
          license_xwalk,
          license_key = normalize_license_key(.data$license_number),
          license_state = stringr::str_to_upper(.data$license_state),
          ccn = .data$ccn
        ),
        by = base::c("license_key", "license_state"),
        relationship = "many-to-many"
      ) |>
      dplyr::distinct(.data$.row, .data$ccn)
  }

  url_by_row <- base::split(url_candidates$ccn, url_candidates$.row)
  npi_by_row <- base::split(npi_candidates$ccn, npi_candidates$.row)
  license_by_row <- base::split(license_candidates$ccn, license_candidates$.row)
  state_ccns <- base::split(universe$facility_id, universe$state)

  feature_cols <- base::c("state", "license_state", ".row", ".name_tokens", ".address_tokens", ".city_key", ".zip_key")
  facility_rows <- purrr::transpose(base::as.list(facility_match_features(facilities)[feature_cols]))

  base::message("Matching ", base::length(facility_rows), " facility records to CCNs.")

  resolved <- purrr::map(
    facility_rows,
    function(facility) {
      key <- base::as.character(facility$.row)

      resolve_facility_ccn(
        facility,
        url_ccns = url_by_row[[key]] %||% base::character(),
        npi_ccns = npi_by_row[[key]] %||% base::character(),
        license_ccns = license_by_row[[key]] %||% base::character(),
        state_ccns = state_ccns,
        profiles = profiles,
        min_score = min_score,
        min_margin = min_margin
      )
    }
  )

  n_per_row <- base::lengths(purrr::map(resolved, "ccn"))
  matches <- tibble::tibble(
    .row = base::rep(facilities$.row, n_per_row),
    ccn = base::unlist(purrr::map(resolved, "ccn")),
    ccn_match_method = base::rep(purrr::map_chr(resolved, "method"), n_per_row),
    ccn_match_score = base::unlist(purrr::map(resolved, "score")),
    ccn_ambiguous = base::rep(purrr::map_lgl(resolved, "ambiguous"), n_per_row),
    ccn_conflict = base::rep(purrr::map_lgl(resolved, "conflict"), n_per_row),
    ccn_block_key = base::rep(purrr::map_chr(resolved, function(r) r$block_key %||% NA_character_), n_per_row),
    npi_ccns = base::rep(purrr::map_chr(resolved, "npi_ccns"), n_per_row)
  )

  tracker_methods <- dplyr::select(url_candidates, ".row", "ccn", "tracker_match_method")

  crosswalk <- facilities |>
    dplyr::left_join(matches, by = ".row", relationship = "one-to-many") |>
    dplyr::left_join(tracker_methods, by = base::c(".row", "ccn")) |>
    dplyr::mutate(
      tracker_match_method = dplyr::if_else(
        stringr::str_starts(dplyr::coalesce(.data$ccn_match_method, ""), "mrf_url"),
        .data$tracker_match_method,
        NA_character_
      )
    ) |>
    dplyr::select(-".row")

  method_counts <- crosswalk |>
    dplyr::distinct(.data$facility_key, .data$ccn_match_method) |>
    dplyr::count(.data$ccn_match_method)
  base::message(
    "CCN match methods (facilities): ",
    base::paste0(dplyr::coalesce(method_counts$ccn_match_method, "unmatched"), "=", method_counts$n, collapse = ", ")
  )

  crosswalk
}

#' How many universe CCNs have at least one matched MRF
#'
#' A CCN counts as `covered` when some facility row maps to it
#' unambiguously; `covered_incl_ambiguous` also counts ambiguous
#' candidates. Crosswalk CCNs outside the universe are reported in
#' `overall$n_ccns_outside_universe` and otherwise ignored.
#'
#' @return list(overall, by_state, by_hospital_type, by_health_system).
coverage_report <- function(crosswalk, universe) {
  require_columns(crosswalk, base::c("ccn", "ccn_ambiguous"), "CCN crosswalk")

  ccn_status <- crosswalk |>
    dplyr::filter(!base::is.na(.data$ccn)) |>
    dplyr::group_by(.data$ccn) |>
    dplyr::summarise(any_unambiguous = base::any(!.data$ccn_ambiguous), .groups = "drop")

  flagged <- universe |>
    dplyr::left_join(ccn_status, by = base::c(facility_id = "ccn")) |>
    dplyr::mutate(
      covered = dplyr::coalesce(.data$any_unambiguous, FALSE),
      covered_incl_ambiguous = !base::is.na(.data$any_unambiguous)
    )

  summarise_coverage <- function(tbl, ...) {
    tbl |>
      dplyr::group_by(...) |>
      dplyr::summarise(
        n_ccn = dplyr::n(),
        n_covered = base::sum(.data$covered),
        n_covered_incl_ambiguous = base::sum(.data$covered_incl_ambiguous),
        share_covered = .data$n_covered / .data$n_ccn,
        .groups = "drop"
      ) |>
      dplyr::arrange(dplyr::desc(.data$n_ccn))
  }

  overall <- summarise_coverage(flagged) |>
    dplyr::mutate(n_ccns_outside_universe = base::sum(!ccn_status$ccn %in% universe$facility_id))

  base::list(
    overall = overall,
    by_state = summarise_coverage(flagged, .data$state),
    by_hospital_type = summarise_coverage(flagged, .data$hospital_type),
    by_health_system = summarise_coverage(flagged, .data$health_sys_id, .data$health_sys_name)
  )
}
