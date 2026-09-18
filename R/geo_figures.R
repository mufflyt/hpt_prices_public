#' Geographic figures: state variation in hospital prices relative to Medicare
#'
#' Each hospital's negotiated facility rate is divided by what traditional
#' Medicare's outpatient system (OPPS) pays that hospital for the same code:
#' the national Addendum B payment rate adjusted by the hospital's wage index,
#'   medicare_opps = rate x (labor share x wage index + (1 - labor share)),
#' with the OPPS labor share of 60%. Wage index is the FY 2026 IPPS final rule
#' (CMS-1833-F) Table 2 value by CCN (the low-wage-index transition value
#' where one is listed, plus any out-migration adjustment). Hospitals absent
#' from Table 2 (mostly critical access hospitals, which Medicare pays on
#' cost) get their state's rural wage index from Table 3, so their ratio is
#' against what OPPS would pay in that area.
#'
#' Hospital-listed "medicare" rates are not used as the denominator: few
#' hospitals list one, many states have fewer than 5, and some state medians
#' are implausible for a national fee schedule.
#'
#' A state's value is the median of its hospitals' ratios. Maps use
#' mysterymaps::mysterymaps_geographic_map() for the state polygons,
#' projection, and theme, with a log-scale diverging fill centred on 1
#' (= the Medicare rate); states with fewer than `min_hospitals` hospitals
#' are suppressed (neutral fill, hatched). A system-weighted sensitivity
#' (state_system_weighted_summary()) checks that the state pattern is not
#' just a few large systems.

# ---- inputs: FY 2026 IPPS wage index ------------------------------------------

ipps_wage_index_source <- function() {
  base::list(
    release = "FY 2026 IPPS final rule (CMS-1833-F), Tables 2, 3, 4A, 4B",
    page_url = "https://www.cms.gov/medicare/payment/prospective-payment-systems/acute-inpatient-pps/fy-2026-ipps-final-rule-home-page",
    zip_url = "https://www.cms.gov/files/zip/fy2026-ipps-fr-tables-2-3-4a-4b.zip"
  )
}

#' Download and unzip the IPPS wage index tables; returns the unzip directory
download_ipps_wage_index <- function(source = ipps_wage_index_source(),
                                     dest_dir = hpt_path("reference", "cms_ipps_wage_index")) {
  zip_path <- base::file.path(dest_dir, base::basename(source$zip_url))
  download_public_file(source$zip_url, zip_path)
  unzip_dir <- stringr::str_remove(zip_path, "\\.zip$")
  utils::unzip(zip_path, exdir = unzip_dir)

  provenance_path <- base::paste0(unzip_dir, "_provenance.csv")
  if (!base::file.exists(provenance_path)) {
    write_csv_atomic(
      tibble::tibble(release = source$release, page_url = source$page_url, zip_url = source$zip_url,
                     zip_sha256 = sha256_file(zip_path), downloaded_at = utc_timestamp()),
      provenance_path
    )
  }
  unzip_dir
}

#' Path to one IPPS table's tab-delimited text file ("2" or "3")
find_ipps_table <- function(table, dir = hpt_path("reference", "cms_ipps_wage_index")) {
  paths <- base::list.files(dir, pattern = base::paste0("Tables ", table, "\\.txt$"), recursive = TRUE, full.names = TRUE)
  if (base::length(paths) == 0L) {
    base::stop("No IPPS Table ", table, " under ", dir, "; run download_ipps_wage_index() first.")
  }
  paths[[base::which.max(base::file.mtime(paths))]]
}

#' Read an IPPS table: one title line, then a tab-delimited header (Latin-1).
#' Column names carry footnote digits ("3,6 FY 2026 Wage Index With Cap"), so
#' columns are found by pattern.
read_ipps_table <- function(path) {
  base::suppressMessages(readr::read_tsv(
    path, skip = 1, locale = readr::locale(encoding = "latin1"),
    col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE
  ))
}

ipps_column <- function(tbl, pattern) {
  hits <- base::grep(pattern, base::names(tbl), value = TRUE)
  if (base::length(hits) != 1L) {
    base::stop("Expected one column matching '", pattern, "', found ", base::length(hits), ".")
  }
  tbl[[hits]]
}

#' FY 2026 wage index by CCN (Table 2)
load_hospital_wage_index <- function(path = find_ipps_table("2")) {
  t2 <- read_ipps_table(path)
  with_cap <- base::as.numeric(ipps_column(t2, "Wage Index With Cap"))
  transition <- base::as.numeric(ipps_column(t2, "Transition for the Discontinuation"))
  out_migration <- base::as.numeric(ipps_column(t2, "^[0-9]*Out-Migration Adjustment$"))

  tibble::tibble(
    ccn = stringr::str_pad(stringr::str_trim(ipps_column(t2, "CCN$")), 6, pad = "0"),
    wage_index = dplyr::coalesce(transition, with_cap) + dplyr::coalesce(out_migration, 0)
  ) |>
    dplyr::filter(!base::is.na(.data$wage_index), base::nchar(.data$ccn) == 6L) |>
    dplyr::distinct(.data$ccn, .keep_all = TRUE)
}

#' Statewide rural wage index (Table 3 rows with a two-digit CBSA)
load_state_rural_wage_index <- function(path = find_ipps_table("3")) {
  t3 <- read_ipps_table(path)
  tibble::tibble(
    cbsa = stringr::str_trim(t3$CBSA),
    state = stringr::str_trim(t3$State),
    rural_wage_index = base::as.numeric(ipps_column(t3, "^Wage Index$"))
  ) |>
    dplyr::filter(base::nchar(.data$cbsa) == 2L, !base::is.na(.data$rural_wage_index)) |>
    dplyr::distinct(.data$state, .keep_all = TRUE) |>
    dplyr::select("state", "rural_wage_index")
}

# ---- hospital ratios and state summaries --------------------------------------

#' Share of the OPPS payment adjusted by the wage index (42 CFR 419.43(c))
opps_labor_share <- function() {
  0.60
}

#' Medicare OPPS payment per hospital for one code
#'
#' @param hospitals tibble(ccn, state).
#' @param opps_rate National unadjusted Addendum B payment rate for the code.
hospital_medicare_benchmark <- function(hospitals, opps_rate, wage_index, rural_wage_index) {
  hospitals |>
    dplyr::left_join(wage_index, by = "ccn") |>
    dplyr::left_join(rural_wage_index, by = "state") |>
    dplyr::mutate(
      wage_index_source = dplyr::if_else(base::is.na(.data$wage_index), "state_rural", "ipps_table_2"),
      wage_index = dplyr::coalesce(.data$wage_index, .data$rural_wage_index),
      medicare_opps = opps_rate * (opps_labor_share() * .data$wage_index + (1 - opps_labor_share()))
    ) |>
    dplyr::select(-"rural_wage_index")
}

#' Hospital facility prices for one code joined to their Medicare benchmark
#'
#' @param prices tibble(ccn, code, payer_type, price) from
#'   ownership_hospital_prices() (facility fees, CCN-matched hospitals).
#' @param hospitals tibble(ccn, state, hospital_type) (dim_hospital).
hospital_price_ratios <- function(prices, hospitals, code, opps_rates, wage_index, rural_wage_index) {
  opps <- opps_rates[opps_rates$code == code, ]
  if (base::nrow(opps) == 0L || base::is.na(opps$payment_rate[[1]])) {
    base::stop("No OPPS payment rate for ", code, "; a Medicare-relative map needs one.")
  }

  benchmark <- hospital_medicare_benchmark(
    dplyr::distinct(hospitals, .data$ccn, .data$state), opps$payment_rate[[1]], wage_index, rural_wage_index
  )

  prices |>
    dplyr::filter(.data$code == !!code) |>
    dplyr::inner_join(benchmark, by = "ccn") |>
    dplyr::filter(!base::is.na(.data$state), !base::is.na(.data$medicare_opps)) |>
    dplyr::mutate(ratio = .data$price / .data$medicare_opps)
}

#' State median ratio and price, with within-state interquartile range
state_ratio_summary <- function(ratios, min_hospitals = 5L) {
  ratios |>
    dplyr::group_by(.data$code, .data$payer_type, .data$state) |>
    dplyr::summarise(
      n_hospitals = dplyr::n_distinct(.data$ccn),
      median_ratio = stats::median(.data$ratio),
      p25_ratio = stats::quantile(.data$ratio, 0.25, names = FALSE),
      p75_ratio = stats::quantile(.data$ratio, 0.75, names = FALSE),
      median_price = stats::median(.data$price),
      p25_price = stats::quantile(.data$price, 0.25, names = FALSE),
      p75_price = stats::quantile(.data$price, 0.75, names = FALSE),
      medicare_opps = stats::median(.data$medicare_opps),
      .groups = "drop"
    ) |>
    dplyr::mutate(low_n = .data$n_hospitals < min_hospitals)
}

#' p90/p10 of state medians and the share of hospital-level variance in
#' log price explained by state, per payer type (states with enough hospitals)
state_spread_stats <- function(ratios, summary) {
  enough <- summary |> dplyr::filter(!.data$low_n)
  spread <- enough |>
    dplyr::group_by(.data$payer_type) |>
    dplyr::summarise(
      n_states = dplyr::n(),
      p90_p10_state_median_price = stats::quantile(.data$median_price, 0.9, names = FALSE) /
        stats::quantile(.data$median_price, 0.1, names = FALSE),
      p90_p10_state_median_ratio = stats::quantile(.data$median_ratio, 0.9, names = FALSE) /
        stats::quantile(.data$median_ratio, 0.1, names = FALSE),
      median_within_state_p75_p25 = stats::median(.data$p75_price / .data$p25_price),
      .groups = "drop"
    )

  explained <- ratios |>
    dplyr::semi_join(enough, by = base::c("payer_type", "state")) |>
    dplyr::group_by(.data$payer_type) |>
    dplyr::group_modify(function(d, key) {
      fit <- stats::lm(base::log(price) ~ state, data = d)
      tibble::tibble(state_r2 = base::summary(fit)$r.squared, n_hospitals = base::nrow(d))
    }) |>
    dplyr::ungroup()

  dplyr::left_join(spread, explained, by = "payer_type")
}

# ---- system-weighting sensitivity -----------------------------------------------

#' One pricing unit per health system: the AHRQ system id (or name), else the
#' hospital itself
#'
#' @param hospitals tibble(ccn, health_sys_id, health_sys_name) (dim_hospital).
hospital_system_units <- function(hospitals) {
  system <- dplyr::coalesce(
    dplyr::na_if(stringr::str_trim(base::as.character(hospitals$health_sys_id)), ""),
    dplyr::na_if(stringr::str_trim(base::as.character(hospitals$health_sys_name)), "")
  )
  tibble::tibble(
    ccn = hospitals$ccn,
    system_unit = dplyr::if_else(base::is.na(system), base::paste0("ccn:", hospitals$ccn), base::paste0("sys:", system))
  ) |>
    dplyr::distinct(.data$ccn, .keep_all = TRUE)
}

#' State medians with equal weight per health system
#'
#' Hospital-weighted state medians let a system with many hospitals in a
#' state, often posting one rate at all of them, dominate that state. Here
#' each system's hospitals in a state collapse to their median first (a
#' hospital with no system is its own unit), then the state value is the
#' median across units. A system in several states is a separate unit in
#' each.
#'
#' @param ratios hospital_price_ratios() rows.
#' @param units hospital_system_units() rows.
state_system_weighted_summary <- function(ratios, units, min_units = 5L) {
  ratios |>
    dplyr::inner_join(units, by = "ccn") |>
    dplyr::group_by(.data$code, .data$payer_type, .data$state, .data$system_unit) |>
    dplyr::summarise(unit_ratio = stats::median(.data$ratio), unit_price = stats::median(.data$price),
                     unit_hospitals = dplyr::n_distinct(.data$ccn), .groups = "drop") |>
    dplyr::group_by(.data$code, .data$payer_type, .data$state) |>
    dplyr::summarise(
      n_units = dplyr::n(),
      n_hospitals = base::sum(.data$unit_hospitals),
      largest_unit_share = base::max(.data$unit_hospitals) / base::sum(.data$unit_hospitals),
      median_ratio = stats::median(.data$unit_ratio),
      median_price = stats::median(.data$unit_price),
      .groups = "drop"
    ) |>
    dplyr::mutate(low_n = .data$n_units < min_units)
}

#' Hospital- vs system-weighted state medians, per state and payer type
compare_system_weighting <- function(hospital_summary, system_summary) {
  hospital_summary |>
    dplyr::select("code", "payer_type", "state", "n_hospitals", hospital_low_n = "low_n",
                  hospital_weighted_ratio = "median_ratio", hospital_weighted_price = "median_price") |>
    dplyr::inner_join(
      system_summary |>
        dplyr::select("code", "payer_type", "state", "n_units", "largest_unit_share", system_low_n = "low_n",
                      system_weighted_ratio = "median_ratio", system_weighted_price = "median_price"),
      by = base::c("code", "payer_type", "state")
    ) |>
    dplyr::mutate(log_change = base::log(.data$system_weighted_ratio / .data$hospital_weighted_ratio))
}

#' Summary of how much system weighting changes the geographic picture
#'
#' Per payer type, over states with at least `min_hospitals` hospitals:
#' Spearman correlation of the two sets of state medians, p90/p10 spread of
#' state medians under each weighting, the share of variance in log price
#' explained by state (hospital level vs system-unit level), and the states
#' whose median moves most.
system_weighting_stats <- function(comparison, ratios, units, n_movers = 3L) {
  kept <- comparison |> dplyr::filter(!.data$hospital_low_n)
  p90_p10 <- function(x) stats::quantile(x, 0.9, names = FALSE) / stats::quantile(x, 0.1, names = FALSE)

  unit_level <- ratios |>
    dplyr::inner_join(units, by = "ccn") |>
    dplyr::semi_join(kept, by = base::c("payer_type", "state")) |>
    dplyr::group_by(.data$payer_type, .data$state, .data$system_unit) |>
    dplyr::summarise(unit_price = stats::median(.data$price), .groups = "drop")
  hospital_level <- ratios |> dplyr::semi_join(kept, by = base::c("payer_type", "state"))
  r2 <- function(d, y) {
    if (dplyr::n_distinct(d$state) < 2L) return(NA_real_)
    base::summary(stats::lm(base::log(d[[y]]) ~ d$state))$r.squared
  }

  kept |>
    dplyr::group_by(.data$payer_type) |>
    dplyr::group_modify(function(d, key) {
      movers <- d |> dplyr::arrange(dplyr::desc(base::abs(.data$log_change))) |> utils::head(n_movers)
      tibble::tibble(
        n_states = base::nrow(d),
        spearman = stats::cor(d$hospital_weighted_ratio, d$system_weighted_ratio, method = "spearman"),
        p90_p10_hospital_weighted = p90_p10(d$hospital_weighted_ratio),
        p90_p10_system_weighted = p90_p10(d$system_weighted_ratio),
        state_r2_hospital_level = r2(hospital_level[hospital_level$payer_type == key$payer_type, ], "price"),
        state_r2_system_level = r2(unit_level[unit_level$payer_type == key$payer_type, ], "unit_price"),
        median_abs_change = stats::median(base::abs(base::exp(d$log_change) - 1)),
        largest_movers = base::paste(base::sprintf("%s %.2fx to %.2fx", movers$state, movers$hospital_weighted_ratio,
                                                   movers$system_weighted_ratio), collapse = "; ")
      )
    }) |>
    dplyr::ungroup()
}

#' Supplementary scatter: hospital- vs system-weighted state ratio
system_weighting_chart <- function(comparison, payer_types, n_labels = 4L) {
  data <- comparison |>
    dplyr::filter(.data$payer_type %in% payer_types, !.data$hospital_low_n, !base::is.na(state_map_name(.data$state))) |>
    dplyr::mutate(panel = base::factor(payer_label(.data$payer_type), levels = payer_label(payer_types)))
  labelled <- data |>
    dplyr::group_by(.data$panel) |>
    dplyr::slice_max(base::abs(.data$log_change), n = n_labels, with_ties = FALSE) |>
    dplyr::ungroup()

  ggplot2::ggplot(data, ggplot2::aes(x = .data$hospital_weighted_ratio, y = .data$system_weighted_ratio)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = "grey55", linetype = "dashed", linewidth = 0.4) +
    ggplot2::geom_point(ggplot2::aes(size = .data$largest_unit_share), shape = 21, colour = "#b2182b",
                        fill = "#b2182b", alpha = 0.55, stroke = 0.3) +
    ggrepel::geom_text_repel(data = labelled, ggplot2::aes(label = .data$state), size = 2.6, min.segment.length = 0,
                             seed = 1) +
    ggplot2::scale_x_continuous(transform = "log", breaks = ratio_breaks(), labels = ratio_labels()) +
    ggplot2::scale_y_continuous(transform = "log", breaks = ratio_breaks(), labels = ratio_labels()) +
    ggplot2::scale_size_area(max_size = 4, name = "Largest system's share\nof the state's hospitals",
                             labels = scales::label_percent(accuracy = 1)) +
    ggplot2::facet_wrap(ggplot2::vars(.data$panel), scales = "free", nrow = 1) +
    ggplot2::labs(x = "Hospital-weighted state median (rate / Medicare)", y = "System-weighted state median") +
    ggplot2::theme_minimal(base_size = 9) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), legend.position = "bottom",
                   strip.text = ggplot2::element_text(face = "bold", size = 10))
}

# ---- drawing ------------------------------------------------------------------

#' Lower-case state name as used by ggplot2::map_data("state")
state_map_name <- function(abb) {
  lookup <- stats::setNames(base::c(datasets::state.name, "District of Columbia"), base::c(datasets::state.abb, "DC"))
  base::unname(lookup[abb])
}

#' Diagonal hatch lines clipped to the given states, as long/lat paths
#'
#' Built with sf on the maps "state" polygons (the same polygons
#' mysterymaps_geographic_map() draws), in plain long/lat so coord_map()
#' projects them with the fill.
state_hatch_lines <- function(state_abb, spacing = 0.55) {
  regions <- base::tolower(state_map_name(state_abb))
  regions <- regions[!base::is.na(regions)]
  if (base::length(regions) == 0L) {
    return(tibble::tibble(long = base::numeric(), lat = base::numeric(), group = base::character()))
  }

  # planar operations on long/lat are fine for drawing hatch lines
  old_s2 <- base::suppressMessages(sf::sf_use_s2(FALSE))
  base::on.exit(base::suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)

  polygons <- sf::st_as_sf(maps::map("state", fill = TRUE, plot = FALSE))
  polygons$region <- base::sub(":.*$", "", polygons$ID)
  area <- base::suppressMessages(sf::st_union(sf::st_make_valid(polygons[polygons$region %in% regions, ])))

  box <- sf::st_bbox(area)
  span <- (box[["xmax"]] - box[["xmin"]]) + (box[["ymax"]] - box[["ymin"]])
  offsets <- base::seq(-span, span, by = spacing)
  lines <- sf::st_sfc(base::lapply(offsets, function(o) {
    x <- base::c(box[["xmin"]], box[["xmax"]])
    sf::st_linestring(base::cbind(x, box[["ymin"]] + o + (x - box[["xmin"]])))
  }), crs = sf::st_crs(area))

  clipped <- base::suppressMessages(base::suppressWarnings(sf::st_cast(sf::st_intersection(lines, area), "MULTILINESTRING")))
  clipped <- base::suppressWarnings(sf::st_cast(sf::st_sf(geometry = clipped), "LINESTRING"))
  coords <- sf::st_coordinates(clipped)
  tibble::tibble(long = coords[, "X"], lat = coords[, "Y"], group = base::as.character(coords[, "L1"]))
}

#' Map fills outside the ratio scale: no data at all, and estimates
#' suppressed because the state has too few hospitals
geo_na_fill <- function() {
  "grey93"
}

geo_suppressed_fill <- function() {
  "grey70"
}

#' Axis and legend breaks for rate / Medicare ratios on a log scale
ratio_breaks <- function() {
  base::c(0.25, 0.5, 0.75, 1, 1.25, 1.5, 2, 3, 4, 6)
}

ratio_labels <- function() {
  scales::label_number(accuracy = 0.01, drop0trailing = TRUE, suffix = "x")
}

#' Log-scale diverging fill for payer/Medicare ratios, centred on 1
ratio_fill_scale <- function(limits, name = "Rate / Medicare") {
  breaks <- base::setdiff(ratio_breaks(), 1.25)
  ggplot2::scale_fill_gradient2(
    name = name, low = "#2166ac", mid = "#f7f7f7", high = "#b2182b", midpoint = 1,
    transform = "log", limits = limits, oob = scales::oob_squish,
    breaks = breaks[breaks >= limits[1] & breaks <= limits[2]],
    labels = ratio_labels(),
    na.value = geo_na_fill()
  )
}

#' One state map of median rate / Medicare for a payer type
#'
#' States with fewer than `min_hospitals` hospitals are suppressed, not just
#' flagged: their estimate is not drawn, they get a neutral fill with
#' hatching, and "no data" states stay a lighter plain grey. The ranking
#' chart still shows their estimates (hollow dots).
#'
#' @param summary state_ratio_summary() rows for one code and payer type.
ratio_state_map <- function(summary, title, limits) {
  if (!requireNamespace("mysterymaps", quietly = TRUE)) {
    base::stop("Package 'mysterymaps' is required: remotes::install_github('mufflyt/mysterymaps')")
  }

  map_data <- summary |>
    dplyr::mutate(state_name = state_map_name(.data$state),
                  median_ratio = dplyr::if_else(.data$low_n, NA_real_, .data$median_ratio)) |>
    dplyr::filter(!base::is.na(.data$state_name))
  suppressed <- map_data$state[map_data$low_n]

  # the function averages outcome_col per state; with one row per state that
  # is the state median itself. Its rate-in-[0, 1] warning does not apply to
  # ratios, and its low-count warning is replaced by the suppression.
  base_map <- base::withCallingHandlers(
    mysterymaps::mysterymaps_geographic_map(
      map_data, state_col = "state_name", outcome_col = "median_ratio",
      title = title, low_states_warn = 0L, na_color = geo_na_fill()
    ),
    warning = function(w) {
      if (base::grepl("outside \\[0, 1\\]", base::conditionMessage(w))) base::invokeRestart("muffleWarning")
    }
  )

  plot <- base::suppressMessages(base_map + ratio_fill_scale(limits))
  if (base::length(suppressed) > 0L) {
    polygons <- ggplot2::map_data("state")
    polygons <- polygons[polygons$region %in% base::tolower(state_map_name(suppressed)), ]
    hatch <- state_hatch_lines(suppressed)
    plot <- plot +
      ggplot2::geom_polygon(
        data = polygons, mapping = ggplot2::aes(x = .data$long, y = .data$lat, group = .data$group),
        inherit.aes = FALSE, fill = geo_suppressed_fill(), colour = "white", linewidth = 0.2
      ) +
      ggplot2::geom_path(
        data = hatch, mapping = ggplot2::aes(x = .data$long, y = .data$lat, group = .data$group),
        inherit.aes = FALSE, linewidth = 0.25, colour = "grey25"
      )
  }
  plot + ggplot2::theme(legend.key.height = ggplot2::unit(1.1, "cm"))
}

#' Shared fill limits so every map panel uses the same colours (from the
#' states that are drawn, not the suppressed ones)
ratio_limits <- function(summary, payer_types) {
  values <- summary$median_ratio[summary$payer_type %in% payer_types & !summary$low_n]
  base::c(base::min(0.5, base::min(values, na.rm = TRUE)), base::max(2, base::max(values, na.rm = TRUE)))
}

payer_label <- function(payer_type) {
  labels <- base::c(commercial = "Commercial", medicaid = "Medicaid", medicare_advantage = "Medicare Advantage",
                    exchange = "Exchange", self_pay_cash = "Cash price")
  base::unname(labels[payer_type])
}

#' Ranked dot-and-interval chart of state medians (dollars)
#'
#' One panel per payer type, states ordered by their median within each
#' panel. Dot = state median of hospital prices; bar = within-state
#' interquartile range; grey tick = the state's median Medicare OPPS
#' payment; hollow dot = fewer than `min_hospitals` hospitals. The 50 states
#' and DC only (territories have one or two hospitals).
state_rank_chart <- function(summary, payer_types = base::c("commercial", "medicaid"), national = NULL) {
  data <- summary |>
    dplyr::filter(.data$payer_type %in% payer_types, !base::is.na(state_map_name(.data$state))) |>
    dplyr::mutate(
      panel = base::factor(payer_label(.data$payer_type), levels = payer_label(payer_types)),
      state_key = base::paste(.data$state, .data$payer_type, sep = "__"),
      reliability = dplyr::if_else(.data$low_n, "Fewer than 5 hospitals", "5 or more hospitals")
    ) |>
    dplyr::arrange(.data$panel, .data$median_price) |>
    dplyr::mutate(state_key = base::factor(.data$state_key, levels = base::unique(.data$state_key)))

  # light bands on every other row so each state can be followed across to its
  # dot; drawn from the same data as the points (a separate band layer would
  # reorder the discrete axis)
  data <- data |>
    dplyr::group_by(.data$panel) |>
    dplyr::mutate(band = base::rank(.data$median_price, ties.method = "first") %% 2L == 0L) |>
    dplyr::ungroup()

  plot <- ggplot2::ggplot(data, ggplot2::aes(y = .data$state_key)) +
    ggplot2::geom_tile(ggplot2::aes(x = 1000, fill = .data$band), width = Inf, height = 1, colour = NA) +
    ggplot2::scale_fill_manual(values = base::c("TRUE" = "grey93", "FALSE" = "white"), guide = "none") +
    ggplot2::geom_segment(ggplot2::aes(x = .data$p25_price, xend = .data$p75_price, yend = .data$state_key),
                          colour = "grey55", linewidth = 0.9) +
    ggplot2::geom_point(ggplot2::aes(x = .data$medicare_opps), shape = 124, size = 3.4, colour = "grey25") +
    ggplot2::geom_point(ggplot2::aes(x = .data$median_price, shape = .data$reliability), size = 2.7,
                        colour = "#b2182b", fill = "#b2182b", stroke = 0.9) +
    ggplot2::scale_shape_manual(values = base::c("5 or more hospitals" = 21, "Fewer than 5 hospitals" = 1), name = NULL) +
    ggplot2::scale_y_discrete(labels = function(x) base::sub("__.*$", "", x), expand = ggplot2::expansion(add = 0.7)) +
    ggplot2::scale_x_continuous(labels = scales::label_dollar(accuracy = 1), transform = "log10",
                                breaks = base::c(250, 500, 1000, 2000, 4000, 8000)) +
    ggplot2::facet_wrap(ggplot2::vars(.data$panel), scales = "free_y", nrow = 1) +
    ggplot2::labs(x = "Negotiated facility rate (log scale)", y = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = "grey85", linewidth = 0.3),
      legend.position = "bottom", legend.text = ggplot2::element_text(size = 10),
      strip.text = ggplot2::element_text(face = "bold", size = 12),
      axis.text.y = ggplot2::element_text(size = 9.5, colour = "grey10", face = "bold"),
      axis.text.x = ggplot2::element_text(size = 9.5, colour = "grey20"),
      panel.spacing.x = ggplot2::unit(1.2, "lines")
    )

  if (!base::is.null(national)) {
    national <- national |>
      dplyr::filter(.data$payer_type %in% payer_types) |>
      dplyr::mutate(panel = base::factor(payer_label(.data$payer_type), levels = payer_label(payer_types)))
    plot <- plot + ggplot2::geom_vline(data = national, ggplot2::aes(xintercept = .data$median_price),
                                       linetype = "dashed", colour = "grey40", linewidth = 0.4)
  }
  plot
}

#' US Census regions (the four the Census Bureau defines), with DC in the South
census_region <- function(abb) {
  regions <- base::list(
    Northeast = base::c("CT", "ME", "MA", "NH", "RI", "VT", "NJ", "NY", "PA"),
    Midwest = base::c("IL", "IN", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE", "ND", "SD"),
    South = base::c("DE", "DC", "FL", "GA", "MD", "NC", "SC", "VA", "WV", "AL", "KY", "MS", "TN", "AR", "LA", "OK", "TX"),
    West = base::c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY", "AK", "CA", "HI", "OR", "WA")
  )
  lookup <- stats::setNames(base::rep(base::names(regions), base::lengths(regions)), base::unlist(regions, use.names = FALSE))
  base::unname(lookup[abb])
}

#' State ranking by Census region: rate relative to Medicare, two payers per row
#'
#' One panel per Census region (2 x 2), so no panel has more than 17 rows and
#' the figure is landscape. Each state is one row carrying both payers: dot =
#' state median of hospital ratios, bar = within-state 25th to 75th
#' percentile, hollow dot = fewer than `min_hospitals` hospitals. The solid
#' line at 1x is the Medicare OPPS payment at each hospital's wage index;
#' dashed lines are the national medians. States are ordered by the first
#' payer's median within each region.
state_region_chart <- function(summary, national, payer_types = base::c("commercial", "medicaid"),
                               colours = base::c(commercial = "#b2182b", medicaid = "#2166ac", medicare_advantage = "#4d4d4d"),
                               limits = base::c(0.1, 8), one_label = "Medicare",
                               breaks = base::c(0.25, 0.5, 1, 2, 4)) {
  data <- summary |>
    dplyr::filter(.data$payer_type %in% payer_types) |>
    dplyr::mutate(region = census_region(.data$state)) |>
    dplyr::filter(!base::is.na(.data$region))
  offsets <- stats::setNames(base::seq(0.17, -0.17, length.out = base::length(payer_types)), payer_types)
  if (base::length(payer_types) == 1L) offsets[] <- 0
  squish <- function(x) base::pmin(base::pmax(x, limits[1]), limits[2])
  national <- national |> dplyr::filter(.data$payer_type %in% payer_types)

  region_panel <- function(region_name) {
    rows <- data |> dplyr::filter(.data$region == region_name)
    order <- rows |>
      dplyr::filter(.data$payer_type == payer_types[1]) |>
      dplyr::arrange(.data$median_ratio) |>
      dplyr::pull("state")
    order <- base::c(base::setdiff(base::unique(rows$state), order), order)
    rows <- rows |>
      dplyr::mutate(
        row = base::match(.data$state, order),
        y = .data$row + offsets[.data$payer_type],
        payer = base::factor(payer_label(.data$payer_type), levels = payer_label(payer_types)),
        reliability = dplyr::if_else(.data$low_n, "Fewer than 5 hospitals", "5 or more hospitals")
      )
    bands <- tibble::tibble(row = base::seq_along(order)) |> dplyr::filter(.data$row %% 2L == 0L)

    ggplot2::ggplot(rows) +
      ggplot2::geom_rect(data = bands, ggplot2::aes(ymin = .data$row - 0.5, ymax = .data$row + 0.5),
                         xmin = -Inf, xmax = Inf, fill = "grey94", colour = NA) +
      ggplot2::geom_vline(xintercept = 1, colour = "grey15", linewidth = 0.6) +
      ggplot2::geom_vline(data = national, ggplot2::aes(xintercept = .data$median_ratio, colour = payer_label(.data$payer_type)),
                          linetype = "dashed", linewidth = 0.45, show.legend = FALSE) +
      ggplot2::geom_segment(ggplot2::aes(x = squish(.data$p25_ratio), xend = squish(.data$p75_ratio),
                                         y = .data$y, yend = .data$y, colour = .data$payer),
                            linewidth = 1.1, alpha = 0.45) +
      ggplot2::geom_point(ggplot2::aes(x = .data$median_ratio, y = .data$y, colour = .data$payer, shape = .data$reliability),
                          size = 2.9, stroke = 1.1, fill = "white") +
      ggplot2::scale_colour_manual(values = stats::setNames(colours[payer_types], payer_label(payer_types)), name = NULL,
                                   breaks = payer_label(payer_types)) +
      ggplot2::scale_shape_manual(values = base::c("5 or more hospitals" = 16, "Fewer than 5 hospitals" = 21), name = NULL) +
      ggplot2::scale_x_continuous(transform = "log", limits = limits, breaks = breaks,
                                  labels = base::ifelse(breaks == 1, one_label, base::paste0(base::format(breaks, trim = TRUE, drop0trailing = TRUE), "x")),
                                  expand = ggplot2::expansion(0)) +
      ggplot2::scale_y_continuous(breaks = base::seq_along(order), labels = order, expand = ggplot2::expansion(add = 0.6)) +
      ggplot2::labs(title = region_name, x = NULL, y = NULL) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_line(colour = "grey85", linewidth = 0.3),
        plot.title = ggplot2::element_text(face = "bold", size = 13),
        axis.text.y = ggplot2::element_text(size = 11, colour = "grey10", face = "bold"),
        axis.text.x = ggplot2::element_text(size = 10.5, colour = "grey20"),
        legend.position = "bottom", legend.text = ggplot2::element_text(size = 11)
      )
  }

  n_rows <- function(region_name) base::length(base::unique(data$state[data$region == region_name]))
  left <- patchwork::wrap_plots(region_panel("Northeast"), region_panel("South"), ncol = 1,
                                heights = base::c(n_rows("Northeast"), n_rows("South")))
  right <- patchwork::wrap_plots(region_panel("Midwest"), region_panel("West"), ncol = 1,
                                 heights = base::c(n_rows("Midwest"), n_rows("West")))
  patchwork::wrap_plots(left, right, ncol = 2, guides = "collect") &
    ggplot2::theme(legend.position = "bottom")
}

#' Caption text shared by the figures (Trilliant attribution is required by
#' its terms of service, 2.2(b))
geo_figure_caption <- function(code, extra = NULL) {
  base::paste(
    base::c(
      extra,
      base::paste0("Ratio = each hospital's negotiated facility rate for CPT ", code,
                   " / the Medicare OPPS payment at that hospital's FY 2026 wage index; state value = median across hospitals."),
      "Data: Trilliant Health Hospital MRF Data Directory (snapshot 2026-07-21); CMS OPPS Addendum B (July 2026); CMS FY 2026 IPPS Tables 2-3."
    ),
    collapse = "\n"
  )
}

#' Write a figure as PNG and PDF
#'
#' One definition: analysis/15 and analysis/16 each carried their own copy,
#' which is how two figure writers drift to different dpi or background.
#'
#' @param dir where the files go; defaults to the pipeline's figure directory,
#'   which is what every caller passed.
save_figure <- function(plot, name, width, height, dir = hpt_path("output", "figures")) {
  for (ext in base::c("png", "pdf")) {
    ggplot2::ggsave(base::file.path(dir, base::paste0(name, ".", ext)), plot,
                    width = width, height = height, dpi = 300, bg = "white")
  }
}
