# =============================================================================
# outputs.R
# =============================================================================
#
# Constants and functions for writing tariff analysis outputs: shock commands,
# CSV files (sector×country, country-level, country×HTS2), summary text files,
# and stacked time-varying outputs.
#
# Also defines shared constants (country codes, partner/sector ordering) used
# across the codebase.
#
# =============================================================================

# -----------------------------------------------------------------------------
# Constants
# -----------------------------------------------------------------------------

# Census country codes
CTY_CANADA <- '1220'
CTY_MEXICO <- '2010'
CTY_CHINA  <- '5700'
USMCA_COUNTRIES <- c(CTY_CANADA, CTY_MEXICO)

# Partner ordering for outputs (different orderings for different outputs)
PARTNER_ORDER_CSV <- c('china', 'canada', 'mexico', 'uk', 'japan', 'eu', 'row', 'ftrow')
PARTNER_ORDER_SHOCKS <- c('China', 'ROW', 'FTROW', 'Canada', 'Mexico', 'Japan', 'EU', 'UK')

# GTAP sector ordering for output CSVs
SECTOR_ORDER <- c(
  'pdr', 'wht', 'gro', 'v_f', 'osd', 'c_b', 'pfb', 'ocr', 'ctl', 'oap',
  'rmk', 'wol', 'frs', 'fsh', 'coa', 'oil', 'gas', 'oxt', 'cmt', 'omt',
  'vol', 'mil', 'pcr', 'sgr', 'ofd', 'b_t', 'tex', 'wap', 'lea', 'lum',
  'ppp', 'p_c', 'chm', 'bph', 'rpp', 'nmm', 'i_s', 'nfm', 'fmp', 'ele',
  'eeq', 'ome', 'mvh', 'otn', 'omf', 'ely', 'gdt', 'wtr', 'cns'
)


#' Build output file path, creating directory if needed
#'
#' @param output_file Base filename
#' @param scenario Scenario name (optional)
#' @param output_base Base output directory (default: 'output')
#'
#' @return Full path to output file
get_output_path <- function(output_file, scenario = NULL, output_base = 'output') {
  if (is.null(scenario)) return(output_file)
  output_dir <- file.path(output_base, scenario)
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  file.path(output_dir, output_file)
}


#' Format summary table (deltas or levels) as vector of lines
#'
#' @param data Data frame with partner and gtap/census value columns
#' @param gtap_total Total GTAP-weighted value
#' @param census_total Total Census-weighted value (optional)
#' @param gtap_col Name of the GTAP value column in data
#' @param census_col Name of the Census value column in data
#' @param header First line of the table (title)
#'
#' @return Character vector of formatted lines
format_summary_table <- function(data, gtap_total, census_total = NULL,
                                 gtap_col, census_col, header) {
  has_census <- !is.null(census_total) && !is.na(census_total)

  lines <- c(
    header,
    '=================================================================',
    ''
  )

  if (has_census) {
    lines <- c(lines,
      sprintf('%-10s  %15s  %18s', '', 'GTAP Weights', '2024 Census Weights'),
      sprintf('%-10s  %15s  %18s', 'Country', '', ''),
      sprintf('%-10s  %15s  %18s', '-------', '------------', '-----------------')
    )
    for (i in 1:nrow(data)) {
      lines <- c(lines, sprintf('%-10s  %14.2f%%  %17.2f%%',
        toupper(data$partner[i]),
        data[[gtap_col]][i] * 100,
        data[[census_col]][i] * 100))
    }
    lines <- c(lines, '', sprintf('%-10s  %14.2f%%  %17.2f%%',
      'TOTAL', gtap_total * 100, census_total * 100))
  } else {
    lines <- c(lines,
      sprintf('%-10s  %15s', '', 'GTAP Weights'),
      sprintf('%-10s  %15s', 'Country', ''),
      sprintf('%-10s  %15s', '-------', '------------')
    )
    for (i in 1:nrow(data)) {
      lines <- c(lines, sprintf('%-10s  %14.2f%%',
        toupper(data$partner[i]),
        data[[gtap_col]][i] * 100))
    }
    lines <- c(lines, '', sprintf('%-10s  %14.2f%%', 'TOTAL', gtap_total * 100))
  }

  lines
}

format_delta_table <- function(country_etrs, gtap_total, census_total = NULL) {
  format_summary_table(
    country_etrs, gtap_total, census_total,
    gtap_col = 'gtap_etr', census_col = 'census_etr',
    header = 'Overall Tariff Deltas by Country (change from baseline scenario):'
  )
}


#' Format coverage table as vector of lines
#'
#' @param coverage_by_partner Data frame with partner and share columns
#' @param coverage_total Data frame with total shares
#'
#' @return Character vector of formatted lines
format_coverage_table <- function(coverage_by_partner, coverage_total) {
  lines <- c(
    '',
    'Tariff Coverage by Country (fraction of 2024 import value):',
    '==========================================================',
    sprintf('%-10s  %12s  %12s  %12s', 'Country', 'Under 232', 'Under non-232', 'Neither'),
    sprintf('%-10s  %12s  %12s  %12s', '-------', '---------', '-----------', '-------')
  )

  for (i in 1:nrow(coverage_by_partner)) {
    lines <- c(lines, sprintf('%-10s  %11.1f%%  %11.1f%%  %11.1f%%',
      toupper(coverage_by_partner$partner[i]),
      coverage_by_partner$share_s232[i] * 100,
      coverage_by_partner$share_ieepa[i] * 100,
      coverage_by_partner$share_neither[i] * 100))
  }

  lines <- c(lines, '', sprintf('%-10s  %11.1f%%  %11.1f%%  %11.1f%%',
    'TOTAL',
    coverage_total$share_s232 * 100,
    coverage_total$share_ieepa * 100,
    coverage_total$share_neither * 100))

  lines
}


#' Write shock commands to txt file for downstream model
#'
#' @param etr_data Data frame with columns: partner, gtap_code, etr
#' @param output_file Path to output file
#' @param scenario Scenario name for output directory
#' @param output_base Base output directory (default: 'output')
#'
#' @return Writes file and returns invisibly
write_shock_commands <- function(etr_data, output_file = 'shocks.txt', scenario,
                                 output_base = 'output') {

  output_path <- get_output_path(output_file, scenario, output_base)

  # Map partner names to proper format
  partner_map <- c(
    'china'  = 'China',
    'canada' = 'Canada',
    'mexico' = 'Mexico',
    'row'    = 'ROW',
    'ftrow'  = 'FTROW',
    'japan'  = 'Japan',
    'eu'     = 'EU',
    'uk'     = 'UK'
  )

  # Prepare data
  shock_commands <- etr_data %>%
    mutate(
      partner_fmt = partner_map[partner],
      etr_pct = round(etr * 100, 1)
    ) %>%
    filter(etr_pct != 0, !is.na(gtap_code)) %>%
    arrange(match(partner_fmt, PARTNER_ORDER_SHOCKS), gtap_code) %>%
    mutate(
      command = sprintf('Shock tms("%s","%s","USA") = %.1f;', gtap_code, partner_fmt, etr_pct)
    )

  # Write to file with blank lines between partners
  con <- file(output_path, 'w')

  for (p in PARTNER_ORDER_SHOCKS) {
    partner_commands <- shock_commands %>% filter(partner_fmt == p)

    if (nrow(partner_commands) > 0) {
      writeLines(partner_commands$command, con)
      writeLines('', con)  # Blank line after each partner
    }
  }

  close(con)

  message(sprintf('Wrote %d shock commands to %s', nrow(shock_commands), output_path))
  invisible(shock_commands)
}


#' Prepare sector × country delta data in wide format (percentage points)
#'
#' @param etr_data Data frame with columns: partner, gtap_code, etr
#'
#' @return Tibble with gtap_code column and one column per partner (in pp)
prepare_sector_country_wide <- function(etr_data, value_col) {

  wide_data <- etr_data %>%
    select(partner, gtap_code, value = all_of(value_col)) %>%
    pivot_wider(names_from = partner, values_from = value, values_fill = 0) %>%
    select(gtap_code, any_of(PARTNER_ORDER_CSV)) %>%
    mutate(across(-gtap_code, ~ .x * 100))

  existing_sectors <- intersect(SECTOR_ORDER, wide_data$gtap_code)

  wide_data %>%
    filter(gtap_code %in% existing_sectors) %>%
    arrange(match(gtap_code, SECTOR_ORDER))
}

prepare_sector_country_deltas <- function(etr_data) {
  prepare_sector_country_wide(etr_data, value_col = 'etr')
}


write_sector_country_wide <- function(etr_data, value_col,
                                      output_file,
                                      scenario = NULL,
                                      output_base = 'output',
                                      label = 'sector and country output') {

  output_path <- get_output_path(output_file, scenario, output_base)
  wide_data <- prepare_sector_country_wide(etr_data, value_col = value_col)
  write_csv(wide_data, output_path)
  message(sprintf('Wrote %s to %s (in pp units)', label, output_path))
  invisible(wide_data)
}


#' Write deltas to CSV in sector (rows) x country (columns) format
#'
#' Values are written in percentage points (pp), i.e., multiplied by 100.
#'
#' @param etr_data Data frame with columns: partner, gtap_code, etr
#' @param output_file Path to output file (default: 'gtap_deltas_by_sector_country.csv')
#' @param scenario Scenario name for output directory
#' @param output_base Base output directory (default: 'output')
#'
#' @return Writes CSV file (in pp units) and returns invisibly
write_sector_country_deltas <- function(etr_data,
                                         output_file = 'gtap_deltas_by_sector_country.csv',
                                         scenario = NULL,
                                         output_base = 'output') {

  write_sector_country_wide(
    etr_data    = etr_data,
    value_col   = 'etr',
    output_file = output_file,
    scenario    = scenario,
    output_base = output_base,
    label       = 'deltas by sector and country'
  )
}


#' Prepare country-level data (census country codes with weighted average)
#'
#' @param hs10_country_etrs Data frame with columns: hs10, cty_code, gtap_code, imports, and value_col
#' @param value_col Name of the value column to aggregate (e.g., 'etr' or 'level')
#'
#' @return Tibble with cty_code, country_name, value (in pp)
prepare_country_level_data <- function(hs10_country_etrs, value_col) {
  census_codes <- load_census_codes()

  hs10_country_etrs %>%
    group_by(cty_code) %>%
    summarise(
      total_imports = sum(imports),
      weighted_val = sum(.data[[value_col]] * imports),
      .groups = 'drop'
    ) %>%
    mutate(
      !!value_col := if_else(total_imports > 0, weighted_val / total_imports, 0) * 100
    ) %>%
    left_join(census_codes, by = 'cty_code') %>%
    select(cty_code, country_name, all_of(value_col)) %>%
    arrange(desc(.data[[value_col]]))
}

prepare_country_level_deltas <- function(hs10_country_etrs) {
  prepare_country_level_data(hs10_country_etrs, value_col = 'etr')
}

prepare_country_level_levels <- function(hs10_country_etrs) {
  prepare_country_level_data(hs10_country_etrs, value_col = 'level')
}


#' Write country-level deltas to CSV (census country codes with overall deltas)
#'
#' @param hs10_country_etrs Data frame with columns: hs10, cty_code, gtap_code, imports, etr
#' @param output_file Path to output file (default: 'deltas_by_census_country.csv')
#' @param scenario Scenario name for output directory
#' @param output_base Base output directory (default: 'output')
#'
#' @return Writes CSV file (in pp units) and returns invisibly
write_country_level_deltas <- function(hs10_country_etrs,
                                        output_file = 'deltas_by_census_country.csv',
                                        scenario = NULL,
                                        output_base = 'output') {

  output_path <- get_output_path(output_file, scenario, output_base)
  country_deltas <- prepare_country_level_deltas(hs10_country_etrs)
  write_csv(country_deltas, output_path)
  message(sprintf('Wrote country-level deltas to %s (in pp units)', output_path))
  invisible(country_deltas)
}


#' Write country-level tariff levels to CSV (census country codes)
#'
#' @param hs10_country_etrs Data frame with columns: hs10, cty_code, gtap_code, imports, level
#' @param output_file Path to output file (default: 'levels_by_census_country.csv')
#' @param scenario Scenario name for output directory
#' @param output_base Base output directory (default: 'output')
#'
#' @return Writes CSV file (in pp units) and returns invisibly
write_country_level_levels <- function(hs10_country_etrs,
                                       output_file = 'levels_by_census_country.csv',
                                       scenario = NULL,
                                       output_base = 'output') {

  output_path <- get_output_path(output_file, scenario, output_base)
  country_levels <- prepare_country_level_levels(hs10_country_etrs)
  write_csv(country_levels, output_path)
  message(sprintf('Wrote country-level tariff levels to %s (in pp units)', output_path))
  invisible(country_levels)
}


#' Prepare country × HTS2 data in wide format (percentage points)
#'
#' @param hs10_country_etrs Data frame with columns: hs10, cty_code, gtap_code, imports, and value_col
#' @param value_col Name of the value column to aggregate (e.g., 'etr' or 'level')
#'
#' @return Tibble with cty_code, country_name, and one column per HTS chapter (in pp)
prepare_country_hts2_data <- function(hs10_country_etrs, value_col) {
  census_codes <- load_census_codes()

  country_hts2 <- hs10_country_etrs %>%
    mutate(hts2 = str_sub(hs10, 1, 2)) %>%
    group_by(cty_code, hts2) %>%
    summarise(
      total_imports = sum(imports),
      weighted_val = sum(.data[[value_col]] * imports),
      .groups = 'drop'
    ) %>%
    mutate(
      value = if_else(total_imports > 0, weighted_val / total_imports, 0) * 100
    ) %>%
    select(cty_code, hts2, value)

  country_hts2_wide <- country_hts2 %>%
    pivot_wider(names_from = hts2, values_from = value, values_fill = 0) %>%
    left_join(census_codes, by = 'cty_code') %>%
    relocate(cty_code, country_name)

  hts2_cols <- setdiff(names(country_hts2_wide), c('cty_code', 'country_name'))
  hts2_cols_sorted <- hts2_cols[order(as.numeric(hts2_cols))]
  country_hts2_wide %>%
    select(cty_code, country_name, all_of(hts2_cols_sorted))
}

prepare_country_hts2_deltas <- function(hs10_country_etrs) {
  prepare_country_hts2_data(hs10_country_etrs, value_col = 'etr')
}

prepare_country_hts2_levels <- function(hs10_country_etrs) {
  prepare_country_hts2_data(hs10_country_etrs, value_col = 'level')
}


#' Write country-level deltas by 2-digit HTS code to CSV
#'
#' @param hs10_country_etrs Data frame with columns: hs10, cty_code, gtap_code, imports, etr
#' @param output_file Path to output file (default: 'deltas_by_census_country_hts2.csv')
#' @param scenario Scenario name for output directory
#' @param output_base Base output directory (default: 'output')
#'
#' @return Writes CSV file (in pp units) and returns invisibly
write_country_hts2_deltas <- function(hs10_country_etrs,
                                       output_file = 'deltas_by_census_country_hts2.csv',
                                       scenario = NULL,
                                       output_base = 'output') {

  output_path <- get_output_path(output_file, scenario, output_base)
  country_hts2_wide <- prepare_country_hts2_deltas(hs10_country_etrs)
  write_csv(country_hts2_wide, output_path)
  message(sprintf('Wrote country × HTS2 deltas to %s (in pp units)', output_path))
  invisible(country_hts2_wide)
}


#' Write country-level tariff levels by 2-digit HTS code to CSV
#'
#' @param hs10_country_etrs Data frame with columns: hs10, cty_code, gtap_code, imports, level
#' @param output_file Path to output file (default: 'levels_by_census_country_hts2.csv')
#' @param scenario Scenario name for output directory
#' @param output_base Base output directory (default: 'output')
#'
#' @return Writes CSV file (in pp units) and returns invisibly
write_country_hts2_levels <- function(hs10_country_etrs,
                                      output_file = 'levels_by_census_country_hts2.csv',
                                      scenario = NULL,
                                      output_base = 'output') {

  output_path <- get_output_path(output_file, scenario, output_base)
  country_hts2_wide <- prepare_country_hts2_levels(hs10_country_etrs)
  write_csv(country_hts2_wide, output_path)
  message(sprintf('Wrote country × HTS2 tariff levels to %s (in pp units)', output_path))
  invisible(country_hts2_wide)
}


#' Compute weighted summary data for a value column (shared helper for deltas and levels)
#'
#' Calculates per-country and total weighted averages using both GTAP and Census weights.
#'
#' @param etr_data Data frame with columns: partner, gtap_code, and the value column
#' @param value_col Name of the value column to weight (e.g., 'etr' or 'level')
#' @param gtap_col Name for the GTAP-weighted result column (e.g., 'gtap_etr' or 'gtap_level')
#' @param census_col Name for the Census-weighted result column (e.g., 'census_etr' or 'census_level')
#' @param hs10_country_etrs Data frame with hs10-level data for Census weights (optional)
#' @param country_mapping Data frame with cty_code → partner mapping (optional)
#' @param weights_file Path to GTAP import weights CSV file
#'
#' @return Named list: by_country (tibble), gtap_total (numeric), census_total (numeric)
calc_weighted_summary <- function(etr_data, value_col, gtap_col, census_col,
                                  hs10_country_etrs = NULL,
                                  country_mapping = NULL,
                                  weights_file = 'resources/gtap_import_weights.csv') {

  gtap_weights <- read_csv(weights_file, show_col_types = FALSE)

  gtap_weights_long <- gtap_weights %>%
    pivot_longer(cols = -gtap_code, names_to = 'partner', values_to = 'import_weight') %>%
    filter(import_weight > 0)

  gtap_weighted_data <- etr_data %>%
    inner_join(gtap_weights_long, by = c('partner', 'gtap_code')) %>%
    mutate(weighted_val = .data[[value_col]] * import_weight)

  gtap_country <- gtap_weighted_data %>%
    group_by(partner) %>%
    summarise(
      !!gtap_col := sum(weighted_val) / sum(import_weight),
      .groups = 'drop'
    )

  gtap_total <- gtap_weighted_data %>%
    summarise(val = sum(weighted_val) / sum(import_weight)) %>%
    pull(val)

  if (!is.null(hs10_country_etrs) && !is.null(country_mapping)) {
    census_weights <- hs10_country_etrs %>%
      left_join(
        country_mapping %>% select(cty_code, partner) %>% distinct(),
        by = 'cty_code'
      ) %>%
      mutate(partner = if_else(is.na(partner), 'row', partner)) %>%
      group_by(partner, gtap_code) %>%
      summarise(import_weight = sum(imports), .groups = 'drop') %>%
      filter(import_weight > 0)

    census_weighted_data <- etr_data %>%
      inner_join(census_weights, by = c('partner', 'gtap_code')) %>%
      mutate(weighted_val = .data[[value_col]] * import_weight)

    census_country <- census_weighted_data %>%
      group_by(partner) %>%
      summarise(
        !!census_col := sum(weighted_val) / sum(import_weight),
        .groups = 'drop'
      )

    census_total <- census_weighted_data %>%
      summarise(val = sum(weighted_val) / sum(import_weight)) %>%
      pull(val)

    by_country <- gtap_country %>%
      left_join(census_country, by = 'partner') %>%
      arrange(desc(.data[[gtap_col]]))
  } else {
    by_country <- gtap_country %>%
      mutate(!!census_col := NA) %>%
      arrange(desc(.data[[gtap_col]]))
    census_total <- NA
  }

  list(
    by_country   = by_country,
    gtap_total   = gtap_total,
    census_total = census_total
  )
}


#' Compute overall delta data (no I/O)
#'
#' Calculates country deltas using GTAP and Census weights plus tariff coverage.
#' Returns all computed data as a list, ready for formatting/writing.
#'
#' @param etr_data Data frame with columns: partner, gtap_code, etr, base_s232, base_ieepa, base_neither
#' @param hs10_country_etrs Data frame with columns: hs10, cty_code, gtap_code, imports, etr (for Census weights)
#' @param country_mapping Data frame with columns: cty_code, partner
#' @param weights_file Path to GTAP import weights CSV file
#'
#' @return Named list: by_country (tibble), gtap_total (numeric), census_total (numeric),
#'   coverage_stats (list or NULL), delta_lines (character), coverage_lines (character)
calc_overall_deltas_data <- function(etr_data, hs10_country_etrs = NULL,
                                   country_mapping = NULL,
                                   weights_file = 'resources/gtap_import_weights.csv') {

  result <- calc_weighted_summary(
    etr_data, value_col = 'etr',
    gtap_col = 'gtap_etr', census_col = 'census_etr',
    hs10_country_etrs = hs10_country_etrs,
    country_mapping = country_mapping,
    weights_file = weights_file
  )

  # Calculate tariff coverage
  coverage_stats <- NULL
  if ('base_s232' %in% names(etr_data)) {
    coverage_by_partner <- etr_data %>%
      group_by(partner) %>%
      summarise(
        imports_s232 = sum(base_s232),
        imports_ieepa = sum(base_ieepa),
        imports_neither = sum(base_neither),
        .groups = 'drop'
      ) %>%
      mutate(
        total_imports = imports_s232 + imports_ieepa + imports_neither,
        share_s232 = imports_s232 / total_imports,
        share_ieepa = imports_ieepa / total_imports,
        share_neither = imports_neither / total_imports
      )

    coverage_total <- etr_data %>%
      summarise(
        imports_s232 = sum(base_s232),
        imports_ieepa = sum(base_ieepa),
        imports_neither = sum(base_neither)
      ) %>%
      mutate(
        total_imports = imports_s232 + imports_ieepa + imports_neither,
        share_s232 = imports_s232 / total_imports,
        share_ieepa = imports_ieepa / total_imports,
        share_neither = imports_neither / total_imports
      )

    coverage_stats <- list(by_partner = coverage_by_partner, total = coverage_total)
  }

  # Build formatted lines
  delta_lines <- format_delta_table(result$by_country, result$gtap_total, result$census_total)
  coverage_lines <- if (!is.null(coverage_stats)) {
    format_coverage_table(coverage_stats$by_partner, coverage_stats$total)
  } else {
    character(0)
  }

  list(
    by_country     = result$by_country,
    gtap_total     = result$gtap_total,
    census_total   = result$census_total,
    coverage_stats = coverage_stats,
    delta_lines    = delta_lines,
    coverage_lines = coverage_lines
  )
}


#' Calculate and print overall tariff deltas by country and total using both GTAP and 2024 Census weights
#'
#' Calculates and prints tariff deltas (change from baseline scenario).
#'
#' @param etr_data Data frame with columns: partner, gtap_code, etr, base_s232, base_ieepa, base_neither
#' @param hs10_country_etrs Data frame with columns: hs10, cty_code, gtap_code, imports, etr (for Census weights)
#' @param country_mapping Data frame with columns: cty_code, partner (for aggregating country data to partners)
#' @param weights_file Path to GTAP import weights CSV file
#' @param output_file Path to output text file (default: 'overall_deltas.txt')
#' @param scenario Scenario name for output directory
#' @param output_base Base output directory (default: 'output')
#'
#' @return Prints overall deltas and returns them invisibly
calc_overall_deltas <- function(etr_data, hs10_country_etrs = NULL,
                                country_mapping = NULL,
                                weights_file = 'resources/gtap_import_weights.csv',
                                output_file = 'overall_deltas.txt',
                                scenario = NULL,
                                output_base = 'output') {

  result <- calc_overall_deltas_data(etr_data, hs10_country_etrs, country_mapping, weights_file)

  # Print to console
  cat('\n')
  cat(paste(result$delta_lines, collapse = '\n'))
  cat('\n\n')
  if (length(result$coverage_lines) > 0) {
    cat(paste(result$coverage_lines, collapse = '\n'))
    cat('\n')
  }

  # Write to file
  output_path <- get_output_path(output_file, scenario, output_base)
  writeLines(c(result$delta_lines, '', result$coverage_lines), output_path)

  message(sprintf('Wrote overall deltas to %s', output_path))

  invisible(list(
    by_country = result$by_country,
    gtap_total = result$gtap_total,
    census_total = result$census_total
  ))
}


# =============================================================================
# Stacked output writers for time-varying scenarios
# =============================================================================

#' Write stacked sector × country delta CSV with date column
#'
#' @param all_partner_etrs Named list (date → partner_etrs tibble)
#' @param output_file Output filename
#' @param scenario Scenario name
#' @param output_base Base output directory
write_sector_country_deltas_stacked <- function(all_partner_etrs,
                                                 output_file = 'gtap_deltas_by_sector_country.csv',
                                                 scenario = NULL,
                                                 output_base = 'output',
                                                 date_intervals = NULL) {

  write_sector_country_wide_stacked(
    all_partner_etrs = all_partner_etrs,
    value_col        = 'etr',
    output_file      = output_file,
    scenario         = scenario,
    output_base      = output_base,
    label            = 'sector x country deltas',
    date_intervals   = date_intervals
  )
}


#' Write stacked country-level delta CSV with date column
#'
#' @param all_hs10_country_etrs Named list (date → hs10_country_etrs tibble)
#' @param output_file Output filename
#' @param scenario Scenario name
#' @param output_base Base output directory
write_country_level_deltas_stacked <- function(all_hs10_country_etrs,
                                                output_file = 'deltas_by_census_country.csv',
                                                scenario = NULL,
                                                output_base = 'output',
                                                date_intervals = NULL) {

  output_path <- get_output_path(output_file, scenario, output_base)

  stacked <- names(all_hs10_country_etrs) %>%
    map_df(function(date_str) {
      prepare_country_level_deltas(all_hs10_country_etrs[[date_str]]) %>%
        mutate(date = date_str, .before = 1)
    })

  if (!is.null(date_intervals)) {
    stacked <- stacked %>%
      left_join(date_intervals, by = 'date') %>%
      relocate(date, valid_from, valid_until)
  }

  write_csv(stacked, output_path)
  message(sprintf('Wrote stacked country-level deltas to %s (in pp units)', output_path))
  invisible(stacked)
}


#' Write stacked country-level tariff level CSV with date column
#'
#' @param all_hs10_country_etrs Named list (date → hs10_country_etrs tibble)
#' @param output_file Output filename
#' @param scenario Scenario name
#' @param output_base Base output directory
write_country_level_levels_stacked <- function(all_hs10_country_etrs,
                                                output_file = 'levels_by_census_country.csv',
                                                scenario = NULL,
                                                output_base = 'output',
                                                date_intervals = NULL) {

  output_path <- get_output_path(output_file, scenario, output_base)

  stacked <- names(all_hs10_country_etrs) %>%
    map_df(function(date_str) {
      prepare_country_level_levels(all_hs10_country_etrs[[date_str]]) %>%
        mutate(date = date_str, .before = 1)
    })

  if (!is.null(date_intervals)) {
    stacked <- stacked %>%
      left_join(date_intervals, by = 'date') %>%
      relocate(date, valid_from, valid_until)
  }

  write_csv(stacked, output_path)
  message(sprintf('Wrote stacked country-level tariff levels to %s (in pp units)', output_path))
  invisible(stacked)
}


#' Write stacked country × HTS2 delta CSV with date column
#'
#' @param all_hs10_country_etrs Named list (date → hs10_country_etrs tibble)
#' @param output_file Output filename
#' @param scenario Scenario name
#' @param output_base Base output directory
write_country_hts2_deltas_stacked <- function(all_hs10_country_etrs,
                                               output_file = 'deltas_by_census_country_hts2.csv',
                                               scenario = NULL,
                                               output_base = 'output',
                                               date_intervals = NULL) {

  output_path <- get_output_path(output_file, scenario, output_base)

  stacked <- names(all_hs10_country_etrs) %>%
    map_df(function(date_str) {
      prepare_country_hts2_deltas(all_hs10_country_etrs[[date_str]]) %>%
        mutate(date = date_str, .before = 1)
    })

  if (!is.null(date_intervals)) {
    stacked <- stacked %>%
      left_join(date_intervals, by = 'date') %>%
      relocate(date, valid_from, valid_until)
  }

  write_csv(stacked, output_path)
  message(sprintf('Wrote stacked country × HTS2 deltas to %s (in pp units)', output_path))
  invisible(stacked)
}


#' Write stacked country × HTS2 tariff level CSV with date column
#'
#' @param all_hs10_country_etrs Named list (date → hs10_country_etrs tibble)
#' @param output_file Output filename
#' @param scenario Scenario name
#' @param output_base Base output directory
#' @param date_intervals Optional tibble with date, valid_from, valid_until
write_country_hts2_levels_stacked <- function(all_hs10_country_etrs,
                                               output_file = 'levels_by_census_country_hts2.csv',
                                               scenario = NULL,
                                               output_base = 'output',
                                               date_intervals = NULL) {

  output_path <- get_output_path(output_file, scenario, output_base)

  stacked <- names(all_hs10_country_etrs) %>%
    map_df(function(date_str) {
      prepare_country_hts2_levels(all_hs10_country_etrs[[date_str]]) %>%
        mutate(date = date_str, .before = 1)
    })

  if (!is.null(date_intervals)) {
    stacked <- stacked %>%
      left_join(date_intervals, by = 'date') %>%
      relocate(date, valid_from, valid_until)
  }

  write_csv(stacked, output_path)
  message(sprintf('Wrote stacked country × HTS2 tariff levels to %s (in pp units)', output_path))
  invisible(stacked)
}


#' Write combined overall deltas text file for time-varying scenarios
#'
#' Each date gets its own section with delta and coverage tables.
#'
#' @param all_overall_data Named list (date → result from calc_overall_deltas_data())
#' @param output_file Output filename
#' @param scenario Scenario name
#' @param output_base Base output directory
write_overall_deltas_combined <- function(all_overall_data,
                                           output_file = 'overall_deltas.txt',
                                           scenario = NULL,
                                           output_base = 'output') {

  output_path <- get_output_path(output_file, scenario, output_base)

  all_lines <- character(0)

  for (date_str in names(all_overall_data)) {
    data <- all_overall_data[[date_str]]

    section_header <- c(
      sprintf('Date: %s', date_str),
      paste(rep('=', 65), collapse = ''),
      ''
    )

    all_lines <- c(all_lines, section_header, data$delta_lines, '', data$coverage_lines, '', '')

    # Print to console
    cat('\n')
    cat(paste(c(section_header, data$delta_lines), collapse = '\n'))
    cat('\n\n')
    if (length(data$coverage_lines) > 0) {
      cat(paste(data$coverage_lines, collapse = '\n'))
      cat('\n')
    }
  }

  writeLines(all_lines, output_path)
  message(sprintf('Wrote combined overall deltas to %s', output_path))
  invisible(all_overall_data)
}


# =============================================================================
# Level (MFN + delta) output functions
# =============================================================================

#' Prepare sector × country tariff level data in wide format (percentage points)
#'
#' @param etr_data Data frame with columns: partner, gtap_code, level
#'
#' @return Tibble with gtap_code column and one column per partner (in pp)
prepare_levels_by_sector_country <- function(etr_data) {
  prepare_sector_country_wide(etr_data, value_col = 'level')
}


#' Write tariff levels to CSV in sector (rows) x country (columns) format
#'
#' Values are written in percentage points (pp), i.e., multiplied by 100.
#'
#' @param etr_data Data frame with columns: partner, gtap_code, level
#' @param output_file Path to output file
#' @param scenario Scenario name for output directory
#' @param output_base Base output directory (default: 'output')
#'
#' @return Writes CSV file (in pp units) and returns invisibly
write_levels_by_sector_country <- function(etr_data,
                                            output_file = 'gtap_levels_by_sector_country.csv',
                                            scenario = NULL,
                                            output_base = 'output') {

  write_sector_country_wide(
    etr_data    = etr_data,
    value_col   = 'level',
    output_file = output_file,
    scenario    = scenario,
    output_base = output_base,
    label       = 'tariff levels by sector and country'
  )
}


#' Compute overall tariff level data (no I/O)
#'
#' Calculates country-level total tariff rates (MFN + delta) using GTAP and Census weights.
#'
#' @param etr_data Data frame with columns: partner, gtap_code, level
#' @param hs10_country_etrs Data frame with columns: hs10, cty_code, gtap_code, imports, level (for Census weights)
#' @param country_mapping Data frame with columns: cty_code, partner
#' @param weights_file Path to GTAP import weights CSV file
#'
#' @return Named list: by_country (tibble), gtap_total (numeric), census_total (numeric), level_lines (character)
calc_overall_levels_data <- function(etr_data, hs10_country_etrs = NULL,
                                     country_mapping = NULL,
                                     weights_file = 'resources/gtap_import_weights.csv') {

  result <- calc_weighted_summary(
    etr_data, value_col = 'level',
    gtap_col = 'gtap_level', census_col = 'census_level',
    hs10_country_etrs = hs10_country_etrs,
    country_mapping = country_mapping,
    weights_file = weights_file
  )

  level_lines <- format_level_table(result$by_country, result$gtap_total, result$census_total)

  list(
    by_country   = result$by_country,
    gtap_total   = result$gtap_total,
    census_total = result$census_total,
    level_lines  = level_lines
  )
}


format_level_table <- function(country_levels, gtap_total, census_total = NULL) {
  format_summary_table(
    country_levels, gtap_total, census_total,
    gtap_col = 'gtap_level', census_col = 'census_level',
    header = 'Overall Tariff Levels by Country (MFN baseline + policy tariffs):'
  )
}


#' Write overall tariff levels to text file
#'
#' @param etr_data Data frame with columns: partner, gtap_code, level
#' @param hs10_country_etrs Data frame with columns: hs10, cty_code, gtap_code, imports, level
#' @param country_mapping Data frame with columns: cty_code, partner
#' @param weights_file Path to GTAP import weights CSV file
#' @param output_file Path to output text file
#' @param scenario Scenario name for output directory
#' @param output_base Base output directory (default: 'output')
#'
#' @return Prints overall levels and returns them invisibly
write_overall_levels <- function(etr_data, hs10_country_etrs = NULL,
                                  country_mapping = NULL,
                                  weights_file = 'resources/gtap_import_weights.csv',
                                  output_file = 'overall_levels.txt',
                                  scenario = NULL,
                                  output_base = 'output') {

  result <- calc_overall_levels_data(etr_data, hs10_country_etrs, country_mapping, weights_file)

  # Print to console
  cat('\n')
  cat(paste(result$level_lines, collapse = '\n'))
  cat('\n\n')

  # Write to file
  output_path <- get_output_path(output_file, scenario, output_base)
  writeLines(result$level_lines, output_path)

  message(sprintf('Wrote overall tariff levels to %s', output_path))

  invisible(list(
    by_country = result$by_country,
    gtap_total = result$gtap_total,
    census_total = result$census_total
  ))
}


#' Write stacked sector × country tariff level CSV with date column
#'
#' @param all_partner_etrs Named list (date → partner_etrs tibble)
#' @param output_file Output filename
#' @param scenario Scenario name
#' @param output_base Base output directory
write_sector_country_wide_stacked <- function(all_partner_etrs,
                                              value_col,
                                              output_file,
                                              scenario = NULL,
                                              output_base = 'output',
                                              label = 'sector x country output',
                                              date_intervals = NULL) {

  output_path <- get_output_path(output_file, scenario, output_base)

  stacked <- names(all_partner_etrs) %>%
    map_df(function(date_str) {
      prepare_sector_country_wide(all_partner_etrs[[date_str]], value_col = value_col) %>%
        mutate(date = date_str, .before = 1)
    })

  if (!is.null(date_intervals)) {
    stacked <- stacked %>%
      left_join(date_intervals, by = 'date') %>%
      relocate(date, valid_from, valid_until)
  }

  write_csv(stacked, output_path)
  message(sprintf('Wrote stacked %s to %s (in pp units)', label, output_path))
  invisible(stacked)
}

write_levels_by_sector_country_stacked <- function(all_partner_etrs,
                                                    output_file = 'gtap_levels_by_sector_country.csv',
                                                    scenario = NULL,
                                                    output_base = 'output',
                                                    date_intervals = NULL) {

  write_sector_country_wide_stacked(
    all_partner_etrs = all_partner_etrs,
    value_col        = 'level',
    output_file      = output_file,
    scenario         = scenario,
    output_base      = output_base,
    label            = 'sector x country tariff levels',
    date_intervals   = date_intervals
  )
}


#' Write combined overall tariff levels text file for time-varying scenarios
#'
#' Each date gets its own section with level tables.
#'
#' @param all_levels_data Named list (date → result from calc_overall_levels_data())
#' @param output_file Output filename
#' @param scenario Scenario name
#' @param output_base Base output directory
write_overall_levels_combined <- function(all_levels_data,
                                           output_file = 'overall_levels.txt',
                                           scenario = NULL,
                                           output_base = 'output') {

  output_path <- get_output_path(output_file, scenario, output_base)

  all_lines <- character(0)

  for (date_str in names(all_levels_data)) {
    data <- all_levels_data[[date_str]]

    section_header <- c(
      sprintf('Date: %s', date_str),
      paste(rep('=', 65), collapse = ''),
      ''
    )

    all_lines <- c(all_lines, section_header, data$level_lines, '', '')

    # Print to console
    cat('\n')
    cat(paste(c(section_header, data$level_lines), collapse = '\n'))
    cat('\n\n')
  }

  writeLines(all_lines, output_path)
  message(sprintf('Wrote combined overall tariff levels to %s', output_path))
  invisible(all_levels_data)
}


# =============================================================================
# BEA Commodity-Level ETR Deltas (for Boston Fed I-O Price Model)
# =============================================================================

#' Write BEA commodity-level ETR deltas
#'
#' Aggregates HS10-level tariff deltas to BEA Summary commodity level using
#' the HS10→NAICS→BEA crosswalk chain. Output is consumed by the Tariff-Model
#' Boston Fed I-O price model (io_price_model_v2.R).
#'
#' @param hs10_country_etrs Tibble with hs10, cty_code, imports, etr columns
#' @param output_file Output filename (default: 'bea_deltas_by_commodity.csv')
#' @param scenario Scenario name for output directory
#' @param output_base Base output directory (default: 'output')
#'
#' @return Writes CSV and returns invisibly
write_bea_commodity_deltas <- function(hs10_country_etrs,
                                        output_file = 'bea_deltas_by_commodity.csv',
                                        scenario = NULL,
                                        output_base = 'output') {

  # Load HS10→BEA crosswalk (built by scripts/build_hs10_bea_crosswalk.R)
  crosswalk_path <- 'resources/hs10_bea_crosswalk.csv'
  if (!file.exists(crosswalk_path)) {
    message('  HS10→BEA crosswalk not found, skipping BEA delta output')
    return(invisible(NULL))
  }
  hs10_bea <- read_csv(crosswalk_path, show_col_types = FALSE,
                        col_types = cols(hs10 = col_character(), bea_code = col_character()))

  # Join HS10 ETRs with BEA codes
  hs10_with_bea <- hs10_country_etrs %>%
    left_join(hs10_bea, by = 'hs10')

  # Coverage diagnostic
  total_imports <- sum(hs10_with_bea$imports)
  matched_imports <- sum(hs10_with_bea$imports[!is.na(hs10_with_bea$bea_code)])
  message(sprintf('  BEA crosswalk coverage: %.1f%% of import value',
                  100 * matched_imports / total_imports))

  # Aggregate to BEA commodity level: import-weighted average ETR delta
  bea_deltas <- hs10_with_bea %>%
    filter(!is.na(bea_code)) %>%
    group_by(bea_code) %>%
    summarise(
      etr_delta = sum(etr * imports) / sum(imports),
      total_imports = sum(imports),
      .groups = 'drop'
    ) %>%
    arrange(bea_code)

  output_path <- get_output_path(output_file, scenario, output_base)
  write_csv(bea_deltas, output_path)
  message(sprintf('  Wrote BEA deltas: %d commodities to %s', nrow(bea_deltas), output_path))

  invisible(bea_deltas)
}


#' Write stacked BEA commodity deltas for time-varying scenarios
#'
#' @param all_hs10_country_etrs Named list (date -> hs10_country_etrs tibble)
#' @param output_file Output filename
#' @param scenario Scenario name
#' @param output_base Base output directory
#'
#' @return Writes stacked CSV with date column and returns invisibly
write_bea_commodity_deltas_stacked <- function(all_hs10_country_etrs,
                                                output_file = 'bea_deltas_by_commodity.csv',
                                                scenario = NULL,
                                                output_base = 'output',
                                                date_intervals = NULL) {

  # Load crosswalk once
  crosswalk_path <- 'resources/hs10_bea_crosswalk.csv'
  if (!file.exists(crosswalk_path)) {
    message('  HS10→BEA crosswalk not found, skipping BEA delta output')
    return(invisible(NULL))
  }
  hs10_bea <- read_csv(crosswalk_path, show_col_types = FALSE,
                        col_types = cols(hs10 = col_character(), bea_code = col_character()))

  # Process each date
  stacked <- map_dfr(names(all_hs10_country_etrs), function(date_str) {
    hs10_data <- all_hs10_country_etrs[[date_str]]

    hs10_data %>%
      left_join(hs10_bea, by = 'hs10') %>%
      filter(!is.na(bea_code)) %>%
      group_by(bea_code) %>%
      summarise(
        etr_delta = sum(etr * imports) / sum(imports),
        total_imports = sum(imports),
        .groups = 'drop'
      ) %>%
      mutate(date = date_str)
  })

  # Reorder columns: date first
  stacked <- stacked %>%
    select(date, bea_code, etr_delta, total_imports) %>%
    arrange(date, bea_code)

  if (!is.null(date_intervals)) {
    stacked <- stacked %>%
      left_join(date_intervals, by = 'date') %>%
      relocate(date, valid_from, valid_until)
  }

  output_path <- get_output_path(output_file, scenario, output_base)
  write_csv(stacked, output_path)
  message(sprintf('  Wrote stacked BEA deltas: %d rows to %s', nrow(stacked), output_path))

  invisible(stacked)
}
