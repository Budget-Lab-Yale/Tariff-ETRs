# =============================================================================
# calculations.R
# =============================================================================
#
# Functions for calculating tariff deltas (counterfactual - baseline) and levels.
#
# Functions:
#   - detect_scenario_structure():        Detect baseline/counterfactual structure
#   - load_scenario_config():             Load all config YAMLs from a directory
#   - calc_etrs_for_config():             Calculate tariff levels for a single config set
#   - calc_delta():                       Compute delta between baseline and counterfactual
#   - do_scenario():                      Run complete analysis (dispatcher)
#   - do_scenario_static():               Run a static counterfactual scenario
#   - do_scenario_time_varying():         Run a time-varying counterfactual scenario
#   - calc_weighted_etr():                Calculate weighted tariff rates at HS10 × country level
#   - aggregate_countries_to_partners():  Aggregate HS10×country results to partner×GTAP level
#   - write_shock_commands():             Write GTAP shock commands to output file
#   - prepare_sector_country_deltas():    Prepare sector × country delta data (no I/O)
#   - write_sector_country_deltas():      Write deltas to CSV in sector x country format
#   - prepare_country_level_deltas():     Prepare country-level delta data (no I/O)
#   - write_country_level_deltas():       Write overall country-level deltas to CSV
#   - prepare_country_hts2_deltas():      Prepare country × HTS2 delta data (no I/O)
#   - write_country_hts2_deltas():        Write country × HTS2 deltas to CSV
#   - format_summary_table():             Format summary table (deltas or levels) as text
#   - calc_weighted_summary():            Shared helper for GTAP/Census weighted averages
#   - calc_overall_deltas_data():         Compute overall delta data (no I/O)
#   - calc_overall_deltas():              Calculate and print overall deltas by country
#   - write_*_stacked():                  Stacked output writers for time-varying scenarios
#   - prepare_levels_by_sector_country(): Prepare sector × country tariff level data (no I/O)
#   - write_levels_by_sector_country():   Write tariff levels to CSV (sector × country)
#   - calc_overall_levels_data():         Compute overall tariff level data (no I/O)
#   - write_overall_levels():             Write overall tariff levels to text file
#   - write_levels_by_sector_country_stacked(): Stacked levels CSV for time-varying
#   - write_overall_levels_combined():    Combined levels text for time-varying
#   - write_overall_deltas_combined():    Combined overall deltas for time-varying scenarios
#   - get_output_path():                  Helper to build output file paths
#
# =============================================================================

# -----------------------------------------------------------------------------
# Scenario structure detection
# -----------------------------------------------------------------------------

#' Extract valid YYYY-MM-DD date subdirectories from a directory
#'
#' @param dir_path Path to directory to scan
#'
#' @return Sorted character vector of date strings, or NULL if none found
find_date_subdirs <- function(dir_path) {
  if (!dir.exists(dir_path)) return(NULL)
  subdirs <- list.dirs(dir_path, full.names = FALSE, recursive = FALSE)
  date_pattern <- '^\\d{4}-\\d{2}-\\d{2}$'
  date_candidates <- subdirs[grepl(date_pattern, subdirs)]
  valid_dates <- date_candidates[!is.na(as.Date(date_candidates, format = '%Y-%m-%d'))]
  if (length(valid_dates) == 0) return(NULL)
  sort(valid_dates)
}


#' Detect scenario structure (baseline + counterfactual)
#'
#' Examines the scenario config directory for:
#'   - baseline/ subfolder (required)
#'   - YYYY-MM-DD subfolders as counterfactual dates (time-varying)
#'   - or config files at the top level (static counterfactual)
#'
#' @param scenario_dir Path to the scenario config directory
#'
#' @return Named list with:
#'   - has_baseline: logical
#'   - baseline_dates: sorted date strings if baseline is time-varying, NULL if static
#'   - counter_dates: sorted date strings if counterfactual is time-varying, NULL if static
detect_scenario_structure <- function(scenario_dir) {
  has_baseline <- dir.exists(file.path(scenario_dir, 'baseline'))

  # Counterfactual dates: YYYY-MM-DD subfolders at top level (excluding baseline/)
  counter_dates <- find_date_subdirs(scenario_dir)

  # Baseline dates: YYYY-MM-DD subfolders within baseline/
  baseline_dates <- NULL
  if (has_baseline) {
    baseline_dates <- find_date_subdirs(file.path(scenario_dir, 'baseline'))
  }

  list(
    has_baseline   = has_baseline,
    baseline_dates = baseline_dates,
    counter_dates  = counter_dates
  )
}


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
      coverage_by_partner$share_232[i] * 100,
      coverage_by_partner$share_ieepa[i] * 100,
      coverage_by_partner$share_neither[i] * 100))
  }

  lines <- c(lines, '', sprintf('%-10s  %11.1f%%  %11.1f%%  %11.1f%%',
    'TOTAL',
    coverage_total$share_232 * 100,
    coverage_total$share_ieepa * 100,
    coverage_total$share_neither * 100))

  lines
}


#' Load all config for a scenario from a single config directory
#'
#' Parses other_params.yaml (required), 232.yaml, ieepa_reciprocal.yaml,
#' ieepa_fentanyl.yaml, and s122.yaml (all optional) from the given directory.
#' Also loads MFN rates from the path specified in other_params$mfn_rates.
#'
#' @param config_path Path to directory containing the config YAML files
#'
#' @return Named list with: params_232, rates_ieepa_reciprocal, rates_ieepa_fentanyl,
#'   rates_s122, other_params, mfn_rates (all tariff components NULL if file missing)
load_scenario_config <- function(config_path) {

  message(sprintf('Loading scenario parameters from %s...', config_path))

  other_params <- read_yaml(file.path(config_path, 'other_params.yaml'))

  # Load MFN rates from path specified in config
  mfn_rates_path <- other_params$mfn_rates
  if (is.null(mfn_rates_path)) {
    stop(sprintf('other_params.yaml in %s must specify mfn_rates path', config_path))
  }
  mfn_rates <- load_mfn_rates(mfn_rates_path)

  # Load tariff configs (all optional - missing file = NULL = zero rates)
  s232_path <- file.path(config_path, '232.yaml')
  params_232 <- if (file.exists(s232_path)) {
    load_232_rates(s232_path)
  } else {
    NULL
  }

  reciprocal_path <- file.path(config_path, 'ieepa_reciprocal.yaml')
  rates_ieepa_reciprocal <- if (file.exists(reciprocal_path)) {
    load_ieepa_rates_yaml(reciprocal_path, rate_col_name = 'ieepa_reciprocal_rate')
  } else {
    NULL
  }

  fentanyl_path <- file.path(config_path, 'ieepa_fentanyl.yaml')
  rates_ieepa_fentanyl <- if (file.exists(fentanyl_path)) {
    load_ieepa_rates_yaml(fentanyl_path, rate_col_name = 'ieepa_fentanyl_rate')
  } else {
    NULL
  }

  s122_path <- file.path(config_path, 's122.yaml')
  rates_s122 <- if (file.exists(s122_path)) {
    load_ieepa_rates_yaml(s122_path, rate_col_name = 's122_rate')
  } else {
    NULL
  }

  list(
    params_232             = params_232,
    rates_ieepa_reciprocal = rates_ieepa_reciprocal,
    rates_ieepa_fentanyl   = rates_ieepa_fentanyl,
    rates_s122             = rates_s122,
    other_params           = other_params,
    mfn_rates              = mfn_rates
  )
}


#' Calculate tariff rates for a single config (one set of YAML files)
#'
#' Takes parsed config and pre-loaded shared data, runs calc_weighted_etr()
#' and aggregate_countries_to_partners(), returns both results.
#' MFN rates are loaded from the config (config$mfn_rates).
#'
#' @param config Named list from load_scenario_config()
#' @param hs10_by_country Import data (hs10, cty_code, gtap_code, imports)
#' @param usmca_shares USMCA share data
#' @param country_mapping Country-to-partner mapping
#'
#' @return Named list with hs10_country_etrs and partner_etrs
calc_etrs_for_config <- function(config, hs10_by_country, usmca_shares, country_mapping) {

  message('Calculating tariff rates at HS10 × country level...')

  # Load metal content shares for Section 232 derivative adjustment
  metal_content <- load_metal_content(
    metal_content_config = config$other_params$metal_content,
    import_data = hs10_by_country
  )

  hs10_country_etrs <- calc_weighted_etr(
    rates_232              = if (!is.null(config$params_232)) config$params_232$rate_matrix else NULL,
    usmca_exempt_232       = if (!is.null(config$params_232)) config$params_232$usmca_exempt else NULL,
    rates_ieepa_reciprocal = config$rates_ieepa_reciprocal,
    rates_ieepa_fentanyl   = config$rates_ieepa_fentanyl,
    rates_s122             = config$rates_s122,
    import_data            = hs10_by_country,
    usmca_data             = usmca_shares,
    us_auto_content_share  = config$other_params$us_auto_content_share,
    auto_rebate            = config$other_params$auto_rebate_rate,
    us_assembly_share      = config$other_params$us_auto_assembly_share,
    ieepa_usmca_exempt     = config$other_params$ieepa_usmca_exception,
    s122_usmca_exempt      = config$other_params$s122_usmca_exception %||% 0,
    metal_content          = metal_content,
    metal_programs         = config$other_params$metal_content$metal_programs %||% character(0),
    program_metal_types    = config$other_params$metal_content$program_metal_types %||% NULL,
    mfn_rates              = config$mfn_rates
  )

  message('Aggregating HS10×country results to partner×GTAP level...')
  partner_etrs <- aggregate_countries_to_partners(
    hs10_country_etrs = hs10_country_etrs,
    country_mapping   = country_mapping
  )

  list(
    hs10_country_etrs = hs10_country_etrs,
    partner_etrs      = partner_etrs
  )
}


#' Compute delta between counterfactual and baseline HS10×country results
#'
#' Joins on (hs10, cty_code) and computes delta = counterfactual level - baseline level.
#' The delta replaces the etr column (used for shocks and delta outputs).
#' The level column retains the counterfactual absolute tariff rate.
#'
#' @param baseline_hs10 HS10×country results from baseline config
#' @param counter_hs10 HS10×country results from counterfactual config
#'
#' @return counter_hs10 with etr replaced by delta (counter level - baseline level)
calc_delta <- function(baseline_hs10, counter_hs10) {
  counter_hs10 %>%
    left_join(
      baseline_hs10 %>% select(hs10, cty_code, level_base = level),
      by = c('hs10', 'cty_code')
    ) %>%
    mutate(
      level_base = if_else(is.na(level_base), 0, level_base),
      etr = level - level_base
    ) %>%
    select(-level_base)
}


#' Run complete tariff analysis for a given scenario
#'
#' Processes baseline and counterfactual configs, computes deltas, and writes outputs.
#'
#' @param scenario Scenario name (corresponds to {config_dir}/{scenario}/ directory)
#' @param config_dir Base directory for scenario configs (default: 'config')
#' @param output_dir Base directory for outputs (default: 'output')
#' @param import_data_path Path to Census Bureau import data files
#' @param use_cache Use cached HS10 data if available (default: TRUE)
#'
#' @return Returns results invisibly
do_scenario <- function(scenario, config_dir = 'config', output_dir = 'output',
                        import_data_path = 'C:/Users/jar335/Downloads', use_cache = TRUE) {

  message(sprintf('\n=========================================================='))
  message(sprintf('Running scenario: %s', scenario))
  message(sprintf('==========================================================\n'))

  #---------------------------
  # Load shared data (same for all dates in a time-varying scenario)
  #---------------------------

  # USMCA share of trade by sector
  usmca_shares <- read_csv('./resources/usmca_shares.csv', show_col_types = FALSE)

  message('Reading import data...')

  # Load country-to-partner mapping (used for USMCA and final aggregation)
  country_mapping <- read_csv(
    'resources/country_partner_mapping.csv',
    col_types = cols(cty_code = col_character()),
    show_col_types = FALSE
  )

  # Read GTAP crosswalk (HS10 -> GTAP)
  crosswalk <- read_csv('resources/hs10_gtap_crosswalk.csv', show_col_types = FALSE)

  # Define cache path for processed data
  cache_dir <- 'cache'
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  cache_file <- file.path(cache_dir, 'hs10_by_country_gtap_2024_con.rds')

  # Load processed HS10 x country x GTAP data (from cache or build from raw)
  if (use_cache && file.exists(cache_file)) {
    message(sprintf('Loading cached HS10 x country x GTAP data from %s...', cache_file))
    hs10_by_country <- readRDS(cache_file) %>%

      # Filter out special HTS codes (chapters 98-99) in case cache predates this filter
      filter(!str_detect(hs10, '^(98|99)')) %>%

      # Filter out rows with missing GTAP codes (unmapped HS10 codes)
      filter(!is.na(gtap_code))
    message(sprintf('Loaded %s cached records', format(nrow(hs10_by_country), big.mark = ',')))
  } else {
    if (use_cache) {
      message('No cache found, processing raw import files...')
    } else {
      message('Cache disabled, processing raw import files...')
    }

    # Load raw HS10-level import data
    hs10_raw <- load_imports_hs10_country(
      import_data_path = import_data_path,
      year = 2024,
      type = 'con'
    )

    # Build data: aggregate by HS10 x country and add GTAP sectors
    hs10_by_country <- hs10_raw %>%

      # Get totals by HS10 and country
      group_by(hs10, cty_code) %>%
      summarise(imports = sum(value), .groups = 'drop') %>%

      # Filter out special HTS codes (chapters 98-99: re-imports, special provisions, etc.)
      filter(!str_detect(hs10, '^(98|99)')) %>%

      # Add GTAP code
      left_join(
        crosswalk %>%
          select(hs10, gtap_code),
        by = 'hs10'
      ) %>%
      mutate(gtap_code = str_to_lower(gtap_code)) %>%
      relocate(gtap_code, .after = hs10) %>%

      # Filter out rows with missing GTAP codes (unmapped HS10 codes)
      filter(!is.na(gtap_code))

    # Save processed data to cache for next time
    message(sprintf('Saving processed data to cache at %s...', cache_file))
    saveRDS(hs10_by_country, cache_file)
    message('Cache saved successfully')
  }

  #---------------------------
  # Detect scenario structure and dispatch
  #---------------------------

  scenario_path <- file.path(config_dir, scenario)
  structure <- detect_scenario_structure(scenario_path)

  if (!structure$has_baseline) {
    stop(sprintf('Scenario "%s" requires a baseline/ subdirectory in %s', scenario, scenario_path))
  }

  #---------------------------
  # Process baseline
  #---------------------------

  message('\n--- Processing baseline ---')
  baseline_results <- list()

  if (is.null(structure$baseline_dates)) {
    # Static baseline: config files directly in baseline/
    baseline_path <- file.path(scenario_path, 'baseline')
    baseline_config <- load_scenario_config(baseline_path)
    baseline_etrs <- calc_etrs_for_config(baseline_config, hs10_by_country, usmca_shares, country_mapping)
    baseline_results[['static']] <- baseline_etrs$hs10_country_etrs
  } else {
    # Time-varying baseline
    for (date_str in structure$baseline_dates) {
      message(sprintf('\n--- Processing baseline date: %s ---', date_str))
      baseline_path <- file.path(scenario_path, 'baseline', date_str)
      baseline_config <- load_scenario_config(baseline_path)
      baseline_etrs <- calc_etrs_for_config(baseline_config, hs10_by_country, usmca_shares, country_mapping)
      baseline_results[[date_str]] <- baseline_etrs$hs10_country_etrs
    }
  }

  #---------------------------
  # Dispatch to static or time-varying counterfactual
  #---------------------------

  if (is.null(structure$counter_dates)) {
    # Static counterfactual (config files at scenario root)
    result <- do_scenario_static(
      scenario          = scenario,
      scenario_path     = scenario_path,
      output_dir        = output_dir,
      hs10_by_country   = hs10_by_country,
      usmca_shares      = usmca_shares,
      country_mapping   = country_mapping,
      baseline_results  = baseline_results
    )
  } else {
    # Time-varying counterfactual
    message(sprintf('\nDetected time-varying counterfactual with %d dates: %s',
                    length(structure$counter_dates), paste(structure$counter_dates, collapse = ', ')))
    result <- do_scenario_time_varying(
      scenario          = scenario,
      scenario_path     = scenario_path,
      output_dir        = output_dir,
      counter_dates     = structure$counter_dates,
      hs10_by_country   = hs10_by_country,
      usmca_shares      = usmca_shares,
      country_mapping   = country_mapping,
      baseline_results  = baseline_results
    )
  }

  message(sprintf('\nScenario %s complete!\n', scenario))
  invisible(result)
}


#' Match a counterfactual date to the appropriate baseline result
#'
#' For static baselines (single entry keyed 'static'), always returns that entry.
#' For time-varying baselines, finds the most recent baseline date <= counter_date.
#'
#' @param counter_date Date string (YYYY-MM-DD) or 'static'
#' @param baseline_results Named list of baseline HS10×country results
#'
#' @return Baseline HS10×country result for this counterfactual date
match_baseline <- function(counter_date, baseline_results) {
  if ('static' %in% names(baseline_results)) {
    return(baseline_results[['static']])
  }

  # Time-varying baseline keys must be valid dates.
  baseline_date_keys <- sort(names(baseline_results))
  baseline_dates <- as.Date(baseline_date_keys, format = '%Y-%m-%d')
  if (any(is.na(baseline_dates))) {
    stop(sprintf('Baseline results contain non-date keys: %s',
                 paste(baseline_date_keys[is.na(baseline_dates)], collapse = ', ')))
  }

  # Static counterfactual + time-varying baseline:
  # use the latest available baseline explicitly.
  if (counter_date == 'static') {
    chosen_idx <- which.max(baseline_dates)
    chosen_date <- baseline_date_keys[chosen_idx]
    message(sprintf('Static counterfactual with time-varying baseline: using latest baseline date %s', chosen_date))
    return(baseline_results[[chosen_date]])
  }

  counter_date_parsed <- as.Date(counter_date, format = '%Y-%m-%d')
  if (is.na(counter_date_parsed)) {
    stop(sprintf('Invalid counterfactual date: %s', counter_date))
  }

  # Time-varying baseline: find most recent date <= counterfactual date.
  eligible_idx <- which(baseline_dates <= counter_date_parsed)
  if (length(eligible_idx) == 0) {
    stop(sprintf('No baseline date found on or before counterfactual date %s (available: %s)',
                 counter_date, paste(baseline_date_keys, collapse = ', ')))
  }
  chosen_date <- baseline_date_keys[max(eligible_idx)]
  baseline_results[[chosen_date]]
}


#' Run a static (non-time-varying) counterfactual scenario
#'
#' @param scenario Scenario name
#' @param scenario_path Full path to scenario config directory
#' @param output_dir Base output directory
#' @param hs10_by_country Pre-loaded import data
#' @param usmca_shares Pre-loaded USMCA shares
#' @param country_mapping Pre-loaded country mapping
#' @param baseline_results Named list of baseline HS10×country results
#'
#' @return partner_etrs invisibly
do_scenario_static <- function(scenario, scenario_path, output_dir,
                                hs10_by_country, usmca_shares, country_mapping,
                                baseline_results) {

  config <- load_scenario_config(scenario_path)
  etrs <- calc_etrs_for_config(config, hs10_by_country, usmca_shares, country_mapping)

  # Compute delta (counterfactual - baseline)
  baseline_hs10 <- match_baseline('static', baseline_results)
  hs10_country_etrs <- calc_delta(baseline_hs10, etrs$hs10_country_etrs)

  # Re-aggregate with delta-adjusted etr
  partner_etrs <- aggregate_countries_to_partners(
    hs10_country_etrs = hs10_country_etrs,
    country_mapping   = country_mapping
  )

  message('Writing outputs...')

  write_shock_commands(
    etr_data    = partner_etrs,
    output_file = 'shocks.txt',
    scenario    = scenario,
    output_base = output_dir
  )

  write_sector_country_deltas(
    etr_data    = partner_etrs,
    output_file = 'deltas_by_sector_country.csv',
    scenario    = scenario,
    output_base = output_dir
  )

  write_country_level_deltas(
    hs10_country_etrs = hs10_country_etrs,
    output_file       = 'deltas_by_census_country.csv',
    scenario          = scenario,
    output_base       = output_dir
  )

  write_country_hts2_deltas(
    hs10_country_etrs = hs10_country_etrs,
    output_file       = 'deltas_by_census_country_hts2.csv',
    scenario          = scenario,
    output_base       = output_dir
  )

  calc_overall_deltas(
    etr_data          = partner_etrs,
    hs10_country_etrs = hs10_country_etrs,
    country_mapping   = country_mapping,
    scenario          = scenario,
    output_base       = output_dir
  )

  write_levels_by_sector_country(
    etr_data    = partner_etrs,
    output_file = 'levels_by_sector_country.csv',
    scenario    = scenario,
    output_base = output_dir
  )

  write_overall_levels(
    etr_data          = partner_etrs,
    hs10_country_etrs = hs10_country_etrs,
    country_mapping   = country_mapping,
    scenario          = scenario,
    output_base       = output_dir
  )

  partner_etrs
}


#' Run a time-varying counterfactual scenario (dated subfolders)
#'
#' Each date subfolder must contain a complete set of config files.
#' Shock files are written per-date; CSVs are stacked with a date column.
#'
#' @param scenario Scenario name
#' @param scenario_path Full path to scenario config directory
#' @param output_dir Base output directory
#' @param counter_dates Sorted character vector of date strings (YYYY-MM-DD)
#' @param hs10_by_country Pre-loaded import data
#' @param usmca_shares Pre-loaded USMCA shares
#' @param country_mapping Pre-loaded country mapping
#' @param baseline_results Named list of baseline HS10×country results
#'
#' @return Named list of per-date partner_etrs
do_scenario_time_varying <- function(scenario, scenario_path, output_dir,
                                      counter_dates, hs10_by_country,
                                      usmca_shares, country_mapping,
                                      baseline_results) {

  # Collect per-date results
  all_partner_etrs      <- list()
  all_hs10_country_etrs <- list()
  all_overall_data      <- list()
  all_levels_data       <- list()

  for (date_str in counter_dates) {
    message(sprintf('\n--- Processing counterfactual date: %s ---', date_str))

    config_path <- file.path(scenario_path, date_str)
    config <- load_scenario_config(config_path)
    etrs <- calc_etrs_for_config(config, hs10_by_country, usmca_shares, country_mapping)

    # Compute delta (counterfactual - baseline)
    baseline_hs10 <- match_baseline(date_str, baseline_results)
    hs10_country_etrs <- calc_delta(baseline_hs10, etrs$hs10_country_etrs)

    # Re-aggregate with delta-adjusted etr
    partner_etrs <- aggregate_countries_to_partners(
      hs10_country_etrs = hs10_country_etrs,
      country_mapping   = country_mapping
    )

    # Write per-date shock commands (into output/scenario/date/shocks.txt)
    write_shock_commands(
      etr_data    = partner_etrs,
      output_file = 'shocks.txt',
      scenario    = file.path(scenario, date_str),
      output_base = output_dir
    )

    all_partner_etrs[[date_str]]      <- partner_etrs
    all_hs10_country_etrs[[date_str]] <- hs10_country_etrs

    # Compute overall delta data for this date (no I/O)
    all_overall_data[[date_str]] <- calc_overall_deltas_data(
      etr_data          = partner_etrs,
      hs10_country_etrs = hs10_country_etrs,
      country_mapping   = country_mapping
    )

    # Compute overall tariff level data for this date (no I/O)
    all_levels_data[[date_str]] <- calc_overall_levels_data(
      etr_data          = partner_etrs,
      hs10_country_etrs = hs10_country_etrs,
      country_mapping   = country_mapping
    )
  }

  # Write stacked outputs
  message('\nWriting stacked outputs...')

  write_sector_country_deltas_stacked(
    all_partner_etrs = all_partner_etrs,
    output_file      = 'deltas_by_sector_country.csv',
    scenario         = scenario,
    output_base      = output_dir
  )

  write_country_level_deltas_stacked(
    all_hs10_country_etrs = all_hs10_country_etrs,
    output_file           = 'deltas_by_census_country.csv',
    scenario              = scenario,
    output_base           = output_dir
  )

  write_country_hts2_deltas_stacked(
    all_hs10_country_etrs = all_hs10_country_etrs,
    output_file           = 'deltas_by_census_country_hts2.csv',
    scenario              = scenario,
    output_base           = output_dir
  )

  write_overall_deltas_combined(
    all_overall_data = all_overall_data,
    scenario         = scenario,
    output_base      = output_dir
  )

  write_levels_by_sector_country_stacked(
    all_partner_etrs = all_partner_etrs,
    output_file      = 'levels_by_sector_country.csv',
    scenario         = scenario,
    output_base      = output_dir
  )

  write_overall_levels_combined(
    all_levels_data = all_levels_data,
    scenario        = scenario,
    output_base     = output_dir
  )

  all_partner_etrs
}


#' Calculate weighted ETR changes at HS10 × country level using tabular config data
#'
#' Takes clean tabular rate data from config parsing and applies USMCA exemptions,
#' auto rebates, and stacking rules to produce final ETRs.
#'
#' @param rates_232 Tibble with columns: hs10, cty_code, s232_[tariff]_rate (one per tariff)
#' @param usmca_exempt_232 Named vector of USMCA exempt flags by tariff name
#' @param rates_ieepa_reciprocal Tibble with columns: hs10, cty_code, ieepa_reciprocal_rate
#' @param rates_ieepa_fentanyl Tibble with columns: hs10, cty_code, ieepa_fentanyl_rate
#' @param rates_s122 Tibble with columns: hs10, cty_code, s122_rate (or NULL if no s122.yaml)
#' @param import_data Data frame with hs10, cty_code, gtap_code, imports
#' @param usmca_data Data frame with USMCA shares by partner and GTAP sector
#' @param us_auto_content_share Share of US content in auto assembly
#' @param auto_rebate Auto rebate rate
#' @param us_assembly_share Share of US assembly in autos
#' @param ieepa_usmca_exempt Apply USMCA exemption to IEEPA tariffs (1 = yes, 0 = no)
#' @param metal_content Tibble with columns: hs10, metal_share (from load_metal_content()).
#'   May also include steel_share, aluminum_share, copper_share, other_metal_share for detail mode.
#' @param metal_programs Character vector of 232 tariff names that are metal programs (e.g., 'steel', 'aluminum')
#' @param program_metal_types Named list: program_name -> metal type string (e.g., list(steel = 'steel', aluminum_base_articles = 'aluminum')).
#'   When provided with per-type share columns, each program is scaled by its type's share instead of aggregate metal_share.
#' @param mfn_rates Tibble with columns: hs8, mfn_rate (MFN baseline tariff rates)
#'
#' @return Data frame with columns: hs10, cty_code, gtap_code, imports, etr, mfn_rate, level, base_232, base_ieepa, base_neither
calc_weighted_etr <- function(rates_232,
                              usmca_exempt_232,
                              rates_ieepa_reciprocal,
                              rates_ieepa_fentanyl,
                              rates_s122 = NULL,
                              import_data,
                              usmca_data,
                              us_auto_content_share,
                              auto_rebate,
                              us_assembly_share,
                              ieepa_usmca_exempt,
                              s122_usmca_exempt = 0,
                              metal_content = NULL,
                              metal_programs = character(0),
                              program_metal_types = NULL,
                              mfn_rates = NULL) {

  # =============================================================================
  # Build complete rate matrix by joining config tables with import data
  # Config tables are now sparse (only non-zero rates), so NAs from joins = 0
  # NULL inputs (missing config files) produce zero-rate columns
  # =============================================================================

  rate_matrix <- import_data %>%
    select(hs10, cty_code, gtap_code, imports)

  # Join 232 rates if provided
  if (!is.null(rates_232)) {
    rate_matrix <- rate_matrix %>%
      left_join(rates_232, by = c('hs10', 'cty_code'))
  }

  # Join IEEPA reciprocal rates if provided, otherwise add zero column
  if (!is.null(rates_ieepa_reciprocal)) {
    rate_matrix <- rate_matrix %>%
      left_join(rates_ieepa_reciprocal, by = c('hs10', 'cty_code'))
  } else {
    rate_matrix <- rate_matrix %>%
      mutate(ieepa_reciprocal_rate = 0)
  }

  # Join IEEPA fentanyl rates if provided, otherwise add zero column
  if (!is.null(rates_ieepa_fentanyl)) {
    rate_matrix <- rate_matrix %>%
      left_join(rates_ieepa_fentanyl, by = c('hs10', 'cty_code'))
  } else {
    rate_matrix <- rate_matrix %>%
      mutate(ieepa_fentanyl_rate = 0)
  }

  # Join Section 122 rates if provided
  if (!is.null(rates_s122)) {
    rate_matrix <- rate_matrix %>%
      left_join(rates_s122, by = c('hs10', 'cty_code'))
  } else {
    rate_matrix <- rate_matrix %>%
      mutate(s122_rate = 0)
  }

  # Get all 232 rate column names
  rate_232_cols <- names(rate_matrix)[str_detect(names(rate_matrix), '^s232_.*_rate$')]

  # Replace NAs (indicates no tariff) with 0 for all rate columns
  all_rate_cols <- c(rate_232_cols, 'ieepa_reciprocal_rate', 'ieepa_fentanyl_rate', 's122_rate')
  rate_matrix <- rate_matrix %>%
    mutate(
      across(
        .cols = all_of(all_rate_cols),
        .fns  = ~ if_else(is.na(.), 0, .)
      )
    )

  # =============================================================================
  # Apply USMCA exemptions and auto rebates
  # =============================================================================

  # Reshape USMCA shares to long format for joining
  usmca_long <- usmca_data %>%
    pivot_longer(cols = -gtap_code, names_to = 'partner', values_to = 'usmca_share') %>%
    mutate(
      cty_code = case_when(
        partner == 'canada' ~ CTY_CANADA,
        partner == 'mexico' ~ CTY_MEXICO,
        TRUE ~ NA
      )
    ) %>%
    filter(!is.na(cty_code)) %>%
    select(gtap_code, cty_code, usmca_share)

  # Join USMCA shares
  rate_matrix <- rate_matrix %>%
    left_join(usmca_long, by = c('cty_code', 'gtap_code')) %>%
    mutate(usmca_share = replace_na(usmca_share, 0))

  # Auto tariff list
  auto_tariffs <- c('automobiles_passenger_and_light_trucks', 'automobile_parts', 'vehicles_completed_mhd')

  # Get tariff names from rate column names
  tariff_names <- str_replace(rate_232_cols, '^s232_(.*)_rate$', '\\1')

  # Apply auto rebate and USMCA exemptions to each 232 tariff
  for (tariff_name in tariff_names) {
    rate_col <- paste0('s232_', tariff_name, '_rate')

    # Apply auto rebate if this is an auto tariff
    if (tariff_name %in% auto_tariffs) {
      rate_matrix <- rate_matrix %>%
        mutate(
          !!rate_col := !!sym(rate_col) - (auto_rebate * us_assembly_share)
        )
    }

    # Apply USMCA exemption if enabled for this tariff
    if (usmca_exempt_232[[tariff_name]] == 1) {
      
      # Auto tariffs use adjusted USMCA share
      if (tariff_name %in% auto_tariffs) {
        rate_matrix <- rate_matrix %>%
          mutate(
            adjusted_usmca_share = usmca_share * us_auto_content_share,
            !!rate_col := if_else(
              cty_code %in% USMCA_COUNTRIES,
              !!sym(rate_col) * (1 - adjusted_usmca_share),
              !!sym(rate_col)
            )
          ) %>%
          select(-adjusted_usmca_share)
        
      # Non-auto tariffs use standard USMCA share
      } else {
        rate_matrix <- rate_matrix %>%
          mutate(
            !!rate_col := if_else(
              cty_code %in% USMCA_COUNTRIES,
              !!sym(rate_col) * (1 - usmca_share),
              !!sym(rate_col)
            )
          )
      }
    }
  }

  # Apply USMCA exemption to IEEPA tariffs if enabled
  if (ieepa_usmca_exempt == 1) {
    rate_matrix <- rate_matrix %>%
      mutate(
        ieepa_reciprocal_rate = if_else(
          cty_code %in% USMCA_COUNTRIES,
          ieepa_reciprocal_rate * (1 - usmca_share),
          ieepa_reciprocal_rate
        ),
        ieepa_fentanyl_rate = if_else(
          cty_code %in% USMCA_COUNTRIES,
          ieepa_fentanyl_rate * (1 - usmca_share),
          ieepa_fentanyl_rate
        )
      )
  }

  # Apply USMCA exemption to Section 122 tariffs if enabled
  if (s122_usmca_exempt == 1) {
    rate_matrix <- rate_matrix %>%
      mutate(
        s122_rate = if_else(
          cty_code %in% USMCA_COUNTRIES,
          s122_rate * (1 - usmca_share),
          s122_rate
        )
      )
  }

  # Clean up USMCA share column
  rate_matrix <- rate_matrix %>%
    select(-usmca_share)

  # Join MFN rates early so reciprocal target-total rules can use Column 1.
  if (!is.null(mfn_rates)) {
    rate_matrix <- rate_matrix %>%
      mutate(hs8 = substr(hs10, 1, 8)) %>%
      left_join(mfn_rates, by = 'hs8')

    n_missing <- sum(is.na(rate_matrix$mfn_rate))
    n_total <- nrow(rate_matrix)
    if (n_missing > 0) {
      n_hs8_missing <- length(unique(rate_matrix$hs8[is.na(rate_matrix$mfn_rate)]))
      message(sprintf('MFN join: %d of %d rows (%d unique HS8 codes) have no MFN rate, defaulting to 0',
                      n_missing, n_total, n_hs8_missing))
    }

    rate_matrix <- rate_matrix %>%
      mutate(
        mfn_rate = if_else(is.na(mfn_rate), 0, mfn_rate)
      ) %>%
      select(-hs8)
  } else {
    rate_matrix <- rate_matrix %>%
      mutate(mfn_rate = 0)
  }

  # Optional reciprocal target-total rule:
  # effective reciprocal add-on = max(target_total_rate - mfn_rate, 0)
  # applied only where reciprocal coverage is active (>0).
  if ('target_total_rate' %in% names(rate_matrix)) {
    rate_matrix <- rate_matrix %>%
      mutate(
        ieepa_reciprocal_rate = if_else(
          !is.na(target_total_rate) & ieepa_reciprocal_rate > 0,
          pmax(target_total_rate - mfn_rate, 0),
          ieepa_reciprocal_rate
        )
      )
  }


  # =============================================================================
  # Apply metal content shares to Section 232 metal program rates
  # For derivative products, only the metal content share of the customs value
  # is subject to 232 tariffs; the non-metal portion receives IEEPA tariffs.
  # =============================================================================

  # Track whether per-type accumulation is active for nonmetal_share computation
  use_per_type_nonmetal <- FALSE

  if (!is.null(metal_content) && length(metal_programs) > 0) {

    # Join metal content shares (may include per-type columns)
    rate_matrix <- rate_matrix %>%
      left_join(metal_content, by = 'hs10') %>%
      mutate(metal_share = if_else(is.na(metal_share), 1.0, metal_share))

    # Check if per-type shares are available and program_metal_types is provided
    has_per_type <- !is.null(program_metal_types) &&
      'steel_share' %in% names(rate_matrix)

    # Map metal type names to column names
    type_col_map <- c(
      steel   = 'steel_share',
      aluminum = 'aluminum_share',
      copper  = 'copper_share',
      other   = 'other_metal_share'
    )

    if (has_per_type) {
      use_per_type_nonmetal <- TRUE
    }

    # Apply metal content share to metal program 232 tariffs
    # This scales the rate: effective_rate = statutory_rate * share
    for (tariff_name in tariff_names) {
      if (tariff_name %in% metal_programs) {
        rate_col <- paste0('s232_', tariff_name, '_rate')

        # Use per-type share if available, otherwise aggregate metal_share
        if (has_per_type && tariff_name %in% names(program_metal_types)) {
          metal_type <- program_metal_types[[tariff_name]]
          type_col <- type_col_map[[metal_type]]
          if (is.null(type_col)) {
            stop(sprintf('Unknown metal type "%s" for program "%s". Expected: %s',
                          metal_type, tariff_name, paste(names(type_col_map), collapse = ', ')))
          }
          rate_matrix <- rate_matrix %>%
            mutate(!!rate_col := !!sym(rate_col) * !!sym(type_col))
        } else {
          rate_matrix <- rate_matrix %>%
            mutate(!!rate_col := !!sym(rate_col) * metal_share)
        }
      }
    }

    # Compute per-type nonmetal share AFTER scaling.
    # Group programs by metal type and check if ANY program of that type is active
    # per row. Add each type's share at most once to avoid double-counting when
    # multiple programs map to the same type (e.g., aluminum_base + aluminum_derivative).
    if (has_per_type) {
      type_to_programs <- split(names(program_metal_types), unlist(program_metal_types))
      rate_matrix <- rate_matrix %>% mutate(.active_type_share = 0)

      for (metal_type in names(type_to_programs)) {
        progs_of_type <- intersect(type_to_programs[[metal_type]], tariff_names)
        type_col <- type_col_map[[metal_type]]
        rate_cols_for_type <- paste0('s232_', progs_of_type, '_rate')
        rate_cols_for_type <- rate_cols_for_type[rate_cols_for_type %in% names(rate_matrix)]

        if (length(rate_cols_for_type) > 0) {
          rate_matrix <- rate_matrix %>%
            mutate(.active_type_share = .active_type_share +
              if_else(pmax(!!!syms(rate_cols_for_type)) > 0, !!sym(type_col), 0))
        }
      }
    }

  } else {
    # No metal content adjustment — add column for downstream stacking logic
    rate_matrix <- rate_matrix %>%
      mutate(metal_share = 1.0)
  }


  # =============================================================================
  # Apply stacking rules to calculate final_rate
  # =============================================================================

  # Calculate effective 232 rate across all tariffs
  if (length(tariff_names) > 0) {

    if (use_per_type_nonmetal) {
      # Per-type mode: metal rates are already scaled by disjoint type shares,
      # so rates from different metal types are additive. Within a single metal
      # type, pmax dedupes overlapping programs (e.g., aluminum_base vs
      # aluminum_derivative). Non-metal 232 programs (autos) cover the full
      # product, so they dominate via pmax against the metal sum.
      nonmetal_232_programs <- setdiff(tariff_names, metal_programs)
      nonmetal_232_cols <- paste0('s232_', nonmetal_232_programs, '_rate')
      nonmetal_232_cols <- nonmetal_232_cols[nonmetal_232_cols %in% names(rate_matrix)]

      # Group metal programs by type and take pmax within each type
      type_to_programs <- split(names(program_metal_types), unlist(program_metal_types))
      type_max_exprs <- list()
      for (metal_type in names(type_to_programs)) {
        progs_of_type <- intersect(type_to_programs[[metal_type]], tariff_names)
        cols_for_type <- paste0('s232_', progs_of_type, '_rate')
        cols_for_type <- cols_for_type[cols_for_type %in% names(rate_matrix)]
        if (length(cols_for_type) > 0) {
          type_max_exprs[[metal_type]] <- rlang::expr(pmax(!!!syms(cols_for_type)))
        }
      }

      # Sum across metal types (disjoint value fractions are additive)
      if (length(type_max_exprs) > 0) {
        metal_sum_expr <- Reduce(function(a, b) rlang::expr(!!a + !!b), type_max_exprs)
      } else {
        metal_sum_expr <- rlang::expr(0)
      }

      # pmax of non-metal 232 (full-product) vs metal sum (partial fractions)
      if (length(nonmetal_232_cols) > 0) {
        nonmetal_max_expr <- rlang::expr(pmax(!!!syms(nonmetal_232_cols)))
        final_expr <- rlang::expr(pmax(!!nonmetal_max_expr, !!metal_sum_expr))
      } else {
        final_expr <- metal_sum_expr
      }

      rate_matrix <- rate_matrix %>%
        mutate(rate_232_max = !!final_expr)

    } else {
      # Aggregate mode: single metal_share applied to all metal programs,
      # so pmax across all 232 columns is correct
      rate_matrix <- rate_matrix %>%
        mutate(rate_232_max = pmax(!!!syms(rate_232_cols)))
    }

  # No 232 tariffs - set max to 0
  } else {
    rate_matrix <- rate_matrix %>%
      mutate(rate_232_max = 0)
  }

  # Compute non-metal share for IEEPA application on the non-metal portion of derivatives.
  # Only for products covered by metal 232 programs (not auto 232, etc.).
  # Per-type mode: nonmetal_share = 1 - sum(active type shares), so IEEPA fills
  # everything not claimed by the specific 232 programs covering each product.
  # Aggregate mode: nonmetal_share = 1 - metal_share (unchanged).
  metal_232_cols <- paste0('s232_', intersect(tariff_names, metal_programs), '_rate')
  metal_232_cols <- metal_232_cols[metal_232_cols %in% names(rate_matrix)]

  if (length(metal_232_cols) > 0) {
    if (use_per_type_nonmetal) {
      # Per-type mode: IEEPA covers everything active 232 programs don't claim
      rate_matrix <- rate_matrix %>%
        mutate(
          nonmetal_share = if_else(pmax(!!!syms(metal_232_cols)) > 0, 1 - .active_type_share, 0)
        )
    } else {
      # Aggregate mode: IEEPA covers the non-metal portion
      rate_matrix <- rate_matrix %>%
        mutate(
          nonmetal_share = if_else(pmax(!!!syms(metal_232_cols)) > 0, 1 - metal_share, 0)
        )
    }
  } else {
    rate_matrix <- rate_matrix %>%
      mutate(nonmetal_share = 0)
  }

  # Clean up temporary accumulator column
  if ('.active_type_share' %in% names(rate_matrix)) {
    rate_matrix <- rate_matrix %>% select(-.active_type_share)
  }

  rate_matrix <- rate_matrix %>%
    mutate(

      # Stacking rules:
      # 1. IEEPA Reciprocal: Mutually exclusive with 232 (applies only to uncovered base)
      #    Exception: for metal 232 derivatives, IEEPA applies to the non-metal portion
      # 2. IEEPA Fentanyl:
      #    - China: STACKS on top of everything (232 + reciprocal + fentanyl)
      #    - Others: Only applies to base not covered by 232 or reciprocal
      # 3. Section 122: Excluded "to the extent the 232 tariff applies," meaning
      #    S122 applies only to the nonmetal_share of 232-covered products (same as IEEPA).
      #    For non-232 products, S122 applies in full.

      final_rate = case_when(
        # China: Fentanyl stacks on top of normal 232-reciprocal logic
        # For metal 232 derivatives: reciprocal and s122 apply to non-metal portion
        cty_code == CTY_CHINA ~ if_else(
          rate_232_max > 0,
          rate_232_max + ieepa_reciprocal_rate * nonmetal_share
            + ieepa_fentanyl_rate + s122_rate * nonmetal_share,
          ieepa_reciprocal_rate + ieepa_fentanyl_rate + s122_rate
        ),

        # Everyone else: 232 takes precedence, then reciprocal + fentanyl + s122
        # For metal 232 derivatives: IEEPA and s122 apply to non-metal portion
        rate_232_max > 0 ~
          rate_232_max + (ieepa_reciprocal_rate + ieepa_fentanyl_rate + s122_rate) * nonmetal_share,

        # Otherwise use all IEEPA + s122
        TRUE ~ ieepa_reciprocal_rate + ieepa_fentanyl_rate + s122_rate
      )
    )

  # =============================================================================
  # Calculate ETR and coverage tracking
  # =============================================================================

  hs10_country_etrs <- rate_matrix %>%
    mutate(
      etr = final_rate,

      # Coverage tracking (mutually exclusive categories for reporting)
      # Note: For China, fentanyl stacks on top, but for coverage we track primary authority.
      # The non-232 bucket includes IEEPA and Section 122.
      base_232     = if_else(rate_232_max > 0, imports, 0),
      base_ieepa   = if_else(
        rate_232_max == 0 & (ieepa_reciprocal_rate > 0 | ieepa_fentanyl_rate > 0 | s122_rate > 0),
        imports, 0
      ),
      base_neither = if_else(
        rate_232_max == 0 & ieepa_reciprocal_rate == 0 & ieepa_fentanyl_rate == 0 & s122_rate == 0,
        imports, 0
      )
    )

  hs10_country_etrs <- hs10_country_etrs %>%
    mutate(level = mfn_rate + etr)

  hs10_country_etrs <- hs10_country_etrs %>%
    select(hs10, cty_code, gtap_code, imports, etr, mfn_rate, level, base_232, base_ieepa, base_neither)

  return(hs10_country_etrs)
}


#' Aggregate HS10×country ETRs to partner×GTAP level for GTAP output
#'
#' Takes HS10-level ETR data and aggregates to 8 partner groups × GTAP sectors
#' using import-weighted averaging. Also aggregates coverage bases for tariff
#' coverage reporting.
#'
#' @param hs10_country_etrs Data frame with columns: hs10, cty_code, gtap_code, imports, etr, mfn_rate, level, base_232, base_ieepa, base_neither
#' @param country_mapping Data frame with columns: cty_code, partner
#'
#' @return Data frame with columns: partner, gtap_code, etr, level, base_232, base_ieepa, base_neither
aggregate_countries_to_partners <- function(hs10_country_etrs, country_mapping) {

  # Map countries to partners
  etrs_with_partner <- hs10_country_etrs %>%
    left_join(
      country_mapping %>% select(cty_code, partner) %>% distinct(),
      by = 'cty_code'
    ) %>%
    # Unmapped countries default to 'row'
    mutate(partner = if_else(is.na(partner), 'row', partner))

  # Aggregate to partner × GTAP level using import-weighted average
  # NOTE: weighted_etr and weighted_level must be computed BEFORE imports is overwritten
  # (dplyr summarise gotcha — later expressions see the scalar aggregate, not the vector)
  partner_etrs <- etrs_with_partner %>%
    group_by(partner, gtap_code) %>%
    summarise(
      total_imports = sum(imports),
      weighted_etr = sum(etr * imports),
      weighted_level = sum(level * imports),
      base_232 = sum(base_232),
      base_ieepa = sum(base_ieepa),
      base_neither = sum(base_neither),
      .groups = 'drop'
    ) %>%
    mutate(
      etr = if_else(total_imports > 0, weighted_etr / total_imports, 0),
      level = if_else(total_imports > 0, weighted_level / total_imports, 0)
    ) %>%
    select(partner, gtap_code, etr, level, base_232, base_ieepa, base_neither)

  return(partner_etrs)
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
prepare_sector_country_deltas <- function(etr_data) {

  etrs_wide <- etr_data %>%
    select(partner, gtap_code, etr) %>%
    pivot_wider(names_from = partner, values_from = etr, values_fill = 0) %>%
    select(gtap_code, any_of(PARTNER_ORDER_CSV)) %>%
    mutate(across(-gtap_code, ~ .x * 100))

  existing_sectors <- intersect(SECTOR_ORDER, etrs_wide$gtap_code)

  etrs_wide %>%
    filter(gtap_code %in% existing_sectors) %>%
    arrange(match(gtap_code, SECTOR_ORDER))
}


#' Write deltas to CSV in sector (rows) x country (columns) format
#'
#' Values are written in percentage points (pp), i.e., multiplied by 100.
#'
#' @param etr_data Data frame with columns: partner, gtap_code, etr
#' @param output_file Path to output file (default: 'deltas_by_sector_country.csv')
#' @param scenario Scenario name for output directory
#' @param output_base Base output directory (default: 'output')
#'
#' @return Writes CSV file (in pp units) and returns invisibly
write_sector_country_deltas <- function(etr_data,
                                         output_file = 'deltas_by_sector_country.csv',
                                         scenario = NULL,
                                         output_base = 'output') {

  output_path <- get_output_path(output_file, scenario, output_base)
  deltas_wide <- prepare_sector_country_deltas(etr_data)
  write_csv(deltas_wide, output_path)
  message(sprintf('Wrote deltas by sector and country to %s (in pp units)', output_path))
  invisible(deltas_wide)
}


#' Prepare country-level deltas (census country codes with overall deltas)
#'
#' @param hs10_country_etrs Data frame with columns: hs10, cty_code, gtap_code, imports, etr
#'
#' @return Tibble with cty_code, country_name, etr (in pp)
prepare_country_level_deltas <- function(hs10_country_etrs) {
  census_codes <- load_census_codes()

  hs10_country_etrs %>%
    group_by(cty_code) %>%
    summarise(
      total_imports = sum(imports),
      weighted_etr = sum(etr * imports),
      .groups = 'drop'
    ) %>%
    mutate(
      etr = if_else(total_imports > 0, weighted_etr / total_imports, 0) * 100
    ) %>%
    left_join(census_codes, by = 'cty_code') %>%
    select(cty_code, country_name, etr) %>%
    arrange(desc(etr))
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


#' Prepare country × HTS2 delta data in wide format (percentage points)
#'
#' @param hs10_country_etrs Data frame with columns: hs10, cty_code, gtap_code, imports, etr
#'
#' @return Tibble with cty_code, country_name, and one column per HTS chapter (in pp)
prepare_country_hts2_deltas <- function(hs10_country_etrs) {
  census_codes <- load_census_codes()

  country_hts2_etrs <- hs10_country_etrs %>%
    mutate(hts2 = str_sub(hs10, 1, 2)) %>%
    group_by(cty_code, hts2) %>%
    summarise(
      total_imports = sum(imports),
      weighted_etr = sum(etr * imports),
      .groups = 'drop'
    ) %>%
    mutate(
      etr = if_else(total_imports > 0, weighted_etr / total_imports, 0) * 100
    ) %>%
    select(cty_code, hts2, etr)

  country_hts2_wide <- country_hts2_etrs %>%
    pivot_wider(names_from = hts2, values_from = etr, values_fill = 0) %>%
    left_join(census_codes, by = 'cty_code') %>%
    relocate(cty_code, country_name)

  hts2_cols <- setdiff(names(country_hts2_wide), c('cty_code', 'country_name'))
  hts2_cols_sorted <- hts2_cols[order(as.numeric(hts2_cols))]
  country_hts2_wide %>%
    select(cty_code, country_name, all_of(hts2_cols_sorted))
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
#' @param etr_data Data frame with columns: partner, gtap_code, etr, base_232, base_ieepa, base_neither
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
  if ('base_232' %in% names(etr_data)) {
    coverage_by_partner <- etr_data %>%
      group_by(partner) %>%
      summarise(
        imports_232 = sum(base_232),
        imports_ieepa = sum(base_ieepa),
        imports_neither = sum(base_neither),
        .groups = 'drop'
      ) %>%
      mutate(
        total_imports = imports_232 + imports_ieepa + imports_neither,
        share_232 = imports_232 / total_imports,
        share_ieepa = imports_ieepa / total_imports,
        share_neither = imports_neither / total_imports
      )

    coverage_total <- etr_data %>%
      summarise(
        imports_232 = sum(base_232),
        imports_ieepa = sum(base_ieepa),
        imports_neither = sum(base_neither)
      ) %>%
      mutate(
        total_imports = imports_232 + imports_ieepa + imports_neither,
        share_232 = imports_232 / total_imports,
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
#' @param etr_data Data frame with columns: partner, gtap_code, etr, base_232, base_ieepa, base_neither
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
                                                 output_file = 'deltas_by_sector_country.csv',
                                                 scenario = NULL,
                                                 output_base = 'output') {

  output_path <- get_output_path(output_file, scenario, output_base)

  stacked <- names(all_partner_etrs) %>%
    map_df(function(date_str) {
      prepare_sector_country_deltas(all_partner_etrs[[date_str]]) %>%
        mutate(date = date_str, .before = 1)
    })

  write_csv(stacked, output_path)
  message(sprintf('Wrote stacked sector × country deltas to %s (in pp units)', output_path))
  invisible(stacked)
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
                                                output_base = 'output') {

  output_path <- get_output_path(output_file, scenario, output_base)

  stacked <- names(all_hs10_country_etrs) %>%
    map_df(function(date_str) {
      prepare_country_level_deltas(all_hs10_country_etrs[[date_str]]) %>%
        mutate(date = date_str, .before = 1)
    })

  write_csv(stacked, output_path)
  message(sprintf('Wrote stacked country-level deltas to %s (in pp units)', output_path))
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
                                               output_base = 'output') {

  output_path <- get_output_path(output_file, scenario, output_base)

  stacked <- names(all_hs10_country_etrs) %>%
    map_df(function(date_str) {
      prepare_country_hts2_deltas(all_hs10_country_etrs[[date_str]]) %>%
        mutate(date = date_str, .before = 1)
    })

  write_csv(stacked, output_path)
  message(sprintf('Wrote stacked country × HTS2 deltas to %s (in pp units)', output_path))
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

  levels_wide <- etr_data %>%
    select(partner, gtap_code, level) %>%
    pivot_wider(names_from = partner, values_from = level, values_fill = 0) %>%
    select(gtap_code, any_of(PARTNER_ORDER_CSV)) %>%
    mutate(across(-gtap_code, ~ .x * 100))

  existing_sectors <- intersect(SECTOR_ORDER, levels_wide$gtap_code)

  levels_wide %>%
    filter(gtap_code %in% existing_sectors) %>%
    arrange(match(gtap_code, SECTOR_ORDER))
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
                                            output_file = 'levels_by_sector_country.csv',
                                            scenario = NULL,
                                            output_base = 'output') {

  output_path <- get_output_path(output_file, scenario, output_base)
  levels_wide <- prepare_levels_by_sector_country(etr_data)
  write_csv(levels_wide, output_path)
  message(sprintf('Wrote tariff levels by sector and country to %s (in pp units)', output_path))
  invisible(levels_wide)
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
write_levels_by_sector_country_stacked <- function(all_partner_etrs,
                                                    output_file = 'levels_by_sector_country.csv',
                                                    scenario = NULL,
                                                    output_base = 'output') {

  output_path <- get_output_path(output_file, scenario, output_base)

  stacked <- names(all_partner_etrs) %>%
    map_df(function(date_str) {
      prepare_levels_by_sector_country(all_partner_etrs[[date_str]]) %>%
        mutate(date = date_str, .before = 1)
    })

  write_csv(stacked, output_path)
  message(sprintf('Wrote stacked sector × country tariff levels to %s (in pp units)', output_path))
  invisible(stacked)
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
