# =============================================================================
# scenario_workflow.R
# =============================================================================
#
# Scenario orchestration: loads scenario definitions from YAML, processes
# baseline and counterfactual configs, computes deltas, and writes outputs.
#
# Functions:
#   - load_scenario_definition():  Load scenario.yaml and normalize entries
#   - do_scenario():               Main entry point (dispatcher)
#   - match_baseline():            Match counterfactual date to baseline result
#   - run_counterfactual():        Run one counterfactual against baseline
#   - do_scenario_static():        Single counterfactual workflow
#   - do_scenario_time_varying():  Multiple counterfactual dates workflow
#
# =============================================================================

#' Load scenario definition from scenario.yaml
#'
#' Reads scenario.yaml and normalizes counterfactual entries to a uniform
#' list of {date, historical, reform} structures. Validates that referenced
#' historical dates exist.
#'
#' @param scenario_dir Path to scenario directory (contains scenario.yaml)
#' @param historical_dir Path to historical configs directory
#'
#' @return Named list with:
#'   - baseline_date: character string (YYYY-MM-DD)
#'   - counterfactual: list of lists, each with date, historical, reform (or NULL)
#'   - series_horizon: character string or NULL
load_scenario_definition <- function(scenario_dir, historical_dir) {
  yaml_path <- file.path(scenario_dir, 'scenario.yaml')
  if (!file.exists(yaml_path)) {
    stop(sprintf('scenario.yaml not found in %s', scenario_dir))
  }

  def <- read_yaml(yaml_path)

  # Validate baseline
  if (is.null(def$baseline)) {
    stop('scenario.yaml must specify a baseline date')
  }
  baseline_date <- as.character(def$baseline)
  baseline_path <- file.path(historical_dir, baseline_date)
  if (!dir.exists(baseline_path)) {
    stop(sprintf('Baseline historical date not found: %s (expected %s)', baseline_date, baseline_path))
  }

  # Normalize counterfactual entries
  if (is.null(def$counterfactual)) {
    stop('scenario.yaml must specify at least one counterfactual entry')
  }

  counterfactual <- lapply(def$counterfactual, function(entry) {
    if (is.character(entry)) {
      # String shorthand: date = historical date, no reform
      list(date = entry, historical = entry, reform = NULL)
    } else if (is.list(entry)) {
      if (is.null(entry$date)) {
        stop('Each counterfactual entry must have a date field')
      }
      date_val <- as.character(entry$date)
      if (length(date_val) != 1 || !grepl('^\\d{4}-\\d{2}-\\d{2}$', date_val)) {
        stop(sprintf('Invalid date in counterfactual entry: "%s"', paste(date_val, collapse = ', ')))
      }
      historical_val <- if (!is.null(entry$historical)) as.character(entry$historical) else date_val
      reform_val <- if (!is.null(entry$reform)) as.character(entry$reform) else NULL
      list(date = date_val, historical = historical_val, reform = reform_val)
    } else {
      stop(sprintf('Invalid counterfactual entry: %s', entry))
    }
  })

  # Validate all referenced historical dates exist
  all_historical <- unique(c(baseline_date, vapply(counterfactual, function(e) e$historical, character(1))))
  for (h in all_historical) {
    h_path <- file.path(historical_dir, h)
    if (!dir.exists(h_path)) {
      stop(sprintf('Historical date not found: %s (expected %s)', h, h_path))
    }
  }

  # Validate reform paths exist (if specified)
  for (entry in counterfactual) {
    if (!is.null(entry$reform)) {
      reform_path <- file.path(scenario_dir, entry$reform)
      if (!dir.exists(reform_path)) {
        stop(sprintf('Reform directory not found: %s', reform_path))
      }
    }
  }

  list(
    baseline_date   = baseline_date,
    counterfactual  = counterfactual,
    series_horizon  = def$series_horizon
  )
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
  # Load scenario definition from scenario.yaml
  #---------------------------

  historical_dir <- file.path(config_dir, 'historical')
  scenario_dir <- file.path(config_dir, 'scenarios', scenario)
  scenario_def <- load_scenario_definition(scenario_dir, historical_dir)

  counter_dates <- vapply(scenario_def$counterfactual, function(e) e$date, character(1))
  message(sprintf('Baseline: %s', scenario_def$baseline_date))
  message(sprintf('Counterfactual dates: %s', paste(counter_dates, collapse = ', ')))

  #---------------------------
  # Process baseline (always a single historical date)
  #---------------------------

  message('\n--- Processing baseline ---')
  baseline_path <- file.path(historical_dir, scenario_def$baseline_date)
  baseline_config <- load_scenario_config(baseline_path)
  baseline_etrs <- calc_etrs_for_config(baseline_config, hs10_by_country, country_mapping)
  baseline_results <- list(static = baseline_etrs$hs10_country_etrs)
  baseline_partner_results <- list(static = baseline_etrs$partner_etrs)

  #---------------------------
  # Write baseline levels output
  #---------------------------

  message('\nWriting baseline levels...')
  write_levels_by_sector_country(
    etr_data    = baseline_partner_results[['static']],
    output_file = 'gtap_levels_by_sector_country.csv',
    scenario    = 'baseline',
    output_base = output_dir
  )
  write_country_level_levels(
    hs10_country_etrs = baseline_results[['static']],
    output_file       = 'levels_by_census_country.csv',
    scenario          = 'baseline',
    output_base       = output_dir
  )
  write_country_hts2_levels(
    hs10_country_etrs = baseline_results[['static']],
    output_file       = 'levels_by_census_country_hts2.csv',
    scenario          = 'baseline',
    output_base       = output_dir
  )
  write_overall_levels(
    etr_data          = baseline_partner_results[['static']],
    hs10_country_etrs = baseline_results[['static']],
    country_mapping   = country_mapping,
    scenario          = 'baseline',
    output_base       = output_dir
  )

  #---------------------------
  # Load counterfactual configs
  #---------------------------

  message('\n--- Loading counterfactual configs ---')
  counter_configs <- list()
  for (entry in scenario_def$counterfactual) {
    message(sprintf('\nLoading config for %s (historical: %s%s)',
                    entry$date, entry$historical,
                    if (!is.null(entry$reform)) sprintf(', reform: %s', entry$reform) else ''))
    historical_path <- file.path(historical_dir, entry$historical)
    reform_path <- if (!is.null(entry$reform)) file.path(scenario_dir, entry$reform) else NULL
    counter_configs[[entry$date]] <- load_config_with_reform(historical_path, reform_path)
  }

  #---------------------------
  # Dispatch to static or time-varying counterfactual
  #---------------------------

  if (length(scenario_def$counterfactual) == 1) {
    # Single counterfactual entry = static scenario
    result <- do_scenario_static(
      scenario         = scenario,
      config           = counter_configs[[counter_dates[1]]],
      output_dir       = output_dir,
      hs10_by_country  = hs10_by_country,
      country_mapping  = country_mapping,
      baseline_results = baseline_results
    )
  } else {
    # Multiple counterfactual entries = time-varying
    message(sprintf('\nTime-varying counterfactual with %d dates: %s',
                    length(counter_dates), paste(counter_dates, collapse = ', ')))
    result <- do_scenario_time_varying(
      scenario         = scenario,
      counter_configs  = counter_configs,
      counter_dates    = counter_dates,
      output_dir       = output_dir,
      hs10_by_country  = hs10_by_country,
      country_mapping  = country_mapping,
      baseline_results = baseline_results,
      series_horizon   = scenario_def$series_horizon
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

#' Run a counterfactual config against the matched baseline
#'
#' Shared helper for static and time-varying counterfactual workflows.
#'
#' @param config Pre-loaded config list (from load_scenario_config or load_config_with_reform)
#' @param baseline_key Baseline matcher key ('static' or YYYY-MM-DD)
#' @param hs10_by_country Pre-loaded import data
#' @param country_mapping Pre-loaded country mapping
#' @param baseline_results Named list of baseline HS10-country results
#' @param write_shocks Whether to write shocks.txt for this run
#' @param shock_scenario Scenario path segment for shock output
#' @param output_dir Base output directory for shock output
#'
#' @return Named list with hs10_country_etrs and partner_etrs
run_counterfactual <- function(config, baseline_key,
                               hs10_by_country, country_mapping,
                               baseline_results,
                               write_shocks = FALSE,
                               shock_scenario = NULL,
                               output_dir = 'output') {

  etrs <- calc_etrs_for_config(config, hs10_by_country, country_mapping)

  baseline_hs10 <- match_baseline(baseline_key, baseline_results)
  hs10_country_etrs <- calc_delta(baseline_hs10, etrs$hs10_country_etrs)

  partner_etrs <- aggregate_countries_to_partners(
    hs10_country_etrs = hs10_country_etrs,
    country_mapping   = country_mapping
  )

  if (write_shocks) {
    if (is.null(shock_scenario)) {
      stop('shock_scenario is required when write_shocks = TRUE')
    }
    write_shock_commands(
      etr_data    = partner_etrs,
      output_file = 'shocks.txt',
      scenario    = shock_scenario,
      output_base = output_dir
    )
  }

  list(
    hs10_country_etrs = hs10_country_etrs,
    partner_etrs = partner_etrs
  )
}

#' Run a static (non-time-varying) counterfactual scenario
#'
#' @param scenario Scenario name
#' @param config Pre-loaded counterfactual config
#' @param output_dir Base output directory
#' @param hs10_by_country Pre-loaded import data
#' @param country_mapping Pre-loaded country mapping
#' @param baseline_results Named list of baseline HS10×country results
#'
#' @return partner_etrs invisibly
do_scenario_static <- function(scenario, config, output_dir,
                                hs10_by_country, country_mapping,
                                baseline_results) {

  counterfactual <- run_counterfactual(
    config           = config,
    baseline_key     = 'static',
    hs10_by_country  = hs10_by_country,
    country_mapping  = country_mapping,
    baseline_results = baseline_results
  )
  hs10_country_etrs <- counterfactual$hs10_country_etrs
  partner_etrs <- counterfactual$partner_etrs

  message('Writing outputs...')

  write_shock_commands(
    etr_data    = partner_etrs,
    output_file = 'shocks.txt',
    scenario    = scenario,
    output_base = output_dir
  )

  write_sector_country_deltas(
    etr_data    = partner_etrs,
    output_file = 'gtap_deltas_by_sector_country.csv',
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
    output_file = 'gtap_levels_by_sector_country.csv',
    scenario    = scenario,
    output_base = output_dir
  )

  write_country_level_levels(
    hs10_country_etrs = hs10_country_etrs,
    output_file       = 'levels_by_census_country.csv',
    scenario          = scenario,
    output_base       = output_dir
  )

  write_country_hts2_levels(
    hs10_country_etrs = hs10_country_etrs,
    output_file       = 'levels_by_census_country_hts2.csv',
    scenario          = scenario,
    output_base       = output_dir
  )

  write_overall_levels(
    etr_data          = partner_etrs,
    hs10_country_etrs = hs10_country_etrs,
    country_mapping   = country_mapping,
    scenario          = scenario,
    output_base       = output_dir
  )

  write_bea_commodity_deltas(
    hs10_country_etrs = hs10_country_etrs,
    scenario          = scenario,
    output_base       = output_dir
  )

  partner_etrs
}

#' Run a time-varying counterfactual scenario
#'
#' Processes pre-loaded configs for each counterfactual date.
#' Shock files are written per-date; CSVs are stacked with a date column.
#'
#' @param scenario Scenario name
#' @param counter_configs Named list of pre-loaded configs (keyed by date string)
#' @param counter_dates Sorted character vector of date strings (YYYY-MM-DD)
#' @param output_dir Base output directory
#' @param hs10_by_country Pre-loaded import data
#' @param country_mapping Pre-loaded country mapping
#' @param baseline_results Named list of baseline HS10×country results
#' @param series_horizon End date for validity intervals (from scenario.yaml)
#'
#' @return Named list of per-date partner_etrs
do_scenario_time_varying <- function(scenario, counter_configs, counter_dates,
                                      output_dir, hs10_by_country,
                                      country_mapping,
                                      baseline_results,
                                      series_horizon = NULL) {

  # Collect per-date results
  all_partner_etrs      <- list()
  all_hs10_country_etrs <- list()
  all_overall_data      <- list()
  all_levels_data       <- list()

  for (date_str in counter_dates) {
    message(sprintf('\n--- Processing counterfactual date: %s ---', date_str))

    counterfactual <- run_counterfactual(
      config           = counter_configs[[date_str]],
      baseline_key     = 'static',
      hs10_by_country  = hs10_by_country,
      country_mapping  = country_mapping,
      baseline_results = baseline_results,
      write_shocks     = TRUE,
      shock_scenario   = file.path(scenario, date_str),
      output_dir       = output_dir
    )
    hs10_country_etrs <- counterfactual$hs10_country_etrs
    partner_etrs <- counterfactual$partner_etrs

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

  # Compute validity intervals from sorted counter_dates
  # Each date is valid from itself until the day before the next date.
  # The last date uses series_horizon from scenario.yaml (or defaults to +1 year).
  if (is.null(series_horizon)) {
    series_horizon <- as.character(as.Date(counter_dates[length(counter_dates)]) + 365)
  }

  date_intervals <- tibble(
    date = counter_dates,
    valid_from = counter_dates,
    valid_until = c(
      as.character(as.Date(counter_dates[-1]) - 1),
      as.character(as.Date(series_horizon))
    )
  )

  # Write stacked outputs
  message('\nWriting stacked outputs...')

  write_sector_country_deltas_stacked(
    all_partner_etrs = all_partner_etrs,
    output_file      = 'gtap_deltas_by_sector_country.csv',
    scenario         = scenario,
    output_base      = output_dir,
    date_intervals   = date_intervals
  )

  write_country_level_deltas_stacked(
    all_hs10_country_etrs = all_hs10_country_etrs,
    output_file           = 'deltas_by_census_country.csv',
    scenario              = scenario,
    output_base           = output_dir,
    date_intervals        = date_intervals
  )

  write_country_hts2_deltas_stacked(
    all_hs10_country_etrs = all_hs10_country_etrs,
    output_file           = 'deltas_by_census_country_hts2.csv',
    scenario              = scenario,
    output_base           = output_dir,
    date_intervals        = date_intervals
  )

  write_overall_deltas_combined(
    all_overall_data = all_overall_data,
    scenario         = scenario,
    output_base      = output_dir
  )

  write_levels_by_sector_country_stacked(
    all_partner_etrs = all_partner_etrs,
    output_file      = 'gtap_levels_by_sector_country.csv',
    scenario         = scenario,
    output_base      = output_dir,
    date_intervals   = date_intervals
  )

  write_country_level_levels_stacked(
    all_hs10_country_etrs = all_hs10_country_etrs,
    output_file           = 'levels_by_census_country.csv',
    scenario              = scenario,
    output_base           = output_dir,
    date_intervals        = date_intervals
  )

  write_country_hts2_levels_stacked(
    all_hs10_country_etrs = all_hs10_country_etrs,
    output_file           = 'levels_by_census_country_hts2.csv',
    scenario              = scenario,
    output_base           = output_dir,
    date_intervals        = date_intervals
  )

  write_overall_levels_combined(
    all_levels_data = all_levels_data,
    scenario        = scenario,
    output_base     = output_dir
  )

  write_bea_commodity_deltas_stacked(
    all_hs10_country_etrs = all_hs10_country_etrs,
    scenario              = scenario,
    output_base           = output_dir,
    date_intervals        = date_intervals
  )

  all_partner_etrs
}
