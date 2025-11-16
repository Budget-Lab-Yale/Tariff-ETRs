# =============================================================================
# calculations.R
# =============================================================================
#
# Functions for calculating effective tariff rates and generating outputs.
#
# Functions:
#   - do_scenario():                      Run complete ETR analysis for a scenario
#   - calc_import_shares():               Calculate import shares for specific HTS codes
#   - calc_weighted_etr():                Calculate weighted ETR changes by country and sector
#   - aggregate_countries_to_partners():  Aggregate country-level ETRs to partner-level
#   - write_shock_commands():             Write GTAP shock commands to output file
#   - write_sector_country_etrs():        Write ETRs to CSV in sector x country format
#   - calc_overall_etrs():                Calculate and print overall ETR changes by country
#
# =============================================================================


#' Run complete ETR analysis for a given scenario
#'
#' @param scenario Scenario name (corresponds to config/{scenario}/ directory)
#' @param import_data_path Path to Census Bureau import data files
#' @param use_cache Use cached HS10 data if available (default: TRUE)
#'
#' @return Returns ETR data invisibly
do_scenario <- function(scenario, import_data_path = 'C:/Users/jar335/Downloads', use_cache = TRUE) {

  message(sprintf('\n=========================================================='))
  message(sprintf('Running scenario: %s', scenario))
  message(sprintf('==========================================================\n'))

  #---------------------------
  # Load scenario parameters
  #---------------------------

  message('Loading scenario parameters...')

  # Section 232 tariffs
  params_232_yaml <- sprintf('config/%s/232.yaml', scenario)
  params_232 <- load_232_rates(params_232_yaml)

  # Deduplicate 232 codes to prevent double-counting
  params_232 <- deduplicate_232_codes(params_232)

  # IEEPA rates
  params_ieepa <- load_ieepa_rates_yaml(sprintf('config/%s/ieepa_rates.yaml', scenario))

  # Other scenario parameters
  other_params <- read_yaml(sprintf('config/%s/other_params.yaml', scenario))

  # USMCA share of trade by sector
  usmca_shares <- read_csv('./resources/usmca_shares.csv', show_col_types = FALSE)

  #------------------
  # Read import data
  #------------------

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
    hs10_by_country <- readRDS(cache_file)
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

      # Add GTAP code
      left_join(
        crosswalk %>%
          select(hs10, gtap_code),
        by = 'hs10'
      ) %>%
      mutate(gtap_code = str_to_lower(gtap_code)) %>%
      relocate(gtap_code, .after = hs10)

    # Save processed data to cache for next time
    message(sprintf('Saving processed data to cache at %s...', cache_file))
    saveRDS(hs10_by_country, cache_file)
    message('Cache saved successfully')
  }

  #------------------------------
  # Calculate tax bases and ETRs
  #------------------------------

  message('Calculating tax bases and ETRs...')

  # Calculate tax bases for 232 at HS10 × country level
  bases <- params_232 %>%
    names() %>%

    # Get bases for each 232 tariff
    map(~ {

      # Get HTS codes from base
      hts_codes <- params_232[[.x]]$base

      # Tag HS10 codes as covered (1) or not (0) using variable-length code matching
      coverage <- calc_import_shares(hts_codes, data = hs10_by_country) %>%
        mutate(tariff = .x, .before = 1) %>%
        rename(share = covered)  # Rename for consistency with downstream code

      return(coverage)
    }) %>%
    bind_rows() %>%

    # Add residual -- tax base uncovered by 232 (at HS10 × country level)
    bind_rows(
      (.) %>%
        group_by(hs10, cty_code) %>%
        summarise(
          tariff = 'residual',
          share  = 1 - sum(share),
          .groups = 'drop'
        )
    )

  # Calculate ETRs
  country_etrs <- calc_weighted_etr(
    bases_data            = bases,
    params_data           = params_232,
    ieepa_data            = params_ieepa,
    import_data           = hs10_by_country,
    usmca_data            = usmca_shares,
    country_mapping       = country_mapping,
    us_auto_content_share = other_params$us_auto_content_share,
    auto_rebate           = other_params$auto_rebate_rate,
    us_assembly_share     = other_params$us_auto_assembly_share,
    ieepa_usmca_exempt    = other_params$ieepa_usmca_exception
  )

  # Aggregate country ETRs to partner level for GTAP output
  message('Aggregating country-level ETRs to partner-level...')
  partner_etrs <- aggregate_countries_to_partners(
    country_etrs    = country_etrs,
    import_data     = hs10_by_country,
    country_mapping = country_mapping
  )

  #------------------
  # Write outputs
  #------------------

  message('Writing outputs...')

  # Write shock commands to file
  write_shock_commands(
    etr_data    = partner_etrs,
    output_file = 'shocks.txt',
    scenario    = scenario
  )

  # Write sector x country ETR CSV
  write_sector_country_etrs(
    etr_data     = partner_etrs,
    output_file  = 'etrs_by_sector_country.csv',
    scenario     = scenario
  )

  # Calculate and print overall ETRs with both GTAP and 2024 Census weights
  calc_overall_etrs(
    etr_data         = partner_etrs,
    import_data      = hs10_by_country,
    bases_data       = bases,
    ieepa_data       = params_ieepa,
    country_mapping  = country_mapping,
    scenario         = scenario
  )

  message(sprintf('\nScenario %s complete!\n', scenario))

  # Return partner-level ETR data invisibly
  invisible(partner_etrs)
}


#' Tag HS10 codes as covered or not covered by a set of HTS codes
#'
#' Given a vector of HTS codes of any length (4, 6, 8, or 10 digits), this function
#' determines which HS10 × country combinations are covered using prefix matching.
#'
#' Prefix matching: A code like "8703" matches all HS10 codes starting with "8703".
#' A code like "870322" matches all HS10 codes starting with "870322", and so on.
#'
#' @param hts_codes Character vector of HTS codes to analyze (4, 6, 8, or 10 digits)
#' @param data Data frame with columns: hs10, cty_code (imports column optional but ignored)
#'
#' @return Data frame with columns: hs10, cty_code, covered (1 if covered, 0 if not)
calc_import_shares <- function(hts_codes, data) {

  # Get unique HS10 × country combinations
  hs10_country <- data %>%
    select(hs10, cty_code) %>%
    distinct()

  # Handle empty code list
  if (length(hts_codes) == 0 || is.null(hts_codes)) {
    return(
      hs10_country %>%
        mutate(covered = 0)
    )
  }

  # Convert codes to character and remove any whitespace
  hts_codes <- as.character(hts_codes) %>% str_trim()

  # Build regex pattern for prefix matching
  # Each code becomes a prefix: "^8703" matches anything starting with 8703
  pattern <- paste0('^(', paste(hts_codes, collapse = '|'), ')')

  # Tag each HS10 code as covered (1) or not (0)
  result <- hs10_country %>%
    mutate(covered = if_else(str_detect(hs10, pattern), 1, 0))

  return(result)
}


#' Calculate weighted ETR changes by country and GTAP sector
#'
#' Calculates change in effective tariff rates from early 2025 baseline.
#' Works at HS10 × country level, then aggregates to GTAP using import weights.
#'
#' @param bases_data Data frame with tariff bases (hs10, cty_code, tariff, share)
#' @param params_data Tariff parameters list
#' @param ieepa_data Data frame with IEEPA rates by hs10 and cty_code
#' @param import_data Data frame with hs10, cty_code, gtap_code, imports
#' @param usmca_data Data frame with USMCA shares by partner and GTAP sector
#' @param country_mapping Data frame mapping cty_code to partner
#' @param us_auto_content_share Share of US content in auto assembly
#' @param auto_rebate Auto rebate rate
#' @param us_assembly_share Share of US assembly in autos
#' @param ieepa_usmca_exempt Apply USMCA exemption to IEEPA tariffs (1 = yes, 0 = no)
#'
#' @return Data frame with columns: cty_code, gtap_code, etr (change from baseline)
calc_weighted_etr <- function(bases_data, params_data,
                              ieepa_data, import_data,
                              usmca_data,
                              country_mapping,
                              us_auto_content_share,
                              auto_rebate,
                              us_assembly_share,
                              ieepa_usmca_exempt) {

  # Extract rates and usmca_exempt flags from params
  rates_and_exemptions <- map(names(params_data), ~ {
    tibble(
      tariff       = .x,
      cty_code     = names(params_data[[.x]]$rate),
      rate         = unlist(params_data[[.x]]$rate),
      usmca_exempt = params_data[[.x]]$usmca_exempt
    )
  }) %>%
    bind_rows()

  # Map countries to partners for USMCA lookup
  country_to_partner <- country_mapping %>%
    select(cty_code, partner) %>%
    distinct()

  # Reshape USMCA shares long by partner (this is still partner-based)
  usmca_by_partner <- usmca_data %>%
    pivot_longer(cols = -gtap_code, names_to = 'partner', values_to = 'usmca_share') %>%
    mutate(usmca_share = replace_na(usmca_share, 0))

  # Join USMCA shares to countries via partner mapping
  usmca_long <- country_to_partner %>%
    left_join(usmca_by_partner, by = 'partner') %>%
    select(cty_code, gtap_code, usmca_share) %>%
    mutate(usmca_share = replace_na(usmca_share, 0))

  # IEEPA rates come as a list with rate_matrix and default_rate
  # Extract rate_matrix and default_rate
  ieepa_rate_matrix <- ieepa_data$rate_matrix
  ieepa_default_rate <- ieepa_data$default_rate

  # Rename 'rate' column to 'ieepa_rate' for consistency
  ieepa_long <- ieepa_rate_matrix %>%
    rename(ieepa_rate = rate) %>%
    mutate(ieepa_rate = replace_na(ieepa_rate, 0))

  # Add gtap_code and imports to bases for aggregation
  bases_with_context <- bases_data %>%
    left_join(
      import_data %>% select(hs10, cty_code, gtap_code, imports),
      by = c('hs10', 'cty_code')
    )

  # Extract default rates from params for unmapped countries
  default_rates_232 <- map(names(params_data), ~ {
    tibble(
      tariff       = .x,
      default_rate = params_data[[.x]]$default_rate
    )
  }) %>%
    bind_rows()

  # Join rates, exemptions, and USMCA shares to bases
  hs10_level_data <- bases_with_context %>%
    left_join(rates_and_exemptions, by = c('tariff', 'cty_code')) %>%
    left_join(default_rates_232, by = 'tariff') %>%
    left_join(usmca_long, by = c('cty_code', 'gtap_code')) %>%
    left_join(ieepa_long, by = c('hs10', 'cty_code')) %>%
    mutate(
      # Use default rates for unmapped countries, 0 only if no default available
      rate = if_else(is.na(rate) & !is.na(default_rate), default_rate, replace_na(rate, 0)),
      usmca_exempt = replace_na(usmca_exempt, 0),
      usmca_share = replace_na(usmca_share, 0),
      # Use IEEPA default rate for unmapped countries
      ieepa_rate = replace_na(ieepa_rate, ieepa_default_rate),
      imports = replace_na(imports, 0)
    ) %>%

    # Apply auto rebate adjustment for all countries
    mutate(
      rate = if_else(
        tariff %in% c('automobiles_passenger_and_light_trucks', 'automobile_parts', 'vehicles_completed_mhd'),
        rate - (auto_rebate * us_assembly_share),
        rate
      )
    ) %>%

    # Apply USMCA exemption logic
    # Canada = 1220, Mexico = 2010
    mutate(
      adjusted_usmca_share = if_else(
        tariff %in% c('automobiles_passenger_and_light_trucks', 'automobile_parts', 'vehicles_completed_mhd'),
        usmca_share * us_auto_content_share,
        usmca_share
      ),
      adjusted_rate = if_else(
        usmca_exempt == 1 & cty_code %in% c('1220', '2010'),
        rate * (1 - adjusted_usmca_share),
        rate
      )
    ) %>%

    # Apply IEEPA rates to residual with USMCA exemption
    mutate(
      adjusted_rate = if_else(
        tariff == 'residual',
        if_else(
          ieepa_usmca_exempt == 1 & cty_code %in% c('1220', '2010'),
          ieepa_rate * (1 - adjusted_usmca_share),
          ieepa_rate
        ),
        adjusted_rate
      )
    )

  # Calculate HS10-level ETRs (sum across tariff categories)
  hs10_etrs <- hs10_level_data %>%
    group_by(hs10, cty_code, gtap_code) %>%
    summarise(
      etr_hs10 = sum(share * adjusted_rate),
      imports = first(imports),  # Same for all tariff rows in group
      .groups = 'drop'
    )

  # Aggregate to GTAP level using import-weighted average
  gtap_etrs <- hs10_etrs %>%
    group_by(cty_code, gtap_code) %>%
    summarise(
      total_imports = sum(imports),
      etr = sum(etr_hs10 * imports),
      .groups = 'drop'
    ) %>%
    mutate(
      etr = if_else(total_imports > 0, etr / total_imports, 0)
    ) %>%
    select(cty_code, gtap_code, etr)

  return(gtap_etrs)
}


#' Aggregate country-level ETRs to partner-level for GTAP output
#'
#' Takes country-level ETR × GTAP data and aggregates to 8 partner groups
#' using import-weighted averaging within each partner.
#'
#' @param country_etrs Data frame with columns: cty_code, gtap_code, etr
#' @param import_data Data frame with columns: cty_code, gtap_code, imports
#' @param country_mapping Data frame with columns: cty_code, partner
#'
#' @return Data frame with columns: partner, gtap_code, etr
aggregate_countries_to_partners <- function(country_etrs, import_data, country_mapping) {

  # Map countries to partners
  etrs_with_partner <- country_etrs %>%
    left_join(
      country_mapping %>% select(cty_code, partner) %>% distinct(),
      by = 'cty_code'
    ) %>%
    # Unmapped countries default to 'row'
    mutate(partner = if_else(is.na(partner), 'row', partner))

  # Get import weights for each country × GTAP combination
  imports_with_partner <- import_data %>%
    select(cty_code, gtap_code, imports) %>%
    left_join(
      country_mapping %>% select(cty_code, partner) %>% distinct(),
      by = 'cty_code'
    ) %>%
    # Unmapped countries default to 'row'
    mutate(partner = if_else(is.na(partner), 'row', partner))

  # Calculate import-weighted average ETRs by partner and GTAP sector
  partner_etrs <- etrs_with_partner %>%
    left_join(
      imports_with_partner %>% select(cty_code, gtap_code, imports),
      by = c('cty_code', 'gtap_code')
    ) %>%
    mutate(imports = replace_na(imports, 0)) %>%
    group_by(partner, gtap_code) %>%
    summarise(
      total_imports = sum(imports),
      weighted_etr = sum(etr * imports),
      .groups = 'drop'
    ) %>%
    mutate(
      etr = if_else(total_imports > 0, weighted_etr / total_imports, 0)
    ) %>%
    select(partner, gtap_code, etr)

  return(partner_etrs)
}


#' Write shock commands to txt file for downstream model
#'
#' @param etr_data Data frame with columns: partner, gtap_code, etr
#' @param output_file Path to output file
#' @param scenario Scenario name for output directory
#'
#' @return Writes file and returns invisibly
write_shock_commands <- function(etr_data, output_file = 'shocks.txt', scenario) {

  # Create output directory if it doesn't exist
  output_dir <- sprintf('output/%s', scenario)
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # Build full output path
  output_path <- file.path(output_dir, output_file)

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

  # Define partner order for output
  partner_order <- c('China', 'ROW', 'FTROW', 'Canada', 'Mexico', 'Japan', 'EU', 'UK')

  # Prepare data
  shock_commands <- etr_data %>%
    mutate(
      partner_fmt = partner_map[partner],
      etr_pct = round(etr * 100, 1)
    ) %>%
    filter(etr_pct > 0) %>%
    arrange(match(partner_fmt, partner_order), gtap_code) %>%
    mutate(
      command = sprintf('Shock tms("%s","%s","USA") = %.1f;', gtap_code, partner_fmt, etr_pct)
    )

  # Write to file with blank lines between partners
  con <- file(output_path, 'w')

  for (p in partner_order) {
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


#' Write ETRs to CSV in sector (rows) x country (columns) format
#'
#' Values are written in percentage points (pp), i.e., multiplied by 100.
#'
#' @param etr_data Data frame with columns: partner, gtap_code, etr
#' @param output_file Path to output file (default: 'etrs_by_sector_country.csv')
#' @param scenario Scenario name for output directory
#'
#' @return Writes CSV file (in pp units) and returns invisibly
write_sector_country_etrs <- function(etr_data,
                                       output_file = 'etrs_by_sector_country.csv',
                                       scenario = NULL) {

  # Create output directory if needed
  if (!is.null(scenario)) {
    output_dir <- sprintf('output/%s', scenario)
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    output_path <- file.path(output_dir, output_file)
  } else {
    output_path <- output_file
  }

  # Define sector order
  sector_order <- c(
    'pdr', 'wht', 'gro', 'v_f', 'osd', 'c_b', 'pfb', 'ocr', 'ctl', 'oap',
    'rmk', 'wol', 'frs', 'fsh', 'coa', 'oil', 'gas', 'oxt', 'cmt', 'omt',
    'vol', 'mil', 'pcr', 'sgr', 'ofd', 'b_t', 'tex', 'wap', 'lea', 'lum',
    'ppp', 'p_c', 'chm', 'bph', 'rpp', 'nmm', 'i_s', 'nfm', 'fmp', 'ele',
    'eeq', 'ome', 'mvh', 'otn', 'omf', 'ely', 'gdt', 'wtr', 'cns'
  )

  # Define country order
  country_order <- c('china', 'canada', 'mexico', 'uk', 'japan', 'eu', 'row', 'ftrow')

  # Pivot ETRs to wide format and convert to percentage points
  etrs_wide <- etr_data %>%
    select(partner, gtap_code, etr) %>%
    pivot_wider(names_from = partner, values_from = etr, values_fill = 0) %>%
    select(gtap_code, any_of(country_order)) %>%
    mutate(across(-gtap_code, ~ .x * 100))

  # Apply sector ordering
  existing_sectors <- intersect(sector_order, etrs_wide$gtap_code)

  etrs_wide <- etrs_wide %>%
    filter(gtap_code %in% existing_sectors) %>%
    arrange(match(gtap_code, sector_order))

  # Write CSV file
  write_csv(etrs_wide, output_path)

  message(sprintf('Wrote ETRs by sector and country to %s (in pp units)', output_path))

  invisible(etrs_wide)
}


#' Calculate and print overall ETR changes by country and total using both GTAP and 2024 Census weights
#'
#' Calculates and prints change in effective tariff rates from early 2025 baseline.
#'
#' @param etr_data Data frame with columns: partner, gtap_code, etr (change from baseline)
#' @param import_data Data frame with columns: cty_code, gtap_code, imports (2024 import values at country level)
#' @param bases_data Data frame with columns: tariff, cty_code, gtap_code, share (optional, for coverage calculation)
#' @param ieepa_data Data frame with columns: gtap_code, cty_code, rate (optional, for coverage calculation)
#' @param country_mapping Data frame with columns: cty_code, partner (for aggregating country data to partners)
#' @param weights_file Path to GTAP import weights CSV file
#' @param output_file Path to output text file (default: 'overall_etrs.txt')
#' @param scenario Scenario name for output directory
#'
#' @return Prints overall ETR changes and returns them invisibly
calc_overall_etrs <- function(etr_data, import_data = NULL,
                              bases_data = NULL, ieepa_data = NULL,
                              country_mapping = NULL,
                              weights_file = 'resources/gtap_import_weights.csv',
                              output_file = 'overall_etrs.txt',
                              scenario = NULL) {

  # ===========================
  # Calculate GTAP-weighted ETRs
  # ===========================

  # Read GTAP import weights
  gtap_weights <- read_csv(weights_file, show_col_types = FALSE)

  # Reshape weights to long format
  gtap_weights_long <- gtap_weights %>%
    pivot_longer(cols = -gtap_code, names_to = 'partner', values_to = 'import_weight') %>%
    filter(import_weight > 0)

  # Join ETRs with GTAP weights
  gtap_weighted_data <- etr_data %>%
    inner_join(gtap_weights_long, by = c('partner', 'gtap_code')) %>%
    mutate(weighted_etr = etr * import_weight)

  # Calculate overall ETR by country using GTAP weights
  gtap_country_etrs <- gtap_weighted_data %>%
    group_by(partner) %>%
    summarise(
      gtap_etr = sum(weighted_etr) / sum(import_weight),
      .groups = 'drop'
    )

  # Calculate total overall ETR using GTAP weights
  gtap_total_etr <- gtap_weighted_data %>%
    summarise(gtap_etr = sum(weighted_etr) / sum(import_weight))

  gtap_total_etr_value <- gtap_total_etr$gtap_etr

  # ===========================
  # Calculate 2024 Census-weighted ETRs
  # ===========================

  if (!is.null(import_data) && !is.null(country_mapping)) {
    # Aggregate country-level imports to partner level for Census weights
    partner_imports <- import_data %>%
      left_join(
        country_mapping %>% select(cty_code, partner) %>% distinct(),
        by = 'cty_code'
      ) %>%
      mutate(partner = if_else(is.na(partner), 'row', partner)) %>%
      group_by(partner, gtap_code) %>%
      summarise(import_weight = sum(imports), .groups = 'drop') %>%
      filter(import_weight > 0)

    # Calculate 2024 Census weights from aggregated import data
    census_weights <- partner_imports

    # Join ETRs with Census weights
    census_weighted_data <- etr_data %>%
      inner_join(census_weights, by = c('partner', 'gtap_code')) %>%
      mutate(weighted_etr = etr * import_weight)

    # Calculate overall ETR by country using Census weights
    census_country_etrs <- census_weighted_data %>%
      group_by(partner) %>%
      summarise(
        census_etr = sum(weighted_etr) / sum(import_weight),
        .groups = 'drop'
      )

    # Calculate total overall ETR using Census weights
    census_total_etr <- census_weighted_data %>%
      summarise(census_etr = sum(weighted_etr) / sum(import_weight))

    census_total_etr_value <- census_total_etr$census_etr

    # Combine both sets of results
    country_etrs <- gtap_country_etrs %>%
      left_join(census_country_etrs, by = 'partner') %>%
      arrange(desc(gtap_etr))

  } else {
    # Only GTAP weights available
    country_etrs <- gtap_country_etrs %>%
      mutate(census_etr = NA) %>%
      arrange(desc(gtap_etr))
    census_total_etr_value <- NA
  }

  # ===========================
  # Print results
  # ===========================

  cat('\n')
  cat('Overall ETRs by Country (change from early 2025 baseline):\n')
  cat('==========================================================\n')
  if (!is.null(import_data)) {
    cat(sprintf('%-10s  %15s  %18s\n', '', 'GTAP Weights', '2024 Census Weights'))
    cat(sprintf('%-10s  %15s  %18s\n', 'Country', '', ''))
    cat(sprintf('%-10s  %15s  %18s\n', '-------', '------------', '-----------------'))
  } else {
    cat(sprintf('%-10s  %15s\n', '', 'GTAP Weights'))
    cat(sprintf('%-10s  %15s\n', 'Country', ''))
    cat(sprintf('%-10s  %15s\n', '-------', '------------'))
  }

  for (i in 1:nrow(country_etrs)) {
    if (!is.null(import_data)) {
      cat(sprintf('%-10s  %14.2f%%  %17.2f%%\n',
                  toupper(country_etrs$partner[i]),
                  country_etrs$gtap_etr[i] * 100,
                  country_etrs$census_etr[i] * 100))
    } else {
      cat(sprintf('%-10s  %14.2f%%\n',
                  toupper(country_etrs$partner[i]),
                  country_etrs$gtap_etr[i] * 100))
    }
  }
  cat('\n')
  if (!is.null(import_data)) {
    cat(sprintf('%-10s  %14.2f%%  %17.2f%%\n',
                'TOTAL',
                gtap_total_etr_value * 100,
                census_total_etr_value * 100))
  } else {
    cat(sprintf('%-10s  %14.2f%%\n',
                'TOTAL',
                gtap_total_etr_value * 100))
  }
  cat('\n')

  # ===========================
  # Calculate and print tariff coverage
  # ===========================

  coverage_stats <- NULL

  if (!is.null(bases_data) && !is.null(ieepa_data) && !is.null(import_data) && !is.null(country_mapping)) {

    # Join bases with imports and IEEPA rates at HS10 × country level
    coverage_data <- bases_data %>%
      left_join(
        import_data %>% select(hs10, cty_code, gtap_code, imports),
        by = c('hs10', 'cty_code')
      ) %>%
      left_join(
        # Extract rate_matrix from ieepa_data list
        ieepa_data$rate_matrix %>% select(hs10, cty_code, rate),
        by = c('hs10', 'cty_code')
      ) %>%
      left_join(
        country_mapping %>% select(cty_code, partner) %>% distinct(),
        by = 'cty_code'
      ) %>%
      mutate(
        partner = if_else(is.na(partner), 'row', partner),
        imports = replace_na(imports, 0),
        rate = replace_na(rate, 0)
      )

    # Calculate coverage by partner (aggregate from country level)
    coverage_by_partner <- coverage_data %>%
      mutate(
        # Calculate import value covered by each tariff category
        import_value = imports * share,
        is_232 = tariff != 'residual',
        is_ieepa = tariff == 'residual' & rate > 0,
        is_neither = tariff == 'residual' & rate == 0
      ) %>%
      group_by(partner) %>%
      summarise(
        total_imports = sum(import_value),
        imports_232 = sum(import_value * is_232),
        imports_ieepa = sum(import_value * is_ieepa),
        imports_neither = sum(import_value * is_neither),
        .groups = 'drop'
      ) %>%
      mutate(
        share_232 = imports_232 / total_imports,
        share_ieepa = imports_ieepa / total_imports,
        share_neither = imports_neither / total_imports
      )

    # Calculate total coverage across all partners
    coverage_total <- coverage_data %>%
      mutate(
        import_value = imports * share,
        is_232 = tariff != 'residual',
        is_ieepa = tariff == 'residual' & rate > 0,
        is_neither = tariff == 'residual' & rate == 0
      ) %>%
      summarise(
        total_imports = sum(import_value),
        imports_232 = sum(import_value * is_232),
        imports_ieepa = sum(import_value * is_ieepa),
        imports_neither = sum(import_value * is_neither)
      ) %>%
      mutate(
        share_232 = imports_232 / total_imports,
        share_ieepa = imports_ieepa / total_imports,
        share_neither = imports_neither / total_imports
      )

    coverage_stats <- list(
      by_partner = coverage_by_partner,
      total = coverage_total
    )

    # Print coverage table
    cat('\n')
    cat('Tariff Coverage by Country (fraction of 2024 import value):\n')
    cat('==========================================================\n')
    cat(sprintf('%-10s  %12s  %12s  %12s\n', 'Country', 'Under 232', 'Under IEEPA', 'Neither'))
    cat(sprintf('%-10s  %12s  %12s  %12s\n', '-------', '---------', '-----------', '-------'))

    for (i in 1:nrow(coverage_by_partner)) {
      cat(sprintf('%-10s  %11.1f%%  %11.1f%%  %11.1f%%\n',
                  toupper(coverage_by_partner$partner[i]),
                  coverage_by_partner$share_232[i] * 100,
                  coverage_by_partner$share_ieepa[i] * 100,
                  coverage_by_partner$share_neither[i] * 100))
    }

    cat('\n')
    cat(sprintf('%-10s  %11.1f%%  %11.1f%%  %11.1f%%\n',
                'TOTAL',
                coverage_total$share_232 * 100,
                coverage_total$share_ieepa * 100,
                coverage_total$share_neither * 100))
    cat('\n')
  }

  # ===========================
  # Write results to file
  # ===========================

  if (!is.null(scenario)) {
    output_dir <- sprintf('output/%s', scenario)
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    output_path <- file.path(output_dir, output_file)
  } else {
    output_path <- output_file
  }

  con <- file(output_path, 'w')

  writeLines('Overall ETRs by Country (change from early 2025 baseline):', con)
  writeLines('==========================================================', con)
  writeLines('', con)

  if (!is.null(import_data)) {
    writeLines(sprintf('%-10s  %15s  %18s', '', 'GTAP Weights', '2024 Census Weights'), con)
    writeLines(sprintf('%-10s  %15s  %18s', 'Country', '', ''), con)
    writeLines(sprintf('%-10s  %15s  %18s', '-------', '------------', '-----------------'), con)
  } else {
    writeLines(sprintf('%-10s  %15s', '', 'GTAP Weights'), con)
    writeLines(sprintf('%-10s  %15s', 'Country', ''), con)
    writeLines(sprintf('%-10s  %15s', '-------', '------------'), con)
  }

  for (i in 1:nrow(country_etrs)) {
    if (!is.null(import_data)) {
      writeLines(sprintf('%-10s  %14.2f%%  %17.2f%%',
                         toupper(country_etrs$partner[i]),
                         country_etrs$gtap_etr[i] * 100,
                         country_etrs$census_etr[i] * 100), con)
    } else {
      writeLines(sprintf('%-10s  %14.2f%%',
                         toupper(country_etrs$partner[i]),
                         country_etrs$gtap_etr[i] * 100), con)
    }
  }

  writeLines('', con)
  if (!is.null(import_data)) {
    writeLines(sprintf('%-10s  %14.2f%%  %17.2f%%',
                       'TOTAL',
                       gtap_total_etr_value * 100,
                       census_total_etr_value * 100), con)
  } else {
    writeLines(sprintf('%-10s  %14.2f%%',
                       'TOTAL',
                       gtap_total_etr_value * 100), con)
  }

  # Write coverage table if available
  if (!is.null(coverage_stats)) {
    writeLines('', con)
    writeLines('', con)
    writeLines('Tariff Coverage by Country (fraction of 2024 import value):', con)
    writeLines('==========================================================', con)
    writeLines(sprintf('%-10s  %12s  %12s  %12s', 'Country', 'Under 232', 'Under IEEPA', 'Neither'), con)
    writeLines(sprintf('%-10s  %12s  %12s  %12s', '-------', '---------', '-----------', '-------'), con)

    coverage_by_partner <- coverage_stats$by_partner
    for (i in 1:nrow(coverage_by_partner)) {
      writeLines(sprintf('%-10s  %11.1f%%  %11.1f%%  %11.1f%%',
                         toupper(coverage_by_partner$partner[i]),
                         coverage_by_partner$share_232[i] * 100,
                         coverage_by_partner$share_ieepa[i] * 100,
                         coverage_by_partner$share_neither[i] * 100), con)
    }

    writeLines('', con)
    coverage_total <- coverage_stats$total
    writeLines(sprintf('%-10s  %11.1f%%  %11.1f%%  %11.1f%%',
                       'TOTAL',
                       coverage_total$share_232 * 100,
                       coverage_total$share_ieepa * 100,
                       coverage_total$share_neither * 100), con)
  }

  close(con)

  message(sprintf('Wrote overall ETRs to %s', output_path))

  # Return results invisibly
  invisible(list(
    by_country = country_etrs,
    gtap_total = gtap_total_etr_value,
    census_total = if (!is.null(import_data)) census_total_etr_value else NA
  ))
}
