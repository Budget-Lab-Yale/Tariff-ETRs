# =============================================================================
# calculations.R
# =============================================================================
#
# Functions for calculating effective tariff rates and generating outputs.
#
# Functions:
#   - do_scenario():                      Run complete ETR analysis for a scenario
#   - calc_import_shares():               Calculate import shares for specific HTS codes
#   - calc_weighted_etr():                Calculate weighted ETR changes at HS10 × country level
#   - aggregate_countries_to_partners():  Aggregate HS10×country ETRs to partner×GTAP level
#   - write_shock_commands():             Write GTAP shock commands to output file
#   - write_sector_country_etrs():        Write ETRs to CSV in sector x country format
#   - write_country_level_etrs():         Write overall country-level ETRs to CSV (census country codes)
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

  # Section 232 tariffs (returns list with rate_matrix and usmca_exempt)
  params_232 <- load_232_rates(sprintf('config/%s/232.yaml', scenario))

  # IEEPA Reciprocal rates (returns tibble with hs10, cty_code, ieepa_reciprocal_rate)
  rates_ieepa_reciprocal <- load_ieepa_rates_yaml(
    sprintf('config/%s/ieepa_reciprocal.yaml', scenario),
    rate_col_name = 'ieepa_reciprocal_rate'
  )

  # IEEPA Fentanyl rates (returns tibble with hs10, cty_code, ieepa_fentanyl_rate)
  rates_ieepa_fentanyl <- load_ieepa_rates_yaml(
    sprintf('config/%s/ieepa_fentanyl.yaml', scenario),
    rate_col_name = 'ieepa_fentanyl_rate'
  )

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

  #------------------------------
  # Calculate ETRs at HS10 × country level
  #------------------------------

  message('Calculating ETRs at HS10 × country level...')

  # Calculate ETRs using tabular config data
  hs10_country_etrs <- calc_weighted_etr(
    rates_232              = params_232$rate_matrix,
    usmca_exempt_232       = params_232$usmca_exempt,
    rates_ieepa_reciprocal = rates_ieepa_reciprocal,
    rates_ieepa_fentanyl   = rates_ieepa_fentanyl,
    import_data            = hs10_by_country,
    usmca_data             = usmca_shares,
    us_auto_content_share  = other_params$us_auto_content_share,
    auto_rebate            = other_params$auto_rebate_rate,
    us_assembly_share      = other_params$us_auto_assembly_share,
    ieepa_usmca_exempt     = other_params$ieepa_usmca_exception
  )

  # Aggregate HS10×country ETRs to partner×GTAP level for GTAP output
  message('Aggregating HS10×country ETRs to partner×GTAP level...')
  partner_etrs <- aggregate_countries_to_partners(
    hs10_country_etrs = hs10_country_etrs,
    country_mapping   = country_mapping
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

  # Write country-level ETR CSV (census country codes with overall ETRs)
  write_country_level_etrs(
    hs10_country_etrs = hs10_country_etrs,
    output_file       = 'etrs_by_census_country.csv',
    scenario          = scenario
  )

  # Calculate and print overall ETRs with both GTAP and 2024 Census weights
  calc_overall_etrs(
    etr_data          = partner_etrs,
    hs10_country_etrs = hs10_country_etrs,
    country_mapping   = country_mapping,
    scenario          = scenario
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


#' Calculate weighted ETR changes at HS10 × country level using tabular config data
#'
#' Takes clean tabular rate data from config parsing and applies USMCA exemptions,
#' auto rebates, and stacking rules to produce final ETRs.
#'
#' @param rates_232 Tibble with columns: hs10, cty_code, s232_[tariff]_rate (one per tariff)
#' @param usmca_exempt_232 Named vector of USMCA exempt flags by tariff name
#' @param rates_ieepa_reciprocal Tibble with columns: hs10, cty_code, ieepa_reciprocal_rate
#' @param rates_ieepa_fentanyl Tibble with columns: hs10, cty_code, ieepa_fentanyl_rate
#' @param import_data Data frame with hs10, cty_code, gtap_code, imports
#' @param usmca_data Data frame with USMCA shares by partner and GTAP sector
#' @param us_auto_content_share Share of US content in auto assembly
#' @param auto_rebate Auto rebate rate
#' @param us_assembly_share Share of US assembly in autos
#' @param ieepa_usmca_exempt Apply USMCA exemption to IEEPA tariffs (1 = yes, 0 = no)
#'
#' @return Data frame with columns: hs10, cty_code, gtap_code, imports, etr, base_232, base_ieepa, base_neither
calc_weighted_etr <- function(rates_232,
                              usmca_exempt_232,
                              rates_ieepa_reciprocal,
                              rates_ieepa_fentanyl,
                              import_data,
                              usmca_data,
                              us_auto_content_share,
                              auto_rebate,
                              us_assembly_share,
                              ieepa_usmca_exempt) {

  # =============================================================================
  # Build complete rate matrix by joining config tables with import data
  # Config tables are now sparse (only non-zero rates), so NAs from joins = 0
  # =============================================================================

  rate_matrix <- import_data %>%
    select(hs10, cty_code, gtap_code, imports) %>%
    left_join(rates_232, by = c('hs10', 'cty_code')) %>%
    left_join(rates_ieepa_reciprocal, by = c('hs10', 'cty_code')) %>%
    left_join(rates_ieepa_fentanyl, by = c('hs10', 'cty_code'))

  # Get all 232 rate column names
  rate_232_cols <- names(rate_matrix)[str_detect(names(rate_matrix), '^s232_.*_rate$')]

  # Replace NAs (indicates no tariff) with 0 for all rate columns
  rate_matrix <- rate_matrix %>%
    mutate(
      across(
        .cols = all_of(c(rate_232_cols, 'ieepa_reciprocal_rate', 'ieepa_fentanyl_rate')), 
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
        partner == 'canada' ~ '1220',
        partner == 'mexico' ~ '2010',
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
              cty_code %in% c('1220', '2010'),
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
              cty_code %in% c('1220', '2010'),
              !!sym(rate_col) * (1 - usmca_share),
              !!sym(rate_col)
            )
          )
      }
    }
  }

  # Apply USMCA exemption to both IEEPA tariffs if enabled
  if (ieepa_usmca_exempt == 1) {
    rate_matrix <- rate_matrix %>%
      mutate(
        ieepa_reciprocal_rate = if_else(
          cty_code %in% c('1220', '2010'),
          ieepa_reciprocal_rate * (1 - usmca_share),
          ieepa_reciprocal_rate
        ),
        ieepa_fentanyl_rate = if_else(
          cty_code %in% c('1220', '2010'),
          ieepa_fentanyl_rate * (1 - usmca_share),
          ieepa_fentanyl_rate
        )
      )
  }

  # Clean up USMCA share column
  rate_matrix <- rate_matrix %>%
    select(-usmca_share)

  
  # =============================================================================
  # Apply stacking rules to calculate final_rate
  # =============================================================================

  # Calculate max of all 232 rates
  if (length(tariff_names) > 0) {
    
    # Get all 232 rate column names
    rate_232_cols <- paste0('s232_', tariff_names, '_rate')

    rate_matrix <- rate_matrix %>%
      mutate(
        
        # Max 232 rate across all tariffs
        rate_232_max = pmax(!!!syms(rate_232_cols)),
        rate_232_max = if_else(is.infinite(rate_232_max), 0, rate_232_max)
      )
    
  # No 232 tariffs - set max to 0
  } else {
    rate_matrix <- rate_matrix %>%
      mutate(rate_232_max = 0)
  }

  rate_matrix <- rate_matrix %>%
    mutate(

      # Stacking rules:
      # 1. IEEPA Reciprocal: Mutually exclusive with 232 (applies only to uncovered base)
      # 2. IEEPA Fentanyl:
      #    - China (5700): STACKS on top of everything (232 + reciprocal + fentanyl)
      #    - Others: Only applies to base not covered by 232 or reciprocal

      final_rate = case_when(
        # China: Fentanyl stacks on top of normal 232-repicorical logic
        cty_code == '5700' ~ if_else(rate_232_max > 0, rate_232_max, ieepa_reciprocal_rate) + ieepa_fentanyl_rate,

        # Everyone else: 232 takes precedence, then reciprocal + fentanyl
        # If 232 applies, use 232
        rate_232_max > 0 ~ rate_232_max,

        # Otherwise use all IEEPA
        TRUE ~ ieepa_reciprocal_rate + ieepa_fentanyl_rate
      )
    )

  # =============================================================================
  # Calculate ETR and coverage tracking
  # =============================================================================

  hs10_country_etrs <- rate_matrix %>%
    mutate(
      etr = final_rate,

      # Coverage tracking (mutually exclusive categories for reporting)
      # Note: For China, fentanyl stacks on top, but for coverage we track primary authority
      base_232     = if_else(rate_232_max > 0, imports, 0),
      base_ieepa   = if_else(rate_232_max == 0 & (ieepa_reciprocal_rate > 0 | ieepa_fentanyl_rate > 0), imports, 0),
      base_neither = if_else(rate_232_max == 0 & ieepa_reciprocal_rate == 0 & ieepa_fentanyl_rate == 0, imports, 0)
    ) %>%
    select(hs10, cty_code, gtap_code, imports, etr, base_232, base_ieepa, base_neither)

  return(hs10_country_etrs)
}


#' Aggregate HS10×country ETRs to partner×GTAP level for GTAP output
#'
#' Takes HS10-level ETR data and aggregates to 8 partner groups × GTAP sectors
#' using import-weighted averaging. Also aggregates coverage bases for tariff
#' coverage reporting.
#'
#' @param hs10_country_etrs Data frame with columns: hs10, cty_code, gtap_code, imports, etr, base_232, base_ieepa, base_neither
#' @param country_mapping Data frame with columns: cty_code, partner
#'
#' @return Data frame with columns: partner, gtap_code, etr, base_232, base_ieepa, base_neither
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
  partner_etrs <- etrs_with_partner %>%
    group_by(partner, gtap_code) %>%
    summarise(
      total_imports = sum(imports),
      weighted_etr = sum(etr * imports),
      base_232 = sum(base_232),
      base_ieepa = sum(base_ieepa),
      base_neither = sum(base_neither),
      .groups = 'drop'
    ) %>%
    mutate(
      etr = if_else(total_imports > 0, weighted_etr / total_imports, 0)
    ) %>%
    select(partner, gtap_code, etr, base_232, base_ieepa, base_neither)

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
    filter(etr_pct > 0, !is.na(gtap_code)) %>%
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


#' Write country-level ETRs to CSV (census country codes with overall ETRs)
#'
#' Calculates overall ETR for each census country code using 2024 census import weights.
#' Outputs a simple two-column CSV: cty_code, etr (in percentage points).
#'
#' @param hs10_country_etrs Data frame with columns: hs10, cty_code, gtap_code, imports, etr
#' @param output_file Path to output file (default: 'etrs_by_census_country.csv')
#' @param scenario Scenario name for output directory
#'
#' @return Writes CSV file (in pp units) and returns invisibly
write_country_level_etrs <- function(hs10_country_etrs,
                                      output_file = 'etrs_by_census_country.csv',
                                      scenario = NULL) {

  # Create output directory if needed
  if (!is.null(scenario)) {
    output_dir <- sprintf('output/%s', scenario)
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    output_path <- file.path(output_dir, output_file)
  } else {
    output_path <- output_file
  }

  # Calculate overall ETR by country using census import weights
  # (aggregates across all sectors and HS10 codes)
  country_etrs <- hs10_country_etrs %>%
    group_by(cty_code) %>%
    summarise(
      total_imports = sum(imports),
      weighted_etr = sum(etr * imports),
      .groups = 'drop'
    ) %>%
    mutate(
      etr = if_else(total_imports > 0, weighted_etr / total_imports, 0) * 100
    ) %>%
    select(cty_code, etr) %>%
    arrange(desc(etr))

  # Write CSV file
  write_csv(country_etrs, output_path)

  message(sprintf('Wrote country-level ETRs to %s (in pp units)', output_path))

  invisible(country_etrs)
}


#' Calculate and print overall ETR changes by country and total using both GTAP and 2024 Census weights
#'
#' Calculates and prints change in effective tariff rates from early 2025 baseline.
#'
#' @param etr_data Data frame with columns: partner, gtap_code, etr, base_232, base_ieepa, base_neither
#' @param hs10_country_etrs Data frame with columns: hs10, cty_code, gtap_code, imports, etr (for Census weights)
#' @param country_mapping Data frame with columns: cty_code, partner (for aggregating country data to partners)
#' @param weights_file Path to GTAP import weights CSV file
#' @param output_file Path to output text file (default: 'overall_etrs.txt')
#' @param scenario Scenario name for output directory
#'
#' @return Prints overall ETR changes and returns them invisibly
calc_overall_etrs <- function(etr_data, hs10_country_etrs = NULL,
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

  if (!is.null(hs10_country_etrs) && !is.null(country_mapping)) {
    # Aggregate HS10×country imports to partner×GTAP level for Census weights
    census_weights <- hs10_country_etrs %>%
      left_join(
        country_mapping %>% select(cty_code, partner) %>% distinct(),
        by = 'cty_code'
      ) %>%
      mutate(partner = if_else(is.na(partner), 'row', partner)) %>%
      group_by(partner, gtap_code) %>%
      summarise(import_weight = sum(imports), .groups = 'drop') %>%
      filter(import_weight > 0)

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
  if (!is.null(hs10_country_etrs)) {
    cat(sprintf('%-10s  %15s  %18s\n', '', 'GTAP Weights', '2024 Census Weights'))
    cat(sprintf('%-10s  %15s  %18s\n', 'Country', '', ''))
    cat(sprintf('%-10s  %15s  %18s\n', '-------', '------------', '-----------------'))
  } else {
    cat(sprintf('%-10s  %15s\n', '', 'GTAP Weights'))
    cat(sprintf('%-10s  %15s\n', 'Country', ''))
    cat(sprintf('%-10s  %15s\n', '-------', '------------'))
  }

  for (i in 1:nrow(country_etrs)) {
    if (!is.null(hs10_country_etrs)) {
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
  if (!is.null(hs10_country_etrs)) {
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

  # Check if etr_data has coverage bases (should be present from calc_weighted_etr)
  if ('base_232' %in% names(etr_data)) {

    # Calculate coverage by partner using pre-computed bases
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

    # Calculate total coverage across all partners
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

  if (!is.null(hs10_country_etrs)) {
    writeLines(sprintf('%-10s  %15s  %18s', '', 'GTAP Weights', '2024 Census Weights'), con)
    writeLines(sprintf('%-10s  %15s  %18s', 'Country', '', ''), con)
    writeLines(sprintf('%-10s  %15s  %18s', '-------', '------------', '-----------------'), con)
  } else {
    writeLines(sprintf('%-10s  %15s', '', 'GTAP Weights'), con)
    writeLines(sprintf('%-10s  %15s', 'Country', ''), con)
    writeLines(sprintf('%-10s  %15s', '-------', '------------'), con)
  }

  for (i in 1:nrow(country_etrs)) {
    if (!is.null(hs10_country_etrs)) {
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
  if (!is.null(hs10_country_etrs)) {
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
    census_total = if (!is.null(hs10_country_etrs)) census_total_etr_value else NA
  ))
}
