# =============================================================================
# functions.R
# =============================================================================
#
# This file contains helper functions for calculating effective tariff rate
# (ETR) changes on U.S. imports by trading partner and GTAP sector. All ETR
# values represent changes from an early 2025 baseline.
#
# Functions:
#   - do_scenario():           Run complete ETR analysis for a scenario
#   - calc_import_shares():    Calculate import shares for specific HS6 codes
#   - calc_weighted_etr():     Calculate weighted ETR changes by partner and sector
#   - write_shock_commands():  Write GTAP shock commands to output file
#   - calc_overall_etrs():     Calculate and print overall ETR changes by country
#
# =============================================================================


#' Run complete ETR analysis for a given scenario
#'
#' @param scenario Scenario name (corresponds to config/{scenario}/ directory)
#' @param import_data_path Path to Census Bureau import data files
#'
#' @return Returns ETR data invisibly
do_scenario <- function(scenario, import_data_path = 'C:/Users/jar335/Downloads') {

  message(sprintf('\n=========================================================='))
  message(sprintf('Running scenario: %s', scenario))
  message(sprintf('==========================================================\n'))

  #---------------------------
  # Load scenario parameters
  #---------------------------

  message('Loading scenario parameters...')

  # Section 232 tariffs
  params_232 <- read_yaml(sprintf('config/%s/232.yaml', scenario))

  # IEEPA rates
  params_ieepa <- read_csv(sprintf('config/%s/ieepa_rates.csv', scenario), show_col_types = FALSE)

  # Other scenario parameters
  other_params <- read_yaml(sprintf('config/%s/other_params.yaml', scenario))

  # USMCA share of trade by sector
  usmca_shares <- read_csv('./resources/usmca_shares.csv', show_col_types = FALSE)

  # Adjust IEEPA rates: fold Korea into ftrow and Vietnam into row
  # Note: Korea and FTROW countries are directly classified via ftrow_codes in import data,
  # but IEEPA config has separate kr column that needs to be merged into ftrow rates
  params_ieepa <- params_ieepa %>%
    mutate(
      ftrow = ftrow * (1 - other_params$kr_share_ftrow) + kr * other_params$kr_share_ftrow,
      row   = row   * (1 - other_params$vn_share_row)   + vn * other_params$vn_share_row
    ) %>%
    select(-kr, -vn)

  #------------------
  # Read import data
  #------------------

  message('Reading import data...')

  # Load country-to-partner mapping
  country_mapping <- read_csv(
    'resources/country_partner_mapping.csv',
    col_types = cols(cty_code = col_character()),
    show_col_types = FALSE
  )

  # Read GTAP crosswalk
  crosswalk <- read_csv('resources/hs6_gtap_crosswalk.csv', show_col_types = FALSE)

  # Get list of all 2024 import files
  file_pattern <- 'dporths6ir24'
  files_2024   <- list.files(path = import_data_path, pattern = file_pattern, full.names = TRUE)

  if (length(files_2024) == 0) {
    stop(sprintf('No import files found at %s matching pattern %s', import_data_path, file_pattern))
  }

  # Define column positions based on the file specification
  col_positions <- fwf_positions(
    start     = c(1, 7, 11, 15, 19, 21),
    end       = c(6, 10, 14, 18, 20, 35),
    col_names = c('hs6_code', 'cty_code', 'port_code', 'year', 'month', 'value_mo')
  )

  # Build data
  hs6_by_country <- files_2024 %>%

    # Read and combine all files
    map_df(~ read_fwf(
      file = .x,
      col_positions = col_positions,
      col_types = cols(
        hs6_code  = col_character(),
        cty_code  = col_character(),
        port_code = col_character(),
        year      = col_integer(),
        month     = col_integer(),
        value_mo  = col_double()
      )
    )) %>%

    # Tag each row with a partner group using mapping
    left_join(
      country_mapping %>% select(cty_code, partner),
      by = 'cty_code'
    ) %>%
    mutate(partner = if_else(is.na(partner), 'row', partner)) %>%

    # Get totals
    group_by(hs6_code, partner) %>%
    summarise(imports = sum(value_mo), .groups = 'drop') %>%

    # Expand to include all HS6 x partner combinations (fill missing with 0)
    complete(
      hs6_code,
      partner = c('china', 'canada', 'mexico', 'uk', 'japan', 'eu', 'row', 'ftrow'),
      fill = list(imports = 0)
    ) %>%

    # Add GTAP code
    left_join(
      crosswalk %>%
        select(hs6_code, gtap_code),
      by = 'hs6_code'
    ) %>%
    mutate(gtap_code = str_to_lower(gtap_code)) %>%
    relocate(gtap_code, .after = hs6_code)

  #------------------------------
  # Calculate tax bases and ETRs
  #------------------------------

  message('Calculating tax bases and ETRs...')

  # Calculate tax bases for 232 -- and IEEPA as residual
  bases <- params_232 %>%
    names() %>%

    # Get bases for each 232 tariff
    map(~ {

      definite_codes <- params_232[[.x]]$base$definite
      all_codes      <- c(params_232[[.x]]$base$definite, params_232[[.x]]$base$maybe)

      share       <- calc_import_shares(definite_codes, data = hs6_by_country)
      share_upper <- calc_import_shares(all_codes, data = hs6_by_country) %>% rename(share_upper = share)

      share %>%
        left_join(share_upper, by = c('partner', 'gtap_code')) %>%
        mutate(tariff = .x, .before = 1) %>%
        return()
    }) %>%
    bind_rows() %>%

    # Add residual -- tax base uncovered by 232
    bind_rows(
      (.) %>%
        group_by(partner, gtap_code) %>%
        summarise(
          tariff      = 'residual',
          share       = 1 - sum(share),
          share_upper = 1 - sum(share_upper),
          .groups = 'drop'
        )
    )

  # Calculate ETRs
  etrs <- calc_weighted_etr(
    bases_data            = bases,
    params_data           = params_232,
    ieepa_data            = params_ieepa,
    usmca_data            = usmca_shares,
    us_auto_content_share = other_params$us_auto_content_share,
    auto_rebate           = other_params$auto_rebate_rate,
    us_assembly_share     = other_params$us_auto_assembly_share,
    ieepa_usmca_exempt    = other_params$ieepa_usmca_exception
  )

  #------------------
  # Write outputs
  #------------------

  message('Writing outputs...')

  # Write shock commands to file
  write_shock_commands(
    etr_data    = etrs,
    output_file = 'shocks.txt',
    scenario    = scenario
  )

  # Write sector x country ETR CSVs
  write_sector_country_etrs(
    etr_data            = etrs,
    output_file_regular = 'etrs_by_sector_country.csv',
    output_file_upper   = 'etrs_by_sector_country_upper.csv',
    scenario            = scenario
  )

  # Calculate and print overall ETRs with both GTAP and 2024 Census weights
  calc_overall_etrs(
    etr_data    = etrs,
    import_data = hs6_by_country,
    scenario    = scenario
  )

  message(sprintf('\nScenario %s complete!\n', scenario))

  # Return ETR data invisibly
  invisible(etrs)
}


#' Calculate import shares for a subset of HS6 codes
#'
#' Given a vector of HS6 codes, this function calculates:
#' 1. Total imports for those specific codes by partner and GTAP sector
#' 2. Total imports for ALL codes by partner and GTAP sector
#' 3. The share (subset / total) by partner and GTAP sector
#'
#' @param hs6_codes Character vector of HS6 codes to analyze
#' @param data Data frame with columns: hs6_code, partner, gtap_code, imports
#'
#' @return Data frame with columns: partner, gtap_code, subset_imports, total_imports, share
calc_import_shares <- function(hs6_codes, data) {

  # Calculate imports for the specified HS6 codes by partner and GTAP
  subset_imports <- data %>%
    filter(hs6_code %in% hs6_codes) %>%
    group_by(partner, gtap_code) %>%
    summarise(subset_imports = sum(imports), .groups = 'drop')

  # Calculate total imports by partner and GTAP
  total_imports <- data %>%
    group_by(partner, gtap_code) %>%
    summarise(total_imports = sum(imports), .groups = 'drop')

  # Join and calculate shares
  result <- total_imports %>%
    left_join(subset_imports, by = c('partner', 'gtap_code')) %>%
    mutate(share = replace_na(subset_imports / total_imports, 0)) %>%
    select(-subset_imports, -total_imports)

  return(result)
}


#' Calculate weighted ETR changes by partner and GTAP sector
#'
#' Calculates change in effective tariff rates from early 2025 baseline.
#'
#' @param bases_data Data frame with tariff bases
#' @param params_data Tariff parameters list
#' @param ieepa_data Data frame with IEEPA rates by partner and GTAP sector
#' @param usmca_data Data frame with USMCA shares by partner and GTAP sector
#' @param us_auto_content_share Share of US content in auto assembly
#' @param auto_rebate Auto rebate rate
#' @param us_assembly_share Share of US assembly in autos
#' @param ieepa_usmca_exempt Apply USMCA exemption to IEEPA tariffs (1 = yes, 0 = no)
#'
#' @return Data frame with columns: partner, gtap_code, etr, etr_upper (change from baseline)
calc_weighted_etr <- function(bases_data, params_data,
                              ieepa_data,
                              usmca_data,
                              us_auto_content_share,
                              auto_rebate,
                              us_assembly_share,
                              ieepa_usmca_exempt) {

  # Extract rates and usmca_exempt flags from params
  rates_and_exemptions <- map(names(params_data), ~ {
    tibble(
      tariff       = .x,
      partner      = names(params_data[[.x]]$rate),
      rate         = unlist(params_data[[.x]]$rate),
      usmca_exempt = params_data[[.x]]$usmca_exempt
    )
  }) %>%
    bind_rows()

  # Reshape USMCA shares long by country
  usmca_long <- usmca_data %>%
    pivot_longer(cols = -gtap_code, names_to = 'partner', values_to = 'usmca_share') %>%
    mutate(usmca_share = replace_na(usmca_share, 0))

  # Reshape IEEPA rates long by country
  ieepa_long <- ieepa_data %>%
    pivot_longer(cols = -gtap_code, names_to = 'partner', values_to = 'ieepa_rate') %>%
    mutate(ieepa_rate = replace_na(ieepa_rate, 0))

  # Join rates, exemptions, and USMCA shares to bases
  bases_data %>%
    left_join(rates_and_exemptions, by = c('tariff', 'partner')) %>%
    left_join(usmca_long, by = c('partner', 'gtap_code')) %>%
    left_join(ieepa_long, by = c('partner', 'gtap_code')) %>%
    mutate(
      rate = replace_na(rate, 0),
      usmca_exempt = replace_na(usmca_exempt, 0),
      usmca_share = replace_na(usmca_share, 0),
      ieepa_rate = replace_na(ieepa_rate, 0)
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
    mutate(
      adjusted_usmca_share = if_else(
        tariff %in% c('automobiles_passenger_and_light_trucks', 'automobile_parts', 'vehicles_completed_mhd'),
        usmca_share * us_auto_content_share,
        usmca_share
      ),
      adjusted_rate = if_else(
        usmca_exempt == 1 & partner %in% c('canada', 'mexico'),
        rate * (1 - adjusted_usmca_share),
        rate
      )
    ) %>%

    # Apply IEEPA rates to residual with USMCA exemption
    mutate(
      adjusted_rate = if_else(
        tariff == 'residual',
        if_else(
          ieepa_usmca_exempt == 1 & partner %in% c('canada', 'mexico'),
          ieepa_rate * (1 - adjusted_usmca_share),
          ieepa_rate
        ),
        adjusted_rate
      )
    ) %>%

    group_by(partner, gtap_code) %>%
    summarise(
      etr       = sum(share * adjusted_rate),
      etr_upper = sum(share_upper * adjusted_rate),
      .groups = 'drop'
    )
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
#' @param etr_data Data frame with columns: partner, gtap_code, etr, etr_upper
#' @param output_file_regular Path to output file for regular ETRs (default: 'etrs_by_sector_country.csv')
#' @param output_file_upper Path to output file for upper bound ETRs (default: 'etrs_by_sector_country_upper.csv')
#' @param scenario Scenario name for output directory
#'
#' @return Writes CSV files (in pp units) and returns invisibly
write_sector_country_etrs <- function(etr_data,
                                       output_file_regular = 'etrs_by_sector_country.csv',
                                       output_file_upper = 'etrs_by_sector_country_upper.csv',
                                       scenario = NULL) {

  # Create output directory if needed
  if (!is.null(scenario)) {
    output_dir <- sprintf('output/%s', scenario)
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    output_path_regular <- file.path(output_dir, output_file_regular)
    output_path_upper <- file.path(output_dir, output_file_upper)
  } else {
    output_path_regular <- output_file_regular
    output_path_upper <- output_file_upper
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

  # Pivot regular ETRs to wide format and convert to percentage points
  etrs_regular_wide <- etr_data %>%
    select(partner, gtap_code, etr) %>%
    pivot_wider(names_from = partner, values_from = etr, values_fill = 0) %>%
    select(gtap_code, any_of(country_order)) %>%
    mutate(across(-gtap_code, ~ .x * 100))

  # Pivot upper bound ETRs to wide format and convert to percentage points
  etrs_upper_wide <- etr_data %>%
    select(partner, gtap_code, etr_upper) %>%
    pivot_wider(names_from = partner, values_from = etr_upper, values_fill = 0) %>%
    select(gtap_code, any_of(country_order)) %>%
    mutate(across(-gtap_code, ~ .x * 100))

  # Apply sector ordering
  existing_sectors <- intersect(sector_order, etrs_regular_wide$gtap_code)

  etrs_regular_wide <- etrs_regular_wide %>%
    filter(gtap_code %in% existing_sectors) %>%
    arrange(match(gtap_code, sector_order))

  etrs_upper_wide <- etrs_upper_wide %>%
    filter(gtap_code %in% existing_sectors) %>%
    arrange(match(gtap_code, sector_order))

  # Write CSV files
  write_csv(etrs_regular_wide, output_path_regular)
  write_csv(etrs_upper_wide, output_path_upper)

  message(sprintf('Wrote regular ETRs by sector and country to %s (in pp units)', output_path_regular))
  message(sprintf('Wrote upper bound ETRs by sector and country to %s (in pp units)', output_path_upper))

  invisible(list(
    regular = etrs_regular_wide,
    upper = etrs_upper_wide
  ))
}


#' Calculate and print overall ETR changes by country and total using both GTAP and 2024 Census weights
#'
#' Calculates and prints change in effective tariff rates from early 2025 baseline.
#'
#' @param etr_data Data frame with columns: partner, gtap_code, etr (change from baseline)
#' @param import_data Data frame with columns: partner, gtap_code, imports (2024 import values)
#' @param weights_file Path to GTAP import weights CSV file
#' @param output_file Path to output text file (default: 'output/overall_etrs.txt')
#' @param scenario Scenario name for output directory
#'
#' @return Prints overall ETR changes and returns them invisibly
calc_overall_etrs <- function(etr_data, import_data = NULL,
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
    mutate(
      weighted_etr = etr * import_weight,
      weighted_etr_upper = etr_upper * import_weight
    )

  # Calculate overall ETR by country using GTAP weights
  gtap_country_etrs <- gtap_weighted_data %>%
    group_by(partner) %>%
    summarise(
      gtap_etr = sum(weighted_etr) / sum(import_weight),
      gtap_etr_upper = sum(weighted_etr_upper) / sum(import_weight),
      .groups = 'drop'
    )

  # Calculate total overall ETR using GTAP weights
  gtap_total_etr <- gtap_weighted_data %>%
    summarise(
      gtap_etr = sum(weighted_etr) / sum(import_weight),
      gtap_etr_upper = sum(weighted_etr_upper) / sum(import_weight)
    )

  gtap_total_etr_value <- gtap_total_etr$gtap_etr
  gtap_total_etr_upper_value <- gtap_total_etr$gtap_etr_upper

  # ===========================
  # Calculate 2024 Census-weighted ETRs
  # ===========================

  if (!is.null(import_data)) {
    # Calculate 2024 Census weights from import data
    census_weights <- import_data %>%
      group_by(partner, gtap_code) %>%
      summarise(import_weight = sum(imports), .groups = 'drop') %>%
      filter(import_weight > 0)

    # Join ETRs with Census weights
    census_weighted_data <- etr_data %>%
      inner_join(census_weights, by = c('partner', 'gtap_code')) %>%
      mutate(
        weighted_etr = etr * import_weight,
        weighted_etr_upper = etr_upper * import_weight
      )

    # Calculate overall ETR by country using Census weights
    census_country_etrs <- census_weighted_data %>%
      group_by(partner) %>%
      summarise(
        census_etr = sum(weighted_etr) / sum(import_weight),
        census_etr_upper = sum(weighted_etr_upper) / sum(import_weight),
        .groups = 'drop'
      )

    # Calculate total overall ETR using Census weights
    census_total_etr <- census_weighted_data %>%
      summarise(
        census_etr = sum(weighted_etr) / sum(import_weight),
        census_etr_upper = sum(weighted_etr_upper) / sum(import_weight)
      )

    census_total_etr_value <- census_total_etr$census_etr
    census_total_etr_upper_value <- census_total_etr$census_etr_upper

    # Combine both sets of results
    country_etrs <- gtap_country_etrs %>%
      left_join(census_country_etrs, by = 'partner') %>%
      arrange(desc(gtap_etr))

  } else {
    # Only GTAP weights available
    country_etrs <- gtap_country_etrs %>%
      mutate(census_etr = NA, census_etr_upper = NA) %>%
      arrange(desc(gtap_etr))
    census_total_etr_value <- NA
    census_total_etr_upper_value <- NA
  }

  # ===========================
  # Print results
  # ===========================

  cat('\n')
  cat('Overall ETRs by Country (change from early 2025 baseline):\n')
  cat('==========================================================\n')
  if (!is.null(import_data)) {
    cat(sprintf('%-10s  %23s  %34s\n', '', 'GTAP Weights', '2024 Census Weights'))
    cat(sprintf('%-10s  %11s  %11s  %16s  %16s\n', 'Country', 'Regular', 'Upper Bnd', 'Regular', 'Upper Bnd'))
    cat(sprintf('%-10s  %11s  %11s  %16s  %16s\n', '-------', '-------', '---------', '-------', '---------'))
  } else {
    cat(sprintf('%-10s  %23s\n', '', 'GTAP Weights'))
    cat(sprintf('%-10s  %11s  %11s\n', 'Country', 'Regular', 'Upper Bnd'))
    cat(sprintf('%-10s  %11s  %11s\n', '-------', '-------', '---------'))
  }

  for (i in 1:nrow(country_etrs)) {
    if (!is.null(import_data)) {
      cat(sprintf('%-10s  %10.2f%%  %10.2f%%  %15.2f%%  %15.2f%%\n',
                  toupper(country_etrs$partner[i]),
                  country_etrs$gtap_etr[i] * 100,
                  country_etrs$gtap_etr_upper[i] * 100,
                  country_etrs$census_etr[i] * 100,
                  country_etrs$census_etr_upper[i] * 100))
    } else {
      cat(sprintf('%-10s  %10.2f%%  %10.2f%%\n',
                  toupper(country_etrs$partner[i]),
                  country_etrs$gtap_etr[i] * 100,
                  country_etrs$gtap_etr_upper[i] * 100))
    }
  }
  cat('\n')
  if (!is.null(import_data)) {
    cat(sprintf('%-10s  %10.2f%%  %10.2f%%  %15.2f%%  %15.2f%%\n',
                'TOTAL',
                gtap_total_etr_value * 100,
                gtap_total_etr_upper_value * 100,
                census_total_etr_value * 100,
                census_total_etr_upper_value * 100))
  } else {
    cat(sprintf('%-10s  %10.2f%%  %10.2f%%\n',
                'TOTAL',
                gtap_total_etr_value * 100,
                gtap_total_etr_upper_value * 100))
  }
  cat('\n')

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
    writeLines(sprintf('%-10s  %23s  %34s', '', 'GTAP Weights', '2024 Census Weights'), con)
    writeLines(sprintf('%-10s  %11s  %11s  %16s  %16s', 'Country', 'Regular', 'Upper Bnd', 'Regular', 'Upper Bnd'), con)
    writeLines(sprintf('%-10s  %11s  %11s  %16s  %16s', '-------', '-------', '---------', '-------', '---------'), con)
  } else {
    writeLines(sprintf('%-10s  %23s', '', 'GTAP Weights'), con)
    writeLines(sprintf('%-10s  %11s  %11s', 'Country', 'Regular', 'Upper Bnd'), con)
    writeLines(sprintf('%-10s  %11s  %11s', '-------', '-------', '---------'), con)
  }

  for (i in 1:nrow(country_etrs)) {
    if (!is.null(import_data)) {
      writeLines(sprintf('%-10s  %10.2f%%  %10.2f%%  %15.2f%%  %15.2f%%',
                         toupper(country_etrs$partner[i]),
                         country_etrs$gtap_etr[i] * 100,
                         country_etrs$gtap_etr_upper[i] * 100,
                         country_etrs$census_etr[i] * 100,
                         country_etrs$census_etr_upper[i] * 100), con)
    } else {
      writeLines(sprintf('%-10s  %10.2f%%  %10.2f%%',
                         toupper(country_etrs$partner[i]),
                         country_etrs$gtap_etr[i] * 100,
                         country_etrs$gtap_etr_upper[i] * 100), con)
    }
  }

  writeLines('', con)
  if (!is.null(import_data)) {
    writeLines(sprintf('%-10s  %10.2f%%  %10.2f%%  %15.2f%%  %15.2f%%',
                       'TOTAL',
                       gtap_total_etr_value * 100,
                       gtap_total_etr_upper_value * 100,
                       census_total_etr_value * 100,
                       census_total_etr_upper_value * 100), con)
  } else {
    writeLines(sprintf('%-10s  %10.2f%%  %10.2f%%',
                       'TOTAL',
                       gtap_total_etr_value * 100,
                       gtap_total_etr_upper_value * 100), con)
  }

  close(con)

  message(sprintf('Wrote overall ETRs to %s', output_path))

  # Return results invisibly
  invisible(list(
    by_country = country_etrs,
    gtap_total = gtap_total_etr_value,
    gtap_total_upper = gtap_total_etr_upper_value,
    census_total = if (!is.null(import_data)) census_total_etr_value else NA,
    census_total_upper = if (!is.null(import_data)) census_total_etr_upper_value else NA
  ))
}
