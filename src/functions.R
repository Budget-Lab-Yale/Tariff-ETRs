# =============================================================================
# functions.R
# =============================================================================
#
# This file contains helper functions for calculating effective tariff rates
# (ETRs) on U.S. imports by trading partner and GTAP sector.
#
# Functions:
#   - calc_import_shares():    Calculate import shares for specific HS6 codes
#   - calc_weighted_etr():     Calculate weighted ETRs by partner and sector
#   - write_shock_commands():  Write GTAP shock commands to output file
#   - calc_overall_etrs():     Calculate and print overall ETRs by country
#
# =============================================================================


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
calc_import_shares <- function(hs6_codes, data = hs6_by_country) {

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


#' Calculate weighted ETR by partner and GTAP sector
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
#' @return Data frame with columns: partner, gtap_code, etr, etr_upper
calc_weighted_etr <- function(bases_data = bases, params_data = params_232,
                              ieepa_data = params_ieepa,
                              usmca_data = usmca_shares,
                              us_auto_content_share = us_auto_content_share,
                              auto_rebate = auto_rebate_rate,
                              us_assembly_share = us_auto_assembly_share,
                              ieepa_usmca_exempt = ieepa_usmca_exception) {

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
write_shock_commands <- function(etr_data, output_file = 'shocks.txt', scenario = scenario) {

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


#' Calculate and print overall ETRs by country and total using both GTAP and 2024 Census weights
#'
#' @param etr_data Data frame with columns: partner, gtap_code, etr
#' @param import_data Data frame with columns: partner, gtap_code, imports (2024 import values)
#' @param weights_file Path to GTAP import weights CSV file
#' @param output_file Path to output text file (default: 'output/overall_etrs.txt')
#' @param scenario Scenario name for output directory
#'
#' @return Prints overall ETRs and returns them invisibly
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
    summarise(gtap_etr = sum(weighted_etr) / sum(import_weight)) %>%
    pull(gtap_etr)

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
      summarise(census_etr = sum(weighted_etr) / sum(import_weight)) %>%
      pull(census_etr)

    # Combine both sets of results
    country_etrs <- gtap_country_etrs %>%
      left_join(census_country_etrs, by = 'partner') %>%
      arrange(desc(gtap_etr))

  } else {
    # Only GTAP weights available
    country_etrs <- gtap_country_etrs %>%
      mutate(census_etr = NA) %>%
      arrange(desc(gtap_etr))
    census_total_etr <- NA
  }

  # ===========================
  # Print results
  # ===========================

  cat('\n')
  cat('Overall ETRs by Country:\n')
  cat('========================\n')
  if (!is.null(import_data)) {
    cat(sprintf('%-10s  %12s  %17s\n', 'Country', 'GTAP Weights', '2024 Census Weights'))
    cat(sprintf('%-10s  %12s  %17s\n', '-------', '------------', '-------------------'))
  } else {
    cat(sprintf('%-10s  %12s\n', 'Country', 'GTAP Weights'))
    cat(sprintf('%-10s  %12s\n', '-------', '------------'))
  }

  for (i in 1:nrow(country_etrs)) {
    if (!is.null(import_data)) {
      cat(sprintf('%-10s  %11.2f%%  %16.2f%%\n',
                  toupper(country_etrs$partner[i]),
                  country_etrs$gtap_etr[i] * 100,
                  country_etrs$census_etr[i] * 100))
    } else {
      cat(sprintf('%-10s  %11.2f%%\n',
                  toupper(country_etrs$partner[i]),
                  country_etrs$gtap_etr[i] * 100))
    }
  }
  cat('\n')
  if (!is.null(import_data)) {
    cat(sprintf('%-10s  %11.2f%%  %16.2f%%\n', 'TOTAL', gtap_total_etr * 100, census_total_etr * 100))
  } else {
    cat(sprintf('%-10s  %11.2f%%\n', 'TOTAL', gtap_total_etr * 100))
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

  writeLines('Overall ETRs by Country:', con)
  writeLines('========================', con)
  writeLines('', con)

  if (!is.null(import_data)) {
    writeLines(sprintf('%-10s  %12s  %17s', 'Country', 'GTAP Weights', '2024 Census Weights'), con)
    writeLines(sprintf('%-10s  %12s  %17s', '-------', '------------', '-------------------'), con)
  } else {
    writeLines(sprintf('%-10s  %12s', 'Country', 'GTAP Weights'), con)
    writeLines(sprintf('%-10s  %12s', '-------', '------------'), con)
  }

  for (i in 1:nrow(country_etrs)) {
    if (!is.null(import_data)) {
      writeLines(sprintf('%-10s  %11.2f%%  %16.2f%%',
                         toupper(country_etrs$partner[i]),
                         country_etrs$gtap_etr[i] * 100,
                         country_etrs$census_etr[i] * 100), con)
    } else {
      writeLines(sprintf('%-10s  %11.2f%%',
                         toupper(country_etrs$partner[i]),
                         country_etrs$gtap_etr[i] * 100), con)
    }
  }

  writeLines('', con)
  if (!is.null(import_data)) {
    writeLines(sprintf('%-10s  %11.2f%%  %16.2f%%', 'TOTAL', gtap_total_etr * 100, census_total_etr * 100), con)
  } else {
    writeLines(sprintf('%-10s  %11.2f%%', 'TOTAL', gtap_total_etr * 100), con)
  }

  close(con)

  message(sprintf('Wrote overall ETRs to %s', output_path))

  # Return results invisibly
  invisible(list(
    by_country = country_etrs,
    gtap_total = gtap_total_etr,
    census_total = if (!is.null(import_data)) census_total_etr else NA
  ))
}
