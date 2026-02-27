# =============================================================================
# generate_mfn_exemption_shares.R
# =============================================================================
#
# Generate MFN exemption shares at HS2 × country level for the tariff pipeline.
#
# Adapts the exemption_shares_v2.R approach to produce HS2 × cty_code output
# (instead of HS2 × partner) for use in calc_weighted_etr().
#
# Approach:
#   1. Load baseline S232 + S301 policy rates from model configs
#   2. Join with cached imports, aggregate to HS2 × cty_code
#   3. Download Census HS2 × country calculated duty data
#   4. Compute: exemption_share = 1 - pmax(census_duty - policy_duty, 0) / implied_mfn_duty
#   5. Floor negative values at 0
#   6. Write sparse CSV (only rows with exemption_share > 0)
#
# Output: resources/mfn_exemption_shares.csv (columns: hs2, cty_code, exemption_share)
#
# Usage: source('analysis/generate_mfn_exemption_shares.R')
# =============================================================================

library(tidyverse)
library(jsonlite)
library(yaml)

# Source model code for config parsing functions
source('src/config_parsing.R')
source('src/data_processing.R')


# =============================================================================
# 1. Load baseline S232 + S301 rates from model configs
# =============================================================================

load_baseline_policy_rates <- function() {
  message('\n--- Loading baseline policy rates ---')

  # Load S232 rates
  params_s232 <- load_s232_rates('config/baseline/s232.yaml')
  rate_matrix_232 <- params_s232$rate_matrix

  # Compute max S232 rate across all programs per HS10 x country
  # NAs are structural: steel HS10 codes have NA in aluminum columns and vice versa.
  # Replace with 0 before computing max (matches calc_weighted_etr() in calculations.R).
  rate_232_cols <- names(rate_matrix_232)[grepl('^s232_', names(rate_matrix_232))]
  s232_rates <- rate_matrix_232 %>%
    mutate(across(all_of(rate_232_cols), ~ if_else(is.na(.), 0, .))) %>%
    rowwise() %>%
    mutate(s232_rate = max(c_across(all_of(rate_232_cols)))) %>%
    ungroup() %>%
    select(hs10, cty_code, s232_rate)

  message(sprintf('S232: %s non-zero HS10 x country combinations',
                  format(nrow(s232_rates), big.mark = ',')))

  # Load S301 rates
  s301_rates <- load_ieepa_rates_yaml(
    'config/baseline/s301.yaml',
    rate_col_name = 's301_rate'
  ) %>%
    filter(s301_rate > 0) %>%
    select(hs10, cty_code, s301_rate)

  message(sprintf('S301: %s non-zero HS10 x country combinations',
                  format(nrow(s301_rates), big.mark = ',')))

  # Combine policy rates
  policy_rates <- s232_rates %>%
    full_join(s301_rates, by = c('hs10', 'cty_code')) %>%
    mutate(
      s232_rate = replace_na(s232_rate, 0),
      s301_rate = replace_na(s301_rate, 0),
      policy_rate = s232_rate + s301_rate
    )

  message(sprintf('Combined policy rates: %s HS10 x country combinations',
                  format(nrow(policy_rates), big.mark = ',')))

  policy_rates
}


# =============================================================================
# 2. Load import data and compute model-implied policy duties at HS2 x country
# =============================================================================

compute_model_hs2_country <- function(policy_rates) {
  message('\n--- Computing model-implied policy duties at HS2 x country ---')

  # Load cached import data
  cache_file <- 'cache/hs10_by_country_gtap_2024_con.rds'
  if (!file.exists(cache_file)) {
    stop('Cache file not found: ', cache_file)
  }
  imports <- readRDS(cache_file)
  message(sprintf('Loaded %s import records from cache',
                  format(nrow(imports), big.mark = ',')))

  # Load MFN rates
  mfn <- read_csv(
    'resources/mfn_rates_2025.csv',
    col_types = cols(hs8 = col_character(), mfn_rate = col_double()),
    show_col_types = FALSE
  ) %>%
    select(hs8, mfn_rate)

  # Join imports with policy rates and MFN rates
  imports_with_rates <- imports %>%
    left_join(policy_rates, by = c('hs10', 'cty_code')) %>%
    mutate(
      s232_rate   = replace_na(s232_rate, 0),
      s301_rate   = replace_na(s301_rate, 0),
      policy_rate = replace_na(policy_rate, 0),
      policy_duty = policy_rate * imports
    ) %>%
    mutate(hs8 = substr(hs10, 1, 8)) %>%
    left_join(mfn, by = 'hs8') %>%
    mutate(
      mfn_rate = replace_na(mfn_rate, 0),
      implied_mfn_duty = mfn_rate * imports
    )

  # Aggregate to HS2 x cty_code
  imports_with_rates %>%
    mutate(hs2 = substr(hs10, 1, 2)) %>%
    group_by(hs2, cty_code) %>%
    summarise(
      imports          = sum(imports),
      policy_duty      = sum(policy_duty),
      implied_mfn_duty = sum(implied_mfn_duty),
      .groups = 'drop'
    )
}


# =============================================================================
# 3. Download Census HS2 x country data
# =============================================================================

download_census_hs2_country <- function(year = 2024) {
  base_url <- 'https://api.census.gov/data/timeseries/intltrade/imports/hs'
  url <- sprintf(
    '%s?get=CAL_DUT_YR,CON_VAL_YR,I_COMMODITY,CTY_CODE&YEAR=%d&MONTH=12&COMM_LVL=HS2',
    base_url, year
  )
  message('\nDownloading Census HS2 x country data...')
  raw <- fromJSON(url)
  h <- raw[1, ]
  d <- as_tibble(raw[-1, ], .name_repair = 'minimal')
  names(d) <- h
  d %>%
    transmute(
      hs2       = I_COMMODITY,
      cty_code  = CTY_CODE,
      census_duty  = as.numeric(CAL_DUT_YR),
      census_value = as.numeric(CON_VAL_YR)
    ) %>%
    filter(
      !grepl('X', cty_code),
      !grepl('^0', cty_code),
      cty_code != '-',
      census_value > 0
    )
}


# =============================================================================
# 4. Merge and compute exemption shares
# =============================================================================

compute_exemption_shares <- function(census_data, model_data) {
  merged <- census_data %>%
    left_join(model_data, by = c('hs2', 'cty_code')) %>%
    mutate(
      imports          = replace_na(imports, 0),
      policy_duty      = replace_na(policy_duty, 0),
      implied_mfn_duty = replace_na(implied_mfn_duty, 0),
      # Residual MFN duty = Census duty - modeled policy duty
      residual_mfn_duty = pmax(census_duty - policy_duty, 0),
      # Exemption share = fraction of MFN waived
      exemption_share = if_else(
        implied_mfn_duty > 0,
        pmax(1 - residual_mfn_duty / implied_mfn_duty, 0),
        0
      )
    )

  merged
}


# =============================================================================
# 5. Main
# =============================================================================

main <- function() {
  cat('\n')
  cat(strrep('*', 75), '\n')
  cat('  Generate MFN Exemption Shares (HS2 x Country)\n')
  cat(strrep('*', 75), '\n')

  # Step 1: Load policy rates
  policy_rates <- load_baseline_policy_rates()

  # Step 2: Compute model-implied duties at HS2 x country
  model_hs2 <- compute_model_hs2_country(policy_rates)

  # Step 3: Download Census data
  census_hs2 <- download_census_hs2_country()

  # Step 4: Compute exemption shares
  merged <- compute_exemption_shares(census_hs2, model_hs2)

  # Step 5: Filter to positive exemption shares (sparse format)
  output <- merged %>%
    filter(exemption_share > 0) %>%
    select(hs2, cty_code, exemption_share) %>%
    arrange(hs2, cty_code)

  # Summary stats
  cat(sprintf('\nTotal HS2 x country combinations with exemption > 0: %s\n',
              format(nrow(output), big.mark = ',')))
  cat(sprintf('Unique countries with exemptions: %d\n', n_distinct(output$cty_code)))
  cat(sprintf('Unique HS2 chapters with exemptions: %d\n', n_distinct(output$hs2)))
  cat(sprintf('Mean exemption share (where > 0): %.1f%%\n', mean(output$exemption_share) * 100))
  cat(sprintf('Median exemption share (where > 0): %.1f%%\n', median(output$exemption_share) * 100))

  # Print partner-level summary using country mapping
  partner_map <- read_csv(
    'resources/country_partner_mapping.csv',
    col_types = cols(cty_code = col_character(), partner = col_character()),
    show_col_types = FALSE
  ) %>%
    select(cty_code, partner)

  partner_summary <- merged %>%
    left_join(partner_map, by = 'cty_code') %>%
    mutate(partner = if_else(is.na(partner), 'row', partner)) %>%
    group_by(partner) %>%
    summarise(
      census_value     = sum(census_value),
      implied_mfn_duty = sum(implied_mfn_duty),
      residual_mfn     = sum(residual_mfn_duty),
      .groups = 'drop'
    ) %>%
    mutate(
      wt_avg_mfn      = implied_mfn_duty / census_value,
      exemption_share = if_else(
        implied_mfn_duty > 0,
        pmax(1 - residual_mfn / implied_mfn_duty, 0),
        0
      )
    ) %>%
    arrange(desc(exemption_share))

  cat('\nPartner-Level Summary:\n')
  cat(strrep('-', 50), '\n')
  for (i in seq_len(nrow(partner_summary))) {
    row <- partner_summary[i, ]
    cat(sprintf('  %-8s  MFN Exemption: %5.1f%%  (Wt Avg MFN: %.2f%%)\n',
                row$partner, row$exemption_share * 100, row$wt_avg_mfn * 100))
  }

  # Write output
  output_file <- 'resources/mfn_exemption_shares.csv'
  write_csv(output, output_file)
  message(sprintf('\nWrote %s rows to %s', format(nrow(output), big.mark = ','), output_file))

  cat('\n')
  cat(strrep('*', 75), '\n')
  cat('  Done\n')
  cat(strrep('*', 75), '\n')

  invisible(list(merged = merged, output = output, partner_summary = partner_summary))
}

results <- main()
