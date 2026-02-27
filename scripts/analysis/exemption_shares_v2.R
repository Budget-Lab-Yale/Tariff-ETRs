# =============================================================================
# exemption_shares_v2.R
# =============================================================================
#
# Corrected exemption share estimation.
#
# Problem with v1: comparing Census calculated duty to MFN rates conflates
# FTA/GSP preferences (what we want) with S232/S301 surcharges (which we
# already model correctly). China's exemption showed -279% because S301
# is included in Census duties.
#
# Fix: use our model's baseline S232 and S301 rates to estimate the policy
# tariff portion of Census duties. Then:
#   residual_mfn_duty = census_cal_duty - model_s232_s301_duty
#   exemption_share   = 1 - residual_mfn_duty / (mfn_rate * import_value)
#
# This isolates the "MFN discount" — i.e., how much of the MFN rate is waived
# by FTAs, GSP, duty-free provisions, etc.
#
# Usage: source('analysis/exemption_shares_v2.R')
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

  # Compute max S232 rate across all programs per HS10 × country
  # NAs are structural: steel HS10 codes have NA in aluminum columns and vice versa.
  # Replace with 0 before computing max (matches calc_weighted_etr() in calculations.R).
  rate_232_cols <- names(rate_matrix_232)[grepl('^s232_', names(rate_matrix_232))]
  s232_rates <- rate_matrix_232 %>%
    mutate(across(all_of(rate_232_cols), ~ if_else(is.na(.), 0, .))) %>%
    rowwise() %>%
    mutate(s232_rate = max(c_across(all_of(rate_232_cols)))) %>%
    ungroup() %>%
    select(hs10, cty_code, s232_rate)

  message(sprintf('S232: %s non-zero HS10 × country combinations',
                  format(nrow(s232_rates), big.mark = ',')))

  # Load S301 rates
  s301_rates <- load_ieepa_rates_yaml(
    'config/baseline/s301.yaml',
    rate_col_name = 's301_rate'
  ) %>%
    filter(s301_rate > 0) %>%
    select(hs10, cty_code, s301_rate)

  message(sprintf('S301: %s non-zero HS10 × country combinations',
                  format(nrow(s301_rates), big.mark = ',')))

  # Join: start with all HS10 × country from crosswalk × Census codes,
  # then left-join policy rates (missing = 0)
  # But that's huge. Instead, just combine the sparse rate tibbles.
  policy_rates <- s232_rates %>%
    full_join(s301_rates, by = c('hs10', 'cty_code')) %>%
    mutate(
      s232_rate = replace_na(s232_rate, 0),
      s301_rate = replace_na(s301_rate, 0),
      # Simplified stacking: s232 + s301 (ignoring metal content scaling)
      policy_rate = s232_rate + s301_rate
    )

  message(sprintf('Combined policy rates: %s HS10 × country combinations',
                  format(nrow(policy_rates), big.mark = ',')))

  policy_rates
}


# =============================================================================
# 2. Load import data and compute model-implied policy duties
# =============================================================================

compute_model_policy_duties <- function(policy_rates) {
  message('\n--- Computing model-implied policy duties ---')

  # Load cached import data
  cache_file <- 'cache/hs10_by_country_gtap_2024_con.rds'
  if (!file.exists(cache_file)) {
    stop('Cache file not found: ', cache_file)
  }
  imports <- readRDS(cache_file)
  message(sprintf('Loaded %s import records from cache',
                  format(nrow(imports), big.mark = ',')))

  # Join imports with policy rates
  # Only rows with non-zero policy rates will match
  imports_with_rates <- imports %>%
    left_join(policy_rates, by = c('hs10', 'cty_code')) %>%
    mutate(
      s232_rate   = replace_na(s232_rate, 0),
      s301_rate   = replace_na(s301_rate, 0),
      policy_rate = replace_na(policy_rate, 0),
      # Implied policy duty = rate × import value
      s232_duty   = s232_rate * imports,
      s301_duty   = s301_rate * imports,
      policy_duty = policy_rate * imports
    )

  imports_with_rates
}


# =============================================================================
# 3. Aggregate to HS2 × partner level
# =============================================================================

aggregate_to_hs2_partner <- function(imports_with_rates) {
  partner_map <- read_csv(
    'resources/country_partner_mapping.csv',
    col_types = cols(cty_code = col_character(), partner = col_character()),
    show_col_types = FALSE
  ) %>%
    select(cty_code, partner)

  # Load MFN rates
  mfn <- read_csv(
    'resources/mfn_rates_2025.csv',
    col_types = cols(hs8 = col_character(), mfn_rate = col_double()),
    show_col_types = FALSE
  ) %>%
    select(hs8, mfn_rate)

  imports_with_rates %>%
    # Add partner
    left_join(partner_map, by = 'cty_code') %>%
    mutate(partner = if_else(is.na(partner), 'row', partner)) %>%
    # Add MFN rate
    mutate(hs8 = substr(hs10, 1, 8)) %>%
    left_join(mfn, by = 'hs8') %>%
    mutate(
      mfn_rate = replace_na(mfn_rate, 0),
      implied_mfn_duty = mfn_rate * imports
    ) %>%
    # Aggregate to HS2 × partner
    mutate(hs2 = substr(hs10, 1, 2)) %>%
    group_by(hs2, partner) %>%
    summarise(
      imports          = sum(imports),
      s232_duty        = sum(s232_duty),
      s301_duty        = sum(s301_duty),
      policy_duty      = sum(policy_duty),
      implied_mfn_duty = sum(implied_mfn_duty),
      .groups = 'drop'
    ) %>%
    mutate(
      wt_avg_mfn    = implied_mfn_duty / imports,
      wt_avg_policy = policy_duty / imports
    )
}


# =============================================================================
# 4. Download Census data and merge
# =============================================================================

download_census_hs2 <- function(year = 2024) {
  base_url <- 'https://api.census.gov/data/timeseries/intltrade/imports/hs'
  url <- sprintf(
    '%s?get=CAL_DUT_YR,CON_VAL_YR,DUT_VAL_YR,I_COMMODITY,CTY_CODE&YEAR=%d&MONTH=12&COMM_LVL=HS2',
    base_url, year
  )
  message('\nDownloading Census HS2 × country data...')
  raw <- fromJSON(url)
  h <- raw[1, ]
  d <- as_tibble(raw[-1, ], .name_repair = 'minimal')
  names(d) <- h
  d %>%
    transmute(
      hs2       = I_COMMODITY,
      cty_code  = CTY_CODE,
      census_duty  = as.numeric(CAL_DUT_YR),
      census_value = as.numeric(CON_VAL_YR),
      census_dut_value = as.numeric(DUT_VAL_YR)
    ) %>%
    filter(
      !grepl('X', cty_code),
      !grepl('^0', cty_code),
      cty_code != '-',
      census_value > 0
    )
}


merge_census_with_model <- function(census_data, model_data) {
  # Map Census country codes to partners
  partner_map <- read_csv(
    'resources/country_partner_mapping.csv',
    col_types = cols(cty_code = col_character(), partner = col_character()),
    show_col_types = FALSE
  ) %>%
    select(cty_code, partner)

  # Aggregate Census to HS2 × partner
  census_by_partner <- census_data %>%
    left_join(partner_map, by = 'cty_code') %>%
    mutate(partner = if_else(is.na(partner), 'row', partner)) %>%
    group_by(hs2, partner) %>%
    summarise(
      census_duty  = sum(census_duty),
      census_value = sum(census_value),
      census_dut_value = sum(census_dut_value),
      .groups = 'drop'
    )

  # Join with model data
  census_by_partner %>%
    left_join(model_data, by = c('hs2', 'partner')) %>%
    mutate(
      # Fill missing model data with 0 (chapters not in our import cache)
      imports          = replace_na(imports, 0),
      policy_duty      = replace_na(policy_duty, 0),
      s232_duty        = replace_na(s232_duty, 0),
      s301_duty        = replace_na(s301_duty, 0),
      implied_mfn_duty = replace_na(implied_mfn_duty, 0),
      wt_avg_mfn       = replace_na(wt_avg_mfn, 0),
      wt_avg_policy    = replace_na(wt_avg_policy, 0),
      # Residual = Census duty - model policy duty
      residual_mfn_duty = census_duty - policy_duty,
      # Census realized ETR
      realized_etr = census_duty / census_value,
      # Corrected MFN-only ETR (after removing policy surcharges)
      mfn_only_etr = pmax(residual_mfn_duty, 0) / census_value,
      # MFN exemption share
      exemption_share = if_else(
        wt_avg_mfn > 0,
        1 - mfn_only_etr / wt_avg_mfn,
        0
      )
    )
}


format_pct <- function(x) sprintf('%.1f%%', x * 100)


# =============================================================================
# 5. Output
# =============================================================================

print_partner_summary <- function(merged) {
  cat('\n')
  cat('Corrected MFN Exemption Shares by Partner\n')
  cat(strrep('=', 75), '\n')
  cat('After subtracting model S232+S301 duties from Census calculated duties\n')
  cat('Exemption = 1 - (residual MFN duty / implied MFN duty)\n\n')

  summary <- merged %>%
    group_by(partner) %>%
    summarise(
      census_value     = sum(census_value),
      census_duty      = sum(census_duty),
      policy_duty      = sum(policy_duty),
      s232_duty        = sum(s232_duty),
      s301_duty        = sum(s301_duty),
      implied_mfn_duty = sum(implied_mfn_duty),
      .groups = 'drop'
    ) %>%
    mutate(
      residual_mfn_duty = pmax(census_duty - policy_duty, 0),
      realized_etr     = census_duty / census_value,
      policy_etr       = policy_duty / census_value,
      mfn_only_etr     = residual_mfn_duty / census_value,
      wt_avg_mfn       = implied_mfn_duty / census_value,
      exemption_share  = if_else(
        wt_avg_mfn > 0,
        1 - mfn_only_etr / wt_avg_mfn,
        0
      )
    ) %>%
    arrange(desc(exemption_share))

  # Add total row
  total <- merged %>%
    summarise(
      census_value = sum(census_value),
      census_duty  = sum(census_duty),
      policy_duty  = sum(policy_duty),
      s232_duty    = sum(s232_duty),
      s301_duty    = sum(s301_duty),
      implied_mfn_duty = sum(implied_mfn_duty)
    ) %>%
    mutate(
      partner = 'TOTAL',
      residual_mfn_duty = pmax(census_duty - policy_duty, 0),
      realized_etr     = census_duty / census_value,
      policy_etr       = policy_duty / census_value,
      mfn_only_etr     = residual_mfn_duty / census_value,
      wt_avg_mfn       = implied_mfn_duty / census_value,
      exemption_share  = 1 - mfn_only_etr / wt_avg_mfn
    )

  display <- bind_rows(total, summary) %>%
    mutate(
      Partner = toupper(partner),
      `Imports ($B)` = sprintf('$%.0f', census_value / 1e9),
      `Census ETR` = format_pct(realized_etr),
      `S232+S301` = format_pct(policy_etr),
      `MFN Only` = format_pct(mfn_only_etr),
      `Wt Avg MFN` = format_pct(wt_avg_mfn),
      `MFN Exemption` = format_pct(exemption_share)
    ) %>%
    select(Partner, `Imports ($B)`, `Census ETR`, `S232+S301`,
           `MFN Only`, `Wt Avg MFN`, `MFN Exemption`)

  print(as.data.frame(display), row.names = FALSE, right = FALSE)

  invisible(summary)
}


print_hs2_detail <- function(merged, partners, title_suffix = '') {
  for (p in partners) {
    cat(sprintf('\n\n%s: HS2 MFN Exemption Shares%s (>$100M Census imports)\n',
                toupper(p), title_suffix))
    cat(strrep('=', 70), '\n\n')

    p_data <- merged %>%
      filter(partner == p, census_value > 1e8, wt_avg_mfn > 0) %>%
      arrange(desc(census_value)) %>%
      mutate(
        `HS2` = hs2,
        `Imports ($B)` = sprintf('$%.1f', census_value / 1e9),
        `Census ETR` = format_pct(realized_etr),
        `Policy` = format_pct(wt_avg_policy),
        `MFN Only` = format_pct(mfn_only_etr),
        `Avg MFN` = format_pct(wt_avg_mfn),
        `Exempt` = format_pct(exemption_share)
      ) %>%
      select(`HS2`, `Imports ($B)`, `Census ETR`, `Policy`,
             `MFN Only`, `Avg MFN`, `Exempt`)

    print(as.data.frame(p_data), row.names = FALSE, right = FALSE)
  }
}


# =============================================================================
# 6. Main
# =============================================================================

main <- function() {
  cat('\n')
  cat(strrep('*', 75), '\n')
  cat('  Corrected MFN Exemption Shares (S232/S301 subtracted)\n')
  cat(strrep('*', 75), '\n')
  cat('\nApproach:\n')
  cat('  1. Compute S232 + S301 duties from model baseline configs\n')
  cat('  2. Subtract from Census calculated duties → residual MFN duty\n')
  cat('  3. Compare residual to import-weighted MFN rate → exemption share\n')
  cat('  4. Exemption = fraction of MFN rate waived by FTAs/GSP/etc.\n')

  # Step 1: Load policy rates from baseline configs
  policy_rates <- load_baseline_policy_rates()

  # Step 2: Join with import data and compute policy duties
  imports_with_rates <- compute_model_policy_duties(policy_rates)

  # Step 3: Aggregate to HS2 × partner
  model_hs2 <- aggregate_to_hs2_partner(imports_with_rates)

  # Step 4: Download Census data
  census_hs2 <- download_census_hs2()

  # Step 5: Merge
  merged <- merge_census_with_model(census_hs2, model_hs2)

  # Step 6: Print results
  partner_summary <- print_partner_summary(merged)

  # Detail for key partners
  print_hs2_detail(merged, c('canada', 'mexico', 'ftrow'))
  print_hs2_detail(merged, c('china', 'eu', 'japan', 'uk', 'row'))

  # ---- Decomposition: what drives the overall gap ----
  cat('\n\nDecomposition of Overall ETR Gap\n')
  cat(strrep('=', 50), '\n\n')

  totals <- merged %>%
    summarise(
      census_value     = sum(census_value),
      census_duty      = sum(census_duty),
      policy_duty      = sum(policy_duty),
      implied_mfn_duty = sum(implied_mfn_duty)
    )

  residual <- pmax(totals$census_duty - totals$policy_duty, 0)

  cat(sprintf('Total US imports (Census 2024):      $%.0fB\n', totals$census_value / 1e9))
  cat(sprintf('Census calculated duty:              $%.1fB  (%.2f%% ETR)\n',
              totals$census_duty / 1e9, totals$census_duty / totals$census_value * 100))
  cat(sprintf('  of which S232 + S301 (model est.): $%.1fB  (%.2f%% ETR)\n',
              totals$policy_duty / 1e9, totals$policy_duty / totals$census_value * 100))
  cat(sprintf('  residual MFN duties:               $%.1fB  (%.2f%% ETR)\n',
              residual / 1e9, residual / totals$census_value * 100))
  cat(sprintf('\nImplied full MFN duty (if all paid):  $%.1fB  (%.2f%% ETR)\n',
              totals$implied_mfn_duty / 1e9, totals$implied_mfn_duty / totals$census_value * 100))
  cat(sprintf('MFN exemption share: %.1f%%\n',
              (1 - residual / totals$implied_mfn_duty) * 100))
  cat(sprintf('MFN revenue gap:     $%.1fB per year\n',
              (totals$implied_mfn_duty - residual) / 1e9))

  # ---- Save outputs ----
  output_dir <- 'analysis/output'
  write_csv(merged, file.path(output_dir, 'exemption_shares_v2_hs2_partner.csv'))
  write_csv(partner_summary, file.path(output_dir, 'exemption_shares_v2_partner_summary.csv'))

  message(sprintf('\nOutput saved to %s/', output_dir))

  cat('\n')
  cat(strrep('*', 75), '\n')
  cat('  Analysis complete\n')
  cat(strrep('*', 75), '\n')

  invisible(list(merged = merged, partner_summary = partner_summary))
}

results <- main()
