## Compare Tracker vs ETRs at HS10 x country level
## Loads tracker snapshot and ETRs computation for matching dates,
## then compares total_rate at the product-country level.

library(tidyverse)
library(yaml)

# --- Paths ---
tracker_dir <- 'C:/Users/jar335/Documents/Repositories/tariff-rate-tracker'
etrs_dir    <- 'C:/Users/jar335/Documents/Repositories/Tariff-etrs'

# --- Load ETRs machinery ---
script_dir <- file.path(etrs_dir, 'src')
source(file.path(script_dir, 'config_parsing.R'))
source(file.path(script_dir, 'data_processing.R'))
source(file.path(script_dir, 'calculations.R'))

# --- Load shared data (same as do_scenario) ---
cache_file <- file.path(etrs_dir, 'cache', 'hs10_by_country_gtap_2024_con.rds')
imports <- readRDS(cache_file) %>%
  filter(!str_detect(hs10, '^(98|99)'), !is.na(gtap_code))
cat('Loaded', nrow(imports), 'import records from cache\n')

country_mapping <- read_csv(
  file.path(etrs_dir, 'resources', 'country_partner_mapping.csv'),
  col_types = cols(.default = col_character())
)

# --- Pick a date to compare ---
# Use 2026_rev_4 from tracker (effective 2026-02-20) and ETRs historical 2026-02-20
historical_date <- '2026-02-20'
tracker_snapshot <- 'snapshot_2026_rev_4.rds'

cat('\n=== Comparing at date:', historical_date, '===\n')

# --- Load tracker snapshot ---
cat('Loading tracker snapshot:', tracker_snapshot, '...\n')
snap <- readRDS(file.path(tracker_dir, 'data', 'timeseries', tracker_snapshot))
cat('  Tracker rows:', nrow(snap), '\n')

# Keep only the columns we need and rename to match ETRs conventions
tracker_rates <- snap %>%
  select(hts10, country, total_rate, total_additional, base_rate,
         rate_232, rate_ieepa_recip, rate_ieepa_fent, rate_301,
         rate_s122, rate_section_201, rate_other, metal_share) %>%
  rename(hs10 = hts10, cty_code = country) %>%
  mutate(cty_code = as.character(cty_code))

# Free memory
rm(snap); gc()

# --- Load ETRs config for same date ---
cat('Loading ETRs config for', historical_date, '...\n')
config_dir <- file.path(etrs_dir, 'config', 'historical', historical_date)
config <- load_scenario_config(config_dir)

# Run ETRs calculation via calc_etrs_for_config (the proper wrapper)
cat('Running ETRs calc_etrs_for_config...\n')
etrs_full <- calc_etrs_for_config(config, imports, country_mapping)
etrs_result <- etrs_full$hs10_country_etrs

# ETRs result has hs10, cty_code, etr (= total rate as fraction)
etrs_rates <- etrs_result %>%
  select(hs10, cty_code, etr, imports,
         any_of(c('s232_rate', 'ieepa_reciprocal_rate', 'ieepa_fentanyl_rate',
                   's301_rate', 's122_rate', 's201_rate', 'other_rate',
                   'mfn_rate', 'metal_share', 'final_rate'))) %>%
  mutate(cty_code = as.character(cty_code))

cat('  ETRs rows:', nrow(etrs_rates), '\n')

# --- Join and compare ---
cat('\nJoining on hs10 x cty_code...\n')
comparison <- tracker_rates %>%
  inner_join(etrs_rates, by = c('hs10', 'cty_code'), suffix = c('_tracker', '_etrs'))

cat('  Matched rows:', nrow(comparison), '\n')
cat('  Tracker-only:', nrow(tracker_rates) - nrow(comparison), '\n')
cat('  ETRs-only:', nrow(etrs_rates) - nrow(comparison), '\n')

# Compare total rates (tracker total_rate vs ETRs etr/final_rate)
# Note: both should be fractions (0.25 = 25%)
if ('final_rate' %in% names(comparison)) {
  comparison <- comparison %>%
    mutate(diff = final_rate - total_rate)
} else {
  comparison <- comparison %>%
    mutate(diff = etr - total_rate)
}

# --- Summary statistics ---
cat('\n=== Rate Difference Summary (ETRs - Tracker) ===\n')
cat(sprintf('  Mean diff:   %+.8f (%.6f pp)\n', mean(comparison$diff), mean(comparison$diff) * 100))
cat(sprintf('  Median diff: %+.8f\n', median(comparison$diff)))
cat(sprintf('  SD diff:     %.8f\n', sd(comparison$diff)))
cat(sprintf('  Min diff:    %+.8f\n', min(comparison$diff)))
cat(sprintf('  Max diff:    %+.8f\n', max(comparison$diff)))
cat(sprintf('  Exact match: %d of %d (%.1f%%)\n',
            sum(comparison$diff == 0), nrow(comparison),
            100 * sum(comparison$diff == 0) / nrow(comparison)))
cat(sprintf('  Within 0.01pp: %d of %d (%.1f%%)\n',
            sum(abs(comparison$diff) < 0.0001), nrow(comparison),
            100 * sum(abs(comparison$diff) < 0.0001) / nrow(comparison)))

# --- Nonzero differences ---
diffs <- comparison %>% filter(abs(diff) > 1e-10)
cat('\n=== Nonzero Differences:', nrow(diffs), 'rows ===\n')

if (nrow(diffs) > 0) {
  # Distribution
  cat('\nDifference magnitude buckets:\n')
  diffs %>%
    mutate(bucket = case_when(
      abs(diff) < 0.0001 ~ '< 0.01pp',
      abs(diff) < 0.001  ~ '0.01-0.1pp',
      abs(diff) < 0.01   ~ '0.1-1pp',
      abs(diff) < 0.1    ~ '1-10pp',
      TRUE               ~ '> 10pp'
    )) %>%
    count(bucket) %>%
    arrange(desc(n)) %>%
    print()

  # Top 20 largest differences (weighted by imports if available)
  cat('\nTop 20 largest absolute differences:\n')
  top_diffs <- diffs %>%
    arrange(desc(abs(diff))) %>%
    head(20)

  if ('imports' %in% names(top_diffs)) {
    top_diffs %>%
      select(hs10, cty_code, total_rate, etr, diff, imports) %>%
      mutate(across(c(total_rate, etr, diff), ~ round(. * 100, 4))) %>%
      print(n = 20)
  } else {
    top_diffs %>%
      select(hs10, cty_code, total_rate, etr, diff) %>%
      mutate(across(c(total_rate, etr, diff), ~ round(. * 100, 4))) %>%
      print(n = 20)
  }

  # By country - which countries have the most differences?
  cat('\nCountries with most differences:\n')
  diffs %>%
    group_by(cty_code) %>%
    summarise(
      n_diffs = n(),
      mean_diff_pp = round(mean(diff) * 100, 4),
      max_abs_diff_pp = round(max(abs(diff)) * 100, 4),
      .groups = 'drop'
    ) %>%
    arrange(desc(n_diffs)) %>%
    head(15) %>%
    print()

  # Import-weighted impact
  if ('imports' %in% names(diffs)) {
    cat('\nImport-weighted mean absolute difference:\n')
    wt_diff <- sum(abs(diffs$diff) * diffs$imports) / sum(comparison$imports)
    cat(sprintf('  %.6f pp\n', wt_diff * 100))
  }
}

cat('\nDone.\n')
