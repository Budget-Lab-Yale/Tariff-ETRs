#!/usr/bin/env Rscript

# =============================================================================
# test_scenarios.R
# =============================================================================
#
# Validation script for counterfactual scenario analysis.
#
# Tests that the tariff calculation pipeline produces internally consistent
# results by computing ETRs under various "what-if" scenarios (selectively
# zeroing tariff authorities) and verifying key invariants.
#
# Approach: Instead of loading separate config directories, we load one full
# config (all authorities present) and call calc_weighted_etr() with selective
# NULLs to simulate "remove authority X" counterfactuals. This validates the
# stacking logic without requiring dedicated config directories for each test.
#
# Tests:
#   1. Baseline identity: same config run twice → delta = 0
#   2. No-IEEPA: zeroing IEEPA reciprocal + fentanyl reduces rates correctly
#   3. Stacking consistency: coverage bases sum to imports, levels non-negative
#   4. Cross-scenario monotonicity: removing purely-additive authorities
#      can't increase rates
#
# Usage:
#   Rscript scripts/test_scenarios.R [config_dir]
#
# Default config_dir: config/2-21_perm/2026-02-24 (has all 5 authority types)
#
# Note on rate_s122: Section 122 is included in the stacking formula but has
# no dedicated scenario toggle in the original plan. This is because S122 is
# a minor authority with no independent policy scenario. The test still
# validates it by zeroing it independently and checking monotonicity.
#
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(yaml)
})

source('src/config_parsing.R')
source('src/data_processing.R')
source('src/calculations.R')


# =============================================================================
# Configuration
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
config_dir <- if (length(args) >= 1) args[[1]] else 'config/2-21_perm/2026-02-24'

message('=============================================================================')
message('Counterfactual Scenario Validation')
message('=============================================================================')
message(sprintf('Config: %s', config_dir))
message(sprintf('Time:   %s', Sys.time()))
message('')


# Track test results
results <- list()

record_test <- function(name, passed, detail = '') {
  results[[length(results) + 1]] <<- list(
    name = name,
    passed = passed,
    detail = detail
  )
  status <- if (passed) 'PASS' else 'FAIL'
  message(sprintf('  [%s] %s', status, name))
  if (nchar(detail) > 0) message(sprintf('         %s', detail))
}


# =============================================================================
# Load shared data
# =============================================================================

message('Loading shared data...')

usmca_shares <- read_csv('resources/usmca_shares.csv', show_col_types = FALSE)
country_mapping <- read_csv(
  'resources/country_partner_mapping.csv',
  col_types = cols(cty_code = col_character()),
  show_col_types = FALSE
)

# Load cached import data
cache_file <- 'cache/hs10_by_country_gtap_2024_con.rds'
if (!file.exists(cache_file)) {
  stop('Cache file not found. Run main.R first to build the cache.')
}
hs10_by_country <- readRDS(cache_file) %>%
  filter(!str_detect(hs10, '^(98|99)')) %>%
  filter(!is.na(gtap_code))
message(sprintf('Loaded %s rows of import data',
                format(nrow(hs10_by_country), big.mark = ',')))

# Load full config
message(sprintf('Loading scenario config from %s...', config_dir))
full_config <- load_scenario_config(config_dir)

# Load metal content for direct calc_weighted_etr() calls
metal_content <- load_metal_content(
  metal_content_config = full_config$other_params$metal_content,
  import_data = hs10_by_country
)

message('')


# =============================================================================
# Helper: run calc_weighted_etr() with selective authority removal
# =============================================================================

#' Run ETR calculation with optional authority zeroing
#'
#' @param config Full config from load_scenario_config()
#' @param import_data HS10 x country import data
#' @param usmca_data USMCA share data
#' @param metal_content Metal content shares
#' @param zero_232 If TRUE, pass NULL for all 232 parameters
#' @param zero_ieepa_recip If TRUE, pass NULL for IEEPA reciprocal
#' @param zero_ieepa_fent If TRUE, pass NULL for IEEPA fentanyl
#' @param zero_s122 If TRUE, pass NULL for Section 122
#' @param zero_s301 If TRUE, pass NULL for Section 301
run_etr <- function(config, import_data, usmca_data, metal_content,
                    zero_232 = FALSE, zero_ieepa_recip = FALSE,
                    zero_ieepa_fent = FALSE, zero_s122 = FALSE,
                    zero_s301 = FALSE) {
  calc_weighted_etr(
    rates_s232              = if (zero_232) NULL else {
      if (!is.null(config$params_s232)) config$params_s232$rate_matrix else NULL
    },
    usmca_exempt_s232       = if (zero_232) NULL else {
      if (!is.null(config$params_s232)) config$params_s232$usmca_exempt else NULL
    },
    s232_target_total_rules = if (zero_232) NULL else {
      if (!is.null(config$params_s232)) config$params_s232$target_total_rules else NULL
    },
    rates_ieepa_reciprocal = if (zero_ieepa_recip) NULL else config$rates_ieepa_reciprocal,
    rates_ieepa_fentanyl   = if (zero_ieepa_fent) NULL else config$rates_ieepa_fentanyl,
    rates_s122             = if (zero_s122) NULL else config$rates_s122,
    rates_s301             = if (zero_s301) NULL else config$rates_s301,
    import_data            = import_data,
    usmca_data             = usmca_data,
    us_auto_content_share  = config$other_params$us_auto_content_share,
    auto_rebate            = config$other_params$auto_rebate_rate,
    us_assembly_share      = config$other_params$us_auto_assembly_share,
    ieepa_usmca_exempt     = config$other_params$ieepa_usmca_exception,
    s122_usmca_exempt      = config$other_params$s122_usmca_exception %||% 0,
    s301_usmca_exempt      = config$other_params$s301_usmca_exception %||% 0,
    metal_content          = metal_content,
    metal_programs         = config$other_params$metal_content$metal_programs %||% character(0),
    program_metal_types    = config$other_params$metal_content$program_metal_types %||% NULL,
    mfn_rates              = config$mfn_rates,
    mfn_exemption_shares   = config$mfn_exemption_shares
  )
}


# =============================================================================
# Compute ETRs for all scenarios
# =============================================================================

message('Computing ETRs for 6 scenarios...')

message('  [1/6] Full authority...')
etrs_full <- run_etr(full_config, hs10_by_country, usmca_shares, metal_content)

message('  [2/6] No IEEPA (reciprocal + fentanyl removed)...')
etrs_no_ieepa <- run_etr(full_config, hs10_by_country, usmca_shares, metal_content,
                          zero_ieepa_recip = TRUE, zero_ieepa_fent = TRUE)

message('  [3/6] No 232...')
etrs_no_232 <- run_etr(full_config, hs10_by_country, usmca_shares, metal_content,
                        zero_232 = TRUE)

message('  [4/6] No S301...')
etrs_no_301 <- run_etr(full_config, hs10_by_country, usmca_shares, metal_content,
                        zero_s301 = TRUE)

message('  [5/6] No S122...')
etrs_no_s122 <- run_etr(full_config, hs10_by_country, usmca_shares, metal_content,
                         zero_s122 = TRUE)

message('  [6/6] MFN only (all authorities removed)...')
etrs_mfn_only <- run_etr(full_config, hs10_by_country, usmca_shares, metal_content,
                          zero_232 = TRUE, zero_ieepa_recip = TRUE,
                          zero_ieepa_fent = TRUE, zero_s122 = TRUE,
                          zero_s301 = TRUE)

message('')


# =============================================================================
# Test 1: Baseline identity (delta = 0 when same config used twice)
# =============================================================================

message('--- Test 1: Baseline identity ---')

# Run the same config a second time
etrs_full_2 <- run_etr(full_config, hs10_by_country, usmca_shares, metal_content)

# Compare via calc_delta (the function used in production)
delta <- calc_delta(etrs_full, etrs_full_2)
max_abs_delta <- max(abs(delta$etr))
record_test(
  'Self-delta is zero',
  max_abs_delta < 1e-10,
  sprintf('max|delta| = %.2e', max_abs_delta)
)

# Compare level column directly
level_diff <- max(abs(etrs_full$level - etrs_full_2$level))
record_test(
  'Level columns identical across runs',
  level_diff < 1e-10,
  sprintf('max|level_diff| = %.2e', level_diff)
)

# Compare all rate columns
etr_diff <- max(abs(etrs_full$etr - etrs_full_2$etr))
record_test(
  'ETR columns identical across runs',
  etr_diff < 1e-10,
  sprintf('max|etr_diff| = %.2e', etr_diff)
)

# Compare coverage bases
base_s232_diff <- max(abs(etrs_full$base_s232 - etrs_full_2$base_s232))
base_ieepa_diff <- max(abs(etrs_full$base_ieepa - etrs_full_2$base_ieepa))
base_neither_diff <- max(abs(etrs_full$base_neither - etrs_full_2$base_neither))
record_test(
  'Coverage bases identical across runs',
  max(base_s232_diff, base_ieepa_diff, base_neither_diff) < 1e-10,
  sprintf('max diffs: s232=%.2e, ieepa=%.2e, neither=%.2e',
          base_s232_diff, base_ieepa_diff, base_neither_diff)
)

message('')


# =============================================================================
# Test 2: No-IEEPA zeros the correct columns and reduces rates
# =============================================================================

message('--- Test 2: No-IEEPA scenario ---')

# Join full and no-IEEPA results for comparison
comparison_ieepa <- etrs_full %>%
  select(hs10, cty_code, level_full = level, base_ieepa_full = base_ieepa) %>%
  inner_join(
    etrs_no_ieepa %>% select(hs10, cty_code, level_no_ieepa = level),
    by = c('hs10', 'cty_code')
  )

# IEEPA reciprocal and fentanyl are always additive in the stacking formula
# (they never replace or reduce another authority), so removing them can only
# decrease or maintain rates.
violations_ieepa <- comparison_ieepa %>%
  filter(level_no_ieepa > level_full + 1e-10)
record_test(
  'No-IEEPA levels <= full levels (IEEPA is purely additive)',
  nrow(violations_ieepa) == 0,
  sprintf('%d violations out of %s rows',
          nrow(violations_ieepa), format(nrow(comparison_ieepa), big.mark = ','))
)

# Check affected row count. The config may have zero IEEPA reciprocal/fentanyl
# rates (sparse YAML), in which case 0% affected is expected.
affected_ieepa <- comparison_ieepa %>%
  filter(level_full - level_no_ieepa > 1e-10)
pct_affected_ieepa <- 100 * nrow(affected_ieepa) / nrow(comparison_ieepa)
has_ieepa_rates <- (!is.null(full_config$rates_ieepa_reciprocal) &&
                      nrow(full_config$rates_ieepa_reciprocal) > 0) ||
                   (!is.null(full_config$rates_ieepa_fentanyl) &&
                      nrow(full_config$rates_ieepa_fentanyl) > 0)
record_test(
  'IEEPA removal: affected rows consistent with config',
  if (has_ieepa_rates) pct_affected_ieepa > 0 else pct_affected_ieepa == 0,
  sprintf('%.1f%% of rows affected (%s rows); IEEPA rates %s',
          pct_affected_ieepa, format(nrow(affected_ieepa), big.mark = ','),
          if (has_ieepa_rates) 'present' else 'absent/empty')
)

# base_ieepa tracks coverage by IEEPA OR S122 (when not covered by 232).
# Removing IEEPA reciprocal+fentanyl still leaves S122 in base_ieepa.
# So we check the weaker property: no-IEEPA base_ieepa <= full base_ieepa.
record_test(
  'No-IEEPA base_ieepa <= full base_ieepa',
  sum(etrs_no_ieepa$base_ieepa) <= sum(etrs_full$base_ieepa) + 1,
  sprintf('no_ieepa: $%.0fM, full: $%.0fM',
          sum(etrs_no_ieepa$base_ieepa) / 1e6,
          sum(etrs_full$base_ieepa) / 1e6)
)

# 232 coverage should be unchanged (IEEPA doesn't affect 232 rates)
coverage_s232_full <- sum(etrs_full$base_s232)
coverage_s232_no_ieepa <- sum(etrs_no_ieepa$base_s232)
record_test(
  '232 coverage unchanged when IEEPA removed',
  abs(coverage_s232_full - coverage_s232_no_ieepa) < 1,
  sprintf('full: $%.0fM, no_ieepa: $%.0fM',
          coverage_s232_full / 1e6, coverage_s232_no_ieepa / 1e6)
)

message('')


# =============================================================================
# Test 3: Stacking consistency (all scenarios)
# =============================================================================

message('--- Test 3: Stacking consistency ---')

scenarios <- list(
  full     = etrs_full,
  no_ieepa = etrs_no_ieepa,
  no_232   = etrs_no_232,
  no_301   = etrs_no_301,
  no_s122  = etrs_no_s122,
  mfn_only = etrs_mfn_only
)

for (scenario_name in names(scenarios)) {
  etrs <- scenarios[[scenario_name]]

  # All levels non-negative (MFN + any policy rates >= 0)
  has_negative <- any(etrs$level < -1e-10)
  record_test(
    sprintf('[%s] All levels non-negative', scenario_name),
    !has_negative,
    sprintf('min(level) = %.6f', min(etrs$level))
  )

  # Coverage bases are mutually exclusive and sum to imports
  base_sum <- etrs$base_s232 + etrs$base_ieepa + etrs$base_neither
  max_coverage_gap <- max(abs(base_sum - etrs$imports))
  record_test(
    sprintf('[%s] Coverage bases sum to imports', scenario_name),
    max_coverage_gap < 1e-6,
    sprintf('max|gap| = %.2e', max_coverage_gap)
  )

  # level == etr for single-config runs (no baseline subtraction)
  max_level_etr_diff <- max(abs(etrs$level - etrs$etr))
  record_test(
    sprintf('[%s] level == etr (single-config identity)', scenario_name),
    max_level_etr_diff < 1e-10,
    sprintf('max|diff| = %.2e', max_level_etr_diff)
  )
}

message('')


# =============================================================================
# Test 4: Cross-scenario monotonicity
# =============================================================================

message('--- Test 4: Cross-scenario monotonicity ---')

check_monotonicity <- function(higher_name, lower_name, etrs_higher, etrs_lower,
                               strict = TRUE) {
  joined <- etrs_higher %>%
    select(hs10, cty_code, level_hi = level) %>%
    inner_join(
      etrs_lower %>% select(hs10, cty_code, level_lo = level),
      by = c('hs10', 'cty_code')
    )
  violations <- joined %>%
    filter(level_lo > level_hi + 1e-10)
  record_test(
    sprintf('%s >= %s%s', higher_name, lower_name,
            if (!strict) ' (informational)' else ''),
    if (strict) nrow(violations) == 0 else TRUE,
    sprintf('%d violations out of %s rows',
            nrow(violations), format(nrow(joined), big.mark = ','))
  )
}

# IEEPA (reciprocal + fentanyl) is purely additive in all stacking branches:
#   With 232:  s232 + IEEPA*nonmetal + fent[+China] + s122 + s301
#   Without 232: IEEPA + fent + s122 + s301
# Removing IEEPA always reduces or maintains the total.
check_monotonicity('full', 'no_ieepa', etrs_full, etrs_no_ieepa)

# S301 is unconditionally cumulative (no mutual exclusion with any authority).
# Removing it always reduces or maintains the total.
check_monotonicity('full', 'no_301', etrs_full, etrs_no_301)

# S122 is additive: scales by nonmetal_share when 232 active, full rate otherwise.
# In both cases it contributes >= 0 to the total.
check_monotonicity('full', 'no_s122', etrs_full, etrs_no_s122)

# 232 interacts with IEEPA via mutual exclusion on the metal portion:
#   With 232: s232_max + IEEPA*nonmetal_share (IEEPA blocked on metal portion)
#   Without 232: full IEEPA (applies to entire product)
# If IEEPA * metal_share > s232_max, removing 232 can INCREASE rates.
# This is expected behavior, not a bug. Report as informational.
check_monotonicity('full', 'no_232', etrs_full, etrs_no_232, strict = FALSE)

# All single-disable scenarios >= MFN-only
check_monotonicity('no_ieepa', 'mfn_only', etrs_no_ieepa, etrs_mfn_only)
check_monotonicity('no_232', 'mfn_only', etrs_no_232, etrs_mfn_only)
check_monotonicity('no_301', 'mfn_only', etrs_no_301, etrs_mfn_only)
check_monotonicity('no_s122', 'mfn_only', etrs_no_s122, etrs_mfn_only)

# Full >= MFN-only (all authorities combined always >= baseline MFN)
check_monotonicity('full', 'mfn_only', etrs_full, etrs_mfn_only)

message('')


# =============================================================================
# Spot-checks
# =============================================================================

message('--- Spot-checks ---')

# Spot-check: no_232 should only affect products that had 232 coverage.
# base_s232 == 0 can mean either (a) no 232 rate or (b) zero imports with a
# 232 rate. Case (b) products have base_s232=0 but their *level* changes when
# 232 is removed (rate changes from s232 branch to IEEPA branch). Filter to
# imports > 0 to isolate products truly not covered by 232.
comparison_232 <- etrs_full %>%
  select(hs10, cty_code, level_full = level, base_s232, imports) %>%
  inner_join(
    etrs_no_232 %>% select(hs10, cty_code, level_no_232 = level),
    by = c('hs10', 'cty_code')
  )

non_232_with_imports <- comparison_232 %>%
  filter(base_s232 == 0, imports > 0)
non_232_changed <- non_232_with_imports %>%
  filter(abs(level_full - level_no_232) > 1e-10)
record_test(
  'no_232 does not affect non-232 products (with imports > 0)',
  nrow(non_232_changed) == 0,
  sprintf('%d non-232 products changed out of %s non-232 rows (imports > 0)',
          nrow(non_232_changed),
          format(nrow(non_232_with_imports), big.mark = ','))
)

# Count of 232-affected products (expect ~1,400 HTS10 codes in steel/aluminum/auto/copper)
n_232_hs10 <- comparison_232 %>%
  filter(base_s232 > 0) %>%
  distinct(hs10) %>%
  nrow()
record_test(
  '232 covers a plausible number of HTS10 codes',
  n_232_hs10 > 500 & n_232_hs10 < 5000,
  sprintf('%s unique HTS10 codes with 232 coverage', format(n_232_hs10, big.mark = ','))
)

# Spot-check: no_301 effects concentrated on China (cty_code 5700)
# S301 primarily targets Chinese products
comparison_301 <- etrs_full %>%
  select(hs10, cty_code, level_full = level) %>%
  inner_join(
    etrs_no_301 %>% select(hs10, cty_code, level_no_301 = level),
    by = c('hs10', 'cty_code')
  )

affected_301 <- comparison_301 %>%
  filter(abs(level_full - level_no_301) > 1e-10)

non_china_301 <- affected_301 %>%
  filter(cty_code != '5700')

pct_china_301 <- if (nrow(affected_301) > 0) {
  100 * (1 - nrow(non_china_301) / nrow(affected_301))
} else {
  100
}
record_test(
  'S301 removal primarily affects China',
  pct_china_301 > 50 || nrow(affected_301) == 0,
  sprintf('%.1f%% of %s affected rows are China (cty_code=5700)',
          pct_china_301, format(nrow(affected_301), big.mark = ','))
)

# Spot-check: MFN-only should have zero coverage for all policy authorities
record_test(
  'MFN-only has zero 232 coverage',
  all(etrs_mfn_only$base_s232 == 0),
  sprintf('sum(base_s232) = %.0f', sum(etrs_mfn_only$base_s232))
)

record_test(
  'MFN-only has zero IEEPA coverage',
  all(etrs_mfn_only$base_ieepa == 0),
  sprintf('sum(base_ieepa) = %.0f', sum(etrs_mfn_only$base_ieepa))
)

record_test(
  'MFN-only: all imports classified as base_neither',
  abs(sum(etrs_mfn_only$base_neither) - sum(etrs_mfn_only$imports)) < 1,
  sprintf('base_neither = $%.0fM, imports = $%.0fM',
          sum(etrs_mfn_only$base_neither) / 1e6,
          sum(etrs_mfn_only$imports) / 1e6)
)

message('')


# =============================================================================
# Summary
# =============================================================================

message('=============================================================================')
message('Summary')
message('=============================================================================')

n_pass <- sum(sapply(results, function(r) r$passed))
n_fail <- sum(sapply(results, function(r) !r$passed))
n_total <- length(results)

message(sprintf('\n%d / %d tests passed', n_pass, n_total))

if (n_fail > 0) {
  message(sprintf('\nFailed tests:'))
  for (r in results) {
    if (!r$passed) {
      message(sprintf('  FAIL: %s (%s)', r$name, r$detail))
    }
  }
}

# Save results to output/validation/
output_dir <- 'output/validation'
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

output_file <- file.path(output_dir, 'scenario_validation.csv')
results_df <- tibble(
  test = sapply(results, function(r) r$name),
  passed = sapply(results, function(r) r$passed),
  detail = sapply(results, function(r) r$detail)
)
write_csv(results_df, output_file)
message(sprintf('\nResults saved to %s', output_file))

message('\n=============================================================================')
if (n_fail == 0) {
  message('All tests passed.')
} else {
  message(sprintf('%d test(s) FAILED.', n_fail))
}
message('=============================================================================')
