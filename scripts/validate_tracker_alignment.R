# =============================================================================
# validate_tracker_alignment.R
# =============================================================================
#
# Validates calc_weighted_etr() alignment with the tracker by comparing
# per-product rates and overall weighted ETRs.
#
# Usage:
#   cd Tariff-ETRs
#   Rscript scripts/validate_tracker_alignment.R [config_path] [snapshot_name]
#
# Defaults:
#   config_path:   config/tracker_rev10
#   snapshot_name:  snapshot_rev_10
# =============================================================================

library(tidyverse)
library(yaml)
library(here)

# Source ETRs functions
source('src/config_parsing.R')
source('src/calculations.R')
source('src/data_processing.R')

# Parse command-line args
args <- commandArgs(trailingOnly = TRUE)
config_path <- if (length(args) >= 1) args[1] else 'config/tracker_rev10'
snapshot_name <- if (length(args) >= 2) args[2] else 'snapshot_rev_10'

# =============================================================================
# 1. Run ETRs calc_weighted_etr() with the specified config
# =============================================================================

message(sprintf('=== Step 1: Run ETRs with %s ===\n', config_path))

config <- load_scenario_config(config_path)

hs10_by_country <- readRDS('cache/hs10_by_country_gtap_2024_con.rds') %>%
  filter(!str_detect(hs10, '^(98|99)')) %>%
  filter(!is.na(gtap_code))
message(sprintf('  %s import records', format(nrow(hs10_by_country), big.mark = ',')))

country_mapping <- read_csv(
  'resources/country_partner_mapping.csv',
  col_types = cols(cty_code = col_character()),
  show_col_types = FALSE
)

etrs_result <- calc_etrs_for_config(config, hs10_by_country, country_mapping)
hs10_etrs <- etrs_result$hs10_country_etrs

total_imports <- sum(hs10_etrs$imports)
etrs_overall <- sum(hs10_etrs$etr * hs10_etrs$imports) / total_imports

message(sprintf('\nETRs overall tariff level: %.2f%%', etrs_overall * 100))

# =============================================================================
# 2. Load tracker snapshot
# =============================================================================

message(sprintf('\n=== Step 2: Load tracker %s ===\n', snapshot_name))

tracker_dir <- normalizePath(here('..', 'Tariff-Rate-Tracker'))
snapshot_path <- file.path(tracker_dir, 'data', 'timeseries', paste0(snapshot_name, '.rds'))
snapshot <- readRDS(snapshot_path)
message(sprintf('  Loaded %s: %s rows', snapshot_name, format(nrow(snapshot), big.mark = ',')))

# =============================================================================
# 3. Per-product rate comparison (matched universe)
# =============================================================================

message('\n=== Step 3: Per-product rate comparison ===\n')

# Join ETRs rates with tracker rates at HS10 x country level
merged <- hs10_etrs %>%
  inner_join(
    snapshot %>% select(hts10, country, total_rate, base_rate,
                        rate_232, rate_ieepa_recip, rate_ieepa_fent,
                        rate_301, rate_s122, rate_section_201, rate_other,
                        metal_share),
    by = c('hs10' = 'hts10', 'cty_code' = 'country')
  )

matched_imports <- sum(merged$imports)
message(sprintf('  Matched: %s rows, $%.1fB (%.1f%% of imports)',
                format(nrow(merged), big.mark = ','),
                matched_imports / 1e9,
                matched_imports / total_imports * 100))

# Overall weighted comparison (matched universe)
etrs_matched_etr <- sum(merged$etr * merged$imports) / matched_imports
tracker_matched_etr <- sum(merged$total_rate * merged$imports) / matched_imports
matched_diff_pp <- (etrs_matched_etr - tracker_matched_etr) * 100

# Per-product rate differences
merged <- merged %>%
  mutate(rate_diff = etr - total_rate)

# Rate diff distribution
cat('\n')
cat(strrep('=', 70), '\n')
cat(sprintf('VALIDATION: ETRs (%s) vs Tracker (%s)\n', config_path, snapshot_name))
cat(strrep('=', 70), '\n')

cat('\n--- Overall (matched universe) ---\n\n')
cat(sprintf('  Matched imports:         $%6.1fB (%.1f%%)\n',
            matched_imports / 1e9, matched_imports / total_imports * 100))
cat(sprintf('  ETRs weighted ETR:       %6.2f%%\n', etrs_matched_etr * 100))
cat(sprintf('  Tracker weighted ETR:    %6.2f%%\n', tracker_matched_etr * 100))
cat(sprintf('  Divergence:              %+6.2fpp\n', matched_diff_pp))

cat('\n')
if (abs(matched_diff_pp) < 0.1) {
  cat('  >> PASS: Divergence < 0.1pp\n')
} else if (abs(matched_diff_pp) < 0.5) {
  cat('  >> CLOSE: Divergence < 0.5pp\n')
} else {
  cat(sprintf('  >> OUTSIDE TARGET: Divergence = %.2fpp (target < 0.1pp)\n', abs(matched_diff_pp)))
}

# Per-product rate diff distribution
cat('\n--- Per-product rate difference (ETRs - Tracker) ---\n\n')
cat(sprintf('  Mean:    %+.4f\n', mean(merged$rate_diff)))
cat(sprintf('  Median:  %+.4f\n', median(merged$rate_diff)))
cat(sprintf('  SD:       %.4f\n', sd(merged$rate_diff)))
cat(sprintf('  Min:     %+.4f\n', min(merged$rate_diff)))
cat(sprintf('  Max:     %+.4f\n', max(merged$rate_diff)))

# Buckets
exact_match <- sum(abs(merged$rate_diff) < 1e-6)
close_match <- sum(abs(merged$rate_diff) < 0.01 & abs(merged$rate_diff) >= 1e-6)
moderate <- sum(abs(merged$rate_diff) >= 0.01 & abs(merged$rate_diff) < 0.05)
large <- sum(abs(merged$rate_diff) >= 0.05)

cat(sprintf('\n  Exact match (|diff| < 0.0001%%): %6d (%5.1f%%)\n',
            exact_match, exact_match / nrow(merged) * 100))
cat(sprintf('  Close       (|diff| < 1pp):      %6d (%5.1f%%)\n',
            close_match, close_match / nrow(merged) * 100))
cat(sprintf('  Moderate    (1pp - 5pp):          %6d (%5.1f%%)\n',
            moderate, moderate / nrow(merged) * 100))
cat(sprintf('  Large       (|diff| >= 5pp):      %6d (%5.1f%%)\n',
            large, large / nrow(merged) * 100))

# =============================================================================
# 4. Country-level comparison
# =============================================================================

cat('\n--- Country-Level Comparison (matched universe) ---\n')

country_comparison <- merged %>%
  group_by(cty_code) %>%
  summarise(
    weighted_etr = sum(etr * imports),
    weighted_tracker = sum(total_rate * imports),
    imports = sum(imports),
    .groups = 'drop'
  ) %>%
  mutate(
    etrs_etr = weighted_etr / imports,
    tracker_etr = weighted_tracker / imports,
    diff_pp = (etrs_etr - tracker_etr) * 100,
    rev_gap = weighted_etr - weighted_tracker
  )

partner_codes <- c(
  'China' = '5700', 'Canada' = '1220', 'Mexico' = '2010',
  'Japan' = '5880', 'UK' = '4120'
)

cat(strrep('-', 70), '\n')
cat(sprintf('%-12s %10s %10s %10s %12s\n', 'Country', 'ETRs', 'Tracker', 'Diff(pp)', 'Imports($B)'))
cat(strrep('-', 70), '\n')

for (pname in names(partner_codes)) {
  code <- partner_codes[pname]
  row <- country_comparison %>% filter(cty_code == code)
  if (nrow(row) > 0) {
    cat(sprintf('%-12s %9.2f%% %9.2f%% %+9.2f %11.1f\n',
                pname, row$etrs_etr * 100, row$tracker_etr * 100,
                row$diff_pp, row$imports / 1e9))
  }
}

cat(strrep('-', 70), '\n')
cat(sprintf('%-12s %9.2f%% %9.2f%% %+9.2f %11.1f\n',
            'MATCHED', etrs_matched_etr * 100, tracker_matched_etr * 100,
            matched_diff_pp, matched_imports / 1e9))

# =============================================================================
# 5. Divergence by authority (what rate column differs most?)
# =============================================================================

cat('\n--- Divergence by Tariff Authority ---\n')
cat('  (Import-weighted mean absolute difference in rate component)\n\n')

# For products that differ, decompose which authority contributes
differing <- merged %>% filter(abs(rate_diff) >= 1e-6)

if (nrow(differing) > 0) {
  # We can't directly compare authority columns since ETRs only outputs etr (total).
  # But we have the tracker's per-authority columns. Summarize tracker authority means.
  cat(sprintf('  Products with any diff: %d (%.1f%% of matched)\n',
              nrow(differing), nrow(differing) / nrow(merged) * 100))
  cat(sprintf('  Import value of differing products: $%.1fB\n',
              sum(differing$imports) / 1e9))

  # Where does the overall divergence come from? Top contributors.
  cat('\n  Top 10 product-countries by |rate_diff * imports|:\n')
  top_products <- differing %>%
    mutate(impact = abs(rate_diff * imports)) %>%
    arrange(desc(impact)) %>%
    head(10)

  cat(sprintf('  %-12s %-6s %8s %8s %8s %10s\n',
              'HS10', 'Cty', 'ETRs%', 'Tracker%', 'Diff(pp)', 'Impact($M)'))
  for (i in seq_len(nrow(top_products))) {
    r <- top_products[i, ]
    cat(sprintf('  %-12s %-6s %7.2f%% %7.2f%% %+7.2f %+10.0f\n',
                r$hs10, r$cty_code, r$etr * 100, r$total_rate * 100,
                r$rate_diff * 100, r$rate_diff * r$imports / 1e6))
  }
}

# =============================================================================
# 6. Unmatched universe
# =============================================================================

unmatched_imports <- total_imports - matched_imports

cat('\n--- Unmatched Universe ---\n\n')
cat(sprintf('  Unmatched imports: $%.1fB (%.1f%%)\n',
            unmatched_imports / 1e9, unmatched_imports / total_imports * 100))

unmatched <- hs10_etrs %>%
  anti_join(
    snapshot %>% select(hts10, country),
    by = c('hs10' = 'hts10', 'cty_code' = 'country')
  )
if (nrow(unmatched) > 0) {
  unmatched_etr <- sum(unmatched$etr * unmatched$imports) / sum(unmatched$imports)
  cat(sprintf('  Unmatched weighted ETR: %.2f%% (ETRs-only)\n', unmatched_etr * 100))
}

message('\nDone.')
