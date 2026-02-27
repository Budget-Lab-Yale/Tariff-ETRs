# =============================================================================
# compare_s122_country_etrs.R
# =============================================================================
#
# Compare our model's country-level tariff levels against an external benchmark
# spreadsheet (s122_actual_country_comparison.xlsx).
#
# Reads from existing output — does NOT rerun the model.
#
# Mapping (output date -> xlsx column):
#   2026-01-01 -> "Before SCOTUS (%)"
#   2026-02-24 -> "S122 @ 10% (%)"
#   2026-07-24 -> "IEEPA Strike Down (%)"
#
# Usage: Rscript scripts/compare_s122_country_etrs.R
# =============================================================================

library(tidyverse)
library(readxl)

# ---- Configuration ----------------------------------------------------------
scenario          <- '2-21_temp'
levels_file       <- sprintf('output/%s/levels_by_census_country.csv', scenario)
benchmark_file    <- 'C:/Users/jar335/Downloads/s122_actual_country_comparison.xlsx'
iso3_mapping_file <- 'resources/census_to_iso3.csv'

# Date -> xlsx column mapping
# Auto-detect dates from the output file; map by position:
#   earliest = Before SCOTUS, middle = S122 @ 10%, latest = IEEPA Strike Down
date_to_regime <- NULL  # will be set after reading model output

# ---- Load model output ------------------------------------------------------
model <- read_csv(levels_file, col_types = cols(cty_code = col_character()))

# Expect columns: date, cty_code, [country_name], level (or etr/rate)
# Detect the value column
value_col <- intersect(names(model), c('level', 'etr', 'rate', 'tariff_level'))[1]
if (is.na(value_col)) stop('Cannot find value column in ', levels_file)

model <- model %>%
  rename(value = !!sym(value_col)) %>%
  mutate(date = as.character(date))

# Auto-detect date -> regime mapping from the sorted dates in the file
dates_sorted <- sort(unique(model$date))
if (length(dates_sorted) != 3) {
  stop('Expected 3 dates in output, found ', length(dates_sorted), ': ',
       paste(dates_sorted, collapse = ', '))
}
date_to_regime <- c('before', 's122_10', 'ieepa_strike')
names(date_to_regime) <- dates_sorted
message(sprintf('Date mapping: %s -> %s',
                paste(names(date_to_regime), collapse = ', '),
                paste(date_to_regime, collapse = ', ')))

model <- model %>%
  mutate(regime = date_to_regime[date])

model_wide <- model %>%
  select(cty_code, regime, value) %>%
  pivot_wider(id_cols = cty_code, names_from = regime,
              values_from = value, names_prefix = 'model_')

# ---- Load Census -> ISO3 mapping --------------------------------------------
census_to_iso3 <- read_csv(iso3_mapping_file,
                           col_types = cols(cty_code = col_character()))

model_wide <- model_wide %>%
  left_join(census_to_iso3, by = 'cty_code') %>%
  filter(!is.na(iso3))

# ---- Load external benchmark ------------------------------------------------
benchmark_raw <- read_excel(benchmark_file, sheet = 'Country Comparison')

s122_benchmark_col <- case_when(
  'S122 @ 10% (%)' %in% names(benchmark_raw) ~ 'S122 @ 10% (%)',
  'S122 @ 15% (%)' %in% names(benchmark_raw) ~ 'S122 @ 15% (%)',
  TRUE ~ NA_character_
)

if (is.na(s122_benchmark_col)) {
  stop('Benchmark file missing expected S122 column (looked for "S122 @ 10% (%)" or "S122 @ 15% (%)").')
}

if (s122_benchmark_col == 'S122 @ 15% (%)') {
  message('Benchmark column is labeled "S122 @ 15% (%)"; using it as the S122 benchmark series.')
}

benchmark <- benchmark_raw %>%
  rename(
    country       = `Import Origin`,
    iso3          = `ISO Code`,
    bm_imports_bn = `US Imports ($bn)`,
    bm_before     = `Before SCOTUS (%)`,
    bm_ieepa      = `IEEPA Strike Down (%)`,
    bm_s122_10    = !!sym(s122_benchmark_col)
  ) %>%
  select(country, iso3, bm_imports_bn, bm_before, bm_s122_10, bm_ieepa)

# ---- Join model to benchmark ------------------------------------------------
comparison <- benchmark %>%
  left_join(model_wide %>% select(iso3, model_before, model_s122_10, model_ieepa_strike),
            by = 'iso3') %>%
  mutate(
    diff_before  = model_before       - bm_before,
    diff_s122_10 = model_s122_10      - bm_s122_10,
    diff_ieepa   = model_ieepa_strike - bm_ieepa
  )

# ---- Print results ----------------------------------------------------------
cat('\n')
cat('=====================================================================\n')
cat('  MODEL vs BENCHMARK COMPARISON (Country-Level Tariff Levels)\n')
cat('=====================================================================\n\n')

# Global
global <- comparison %>% filter(iso3 == 'WLD')
if (nrow(global) == 1 && !is.na(global$model_before)) {
  cat(sprintf('GLOBAL:\n'))
  cat(sprintf('  %-25s  %8s  %8s  %8s\n', '', 'Before', 'S122@10%', 'Strike'))
  cat(sprintf('  %-25s  %8.2f  %8.2f  %8.2f\n', 'Model',
              global$model_before, global$model_s122_10, global$model_ieepa_strike))
  cat(sprintf('  %-25s  %8.2f  %8.2f  %8.2f\n', 'Benchmark',
              global$bm_before, global$bm_s122_10, global$bm_ieepa))
  cat(sprintf('  %-25s  %+8.2f  %+8.2f  %+8.2f\n', 'Difference (pp)',
              global$diff_before, global$diff_s122_10, global$diff_ieepa))
}

# Top countries by import value
cat('\n---------------------------------------------------------------------\n')
cat('  Top 30 Countries by Import Value\n')
cat('---------------------------------------------------------------------\n\n')

top <- comparison %>%
  filter(!iso3 %in% c('WLD', 'EU'), !is.na(model_before)) %>%
  arrange(desc(bm_imports_bn)) %>%
  slice_head(n = 30)

cat(sprintf('%-20s %3s %5s | %-19s | %-19s | %-19s\n',
            '', '', '',
            '  Before SCOTUS', '    S122 @ 10%', '  IEEPA Strike'))
cat(sprintf('%-20s %3s %5s | %6s %6s %5s | %6s %6s %5s | %6s %6s %5s\n',
            'Country', 'ISO', 'Imp$B',
            'Us', 'Them', 'Diff',
            'Us', 'Them', 'Diff',
            'Us', 'Them', 'Diff'))
cat(strrep('-', 101), '\n')

for (i in seq_len(nrow(top))) {
  r <- top[i, ]
  cat(sprintf('%-20s %3s %5.1f | %5.1f %5.1f %+5.1f | %5.1f %5.1f %+5.1f | %5.1f %5.1f %+5.1f\n',
              str_trunc(r$country, 20),
              r$iso3,
              r$bm_imports_bn,
              r$model_before, r$bm_before, r$diff_before,
              r$model_s122_10, r$bm_s122_10, r$diff_s122_10,
              r$model_ieepa_strike, r$bm_ieepa, r$diff_ieepa))
}

# ---- Summary statistics -----------------------------------------------------
matched <- comparison %>%
  filter(!is.na(model_before), !iso3 %in% c('WLD', 'EU'))

cat('\n---------------------------------------------------------------------\n')
cat('  Summary Statistics (matched countries)\n')
cat('---------------------------------------------------------------------\n\n')

cat(sprintf('  Countries in benchmark: %d\n', nrow(benchmark)))
cat(sprintf('  Countries matched to model: %d\n', nrow(matched)))
cat(sprintf('  Unmatched: %d\n\n',
            nrow(comparison %>% filter(is.na(model_before), !iso3 %in% c('WLD', 'EU')))))

for (col_label in c('Before SCOTUS', 'S122 @ 10%', 'IEEPA Strike')) {
  diff_col <- case_when(
    col_label == 'Before SCOTUS' ~ 'diff_before',
    col_label == 'S122 @ 10%'    ~ 'diff_s122_10',
    col_label == 'IEEPA Strike'  ~ 'diff_ieepa'
  )
  diffs <- matched[[diff_col]]
  diffs <- diffs[!is.na(diffs)]

  cat(sprintf('  %s:\n', col_label))
  cat(sprintf('    Mean diff:       %+6.2f pp\n', mean(diffs)))
  cat(sprintf('    Mean abs diff:   %6.2f pp\n', mean(abs(diffs))))
  cat(sprintf('    Median abs diff: %6.2f pp\n', median(abs(diffs))))
  cat(sprintf('    Max abs diff:    %6.2f pp\n', max(abs(diffs))))
  cat(sprintf('    Within 1pp:      %d / %d (%.0f%%)\n',
              sum(abs(diffs) < 1), length(diffs),
              sum(abs(diffs) < 1) / length(diffs) * 100))
  cat(sprintf('    Within 2pp:      %d / %d (%.0f%%)\n',
              sum(abs(diffs) < 2), length(diffs),
              sum(abs(diffs) < 2) / length(diffs) * 100))
  cat('\n')
}

# ---- Countries with largest differences ------------------------------------
cat('---------------------------------------------------------------------\n')
cat('  Largest Differences (by max absolute diff across regimes)\n')
cat('---------------------------------------------------------------------\n\n')

outliers <- matched %>%
  mutate(max_abs_diff = pmax(abs(diff_before), abs(diff_s122_10), abs(diff_ieepa))) %>%
  arrange(desc(max_abs_diff)) %>%
  slice_head(n = 20)

cat(sprintf('%-20s %3s %5s | %-19s | %-19s | %-19s\n',
            '', '', '',
            '  Before SCOTUS', '    S122 @ 10%', '  IEEPA Strike'))
cat(sprintf('%-20s %3s %5s | %6s %6s %5s | %6s %6s %5s | %6s %6s %5s\n',
            'Country', 'ISO', 'Imp$B',
            'Us', 'Them', 'Diff',
            'Us', 'Them', 'Diff',
            'Us', 'Them', 'Diff'))
cat(strrep('-', 101), '\n')

for (i in seq_len(nrow(outliers))) {
  r <- outliers[i, ]
  cat(sprintf('%-20s %3s %5.1f | %5.1f %5.1f %+5.1f | %5.1f %5.1f %+5.1f | %5.1f %5.1f %+5.1f\n',
              str_trunc(r$country, 20),
              r$iso3,
              r$bm_imports_bn,
              r$model_before, r$bm_before, r$diff_before,
              r$model_s122_10, r$bm_s122_10, r$diff_s122_10,
              r$model_ieepa_strike, r$bm_ieepa, r$diff_ieepa))
}

cat('\nDone.\n')
