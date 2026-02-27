# =============================================================================
# compare_cbo_etrs.R
# =============================================================================
#
# Compare our model's country-level tariff levels (2026-01-01) against CBO's
# product_country_etr.csv from the conventional-tariff-analysis-model
# (nov15_tariff_policy scenario).
#
# CBO columns:
#   effective_tariff_rate = MFN / baseline tariff
#   new_tariff_rate       = new policy tariff (232, reciprocal, etc.)
#   total_etr             = effective + new
#
# Our model:
#   level = total tariff including MFN (comparable to CBO's total_etr)
#
# Both are aggregated to country level using our 2024 import weights.
#
# Usage: Rscript scripts/compare_cbo_etrs.R
# =============================================================================

library(tidyverse)

# ---- Configuration ----------------------------------------------------------
scenario          <- '2-21_temp'
our_levels_file   <- sprintf('output/%s/levels_by_census_country.csv', scenario)
cbo_file          <- '../conventional-tariff-analysis-model/outputs/nov15_tariff_policy/product_country_etr.csv'
cache_file        <- 'cache/hs10_by_country_gtap_2024_con.rds'
iso3_mapping_file <- 'resources/census_to_iso3.csv'
compare_date      <- '2026-01-01'

# ---- Load our import weights ------------------------------------------------
message('Loading import weights from cache...')
imports_raw <- readRDS(cache_file) %>%
  as_tibble() %>%
  mutate(cty_code = as.character(cty_code))

# Aggregate to HS10 x country (drop GTAP dimension)
imports_hs10_cty <- imports_raw %>%
  group_by(hs10, cty_code) %>%
  summarise(imports = sum(imports), .groups = 'drop')

message(sprintf('  %s HS10 x country rows, %s total imports',
                format(nrow(imports_hs10_cty), big.mark = ','),
                format(sum(imports_hs10_cty$imports), big.mark = ',')))

# ---- Load CBO product-country ETRs -----------------------------------------
message('Loading CBO product_country_etr.csv...')
cbo <- read_csv(cbo_file, col_types = cols(
  I_COMMODITY = col_character(),
  CTY_CODE    = col_character(),
  Country     = col_character(),
  effective_tariff_rate = col_double(),
  new_tariff_rate       = col_double(),
  total_etr             = col_double()
))

message(sprintf('  %s rows, %d countries, %d products',
                format(nrow(cbo), big.mark = ','),
                n_distinct(cbo$CTY_CODE),
                n_distinct(cbo$I_COMMODITY)))

# ---- Join CBO with import weights ------------------------------------------
# Match on HS10 and country code
cbo_weighted <- cbo %>%
  rename(hs10 = I_COMMODITY, cty_code = CTY_CODE) %>%
  inner_join(imports_hs10_cty, by = c('hs10', 'cty_code'))

message(sprintf('  Matched %s of %s CBO rows to import data (%.1f%%)',
                format(nrow(cbo_weighted), big.mark = ','),
                format(nrow(cbo), big.mark = ','),
                nrow(cbo_weighted) / nrow(cbo) * 100))

# Aggregate to country level using import weights
cbo_country <- cbo_weighted %>%
  group_by(cty_code) %>%
  summarise(
    cbo_total_etr   = sum(total_etr * imports) / sum(imports) * 100,
    cbo_new_rate    = sum(new_tariff_rate * imports) / sum(imports) * 100,
    cbo_mfn         = sum(effective_tariff_rate * imports) / sum(imports) * 100,
    cbo_imports     = sum(imports),
    .groups = 'drop'
  )

message(sprintf('  Aggregated to %d countries', nrow(cbo_country)))

# ---- Load our model output --------------------------------------------------
message('Loading our model output...')
our_levels <- read_csv(our_levels_file, col_types = cols(
  cty_code = col_character()
)) %>%
  filter(date == compare_date) %>%
  select(cty_code, country_name, our_level = level)

message(sprintf('  %d countries for date %s', nrow(our_levels), compare_date))

# ---- Load country imports for our model (total by country) ------------------
our_imports <- imports_hs10_cty %>%
  group_by(cty_code) %>%
  summarise(our_imports = sum(imports), .groups = 'drop')

# ---- Join everything --------------------------------------------------------
comparison <- cbo_country %>%
  full_join(our_levels, by = 'cty_code') %>%
  left_join(our_imports, by = 'cty_code')

# Load ISO3 mapping for display
census_to_iso3 <- read_csv(iso3_mapping_file, col_types = cols(
  cty_code = col_character()
))

comparison <- comparison %>%
  left_join(census_to_iso3, by = 'cty_code') %>%
  mutate(
    diff_total = our_level - cbo_total_etr,
    diff_new   = our_level - cbo_mfn - cbo_new_rate  # same as diff_total
  )

# ---- Print results ----------------------------------------------------------
cat('\n')
cat('=====================================================================\n')
cat('  OUR MODEL vs CBO COMPARISON (Country-Level Tariff Levels)\n')
cat(sprintf('  Our scenario: %s, date: %s\n', scenario, compare_date))
cat(sprintf('  CBO scenario: nov15_tariff_policy\n'))
cat('=====================================================================\n\n')

# Global weighted average
global_cbo <- with(comparison %>% filter(!is.na(cbo_total_etr), !is.na(our_level)),
                   sum(cbo_total_etr * cbo_imports) / sum(cbo_imports))
global_our <- with(comparison %>% filter(!is.na(cbo_total_etr), !is.na(our_level)),
                   sum(our_level * cbo_imports) / sum(cbo_imports))

cat(sprintf('GLOBAL (import-weighted, matched countries only):\n'))
cat(sprintf('  Our model:  %6.2f%%\n', global_our))
cat(sprintf('  CBO model:  %6.2f%%\n', global_cbo))
cat(sprintf('  Difference: %+6.2f pp\n', global_our - global_cbo))

# Top countries by import value
cat('\n---------------------------------------------------------------------\n')
cat('  Top 30 Countries by Import Value\n')
cat('---------------------------------------------------------------------\n\n')

matched <- comparison %>%
  filter(!is.na(our_level), !is.na(cbo_total_etr))

top <- matched %>%
  arrange(desc(cbo_imports)) %>%
  slice_head(n = 30)

cat(sprintf('%-22s %3s %7s | %7s %7s %7s | %7s\n',
            '', '', 'Imp($M)',
            'Us(%)', 'CBO(%)', 'Diff',
            'CBO New'))
cat(strrep('-', 78), '\n')

for (i in seq_len(nrow(top))) {
  r <- top[i, ]
  label <- if (!is.na(r$country_name)) str_trunc(r$country_name, 22) else r$cty_code
  iso <- if (!is.na(r$iso3)) r$iso3 else '   '
  cat(sprintf('%-22s %3s %7.0f | %7.2f %7.2f %+7.2f | %7.2f\n',
              label, iso,
              r$cbo_imports / 1e6,
              r$our_level, r$cbo_total_etr, r$diff_total,
              r$cbo_new_rate))
}

# ---- Summary statistics -----------------------------------------------------
cat('\n---------------------------------------------------------------------\n')
cat('  Summary Statistics (matched countries)\n')
cat('---------------------------------------------------------------------\n\n')

cat(sprintf('  Countries in CBO output: %d\n', sum(!is.na(comparison$cbo_total_etr))))
cat(sprintf('  Countries in our output: %d\n', sum(!is.na(comparison$our_level))))
cat(sprintf('  Countries matched: %d\n', nrow(matched)))
cat(sprintf('  CBO-only (no match in ours): %d\n',
            sum(!is.na(comparison$cbo_total_etr) & is.na(comparison$our_level))))
cat(sprintf('  Ours-only (no match in CBO): %d\n\n',
            sum(is.na(comparison$cbo_total_etr) & !is.na(comparison$our_level))))

diffs <- matched$diff_total
cat(sprintf('  Total ETR difference (Us - CBO):\n'))
cat(sprintf('    Mean diff:       %+6.2f pp\n', mean(diffs)))
cat(sprintf('    Mean abs diff:   %6.2f pp\n', mean(abs(diffs))))
cat(sprintf('    Median abs diff: %6.2f pp\n', median(abs(diffs))))
cat(sprintf('    Max abs diff:    %6.2f pp (cty %s)\n',
            max(abs(diffs)),
            matched$cty_code[which.max(abs(diffs))]))
cat(sprintf('    Within 1pp:      %d / %d (%.0f%%)\n',
            sum(abs(diffs) < 1), length(diffs),
            sum(abs(diffs) < 1) / length(diffs) * 100))
cat(sprintf('    Within 5pp:      %d / %d (%.0f%%)\n',
            sum(abs(diffs) < 5), length(diffs),
            sum(abs(diffs) < 5) / length(diffs) * 100))

# Import-weighted summary
w_diffs <- matched %>%
  summarise(
    wmean_diff = sum(diff_total * cbo_imports) / sum(cbo_imports),
    wmean_abs_diff = sum(abs(diff_total) * cbo_imports) / sum(cbo_imports)
  )
cat(sprintf('\n  Import-weighted:\n'))
cat(sprintf('    Weighted mean diff:     %+6.2f pp\n', w_diffs$wmean_diff))
cat(sprintf('    Weighted mean abs diff: %6.2f pp\n', w_diffs$wmean_abs_diff))

# ---- Largest differences ----------------------------------------------------
cat('\n---------------------------------------------------------------------\n')
cat('  Largest Differences (top 20 by absolute diff, weighted by imports)\n')
cat('---------------------------------------------------------------------\n\n')

outliers <- matched %>%
  arrange(desc(abs(diff_total) * cbo_imports)) %>%
  slice_head(n = 20)

cat(sprintf('%-22s %3s %7s | %7s %7s %7s | %7s %7s\n',
            '', '', 'Imp($M)',
            'Us(%)', 'CBO(%)', 'Diff',
            'CBO MFN', 'CBO New'))
cat(strrep('-', 85), '\n')

for (i in seq_len(nrow(outliers))) {
  r <- outliers[i, ]
  label <- if (!is.na(r$country_name)) str_trunc(r$country_name, 22) else r$cty_code
  iso <- if (!is.na(r$iso3)) r$iso3 else '   '
  cat(sprintf('%-22s %3s %7.0f | %7.2f %7.2f %+7.2f | %7.2f %7.2f\n',
              label, iso,
              r$cbo_imports / 1e6,
              r$our_level, r$cbo_total_etr, r$diff_total,
              r$cbo_mfn, r$cbo_new_rate))
}

# ---- Component breakdown for key countries ----------------------------------
cat('\n---------------------------------------------------------------------\n')
cat('  Component Breakdown for Key Countries\n')
cat('---------------------------------------------------------------------\n\n')

key_codes <- c('5700', '1220', '2010', '4120', '5880')  # CHN, CAN, MEX, GBR, JPN
key <- matched %>% filter(cty_code %in% key_codes)

if (nrow(key) > 0) {
  cat(sprintf('%-22s %3s | %7s %7s %7s | %7s %7s %7s\n',
              '', '',
              'Us Tot', 'CBO Tot', 'Diff',
              'CBO MFN', 'CBO New', 'CBO Tot'))
  cat(strrep('-', 85), '\n')

  for (i in seq_len(nrow(key))) {
    r <- key[i, ]
    label <- if (!is.na(r$country_name)) str_trunc(r$country_name, 22) else r$cty_code
    iso <- if (!is.na(r$iso3)) r$iso3 else '   '
    cat(sprintf('%-22s %3s | %7.2f %7.2f %+7.2f | %7.2f %7.2f %7.2f\n',
                label, iso,
                r$our_level, r$cbo_total_etr, r$diff_total,
                r$cbo_mfn, r$cbo_new_rate, r$cbo_total_etr))
  }
}

cat('\nDone.\n')
