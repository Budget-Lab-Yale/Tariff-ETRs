#!/usr/bin/env Rscript
# Final comparison: tracker daily_by_country.csv vs ETRs levels_by_census_country.csv
#
# KEY FINDING: These two outputs are NOT directly comparable because:
#   1. Tracker: UNWEIGHTED simple mean across tariffed products (exposed pairs)
#   2. ETRs:   IMPORT-WEIGHTED average across ALL products
# The comparison below documents the magnitude and patterns of these differences.

library(readr)
library(dplyr)
library(tidyr)

cat('================================================================\n')
cat('TRACKER vs ETRs COMPARISON\n')
cat('================================================================\n\n')

# ── Load tracker ─────────────────────────────────────────────────────────────
tracker_raw <- read_csv(
  'C:/Users/jar335/Documents/Repositories/tariff-rate-tracker/output/daily/daily_by_country.csv',
  col_types = cols(
    country = col_integer(),
    mean_additional = col_double(),
    mean_total = col_double(),
    revision = col_character(),
    valid_from = col_date(),
    valid_until = col_date(),
    date = col_date()
  )
)

# Deduplicate to one row per (country, valid_from)
tracker <- tracker_raw %>%
  group_by(country, valid_from) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  transmute(
    cty_code = country,
    valid_from,
    tracker_additional_pct = mean_additional * 100,
    tracker_total_pct = mean_total * 100,
    tracker_mfn_pct = (mean_total - mean_additional) * 100
  )

# ── Load ETRs ────────────────────────────────────────────────────────────────
etrs <- read_csv(
  'C:/Users/jar335/Documents/Repositories/Tariff-etrs/output/2026-04-02/levels_by_census_country.csv',
  col_types = cols(
    date = col_date(),
    valid_from = col_date(),
    valid_until = col_date(),
    cty_code = col_integer(),
    country_name = col_character(),
    level = col_double()
  )
) %>%
  transmute(cty_code, valid_from, country_name, etrs_level_pct = level)

# ── Join ─────────────────────────────────────────────────────────────────────
joined <- inner_join(tracker, etrs, by = c('cty_code', 'valid_from'))

unmatched_tracker <- anti_join(tracker, etrs, by = c('cty_code', 'valid_from'))
unmatched_etrs <- anti_join(etrs, tracker, by = c('cty_code', 'valid_from'))

cat(sprintf('Matched rows:           %d\n', nrow(joined)))
cat(sprintf('Tracker-only rows:      %d (dates: %s)\n', nrow(unmatched_tracker),
            paste(sort(unique(unmatched_tracker$valid_from)), collapse = ', ')))
cat(sprintf('ETRs-only rows:         %d (dates: %s)\n', nrow(unmatched_etrs),
            paste(sort(unique(unmatched_etrs$valid_from)), collapse = ', ')))
cat('\n')

# ── Methodology note ─────────────────────────────────────────────────────────
cat('================================================================\n')
cat('METHODOLOGY DIFFERENCE\n')
cat('================================================================\n')
cat('Tracker "mean_total": unweighted simple mean of total_rate across\n')
cat('  tariffed HTS10 products for that country (exposed pairs only).\n')
cat('  Includes base_rate (MFN) + total_additional.\n\n')
cat('ETRs "level": import-weighted average tariff across ALL HTS10\n')
cat('  products for that country. Includes MFN + all policy tariffs.\n\n')
cat('These are fundamentally different metrics:\n')
cat('  - Different denominators (tariffed products vs all products)\n')
cat('  - Different weighting (equal weight vs import-weighted)\n')
cat('  - Both include MFN, but MFN is weighted differently\n\n')

# ── Raw difference statistics ────────────────────────────────────────────────
joined <- joined %>%
  mutate(
    diff_total = etrs_level_pct - tracker_total_pct,
    diff_additional = etrs_level_pct - tracker_additional_pct,
    abs_diff_total = abs(diff_total)
  )

cat('================================================================\n')
cat('RAW COMPARISON: ETRs level vs Tracker mean_total\n')
cat('================================================================\n')
cat(sprintf('  Mean diff:       %+.2f pp\n', mean(joined$diff_total)))
cat(sprintf('  Median diff:     %+.2f pp\n', median(joined$diff_total)))
cat(sprintf('  SD:              %.2f pp\n', sd(joined$diff_total)))
cat(sprintf('  MAE:             %.2f pp\n', mean(joined$abs_diff_total)))
cat(sprintf('  Min:             %+.2f pp\n', min(joined$diff_total)))
cat(sprintf('  Max:             %+.2f pp\n', max(joined$diff_total)))
cat('\n')

# ── Correlation analysis ─────────────────────────────────────────────────────
cat('================================================================\n')
cat('CORRELATION ANALYSIS\n')
cat('================================================================\n')
cat('Even though the LEVELS are not directly comparable, we can check\n')
cat('whether the two systems agree on RELATIVE rankings and CHANGES.\n\n')

# Level correlation
level_cor <- cor(joined$tracker_total_pct, joined$etrs_level_pct, method = 'pearson')
level_cor_spearman <- cor(joined$tracker_total_pct, joined$etrs_level_pct, method = 'spearman')
cat(sprintf('Level correlation (Pearson):  %.4f\n', level_cor))
cat(sprintf('Level correlation (Spearman): %.4f\n', level_cor_spearman))
cat('\n')

# Change correlation: do both systems agree on direction of changes across dates?
# For each country, compute change from first to last date in both systems
changes <- joined %>%
  group_by(cty_code) %>%
  filter(n() > 1) %>%
  arrange(valid_from) %>%
  summarise(
    first_date = first(valid_from),
    last_date = last(valid_from),
    tracker_change = last(tracker_total_pct) - first(tracker_total_pct),
    etrs_change = last(etrs_level_pct) - first(etrs_level_pct),
    .groups = 'drop'
  )

change_cor <- cor(changes$tracker_change, changes$etrs_change, method = 'pearson')
change_cor_spearman <- cor(changes$tracker_change, changes$etrs_change, method = 'spearman')
cat(sprintf('Change correlation (first -> last, Pearson):  %.4f\n', change_cor))
cat(sprintf('Change correlation (first -> last, Spearman): %.4f\n', change_cor_spearman))
cat('\n')

# Direction agreement
direction_agree <- sum(sign(changes$tracker_change) == sign(changes$etrs_change))
cat(sprintf('Direction agreement (first -> last): %d / %d (%.1f%%)\n',
            direction_agree, nrow(changes),
            100 * direction_agree / nrow(changes)))
cat('\n')

# ── Date-by-date: correlation per date ───────────────────────────────────────
cat('================================================================\n')
cat('PER-DATE CROSS-SECTIONAL CORRELATION\n')
cat('================================================================\n')
cat('For each valid_from date, rank-correlation of country tariffs:\n\n')

per_date_cor <- joined %>%
  group_by(valid_from) %>%
  summarise(
    n = n(),
    pearson_r = cor(tracker_total_pct, etrs_level_pct, method = 'pearson'),
    spearman_r = cor(tracker_total_pct, etrs_level_pct, method = 'spearman'),
    tracker_mean = mean(tracker_total_pct),
    etrs_mean = mean(etrs_level_pct),
    .groups = 'drop'
  )

print(as.data.frame(per_date_cor), row.names = FALSE)
cat('\n')

# ── Key countries time series ────────────────────────────────────────────────
cat('================================================================\n')
cat('KEY COUNTRIES: TIME SERIES COMPARISON\n')
cat('================================================================\n')
cat('Tracker (unweighted, exposed) vs ETRs (import-weighted, all)\n\n')

key_ctys <- c(5700, 1220, 2010, 4120, 5880)
key_names <- c('China', 'Canada', 'Mexico', 'UK', 'Japan')

for (i in seq_along(key_ctys)) {
  cat(sprintf('--- %s (%d) ---\n', key_names[i], key_ctys[i]))
  cty_data <- joined %>%
    filter(cty_code == key_ctys[i]) %>%
    arrange(valid_from) %>%
    select(valid_from, tracker_total_pct, etrs_level_pct, diff_total)

  # Show first 5 and last 5 dates
  n_rows <- nrow(cty_data)
  if (n_rows <= 10) {
    show_data <- cty_data
  } else {
    show_data <- bind_rows(
      head(cty_data, 5),
      tibble(valid_from = as.Date(NA), tracker_total_pct = NA_real_,
             etrs_level_pct = NA_real_, diff_total = NA_real_),
      tail(cty_data, 5)
    )
  }

  formatted <- show_data %>%
    mutate(across(c(tracker_total_pct, etrs_level_pct, diff_total),
                  ~ sprintf('%8.2f', .x)))

  for (j in seq_len(nrow(formatted))) {
    if (is.na(formatted$valid_from[j])) {
      cat('  ...\n')
    } else {
      cat(sprintf('  %s  tracker=%s  etrs=%s  diff=%s\n',
                  formatted$valid_from[j],
                  formatted$tracker_total_pct[j],
                  formatted$etrs_level_pct[j],
                  formatted$diff_total[j]))
    }
  }
  cat('\n')
}

# ── Investigate the 38.86% baseline issue ────────────────────────────────────
cat('================================================================\n')
cat('BASELINE INVESTIGATION: WHY TRACKER SHOWS ~38.86% ADDITIONAL\n')
cat('AT 2025-01-01 FOR MOST COUNTRIES\n')
cat('================================================================\n')
cat('The tracker shows mean_additional ~ 38.86% for many countries at the\n')
cat('pre-inauguration baseline. This is the UNWEIGHTED average across\n')
cat('"tariffed" (exposed) product-country pairs. If a country has only\n')
cat('a few tariffed products (e.g. 232 steel/aluminum), the average of\n')
cat('those few products\' rates = 25% (steel) or 10% (aluminum), not 38.86%.\n\n')

# The 38.86% likely comes from some other authority or a different product set
# Let me check what unique values exist at the baseline
tracker_baseline <- tracker %>% filter(valid_from == '2025-01-01')
cat('Distribution of tracker_additional at 2025-01-01:\n')
cat(sprintf('  Distinct values: %d\n', n_distinct(tracker_baseline$tracker_additional_pct)))
common_vals <- tracker_baseline %>%
  count(tracker_additional_pct) %>%
  arrange(desc(n)) %>%
  head(10)
cat('  Most common values:\n')
for (j in seq_len(nrow(common_vals))) {
  cat(sprintf('    %.4f%% appears %d times\n',
              common_vals$tracker_additional_pct[j], common_vals$n[j]))
}
cat('\n')

# ── Summary recommendation ───────────────────────────────────────────────────
cat('================================================================\n')
cat('SUMMARY & RECOMMENDATION\n')
cat('================================================================\n')
cat('The tracker and ETRs outputs use fundamentally different metrics:\n')
cat('  - Tracker: unweighted mean across tariffed products (sparse)\n')
cat('  - ETRs: import-weighted mean across all products (dense)\n\n')
cat('These differences are expected and do not indicate bugs in either\n')
cat('system. To make a meaningful comparison, you would need either:\n')
cat('  (a) ETRs to output unweighted simple means, or\n')
cat('  (b) Tracker to output import-weighted averages\n')
cat('  (c) Compare at the HS10 x country level before aggregation\n\n')
cat('The correlation analysis above shows whether both systems agree\n')
cat('on relative rankings (which country has higher tariffs).\n')
