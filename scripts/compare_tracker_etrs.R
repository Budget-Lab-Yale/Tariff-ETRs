# Compare tracker vs ETRs weighted ETR for 2026-02-24
# Runs both calculations on the same imports to isolate rate differences
library(tidyverse)
library(here)

tracker_dir <- 'C:/Users/jar335/Documents/Repositories/tariff-rate-tracker'
etrs_dir    <- 'C:/Users/jar335/Documents/Repositories/Tariff-ETRs'

# ============================================================================
# 1. Load shared imports (same data for both)
# ============================================================================
cache_file <- file.path(etrs_dir, 'cache', 'hs10_by_country_gtap_2024_con.rds')
imports <- readRDS(cache_file) %>%
  filter(!str_detect(hs10, '^(98|99)'), !is.na(gtap_code))
message('Imports: ', nrow(imports), ' rows, $',
        round(sum(imports$imports) / 1e9, 1), 'B total')

imports_agg <- imports %>%
  group_by(hs10, cty_code) %>%
  summarise(imports = sum(imports), .groups = 'drop') %>%
  filter(imports > 0)
total_imports <- sum(imports_agg$imports)
message('Aggregated to ', nrow(imports_agg), ' hs10 x country pairs')

# ============================================================================
# 2. Tracker: load timeseries, get snapshot for 2026-02-24, apply stacking
# ============================================================================
message('\n=== TRACKER SIDE ===')
# Source tracker helpers — pass explicit path to avoid here() issues
source(file.path(tracker_dir, 'src', 'helpers.R'))
ts <- readRDS(file.path(tracker_dir, 'data', 'timeseries', 'rate_timeseries.rds'))
message('Timeseries: ', nrow(ts), ' rows')

pp <- load_policy_params(yaml_path = file.path(tracker_dir, 'config', 'policy_params.yaml'))
snapshot <- get_rates_at_date(ts, '2026-02-24', policy_params = pp)
message('Snapshot for 2026-02-24: ', nrow(snapshot), ' rows')

# Apply stacking rules (same as daily series)
snapshot_stacked <- apply_stacking_rules(snapshot, cty_china = '5700')

# Join with imports
tracker_rated <- snapshot_stacked %>%
  select(hts10, country, total_rate, total_additional, base_rate,
         rate_232, rate_ieepa_recip, rate_ieepa_fent, rate_301,
         rate_s122, rate_section_201, rate_other) %>%
  inner_join(
    imports_agg %>% rename(hts10 = hs10, country = cty_code),
    by = c('hts10', 'country')
  )

tracker_etr <- sum(tracker_rated$total_rate * tracker_rated$imports) / total_imports
tracker_matched <- sum(tracker_rated$imports) / 1e9
message(sprintf('Tracker weighted ETR: %.4f%% (matched $%.1fB of $%.1fB)',
                tracker_etr * 100, tracker_matched, total_imports / 1e9))

# Authority decomposition
tracker_net <- compute_net_authority_contributions(snapshot_stacked, cty_china = '5700')
tracker_net_rated <- tracker_net %>%
  inner_join(imports_agg %>% rename(hts10 = hs10, country = cty_code), by = c('hts10', 'country'))
message(sprintf('  MFN:     %.4f%%', sum(tracker_net_rated$base_rate * tracker_net_rated$imports) / total_imports * 100))
message(sprintf('  S232:    %.4f%%', sum(tracker_net_rated$net_232 * tracker_net_rated$imports) / total_imports * 100))
message(sprintf('  IEEPA-R: %.4f%%', sum(tracker_net_rated$net_ieepa * tracker_net_rated$imports) / total_imports * 100))
message(sprintf('  IEEPA-F: %.4f%%', sum(tracker_net_rated$net_fentanyl * tracker_net_rated$imports) / total_imports * 100))
message(sprintf('  S301:    %.4f%%', sum(tracker_net_rated$net_301 * tracker_net_rated$imports) / total_imports * 100))
message(sprintf('  S122:    %.4f%%', sum(tracker_net_rated$net_s122 * tracker_net_rated$imports) / total_imports * 100))
message(sprintf('  S201:    %.4f%%', sum(tracker_net_rated$net_section_201 * tracker_net_rated$imports) / total_imports * 100))

# Free timeseries memory
rm(ts, snapshot, snapshot_stacked, tracker_net); gc()

# ============================================================================
# 3. ETRs: load config and run calculation
# ============================================================================
message('\n=== ETRS SIDE ===')
script_dir <- file.path(etrs_dir, 'src')
source(file.path(script_dir, 'config_parsing.R'))
source(file.path(script_dir, 'data_processing.R'))
source(file.path(script_dir, 'outputs.R'))
source(file.path(script_dir, 'config_overlay.R'))
source(file.path(script_dir, 'etr_engine.R'))
source(file.path(script_dir, 'scenario_workflow.R'))

country_mapping <- read_csv(
  file.path(etrs_dir, 'resources', 'country_partner_mapping.csv'),
  col_types = cols(.default = col_character())
)

config_dir <- file.path(etrs_dir, 'config', 'historical', '2026-02-24')
config <- load_scenario_config(config_dir)
etrs_full <- calc_etrs_for_config(config, imports, country_mapping)
etrs_result <- etrs_full$hs10_country_etrs

etrs_etr <- sum(etrs_result$etr * etrs_result$imports) / total_imports
etrs_matched <- sum(etrs_result$imports) / 1e9
message(sprintf('ETRs weighted ETR: %.4f%% (matched $%.1fB of $%.1fB)',
                etrs_etr * 100, etrs_matched, total_imports / 1e9))

# Authority decomposition
message(sprintf('  MFN:     %.4f%%', sum(etrs_result$contrib_mfn * etrs_result$imports) / total_imports * 100))
message(sprintf('  S232:    %.4f%%', sum(etrs_result$contrib_s232 * etrs_result$imports) / total_imports * 100))
message(sprintf('  IEEPA-R: %.4f%%', sum(etrs_result$contrib_ieepa_reciprocal * etrs_result$imports) / total_imports * 100))
message(sprintf('  IEEPA-F: %.4f%%', sum(etrs_result$contrib_ieepa_fentanyl * etrs_result$imports) / total_imports * 100))
message(sprintf('  S301:    %.4f%%', sum(etrs_result$contrib_s301 * etrs_result$imports) / total_imports * 100))
message(sprintf('  S122:    %.4f%%', sum(etrs_result$contrib_s122 * etrs_result$imports) / total_imports * 100))
message(sprintf('  S201:    %.4f%%', sum(etrs_result$contrib_s201 * etrs_result$imports) / total_imports * 100))

# ============================================================================
# 4. Compare at HS10 x country level
# ============================================================================
message('\n=== HS10 x COUNTRY COMPARISON ===')
combined <- tracker_rated %>%
  select(hts10, country, tracker_rate = total_rate, imports) %>%
  inner_join(
    etrs_result %>% select(hs10, cty_code, etrs_rate = etr) %>%
      rename(hts10 = hs10, country = cty_code),
    by = c('hts10', 'country')
  ) %>%
  mutate(diff = etrs_rate - tracker_rate)

message(sprintf('Matched rows: %d', nrow(combined)))
message(sprintf('Exact match: %d (%.1f%%)', sum(combined$diff == 0),
                100 * sum(combined$diff == 0) / nrow(combined)))
message(sprintf('Nonzero diffs: %d', sum(abs(combined$diff) > 1e-10)))

diffs <- combined %>% filter(abs(diff) > 1e-10)
if (nrow(diffs) > 0) {
  message(sprintf('Import-weighted diff: %.6f pp',
                  sum(diffs$diff * diffs$imports) / total_imports * 100))

  message('\nTop 20 by import-weighted impact:')
  diffs %>%
    mutate(weighted_impact = diff * imports) %>%
    arrange(desc(abs(weighted_impact))) %>%
    head(20) %>%
    mutate(across(c(tracker_rate, etrs_rate, diff), ~ round(. * 100, 4)),
           imports_m = round(imports / 1e6, 1),
           impact_pp = round(weighted_impact / total_imports * 100, 6)) %>%
    select(hts10, country, tracker_rate, etrs_rate, diff, imports_m, impact_pp) %>%
    print(n = 20)
}

message('\n=== SUMMARY ===')
message(sprintf('Tracker ETR: %.4f%%', tracker_etr * 100))
message(sprintf('ETRs ETR:    %.4f%%', etrs_etr * 100))
message(sprintf('Gap:         %.4f pp (ETRs - Tracker)', (etrs_etr - tracker_etr) * 100))
