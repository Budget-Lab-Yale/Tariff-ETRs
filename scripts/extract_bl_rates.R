# Extract Budget Lab HTS10 x country rates for cross-validation with CBO
#
# This script loads a historical config via load_scenario_config() and outputs
# a CSV of (hs10, cty_code, final_rate) for comparison.
#
# Usage:
#   Rscript scripts/extract_bl_rates.R [historical_date]
#
# Default historical_date: 2025-11-15 (closest to CBO's Nov 15 snapshot)

library(tidyverse)
library(yaml)

# Source core functions
source('src/config_parsing.R')
source('src/calculations.R')
source('src/data_processing.R')

args <- commandArgs(trailingOnly = TRUE)
historical_date <- if (length(args) >= 1) args[[1]] else '2025-11-15'
config_dir <- file.path('config/historical', historical_date)

if (!dir.exists(config_dir)) {
  stop(sprintf('Historical config not found: %s', config_dir))
}

message('Loading scenario config from: ', config_dir)
config <- load_scenario_config(config_dir)

# Load import data from cache
cache_file <- 'cache/hs10_by_country_gtap_2024_con.rds'
message('Loading cached import data from: ', cache_file)
hs10_by_country <- readRDS(cache_file) %>%
  filter(!str_detect(hs10, '^(98|99)')) %>%
  filter(!is.na(gtap_code))
message(sprintf('Loaded %s records', format(nrow(hs10_by_country), big.mark = ',')))

# Load country mapping
country_mapping <- read_csv(
  'resources/country_partner_mapping.csv',
  col_types = cols(cty_code = col_character()),
  show_col_types = FALSE
)

# Calculate ETRs using the production path
message('Calculating ETRs...')
results <- calc_etrs_for_config(config, hs10_by_country, country_mapping)

# Convert to format comparable with CBO: (I_COMMODITY, CTY_CODE, new_rate)
# CBO rates are in percentage points (e.g., 25 for 25%), BL rates are in decimal (0.25)
bl_spec <- results$hs10_country_etrs %>%
  select(hs10, cty_code, etr, imports) %>%
  rename(I_COMMODITY = hs10, CTY_CODE = cty_code, new_rate = etr) %>%
  mutate(new_rate = new_rate * 100)  # Convert to percentage

output_path <- 'scripts/bl_tariff_spec.csv'
write_csv(bl_spec, output_path)

message(sprintf('Saved Budget Lab tariff spec to: %s', output_path))
message(sprintf('Shape: %d rows', nrow(bl_spec)))
message(sprintf('Non-zero rates: %d', sum(bl_spec$new_rate > 0)))
message(sprintf('Unique countries: %d', n_distinct(bl_spec$CTY_CODE)))
message(sprintf('Unique HTS codes: %d', n_distinct(bl_spec$I_COMMODITY)))

# Summary stats
message('\n--- Summary by country (top 20 by avg rate) ---')
summary_stats <- bl_spec %>%
  group_by(CTY_CODE) %>%
  summarise(
    avg_rate = weighted.mean(new_rate, imports),
    max_rate = max(new_rate),
    n_products = n_distinct(I_COMMODITY),
    n_nonzero = sum(new_rate > 0)
  ) %>%
  arrange(desc(avg_rate))

print(summary_stats %>% head(20))
