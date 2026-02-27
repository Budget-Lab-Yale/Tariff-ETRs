# Extract Budget Lab HTS10 x country rates for cross-validation with CBO
#
# This script sources the Budget Lab config parsing and calculation functions,
# loads the 11-20 scenario (closest to CBO's Nov 15 snapshot), and outputs
# a CSV of (hs10, cty_code, final_rate) for comparison.

library(tidyverse)
library(yaml)

# Source core functions
source('src/config_parsing.R')
source('src/calculations.R')
source('src/data_processing.R')

scenario <- '11-20'
config_dir <- 'config'

message('Loading scenario parameters for: ', scenario)

# Load 232 tariff rates
params_s232 <- load_s232_rates(file.path(config_dir, scenario, 's232.yaml'))

# Load IEEPA reciprocal rates
rates_ieepa_reciprocal <- load_ieepa_rates_yaml(
  file.path(config_dir, scenario, 'ieepa_reciprocal.yaml'),
  rate_col_name = 'ieepa_reciprocal_rate'
)

# Load IEEPA fentanyl rates
rates_ieepa_fentanyl <- load_ieepa_rates_yaml(
  file.path(config_dir, scenario, 'ieepa_fentanyl.yaml'),
  rate_col_name = 'ieepa_fentanyl_rate'
)

# Load Section 122 rates (if exists)
s122_path <- file.path(config_dir, scenario, 's122.yaml')
if (file.exists(s122_path)) {
  rates_s122 <- load_ieepa_rates_yaml(s122_path, rate_col_name = 's122_rate')
} else {
  rates_s122 <- NULL
}

# Other params
other_params <- read_yaml(file.path(config_dir, scenario, 'other_params.yaml'))

# USMCA shares
usmca_shares <- read_csv('./resources/usmca_shares.csv', show_col_types = FALSE)

# Load import data from cache
cache_file <- 'cache/hs10_by_country_gtap_2024_con.rds'
message('Loading cached import data from: ', cache_file)
hs10_by_country <- readRDS(cache_file) %>%
  filter(!str_detect(hs10, '^(98|99)')) %>%
  filter(!is.na(gtap_code))
message(sprintf('Loaded %s records', format(nrow(hs10_by_country), big.mark = ',')))

# Calculate ETRs at HS10 x country level
message('Calculating ETRs...')
hs10_country_etrs <- calc_weighted_etr(
  rates_s232              = params_s232$rate_matrix,
  usmca_exempt_s232       = params_s232$usmca_exempt,
  rates_ieepa_reciprocal = rates_ieepa_reciprocal,
  rates_ieepa_fentanyl   = rates_ieepa_fentanyl,
  rates_s122             = rates_s122,
  import_data            = hs10_by_country,
  usmca_data             = usmca_shares,
  us_auto_content_share  = other_params$us_auto_content_share,
  auto_rebate            = other_params$auto_rebate_rate,
  us_assembly_share      = other_params$us_auto_assembly_share,
  ieepa_usmca_exempt     = other_params$ieepa_usmca_exception
)

# Convert to format comparable with CBO: (I_COMMODITY, CTY_CODE, new_rate)
# CBO rates are in percentage points (e.g., 25 for 25%), BL rates are in decimal (0.25)
# Convert BL to percentage for comparison
bl_spec <- hs10_country_etrs %>%
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
