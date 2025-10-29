# =============================================================================
# main.R
# =============================================================================
#
# This script calculates effective tariff rates (ETRs) on U.S. imports by
# trading partner and GTAP sector under various tariff scenarios including:
#   - Section 232 tariffs (steel, aluminum, softwood, furniture, autos, etc.)
#   - IEEPA tariffs (residual catch-all for imports not covered by 232)
#   - USMCA exemptions with content requirements
#
# Workflow:
#   1. Load scenario-specific tariff parameters from config/{scenario}/
#   2. Read and process 2024 U.S. import data (HS6 level)
#   3. Calculate import shares subject to each tariff by partner and sector
#   4. Compute weighted ETRs incorporating all tariff laws and exemptions
#   5. Write GTAP shock commands to output/{scenario}/
#   6. Calculate and print overall ETRs by country and total
#
# To run a different scenario: Change the 'scenario' variable below and ensure
# corresponding config files exist in config/{scenario}/
#
# =============================================================================

library(tidyverse)
library(yaml)

# Load helper functions
source('src/functions.R')


#---------------------------
# Set tariff law parameters
#---------------------------

# Tariff law scenario name
scenario <- '10_29'

# Section 232 tariffs
params_232 <- read_yaml(sprintf('config/%s/232.yaml', scenario))

# IEEPA rates
params_ieepa <- read_csv(sprintf('config/%s/ieepa_rates.csv', scenario), show_col_types = FALSE)

# USMCA share of trade by sector
usmca_shares <- read_csv('./resources/usmca_shares.csv')

# Other params
us_auto_content_share  = 0.4
us_auto_assembly_share = 0.33
auto_rebate_rate       = 0.0375
ieepa_usmca_exception  = 1
kr_share_ftrow         = 0.57
vn_share_row           = 0.94

# Adjust IEEPA rates: fold Korea into ftrow and Vietnam into row
# Note: Korea and FTROW countries are directly classified via ftrow_codes in import data,
# but IEEPA config has separate kr column that needs to be merged into ftrow rates
params_ieepa <- params_ieepa %>%
  mutate(
    ftrow = ftrow * (1 - kr_share_ftrow) + kr * kr_share_ftrow,
    row   = row   * (1 - vn_share_row)   + vn * vn_share_row
  ) %>%
  select(-kr, -vn)

#----------------------
# Set other parameters
#----------------------

# Path to Census Bureau import data files
import_data_path <- 'C:/Users/jar335/Downloads'

# Load country-to-partner mapping
country_mapping <- read_csv('resources/country_partner_mapping.csv', show_col_types = FALSE)


#------------------
# Read import data
#------------------

# Read GTAP crosswalk
crosswalk <- read_csv('resources/hs6_gtap_crosswalk.csv', show_col_types = FALSE)

# Get list of all 2024 import files
file_pattern <- 'dporths6ir24'
files_2024   <- list.files(path = import_data_path, pattern = file_pattern, full.names = TRUE)

# Define column positions based on the file specification
col_positions <- fwf_positions(
  start     = c(1, 7, 11, 15, 19, 21),
  end       = c(6, 10, 14, 18, 20, 35),
  col_names = c('hs6_code', 'cty_code', 'port_code', 'year', 'month', 'value_mo')
)

# Build data
hs6_by_country <- files_2024 %>%

  # Read and combine all files
  map_df(~ read_fwf(
    file = .x,
    col_positions = col_positions,
    col_types = cols(
      hs6_code  = col_character(),
      cty_code  = col_character(),
      port_code = col_character(),
      year      = col_integer(),
      month     = col_integer(),
      value_mo  = col_double()
    )
  )) %>%

  # Tag each row with a partner group using mapping
  left_join(
    country_mapping %>% select(cty_code, partner),
    by = 'cty_code'
  ) %>%
  mutate(partner = if_else(is.na(partner), 'row', partner)) %>%

  # Get totals
  group_by(hs6_code, partner) %>%
  summarise(imports = sum(value_mo), .groups = 'drop') %>%

  # Expand to include all HS6 x partner combinations (fill missing with 0)
  complete(
    hs6_code,
    partner = c('china', 'canada', 'mexico', 'uk', 'japan', 'eu', 'row', 'ftrow'),
    fill = list(imports = 0)
  ) %>%

  # Add GTAP code
  left_join(
    crosswalk %>%
      select(hs6_code, gtap_code),
    by = 'hs6_code'
  ) %>%
  mutate(gtap_code = str_to_lower(gtap_code)) %>%
  relocate(gtap_code, .after = hs6_code)


#------------------------------
# Calculate tax bases and ETRs
#------------------------------

# Calculate tax bases for 232 -- and IEEPA as residual
bases <- params_232 %>%
  names() %>%

  # Get bases for each 232 tariff
  map(~ {

    definite_codes <- params_232[[.x]]$base$definite
    all_codes      <- c(params_232[[.x]]$base$definite, params_232[[.x]]$base$maybe)

    share       <- calc_import_shares(definite_codes)
    share_upper <- calc_import_shares(all_codes) %>% rename(share_upper = share) 

    share %>%
      left_join(share_upper, by = c('partner', 'gtap_code')) %>%
      mutate(tariff = .x, .before = 1) %>%
      return()
  }) %>%
  bind_rows() %>%

  # Add residual -- tax base uncovered by 232
  bind_rows(
    (.) %>%
      group_by(partner, gtap_code) %>%
      summarise(
        tariff      = 'residual',
        share       = 1 - sum(share),
        share_upper = 1 - sum(share_upper),
        .groups = 'drop'
      )
  )

# Calculate ETRs
etrs <- calc_weighted_etr(
  bases_data            = bases, 
  params_data           = params_232,
  ieepa_data            = params_ieepa,
  usmca_data            = usmca_shares,
  us_auto_content_share = us_auto_content_share,
  auto_rebate           = auto_rebate_rate,
  us_assembly_share     = us_auto_assembly_share,
  ieepa_usmca_exempt    = ieepa_usmca_exception
)

# Write shock commands to file
write_shock_commands(
  etr_data    = etrs,
  output_file = 'shocks.txt',
  scenario    = scenario
)

# Write sector x country ETR CSVs
write_sector_country_etrs(
  etr_data            = etrs,
  output_file_regular = 'etrs_by_sector_country.csv',
  output_file_upper   = 'etrs_by_sector_country_upper.csv',
  scenario            = scenario
)

# Calculate and print overall ETRs with both GTAP and 2024 Census weights
calc_overall_etrs(
  etr_data    = etrs,
  import_data = hs6_by_country,
  scenario    = scenario
)


