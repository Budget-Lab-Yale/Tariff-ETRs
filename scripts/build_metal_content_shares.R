# =============================================================================
# build_metal_content_shares.R
# =============================================================================
#
# Standalone script to compute metal content shares from BEA Input-Output data.
# Run once (or whenever BEA data updates) to produce two output files:
#   - resources/metal_content_shares_domestic.csv  (domestic requirements)
#   - resources/metal_content_shares_total.csv     (total requirements)
#
# Both tables are Industry-by-Commodity: row 331 gives the Primary Metals
# requirement per $1 of each commodity's output.
#
# Domestic requirements: traces the full supply chain through domestic
#   production only (Leontief inverse excluding imports). Captures indirect
#   metal content while excluding imported intermediates.
#
# Total requirements: traces the full global supply chain (Leontief inverse
#   including imports). Captures all metal content however indirectly embedded,
#   but includes imported metal not subject to the 232 tariff.
#
# Input:
#   resources/bea/BEA - Domestic Requirements, Industry-by-Commodities - Summary - 2024.csv
#   resources/bea/BEA - Total Requirements, Industry-by-Commodities - Summary - 2024.csv
#   resources/gtap_bea_crosswalk.csv
#
# Output:
#   resources/metal_content_shares_domestic.csv
#   resources/metal_content_shares_total.csv
#
# =============================================================================

library(tidyverse)


# =============================================================================
# Helper: read a BEA requirements CSV and extract row 331
# =============================================================================

read_bea_requirements <- function(path) {

  message('Reading: ', path)

  # Row 4 has BEA industry codes as column headers; row 5 has descriptions
  code_row <- read_csv(path, skip = 3, n_max = 1, col_names = FALSE,
                       show_col_types = FALSE, name_repair = 'minimal')
  col_codes <- as.character(code_row[1, ])
  col_codes[1:2] <- c('row_code', 'row_name')

  # Read data starting from row 6, using BEA codes as column names
  req_raw <- read_csv(path, skip = 5, col_names = col_codes,
                      show_col_types = FALSE, name_repair = 'minimal')
  req_raw <- req_raw %>%
    mutate(row_code = str_trim(row_code))

  message(sprintf('  Read %d rows x %d cols', nrow(req_raw), ncol(req_raw)))

  # Extract row 331 (Primary Metals)
  commodity_cols <- col_codes[3:length(col_codes)]

  row_331 <- req_raw %>% filter(row_code == '331')
  if (nrow(row_331) != 1) {
    stop('Expected exactly 1 row with code 331, found ', nrow(row_331))
  }

  # Pivot to long format and cap at 1.0
  row_331 %>%
    select(all_of(commodity_cols)) %>%
    mutate(across(everything(), ~ {
      x <- as.character(.)
      x <- if_else(x == '---' | is.na(x), '0', x)
      as.numeric(x)
    })) %>%
    pivot_longer(everything(), names_to = 'bea_code', values_to = 'metal_share') %>%
    mutate(metal_share = pmin(metal_share, 1.0))
}


# =============================================================================
# 1. Extract row 331 from both tables
# =============================================================================

domestic_331 <- read_bea_requirements(
  'resources/bea/BEA - Domestic Requirements, Industry-by-Commodities - Summary - 2024.csv'
)
total_331 <- read_bea_requirements(
  'resources/bea/BEA - Total Requirements, Industry-by-Commodities - Summary - 2024.csv'
)


# =============================================================================
# 2. Join with GTAP-BEA crosswalk and write outputs
# =============================================================================

crosswalk <- read_csv('resources/gtap_bea_crosswalk.csv', show_col_types = FALSE)
message(sprintf('Loaded GTAP-BEA crosswalk: %d mappings', nrow(crosswalk)))

key_sectors <- c('I_S', 'NFM', 'FMP', 'OME', 'ELE', 'EEQ', 'MVH', 'OTN', 'LUM', 'OMF')

build_and_write <- function(req_331, output_file, label) {

  shares <- crosswalk %>%
    left_join(req_331 %>% select(bea_code, metal_share), by = 'bea_code')

  # Check for unmapped BEA codes
  unmapped <- shares %>% filter(is.na(metal_share))
  if (nrow(unmapped) > 0) {
    warning(sprintf('%d GTAP codes have no BEA match: %s',
                    nrow(unmapped), paste(unmapped$gtap_code, collapse = ', ')))
  }

  write_csv(shares, output_file)
  message(sprintf('\nWrote %d rows to %s', nrow(shares), output_file))

  message(sprintf('--- %s: key manufacturing sectors ---', label))
  shares %>%
    filter(gtap_code %in% key_sectors) %>%
    arrange(desc(metal_share)) %>%
    mutate(metal_share = sprintf('%.4f', metal_share)) %>%
    print(n = Inf)
}

build_and_write(domestic_331, 'resources/metal_content_shares_domestic.csv', 'Domestic requirements')
build_and_write(total_331, 'resources/metal_content_shares_total.csv', 'Total requirements')
