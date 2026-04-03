# =============================================================================
# build_io_matrix.R
# =============================================================================
#
# Standalone script to extract full BEA Leontief requirement matrices from
# BEA Industry-by-Commodity summary CSVs. Outputs clean CSV files with BEA
# codes as row and column names.
#
# Input:
#   resources/bea/BEA - Domestic Requirements, Industry-by-Commodities - Summary - 2024.csv
#   resources/bea/BEA - Total Requirements, Industry-by-Commodities - Summary - 2024.csv
#
# Output:
#   resources/io/bea_requirements_domestic.csv
#   resources/io/bea_requirements_total.csv
#
# =============================================================================

library(tidyverse)


#' Read a full BEA requirements matrix
#'
#' Generalizes the row-331-only reader from build_metal_content_shares.R to
#' extract all rows. Returns a tidy matrix with BEA codes as row/column names.
#'
#' @param path Path to BEA requirements CSV
#' @return Tibble with bea_code column + numeric columns for each industry
read_bea_requirements_full <- function(path) {

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

  # Get commodity columns (everything except row_code and row_name)
  commodity_cols <- col_codes[3:length(col_codes)]

  # Filter out non-industry rows (totals, blank rows, etc.)
  # Keep only rows where row_code matches one of the commodity columns
  valid_codes <- commodity_cols
  req_clean <- req_raw %>%
    filter(row_code %in% valid_codes)

  message(sprintf('  Retained %d industry rows', nrow(req_clean)))

  # Convert all commodity columns to numeric (treating '---' as 0)
  result <- req_clean %>%
    select(bea_code = row_code, all_of(commodity_cols)) %>%
    mutate(across(-bea_code, ~ {
      x <- as.character(.)
      x <- if_else(x == '---' | is.na(x), '0', x)
      as.numeric(x)
    }))

  return(result)
}


# =============================================================================
# Extract both matrices
# =============================================================================

domestic <- read_bea_requirements_full(
  'resources/bea/BEA - Domestic Requirements, Industry-by-Commodities - Summary - 2024.csv'
)

total <- read_bea_requirements_full(
  'resources/bea/BEA - Total Requirements, Industry-by-Commodities - Summary - 2024.csv'
)

# =============================================================================
# Write outputs
# =============================================================================

dir.create('resources/io', showWarnings = FALSE, recursive = TRUE)

write_csv(domestic, 'resources/io/bea_requirements_domestic.csv')
message(sprintf('Wrote %d x %d matrix to resources/io/bea_requirements_domestic.csv',
                nrow(domestic), ncol(domestic) - 1))

write_csv(total, 'resources/io/bea_requirements_total.csv')
message(sprintf('Wrote %d x %d matrix to resources/io/bea_requirements_total.csv',
                nrow(total), ncol(total) - 1))

# Quick validation: diagonal should be >= 1.0 (Leontief inverse property)
diag_check <- sapply(domestic$bea_code, function(code) {
  if (code %in% names(domestic)) domestic[[code]][domestic$bea_code == code] else NA
})
below_one <- sum(diag_check < 1.0, na.rm = TRUE)
if (below_one > 0) {
  warning(sprintf('%d diagonal elements < 1.0 (unexpected for Leontief inverse)', below_one))
} else {
  message('Validation: all diagonal elements >= 1.0 (as expected)')
}
