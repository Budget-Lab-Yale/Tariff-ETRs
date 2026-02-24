# =============================================================================
# build_metal_content_shares.R
# =============================================================================
#
# Standalone script to compute metal content shares from BEA Input-Output data.
# Run once (or whenever BEA data updates) to produce output files at two
# levels of granularity:
#
# GTAP-level (original):
#   - resources/metal_content_shares_domestic.csv
#   - resources/metal_content_shares_total.csv
#
# NAICS/HS10-level (new):
#   - resources/metal_content_shares_naics_domestic.csv
#   - resources/metal_content_shares_naics_total.csv
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
#   resources/hs10_naics_crosswalk.csv          (for NAICS-level output)
#   resources/naics_bea_summary_crosswalk.csv   (for NAICS-level output)
#
# Output:
#   resources/metal_content_shares_domestic.csv
#   resources/metal_content_shares_total.csv
#   resources/metal_content_shares_naics_domestic.csv
#   resources/metal_content_shares_naics_total.csv
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


# =============================================================================
# 3. Build NAICS-level (HS10-granularity) shares
# =============================================================================
#
# Chain: HS10 -> NAICS (6-digit) -> BEA summary code -> row-331 metal share
# Uses longest-prefix matching for NAICS -> BEA (BEA codes are 2-6 digits).
# HS10 codes that can't be matched to a BEA code get metal_share = 1.0
# (conservative default, same as GTAP fallback).
# =============================================================================

message('\n--- Building NAICS-level (HS10-granularity) shares ---')

hs10_naics <- read_csv('resources/hs10_naics_crosswalk.csv',
                        show_col_types = FALSE,
                        col_types = cols(hs10 = col_character(), naics = col_character()))
message(sprintf('Loaded HS10-NAICS crosswalk: %d mappings', nrow(hs10_naics)))

naics_bea <- read_csv('resources/naics_bea_summary_crosswalk.csv',
                       show_col_types = FALSE,
                       col_types = cols(naics = col_character(), bea_summary_code = col_character()))
message(sprintf('Loaded NAICS-BEA crosswalk: %d mappings', nrow(naics_bea)))


#' Match 6-digit NAICS codes to BEA summary codes via longest prefix
#'
#' For each NAICS code, tries exact match first, then progressively shorter
#' prefixes (5, 4, 3, 2 digits). Non-digit trailing characters (e.g., 'X' in
#' CBO codes like '11211X') are stripped before matching.
#'
#' @param naics_codes Character vector of NAICS codes to match
#' @param naics_bea_lookup Tibble with columns: naics, bea_summary_code
#'
#' @return Character vector of BEA summary codes (NA where no match found)
match_naics_to_bea <- function(naics_codes, naics_bea_lookup) {

  # Build a named lookup vector for fast matching
  bea_lookup <- setNames(naics_bea_lookup$bea_summary_code, naics_bea_lookup$naics)

  sapply(naics_codes, function(code) {
    # Strip trailing non-digit characters (e.g., 'X' suffixes from CBO)
    clean <- str_remove(code, '[^0-9]+$')

    # Try progressively shorter prefixes
    for (len in nchar(clean):2) {
      prefix <- substr(clean, 1, len)
      if (!is.na(bea_lookup[prefix])) {
        return(bea_lookup[prefix])
      }
    }
    return(NA_character_)
  }, USE.NAMES = FALSE)
}

# Match each HS10's NAICS to a BEA summary code
hs10_bea <- hs10_naics %>%
  mutate(bea_code = match_naics_to_bea(naics, naics_bea))

matched_count <- sum(!is.na(hs10_bea$bea_code))
message(sprintf('NAICS->BEA matching: %d of %d matched (%.1f%%)',
                matched_count, nrow(hs10_bea),
                100 * matched_count / nrow(hs10_bea)))

# Show unmatched NAICS codes
unmatched_naics <- hs10_bea %>%
  filter(is.na(bea_code)) %>%
  distinct(naics)
if (nrow(unmatched_naics) > 0) {
  message(sprintf('Unmatched NAICS codes (%d): %s',
                  nrow(unmatched_naics),
                  paste(unmatched_naics$naics, collapse = ', ')))
}


build_and_write_naics <- function(req_331, output_file, label) {

  # Join HS10->BEA with row-331 metal shares
  shares <- hs10_bea %>%
    left_join(req_331 %>% select(bea_code, metal_share), by = 'bea_code') %>%
    # Unmatched HS10 codes (no BEA match) default to 1.0 (conservative)
    mutate(metal_share = if_else(is.na(metal_share), 1.0, metal_share))

  write_csv(shares, output_file)
  message(sprintf('\nWrote %d rows to %s', nrow(shares), output_file))

  message(sprintf('--- %s (NAICS): share distribution ---', label))
  message(sprintf('  Mean: %.4f, Median: %.4f, Min: %.4f, Max: %.4f',
                  mean(shares$metal_share), median(shares$metal_share),
                  min(shares$metal_share), max(shares$metal_share)))

  # Show BEA code distribution for key metal-related codes
  message(sprintf('--- %s (NAICS): key BEA sectors ---', label))
  shares %>%
    filter(bea_code %in% c('331', '332', '333', '334', '335', '3361MV', '3364OT', '339')) %>%
    group_by(bea_code) %>%
    summarise(n_hs10 = n(), metal_share = first(metal_share), .groups = 'drop') %>%
    arrange(desc(metal_share)) %>%
    print(n = Inf)
}

build_and_write_naics(domestic_331, 'resources/metal_content_shares_naics_domestic.csv', 'Domestic requirements')
build_and_write_naics(total_331, 'resources/metal_content_shares_naics_total.csv', 'Total requirements')
