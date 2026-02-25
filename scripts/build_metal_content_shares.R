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
# BEA Detail-level (per-metal-type):
#   C:/Users/jar335/Downloads/IxC_TR_Detail.xlsx  (2017 benchmark Detail IO)
#
#   - resources/naics_bea_detail_crosswalk.csv     (NAICS → BEA detail code)
#   - resources/metal_content_shares_detail_total.csv  (BEA detail commodity level)
#   - resources/metal_content_shares_detail_hs10_total.csv  (HS10 level, per-metal-type)
#
# =============================================================================

library(tidyverse)
library(readxl)


# =============================================================================
# Metal sub-industry groupings for per-metal-type shares
# =============================================================================

STEEL_INDUSTRIES    <- c('331110', '331200', '331510')
ALUMINUM_INDUSTRIES <- c('331313', '331314', '33131B')
COPPER_INDUSTRIES   <- c('331410', '331420')
OTHER_NF_INDUSTRIES <- c('331490')
NF_FOUNDRIES        <- '331520'


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
# Helper: read BEA Detail IxC Total Requirements and extract per-metal-type shares
# =============================================================================
#
# Reads the 2017 Detail IO table (402 commodity columns × ~400 industry rows).
# Extracts the 10 metal sub-industry rows (331*), sums their contributions to
# each commodity into steel/aluminum/copper/other_metal buckets.
#
# 331520 (NF foundries) is split proportionally across metal types based on its
# own column inputs from the other 331* industries.
# =============================================================================

read_bea_detail_requirements <- function(path, sheet = '2017') {

  message('Reading BEA Detail IO table: ', path, ' (sheet: ', sheet, ')')

  # Row 5 has BEA detail codes as column headers
  code_row <- read_excel(path, sheet = sheet, skip = 4, n_max = 1,
                          col_names = FALSE)
  col_codes <- as.character(code_row[1, ])
  col_codes[1:2] <- c('row_code', 'row_name')

  # Read data starting from row 6
  req_raw <- read_excel(path, sheet = sheet, skip = 5, col_names = col_codes)
  req_raw <- req_raw %>%
    mutate(row_code = str_trim(as.character(row_code)))

  # Commodity columns (skip row_code, row_name)
  commodity_cols <- col_codes[3:length(col_codes)]
  # Drop empty trailing columns
  commodity_cols <- commodity_cols[commodity_cols != '' & !is.na(commodity_cols)]

  message(sprintf('  Read %d rows x %d commodity cols', nrow(req_raw), length(commodity_cols)))

  # Extract all 331* rows
  metal_rows <- req_raw %>%
    filter(str_starts(row_code, '331'))

  all_metal_codes <- c(STEEL_INDUSTRIES, ALUMINUM_INDUSTRIES, COPPER_INDUSTRIES,
                        OTHER_NF_INDUSTRIES, NF_FOUNDRIES)
  found_codes <- metal_rows$row_code
  missing <- setdiff(all_metal_codes, found_codes)
  if (length(missing) > 0) {
    stop('Missing expected metal industry rows: ', paste(missing, collapse = ', '))
  }

  message(sprintf('  Found %d metal sub-industry rows: %s',
                  nrow(metal_rows), paste(found_codes, collapse = ', ')))

  # Convert to numeric matrix for fast computation
  metal_data <- metal_rows %>%
    select(row_code, all_of(commodity_cols)) %>%
    mutate(across(-row_code, ~ {
      x <- as.character(.)
      x <- if_else(x == '---' | is.na(x), '0', x)
      as.numeric(x)
    }))

  # --------------------------------------------------------------------------
  # Compute 331520 proportional split weights from its own column inputs
  # --------------------------------------------------------------------------

  # Find 331520 column index in the commodity columns
  nf_foundry_col <- NF_FOUNDRIES
  if (!nf_foundry_col %in% commodity_cols) {
    stop('331520 not found in commodity columns')
  }

  # Get 331520's column values for non-foundry metal industries
  nf_col_data <- metal_data %>%
    filter(row_code != NF_FOUNDRIES) %>%
    select(row_code, !!nf_foundry_col) %>%
    rename(input_val = !!nf_foundry_col)

  # Compute type weights for splitting 331520
  steel_input <- nf_col_data %>% filter(row_code %in% STEEL_INDUSTRIES) %>% pull(input_val) %>% sum()
  alum_input  <- nf_col_data %>% filter(row_code %in% ALUMINUM_INDUSTRIES) %>% pull(input_val) %>% sum()
  copper_input <- nf_col_data %>% filter(row_code %in% COPPER_INDUSTRIES) %>% pull(input_val) %>% sum()
  other_input <- nf_col_data %>% filter(row_code %in% OTHER_NF_INDUSTRIES) %>% pull(input_val) %>% sum()
  total_input <- steel_input + alum_input + copper_input + other_input

  nf_split_steel  <- steel_input / total_input
  nf_split_alum   <- alum_input / total_input
  nf_split_copper <- copper_input / total_input
  nf_split_other  <- other_input / total_input

  message(sprintf('  NF foundry (331520) split: steel=%.1f%%, aluminum=%.1f%%, copper=%.1f%%, other=%.1f%%',
                  nf_split_steel * 100, nf_split_alum * 100,
                  nf_split_copper * 100, nf_split_other * 100))

  # --------------------------------------------------------------------------
  # Sum per-metal-type shares for each commodity
  # --------------------------------------------------------------------------

  # Get row vectors for each metal type group
  get_row_sum <- function(codes) {
    metal_data %>%
      filter(row_code %in% codes) %>%
      select(all_of(commodity_cols)) %>%
      summarise(across(everything(), sum))
  }

  steel_row  <- get_row_sum(STEEL_INDUSTRIES)
  alum_row   <- get_row_sum(ALUMINUM_INDUSTRIES)
  copper_row <- get_row_sum(COPPER_INDUSTRIES)
  other_row  <- get_row_sum(OTHER_NF_INDUSTRIES)

  # NF foundries row (331520) — split proportionally
  nf_row <- metal_data %>%
    filter(row_code == NF_FOUNDRIES) %>%
    select(all_of(commodity_cols))

  # Add proportional 331520 contribution to each type
  steel_vals  <- as.numeric(steel_row[1, ]) + as.numeric(nf_row[1, ]) * nf_split_steel
  alum_vals   <- as.numeric(alum_row[1, ]) + as.numeric(nf_row[1, ]) * nf_split_alum
  copper_vals <- as.numeric(copper_row[1, ]) + as.numeric(nf_row[1, ]) * nf_split_copper
  other_vals  <- as.numeric(other_row[1, ]) + as.numeric(nf_row[1, ]) * nf_split_other
  total_vals  <- steel_vals + alum_vals + copper_vals + other_vals

  # Build output tibble
  detail_shares <- tibble(
    bea_detail_code    = commodity_cols,
    steel_share        = steel_vals,
    aluminum_share     = alum_vals,
    copper_share       = copper_vals,
    other_metal_share  = other_vals,
    metal_share        = total_vals
  ) %>%
    mutate(metal_share = pmin(metal_share, 1.0))

  message(sprintf('  Detail shares: %d commodities, mean metal_share = %.4f',
                  nrow(detail_shares), mean(detail_shares$metal_share)))

  return(detail_shares)
}


# =============================================================================
# Helper: extract NAICS → BEA detail code crosswalk from NAICS Codes sheet
# =============================================================================

extract_naics_bea_detail_crosswalk <- function(path, sheet = 'NAICS Codes') {

  message('Extracting NAICS-BEA detail crosswalk from: ', path)

  # Row 5 has column headers (Sector, Summary, U.Summary, Detail, ...); skip it
  raw <- read_excel(path, sheet = sheet, skip = 5, col_names = FALSE)

  # Col 4 (index 4) = BEA detail code, Col 7 (index 7) = related NAICS codes
  crosswalk_raw <- raw %>%
    transmute(
      bea_detail_code = as.character(...4),
      naics_raw       = as.character(...7)
    ) %>%
    filter(!is.na(bea_detail_code) & bea_detail_code != '' &
           !is.na(naics_raw) & naics_raw != '' & naics_raw != 'n.a.')

  message(sprintf('  Found %d BEA detail codes with NAICS mappings', nrow(crosswalk_raw)))

  # Expand multi-NAICS entries (e.g., '331315, 331318' or '11111-2')
  crosswalk <- crosswalk_raw %>%
    mutate(naics_list = str_split(naics_raw, ',\\s*')) %>%
    unnest(naics_list) %>%
    mutate(naics = str_trim(naics_list)) %>%
    # Expand dash ranges (e.g., '11111-2' -> '11111', '11112')
    mutate(naics = map(naics, function(n) {
      if (str_detect(n, '-')) {
        parts <- str_split(n, '-')[[1]]
        prefix <- parts[1]
        suffix <- parts[2]
        # The suffix digits replace the last N digits of the prefix
        suffix_len <- nchar(suffix)
        base <- substr(prefix, 1, nchar(prefix) - suffix_len)
        start <- as.integer(substr(prefix, nchar(prefix) - suffix_len + 1, nchar(prefix)))
        end <- as.integer(suffix)
        sprintf(paste0('%s%0', suffix_len, 'd'), base, start:end)
      } else {
        n
      }
    })) %>%
    unnest(naics) %>%
    select(naics, bea_detail_code) %>%
    distinct()

  message(sprintf('  Expanded to %d NAICS → BEA detail mappings', nrow(crosswalk)))

  return(crosswalk)
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


# =============================================================================
# 4. Build BEA Detail-level per-metal-type shares
# =============================================================================
#
# Uses the 2017 benchmark Detail IO table (~402 commodities, 10 metal
# sub-industries). Each commodity gets steel/aluminum/copper/other_metal shares.
# Then chains HS10 -> NAICS -> BEA detail code to produce HS10-level shares.
# =============================================================================

message('\n--- Building BEA Detail per-metal-type shares ---')

detail_io_path <- 'C:/Users/jar335/Downloads/IxC_TR_Detail.xlsx'

if (!file.exists(detail_io_path)) {
  message('BEA Detail IO file not found: ', detail_io_path)
  message('Skipping Section 4 (detail per-metal-type shares)')
} else {

  # 4a. Extract per-metal-type shares at BEA detail commodity level
  detail_shares <- read_bea_detail_requirements(detail_io_path)

  write_csv(detail_shares, 'resources/metal_content_shares_detail_total.csv')
  message(sprintf('Wrote %d rows to resources/metal_content_shares_detail_total.csv',
                  nrow(detail_shares)))

  # Show key metal-related commodities
  message('--- Detail shares: key metal commodities ---')
  detail_shares %>%
    filter(str_starts(bea_detail_code, '331') |
           bea_detail_code %in% c('336370', '335920', '332710', '333120')) %>%
    mutate(across(where(is.numeric), ~ sprintf('%.4f', .))) %>%
    print(n = Inf)

  # 4b. Extract NAICS → BEA detail crosswalk
  naics_bea_detail <- extract_naics_bea_detail_crosswalk(detail_io_path)

  write_csv(naics_bea_detail, 'resources/naics_bea_detail_crosswalk.csv')
  message(sprintf('Wrote %d rows to resources/naics_bea_detail_crosswalk.csv',
                  nrow(naics_bea_detail)))

  # 4c. Chain HS10 -> NAICS -> BEA detail -> per-metal-type shares
  message('\n--- Chaining HS10 -> NAICS -> BEA detail -> per-metal shares ---')

  # Reuse existing hs10_naics crosswalk (already loaded in Section 3)
  # Match NAICS to BEA detail codes (same longest-prefix logic as summary)
  hs10_bea_detail <- hs10_naics %>%
    mutate(bea_detail_code = match_naics_to_bea(naics, naics_bea_detail %>%
      rename(bea_summary_code = bea_detail_code)))

  matched_detail <- sum(!is.na(hs10_bea_detail$bea_detail_code))
  message(sprintf('NAICS->BEA detail matching: %d of %d matched (%.1f%%)',
                  matched_detail, nrow(hs10_bea_detail),
                  100 * matched_detail / nrow(hs10_bea_detail)))

  # Join with detail shares
  hs10_detail_shares <- hs10_bea_detail %>%
    left_join(detail_shares, by = 'bea_detail_code') %>%
    # Unmatched HS10 codes: per-type shares = 0, total metal_share = 1.0
    mutate(
      steel_share       = if_else(is.na(steel_share), 0, steel_share),
      aluminum_share    = if_else(is.na(aluminum_share), 0, aluminum_share),
      copper_share      = if_else(is.na(copper_share), 0, copper_share),
      other_metal_share = if_else(is.na(other_metal_share), 0, other_metal_share),
      metal_share       = if_else(is.na(metal_share), 1.0, metal_share)
    )

  write_csv(hs10_detail_shares,
            'resources/metal_content_shares_detail_hs10_total.csv')
  message(sprintf('Wrote %d rows to resources/metal_content_shares_detail_hs10_total.csv',
                  nrow(hs10_detail_shares)))

  # Summary stats
  message(sprintf('--- Detail HS10 share distribution ---'))
  message(sprintf('  metal_share:  mean=%.4f, median=%.4f, range=[%.4f, %.4f]',
                  mean(hs10_detail_shares$metal_share),
                  median(hs10_detail_shares$metal_share),
                  min(hs10_detail_shares$metal_share),
                  max(hs10_detail_shares$metal_share)))
  message(sprintf('  steel_share:  mean=%.4f', mean(hs10_detail_shares$steel_share)))
  message(sprintf('  aluminum_share: mean=%.4f', mean(hs10_detail_shares$aluminum_share)))
  message(sprintf('  copper_share: mean=%.4f', mean(hs10_detail_shares$copper_share)))
  message(sprintf('  other_metal:  mean=%.4f', mean(hs10_detail_shares$other_metal_share)))
}
