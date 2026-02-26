# =============================================================================
# build_mfn_rates.R
# =============================================================================
#
# Standalone script to download and parse MFN (General) tariff rates from the
# USITC Harmonized Tariff Schedule CSV.
#
# The HTS CSV contains rates as free-text strings (e.g., "5.5%", "Free",
# "1.5¢/kg + 5%", "4.4¢/kg"). This script parses the ad valorem component
# following the GTA methodology:
#   - "Free"            -> 0.0
#   - "5.5%"            -> 0.055  (pure ad valorem)
#   - "1.5¢/kg + 5%"   -> 0.05   (compound: extract AV component)
#   - "4.4¢/kg"         -> 0.0    (specific only: no AV component)
#
# The HTS stores rates at the HS8 level. HS10 statistical suffixes inherit
# from their HS8 parent. Output is at HS8 for compactness; at runtime, join
# via substr(hs10, 1, 8).
#
# Input:
#   Downloaded from USITC:
#   https://www.usitc.gov/sites/default/files/tata/hts/hts_2025_basic_edition_csv.csv
#
# Output:
#   resources/mfn_rates_2025.csv
#     Columns: hs8, mfn_rate, rate_type, rate_raw
#
# Usage:
#   Rscript scripts/build_mfn_rates.R
#
# =============================================================================

library(tidyverse)

# =============================================================================
# Configuration
# =============================================================================

HTS_URL <- 'https://www.usitc.gov/sites/default/files/tata/hts/hts_2025_basic_edition_csv.csv'
HTS_LOCAL <- 'resources/hts_2025_basic_edition.csv'
OUTPUT_FILE <- 'resources/mfn_rates_2025.csv'

# =============================================================================
# Download HTS CSV if not already cached
# =============================================================================

if (!file.exists(HTS_LOCAL)) {
  message('Downloading HTS 2025 Basic Edition CSV...')
  download.file(HTS_URL, HTS_LOCAL, mode = 'wb', quiet = FALSE)
  message(sprintf('Downloaded to %s', HTS_LOCAL))
} else {
  message(sprintf('Using cached HTS CSV: %s', HTS_LOCAL))
}

# =============================================================================
# Read and parse
# =============================================================================

hts_raw <- read_csv(HTS_LOCAL, show_col_types = FALSE, col_types = cols(.default = col_character()))

message(sprintf('Read %s rows from HTS CSV', format(nrow(hts_raw), big.mark = ',')))

# Standardize column names
hts <- hts_raw %>%
  rename(
    hts_number = `HTS Number`,
    indent = Indent,
    description = Description,
    general_rate = `General Rate of Duty`
  ) %>%
  select(hts_number, indent, description, general_rate) %>%
  mutate(
    # Strip dots to get pure digit code
    hts_digits = str_remove_all(hts_number, '\\.'),
    n_digits = nchar(hts_digits),
    general_rate = str_trim(general_rate)
  )

# =============================================================================
# Parse ad valorem component from rate strings
# =============================================================================

#' Parse the ad valorem component from an HTS General Rate of Duty string.
#'
#' @param rate_str Character string like "5.5%", "Free", "1.5¢/kg + 5%"
#' @return Numeric ad valorem rate as decimal (e.g., 0.055), or 0 for specific-only/Free
parse_av_rate <- function(rate_str) {
  # Empty or NA
  if (is.na(rate_str) || rate_str == '') return(NA_real_)

  # Free
  if (rate_str == 'Free') return(0)

  # Pure ad valorem: "5.5%"
  m <- str_match(rate_str, '^([0-9.]+)%$')
  if (!is.na(m[1, 2])) return(as.numeric(m[1, 2]) / 100)

  # Compound: contains % and + (e.g., "1.5¢/kg + 5%", "80.4¢/kg + 6.4%")
  # Extract the ad valorem component (the part with %)
  if (str_detect(rate_str, '%') && str_detect(rate_str, '\\+')) {
    m <- str_match(rate_str, '([0-9.]+)%')
    if (!is.na(m[1, 2])) return(as.numeric(m[1, 2]) / 100)
  }

  # Specific only (no % sign) - ad valorem component is 0
  if (!str_detect(rate_str, '%')) return(0)

  # Fallback: has % but doesn't match above patterns
  # Try to extract any percentage
  m <- str_match(rate_str, '([0-9.]+)%')
  if (!is.na(m[1, 2])) return(as.numeric(m[1, 2]) / 100)

  # Truly unparseable
  return(NA_real_)
}

#' Classify the rate type
classify_rate <- function(rate_str) {
  if (is.na(rate_str) || rate_str == '') return('empty')
  if (rate_str == 'Free') return('free')
  if (str_detect(rate_str, '^[0-9.]+%$')) return('ad_valorem')
  if (str_detect(rate_str, '%') && str_detect(rate_str, '\\+')) return('compound')
  if (!str_detect(rate_str, '%')) return('specific')
  return('other')
}

# =============================================================================
# Build HS8-level rate table
# =============================================================================

# Extract HS8-level rows (8-digit codes carry the statutory rate)
hs8_rates <- hts %>%
  filter(n_digits == 8, !is.na(general_rate), general_rate != '') %>%
  mutate(
    hs8 = hts_digits,
    mfn_rate = sapply(general_rate, parse_av_rate),
    rate_type = sapply(general_rate, classify_rate)
  ) %>%
  select(hs8, mfn_rate, rate_type, rate_raw = general_rate)

message(sprintf('Parsed %s HS8-level rates', format(nrow(hs8_rates), big.mark = ',')))

# =============================================================================
# Validate against HS10 universe
# =============================================================================

crosswalk <- read_csv('resources/hs10_gtap_crosswalk.csv', show_col_types = FALSE,
                       col_types = cols(hs10 = col_character()))
hs10_codes <- unique(crosswalk$hs10)
hs8_from_hs10 <- unique(substr(hs10_codes, 1, 8))

matched <- sum(hs8_from_hs10 %in% hs8_rates$hs8)
unmatched <- hs8_from_hs10[!hs8_from_hs10 %in% hs8_rates$hs8]

message(sprintf('HS8 coverage of HS10 universe: %d / %d (%.1f%%)',
                matched, length(hs8_from_hs10),
                100 * matched / length(hs8_from_hs10)))

# For unmatched HS8, check if HS10 codes have direct rates at the 10-digit level
# (some HTS entries only have rates at HS10, not HS8)
if (length(unmatched) > 0) {
  hs10_direct <- hts %>%
    filter(n_digits == 10, !is.na(general_rate), general_rate != '') %>%
    mutate(
      hs8 = substr(hts_digits, 1, 8),
      mfn_rate = sapply(general_rate, parse_av_rate),
      rate_type = sapply(general_rate, classify_rate)
    ) %>%
    # Take first HS10 rate per HS8 group (they typically share the same rate)
    filter(hs8 %in% unmatched) %>%
    group_by(hs8) %>%
    slice(1) %>%
    ungroup() %>%
    select(hs8, mfn_rate, rate_type, rate_raw = general_rate)

  if (nrow(hs10_direct) > 0) {
    message(sprintf('Recovered %d HS8 rates from HS10-level entries', nrow(hs10_direct)))
    hs8_rates <- bind_rows(hs8_rates, hs10_direct)
  }

  # Re-check coverage
  still_unmatched <- unmatched[!unmatched %in% hs10_direct$hs8]
  if (length(still_unmatched) > 0) {
    message(sprintf('Still unmatched: %d HS8 codes (will default to 0 at runtime)',
                    length(still_unmatched)))
  }
}

# =============================================================================
# Summary statistics
# =============================================================================

rate_summary <- hs8_rates %>%
  group_by(rate_type) %>%
  summarise(
    n = n(),
    mean_rate = mean(mfn_rate, na.rm = TRUE),
    median_rate = median(mfn_rate, na.rm = TRUE),
    .groups = 'drop'
  )

message('\nRate type breakdown:')
for (i in seq_len(nrow(rate_summary))) {
  message(sprintf('  %-12s: %5d codes, mean = %.2f%%, median = %.2f%%',
                  rate_summary$rate_type[i],
                  rate_summary$n[i],
                  rate_summary$mean_rate[i] * 100,
                  rate_summary$median_rate[i] * 100))
}

overall <- hs8_rates %>%
  filter(!is.na(mfn_rate)) %>%
  summarise(
    n = n(),
    mean = mean(mfn_rate),
    median = median(mfn_rate),
    p75 = quantile(mfn_rate, 0.75),
    p90 = quantile(mfn_rate, 0.90),
    max = max(mfn_rate)
  )

message(sprintf('\nOverall (n=%d): mean=%.2f%%, median=%.2f%%, p75=%.2f%%, p90=%.2f%%, max=%.1f%%',
                overall$n, overall$mean * 100, overall$median * 100,
                overall$p75 * 100, overall$p90 * 100, overall$max * 100))

nonzero <- hs8_rates %>% filter(!is.na(mfn_rate), mfn_rate > 0)
message(sprintf('Non-zero (n=%d): mean=%.2f%%, median=%.2f%%',
                nrow(nonzero), mean(nonzero$mfn_rate) * 100, median(nonzero$mfn_rate) * 100))

# =============================================================================
# Write output
# =============================================================================

hs8_rates %>%
  filter(!is.na(mfn_rate)) %>%
  arrange(hs8) %>%
  write_csv(OUTPUT_FILE)

message(sprintf('\nWrote %s rows to %s', format(nrow(hs8_rates %>% filter(!is.na(mfn_rate))), big.mark = ','), OUTPUT_FILE))
