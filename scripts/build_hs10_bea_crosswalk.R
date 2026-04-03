#' build_hs10_bea_crosswalk.R
#'
#' One-time script that chains HS10 -> NAICS -> BEA to produce
#' resources/hs10_bea_crosswalk.csv.
#'
#' Run from the Tariff-ETRs root directory.

suppressPackageStartupMessages(library(tidyverse))

# ==== Load crosswalks ====

hs10_naics <- read_csv('resources/hs10_naics_crosswalk.csv', show_col_types = FALSE)
naics_bea <- read_csv('resources/naics_bea_summary_crosswalk.csv', show_col_types = FALSE)

# ==== Longest-prefix NAICS -> BEA matching ====

match_naics_to_bea <- function(naics_codes, naics_bea_lookup) {
  bea_lookup <- setNames(naics_bea_lookup$bea_summary_code, naics_bea_lookup$naics)
  sapply(naics_codes, function(code) {
    clean <- gsub('[^0-9]', '', code)
    for (len in nchar(clean):2) {
      prefix <- substr(clean, 1, len)
      if (prefix %in% names(bea_lookup)) return(bea_lookup[prefix])
    }
    return(NA_character_)
  }, USE.NAMES = FALSE)
}

# ==== Build HS10 -> BEA crosswalk ====

hs10_bea <- hs10_naics %>%
  mutate(bea_code = match_naics_to_bea(naics, naics_bea)) %>%
  select(hs10, bea_code)

# ==== Coverage statistics ====

n_total <- nrow(hs10_bea)
n_matched <- sum(!is.na(hs10_bea$bea_code))
n_unmatched <- n_total - n_matched
pct_matched <- round(100 * n_matched / n_total, 1)

cat('Coverage statistics:\n')
cat(sprintf('  Total rows:   %d\n', n_total))
cat(sprintf('  Matched:      %d (%.1f%%)\n', n_matched, pct_matched))
cat(sprintf('  Unmatched:    %d (%.1f%%)\n', n_unmatched, 100 - pct_matched))

# ==== Write output ====

write_csv(hs10_bea, 'resources/hs10_bea_crosswalk.csv')
cat('\nWrote resources/hs10_bea_crosswalk.csv\n')
