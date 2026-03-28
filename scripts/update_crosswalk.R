# Update hs10_gtap_crosswalk.csv to include all HS10 codes from the tracker CSV
#
# Maps new codes by matching at HS6 level to existing crosswalk entries.
# Codes with no HS6 match fall back to HS4, then HS2.

library(tidyverse)

# Load current crosswalk
xwalk <- read_csv('resources/hs10_gtap_crosswalk.csv', show_col_types = FALSE,
                   col_types = cols(hs10 = col_character()))

# Get all HS10 codes from the latest tracker CSV
csv_data <- read_csv('config/historical/2026-02-20/statutory_rates.csv.gz',
                      show_col_types = FALSE, col_types = cols(hts10 = col_character()),
                      col_select = 'hts10')
csv_hs10 <- sort(unique(csv_data$hts10))
rm(csv_data); gc(verbose = FALSE)
message(sprintf('Crosswalk HS10 codes: %s', format(nrow(xwalk), big.mark = ',')))
message(sprintf('Tracker CSV HS10 codes: %s', format(length(csv_hs10), big.mark = ',')))

# Find codes missing from crosswalk
missing <- setdiff(csv_hs10, xwalk$hs10)
message(sprintf('Missing from crosswalk: %s', format(length(missing), big.mark = ',')))

if (length(missing) == 0) {
  message('Nothing to do!')
  quit(status = 0)
}

# Build HS6 -> GTAP lookup from existing crosswalk (use most common mapping per HS6)
hs6_lookup <- xwalk %>%
  mutate(hs6 = substr(hs10, 1, 6)) %>%
  count(hs6, gtap_code) %>%
  group_by(hs6) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(hs6, gtap_code)

# Build HS4 fallback
hs4_lookup <- xwalk %>%
  mutate(hs4 = substr(hs10, 1, 4)) %>%
  count(hs4, gtap_code) %>%
  group_by(hs4) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(hs4, gtap_code_hs4 = gtap_code)

# Build HS2 fallback
hs2_lookup <- xwalk %>%
  mutate(hs2 = substr(hs10, 1, 2)) %>%
  count(hs2, gtap_code) %>%
  group_by(hs2) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(hs2, gtap_code_hs2 = gtap_code)

# Map missing codes
new_rows <- tibble(hs10 = missing) %>%
  mutate(
    hs6_code = substr(hs10, 1, 6),
    hs4 = substr(hs10, 1, 4),
    hs2 = substr(hs10, 1, 2)
  ) %>%
  left_join(hs6_lookup, by = c('hs6_code' = 'hs6')) %>%
  left_join(hs4_lookup, by = 'hs4') %>%
  left_join(hs2_lookup, by = 'hs2') %>%
  mutate(
    gtap_code = coalesce(gtap_code, gtap_code_hs4, gtap_code_hs2),
    description = ''
  ) %>%
  select(hs10, hs6_code, gtap_code, description)

matched <- sum(!is.na(new_rows$gtap_code))
unmatched <- sum(is.na(new_rows$gtap_code))
message(sprintf('Mapped: %d (HS6/HS4/HS2 match)', matched))
if (unmatched > 0) {
  message(sprintf('WARNING: %d codes could not be mapped:', unmatched))
  print(new_rows %>% filter(is.na(gtap_code)) %>% select(hs10, hs6_code))
}

# Combine and sort
updated <- bind_rows(xwalk, new_rows %>% filter(!is.na(gtap_code))) %>%
  arrange(hs10)

message(sprintf('\nUpdated crosswalk: %s rows (was %s)',
                format(nrow(updated), big.mark = ','),
                format(nrow(xwalk), big.mark = ',')))

write_csv(updated, 'resources/hs10_gtap_crosswalk.csv')
message('Wrote resources/hs10_gtap_crosswalk.csv')
