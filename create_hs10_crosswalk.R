# Script to create HS10 → GTAP crosswalk from HS6 crosswalk
# All HS10 codes inherit their GTAP sector from their HS6 prefix

library(tidyverse)

cat('Creating HS10 → GTAP crosswalk...\n\n')

# Source functions to get access to load_imports_hs10_country()
source('src/functions.R')

# Load the existing HS6 → GTAP crosswalk
hs6_crosswalk <- read_csv('resources/hs6_gtap_crosswalk.csv', show_col_types = FALSE)

cat(sprintf('Loaded HS6 crosswalk with %s codes\n', format(nrow(hs6_crosswalk), big.mark = ',')))

# Load 2024 HS10 import data to get all unique HS10 codes
hs10_data <- load_imports_hs10_country(
  import_data_path = 'C:/Users/jar335/Downloads',
  year = 2024,
  type = 'con'
)

# Get unique HS10 codes
unique_hs10 <- hs10_data %>%
  distinct(hs10) %>%
  arrange(hs10)

cat(sprintf('Found %s unique HS10 codes in 2024 data\n', format(nrow(unique_hs10), big.mark = ',')))

# Create HS10 crosswalk by extracting HS6 prefix and joining
hs10_crosswalk <- unique_hs10 %>%
  mutate(hs6_code = substr(hs10, 1, 6)) %>%
  left_join(
    hs6_crosswalk %>% select(hs6_code, gtap_code, description),
    by = 'hs6_code'
  ) %>%
  select(hs10, hs6_code, gtap_code, description)

# Check for any unmapped codes
unmapped <- hs10_crosswalk %>%
  filter(is.na(gtap_code))

if (nrow(unmapped) > 0) {
  cat(sprintf('\nWARNING: %s HS10 codes could not be mapped to GTAP sectors:\n', nrow(unmapped)))
  print(head(unmapped, 20))
  cat('\nThese codes will need manual mapping or may indicate missing HS6 codes in the crosswalk.\n')
} else {
  cat('\nAll HS10 codes successfully mapped to GTAP sectors!\n')
}

# Write to CSV
output_path <- 'resources/hs10_gtap_crosswalk.csv'
write_csv(hs10_crosswalk, output_path)

cat(sprintf('\nWrote HS10 crosswalk to %s\n', output_path))
cat(sprintf('Total records: %s\n', format(nrow(hs10_crosswalk), big.mark = ',')))

# Show summary by GTAP sector
cat('\n=== HS10 Codes per GTAP Sector ===\n')
sector_summary <- hs10_crosswalk %>%
  filter(!is.na(gtap_code)) %>%
  count(gtap_code, name = 'n_hs10_codes') %>%
  arrange(desc(n_hs10_codes))

print(sector_summary, n = 20)

cat('\nDone!\n')
