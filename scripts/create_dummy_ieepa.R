library(tidyverse)

# Read crosswalk to get unique GTAP sectors
crosswalk <- read_csv('resources/hs6_gtap_crosswalk.csv', show_col_types = FALSE)
unique_gtap <- sort(unique(tolower(crosswalk$gtap_code)))

# Create dummy IEEPA rates (0.2 for all country-sector combinations)
ieepa_rates <- tibble(
  gtap_code = unique_gtap,
  china  = 0.2,
  canada = 0.2,
  mexico = 0.2,
  japan  = 0.2,
  uk     = 0.2,
  eu     = 0.2,
  row    = 0.2
)

# Write to CSV
write_csv(ieepa_rates, 'config/ieepa_rates.csv')
cat('Created config/ieepa_rates.csv with', nrow(ieepa_rates), 'GTAP sectors\n')
