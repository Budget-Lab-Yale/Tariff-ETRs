## Final diagnosis: 8544 products in tracker
library(tidyverse)

tracker_dir <- 'C:/Users/jar335/Documents/Repositories/tariff-rate-tracker'

snap <- readRDS(file.path(tracker_dir, 'data', 'timeseries', 'snapshot_2026_rev_4.rds'))

# Focus on the exact products from our comparison that had big diffs
problem_products <- c('8544606000', '8544499000', '8544492000', '8544602000', '8544190000')

cat('=== Tracker snapshot for problem products × Canada ===\n')
snap %>%
  filter(hts10 %in% problem_products, country == '1220') %>%
  select(hts10, rate_232, metal_share, aluminum_share, copper_share,
         steel_share, deriv_type, statutory_rate_232) %>%
  print()

cat('\n=== Tracker: ALL aluminum derivs where aluminum_share >= 1 × Canada ===\n')
snap %>%
  filter(deriv_type == 'aluminum', country == '1220', aluminum_share >= 1) %>%
  select(hts10, rate_232, metal_share, aluminum_share, deriv_type) %>%
  print(n = 30)

cat('\n=== Tracker: ALL aluminum derivs × Canada, sorted by rate_232 desc ===\n')
snap %>%
  filter(deriv_type == 'aluminum', country == '1220') %>%
  select(hts10, rate_232, metal_share, aluminum_share, statutory_rate_232) %>%
  arrange(desc(rate_232)) %>%
  print(n = 20)

# Check the crucial detail: is rate_232 scaled or not for 8544606000?
# statutory_rate_232 = 0.5, rate_232 = 0.5 means NO scaling was applied
cat('\n=== Products where rate_232 == statutory_rate_232 (unscaled) AND not primary chapter ===\n')
unscaled <- snap %>%
  filter(deriv_type == 'aluminum', country == '1220',
         abs(rate_232 - statutory_rate_232) < 1e-10,
         !substr(hts10, 1, 2) %in% c('72', '73', '76', '74')) %>%
  select(hts10, rate_232, metal_share, aluminum_share, statutory_rate_232, deriv_type)
cat(nrow(unscaled), 'products\n')
print(unscaled, n = 40)

# Cross-reference: are these in heading product lists?
# Load all heading product lists
copper <- read_csv(file.path(tracker_dir, 'resources', 's232_copper_products.csv'),
                   col_types = cols(.default = col_character()))[[1]]

# Check which unscaled Ch85+ products are copper heading products
cat('\n=== Of these, copper heading products: ===\n')
unscaled_copper <- unscaled %>% filter(substr(hs10, 1, 8) %in% copper)
print(unscaled_copper)

cat('\n=== Of these, NOT copper heading: ===\n')
unscaled_not_copper <- unscaled %>% filter(!substr(hs10, 1, 8) %in% copper)
print(unscaled_not_copper, n = 30)

rm(snap); gc()
cat('\nDone.\n')
