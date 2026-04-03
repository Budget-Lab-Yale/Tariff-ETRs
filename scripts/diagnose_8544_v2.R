## Focused diagnosis: why does tracker have metal_share=1 for 8544606000
## when BEA says aluminum_share=0.0832?
library(tidyverse)

tracker_dir <- 'C:/Users/jar335/Documents/Repositories/tariff-rate-tracker'

# Load tracker snapshot
snap <- readRDS(file.path(tracker_dir, 'data', 'timeseries', 'snapshot_2026_rev_4.rds'))

# All aluminum derivatives in the tracker
alum_derivs <- snap %>%
  filter(deriv_type == 'aluminum', country == '1220') %>%
  select(hts10, rate_232, metal_share, aluminum_share, copper_share,
         steel_share, other_metal_share, statutory_rate_232, deriv_type) %>%
  rename(hs10 = hts10)

cat('Aluminum derivatives × Canada:', nrow(alum_derivs), '\n\n')

# How many have metal_share == 1?
cat('metal_share == 1:', sum(alum_derivs$metal_share == 1), '\n')
cat('metal_share < 1:', sum(alum_derivs$metal_share < 1), '\n')
cat('aluminum_share == 0:', sum(alum_derivs$aluminum_share == 0), '\n')
cat('aluminum_share > 0:', sum(alum_derivs$aluminum_share > 0), '\n\n')

# Show the ones with metal_share == 1 and aluminum_share == 0
cat('=== Alum derivs with metal_share=1 AND aluminum_share=0 ===\n')
anomalous <- alum_derivs %>%
  filter(metal_share == 1, aluminum_share == 0)
cat(nrow(anomalous), 'products\n')
print(anomalous, n = 30)

# Show some with aluminum_share > 0 (working correctly)
cat('\n=== Alum derivs with aluminum_share > 0 (correctly scaled) ===\n')
working <- alum_derivs %>%
  filter(aluminum_share > 0) %>%
  head(20)
print(working)

# Check: are the anomalous ones the same as the heading/derivative overlap?
# Load copper heading products
copper_prods <- read_csv(file.path(tracker_dir, 'resources', 's232_copper_products.csv'),
                         col_types = cols(.default = col_character()))
copper_hts8 <- copper_prods[[1]]
copper_pattern <- paste0('^(', paste(copper_hts8, collapse = '|'), ')')

# Check overlap
anomalous_in_copper <- anomalous %>%
  filter(grepl(copper_pattern, hs10))
cat('\nAnomalous alum derivs that are also copper heading:', nrow(anomalous_in_copper), '\n')
cat('Anomalous alum derivs NOT copper heading:', nrow(anomalous) - nrow(anomalous_in_copper), '\n')

if (nrow(anomalous) - nrow(anomalous_in_copper) > 0) {
  cat('\nThese are NOT copper but have metal_share=1, aluminum_share=0:\n')
  anomalous %>%
    filter(!grepl(copper_pattern, hs10)) %>%
    print(n = 30)
}

# Compare with actual BEA shares (load from ETRs)
cat('\n=== Cross-check with ETRs BEA shares ===\n')
etrs_dir <- 'C:/Users/jar335/Documents/Repositories/Tariff-etrs'
source(file.path(etrs_dir, 'src', 'config_parsing.R'))
imports <- readRDS(file.path(etrs_dir, 'cache', 'hs10_by_country_gtap_2024_con.rds')) %>%
  filter(!stringr::str_detect(hs10, '^(98|99)'), !is.na(gtap_code))

other_params <- yaml::read_yaml(file.path(etrs_dir, 'config', 'historical', '2026-02-20', 'other_params.yaml'))
bea_shares <- load_metal_content(
  metal_content_config = other_params$metal_content,
  import_data = imports
)

# Join ETRs BEA shares with the anomalous tracker products
comparison <- anomalous %>%
  left_join(bea_shares %>% rename(
    bea_metal_share = metal_share,
    bea_steel = steel_share,
    bea_aluminum = aluminum_share,
    bea_copper = copper_share
  ), by = 'hs10')

cat('BEA shares for anomalous products:\n')
comparison %>%
  select(hs10, metal_share, aluminum_share, bea_metal_share, bea_aluminum, bea_copper) %>%
  print(n = 30)

rm(snap); gc()
cat('\nDone.\n')
