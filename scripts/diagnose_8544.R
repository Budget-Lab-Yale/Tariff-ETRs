## Diagnose 8544 divergence between Tracker and ETRs
library(tidyverse)
library(yaml)

tracker_dir <- 'C:/Users/jar335/Documents/Repositories/tariff-rate-tracker'
etrs_dir    <- 'C:/Users/jar335/Documents/Repositories/Tariff-etrs'

script_dir <- file.path(etrs_dir, 'src')
source(file.path(script_dir, 'config_parsing.R'))
source(file.path(script_dir, 'data_processing.R'))
source(file.path(script_dir, 'calculations.R'))

# --- Load tracker snapshot for 8544 products ---
cat('=== TRACKER VIEW ===\n')
snap <- readRDS(file.path(tracker_dir, 'data', 'timeseries', 'snapshot_2026_rev_4.rds'))

tracker_8544 <- snap %>%
  filter(str_starts(hts10, '8544')) %>%
  rename(hs10 = hts10, cty_code = country) %>%
  mutate(cty_code = as.character(cty_code))

cat('Tracker 8544 products:', length(unique(tracker_8544$hs10)), '\n')
cat('Tracker 8544 rows:', nrow(tracker_8544), '\n\n')

# Show rate breakdown for the biggest outlier: 8544606000, Canada
cat('--- Tracker: 8544606000 × Canada (1220) ---\n')
tracker_8544 %>%
  filter(hs10 == '8544606000', cty_code == '1220') %>%
  select(hs10, cty_code, base_rate, rate_232, rate_ieepa_recip, rate_ieepa_fent,
         rate_301, rate_s122, rate_section_201, rate_other,
         metal_share, total_additional, total_rate,
         deriv_type, steel_share, aluminum_share, copper_share,
         statutory_rate_232, statutory_rate_s122) %>%
  glimpse()

cat('\n--- Tracker: 8544606000 × China (5700) ---\n')
tracker_8544 %>%
  filter(hs10 == '8544606000', cty_code == '5700') %>%
  select(hs10, cty_code, base_rate, rate_232, rate_ieepa_recip, rate_ieepa_fent,
         rate_301, rate_s122, rate_section_201, rate_other,
         metal_share, total_additional, total_rate,
         deriv_type, steel_share, aluminum_share, copper_share,
         statutory_rate_232, statutory_rate_s122) %>%
  glimpse()

# All 8544 products with nonzero 232 in tracker
cat('\n--- Tracker: All 8544 with rate_232 > 0 (sample countries) ---\n')
tracker_8544 %>%
  filter(rate_232 > 0, cty_code == '1220') %>%
  select(hs10, rate_232, metal_share, deriv_type, copper_share,
         statutory_rate_232, total_rate) %>%
  print(n = 30)

rm(snap); gc()

# --- Load ETRs view ---
cat('\n\n=== ETRs VIEW ===\n')

# Load the statutory_rates CSV to see what ETRs starts from
config_dir <- file.path(etrs_dir, 'config', 'historical', '2026-02-20')
csv_path <- file.path(config_dir, 'statutory_rates.csv.gz')
csv <- read_csv(csv_path, col_types = cols(.default = col_character()))

cat('CSV columns:', paste(names(csv), collapse=', '), '\n\n')

csv_8544 <- csv %>%
  filter(str_starts(hts10, '8544'))

cat('CSV 8544 rows:', nrow(csv_8544), '\n')
cat('CSV 8544 products:', length(unique(csv_8544$hts10)), '\n\n')

# Show the CSV rates for 8544606000
cat('--- CSV: 8544606000 × Canada (1220) ---\n')
csv_8544 %>%
  filter(hts10 == '8544606000', cty_code == '1220') %>%
  glimpse()

cat('\n--- CSV: 8544606000 × China (5700) ---\n')
csv_8544 %>%
  filter(hts10 == '8544606000', cty_code == '5700') %>%
  glimpse()

# Check which 232 programs cover 8544
cat('\nS232 columns with nonzero values for 8544:\n')
s232_cols <- names(csv_8544)[str_starts(names(csv_8544), 's232_')]
for (col in s232_cols) {
  nonzero <- csv_8544 %>%
    filter(as.numeric(.data[[col]]) > 0)
  if (nrow(nonzero) > 0) {
    cat(sprintf('  %s: %d rows (products: %s)\n', col, nrow(nonzero),
                paste(head(unique(nonzero$hts10), 5), collapse=', ')))
  }
}

# Now load config and compute ETRs for these products
cat('\n--- Loading ETRs config ---\n')
config <- load_scenario_config(config_dir)

# Check metal content for 8544
cat('\n--- Metal content config ---\n')
cat('metal_content method:', config$other_params$metal_content$method, '\n')
cat('metal_programs:', paste(config$other_params$metal_content$metal_programs, collapse=', '), '\n')

# Load metal content shares
metal_content <- load_metal_content(
  metal_content_config = config$other_params$metal_content,
  import_data = readRDS(file.path(etrs_dir, 'cache', 'hs10_by_country_gtap_2024_con.rds'))
)

cat('\nMetal content for 8544 products:\n')
metal_content %>%
  filter(str_starts(hs10, '8544')) %>%
  print(n = 30)

# Check the s232 rate_matrix for 8544
cat('\n--- ETRs s232 rate_matrix for 8544 × Canada ---\n')
if (!is.null(config$params_s232)) {
  config$params_s232$rate_matrix %>%
    filter(str_starts(hs10, '8544'), cty_code == '1220') %>%
    head(20) %>%
    print()
}

# Run ETRs calculation and extract 8544
imports <- readRDS(file.path(etrs_dir, 'cache', 'hs10_by_country_gtap_2024_con.rds')) %>%
  filter(!str_detect(hs10, '^(98|99)'), !is.na(gtap_code))
country_mapping <- read_csv(
  file.path(etrs_dir, 'resources', 'country_partner_mapping.csv'),
  col_types = cols(.default = col_character())
)

etrs_full <- calc_etrs_for_config(config, imports, country_mapping)
etrs_8544 <- etrs_full$hs10_country_etrs %>%
  filter(str_starts(hs10, '8544'))

cat('\n--- ETRs: 8544606000 × Canada (1220) ---\n')
etrs_8544 %>%
  filter(hs10 == '8544606000', cty_code == '1220') %>%
  glimpse()

cat('\n--- ETRs: 8544606000 × China (5700) ---\n')
etrs_8544 %>%
  filter(hs10 == '8544606000', cty_code == '5700') %>%
  glimpse()

cat('\n--- ETRs: All 8544 with s232 coverage × Canada ---\n')
etrs_8544 %>%
  filter(cty_code == '1220') %>%
  select(hs10, etr, any_of(c('s232_rate', 'metal_share', 'mfn_rate',
                              's122_rate', 'final_rate'))) %>%
  filter(if_any(any_of('s232_rate'), ~ . > 0)) %>%
  print(n = 30)

cat('\nDone.\n')
