#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(tidyverse)
  library(yaml)
})

args <- commandArgs(trailingOnly = TRUE)
config_dir <- if (length(args) >= 1) args[[1]] else 'config/2-21_temp/2026-01-01'

source('src/data_processing.R')
source('src/config_parsing.R')

reciprocal_file <- file.path(config_dir, 'ieepa_reciprocal.yaml')
other_params_file <- file.path(config_dir, 'other_params.yaml')
crosswalk_file <- 'resources/hs10_gtap_crosswalk.csv'

if (!file.exists(reciprocal_file)) {
  stop(sprintf('Missing file: %s', reciprocal_file))
}

if (!file.exists(other_params_file)) {
  stop(sprintf('Missing file: %s', other_params_file))
}

params <- read_yaml(other_params_file)
mfn_file <- params$mfn_rates
if (is.null(mfn_file)) {
  mfn_file <- 'resources/mfn_rates_2025.csv'
}

message(sprintf('Verifying target-total rules in %s', config_dir))
message(sprintf('Using MFN file: %s', mfn_file))

rec_rates <- load_ieepa_rates_yaml(
  reciprocal_file,
  rate_col_name = 'ieepa_reciprocal_rate',
  crosswalk_file = crosswalk_file
)

if (!'target_total_rate' %in% names(rec_rates)) {
  stop('No target_total_rate column found. Check target_total_rules in ieepa_reciprocal.yaml')
}

mfn_rates <- load_mfn_rates(mfn_file)

check_df <- rec_rates %>%
  mutate(hs8 = substr(hs10, 1, 8)) %>%
  left_join(mfn_rates, by = 'hs8') %>%
  mutate(mfn_rate = replace_na(mfn_rate, 0)) %>%
  filter(!is.na(target_total_rate)) %>%
  mutate(effective_addon = pmax(target_total_rate - mfn_rate, 0))

bad_target <- check_df %>%
  filter(mfn_rate >= target_total_rate, effective_addon > 1e-12)

if (nrow(bad_target) > 0) {
  print(bad_target %>% select(hs10, cty_code, target_total_rate, mfn_rate, effective_addon) %>% head(20))
  stop(sprintf('Target-total check failed: %d rows should be zero add-on but are non-zero', nrow(bad_target)))
}

config <- read_yaml(reciprocal_file)
mnemonic_map <- get_mnemonic_mapping()
hs10_codes <- read_csv(crosswalk_file, show_col_types = FALSE) %>%
  mutate(hs10 = as.character(hs10)) %>%
  pull(hs10) %>%
  unique()

zero_overrides <- config$product_country_rates %>%
  keep(~ !is.null(.x$rate) && .x$rate == 0)

expand_override <- function(exemption) {
  country_ids <- exemption$countries
  if (is.null(country_ids)) {
    country_ids <- exemption$country
  }
  country_ids <- as.character(unlist(country_ids))

  country_codes <- map(country_ids, function(country_id) {
    if (country_id %in% names(mnemonic_map)) {
      mnemonic_map[[country_id]]
    } else if (grepl('^[0-9]+$', country_id)) {
      country_id
    } else {
      stop(sprintf('Invalid country identifier in override %s: %s', exemption$name, country_id))
    }
  }) %>%
    unlist() %>%
    unique()

  matching_hs10 <- exemption$hts %>%
    map(~ hs10_codes[str_starts(hs10_codes, as.character(.x))]) %>%
    unlist() %>%
    unique()

  if (length(matching_hs10) == 0 || length(country_codes) == 0) {
    return(tibble(hs10 = character(), cty_code = character()))
  }

  expand_grid(hs10 = matching_hs10, cty_code = country_codes)
}

zero_override_keys <- zero_overrides %>%
  map_df(expand_override) %>%
  distinct()

conflicts <- zero_override_keys %>%
  inner_join(rec_rates %>% select(hs10, cty_code), by = c('hs10', 'cty_code'))

if (nrow(conflicts) > 0) {
  print(conflicts %>% head(20))
  stop(sprintf('Exemption check failed: %d zero-override rows still have reciprocal coverage', nrow(conflicts)))
}

summary_tbl <- check_df %>%
  summarise(
    rows_with_target_total = n(),
    countries_with_target_total = n_distinct(cty_code),
    avg_effective_addon = mean(effective_addon),
    share_zero_effective_addon = mean(effective_addon == 0)
  )

print(summary_tbl)
message('Target-total verification passed.')
