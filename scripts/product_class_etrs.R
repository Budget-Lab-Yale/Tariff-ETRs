library(tidyverse)

# GTAP sector -> product class mapping
sector_groups <- tribble(
  ~gtap_code, ~product_class,
  'pdr', 'Agriculture',
  'wht', 'Agriculture',
  'gro', 'Agriculture',
  'v_f', 'Agriculture',
  'osd', 'Agriculture',
  'c_b', 'Agriculture',
  'pfb', 'Agriculture',
  'ocr', 'Agriculture',
  'ctl', 'Agriculture',
  'oap', 'Agriculture',
  'rmk', 'Agriculture',
  'wol', 'Agriculture',
  'frs', 'Agriculture',
  'fsh', 'Agriculture',
  'cmt', 'Food Products',
  'omt', 'Food Products',
  'vol', 'Food Products',
  'mil', 'Food Products',
  'pcr', 'Food Products',
  'sgr', 'Food Products',
  'ofd', 'Food Products',
  'b_t', 'Food Products',
  'coa', 'Energy & Mining',
  'oil', 'Energy & Mining',
  'gas', 'Energy & Mining',
  'oxt', 'Energy & Mining',
  'p_c', 'Energy & Mining',
  'tex', 'Light Manufacturing',
  'wap', 'Light Manufacturing',
  'lea', 'Light Manufacturing',
  'lum', 'Light Manufacturing',
  'ppp', 'Light Manufacturing',
  'omf', 'Light Manufacturing',
  'chm', 'Chemicals & Pharma',
  'bph', 'Chemicals & Pharma',
  'rpp', 'Chemicals & Pharma',
  'nmm', 'Metals & Minerals',
  'i_s', 'Metals & Minerals',
  'nfm', 'Metals & Minerals',
  'fmp', 'Metals & Minerals',
  'ele', 'Electronics & Electrical',
  'eeq', 'Electronics & Electrical',
  'ome', 'Machinery',
  'mvh', 'Motor Vehicles',
  'otn', 'Other Transport',
  'ely', 'Utilities',
  'gdt', 'Utilities',
  'wtr', 'Utilities',
  'cns', 'Utilities'
)

# Read sector x country ETRs for each scenario (in pp)
read_etrs <- function(scenario) {
  read_csv(sprintf('output/%s/etrs_by_sector_country.csv', scenario), show_col_types = FALSE)
}

# Build census weights from cached import data + country mapping
country_mapping <- read_csv('resources/country_partner_mapping.csv',
  col_types = cols(cty_code = col_character()), show_col_types = FALSE)

hs10_by_country <- readRDS('cache/hs10_by_country_gtap_2024_con.rds') %>%
  filter(!str_detect(hs10, '^(98|99)'), !is.na(gtap_code))

weights <- hs10_by_country %>%
  left_join(country_mapping %>% select(cty_code, partner) %>% distinct(), by = 'cty_code') %>%
  mutate(partner = if_else(is.na(partner), 'row', partner)) %>%
  group_by(gtap_code, partner) %>%
  summarise(weight = sum(imports), .groups = 'drop') %>%
  filter(weight > 0)

scenarios <- c('2-20_with-ieepa', '2-20', '2-20_with-s122-preempt')
labels <- c('Before SCOTUS', 'After SCOTUS', 'With Section 122')

results <- map2_df(scenarios, labels, function(scen, lab) {
  etrs <- read_etrs(scen) %>%
    pivot_longer(-gtap_code, names_to = 'partner', values_to = 'etr') %>%
    inner_join(weights, by = c('gtap_code', 'partner')) %>%
    inner_join(sector_groups, by = 'gtap_code') %>%
    group_by(product_class) %>%
    summarise(etr = sum(etr * weight) / sum(weight), .groups = 'drop') %>%
    mutate(scenario = lab)
})

totals <- map2_df(scenarios, labels, function(scen, lab) {
  etrs <- read_etrs(scen) %>%
    pivot_longer(-gtap_code, names_to = 'partner', values_to = 'etr') %>%
    inner_join(weights, by = c('gtap_code', 'partner'))
  tibble(product_class = 'TOTAL', etr = sum(etrs$etr * etrs$weight) / sum(etrs$weight), scenario = lab)
})

all_results <- bind_rows(totals, results) %>%
  pivot_wider(names_from = scenario, values_from = etr)

cat(sprintf('%-25s  %15s  %14s  %18s\n', 'Product Class', 'Before SCOTUS', 'After SCOTUS', 'With Section 122'))
cat(sprintf('%-25s  %15s  %14s  %18s\n', '-------------', '-------------', '------------', '----------------'))
for (i in 1:nrow(all_results)) {
  cat(sprintf('%-25s  %14.2f%%  %13.2f%%  %17.2f%%\n',
    all_results$product_class[i],
    all_results[['Before SCOTUS']][i],
    all_results[['After SCOTUS']][i],
    all_results[['With Section 122']][i]))
}
