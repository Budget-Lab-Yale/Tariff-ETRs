library(tidyverse)

# Load import weights for Korea only
imports <- readRDS('cache/hs10_by_country_gtap_2024_con.rds') %>%
  as_tibble() %>%
  filter(cty_code == '5800') %>%
  group_by(hs10) %>%
  summarise(imports = sum(imports), .groups = 'drop')

# Load CBO Korea data (pre-filtered to /tmp/cbo_korea.csv)
cbo_raw <- read_csv(
  'cache/cbo_korea.csv',
  col_types = cols(I_COMMODITY = col_character(), CTY_CODE = col_character(),
                   Country = col_character(),
                   effective_tariff_rate = col_double(),
                   new_tariff_rate = col_double(),
                   total_etr = col_double())
) %>%
  rename(hs10 = I_COMMODITY)

# Join with import weights
joined <- cbo_raw %>% inner_join(imports, by = 'hs10')
total_imports <- sum(joined$imports)

cat('CBO Korea rate distribution (import-weighted):\n\n')

joined %>%
  mutate(rate_bucket = case_when(
    new_tariff_rate == 0 ~ '0% (exempt)',
    abs(new_tariff_rate - 0.10) < 0.001 ~ '10%',
    abs(new_tariff_rate - 0.125) < 0.001 ~ '12.5%',
    abs(new_tariff_rate - 0.15) < 0.001 ~ '15% (reciprocal)',
    abs(new_tariff_rate - 0.20) < 0.001 ~ '20%',
    abs(new_tariff_rate - 0.2375) < 0.001 ~ '23.75% (auto parts w/rebate)',
    abs(new_tariff_rate - 0.25) < 0.001 ~ '25% (autos)',
    abs(new_tariff_rate - 0.50) < 0.001 ~ '50% (232 metals)',
    TRUE ~ sprintf('%.2f%%', new_tariff_rate * 100)
  )) %>%
  group_by(rate_bucket) %>%
  summarise(
    n_products = n(),
    imports_bn = sum(imports) / 1e9,
    import_share = sum(imports) / total_imports * 100,
    wtd_new_rate = sum(new_tariff_rate * imports) / sum(imports) * 100,
    wtd_total_etr = sum(total_etr * imports) / sum(imports) * 100,
    .groups = 'drop'
  ) %>%
  arrange(desc(imports_bn)) %>%
  print(n = 30, width = 120)

cat(sprintf('\nTotal matched imports: $%.1fB\n', total_imports / 1e9))

# Overall weighted averages
wavg_new <- sum(joined$new_tariff_rate * joined$imports) / sum(joined$imports) * 100
wavg_total <- sum(joined$total_etr * joined$imports) / sum(joined$imports) * 100
wavg_mfn <- sum(joined$effective_tariff_rate * joined$imports) / sum(joined$imports) * 100
cat(sprintf('CBO weighted avg MFN:       %.2f%%\n', wavg_mfn))
cat(sprintf('CBO weighted avg new tariff: %.2f%%\n', wavg_new))
cat(sprintf('CBO weighted avg total ETR:  %.2f%%\n', wavg_total))
cat(sprintf('Our model Korea ETR:         13.37%%\n'))
cat(sprintf('Gap (us - CBO):              %.2f pp\n', 13.37 - wavg_total))

# Top 10 products by import value for Korea — show rates
cat('\nTop 20 Korea products by import value:\n\n')
cat(sprintf('%-12s %8s %8s %8s %8s\n', 'HS10', 'Imp($M)', 'MFN(%)', 'New(%)', 'Total(%)'))
cat(strrep('-', 50), '\n')
top <- joined %>% arrange(desc(imports)) %>% slice_head(n = 20)
for (i in seq_len(nrow(top))) {
  r <- top[i, ]
  cat(sprintf('%-12s %8.0f %8.2f %8.2f %8.2f\n',
              r$hs10, r$imports / 1e6,
              r$effective_tariff_rate * 100,
              r$new_tariff_rate * 100,
              r$total_etr * 100))
}
