# =============================================================================
# gta_report.R
# =============================================================================
#
# Recreate the Global Trade Alert "Section 122 in effect" analysis.
# Compares three tariff regimes:
#   1. Pre-ruling IEEPA    (2026-01-01 config)
#   2. Post-strikedown 232  (2026-07-24 config, IEEPA struck down)
#   3. S122 at 10%          (2026-02-24 config)
#
# Key outputs:
#   - Overall trade-weighted ETR for each regime
#   - Cleveland dot plot: top 20 import sources by ETR under each regime
#   - Product category comparison: macro categories by regime
#   - Contribution decomposition tables
#
# Usage: Rscript scripts/gta_report.R
# =============================================================================

library(tidyverse)
library(yaml)

source('src/config_parsing.R')
source('src/data_processing.R')
source('src/calculations.R')

# ---- Config paths (using 2-21_temp which has all 3 dates) -------------------
scenario <- '2-21_temp'
config_paths <- list(
  ieepa    = sprintf('config/%s/2026-01-01', scenario),
  s122     = sprintf('config/%s/2026-02-24', scenario),
  baseline = sprintf('config/%s/2026-07-24', scenario)
)

regime_labels <- c(
  ieepa    = 'Pre-Ruling (IEEPA)',
  baseline = '232-Only (Post-Strikedown)',
  s122     = 'S122 at 10%'
)

regime_colors <- c(
  'Pre-Ruling (IEEPA)'          = '#b2182b',
  '232-Only (Post-Strikedown)'   = '#2166ac',
  'S122 at 10%'                  = '#1b7837'
)

# ---- Macro product category definitions ------------------------------------
macro_categories <- tribble(
  ~macro, ~chapters,                                          ~description,
  '1',    sprintf('%02d', c(1:5, 6:14, 15, 16:24)),          'Agriculture & Food',
  '2',    sprintf('%02d', 25:27),                             'Energy & Minerals',
  '3',    sprintf('%02d', c(28:38, 39:40)),                   'Chemicals & Plastics',
  '4',    c('71', sprintf('%02d', 72:83)),                    'Metals',
  '5',    sprintf('%02d', c(50:63, 64:67)),                   'Textiles & Apparel',
  '6',    sprintf('%02d', 84:85),                             'Machinery & Electronics',
  '7',    sprintf('%02d', 86:89),                             'Vehicles & Transport',
  '8',    sprintf('%02d', c(41:43, 44:46, 47:49, 68:70,
                            90:92, 93, 94:96, 97)),           'Other Manufactured'
)

chapter_to_macro <- macro_categories %>%
  unnest(chapters) %>%
  select(hts2 = chapters, macro, description)

# ---- Load shared data -------------------------------------------------------
message('Loading shared data...')
usmca_shares    <- read_csv('resources/usmca_shares.csv', show_col_types = FALSE)
country_mapping <- read_csv('resources/country_partner_mapping.csv',
                            col_types = cols(cty_code = col_character()),
                            show_col_types = FALSE)
census_codes    <- read_csv('resources/census_codes.csv',
                            col_types = cols(Code = col_character()),
                            show_col_types = FALSE) %>%
  rename(cty_code = Code, country_name = Name)

hs10_by_country <- readRDS('cache/hs10_by_country_gtap_2024_con.rds') %>%
  filter(!str_detect(hs10, '^(98|99)'), !is.na(gtap_code))

# ---- Helper: get hs10 x country ETRs for one config -------------------------
get_hs10_country_etrs <- function(config_path) {
  config <- load_scenario_config(config_path)
  etrs   <- calc_etrs_for_config(config, hs10_by_country, usmca_shares, country_mapping)
  etrs$hs10_country_etrs
}

# ---- Compute ETRs for all three regimes ------------------------------------
etrs <- list()
for (regime in names(config_paths)) {
  message(sprintf('\n--- Computing ETRs: %s ---', regime_labels[regime]))
  etrs[[regime]] <- get_hs10_country_etrs(config_paths[[regime]])
}

# ---- Overall ETR by regime --------------------------------------------------
overall <- tibble(
  regime = names(regime_labels),
  label  = regime_labels,
  total_etr = map_dbl(regime, function(r) {
    df <- etrs[[r]]
    sum(df$etr * df$imports) / sum(df$imports) * 100
  })
)

cat('\n===================================================================\n')
cat('  Overall Trade-Weighted ETR by Regime\n')
cat('===================================================================\n\n')
for (i in seq_len(nrow(overall))) {
  cat(sprintf('  %-35s  %6.2f%%\n', overall$label[i], overall$total_etr[i]))
}
cat('\n')

# ---- Country-level aggregation for each regime ------------------------------
clean_country_name <- function(x) {
  x %>%
    str_replace(' \\(Federal Republic of Germany\\)', '') %>%
    str_replace(' \\(Republic of Korea\\)', '') %>%
    str_replace(' \\(People.s Republic\\)', '') %>%
    str_replace(', S\\.A\\.R\\.', '')
}

country_by_regime <- map_dfr(names(regime_labels), function(r) {
  df <- etrs[[r]]
  df %>%
    group_by(cty_code) %>%
    summarise(
      weighted_etr = sum(etr * imports),
      imports      = sum(imports),
      .groups = 'drop'
    ) %>%
    mutate(
      etr          = if_else(imports > 0, weighted_etr / imports, 0) * 100,
      regime       = regime_labels[r]
    )
}) %>%
  left_join(census_codes, by = 'cty_code') %>%
  mutate(country_name = clean_country_name(country_name))

# Top 20 by imports (across all regimes, pick the IEEPA ranking)
top20_codes <- country_by_regime %>%
  filter(regime == regime_labels['ieepa']) %>%
  arrange(desc(imports)) %>%
  slice_head(n = 20) %>%
  pull(cty_code)

top20_data <- country_by_regime %>%
  filter(cty_code %in% top20_codes) %>%
  mutate(regime = factor(regime, levels = regime_labels))

# Order countries by S122 ETR (descending)
country_order <- top20_data %>%
  filter(regime == regime_labels['s122']) %>%
  arrange(etr) %>%
  pull(country_name)

top20_data <- top20_data %>%
  mutate(country_name = factor(country_name, levels = country_order))

# ---- Chart 1: Cleveland dot plot -- ETR by country and regime ----------------
message('\nGenerating charts...')

p_country_dots <- ggplot(top20_data, aes(x = etr, y = country_name, color = regime)) +
  geom_point(size = 3.5, alpha = 0.85) +
  scale_color_manual(values = regime_colors, guide = guide_legend(nrow = 1)) +
  scale_x_continuous(labels = function(x) paste0(x, '%'),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = 'Effective Tariff Rate by Import Source',
    subtitle = sprintf(
      'Top 20 US import sources | IEEPA = %.1f%%, 232-Only = %.1f%%, S122 = %.1f%%',
      overall$total_etr[overall$regime == 'ieepa'],
      overall$total_etr[overall$regime == 'baseline'],
      overall$total_etr[overall$regime == 's122']
    ),
    x     = 'Effective Tariff Rate',
    y     = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position  = 'top',
    panel.grid.major.y = element_line(color = '#eeeeee'),
    panel.grid.minor   = element_blank(),
    plot.title         = element_text(face = 'bold')
  )

# ---- Product-level aggregation for each regime ------------------------------
product_by_regime <- map_dfr(names(regime_labels), function(r) {
  df <- etrs[[r]]
  df %>%
    mutate(hts2 = str_sub(hs10, 1, 2)) %>%
    left_join(chapter_to_macro, by = 'hts2') %>%
    group_by(description) %>%
    summarise(
      weighted_etr = sum(etr * imports),
      imports      = sum(imports),
      .groups = 'drop'
    ) %>%
    mutate(
      etr    = if_else(imports > 0, weighted_etr / imports, 0) * 100,
      regime = regime_labels[r]
    )
}) %>%
  mutate(regime = factor(regime, levels = regime_labels))

# Order by S122 ETR
product_order <- product_by_regime %>%
  filter(regime == regime_labels['s122']) %>%
  arrange(etr) %>%
  pull(description)

product_by_regime <- product_by_regime %>%
  mutate(description = factor(description, levels = product_order))

# ---- Chart 2: Grouped bar -- ETR by product and regime ----------------------
p_product_bars <- ggplot(product_by_regime,
                         aes(x = etr, y = description, fill = regime)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  geom_text(aes(label = sprintf('%.1f%%', etr)),
            position = position_dodge(width = 0.7), hjust = -0.1, size = 3) +
  scale_fill_manual(values = regime_colors, guide = guide_legend(nrow = 1)) +
  scale_x_continuous(labels = function(x) paste0(x, '%'),
                     expand = expansion(mult = c(0, 0.2))) +
  labs(
    title    = 'Effective Tariff Rate by Product Category',
    subtitle = 'Import-weighted average ETR by macro product group',
    x     = 'Effective Tariff Rate',
    y     = NULL,
    fill  = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position    = 'top',
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.title         = element_text(face = 'bold')
  )

# ---- Chart 3: Stacked bar -- ETR contributions by partner --------------------
aggregate_to_partners <- function(df) {
  partner_map <- country_mapping %>%
    mutate(partner_label = case_when(
      partner == 'china'  ~ 'China',
      partner == 'canada' ~ 'Canada',
      partner == 'mexico' ~ 'Mexico',
      partner == 'uk'     ~ 'UK',
      partner == 'eu'     ~ 'EU',
      partner == 'japan'  ~ 'Japan',
      TRUE                ~ 'ROW'
    )) %>%
    select(cty_code, partner_label)

  total_imports <- sum(df$imports)

  df %>%
    left_join(partner_map, by = 'cty_code') %>%
    mutate(partner_label = if_else(is.na(partner_label), 'ROW', partner_label)) %>%
    group_by(partner_label) %>%
    summarise(
      weighted_etr = sum(etr * imports),
      imports      = sum(imports),
      .groups = 'drop'
    ) %>%
    mutate(
      etr          = if_else(imports > 0, weighted_etr / imports, 0) * 100,
      contribution = weighted_etr / total_imports * 100,
      pct          = contribution / sum(contribution) * 100
    )
}

partner_colors <- c(
  'China'  = '#e41a1c',
  'EU'     = '#377eb8',
  'Canada' = '#4daf4a',
  'Mexico' = '#984ea3',
  'Japan'  = '#ff7f00',
  'UK'     = '#a65628',
  'ROW'    = '#999999'
)

stacked_all <- map_dfr(names(regime_labels), function(r) {
  aggregate_to_partners(etrs[[r]]) %>%
    mutate(regime_label = regime_labels[r])
}) %>%
  mutate(regime_label = factor(regime_label, levels = rev(regime_labels)))

# Partner order: largest first by IEEPA pct, ROW last
partner_order_df <- stacked_all %>%
  filter(regime_label == regime_labels['ieepa'], partner_label != 'ROW') %>%
  arrange(desc(pct))
partner_order <- c(partner_order_df$partner_label, 'ROW')

stacked_all <- stacked_all %>%
  mutate(partner_label = factor(partner_label, levels = rev(partner_order)))

p_stacked <- ggplot(stacked_all, aes(x = regime_label, y = pct, fill = partner_label)) +
  geom_col(width = 0.6, color = 'white', linewidth = 0.3) +
  geom_text(aes(label = if_else(pct >= 3, sprintf('%.0f%%', pct), '')),
            position = position_stack(vjust = 0.5), size = 3.5, color = 'white',
            fontface = 'bold') +
  coord_flip() +
  scale_fill_manual(values = partner_colors,
                    breaks = partner_order,
                    guide  = guide_legend(nrow = 1)) +
  scale_y_continuous(labels = function(x) paste0(x, '%'),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(
    title    = 'ETR Composition by Partner',
    subtitle = sprintf(
      'IEEPA = %.1f%%, 232-Only = %.1f%%, S122 = %.1f%% (each bar sums to 100%%)',
      overall$total_etr[overall$regime == 'ieepa'],
      overall$total_etr[overall$regime == 'baseline'],
      overall$total_etr[overall$regime == 's122']
    ),
    x = NULL, y = '% of Total ETR', fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position    = 'top',
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.title         = element_text(face = 'bold')
  )

# ---- Chart 4: Change from IEEPA to S122 by country (lollipop) ---------------
change_data <- top20_data %>%
  select(country_name, cty_code, regime, etr) %>%
  pivot_wider(names_from = regime, values_from = etr) %>%
  rename(
    etr_ieepa    = `Pre-Ruling (IEEPA)`,
    etr_baseline = `232-Only (Post-Strikedown)`,
    etr_s122     = `S122 at 10%`
  ) %>%
  mutate(
    change = etr_s122 - etr_ieepa,
    direction = if_else(change >= 0, 'higher', 'lower')
  )

change_order <- change_data %>% arrange(change) %>% pull(country_name)
change_data <- change_data %>%
  mutate(country_name = factor(country_name, levels = change_order))

p_change <- ggplot(change_data, aes(x = change, y = country_name, fill = direction)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = sprintf('%+.1f pp', change),
                hjust = if_else(change >= 0, -0.1, 1.1)),
            size = 3) +
  geom_vline(xintercept = 0, linewidth = 0.3) +
  scale_fill_manual(values = c(higher = '#b2182b', lower = '#2166ac')) +
  scale_x_continuous(labels = function(x) paste0(x, ' pp'),
                     expand = expansion(mult = c(0.15, 0.15))) +
  labs(
    title    = 'Change in ETR: S122 at 10% vs Pre-Ruling IEEPA',
    subtitle = 'Percentage point difference (top 20 import sources)',
    x = 'Percentage Point Change', y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.title         = element_text(face = 'bold')
  )

# ---- Contribution tables (text) ---------------------------------------------
decompose_contributions <- function(df) {
  total_imports_df <- sum(df$imports)
  total_etr <- sum(df$etr * df$imports) / total_imports_df * 100

  # By country (top 10 + ROW)
  country_data <- df %>%
    group_by(cty_code) %>%
    summarise(
      weighted_etr = sum(etr * imports),
      imports      = sum(imports),
      .groups = 'drop'
    ) %>%
    mutate(
      etr          = if_else(imports > 0, weighted_etr / imports, 0) * 100,
      contribution = weighted_etr / total_imports_df * 100,
      import_share = imports / total_imports_df * 100
    ) %>%
    left_join(census_codes, by = 'cty_code') %>%
    mutate(country_name = clean_country_name(country_name)) %>%
    arrange(desc(imports))

  top10 <- country_data %>% slice_head(n = 10)
  rest  <- country_data %>% slice_tail(n = nrow(country_data) - 10)

  rest_row <- tibble(
    cty_code     = 'REST',
    imports      = sum(rest$imports),
    weighted_etr = sum(rest$weighted_etr),
    etr          = if_else(sum(rest$imports) > 0,
                           sum(rest$weighted_etr) / sum(rest$imports), 0) * 100,
    contribution = sum(rest$contribution),
    import_share = sum(rest$import_share),
    country_name = sprintf('Rest of World (%d countries)', nrow(rest))
  )

  by_country <- bind_rows(top10, rest_row) %>%
    select(country_name, import_share, etr, contribution)

  # By product category
  by_section <- df %>%
    mutate(hts2 = str_sub(hs10, 1, 2)) %>%
    left_join(chapter_to_macro, by = 'hts2') %>%
    group_by(macro, description) %>%
    summarise(
      weighted_etr = sum(etr * imports),
      imports      = sum(imports),
      .groups = 'drop'
    ) %>%
    mutate(
      etr          = if_else(imports > 0, weighted_etr / imports, 0) * 100,
      contribution = weighted_etr / total_imports_df * 100,
      import_share = imports / total_imports_df * 100
    ) %>%
    arrange(desc(contribution)) %>%
    select(description, import_share, etr, contribution)

  list(total_etr = total_etr, by_country = by_country, by_section = by_section)
}

print_table <- function(tbl, label_col) {
  header <- sprintf('%-40s %12s %10s %14s',
                    'Category', 'Import Sh.', 'ETR', 'Contribution')
  cat(header, '\n')
  cat(strrep('-', nchar(header)), '\n')

  for (i in seq_len(nrow(tbl))) {
    cat(sprintf('%-40s %11.1f%% %9.1f%% %13.2f pp\n',
                str_trunc(tbl[[label_col]][i], 40),
                tbl$import_share[i],
                tbl$etr[i],
                tbl$contribution[i]))
  }

  cat(sprintf('%-40s %11.1f%% %9s  %13.2f pp\n',
              'TOTAL', sum(tbl$import_share), '',
              sum(tbl$contribution)))
}

for (r in names(regime_labels)) {
  result <- decompose_contributions(etrs[[r]])

  cat(sprintf('\n==========================================================\n'))
  cat(sprintf('  %s  |  Total ETR: %.2f%%\n', regime_labels[r], result$total_etr))
  cat(sprintf('==========================================================\n'))

  cat('\n--- Contributions by Country (Top 10 + ROW) ---\n\n')
  print_table(result$by_country, 'country_name')

  cat('\n--- Contributions by Product Category ---\n\n')
  print_table(result$by_section, 'description')
  cat('\n')
}

# ---- Save charts ------------------------------------------------------------
output_dir <- sprintf('output/%s', scenario)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

ggsave(file.path(output_dir, 'gta_country_dots.png'),
       p_country_dots, width = 12, height = 10, dpi = 200, bg = 'white')
message(sprintf('Saved %s/gta_country_dots.png', output_dir))

ggsave(file.path(output_dir, 'gta_product_bars.png'),
       p_product_bars, width = 12, height = 6, dpi = 200, bg = 'white')
message(sprintf('Saved %s/gta_product_bars.png', output_dir))

ggsave(file.path(output_dir, 'gta_stacked_partners.png'),
       p_stacked, width = 12, height = 5, dpi = 200, bg = 'white')
message(sprintf('Saved %s/gta_stacked_partners.png', output_dir))

ggsave(file.path(output_dir, 'gta_change_ieepa_to_s122.png'),
       p_change, width = 12, height = 10, dpi = 200, bg = 'white')
message(sprintf('Saved %s/gta_change_ieepa_to_s122.png', output_dir))

message('\nDone!')
