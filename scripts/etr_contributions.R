# =============================================================================
# etr_contributions.R
# =============================================================================
#
# For the 2-21_temp scenario, decompose the total census-weighted ETR into
# contributions by country (top 10 + ROW) and by HTS section, for:
#   - 2026-01-01 regime (IEEPA active,  ~14.5%)
#   - 2026-02-24 regime (S122 replaces, ~12.0%)
#
# "Contribution" = country_imports / total_imports * country_etr
# so contributions sum to the total ETR.
#
# Usage: Rscript scripts/etr_contributions.R
# =============================================================================

library(tidyverse)
library(yaml)

source('src/config_parsing.R')
source('src/data_processing.R')
source('src/calculations.R')

# ---- Macro product category definitions --------------------------------------
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

# Expand to chapter-level lookup
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

# ---- Helper: get hs10 x country ETRs for one config date --------------------
get_hs10_country_etrs <- function(config_path) {
  config <- load_scenario_config(config_path)
  etrs   <- calc_etrs_for_config(config, hs10_by_country, usmca_shares, country_mapping)
  etrs$hs10_country_etrs  # hs10, cty_code, gtap_code, imports, etr
}

# ---- Helper: decompose a df into contributions by country & product --------
decompose_contributions <- function(df) {
  # df must have: hs10, cty_code, imports, etr

  total_imports_df <- sum(df$imports)
  total_etr <- sum(df$etr * df$imports) / total_imports_df * 100

  # --- By country (top 10 by imports + everyone else as ROW) -----------------
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
    country_name = 'Rest of World'
  )

  by_country <- bind_rows(top10, rest_row) %>%
    select(country_name, import_share, etr, contribution)

  # --- By macro product category -----------------------------------------------
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

  list(
    total_etr  = total_etr,
    by_country = by_country,
    by_section = by_section
  )
}


# ---- Helper: print a contributions table ------------------------------------
print_table <- function(tbl, label_col, show_cols) {
  # label_col: name of the label column (e.g. 'country_name' or 'description')
  # show_cols: named vector of column names to display

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


# ---- Compute hs10 x country ETRs for all three dates -------------------------
message('\n--- Computing hs10 x country ETRs for IEEPA (2026-01-01) ---')
df_ieepa <- get_hs10_country_etrs('config/2-21_temp/2026-01-01')

message('\n--- Computing hs10 x country ETRs for S122 (2026-02-24) ---')
df_s122 <- get_hs10_country_etrs('config/2-21_temp/2026-02-24')

message('\n--- Computing hs10 x country ETRs for 232-only baseline (2026-07-24) ---')
df_232 <- get_hs10_country_etrs('config/2-21_temp/2026-07-24')

# ---- Total ETR decompositions (as before) -----------------------------------
all_results <- list()
for (lbl in c('ieepa', 's122')) {
  df <- if (lbl == 'ieepa') df_ieepa else df_s122
  date_label <- if (lbl == 'ieepa') '2026-01-01' else '2026-02-24'

  result <- decompose_contributions(df)
  result$date <- date_label
  all_results[[date_label]] <- result

  cat(sprintf('\n==========================================================\n'))
  cat(sprintf('  %s  |  Total Census-Weighted ETR: %.2f%%\n',
              date_label, result$total_etr))
  cat(sprintf('==========================================================\n'))

  cat('\n--- Contributions by Country (Top 10 + ROW) ---\n\n')
  print_table(result$by_country, 'country_name', NULL)

  cat('\n--- Contributions by Product Category ---\n\n')
  print_table(result$by_section, 'description', NULL)
  cat('\n')
}

# ---- Net-of-232 decompositions ----------------------------------------------
# Subtract 232-only ETR at hs10 x country level to isolate IEEPA/S122 increment
message('\n--- Computing net-of-232 contributions ---')

df_net_ieepa <- df_ieepa %>%
  left_join(df_232 %>% select(hs10, cty_code, etr_232 = etr),
            by = c('hs10', 'cty_code')) %>%
  mutate(etr = etr - etr_232) %>%
  select(-etr_232)

df_net_s122 <- df_s122 %>%
  left_join(df_232 %>% select(hs10, cty_code, etr_232 = etr),
            by = c('hs10', 'cty_code')) %>%
  mutate(etr = etr - etr_232) %>%
  select(-etr_232)

net_results <- list()
for (lbl in c('ieepa', 's122')) {
  df <- if (lbl == 'ieepa') df_net_ieepa else df_net_s122
  date_label <- if (lbl == 'ieepa') '2026-01-01' else '2026-02-24'
  regime_name <- if (lbl == 'ieepa') 'IEEPA' else 'S122'

  result <- decompose_contributions(df)
  result$date <- date_label
  net_results[[date_label]] <- result

  cat(sprintf('\n==========================================================\n'))
  cat(sprintf('  %s net of 232  |  %s increment ETR: %.2f%%\n',
              regime_name, regime_name, result$total_etr))
  cat(sprintf('==========================================================\n'))

  cat('\n--- Contributions by Country (Top 10 + ROW) ---\n\n')
  print_table(result$by_country, 'country_name', NULL)

  cat('\n--- Contributions by Product Category ---\n\n')
  print_table(result$by_section, 'description', NULL)
  cat('\n')
}


# ---- Charts ------------------------------------------------------------------
message('\nGenerating charts...')

library(gridExtra)

# ---- Helper: aggregate hs10 x country df to partner-level contributions ------
aggregate_to_partners <- function(df) {
  # Map countries to 7 partner groups: China, Canada, Mexico, UK, EU, Japan, ROW
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
      pct          = contribution / (sum(contribution)) * 100
    )
}

# ---- Helper: stacked bar chart for partner contributions -------------------
make_stacked_bar <- function(stacked_df, title, subtitle, x_label = '% of Total ETR') {
  partner_colors <- c(
    'China'  = '#e41a1c',
    'EU'     = '#377eb8',
    'Canada' = '#4daf4a',
    'Mexico' = '#984ea3',
    'Japan'  = '#ff7f00',
    'UK'     = '#a65628',
    'ROW'    = '#999999'
  )

  # Order segments: largest partners first (by IEEPA share), ROW always last
  partner_order_df <- stacked_df %>%
    filter(regime_label == 'IEEPA', partner_label != 'ROW') %>%
    arrange(desc(pct))
  partner_order <- c(partner_order_df$partner_label, 'ROW')

  stacked_df <- stacked_df %>%
    mutate(partner_label = factor(partner_label, levels = rev(partner_order)))

  ggplot(stacked_df, aes(x = regime_label, y = pct, fill = partner_label)) +
    geom_col(width = 0.6, color = 'white', linewidth = 0.3) +
    geom_text(aes(label = if_else(pct >= 3, sprintf('%.0f%%', pct), '')),
              position = position_stack(vjust = 0.5), size = 3.5, color = 'white',
              fontface = 'bold') +
    coord_flip() +
    scale_fill_manual(values = partner_colors,
                      breaks = partner_order,
                      guide = guide_legend(nrow = 1)) +
    scale_y_continuous(labels = function(x) paste0(x, '%'),
                       expand = expansion(mult = c(0, 0.02))) +
    labs(title = title, subtitle = subtitle,
         x = NULL, y = x_label, fill = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = 'top',
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = 'bold')
    )
}

# ---- Stacked bar: total ETR contributions by partner -----------------------
partner_ieepa <- aggregate_to_partners(df_ieepa) %>%
  mutate(regime_label = 'IEEPA')
partner_s122 <- aggregate_to_partners(df_s122) %>%
  mutate(regime_label = 'S122')

stacked_total <- bind_rows(partner_ieepa, partner_s122) %>%
  mutate(regime_label = factor(regime_label, levels = c('S122', 'IEEPA')))

p_stacked_total <- make_stacked_bar(
  stacked_total,
  title = 'ETR Composition by Partner',
  subtitle = sprintf('IEEPA = %.1f%%, S122 = %.1f%% (each bar sums to 100%%)',
                     sum(partner_ieepa$contribution),
                     sum(partner_s122$contribution)))

# ---- Stacked bar: net-of-232 contributions by partner ----------------------
net_partner_ieepa <- aggregate_to_partners(df_net_ieepa) %>%
  mutate(regime_label = 'IEEPA')
net_partner_s122 <- aggregate_to_partners(df_net_s122) %>%
  mutate(regime_label = 'S122')

stacked_net <- bind_rows(net_partner_ieepa, net_partner_s122) %>%
  mutate(regime_label = factor(regime_label, levels = c('S122', 'IEEPA')))

p_stacked_net <- make_stacked_bar(
  stacked_net,
  title = 'ETR Composition by Partner: Net of Section 232',
  subtitle = sprintf('IEEPA increment = %.1f pp, S122 increment = %.1f pp (each bar sums to 100%%)',
                     sum(net_partner_ieepa$contribution),
                     sum(net_partner_s122$contribution)))

# ---- Product: two-panel charts (kept as before) ----------------------------

# Helper: two-panel chart (difference | levels)
make_two_panel <- function(wide_df, long_df, title, subtitle, value_col = 'pct',
                           right_title = 'Share of Total ETR',
                           right_y = '% of Total ETR',
                           fmt = '%.1f%%') {
  p_left <- ggplot(wide_df, aes(x = label, y = gap)) +
    geom_col(fill = '#737373', width = 0.6) +
    geom_text(aes(label = sprintf('%+.1f pp', gap),
                  hjust = if_else(gap >= 0, -0.1, 1.1)),
              size = 3) +
    coord_flip() +
    scale_y_continuous(labels = function(x) paste0(x, ' pp'),
                       expand = expansion(mult = c(0.15, 0.15))) +
    geom_hline(yintercept = 0, linewidth = 0.3) +
    labs(title = 'Difference (S122 \u2212 IEEPA)', x = NULL,
         y = 'Percentage Point Difference') +
    theme_minimal(base_size = 11) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(face = 'bold', size = 11))

  p_right <- ggplot(long_df, aes(x = label, y = .data[[value_col]], fill = regime)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.65) +
    geom_text(aes(label = sprintf(fmt, .data[[value_col]])),
              position = position_dodge(width = 0.7), hjust = -0.1, size = 3) +
    coord_flip() +
    scale_fill_manual(values = c('#2166ac', '#b2182b')) +
    scale_y_continuous(labels = function(x) paste0(x, '%'),
                       expand = expansion(mult = c(0, 0.15))) +
    labs(title = right_title, x = NULL, y = right_y, fill = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position = 'top',
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.title = element_text(face = 'bold', size = 11))

  legend_grob <- cowplot::get_legend(p_right)
  p_right_no_legend <- p_right + theme(legend.position = 'none')

  title_grob <- grid::textGrob(
    title, gp = grid::gpar(fontface = 'bold', fontsize = 14),
    hjust = 0, x = 0.02)
  subtitle_grob <- grid::textGrob(
    subtitle, gp = grid::gpar(fontsize = 11),
    hjust = 0, x = 0.02)

  gridExtra::arrangeGrob(
    title_grob, subtitle_grob, legend_grob,
    gridExtra::arrangeGrob(p_left, p_right_no_legend, ncol = 2, widths = c(1, 1)),
    ncol = 1, heights = c(0.04, 0.03, 0.04, 0.89))
}

# Product: total contributions
product_wide <- all_results[['2026-01-01']]$by_section %>%
  mutate(pct_ieepa = contribution / all_results[['2026-01-01']]$total_etr * 100) %>%
  select(description, pct_ieepa) %>%
  left_join(
    all_results[['2026-02-24']]$by_section %>%
      mutate(pct_s122 = contribution / all_results[['2026-02-24']]$total_etr * 100) %>%
      select(description, pct_s122),
    by = 'description'
  ) %>%
  mutate(gap = pct_s122 - pct_ieepa, label = description)

product_order <- product_wide %>% arrange(desc(gap)) %>% pull(label)
product_wide <- product_wide %>%
  mutate(label = factor(label, levels = product_order))

product_long <- product_wide %>%
  pivot_longer(cols = c(pct_ieepa, pct_s122),
               names_to = 'regime', values_to = 'pct') %>%
  mutate(regime = factor(regime, levels = c('pct_ieepa', 'pct_s122'),
                         labels = c('IEEPA Regime', 'S122 Regime')))

p_product <- make_two_panel(
  product_wide, product_long,
  title = 'ETR Contributions by Product: IEEPA vs S122 Regime',
  subtitle = 'Share of overall census-weighted ETR (each regime sums to 100%); ranked by difference')

# Product: net-of-232 contributions
net_product_wide <- net_results[['2026-01-01']]$by_section %>%
  mutate(pct_ieepa = contribution / net_results[['2026-01-01']]$total_etr * 100) %>%
  select(description, pct_ieepa) %>%
  left_join(
    net_results[['2026-02-24']]$by_section %>%
      mutate(pct_s122 = contribution / net_results[['2026-02-24']]$total_etr * 100) %>%
      select(description, pct_s122),
    by = 'description'
  ) %>%
  mutate(gap = pct_s122 - pct_ieepa, label = description)

net_product_order <- net_product_wide %>% arrange(desc(gap)) %>% pull(label)
net_product_wide <- net_product_wide %>%
  mutate(label = factor(label, levels = net_product_order))

net_product_long <- net_product_wide %>%
  pivot_longer(cols = c(pct_ieepa, pct_s122),
               names_to = 'regime', values_to = 'pct') %>%
  mutate(regime = factor(regime, levels = c('pct_ieepa', 'pct_s122'),
                         labels = c('IEEPA Increment', 'S122 Increment')))

p_net_product <- make_two_panel(
  net_product_wide, net_product_long,
  title = 'ETR Contributions by Product: Net of Section 232',
  subtitle = sprintf(
    'IEEPA increment = %.1f pp, S122 increment = %.1f pp (each sums to 100%%); ranked by difference',
    net_results[['2026-01-01']]$total_etr, net_results[['2026-02-24']]$total_etr))

# Product: net-of-232 ETR levels
etr_product_wide <- net_results[['2026-01-01']]$by_section %>%
  select(description, etr_ieepa = etr) %>%
  left_join(
    net_results[['2026-02-24']]$by_section %>%
      select(description, etr_s122 = etr),
    by = 'description'
  ) %>%
  mutate(gap = etr_s122 - etr_ieepa, label = description)

etr_product_order <- etr_product_wide %>% arrange(desc(gap)) %>% pull(label)
etr_product_wide <- etr_product_wide %>%
  mutate(label = factor(label, levels = etr_product_order))

etr_product_long <- etr_product_wide %>%
  pivot_longer(cols = c(etr_ieepa, etr_s122),
               names_to = 'regime', values_to = 'etr') %>%
  mutate(regime = factor(regime, levels = c('etr_ieepa', 'etr_s122'),
                         labels = c('IEEPA Increment', 'S122 Increment')))

p_etr_product <- make_two_panel(
  etr_product_wide, etr_product_long,
  title = 'Net-of-232 ETR by Product: IEEPA vs S122',
  subtitle = 'Import-weighted average ETR after subtracting Section 232 baseline; ranked by difference',
  value_col = 'etr', right_title = 'ETR (Net of 232)', right_y = 'Effective Tariff Rate')

# ---- Detailed ETR level charts ----------------------------------------------

# Helper to clean country names
clean_country_name <- function(x) {
  x %>%
    str_replace(' \\(Federal Republic of Germany\\)', '') %>%
    str_replace(' \\(Republic of Korea\\)', '') %>%
    str_replace(' \\(\\d+ countries\\)', '')
}

# Helper: compute country-level ETRs (top N + ROW) from hs10 x country df
compute_country_etrs <- function(df, top_n = 20) {
  country_data <- df %>%
    group_by(cty_code) %>%
    summarise(weighted_etr = sum(etr * imports), imports = sum(imports),
              .groups = 'drop') %>%
    mutate(etr = if_else(imports > 0, weighted_etr / imports, 0) * 100) %>%
    left_join(census_codes, by = 'cty_code') %>%
    arrange(desc(imports))

  top <- country_data %>% slice_head(n = top_n)
  rest <- country_data %>% slice_tail(n = nrow(country_data) - top_n)

  rest_row <- tibble(
    cty_code = 'REST', imports = sum(rest$imports),
    weighted_etr = sum(rest$weighted_etr),
    etr = if_else(sum(rest$imports) > 0,
                  sum(rest$weighted_etr) / sum(rest$imports), 0) * 100,
    country_name = 'Rest of World')

  bind_rows(top, rest_row) %>%
    mutate(country_name = clean_country_name(country_name)) %>%
    select(country_name, etr)
}

# Country ETR levels: top 20 + ROW
etr_cty_ieepa <- compute_country_etrs(df_ieepa, 20)
etr_cty_s122  <- compute_country_etrs(df_s122, 20)

total_etr_country_wide <- etr_cty_ieepa %>%
  rename(etr_ieepa = etr) %>%
  left_join(etr_cty_s122 %>% rename(etr_s122 = etr), by = 'country_name') %>%
  mutate(gap = etr_s122 - etr_ieepa, label = country_name)

total_etr_country_order <- total_etr_country_wide %>% arrange(desc(gap)) %>% pull(label)
total_etr_country_wide <- total_etr_country_wide %>%
  mutate(label = factor(label, levels = total_etr_country_order))

total_etr_country_long <- total_etr_country_wide %>%
  pivot_longer(cols = c(etr_ieepa, etr_s122),
               names_to = 'regime', values_to = 'etr') %>%
  mutate(regime = factor(regime, levels = c('etr_ieepa', 'etr_s122'),
                         labels = c('IEEPA Regime', 'S122 Regime')))

p_total_etr_country <- make_two_panel(
  total_etr_country_wide, total_etr_country_long,
  title = 'ETR by Country: IEEPA vs S122',
  subtitle = 'Import-weighted average ETR (top 20 by imports + ROW); ranked by difference',
  value_col = 'etr', right_title = 'ETR by Regime', right_y = 'Effective Tariff Rate')

# Product ETR levels (total, using macro categories)
total_etr_product_wide <- all_results[['2026-01-01']]$by_section %>%
  select(description, etr_ieepa = etr) %>%
  left_join(
    all_results[['2026-02-24']]$by_section %>%
      select(description, etr_s122 = etr),
    by = 'description'
  ) %>%
  mutate(gap = etr_s122 - etr_ieepa, label = description)

total_etr_product_order <- total_etr_product_wide %>% arrange(desc(gap)) %>% pull(label)
total_etr_product_wide <- total_etr_product_wide %>%
  mutate(label = factor(label, levels = total_etr_product_order))

total_etr_product_long <- total_etr_product_wide %>%
  pivot_longer(cols = c(etr_ieepa, etr_s122),
               names_to = 'regime', values_to = 'etr') %>%
  mutate(regime = factor(regime, levels = c('etr_ieepa', 'etr_s122'),
                         labels = c('IEEPA Regime', 'S122 Regime')))

p_total_etr_product <- make_two_panel(
  total_etr_product_wide, total_etr_product_long,
  title = 'ETR by Product: IEEPA vs S122',
  subtitle = 'Import-weighted average ETR; ranked by difference',
  value_col = 'etr', right_title = 'ETR by Regime', right_y = 'Effective Tariff Rate')

# ---- Save all charts --------------------------------------------------------
output_dir <- 'output/2-21_temp'
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

ggsave(file.path(output_dir, 'etr_contributions_by_country.png'),
       p_stacked_total, width = 10, height = 4, dpi = 200, bg = 'white')
message(sprintf('Saved %s/etr_contributions_by_country.png', output_dir))

ggsave(file.path(output_dir, 'etr_contributions_by_country_net232.png'),
       p_stacked_net, width = 10, height = 4, dpi = 200, bg = 'white')
message(sprintf('Saved %s/etr_contributions_by_country_net232.png', output_dir))

ggsave(file.path(output_dir, 'etr_contributions_by_product.png'),
       p_product, width = 14, height = 6, dpi = 200, bg = 'white')
message(sprintf('Saved %s/etr_contributions_by_product.png', output_dir))

ggsave(file.path(output_dir, 'etr_contributions_by_product_net232.png'),
       p_net_product, width = 14, height = 6, dpi = 200, bg = 'white')
message(sprintf('Saved %s/etr_contributions_by_product_net232.png', output_dir))

ggsave(file.path(output_dir, 'etr_levels_by_product_net232.png'),
       p_etr_product, width = 14, height = 6, dpi = 200, bg = 'white')
message(sprintf('Saved %s/etr_levels_by_product_net232.png', output_dir))

ggsave(file.path(output_dir, 'etr_levels_by_country.png'),
       p_total_etr_country, width = 14, height = 14, dpi = 200, bg = 'white')
message(sprintf('Saved %s/etr_levels_by_country.png', output_dir))

ggsave(file.path(output_dir, 'etr_levels_by_product.png'),
       p_total_etr_product, width = 14, height = 6, dpi = 200, bg = 'white')
message(sprintf('Saved %s/etr_levels_by_product.png', output_dir))
