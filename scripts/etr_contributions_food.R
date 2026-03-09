# =============================================================================
# etr_contributions_food.R
# =============================================================================
#
# Food-specific version of etr_contributions.R. Filters to HTS chapters 01-24
# (Agriculture & Food) and breaks down by food sub-category. The "overall" row
# is the food-total ETR rather than all-imports.
#
# Outputs:
#   - Console tables: food sub-category ETRs, country ETRs (food only)
#   - Charts: output/2-21_temp/etr_levels_by_food_product.png
#             output/2-21_temp/etr_levels_by_food_country.png
#
# Usage: Rscript scripts/etr_contributions_food.R
# =============================================================================

library(tidyverse)
library(yaml)
library(gridExtra)
library(ggtext)

source('src/config_parsing.R')
source('src/data_processing.R')
source('src/calculations.R')

# ---- Food sub-category definitions ------------------------------------------
food_categories <- tribble(
  ~food_cat, ~chapters,                               ~description,
  '1',       sprintf('%02d', 1:2),                     'Live Animals & Meat',
  '2',       c('03'),                                  'Fish & Seafood',
  '3',       c('04', '05'),                            'Dairy, Eggs & Animal Products',
  '4',       sprintf('%02d', 6:8),                     'Plants, Vegetables & Fruit',
  '5',       sprintf('%02d', 9:14),                    'Coffee, Tea, Cereals & Seeds',
  '6',       c('15'),                                  'Fats & Oils',
  '7',       sprintf('%02d', 16:21),                   'Prepared Foods & Beverages',
  '8',       c('22'),                                  'Beverages & Spirits'
)

chapter_to_food <- food_categories %>%
  unnest(chapters) %>%
  select(hts2 = chapters, food_cat, description)

food_chapters <- chapter_to_food$hts2

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

# ---- Helper: filter to food only --------------------------------------------
filter_food <- function(df) {
  df %>%
    mutate(hts2 = str_sub(hs10, 1, 2)) %>%
    filter(hts2 %in% food_chapters)
}

# ---- Helper: clean country names ---------------------------------------------
clean_country_name <- function(x) {
  x %>%
    str_replace(' \\(Federal Republic of Germany\\)', '') %>%
    str_replace(' \\(Republic of Korea\\)', '') %>%
    str_replace(' \\(\\d+ countries\\)', '')
}

# ---- Helper: compute partner-level ETRs (top N + ROW) -----------------------
compute_country_etrs <- function(df, top_n = 10) {
  partner_map <- country_mapping %>%
    mutate(partner_label = case_when(
      partner == 'china'  ~ 'China',
      partner == 'canada' ~ 'Canada',
      partner == 'mexico' ~ 'Mexico',
      partner == 'uk'     ~ 'United Kingdom',
      partner == 'eu'     ~ 'European Union',
      partner == 'japan'  ~ 'Japan',
      partner == 'ftrow'  ~ NA_character_,
      TRUE                ~ NA_character_
    )) %>%
    select(cty_code, partner_label)

  partner_data <- df %>%
    left_join(partner_map, by = 'cty_code') %>%
    left_join(census_codes, by = 'cty_code') %>%
    mutate(partner_label = if_else(
      is.na(partner_label),
      clean_country_name(country_name),
      partner_label
    )) %>%
    group_by(partner_label) %>%
    summarise(weighted_etr = sum(etr * imports), imports = sum(imports),
              .groups = 'drop') %>%
    mutate(etr = if_else(imports > 0, weighted_etr / imports, 0) * 100) %>%
    arrange(desc(imports))

  top <- partner_data %>% slice_head(n = top_n)
  rest <- partner_data %>% slice_tail(n = nrow(partner_data) - top_n)

  rest_row <- tibble(
    partner_label = 'Rest of World',
    imports = sum(rest$imports),
    weighted_etr = sum(rest$weighted_etr),
    etr = if_else(sum(rest$imports) > 0,
                  sum(rest$weighted_etr) / sum(rest$imports), 0) * 100)

  bind_rows(top, rest_row) %>%
    select(country_name = partner_label, etr)
}

# ---- Helper: compute food sub-category ETRs ---------------------------------
compute_food_etrs <- function(df) {
  df %>%
    left_join(chapter_to_food, by = 'hts2') %>%
    group_by(food_cat, description) %>%
    summarise(weighted_etr = sum(etr * imports), imports = sum(imports),
              .groups = 'drop') %>%
    mutate(etr = if_else(imports > 0, weighted_etr / imports, 0) * 100) %>%
    select(description, etr)
}

# ---- Helper: two-panel chart (difference | levels) --------------------------
make_two_panel <- function(wide_df, long_df, title, subtitle) {
  is_overall <- str_detect(as.character(wide_df$label), '^\\*\\*')

  p_left <- ggplot(wide_df, aes(x = label, y = gap)) +
    geom_col(aes(fill = is_overall), width = 0.6, show.legend = FALSE) +
    scale_fill_manual(values = c('TRUE' = '#252525', 'FALSE' = '#737373')) +
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
          plot.title = element_text(face = 'bold', size = 11),
          axis.text.y = element_markdown())

  p_right <- ggplot(long_df, aes(x = label, y = etr, fill = regime)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.75) +
    geom_text(aes(label = sprintf('%.1f%%', etr)),
              position = position_dodge(width = 0.8), hjust = -0.1, size = 2.5) +
    coord_flip() +
    scale_fill_manual(values = c('#2166ac', '#b2182b', '#4daf4a')) +
    scale_y_continuous(labels = function(x) paste0(x, '%'),
                       expand = expansion(mult = c(0, 0.18))) +
    labs(title = 'ETR by Regime', x = NULL,
         y = 'Effective Tariff Rate', fill = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position = 'top',
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.title = element_text(face = 'bold', size = 11))

  # Extract legend as a standalone grob via ggplotGrob
  p_legend <- p_right +
    guides(fill = guide_legend(nrow = 1)) +
    theme(legend.position = 'top')
  g_table <- ggplotGrob(p_legend)
  legend_grob <- g_table$grobs[[
    which(sapply(g_table$grobs, function(g) !is.null(g$name) && g$name == 'guide-box'))
  ]]

  p_right <- p_right + theme(legend.position = 'none')

  title_grob <- grid::textGrob(
    title, gp = grid::gpar(fontface = 'bold', fontsize = 14),
    hjust = 0, x = 0.02)
  subtitle_grob <- grid::textGrob(
    subtitle, gp = grid::gpar(fontsize = 11),
    hjust = 0, x = 0.02)

  gridExtra::arrangeGrob(
    title_grob, subtitle_grob, legend_grob,
    gridExtra::arrangeGrob(p_left, p_right, ncol = 2, widths = c(1, 1)),
    ncol = 1, heights = c(0.04, 0.03, 0.04, 0.89))
}

# ---- Helper: build wide/long data for two-panel chart -----------------------
prepare_chart_data <- function(etrs_ieepa, etrs_s122, etrs_jul, label_col,
                               overall_ieepa = NULL, overall_s122 = NULL,
                               overall_jul = NULL) {
  wide <- etrs_ieepa %>%
    rename(etr_ieepa = etr) %>%
    left_join(etrs_s122 %>% rename(etr_s122 = etr), by = label_col) %>%
    left_join(etrs_jul %>% rename(etr_jul = etr), by = label_col) %>%
    mutate(gap = etr_s122 - etr_ieepa, label = .data[[label_col]])

  # Append overall row if provided
  if (!is.null(overall_ieepa)) {
    overall_row <- tibble(etr_ieepa = overall_ieepa, etr_s122 = overall_s122,
                          etr_jul = overall_jul,
                          gap = overall_s122 - overall_ieepa, label = 'All Food')
    wide <- bind_rows(wide, overall_row)
  }

  # Bold the overall label for ggtext rendering
  wide <- wide %>%
    mutate(label = if_else(label == 'All Food', '**All Food**', label))

  # Order: ranked by gap, but overall always at bottom (= top of flipped chart)
  category_order <- wide %>% filter(label != '**All Food**') %>%
    arrange(desc(gap)) %>% pull(label)
  label_order <- c(category_order, if ('**All Food**' %in% wide$label) '**All Food**')
  wide <- wide %>% mutate(label = factor(label, levels = label_order))

  long <- wide %>%
    pivot_longer(cols = c(etr_ieepa, etr_s122, etr_jul),
                 names_to = 'regime', values_to = 'etr') %>%
    mutate(regime = factor(regime, levels = c('etr_ieepa', 'etr_s122', 'etr_jul'),
                           labels = c('Pre-SCOTUS (IEEPA regime)',
                                      'Post-SCOTUS (122 regime)',
                                      'Post-SCOTUS, no 122')))

  list(wide = wide, long = long)
}

# ---- Helper: print comparison table ------------------------------------------
print_comparison <- function(wide_df, label_col) {
  header <- sprintf('%-40s %12s %12s %12s', 'Category', 'IEEPA ETR', 'S122 ETR', 'Difference')
  cat(header, '\n')
  cat(strrep('-', nchar(header)), '\n')
  for (i in seq_len(nrow(wide_df))) {
    cat(sprintf('%-40s %11.1f%% %11.1f%% %+11.1f pp\n',
                str_trunc(wide_df[[label_col]][i], 40),
                wide_df$etr_ieepa[i],
                wide_df$etr_s122[i],
                wide_df$gap[i]))
  }
}

# ---- Compute ETRs for all three regimes -------------------------------------
message('\n--- Computing ETRs for IEEPA regime (2026-01-01) ---')
df_ieepa <- get_hs10_country_etrs('config/2-21_temp/2026-01-01')

message('\n--- Computing ETRs for S122 regime (2026-02-24) ---')
df_s122 <- get_hs10_country_etrs('config/2-21_temp/2026-02-24')

message('\n--- Computing ETRs for Jul 24 regime (2026-07-24) ---')
df_jul <- get_hs10_country_etrs('config/2-21_temp/2026-07-24')

# ---- Filter to food only ----------------------------------------------------
df_ieepa_food <- filter_food(df_ieepa)
df_s122_food  <- filter_food(df_s122)
df_jul_food   <- filter_food(df_jul)

# ---- Food overall summary ---------------------------------------------------
food_imports <- sum(df_ieepa_food$imports)
etr_ieepa_food <- sum(df_ieepa_food$etr * df_ieepa_food$imports) / food_imports * 100
etr_s122_food  <- sum(df_s122_food$etr * df_s122_food$imports)   / food_imports * 100
etr_jul_food   <- sum(df_jul_food$etr * df_jul_food$imports)     / food_imports * 100

cat(sprintf('\n==========================================================\n'))
cat(sprintf('  Food Import-Weighted ETR (HTS Chapters 01-22)\n'))
cat(sprintf('  IEEPA   (2026-01-01):  %5.2f%%\n', etr_ieepa_food))
cat(sprintf('  S122    (2026-02-24):  %5.2f%%\n', etr_s122_food))
cat(sprintf('  Jul 24  (2026-07-24):  %5.2f%%\n', etr_jul_food))
cat(sprintf('==========================================================\n'))

# ---- Country comparison (food only, top 15 + ROW) ---------------------------
etr_cty_ieepa <- compute_country_etrs(df_ieepa_food, 15)
etr_cty_s122  <- compute_country_etrs(df_s122_food, 15)
etr_cty_jul   <- compute_country_etrs(df_jul_food, 15)

country_chart <- prepare_chart_data(etr_cty_ieepa, etr_cty_s122, etr_cty_jul,
                                    'country_name',
                                    etr_ieepa_food, etr_s122_food, etr_jul_food)

cat('\n--- Food ETR by Country (Top 15 by Food Imports + ROW) ---\n\n')
print_comparison(country_chart$wide %>% arrange(desc(etr_ieepa)), 'label')

# ---- Food sub-category comparison -------------------------------------------
etr_food_ieepa <- compute_food_etrs(df_ieepa_food)
etr_food_s122  <- compute_food_etrs(df_s122_food)
etr_food_jul   <- compute_food_etrs(df_jul_food)

product_chart <- prepare_chart_data(etr_food_ieepa, etr_food_s122, etr_food_jul,
                                    'description',
                                    etr_ieepa_food, etr_s122_food, etr_jul_food)

cat('\n--- ETR by Food Sub-Category ---\n\n')
print_comparison(product_chart$wide %>% arrange(desc(etr_ieepa)), 'label')

# ---- Charts ------------------------------------------------------------------
message('\nGenerating charts...')

p_country <- make_two_panel(
  country_chart$wide, country_chart$long,
  title = 'Food ETR by Country',
  subtitle = sprintf(
    'Import-weighted average ETR for food (HTS 01-22); overall food: Pre-SCOTUS %.1f%%, Post-SCOTUS (122) %.1f%%, Post-SCOTUS (no 122) %.1f%%',
    etr_ieepa_food, etr_s122_food, etr_jul_food))

p_product <- make_two_panel(
  product_chart$wide, product_chart$long,
  title = 'Food ETR by Sub-Category',
  subtitle = 'Import-weighted average ETR by food sub-category; ranked by Feb 24 \u2212 Jan 1 difference')

# ---- Save charts -------------------------------------------------------------
output_dir <- 'output/2-21_temp'
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

ggsave(file.path(output_dir, 'etr_levels_by_food_country.png'),
       p_country, width = 14, height = 10, dpi = 200, bg = 'white')
message(sprintf('Saved %s/etr_levels_by_food_country.png', output_dir))

ggsave(file.path(output_dir, 'etr_levels_by_food_product.png'),
       p_product, width = 14, height = 8, dpi = 200, bg = 'white')
message(sprintf('Saved %s/etr_levels_by_food_product.png', output_dir))

# ---- Write data files -------------------------------------------------------
food_country_data <- country_chart$wide %>%
  transmute(
    category = str_remove_all(as.character(label), '\\*'),
    etr_pre_scotus = etr_ieepa,
    etr_post_scotus_122 = etr_s122,
    etr_post_scotus_no_122 = etr_jul,
    diff_122_vs_ieepa = gap
  ) %>%
  arrange(desc(etr_pre_scotus))

food_product_data <- product_chart$wide %>%
  transmute(
    category = str_remove_all(as.character(label), '\\*'),
    etr_pre_scotus = etr_ieepa,
    etr_post_scotus_122 = etr_s122,
    etr_post_scotus_no_122 = etr_jul,
    diff_122_vs_ieepa = gap
  ) %>%
  arrange(desc(etr_pre_scotus))

write_csv(food_country_data, file.path(output_dir, 'food_etr_by_country.csv'))
message(sprintf('Saved %s/food_etr_by_country.csv', output_dir))

write_csv(food_product_data, file.path(output_dir, 'food_etr_by_product.csv'))
message(sprintf('Saved %s/food_etr_by_product.csv', output_dir))
