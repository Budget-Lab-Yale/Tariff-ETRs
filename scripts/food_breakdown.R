library(tidyverse)
library(yaml)
library(gridExtra)
source('src/config_parsing.R')
source('src/data_processing.R')
source('src/calculations.R')

food_chapters <- tribble(
  ~hts2, ~chapter_desc,
  '01', 'Live Animals',
  '02', 'Meat',
  '03', 'Fish & Seafood',
  '04', 'Dairy, Eggs, Honey',
  '05', 'Other Animal Products',
  '06', 'Live Plants & Flowers',
  '07', 'Vegetables',
  '08', 'Fruit & Nuts',
  '09', 'Coffee, Tea, Spices',
  '10', 'Cereals',
  '11', 'Milling Products',
  '12', 'Oil Seeds & Grains',
  '13', 'Gums & Resins',
  '14', 'Vegetable Plaiting Materials',
  '15', 'Fats & Oils',
  '16', 'Meat & Fish Preparations',
  '17', 'Sugar & Confectionery',
  '18', 'Cocoa & Chocolate',
  '19', 'Cereal & Bakery Products',
  '20', 'Veg. & Fruit Preparations',
  '21', 'Misc. Food Preparations',
  '22', 'Beverages',
  '23', 'Animal Feed & Residues',
  '24', 'Tobacco'
)

usmca_shares    <- read_csv('resources/usmca_shares.csv', show_col_types = FALSE)
country_mapping <- read_csv('resources/country_partner_mapping.csv',
                            col_types = cols(cty_code = col_character()),
                            show_col_types = FALSE)
hs10_by_country <- readRDS('cache/hs10_by_country_gtap_2024_con.rds') %>%
  filter(!str_detect(hs10, '^(98|99)'), !is.na(gtap_code))

# ---- Compute food ETRs for each regime ---------------------------------------
compute_food_etrs <- function(config_path) {
  config <- load_scenario_config(config_path)
  etrs   <- calc_etrs_for_config(config, hs10_by_country, usmca_shares, country_mapping)
  df     <- etrs$hs10_country_etrs

  df %>%
    mutate(hts2 = str_sub(hs10, 1, 2)) %>%
    filter(hts2 %in% food_chapters$hts2) %>%
    left_join(food_chapters, by = 'hts2') %>%
    group_by(hts2, chapter_desc) %>%
    summarise(
      weighted_etr = sum(etr * imports),
      imports      = sum(imports),
      .groups = 'drop'
    ) %>%
    mutate(etr = if_else(imports > 0, weighted_etr / imports, 0) * 100)
}

ieepa_food <- compute_food_etrs('config/2-21_temp/2026-01-01')
s122_food  <- compute_food_etrs('config/2-21_temp/2026-02-24')

# ---- Compute OVERALL row -----------------------------------------------------
overall_ieepa <- ieepa_food %>%
  summarise(
    weighted_etr = sum(weighted_etr),
    imports      = sum(imports)
  ) %>%
  mutate(hts2 = 'ALL', chapter_desc = 'OVERALL',
         etr = if_else(imports > 0, weighted_etr / imports, 0) * 100)

overall_s122 <- s122_food %>%
  summarise(
    weighted_etr = sum(weighted_etr),
    imports      = sum(imports)
  ) %>%
  mutate(hts2 = 'ALL', chapter_desc = 'OVERALL',
         etr = if_else(imports > 0, weighted_etr / imports, 0) * 100)

# ---- Combine chapters + OVERALL, compute gap --------------------------------
food_wide <- bind_rows(ieepa_food, overall_ieepa) %>%
  select(hts2, chapter_desc, etr_ieepa = etr) %>%
  left_join(
    bind_rows(s122_food, overall_s122) %>% select(hts2, etr_s122 = etr),
    by = 'hts2'
  ) %>%
  mutate(
    gap   = etr_ieepa - etr_s122,
    label = if_else(hts2 == 'ALL', 'OVERALL',
                    sprintf('%s (%s)', chapter_desc, hts2))
  )

# Rank by signed gap (largest positive at top); OVERALL always at bottom
chapter_order <- food_wide %>%
  filter(hts2 != 'ALL') %>%
  arrange(gap) %>%
  pull(label)
chapter_order <- c('OVERALL', chapter_order)

food_wide <- food_wide %>%
  mutate(label = factor(label, levels = chapter_order))

# Long form for right panel
food_long <- food_wide %>%
  pivot_longer(
    cols = c(etr_ieepa, etr_s122),
    names_to = 'regime',
    values_to = 'etr'
  ) %>%
  mutate(
    regime = factor(regime,
                    levels = c('etr_ieepa', 'etr_s122'),
                    labels = c('IEEPA Regime', 'S122 Regime'))
  )

# ---- Left panel: signed difference ------------------------------------------
p_left <- ggplot(food_wide, aes(x = label, y = gap)) +
  geom_col(aes(fill = gap > 0), width = 0.6) +
  geom_text(aes(label = sprintf('%+.1f pp', gap),
                hjust = if_else(gap >= 0, -0.1, 1.1)),
            size = 3) +
  coord_flip() +
  scale_fill_manual(values = c('TRUE' = '#2166ac', 'FALSE' = '#b2182b')) +
  scale_y_continuous(labels = function(x) paste0(x, ' pp'),
                     expand = expansion(mult = c(0.15, 0.15))) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  guides(fill = 'none') +
  labs(
    title = 'Difference (IEEPA - S122)',
    x = NULL,
    y = 'Percentage Point Difference'
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = 'bold', size = 11)
  )

# ---- Right panel: ETR levels by regime --------------------------------------
p_right <- ggplot(food_long, aes(x = label, y = etr, fill = regime)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  geom_text(aes(label = sprintf('%.1f%%', etr)),
            position = position_dodge(width = 0.7), hjust = -0.1, size = 3) +
  coord_flip() +
  scale_fill_manual(values = c('#2166ac', '#b2182b')) +
  scale_y_continuous(labels = function(x) paste0(x, '%'),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = 'ETR by Regime',
    x = NULL,
    y = 'Effective Tariff Rate',
    fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = 'top',
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(face = 'bold', size = 11)
  )

# ---- Combine panels ----------------------------------------------------------
# Extract legend from right panel
legend_grob <- cowplot::get_legend(p_right)
p_right_no_legend <- p_right + theme(legend.position = 'none')

title_grob <- grid::textGrob(
  'Food Product ETRs: IEEPA vs S122 Regime',
  gp = grid::gpar(fontface = 'bold', fontsize = 14),
  hjust = 0, x = 0.02
)
subtitle_grob <- grid::textGrob(
  'Ranked by IEEPA-S122 difference (largest at top)',
  gp = grid::gpar(fontsize = 11),
  hjust = 0, x = 0.02
)

p <- gridExtra::arrangeGrob(
  title_grob,
  subtitle_grob,
  legend_grob,
  gridExtra::arrangeGrob(p_left, p_right_no_legend, ncol = 2, widths = c(1, 1)),
  ncol = 1,
  heights = c(0.04, 0.03, 0.04, 0.89)
)

output_dir <- 'output/2-21_temp'
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
ggsave(file.path(output_dir, 'food_etrs_ieepa_vs_s122.png'),
       p, width = 14, height = 10, dpi = 200, bg = 'white')
message(sprintf('Saved %s/food_etrs_ieepa_vs_s122.png', output_dir))
