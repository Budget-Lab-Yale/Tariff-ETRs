library(tidyverse)

# Stacking exercise data (Census weights, levels)
data <- tribble(
  ~scenario,        ~layer, ~label,           ~level,
  'Current Policy\n(with S122)',  0,      'Base (ex-S122)',  8.17,
  'Current Policy\n(with S122)',  1,      'S122',           11.02,
  'Current Policy\n(with S122)',  2,      'Pharma 232',     11.43,
  'Current Policy\n(with S122)',  3,      'Metal Overhaul', 12.92,
  'Current Policy After\nS122 Expiration',   0,      'Base (ex-S122)',  8.17,
  'Current Policy After\nS122 Expiration',   1,      'Pharma 232',      8.57,
  'Current Policy After\nS122 Expiration',   2,      'Metal Overhaul', 10.35
)

# Compute marginal contribution of each layer
stacked <- data %>%
  group_by(scenario) %>%
  arrange(layer) %>%
  mutate(
    marginal = level - lag(level, default = 0),
    marginal = if_else(layer == 0, level, marginal)
  ) %>%
  ungroup()

# Set factor order for stacking (reverse so base is at bottom)
layer_order <- c('Metal Overhaul', 'Pharma 232', 'S122', 'Base (ex-S122)')
stacked$label <- factor(stacked$label, levels = layer_order)
stacked$scenario <- factor(stacked$scenario, levels = c('Current Policy\n(with S122)', 'Current Policy After\nS122 Expiration'))

# Colors
layer_colors <- c(
  'Base (ex-S122)' = '#4A7C91',
  'S122'           = '#E8A838',
  'Pharma 232'     = '#C75B7A',
  'Metal Overhaul' = '#6B4C9A'
)

p <- ggplot(stacked, aes(x = scenario, y = marginal, fill = label)) +
  geom_col(width = 0.6, color = 'white', linewidth = 0.3) +
  geom_text(
    aes(label = sprintf('+%.1fpp', marginal)),
    position = position_stack(vjust = 0.5),
    size = 3.5, color = 'white', fontface = 'bold'
  ) +
  # Total label on top
  geom_text(
    data = data %>% group_by(scenario) %>% summarise(total = max(level), .groups = 'drop'),
    aes(x = scenario, y = total, label = sprintf('%.1f%%', total), fill = NULL),
    vjust = -0.5, size = 4, fontface = 'bold'
  ) +
  scale_fill_manual(values = layer_colors, name = 'Policy Layer') +
  scale_y_continuous(
    labels = function(x) paste0(x, '%'),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = 'Pre-Substitution Average ETR by Policy Layer',
    subtitle = '2024 Census import weights',
    x = NULL,
    y = 'Pre-Substitution Average ETR'
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = 'bottom',
    plot.title = element_text(face = 'bold')
  )

ggsave('output/stacking_chart.png', p, width = 8, height = 6, dpi = 150)
cat('Saved to output/stacking_chart.png\n')
