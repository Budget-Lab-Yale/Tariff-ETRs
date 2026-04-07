# =============================================================================
# decompose_metal_overhaul.R
# =============================================================================
# Decompose the trade-weighted tariff change from the April 6 metal 232 overhaul
# into: (1) Annex II scope removal, (2) rate changes on I-B/III,
# (3) content share basis change (metal_share → 100%).
#
# Key regime fact: by 2026-02-24, IEEPA reciprocal was overturned (rate = 0).
# The non-232 "reciprocal" is Section 122 at 10% flat.
# S122 is excluded to the extent 232 applies (scales by nonmetal_share).
#
# Uses cached import data + BEA shares + reform YAML for annex classification.
# Does NOT run the full model — computes analytically.
# =============================================================================

library(dplyr)
library(readr)
library(yaml)
library(stringr)
library(rlang)

# ---- Load data ----
message('Loading import data...')
imports <- readRDS('cache/hs10_by_country_gtap_2024_con.rds')
message(sprintf('  %d rows, $%.1fB total imports',
                nrow(imports), sum(imports$imports) / 1e9))

message('Loading BEA metal content shares...')
bea <- read_csv('resources/metal_content_shares_detail_hs10_total.csv',
                 show_col_types = FALSE,
                 col_types = cols(hs10 = col_character())) %>%
  select(hs10, metal_share, steel_share, aluminum_share, copper_share)

# ---- Classify products into annexes ----
message('Parsing reform YAML for annex classification...')
reform <- yaml::read_yaml(
  'config/scenarios/metal_232_overhaul/reforms/metal_232/s232.yaml'
)

# Build annex lookup as a tibble
annex_rows <- list()
for (prog_name in names(reform)) {
  codes <- reform[[prog_name]]$base
  if (is.null(codes)) next

  annex <- case_when(
    str_detect(prog_name, '_ia$') ~ 'I-A',
    str_detect(prog_name, '_ib$') ~ 'I-B',
    str_detect(prog_name, '_iii$') ~ 'III',
    prog_name %in% c('steel', 'aluminum', 'aluminum_derivatives',
                      'steel_derivatives') ~ 'legacy_zero',
    TRUE ~ NA_character_
  )
  if (is.na(annex)) next

  rate <- reform[[prog_name]]$rates$default
  tt <- reform[[prog_name]]$target_total$default

  annex_rows <- c(annex_rows, lapply(codes, function(code) {
    tibble(prefix = as.character(code), annex = annex,
           new_s232_rate = rate, target_total = tt %||% NA_real_,
           prefix_len = nchar(as.character(code)),
           priority = if_else(annex == 'legacy_zero', 0L, 1L))
  }))
}
annex_lookup <- bind_rows(annex_rows)

# Fast classification: build prefix→annex lookup, match via substr join
message('Classifying products by annex...')
unique_hs10 <- tibble(hs10 = unique(imports$hs10))

# Build flat prefix→annex tibble (one row per prefix)
prefix_entries <- list()
for (prog_name in names(reform)) {
  codes <- reform[[prog_name]]$base
  if (is.null(codes)) next

  annex <- case_when(
    str_detect(prog_name, '_ia$') ~ 'I-A',
    str_detect(prog_name, '_ib$') ~ 'I-B',
    str_detect(prog_name, '_iii$') ~ 'III',
    prog_name %in% c('steel', 'aluminum', 'aluminum_derivatives',
                      'steel_derivatives') ~ 'legacy_zero',
    TRUE ~ NA_character_
  )
  if (is.na(annex)) next

  rate <- reform[[prog_name]]$rates$default
  tt <- reform[[prog_name]]$target_total$default

  prefix_entries[[length(prefix_entries) + 1]] <- tibble(
    prefix = as.character(codes),
    prefix_len = nchar(as.character(codes)),
    annex = annex,
    new_s232_rate = rate,
    target_total = tt %||% NA_real_,
    priority = if_else(annex == 'legacy_zero', 0L, 1L)
  )
}
prefix_tbl <- bind_rows(prefix_entries)
message(sprintf('  %d prefix entries across %d programs',
                nrow(prefix_tbl), length(prefix_entries)))

# For each prefix length, do a fast equi-join
annex_df <- unique_hs10 %>% mutate(annex = NA_character_, new_s232_rate = NA_real_,
                                     target_total = NA_real_, best_score = -1L)

for (plen in sort(unique(prefix_tbl$prefix_len))) {
  ptbl <- prefix_tbl %>% filter(prefix_len == plen)
  # Deduplicate: if multiple programs have the same prefix, keep highest priority
  ptbl <- ptbl %>%
    group_by(prefix) %>%
    arrange(desc(priority)) %>%
    slice(1) %>%
    ungroup()

  joined <- unique_hs10 %>%
    mutate(.pfx = substr(hs10, 1, plen)) %>%
    inner_join(ptbl, by = c('.pfx' = 'prefix')) %>%
    select(hs10, annex_new = annex, rate_new = new_s232_rate,
           tt_new = target_total, priority, prefix_len)

  if (nrow(joined) == 0) next

  # Score = priority * 100 + prefix_len (higher is better)
  joined <- joined %>% mutate(score = priority * 100L + prefix_len)

  annex_df <- annex_df %>%
    left_join(joined %>% select(hs10, annex_new, rate_new, tt_new, score),
              by = 'hs10') %>%
    mutate(
      should_update = !is.na(score) & score > best_score,
      annex = if_else(should_update, annex_new, annex),
      new_s232_rate = if_else(should_update, rate_new, new_s232_rate),
      target_total = if_else(should_update, tt_new, target_total),
      best_score = if_else(should_update, score, best_score)
    ) %>%
    select(hs10, annex, new_s232_rate, target_total, best_score)
}

annex_df <- annex_df %>%
  mutate(
    annex = if_else(!is.na(annex) & annex == 'legacy_zero', 'II', annex),
    new_s232_rate = if_else(!is.na(annex) & annex == 'II', 0, new_s232_rate)
  ) %>%
  select(hs10, annex, new_s232_rate, target_total)

# Summary of annex classification
cat('\nAnnex classification:\n')
annex_df %>%
  group_by(annex) %>%
  summarise(n = n(), .groups = 'drop') %>%
  { for (i in seq_len(nrow(.))) {
      cat(sprintf('  %-10s %5d products\n', .[i,]$annex, .[i,]$n))
    }
  }
cat(sprintf('  %-10s %5d products\n', 'not 232',
    sum(is.na(annex_df$annex))))

# ---- Load MFN rates ----
message('\nLoading MFN rates...')
mfn <- read_csv('resources/mfn_rates_2025.csv', show_col_types = FALSE,
                  col_types = cols(hs8 = col_character()))
mfn_df <- unique_hs10 %>%
  mutate(hs8 = substr(hs10, 1, 8)) %>%
  left_join(mfn, by = 'hs8') %>%
  mutate(mfn_rate = if_else(is.na(mfn_rate), 0, mfn_rate)) %>%
  select(hs10, mfn_rate)

# ---- Merge everything ----
# Key regime fact: on 2026-02-24, IEEPA reciprocal = 0, S122 = 10% flat.
# S122 is excluded to the extent 232 applies (same nonmetal_share scaling).
S122_RATE <- 0.10

message('Building analysis dataset...')
analysis <- imports %>%
  left_join(annex_df, by = 'hs10') %>%
  left_join(bea, by = 'hs10') %>%
  left_join(mfn_df, by = 'hs10') %>%
  mutate(
    hts2 = substr(hs10, 1, 2),
    is_primary = hts2 %in% c('72', '73', '74', '76'),
    metal_share = case_when(
      is_primary ~ 1.0,
      !is.na(metal_share) ~ metal_share,
      TRUE ~ 0.5
    ),
    was_232 = !is.na(annex),

    # ======================================================
    # OLD REGIME (pre-April 6):
    #   232: 50% on metal content value
    #   S122: 10% on non-232 base (nonmetal_share)
    #   For 232-covered products: nonmetal_share = 1 - metal_share
    #   For non-232 products: S122 applies in full (but not relevant here)
    # ======================================================
    old_s232 = if_else(was_232, 0.50 * metal_share, 0),
    old_s122_on_nonmetal = if_else(was_232, S122_RATE * (1 - metal_share), 0),
    old_total = old_s232 + old_s122_on_nonmetal,

    # ======================================================
    # NEW REGIME (post-April 6):
    #   Annex I-A: 50% on full value, S122 displaced (nonmetal_share=0)
    #   Annex I-B: 25% on full value, S122 displaced
    #   Annex III: max(15% - MFN, 0) floor, S122 displaced
    #   Annex II:  removed from 232 → S122 applies in full
    # ======================================================
    new_s232 = case_when(
      is.na(annex) ~ 0,
      annex == 'II' ~ 0,
      annex == 'III' ~ pmax(0.15 - mfn_rate, 0),
      TRUE ~ new_s232_rate
    ),
    new_s122 = case_when(
      is.na(annex) ~ 0,
      annex == 'II' ~ S122_RATE,  # falls back to full S122
      TRUE ~ 0  # displaced by 232
    ),
    new_total = new_s232 + new_s122,

    delta = new_total - old_total,

    # ======================================================
    # DECOMPOSITION (for 232-affected products only)
    # old_total = 50% * ms + 10% * (1-ms)   = 10% + 40%*ms
    # new_total = new_rate + 0               (for I-A/I-B)
    # delta     = new_rate - 10% - 40%*ms
    #
    # Decompose into:
    # (a) Rate effect: change in statutory rate holding content basis at old
    #     = ms * (new_rate - 50%)
    # (b) Content broadening: going from ms to 100% at the new rate
    #     = new_rate * (1 - ms)
    # (c) S122 displacement: S122 no longer applies to non-metal portion
    #     = -10% * (1 - ms)
    # (d) Scope removal (Annex II only): entire delta
    # ======================================================
    decomp_scope = case_when(
      annex == 'II' ~ delta,
      TRUE ~ 0
    ),
    decomp_rate = case_when(
      is.na(annex) | annex == 'II' ~ 0,
      annex == 'III' ~ metal_share * (pmax(0.15 - mfn_rate, 0) - 0.50),
      TRUE ~ metal_share * (new_s232_rate - 0.50)
    ),
    decomp_content_broadening = case_when(
      is.na(annex) | annex == 'II' ~ 0,
      annex == 'III' ~ pmax(0.15 - mfn_rate, 0) * (1 - metal_share),
      TRUE ~ new_s232_rate * (1 - metal_share)
    ),
    decomp_s122_displacement = case_when(
      is.na(annex) | annex == 'II' ~ 0,
      TRUE ~ -S122_RATE * (1 - metal_share)
    )
  )

# Verify decomposition
analysis <- analysis %>%
  mutate(decomp_check = decomp_scope + decomp_rate +
           decomp_content_broadening + decomp_s122_displacement)
max_err <- max(abs(analysis$delta - analysis$decomp_check))
message(sprintf('Decomposition check: max error = %.8f', max_err))

# ---- Trade-weighted aggregation ----
total_imports <- sum(analysis$imports)
s232_imports <- sum(analysis$imports[analysis$was_232])

cat('\n')
cat('========================================================================\n')
cat('DECOMPOSITION: Metal 232 Overhaul (April 6, 2026)\n')
cat('========================================================================\n')
cat(sprintf('Total imports: $%.1fB\n', total_imports / 1e9))
cat(sprintf('232-affected imports (old scope): $%.1fB\n', s232_imports / 1e9))
cat(sprintf('Non-232 tariff: S122 at %.0f%% (IEEPA overturned)\n', S122_RATE * 100))

cat('\n--- Products by Annex ---\n')
annex_summary <- analysis %>%
  filter(was_232) %>%
  group_by(annex) %>%
  summarise(
    n_products = n_distinct(hs10),
    imports_B = sum(imports) / 1e9,
    avg_metal_share = weighted.mean(metal_share, imports),
    avg_old = weighted.mean(old_total, imports) * 100,
    avg_new = weighted.mean(new_total, imports) * 100,
    avg_delta = weighted.mean(delta, imports) * 100,
    .groups = 'drop'
  ) %>%
  arrange(annex)

for (i in seq_len(nrow(annex_summary))) {
  r <- annex_summary[i, ]
  cat(sprintf('\n  Annex %-3s: %4d products, $%6.1fB imports\n',
      r$annex, r$n_products, r$imports_B))
  cat(sprintf('    avg BEA metal_share = %.1f%%\n', r$avg_metal_share * 100))
  cat(sprintf('    old tariff (232+S122) = %.2f%%\n', r$avg_old))
  cat(sprintf('    new tariff (232+S122) = %.2f%%\n', r$avg_new))
  cat(sprintf('    delta = %+.2f pp\n', r$avg_delta))
}

# Overall effect on total US tariff average
cat('\n--- Trade-Weighted Effect on Overall US Tariff (pp) ---\n')
cat('    (measured as share-of-total-imports × per-product delta)\n\n')

components <- c(
  'Annex II scope removal' = sum(analysis$decomp_scope * analysis$imports) /
    total_imports * 100,
  'Rate effect (statutory rate change)' =
    sum(analysis$decomp_rate * analysis$imports) / total_imports * 100,
  'Content broadening (ms → 100%)' =
    sum(analysis$decomp_content_broadening * analysis$imports) / total_imports * 100,
  'S122 displacement (nonmetal portion)' =
    sum(analysis$decomp_s122_displacement * analysis$imports) / total_imports * 100
)

for (name in names(components)) {
  cat(sprintf('  %-40s %+.3f pp\n', name, components[[name]]))
}
cat(sprintf('  %-40s -------\n', ''))
cat(sprintf('  %-40s %+.3f pp\n', 'NET EFFECT', sum(components)))

direct <- sum(analysis$delta * analysis$imports) / total_imports * 100
cat(sprintf('\n  Direct delta (cross-check):            %+.3f pp\n', direct))
cat(sprintf('  GTA published:                         -0.530 pp\n'))

# ---- Sensitivity: GTA-style flat content shares ----
cat('\n--- SENSITIVITY: Flat Content Shares ---\n')
cat('    (replacing BEA product-level with flat program-level ratios)\n\n')

# For each share level, compute the alternative delta
for (flat_ms in c(0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.50)) {
  alt <- analysis %>%
    filter(was_232) %>%
    mutate(
      alt_ms = if_else(is_primary, 1.0, flat_ms),
      alt_old = 0.50 * alt_ms + S122_RATE * (1 - alt_ms),
      alt_delta = new_total - alt_old
    )
  alt_net <- sum(alt$alt_delta * alt$imports) / total_imports * 100
  cat(sprintf('  flat derivative share = %2.0f%%:  net = %+.3f pp\n',
      flat_ms * 100, alt_net))
}

# Breakeven: what flat derivative share makes the net delta = -0.53pp?
cat('\n--- BREAKEVEN: What share matches GTA\'s -0.53pp? ---\n')
gta_target <- -0.530 / 100  # in rate form

# Binary search
lo <- 0.10; hi <- 1.0
for (iter in 1:50) {
  mid <- (lo + hi) / 2
  alt <- analysis %>%
    filter(was_232) %>%
    mutate(
      alt_ms = if_else(is_primary, 1.0, mid),
      alt_old = 0.50 * alt_ms + S122_RATE * (1 - alt_ms),
      alt_delta = new_total - alt_old
    )
  alt_net <- sum(alt$alt_delta * alt$imports) / total_imports
  if (alt_net > gta_target) lo <- mid else hi <- mid
}
cat(sprintf('  Flat derivative share that matches GTA: %.1f%%\n', mid * 100))

cat('\n========================================================================\n')
