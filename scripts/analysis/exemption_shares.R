# =============================================================================
# exemption_shares.R
# =============================================================================
#
# Estimate product × country effective exemption shares by comparing
# realized ETR (Census calculated duty / customs value) with MFN rates.
#
# Exemption share = 1 - (realized_etr / mfn_rate)
#   = 1.0 → all imports enter duty-free (full FTA preference)
#   = 0.5 → half the MFN rate is effectively waived
#   = 0.0 → imports pay full MFN
#   < 0.0 → imports pay MORE than MFN (S232/S301 surcharges)
#
# This produces exemption shares at HS2 × partner and HS6 × partner levels
# that could be used to adjust baseline tariff calculations.
#
# Usage: source('analysis/exemption_shares.R')
# =============================================================================

library(tidyverse)
library(jsonlite)


# =============================================================================
# 1. Download Census data
# =============================================================================

download_census <- function(comm_lvl, year = 2024) {
  base_url <- 'https://api.census.gov/data/timeseries/intltrade/imports/hs'
  url <- sprintf(
    '%s?get=CAL_DUT_YR,CON_VAL_YR,DUT_VAL_YR,I_COMMODITY,CTY_CODE&YEAR=%d&MONTH=12&COMM_LVL=%s',
    base_url, year, comm_lvl
  )
  message(sprintf('Downloading %s-level data for %d...', comm_lvl, year))
  raw <- fromJSON(url)
  h <- raw[1, ]
  d <- as_tibble(raw[-1, ], .name_repair = 'minimal')
  names(d) <- h
  d %>%
    transmute(
      hs_code   = I_COMMODITY,
      cty_code  = CTY_CODE,
      cal_duty  = as.numeric(CAL_DUT_YR),
      con_value = as.numeric(CON_VAL_YR),
      dut_value = as.numeric(DUT_VAL_YR)
    ) %>%
    filter(
      !grepl('X', cty_code),
      !grepl('^0', cty_code),
      cty_code != '-',
      con_value > 0
    )
}


# =============================================================================
# 2. Map countries to partners
# =============================================================================

add_partners <- function(data) {
  partner_map <- read_csv(
    'resources/country_partner_mapping.csv',
    col_types = cols(cty_code = col_character(), partner = col_character()),
    show_col_types = FALSE
  ) %>%
    select(cty_code, partner)

  data %>%
    left_join(partner_map, by = 'cty_code') %>%
    mutate(partner = if_else(is.na(partner), 'row', partner))
}


# =============================================================================
# 3. Load MFN rates
# =============================================================================

load_mfn_hs2 <- function() {
  read_csv(
    'resources/mfn_rates_2025.csv',
    col_types = cols(hs8 = col_character(), mfn_rate = col_double()),
    show_col_types = FALSE
  ) %>%
    mutate(hs2 = substr(hs8, 1, 2)) %>%
    group_by(hs2) %>%
    summarise(mfn_rate = mean(mfn_rate), .groups = 'drop')
}

load_mfn_hs6 <- function() {
  read_csv(
    'resources/mfn_rates_2025.csv',
    col_types = cols(hs8 = col_character(), mfn_rate = col_double()),
    show_col_types = FALSE
  ) %>%
    mutate(hs6 = substr(hs8, 1, 6)) %>%
    group_by(hs6) %>%
    summarise(mfn_rate = mean(mfn_rate), .groups = 'drop')
}


# =============================================================================
# 4. Compute exemption shares
# =============================================================================

calc_exemption_shares <- function(data, mfn, hs_digits) {
  # Aggregate to HS × partner level
  agg <- data %>%
    mutate(hs = substr(hs_code, 1, hs_digits)) %>%
    group_by(hs, partner) %>%
    summarise(
      cal_duty  = sum(cal_duty),
      con_value = sum(con_value),
      dut_value = sum(dut_value),
      .groups = 'drop'
    ) %>%
    mutate(
      realized_etr   = cal_duty / con_value,
      dutiable_share = dut_value / con_value
    )

  # Join MFN rates
  mfn_renamed <- mfn %>% rename(hs = 1)
  agg %>%
    left_join(mfn_renamed, by = 'hs') %>%
    mutate(
      # Exemption share: what fraction of MFN is waived
      # Capped at 1.0 (can't have >100% exemption)
      # Negative = paying more than MFN (surcharges)
      exemption_share = if_else(
        mfn_rate > 0,
        1 - realized_etr / mfn_rate,
        if_else(realized_etr == 0, 0, -Inf)
      )
    )
}

format_pct <- function(x) sprintf('%.1f%%', x * 100)


# =============================================================================
# 5. Main
# =============================================================================

main <- function() {
  cat('\n')
  cat(strrep('*', 70), '\n')
  cat('  Product × Country Effective Exemption Shares\n')
  cat(strrep('*', 70), '\n')
  cat('\nExemption share = 1 - (realized_etr / mfn_rate)\n')
  cat('  1.0 = fully exempt (FTA/GSP), 0 = pays full MFN, <0 = surcharges\n\n')

  # ---- HS2 × partner ----
  hs2_data <- download_census('HS2') %>% add_partners()
  mfn_hs2 <- load_mfn_hs2()
  hs2_exemptions <- calc_exemption_shares(hs2_data, mfn_hs2, 2)

  # ---- Overall exemption by partner (import-weighted) ----
  cat('\n')
  cat('Overall Import-Weighted Exemption Share by Partner\n')
  cat(strrep('=', 55), '\n\n')

  partner_overall <- hs2_exemptions %>%
    filter(is.finite(exemption_share), mfn_rate > 0) %>%
    group_by(partner) %>%
    summarise(
      total_value       = sum(con_value),
      dutiable_value    = sum(dut_value),
      total_duty        = sum(cal_duty),
      weighted_mfn      = sum(mfn_rate * con_value) / sum(con_value),
      realized_etr      = sum(cal_duty) / sum(con_value),
      .groups = 'drop'
    ) %>%
    mutate(
      exemption_share = 1 - realized_etr / weighted_mfn,
      duty_free_share = 1 - dutiable_value / total_value
    ) %>%
    arrange(desc(exemption_share))

  partner_display <- partner_overall %>%
    mutate(
      Partner = toupper(partner),
      `Imports ($B)` = sprintf('$%.0f', total_value / 1e9),
      `Wt Avg MFN` = format_pct(weighted_mfn),
      `Realized ETR` = format_pct(realized_etr),
      `Exemption Share` = format_pct(exemption_share),
      `Duty-Free Share` = format_pct(duty_free_share)
    ) %>%
    select(Partner, `Imports ($B)`, `Wt Avg MFN`, `Realized ETR`,
           `Exemption Share`, `Duty-Free Share`)

  print(as.data.frame(partner_display), row.names = FALSE, right = FALSE)

  # ---- HS2 × partner detail for FTA partners ----
  fta_partners <- c('canada', 'mexico', 'ftrow')

  for (p in fta_partners) {
    cat(sprintf('\n\n%s: HS2-Level Exemption Shares (>$100M imports, MFN > 0)\n', toupper(p)))
    cat(strrep('=', 65), '\n\n')

    p_data <- hs2_exemptions %>%
      filter(partner == p, con_value > 1e8, mfn_rate > 0, is.finite(exemption_share)) %>%
      arrange(desc(con_value)) %>%
      mutate(
        `HS2` = hs,
        `Imports ($B)` = sprintf('$%.1f', con_value / 1e9),
        `MFN Rate` = format_pct(mfn_rate),
        `Realized` = format_pct(realized_etr),
        `Exemption` = format_pct(exemption_share)
      ) %>%
      select(`HS2`, `Imports ($B)`, `MFN Rate`, `Realized`, `Exemption`)

    print(as.data.frame(p_data), row.names = FALSE, right = FALSE)
  }

  # ---- Non-FTA partners: show where realized > MFN (surcharges) ----
  cat('\n\nNon-FTA Partners: HS2 Chapters Where Realized ETR > MFN (Surcharges)\n')
  cat(strrep('=', 70), '\n')
  cat('(Indicates S232/S301 tariffs pushing realized rate above MFN)\n\n')

  surcharge_data <- hs2_exemptions %>%
    filter(
      partner %in% c('china', 'eu', 'uk', 'japan', 'row'),
      exemption_share < -0.05,
      con_value > 5e8,
      is.finite(exemption_share)
    ) %>%
    arrange(partner, exemption_share) %>%
    mutate(
      Partner = toupper(partner),
      `HS2` = hs,
      `Imports ($B)` = sprintf('$%.1f', con_value / 1e9),
      `MFN Rate` = format_pct(mfn_rate),
      `Realized` = format_pct(realized_etr),
      `Surcharge` = format_pct(-exemption_share)
    ) %>%
    select(Partner, `HS2`, `Imports ($B)`, `MFN Rate`, `Realized`, `Surcharge`)

  print(as.data.frame(surcharge_data), row.names = FALSE, right = FALSE)

  # ---- Download HS6 for detailed FTA analysis ----
  cat('\n\nDownloading HS6-level data for detailed FTA analysis...\n')
  hs6_data <- download_census('HS6') %>% add_partners()
  mfn_hs6 <- load_mfn_hs6()
  hs6_exemptions <- calc_exemption_shares(hs6_data, mfn_hs6, 6)

  message(sprintf('HS6 exemptions: %s rows', format(nrow(hs6_exemptions), big.mark = ',')))

  # ---- HS6 exemption summary for FTA partners ----
  for (p in fta_partners) {
    cat(sprintf('\n\n%s: HS6 Exemption Distribution (MFN > 0, >$10M imports)\n', toupper(p)))
    cat(strrep('=', 65), '\n\n')

    p_hs6 <- hs6_exemptions %>%
      filter(partner == p, mfn_rate > 0, con_value > 1e7, is.finite(exemption_share))

    # Distribution of exemption shares
    bins <- p_hs6 %>%
      mutate(
        exemption_bin = case_when(
          exemption_share >= 0.95 ~ 'Fully exempt (>95%)',
          exemption_share >= 0.50 ~ 'Mostly exempt (50-95%)',
          exemption_share >= 0.05 ~ 'Partially exempt (5-50%)',
          exemption_share >= -0.05 ~ 'Pays ~full MFN (-5% to 5%)',
          TRUE ~ 'Pays above MFN (surcharge)'
        ),
        exemption_bin = factor(exemption_bin, levels = c(
          'Fully exempt (>95%)',
          'Mostly exempt (50-95%)',
          'Partially exempt (5-50%)',
          'Pays ~full MFN (-5% to 5%)',
          'Pays above MFN (surcharge)'
        ))
      ) %>%
      group_by(exemption_bin) %>%
      summarise(
        n_hs6 = n(),
        imports = sum(con_value),
        .groups = 'drop'
      ) %>%
      mutate(
        import_share = imports / sum(imports),
        Category = as.character(exemption_bin),
        `HS6 Lines` = n_hs6,
        `Imports ($B)` = sprintf('$%.1f', imports / 1e9),
        `Import Share` = format_pct(import_share)
      ) %>%
      select(Category, `HS6 Lines`, `Imports ($B)`, `Import Share`)

    print(as.data.frame(bins), row.names = FALSE, right = FALSE)
  }

  # ---- HS6 × partner: top non-exempt products for Canada/Mexico ----
  for (p in c('canada', 'mexico')) {
    cat(sprintf('\n\n%s: Top 20 Non-Exempt HS6 Products (realized > 1%%, >$50M imports)\n', toupper(p)))
    cat(strrep('=', 70), '\n\n')

    top_nonexempt <- hs6_exemptions %>%
      filter(
        partner == p,
        realized_etr > 0.01,
        con_value > 5e7,
        is.finite(exemption_share)
      ) %>%
      arrange(desc(cal_duty)) %>%
      head(20) %>%
      mutate(
        `HS6` = hs,
        `Imports ($M)` = sprintf('$%.0f', con_value / 1e6),
        `Duty ($M)` = sprintf('$%.0f', cal_duty / 1e6),
        `MFN` = format_pct(mfn_rate),
        `Realized` = format_pct(realized_etr),
        `Exemption` = format_pct(exemption_share)
      ) %>%
      select(`HS6`, `Imports ($M)`, `Duty ($M)`, `MFN`, `Realized`, `Exemption`)

    print(as.data.frame(top_nonexempt), row.names = FALSE, right = FALSE)
  }

  # ---- China: exemption distribution (should be ~0) ----
  cat('\n\nCHINA: HS6 Exemption Distribution (MFN > 0, >$10M imports)\n')
  cat(strrep('=', 65), '\n\n')

  china_hs6 <- hs6_exemptions %>%
    filter(partner == 'china', mfn_rate > 0, con_value > 1e7, is.finite(exemption_share))

  china_bins <- china_hs6 %>%
    mutate(
      exemption_bin = case_when(
        exemption_share >= 0.95 ~ 'Fully exempt (>95%)',
        exemption_share >= 0.50 ~ 'Mostly exempt (50-95%)',
        exemption_share >= 0.05 ~ 'Partially exempt (5-50%)',
        exemption_share >= -0.05 ~ 'Pays ~full MFN (-5% to 5%)',
        exemption_share >= -1.0 ~ 'S301/S232 surcharge (up to 2x MFN)',
        TRUE ~ 'Heavy surcharge (>2x MFN)'
      ),
      exemption_bin = factor(exemption_bin, levels = c(
        'Fully exempt (>95%)',
        'Mostly exempt (50-95%)',
        'Partially exempt (5-50%)',
        'Pays ~full MFN (-5% to 5%)',
        'S301/S232 surcharge (up to 2x MFN)',
        'Heavy surcharge (>2x MFN)'
      ))
    ) %>%
    group_by(exemption_bin) %>%
    summarise(
      n_hs6 = n(),
      imports = sum(con_value),
      .groups = 'drop'
    ) %>%
    mutate(
      import_share = imports / sum(imports),
      Category = as.character(exemption_bin),
      `HS6 Lines` = n_hs6,
      `Imports ($B)` = sprintf('$%.1f', imports / 1e9),
      `Import Share` = format_pct(import_share)
    ) %>%
    select(Category, `HS6 Lines`, `Imports ($B)`, `Import Share`)

  print(as.data.frame(china_bins), row.names = FALSE, right = FALSE)

  # ---- Save outputs ----
  output_dir <- 'analysis/output'

  # HS2 × partner exemptions
  hs2_out <- hs2_exemptions %>%
    filter(is.finite(exemption_share)) %>%
    select(hs, partner, con_value, dut_value, cal_duty,
           realized_etr, dutiable_share, mfn_rate, exemption_share) %>%
    arrange(partner, hs)
  write_csv(hs2_out, file.path(output_dir, 'exemption_shares_hs2_partner.csv'))

  # HS6 × partner exemptions
  hs6_out <- hs6_exemptions %>%
    filter(is.finite(exemption_share)) %>%
    select(hs, partner, con_value, dut_value, cal_duty,
           realized_etr, dutiable_share, mfn_rate, exemption_share) %>%
    arrange(partner, hs)
  write_csv(hs6_out, file.path(output_dir, 'exemption_shares_hs6_partner.csv'))

  # Partner-level summary
  write_csv(partner_overall, file.path(output_dir, 'exemption_shares_partner_summary.csv'))

  message(sprintf('\nOutput saved to %s/', output_dir))

  cat('\n')
  cat(strrep('*', 70), '\n')
  cat('  Analysis complete\n')
  cat(strrep('*', 70), '\n')

  invisible(list(
    hs2_exemptions = hs2_exemptions,
    hs6_exemptions = hs6_exemptions,
    partner_overall = partner_overall
  ))
}

results <- main()
