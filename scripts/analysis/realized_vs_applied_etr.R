# =============================================================================
# realized_vs_applied_etr.R
# =============================================================================
#
# Side analysis: Compare realized ETR (calculated duties / customs value from
# Census data) with our model's applied baseline ETR for 2024 trade.
#
# Realized ETR = Census "calculated duty" / consumption value
# Applied ETR  = our model's baseline tariff rates (MFN + S232 + S301)
#
# Key question: How much lower is the realized ETR, and which countries/products
# drive the gap?
#
# Gap sources:
#   - FTA/GSP preferences (duty-free entry under trade agreements)
#   - De minimis exemptions (sub-$800 shipments)
#   - Foreign Trade Zones / bonded warehouses
#   - Specific/compound duties not captured in our ad valorem MFN rates
#   - TRQs (tariff-rate quotas) with lower in-quota rates
#
# Data source: Census Bureau International Trade API
#   Endpoint: api.census.gov/data/timeseries/intltrade/imports/hs
#   Variables: CAL_DUT_YR (calculated duty YTD), CON_VAL_YR (consumption value YTD)
#
# Usage: source('analysis/realized_vs_applied_etr.R')
# =============================================================================

library(tidyverse)
library(jsonlite)


# =============================================================================
# 1. Download Census data via API
# =============================================================================

download_census_duties <- function(comm_lvl = 'HS2', year = 2024) {
  base_url <- 'https://api.census.gov/data/timeseries/intltrade/imports/hs'

  # Use December YTD fields for annual totals
  params <- list(
    get = 'CAL_DUT_YR,CON_VAL_YR,DUT_VAL_YR,I_COMMODITY,CTY_CODE',
    YEAR = year,
    MONTH = 12,
    COMM_LVL = comm_lvl
  )

  url <- paste0(
    base_url, '?',
    paste(names(params), params, sep = '=', collapse = '&')
  )

  message(sprintf('Downloading %s-level data for %d...', comm_lvl, year))
  raw <- fromJSON(url)

  # First row is header, rest is data
  headers <- raw[1, ]
  data <- as_tibble(raw[-1, ], .name_repair = 'minimal')
  names(data) <- headers

  data %>%
    transmute(
      hs_code    = I_COMMODITY,
      cty_code   = CTY_CODE,
      cal_duty   = as.numeric(CAL_DUT_YR),
      con_value  = as.numeric(CON_VAL_YR),
      dut_value  = as.numeric(DUT_VAL_YR)
    ) %>%
    # Exclude aggregate country codes (regional/continent totals)
    # Aggregates: start with '0', contain 'X', or are '-'
    filter(
      !grepl('X', cty_code),
      !grepl('^0', cty_code),
      cty_code != '-'
    ) %>%
    filter(con_value > 0)
}


# =============================================================================
# 2. Map country codes to partner groups
# =============================================================================

map_to_partners <- function(data) {
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
# 3. Compute realized ETR at various aggregation levels
# =============================================================================

# Overall realized ETR by partner
calc_partner_realized_etr <- function(data) {
  data %>%
    group_by(partner) %>%
    summarise(
      cal_duty   = sum(cal_duty),
      con_value  = sum(con_value),
      dut_value  = sum(dut_value),
      .groups = 'drop'
    ) %>%
    mutate(
      realized_etr = cal_duty / con_value,
      dutiable_share = dut_value / con_value
    ) %>%
    arrange(desc(con_value))
}

# Overall total
calc_total_realized_etr <- function(data) {
  data %>%
    summarise(
      cal_duty   = sum(cal_duty),
      con_value  = sum(con_value),
      dut_value  = sum(dut_value)
    ) %>%
    mutate(
      realized_etr = cal_duty / con_value,
      dutiable_share = dut_value / con_value
    )
}

# By HS chapter x partner
calc_hs2_partner_realized_etr <- function(data) {
  data %>%
    mutate(hs2 = substr(hs_code, 1, 2)) %>%
    group_by(hs2, partner) %>%
    summarise(
      cal_duty   = sum(cal_duty),
      con_value  = sum(con_value),
      dut_value  = sum(dut_value),
      .groups = 'drop'
    ) %>%
    mutate(
      realized_etr = cal_duty / con_value,
      dutiable_share = dut_value / con_value
    )
}


# =============================================================================
# 4. Format and print results
# =============================================================================

format_pct <- function(x) sprintf('%.2f%%', x * 100)

print_partner_table <- function(partner_data, title) {
  cat(sprintf('\n%s\n%s\n\n', title, strrep('=', nchar(title))))

  # Add total row
  total <- partner_data %>%
    summarise(
      cal_duty = sum(cal_duty),
      con_value = sum(con_value),
      dut_value = sum(dut_value)
    ) %>%
    mutate(
      partner = 'TOTAL',
      realized_etr = cal_duty / con_value,
      dutiable_share = dut_value / con_value
    )

  display <- bind_rows(total, partner_data) %>%
    mutate(
      Partner = toupper(partner),
      `Import Value ($B)` = sprintf('$%.1f', con_value / 1e9),
      `Calc Duty ($B)` = sprintf('$%.1f', cal_duty / 1e9),
      `Realized ETR` = format_pct(realized_etr),
      `Dutiable Share` = format_pct(dutiable_share)
    ) %>%
    select(Partner, `Import Value ($B)`, `Calc Duty ($B)`, `Realized ETR`, `Dutiable Share`)

  print(as.data.frame(display), row.names = FALSE, right = FALSE)
}


print_top_gap_products <- function(hs2_data, n = 20) {
  cat('\n\nTop HS2 Chapters by Realized ETR (>$1B imports)\n')
  cat(strrep('=', 55), '\n\n')

  hs2_summary <- hs2_data %>%
    group_by(hs2) %>%
    summarise(
      cal_duty = sum(cal_duty),
      con_value = sum(con_value),
      .groups = 'drop'
    ) %>%
    mutate(realized_etr = cal_duty / con_value) %>%
    filter(con_value > 1e9) %>%
    arrange(desc(realized_etr)) %>%
    head(n) %>%
    mutate(
      `HS2` = hs2,
      `Import Value ($B)` = sprintf('$%.1f', con_value / 1e9),
      `Calc Duty ($B)` = sprintf('$%.1f', cal_duty / 1e9),
      `Realized ETR` = format_pct(realized_etr)
    ) %>%
    select(`HS2`, `Import Value ($B)`, `Calc Duty ($B)`, `Realized ETR`)

  print(as.data.frame(hs2_summary), row.names = FALSE, right = FALSE)
}


print_china_detail <- function(hs2_data) {
  cat('\n\nChina: Top HS2 Chapters by Realized ETR (>$500M imports)\n')
  cat(strrep('=', 60), '\n\n')

  china <- hs2_data %>%
    filter(partner == 'china') %>%
    filter(con_value > 5e8) %>%
    arrange(desc(realized_etr)) %>%
    mutate(
      `HS2` = hs2,
      `Import Value ($B)` = sprintf('$%.1f', con_value / 1e9),
      `Calc Duty ($B)` = sprintf('$%.1f', cal_duty / 1e9),
      `Realized ETR` = format_pct(realized_etr)
    ) %>%
    select(`HS2`, `Import Value ($B)`, `Calc Duty ($B)`, `Realized ETR`)

  print(as.data.frame(china), row.names = FALSE, right = FALSE)
}


# =============================================================================
# 5. Compare with our model's applied baseline
# =============================================================================

# Load our model's baseline output if available
load_applied_baseline <- function() {
  # Check if baseline output exists (from a prior run)
  levels_file <- 'output/2-21_temp/levels_by_sector_country.csv'
  if (!file.exists(levels_file)) {
    message('No baseline output found at ', levels_file)
    return(NULL)
  }

  # The first date's data represents the baseline + counterfactual level
  # We need the baseline level, which is level - delta
  # Actually, the baseline overall level is embedded in the output tables
  # Let's just read overall_levels.txt
  NULL
}


# =============================================================================
# 6. HS6-level analysis for detailed product gaps
# =============================================================================

download_hs6_for_partners <- function(year = 2024) {
  # Download HS6-level data for key partner countries
  # This is too much data for a single API call for all countries,
  # so we download by partner group

  partner_map <- read_csv(
    'resources/country_partner_mapping.csv',
    col_types = cols(cty_code = col_character()),
    show_col_types = FALSE
  )

  # Get unique country codes for mapped countries + a sample of ROW
  mapped_codes <- unique(partner_map$cty_code)

  # Download for each country code separately (API can filter by CTY_CODE)
  base_url <- 'https://api.census.gov/data/timeseries/intltrade/imports/hs'

  all_data <- list()

  for (code in mapped_codes) {
    url <- sprintf(
      '%s?get=CAL_DUT_YR,CON_VAL_YR,I_COMMODITY,CTY_CODE&YEAR=%d&MONTH=12&COMM_LVL=HS6&CTY_CODE=%s',
      base_url, year, code
    )

    tryCatch({
      raw <- fromJSON(url)
      if (nrow(raw) > 1) {
        headers <- raw[1, ]
        data <- as_tibble(raw[-1, ], .name_repair = 'minimal')
        names(data) <- headers
        all_data[[code]] <- data
      }
    }, error = function(e) {
      message(sprintf('  Skipping country %s: %s', code, e$message))
    })
  }

  bind_rows(all_data) %>%
    transmute(
      hs6       = I_COMMODITY,
      cty_code  = CTY_CODE,
      cal_duty  = as.numeric(CAL_DUT_YR),
      con_value = as.numeric(CON_VAL_YR)
    ) %>%
    filter(con_value > 0)
}


# =============================================================================
# 7. Load MFN rates and compute applied ETR for comparison
# =============================================================================

load_mfn_baseline <- function() {
  mfn <- read_csv(
    'resources/mfn_rates_2025.csv',
    col_types = cols(hs8 = col_character(), mfn_rate = col_double()),
    show_col_types = FALSE
  ) %>%
    select(hs8, mfn_rate)

  # Aggregate to HS2 level (unweighted average as a rough reference)
  mfn %>%
    mutate(hs2 = substr(hs8, 1, 2)) %>%
    group_by(hs2) %>%
    summarise(
      avg_mfn_rate = mean(mfn_rate),
      n_lines = n(),
      .groups = 'drop'
    )
}


# =============================================================================
# 8. Main execution
# =============================================================================

main <- function() {
  cat('\n')
  cat(strrep('*', 70), '\n')
  cat('  Realized vs Applied ETR Analysis — 2024 Trade Data\n')
  cat(strrep('*', 70), '\n')
  cat('\nData source: Census Bureau API (calculated duty / consumption value)\n')
  cat('Realized ETR = actual duties assessed / import value\n')
  cat('This differs from "applied" (statutory) rates due to FTAs, GSP,\n')
  cat('de minimis, FTZs, TRQs, and specific/compound duty conversions.\n')

  # ---- Download HS2 x country data ----
  hs2_data <- download_census_duties('HS2', 2024)
  hs2_data <- map_to_partners(hs2_data)

  message(sprintf('Downloaded %s HS2 x country records', format(nrow(hs2_data), big.mark = ',')))

  # ---- Partner-level summary ----
  partner_etr <- calc_partner_realized_etr(hs2_data)
  print_partner_table(partner_etr, 'Realized ETR by Partner Group (2024)')

  # ---- Compare with our model's baseline ----
  cat('\n\nComparison with Model Baseline ETR\n')
  cat(strrep('=', 40), '\n\n')
  cat('Our model\'s implied baseline ETR (from levels - deltas):\n\n')

  # Hardcode the baseline values from the user's earlier output
  model_baseline <- tibble(
    partner = c('TOTAL', 'china', 'mexico', 'row', 'ftrow', 'japan', 'canada', 'eu', 'uk'),
    applied_etr = c(0.0340, 0.1135, 0.0344, 0.0244, 0.0232, 0.0166, 0.0161, 0.0127, 0.0107)
  )

  comparison <- model_baseline %>%
    left_join(
      bind_rows(
        tibble(partner = 'TOTAL',
               realized_etr = sum(partner_etr$cal_duty) / sum(partner_etr$con_value)),
        partner_etr %>% select(partner, realized_etr)
      ),
      by = 'partner'
    ) %>%
    mutate(
      gap = applied_etr - realized_etr,
      gap_pct = gap / applied_etr
    )

  comparison_display <- comparison %>%
    mutate(
      Partner = toupper(partner),
      `Applied (Model)` = format_pct(applied_etr),
      `Realized (Census)` = format_pct(realized_etr),
      `Gap` = format_pct(gap),
      `Gap as % of Applied` = format_pct(gap_pct)
    ) %>%
    select(Partner, `Applied (Model)`, `Realized (Census)`, Gap, `Gap as % of Applied`)

  print(as.data.frame(comparison_display), row.names = FALSE, right = FALSE)

  # ---- HS2-level detail ----
  hs2_partner_etr <- calc_hs2_partner_realized_etr(hs2_data)
  print_top_gap_products(hs2_partner_etr)
  print_china_detail(hs2_partner_etr)

  # ---- MFN comparison at HS2 level ----
  cat('\n\nHS2 Chapter: Realized ETR vs Average MFN Rate (>$5B imports)\n')
  cat(strrep('=', 65), '\n\n')

  mfn_by_hs2 <- load_mfn_baseline()

  hs2_summary <- hs2_data %>%
    mutate(hs2 = substr(hs_code, 1, 2)) %>%
    group_by(hs2) %>%
    summarise(
      cal_duty = sum(cal_duty),
      con_value = sum(con_value),
      .groups = 'drop'
    ) %>%
    mutate(realized_etr = cal_duty / con_value) %>%
    left_join(mfn_by_hs2, by = 'hs2') %>%
    mutate(gap = avg_mfn_rate - realized_etr) %>%
    filter(con_value > 5e9) %>%
    arrange(desc(gap))

  hs2_display <- hs2_summary %>%
    mutate(
      `HS2` = hs2,
      `Import Val ($B)` = sprintf('$%.1f', con_value / 1e9),
      `Realized ETR` = format_pct(realized_etr),
      `Avg MFN Rate` = format_pct(avg_mfn_rate),
      `Gap (MFN - Realized)` = format_pct(gap)
    ) %>%
    select(`HS2`, `Import Val ($B)`, `Realized ETR`, `Avg MFN Rate`, `Gap (MFN - Realized)`)

  print(as.data.frame(hs2_display), row.names = FALSE, right = FALSE)

  # ---- FTA preference analysis ----
  cat('\n\nFTA Preference Analysis: Duty-Free Share by Partner\n')
  cat(strrep('=', 55), '\n\n')
  cat('Shows what fraction of imports enter duty-free (dutiable_value = 0)\n\n')

  # Approximate: where dutiable value is 0 or very small relative to import value
  fta_analysis <- hs2_data %>%
    group_by(partner) %>%
    summarise(
      total_imports = sum(con_value),
      duty_free_imports = sum(con_value[dut_value == 0]),
      dutiable_imports = sum(con_value[dut_value > 0]),
      .groups = 'drop'
    ) %>%
    mutate(
      duty_free_share = duty_free_imports / total_imports,
      dutiable_share = dutiable_imports / total_imports
    ) %>%
    arrange(desc(duty_free_share))

  fta_display <- fta_analysis %>%
    mutate(
      Partner = toupper(partner),
      `Total Imports ($B)` = sprintf('$%.1f', total_imports / 1e9),
      `Duty-Free ($B)` = sprintf('$%.1f', duty_free_imports / 1e9),
      `Duty-Free Share` = format_pct(duty_free_share)
    ) %>%
    select(Partner, `Total Imports ($B)`, `Duty-Free ($B)`, `Duty-Free Share`)

  print(as.data.frame(fta_display), row.names = FALSE, right = FALSE)

  # ---- FTA preference by partner x HS2 ----
  cat('\n\nCanada & Mexico: Highest Realized ETR Chapters (>$1B imports)\n')
  cat(strrep('=', 60), '\n')
  cat('(These should mostly be zero under USMCA, gaps indicate non-qualifying goods)\n\n')

  for (p in c('canada', 'mexico')) {
    cat(sprintf('--- %s ---\n', toupper(p)))
    p_data <- hs2_partner_etr %>%
      filter(partner == p, con_value > 1e9) %>%
      arrange(desc(realized_etr)) %>%
      head(10) %>%
      mutate(
        `HS2` = hs2,
        `Import Val ($B)` = sprintf('$%.1f', con_value / 1e9),
        `Realized ETR` = format_pct(realized_etr)
      ) %>%
      select(`HS2`, `Import Val ($B)`, `Realized ETR`)

    print(as.data.frame(p_data), row.names = FALSE, right = FALSE)
    cat('\n')
  }

  # ---- Save detailed data ----
  output_dir <- 'analysis/output'
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  write_csv(partner_etr, file.path(output_dir, 'realized_etr_by_partner_2024.csv'))
  write_csv(hs2_partner_etr, file.path(output_dir, 'realized_etr_by_hs2_partner_2024.csv'))
  write_csv(comparison, file.path(output_dir, 'applied_vs_realized_comparison_2024.csv'))

  message(sprintf('\nDetailed data saved to %s/', output_dir))

  cat('\n')
  cat(strrep('*', 70), '\n')
  cat('  Analysis complete\n')
  cat(strrep('*', 70), '\n')

  invisible(list(
    partner_etr = partner_etr,
    hs2_partner_etr = hs2_partner_etr,
    comparison = comparison,
    hs2_data = hs2_data
  ))
}


# Run
results <- main()
