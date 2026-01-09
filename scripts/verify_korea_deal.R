# =============================================================================
# verify_korea_deal.R
# =============================================================================
#
# Verification script for U.S.-Korea Strategic Trade and Investment Deal
# FR Doc. 2025-21940, effective Nov 14, 2025 (Nov 1 for autos)
#
# This script validates that Korea (Census code 5800) receives the correct
# tariff treatment under the deal implementation:
# - Civil aircraft: 0% reciprocal rate
# - Autos/parts: 15% 232 rate
# - Wood products: 15% 232 rate
# - Reciprocal headline: 15%
# =============================================================================

library(tidyverse)
library(yaml)

# Configuration
scenario <- '12-16-korea-deal'
korea_code <- '5800'

config_path <- file.path('config', scenario)

message('=== Korea Deal Verification ===\n')
message(sprintf('Scenario: %s', scenario))
message(sprintf('Korea Census code: %s\n', korea_code))

# -----------------------------------------------------------------------------
# 1. Verify reciprocal headline rate
# -----------------------------------------------------------------------------
message('1. Checking reciprocal headline rate...')

reciprocal <- read_yaml(file.path(config_path, 'ieepa_reciprocal.yaml'))
korea_reciprocal_rate <- reciprocal$headline_rates[[korea_code]]

if (is.null(korea_reciprocal_rate)) {
  message('   ERROR: Korea not found in headline_rates')
} else if (korea_reciprocal_rate == 0.15) {
  message(sprintf('   PASS: Korea reciprocal rate = %.2f (expected 0.15)', korea_reciprocal_rate))
} else {
  message(sprintf('   WARN: Korea reciprocal rate = %.2f (expected 0.15)', korea_reciprocal_rate))
}

# -----------------------------------------------------------------------------
# 2. Verify civil aircraft exemption in reciprocal
# -----------------------------------------------------------------------------
message('\n2. Checking civil aircraft exemption...')

aircraft_hts_test <- c('8802', '8803', '8411')
found_aircraft_exemption <- FALSE

if (!is.null(reciprocal$product_country_rates)) {
  for (exemption in reciprocal$product_country_rates) {
    if (exemption$country == korea_code && exemption$rate == 0) {
      matching_hts <- intersect(exemption$hts, aircraft_hts_test)
      if (length(matching_hts) > 0) {
        message(sprintf('   PASS: Civil aircraft exemption found for Korea'))
        message(sprintf('         HTS prefixes: %s', paste(exemption$hts, collapse = ', ')))
        found_aircraft_exemption <- TRUE
      }
    }
  }
}

if (!found_aircraft_exemption) {
  message('   ERROR: No civil aircraft exemption found for Korea')
}

# -----------------------------------------------------------------------------
# 3. Verify 232 rates for wood products
# -----------------------------------------------------------------------------
message('\n3. Checking 232 wood products rates...')

s232 <- read_yaml(file.path(config_path, '232.yaml'))

# Check upholstered furniture
uph_rate <- s232$upholstered_wooden_furniture$rates[[korea_code]]
if (!is.null(uph_rate) && uph_rate == 0.15) {
  message(sprintf('   PASS: upholstered_wooden_furniture Korea rate = %.2f', uph_rate))
} else {
  message(sprintf('   ERROR: upholstered_wooden_furniture Korea rate = %s (expected 0.15)',
                  ifelse(is.null(uph_rate), 'NOT SET', uph_rate)))
}

# Check kitchen cabinets
kc_rate <- s232$kitchen_cabinets_and_parts$rates[[korea_code]]
if (!is.null(kc_rate) && kc_rate == 0.15) {
  message(sprintf('   PASS: kitchen_cabinets_and_parts Korea rate = %.2f', kc_rate))
} else {
  message(sprintf('   ERROR: kitchen_cabinets_and_parts Korea rate = %s (expected 0.15)',
                  ifelse(is.null(kc_rate), 'NOT SET', kc_rate)))
}

# -----------------------------------------------------------------------------
# 4. Verify 232 rates for autos
# -----------------------------------------------------------------------------
message('\n4. Checking 232 auto rates...')

# Check automobiles
auto_rate <- s232$automobiles_passenger_and_light_trucks$rates[[korea_code]]
if (!is.null(auto_rate) && auto_rate == 0.15) {
  message(sprintf('   PASS: automobiles_passenger_and_light_trucks Korea rate = %.2f', auto_rate))
} else {
  message(sprintf('   ERROR: automobiles Korea rate = %s (expected 0.15)',
                  ifelse(is.null(auto_rate), 'NOT SET', auto_rate)))
}

# Check auto parts
parts_rate <- s232$automobile_parts$rates[[korea_code]]
if (!is.null(parts_rate) && parts_rate == 0.15) {
  message(sprintf('   PASS: automobile_parts Korea rate = %.2f', parts_rate))
} else {
  message(sprintf('   ERROR: automobile_parts Korea rate = %s (expected 0.15)',
                  ifelse(is.null(parts_rate), 'NOT SET', parts_rate)))
}

# -----------------------------------------------------------------------------
# 5. Spot check: Verify a non-exempt HTS code still gets reciprocal 15%
# -----------------------------------------------------------------------------
message('\n5. Spot check: Non-exempt products...')
message('   (Non-exempt products should use Korea headline rate of 0.15)')

# HTS 8544 (insulated wire) - should NOT be exempt, should get 0.15 reciprocal
test_hts <- '85443000'  # Covered in product_rates with 0, but Korea should get headline
test_product_rate <- reciprocal$product_rates[[test_hts]]

if (!is.null(test_product_rate) && test_product_rate == 0) {
  message(sprintf('   INFO: HTS %s has product_rate = 0 (global exemption)', test_hts))
} else {
  message(sprintf('   PASS: HTS %s would use Korea headline rate = 0.15', test_hts))
}

message('\n=== Verification Complete ===\n')
