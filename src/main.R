# =============================================================================
# main.R
# =============================================================================
#
# This script runs effective tariff rate (ETR) analysis for one or more
# scenarios. ETR values represent changes from an early 2025 baseline.
# Each scenario consists of:
#   - Section 232 tariffs (steel, aluminum, softwood, furniture, autos, etc.)
#   - IEEPA tariffs (residual catch-all for imports not covered by 232)
#   - USMCA exemptions with content requirements
#
# Usage:
#   1. Specify scenario names in the 'scenarios' vector below
#   2. Ensure corresponding config files exist in config/{scenario}/
#   3. Run: source('src/main.R')
#
# Each scenario will produce output files in output/{scenario}/
#
# =============================================================================

library(tidyverse)
library(yaml)

# Load helper functions
source('src/config_parsing.R')
source('src/data_processing.R')
source('src/calculations.R')


#------------------
# Configuration
#------------------

# Path to Census Bureau import data files
import_data_path <- 'C:/Users/jar335/Downloads'

# Use cached data if available (set to FALSE to force re-processing)
use_cache <- T

# Scenarios to run (add/remove as needed)
scenarios <- c(
  '10-30', 
  '10-30-ex-ieepa'
)


#------------------
# Run scenarios
#------------------

# Run each scenario
for (scenario in scenarios) {
  do_scenario(
    scenario         = scenario,
    import_data_path = import_data_path,
    use_cache        = use_cache
  )
}


