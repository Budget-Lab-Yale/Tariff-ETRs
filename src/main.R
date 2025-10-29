# =============================================================================
# main.R
# =============================================================================
#
# This script runs effective tariff rate (ETR) analysis for one or more
# scenarios. Each scenario consists of:
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
source('src/functions.R')


#------------------
# Configuration
#------------------

# Path to Census Bureau import data files
import_data_path <- 'C:/Users/jar335/Downloads'

# Scenarios to run (add/remove as needed)
scenarios <- c(
  '10_29'
)


#------------------
# Run scenarios
#------------------

# Run each scenario
for (scenario in scenarios) {
  do_scenario(
    scenario         = scenario,
    import_data_path = import_data_path
  )
}

message('\n==========================================================')
message(sprintf('All scenarios complete! Ran %d scenario(s).', length(scenarios)))
message('==========================================================\n')
