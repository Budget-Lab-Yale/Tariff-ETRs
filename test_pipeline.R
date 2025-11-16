# Test script for the refactored pipeline
# Tests the full ETR calculation with HS10 data and new 232.yaml structure

library(tidyverse)
library(yaml)

cat('Testing refactored ETR pipeline with HS10 data...\n\n')

# Source functions
source('src/functions.R')

# Test with 10-30 scenario
cat('Running scenario: 10-30\n')
cat('======================================\n\n')

# Run the scenario
result <- do_scenario(scenario = '10-30')

cat('\n\n======================================\n')
cat('Test completed successfully!\n')
