# Full end-to-end: rebuild tracker timeseries, regenerate ETRs configs,
# run the 3-date test scenario, compare against h2avg results.

tracker_dir <- 'C:/Users/jar335/Documents/Repositories/tariff-rate-tracker'
etrs_dir <- 'C:/Users/jar335/Documents/Repositories/Tariff-ETRs'

cat('\n========================================\n')
cat('STEP 1: Rebuild tracker timeseries\n')
cat('========================================\n\n')

# Run the full build from the tracker directory
setwd(tracker_dir)
source(file.path(tracker_dir, 'src', '00_build_timeseries.R'))

build_full_timeseries(
  start_from = NULL  # Full rebuild
)

cat('\n========================================\n')
cat('STEP 2: Regenerate ETRs configs\n')
cat('========================================\n\n')

# Now source generate_etrs_config (helpers.R already loaded by 00_build)
source(file.path(tracker_dir, 'src', 'generate_etrs_config.R'))

# Load freshly built timeseries
ts <- readRDS(file.path(tracker_dir, 'data', 'timeseries', 'rate_timeseries.rds'))
pp <- load_policy_params()

setwd(etrs_dir)

generate_etrs_configs_all_revisions(
  ts = ts,
  output_base = 'config/historical',
  policy_params = pp,
  etrs_resources_dir = 'resources'
)

cat('\n========================================\n')
cat('STEP 3: Run ETRs test scenario (h2avg)\n')
cat('========================================\n\n')

# Make sure h2avg is the active USMCA file
file.copy('resources/usmca_product_shares_2025_h2avg.csv',
          'resources/usmca_product_shares_2025.csv', overwrite = TRUE)

# Source ETRs code
source('src/config_parsing.R')
source('src/data_processing.R')
source('src/calculations.R')

do_scenario(
  scenario = 'usmca_test_h2avg',
  config_dir = 'config',
  output_dir = 'output',
  import_data_path = 'C:/Users/jar335/Downloads',
  use_cache = TRUE
)

cat('\n========================================\n')
cat('STEP 4: Compare against previous results\n')
cat('========================================\n\n')

# Read the overall levels from the new run
new_levels <- readLines('output/usmca_test_h2avg/overall_levels.txt')
cat('=== New results (after full tracker regen) ===\n')
cat(paste(new_levels, collapse = '\n'), '\n')

cat('\n========================================\n')
cat('DONE\n')
cat('========================================\n')
