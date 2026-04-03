# Regenerate ETRs configs from individual tracker snapshots
# (Avoids the OOM from combining 38 snapshots into one 950MB+ timeseries)

tracker_dir <- 'C:/Users/jar335/Documents/Repositories/tariff-rate-tracker'
etrs_dir <- 'C:/Users/jar335/Documents/Repositories/Tariff-ETRs'

# Source tracker code from tracker directory
setwd(tracker_dir)
library(here)
source(here('src', 'helpers.R'))
source(here('src', 'generate_etrs_config.R'))

pp <- load_policy_params()

# Load revision dates to map snapshot files -> dates
rev_dates <- read.csv(here('config', 'revision_dates.csv'), stringsAsFactors = FALSE)
cat(sprintf('Found %d revisions\n', nrow(rev_dates)))

# Ch99 data for deal rates
ch99_path <- here('data', 'processed', 'chapter99_rates.rds')
ch99_data <- if (file.exists(ch99_path)) readRDS(ch99_path) else NULL

setwd(etrs_dir)

# Process each snapshot individually
for (i in seq_len(nrow(rev_dates))) {
  rev_id <- rev_dates$revision[i]
  date_str <- rev_dates$effective_date[i]

  snapshot_path <- file.path(tracker_dir, 'data', 'timeseries',
                             paste0('snapshot_', rev_id, '.rds'))
  if (!file.exists(snapshot_path)) {
    cat(sprintf('  SKIP %s: snapshot not found\n', rev_id))
    next
  }

  snapshot <- readRDS(snapshot_path)
  output_dir <- file.path('config', 'historical', date_str)

  generate_etrs_config(
    ts = snapshot,
    date = date_str,
    output_dir = output_dir,
    policy_params = pp,
    etrs_resources_dir = 'resources',
    ch99_data = ch99_data
  )

  # Free memory
  rm(snapshot)
  gc(verbose = FALSE)
}

cat('\n========================================\n')
cat('Config regeneration complete.\n')
cat('========================================\n')

# Now run the test scenario
cat('\nRunning usmca_test_h2avg scenario...\n\n')

# Make sure h2avg is the active USMCA file
file.copy('resources/usmca_product_shares_2025_h2avg.csv',
          'resources/usmca_product_shares_2025.csv', overwrite = TRUE)

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

# Compare
cat('\n========================================\n')
cat('COMPARISON: before vs after regen\n')
cat('========================================\n\n')

before <- readLines('output/usmca_test_h2avg/overall_levels_BEFORE_REGEN.txt')
after <- readLines('output/usmca_test_h2avg/overall_levels.txt')

if (identical(before, after)) {
  cat('RESULT: IDENTICAL -- configs from tracker produce same ETR results\n')
} else {
  cat('RESULT: DIFFERENCES FOUND\n\n')
  cat('=== BEFORE ===\n')
  cat(paste(before, collapse = '\n'), '\n\n')
  cat('=== AFTER ===\n')
  cat(paste(after, collapse = '\n'), '\n')
}
