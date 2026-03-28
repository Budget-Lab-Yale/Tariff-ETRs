# Export tracker snapshots to config/historical/
# Run from the tariff-rate-tracker directory:
#   Rscript ../Tariff-ETRs/scripts/export_historical.R

library(tidyverse)
library(here)
library(yaml)

source(here('src', 'helpers.R'))
source(here('src', 'generate_etrs_config.R'))

output_base <- 'C:/Users/jar335/Documents/Repositories/Tariff-ETRs/config/historical'
etrs_resources <- 'C:/Users/jar335/Documents/Repositories/Tariff-ETRs/resources'
policy_params <- load_policy_params()

# Load ch99 data if available
ch99_path <- here('data', 'processed', 'chapter99_rates.rds')
ch99_data <- if (file.exists(ch99_path)) readRDS(ch99_path) else NULL

# Get all snapshot files
snap_dir <- here('data', 'timeseries')
snap_files <- list.files(snap_dir, pattern = '^snapshot_.*\\.rds$', full.names = TRUE)
message(sprintf('Found %d snapshot files', length(snap_files)))

successes <- 0
failures <- 0

for (sf in snap_files) {
  snap <- readRDS(sf)
  # valid_from is only set during combine; use effective_date from snapshot
  date_str <- as.character(snap$effective_date[1])
  if (is.na(date_str)) {
    message(sprintf('  SKIP: %s has no effective_date', basename(sf)))
    next
  }
  out_dir <- file.path(output_base, date_str)

  message(sprintf('\n=== Exporting %s (from %s) ===', date_str, basename(sf)))

  tryCatch({
    generate_etrs_config(
      ts = snap,
      date = as.Date(date_str),
      output_dir = out_dir,
      policy_params = policy_params,
      etrs_resources_dir = etrs_resources,
      ch99_data = ch99_data
    )
    message(sprintf('  OK: %s', out_dir))
    successes <- successes + 1
  }, error = function(e) {
    message(sprintf('  FAILED: %s', conditionMessage(e)))
    failures <<- failures + 1
  })

  rm(snap)
  gc(verbose = FALSE)
}

message(sprintf('\nDone! %d succeeded, %d failed', successes, failures))
