# =============================================================================
# main.R
# =============================================================================
#
# This script runs tariff analysis for one or more scenarios, computing
# deltas (counterfactual - baseline) and absolute tariff levels.
# All scenarios share a baseline config (config/baseline/); each scenario
# provides counterfactual config(s):
#   - Section 232 tariffs (steel, aluminum, softwood, furniture, autos, etc.)
#   - IEEPA tariffs (residual catch-all for imports not covered by 232)
#   - USMCA exemptions with content requirements
#
# Usage (interactive):
#   1. Specify scenario names in the 'scenarios' vector below
#   2. Ensure corresponding config files exist in config/{scenario}/
#   3. Run: source('src/main.R')
#
# Usage (command line):
#   Rscript src/main.R --scenario 11-17 --scenario 12-16
#   Rscript src/main.R --scenario 11-17 --config-dir /path/to/config --output-dir /path/to/output
#   Rscript src/main.R --scenario 11-17 --no-cache
#
# CLI arguments:
#   --scenario       Scenario name (can be specified multiple times)
#   --config-dir     Path to config directory (default: config/)
#   --output-dir     Path to output directory (default: output/)
#   --import-data-path  Path to Census import data (default: C:/Users/jar335/Downloads)
#   --no-cache       Disable cache, force re-processing of import data
#
# Each scenario will produce output files in {output-dir}/{scenario}/
#
# =============================================================================

library(tidyverse)
library(yaml)

# Determine script directory for sourcing helper files
# This allows the script to be called from any working directory
get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep('--file=', args, value = TRUE)
  if (length(file_arg) > 0) {
    # Called via Rscript
    path <- sub('--file=', '', file_arg)
    return(dirname(normalizePath(path)))
  } else {
    # Sourced interactively - assume we're in project root
    return('src')
  }
}

script_dir <- get_script_dir()

# Load helper functions
source(file.path(script_dir, 'config_parsing.R'))
source(file.path(script_dir, 'data_processing.R'))
source(file.path(script_dir, 'calculations.R'))


#------------------
# Parse CLI arguments
#------------------

parse_cli_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)

  # Defaults
  config <- list(
    scenarios = character(0),
    config_dir = 'config',
    output_dir = 'output',
    import_data_path = 'C:/Users/jar335/Downloads',
    use_cache = TRUE
  )

  # No args = interactive mode, return NULL to signal use of hardcoded defaults
  if (length(args) == 0) {
    return(NULL)
  }

  i <- 1
  while (i <= length(args)) {
    arg <- args[i]

    if (arg == '--scenario') {
      i <- i + 1
      config$scenarios <- c(config$scenarios, args[i])
    } else if (arg == '--config-dir') {
      i <- i + 1
      config$config_dir <- args[i]
    } else if (arg == '--output-dir') {
      i <- i + 1
      config$output_dir <- args[i]
    } else if (arg == '--import-data-path') {
      i <- i + 1
      config$import_data_path <- args[i]
    } else if (arg == '--no-cache') {
      config$use_cache <- FALSE
    } else {
      stop(paste('Unknown argument:', arg))
    }

    i <- i + 1
  }

  if (length(config$scenarios) == 0) {
    stop('At least one --scenario is required when using CLI mode')
  }

  return(config)
}


#------------------
# Configuration
#------------------

cli_config <- parse_cli_args()

if (is.null(cli_config)) {
  # Interactive mode: use hardcoded defaults
  config_dir <- 'config'
  output_dir <- 'output'
  import_data_path <- 'C:/Users/jar335/Downloads'
  use_cache <- TRUE
  scenarios <- c(
    '2-21_perm',
    '2-21_temp'
  )
} else {
  # CLI mode: use parsed arguments
  config_dir <- cli_config$config_dir
  output_dir <- cli_config$output_dir
  import_data_path <- cli_config$import_data_path
  use_cache <- cli_config$use_cache
  scenarios <- cli_config$scenarios
}


#------------------
# Run scenarios
#------------------

# Run each scenario
for (scenario in scenarios) {
  do_scenario(
    scenario         = scenario,
    config_dir       = config_dir,
    output_dir       = output_dir,
    import_data_path = import_data_path,
    use_cache        = use_cache
  )
}


