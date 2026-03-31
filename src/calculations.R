# =============================================================================
# calculations.R — compatibility shim
# =============================================================================
#
# This file was split into four focused modules:
#   - outputs.R:            Constants and output writing functions
#   - config_overlay.R:     Config loading and overlay merge logic
#   - etr_engine.R:         Core tariff calculation functions
#   - scenario_workflow.R:  Scenario orchestration (do_scenario, etc.)
#
# Scripts that source('src/calculations.R') will get all four modules loaded.
# New code should source the individual files via main.R instead.
#
# =============================================================================

# Determine script directory (same logic as main.R)
.calc_script_dir <- if (exists('script_dir')) {
  script_dir
} else {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep('--file=', args, value = TRUE)
  if (length(file_arg) > 0) {
    dirname(normalizePath(sub('--file=', '', file_arg)))
  } else {
    'src'
  }
}

source(file.path(.calc_script_dir, 'outputs.R'))
source(file.path(.calc_script_dir, 'config_overlay.R'))
source(file.path(.calc_script_dir, 'etr_engine.R'))
source(file.path(.calc_script_dir, 'scenario_workflow.R'))
