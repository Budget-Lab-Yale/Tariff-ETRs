# Regenerate all historical configs from the tracker's timeseries
# Requires: tariff-rate-tracker repo with rate_timeseries.rds already built

tracker_dir <- 'C:/Users/jar335/Documents/Repositories/tariff-rate-tracker'

# Source tracker code
source(file.path(tracker_dir, 'src', 'helpers.R'))
source(file.path(tracker_dir, 'src', 'generate_etrs_config.R'))

# Load policy params from the tracker's config (not ETRs)
pp_path <- file.path(tracker_dir, 'config', 'policy_params.yaml')
policy_params <- load_policy_params(yaml_path = pp_path)

# Load timeseries
ts_path <- file.path(tracker_dir, 'data', 'timeseries', 'rate_timeseries.rds')
message(sprintf('Loading timeseries from %s ...', ts_path))
ts <- readRDS(ts_path)
message(sprintf('Loaded %s rows', format(nrow(ts), big.mark = ',')))

# Output to our config/historical directory
output_base <- 'config/historical'
etrs_resources <- 'resources'

# Regenerate all revision configs
generate_etrs_configs_all_revisions(
  ts = ts,
  output_base = output_base,
  policy_params = policy_params,
  etrs_resources_dir = etrs_resources,
  tracker_root = tracker_dir
)
