# Regen just the remaining dates that ran out of memory
tracker_dir <- 'C:/Users/jar335/Documents/Repositories/tariff-rate-tracker'

source(file.path(tracker_dir, 'src', 'helpers.R'))
source(file.path(tracker_dir, 'src', 'generate_etrs_config.R'))

pp_path <- file.path(tracker_dir, 'config', 'policy_params.yaml')
policy_params <- load_policy_params(yaml_path = pp_path)

ts_path <- file.path(tracker_dir, 'data', 'timeseries', 'rate_timeseries.rds')
message(sprintf('Loading timeseries from %s ...', ts_path))
ts <- readRDS(ts_path)
message(sprintf('Loaded %s rows', format(nrow(ts), big.mark = ',')))

remaining_dates <- c('2026-02-12', '2026-02-24')

for (d in remaining_dates) {
  output_dir <- file.path('config/historical', d)
  generate_etrs_config(
    ts = ts,
    date = d,
    output_dir = output_dir,
    policy_params = policy_params,
    etrs_resources_dir = 'resources',
    tracker_root = tracker_dir
  )
}
