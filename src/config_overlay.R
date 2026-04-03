# =============================================================================
# config_overlay.R
# =============================================================================
#
# Functions for loading and merging tariff scenario configurations.
# Handles CSV-based configs from tariff-rate-tracker, YAML reform overlays,
# and authority-level merge logic.
#
# Functions:
#   - load_scenario_config():    Load CSV config from a historical directory
#   - load_config_with_reform(): Load historical config + optional reform overlay
#   - merge_other_params():      Shallow-merge reform other_params over historical
#   - load_yaml_overlay():       Load YAML rate override files
#   - merge_s232_rate_matrix():  Column-level merge for S232 rate matrix
#   - merge_overlay():           Merge YAML overlay onto CSV baseline config
#
# =============================================================================


#' Load config from a directory (CSV mode required)
#'
#' Loads statutory_rates.csv.gz (required) and other_params.yaml (required)
#' from the given directory. Optionally applies YAML overlay files co-located
#' in the same directory.
#'
#' @param config_path Path to directory containing statutory_rates.csv.gz + other_params.yaml
#'
#' @return Named list with: params_s232, rates_ieepa_reciprocal, rates_ieepa_fentanyl,
#'   rates_s122, rates_s301, rates_s201, rates_other, other_params, mfn_rates, etc.
load_scenario_config <- function(config_path) {

  message(sprintf('Loading scenario parameters from %s...', config_path))

  other_params <- read_yaml(file.path(config_path, 'other_params.yaml'))

  # Load product-level USMCA shares (required for active configs, optional for archive)
  usmca_shares_path <- other_params$usmca_product_shares
  usmca_product_shares <- if (!is.null(usmca_shares_path) && file.exists(usmca_shares_path)) {
    shares <- read_csv(usmca_shares_path, show_col_types = FALSE,
                       col_types = cols(hts10 = col_character(), cty_code = col_character()))
    message(sprintf('  Loaded product-level USMCA shares: %s rows', format(nrow(shares), big.mark = ',')))
    shares
  } else if (!is.null(usmca_shares_path)) {
    stop(sprintf('usmca_product_shares file not found: %s', usmca_shares_path))
  } else {
    message('  No usmca_product_shares specified — USMCA exemptions disabled')
    tibble(hts10 = character(), cty_code = character(), usmca_share = numeric())
  }

  # =========================================================================
  # CSV path: statutory_rates.csv.gz from tracker
  # =========================================================================

  csv_path <- file.path(config_path, 'statutory_rates.csv.gz')
  if (!file.exists(csv_path)) {
    stop(sprintf('statutory_rates.csv.gz not found in %s — CSV mode is required', config_path))
  }

  config <- load_statutory_csv(csv_path, other_params, usmca_product_shares)

  # Load MFN exemption shares (CSV mfn_rate is statutory; exemptions applied by ETRs)
  if (!is.null(other_params$mfn_exemption_shares)) {
    config$mfn_exemption_shares <- load_mfn_exemption_shares(other_params$mfn_exemption_shares)
  }

  # Check for YAML overlay files (counterfactual modifications)
  yaml_overlay <- load_yaml_overlay(config_path, overlay_mode = TRUE)
  if (!is.null(yaml_overlay)) {
    config <- merge_overlay(config, yaml_overlay)
  }

  config
}


#' Load a historical config with optional reform overlay
#'
#' Loads the CSV-based config from a historical directory, then optionally
#' applies a reform overlay (other_params merge + YAML rate overlays).
#'
#' @param historical_path Path to historical config directory (must contain statutory_rates.csv.gz)
#' @param reform_path Path to reform directory (optional, NULL = no reform)
#'
#' @return Config list (same schema as load_scenario_config)
load_config_with_reform <- function(historical_path, reform_path = NULL) {

  config <- load_scenario_config(historical_path)

  if (is.null(reform_path) || !dir.exists(reform_path)) {
    return(config)
  }

  message(sprintf('  Applying reform overlay from %s...', reform_path))

  # Merge other_params if reform provides overrides
  config$other_params <- merge_other_params(config$other_params, reform_path)

  # Disable entire authorities if specified in other_params
  disabled <- config$other_params$disable_authorities
  if (!is.null(disabled)) {
    authority_keys <- list(
      s232 = 'params_s232', ieepa_reciprocal = 'rates_ieepa_reciprocal',
      ieepa_fentanyl = 'rates_ieepa_fentanyl', s122 = 'rates_s122',
      s301 = 'rates_s301', s201 = 'rates_s201', other = 'rates_other'
    )
    for (auth in disabled) {
      key <- authority_keys[[auth]]
      if (!is.null(key)) {
        config[[key]] <- NULL
        message(sprintf('  Disabled authority: %s', auth))
      }
    }
  }

  # Load and merge YAML rate overlays
  yaml_overlay <- load_yaml_overlay(reform_path, overlay_mode = TRUE)
  if (!is.null(yaml_overlay)) {
    config <- merge_overlay(config, yaml_overlay)
  }

  config
}


#' Shallow-merge reform other_params over historical other_params
#'
#' Uses modifyList() for a shallow merge: top-level keys from the reform
#' override the historical values. To override nested keys (e.g., metal_content),
#' include the full block in the reform other_params.yaml.
#'
#' @param base_params Named list from historical other_params.yaml
#' @param reform_path Path to reform directory
#'
#' @return Merged other_params list
merge_other_params <- function(base_params, reform_path) {
  reform_params_file <- file.path(reform_path, 'other_params.yaml')
  if (!file.exists(reform_params_file)) return(base_params)
  reform_params <- read_yaml(reform_params_file)
  message(sprintf('  Merging reform other_params: %s', paste(names(reform_params), collapse = ', ')))
  modifyList(base_params, reform_params)
}


#' Load YAML overlay files for counterfactual modifications on CSV baseline
#'
#' Scans a config directory for YAML tariff files and loads them with overlay_mode=TRUE
#' (preserving explicit zeros). Returns NULL if no YAML tariff files are found.
#'
#' @param config_path Path to config directory
#' @param overlay_mode Passed to YAML loaders (TRUE preserves zeros)
#'
#' @return Named list of loaded configs, or NULL if no YAML files found
load_yaml_overlay <- function(config_path, overlay_mode = TRUE) {

  yaml_files <- list(
    s232              = file.path(config_path, 's232.yaml'),
    ieepa_reciprocal  = file.path(config_path, 'ieepa_reciprocal.yaml'),
    ieepa_fentanyl    = file.path(config_path, 'ieepa_fentanyl.yaml'),
    s122              = file.path(config_path, 's122.yaml'),
    s301              = file.path(config_path, 's301.yaml'),
    s201              = file.path(config_path, 's201.yaml'),
    other             = file.path(config_path, 'other.yaml')
  )

  found <- vapply(yaml_files, file.exists, logical(1))
  if (!any(found)) return(NULL)

  message(sprintf('  Loading YAML overlay: %s', paste(names(found)[found], collapse = ', ')))

  overlay <- list()

  if (found[['s232']]) {
    overlay$params_s232 <- load_s232_rates(yaml_files[['s232']], overlay_mode = overlay_mode)
  }

  rate_map <- list(
    ieepa_reciprocal = 'ieepa_reciprocal_rate',
    ieepa_fentanyl   = 'ieepa_fentanyl_rate',
    s122             = 's122_rate',
    s301             = 's301_rate',
    s201             = 's201_rate',
    other            = 'other_rate'
  )

  for (authority in names(rate_map)) {
    if (found[[authority]]) {
      overlay[[paste0('rates_', authority)]] <- load_ieepa_rates_yaml(
        yaml_files[[authority]],
        rate_col_name = rate_map[[authority]],
        overlay_mode = overlay_mode
      )
    }
  }

  overlay
}


#' Column-level merge for S232 rate matrix
#'
#' Unlike single-column authorities, S232 has multiple program columns
#' (s232_steel_rate, s232_aluminum_rate, etc.). This function merges at
#' the column level: overlay only updates the program columns it defines,
#' leaving other programs' rates unchanged for matching (hs10, cty_code) rows.
#'
#' @param base_matrix Base S232 rate_matrix tibble
#' @param overlay_matrix Overlay S232 rate_matrix tibble (may have fewer columns)
#'
#' @return Merged rate_matrix tibble
merge_s232_rate_matrix <- function(base_matrix, overlay_matrix) {
  if (is.null(overlay_matrix)) return(base_matrix)
  if (is.null(base_matrix)) return(overlay_matrix)

  overlay_cols <- setdiff(names(overlay_matrix), c('hs10', 'cty_code'))
  base_only_cols <- setdiff(names(base_matrix), c('hs10', 'cty_code', overlay_cols))

  # Rows not touched by overlay: keep as-is
  untouched <- base_matrix %>%
    anti_join(overlay_matrix, by = c('hs10', 'cty_code'))

  # Overlay rows: take overlay columns, join base-only columns from base
  updated <- overlay_matrix %>%
    left_join(
      base_matrix %>% select(hs10, cty_code, all_of(base_only_cols)),
      by = c('hs10', 'cty_code')
    )

  bind_rows(untouched, updated)
}


#' Merge YAML overlay onto CSV baseline config
#'
#' For each authority present in the overlay, YAML rates overwrite CSV baseline
#' rates for matching (hs10, cty_code) pairs. New rows from YAML are added.
#' Explicit zeros in YAML suppress positive CSV baseline rates.
#'
#' @param base_config Config list from load_statutory_csv()
#' @param overlay Config list from load_yaml_overlay()
#'
#' @return Merged config list
merge_overlay <- function(base_config, overlay) {

  # Helper: merge a rate tibble (anti_join + bind_rows upsert pattern)
  merge_rate_tibble <- function(base_tibble, overlay_tibble) {
    if (is.null(overlay_tibble)) return(base_tibble)
    if (is.null(base_tibble)) return(overlay_tibble)

    # Remove matching rows from base, then add all overlay rows
    base_tibble %>%
      anti_join(overlay_tibble, by = c('hs10', 'cty_code')) %>%
      bind_rows(overlay_tibble)
  }

  # Merge s232 (special: has rate_matrix, usmca_exempt, target_total_rules)
  # An overlay with an empty rate_matrix means "set all 232 to zero" (e.g., default: 0).
  # S232 uses column-level merge: overlay only touches the program columns it defines,
  # leaving other programs' rates unchanged for matching (hs10, cty_code) rows.
  if (!is.null(overlay$params_s232)) {
    overlay_s232_empty <- is.null(overlay$params_s232$rate_matrix) ||
      nrow(overlay$params_s232$rate_matrix) == 0
    if (overlay_s232_empty && !is.null(base_config$params_s232)) {
      # Overlay says "zero all 232" — suppress base entirely
      message('  s232 overlay has no positive rates — suppressing baseline s232')
      base_config$params_s232 <- NULL
    } else if (is.null(base_config$params_s232)) {
      base_config$params_s232 <- overlay$params_s232
    } else {
      base_config$params_s232$rate_matrix <- merge_s232_rate_matrix(
        base_config$params_s232$rate_matrix,
        overlay$params_s232$rate_matrix
      )
      # Overlay usmca_exempt flags (overlay wins for matching programs)
      if (!is.null(overlay$params_s232$usmca_exempt)) {
        for (nm in names(overlay$params_s232$usmca_exempt)) {
          base_config$params_s232$usmca_exempt[[nm]] <- overlay$params_s232$usmca_exempt[[nm]]
        }
      }
      # Overlay target_total_rules (overlay wins for matching programs)
      if (!is.null(overlay$params_s232$target_total_rules)) {
        for (nm in names(overlay$params_s232$target_total_rules)) {
          base_config$params_s232$target_total_rules[[nm]] <- overlay$params_s232$target_total_rules[[nm]]
        }
      }
    }
    message(sprintf('  Merged s232 overlay: %s rows',
                    format(nrow(base_config$params_s232$rate_matrix), big.mark = ',')))
  }

  # Merge other authorities
  authority_keys <- c('rates_ieepa_reciprocal', 'rates_ieepa_fentanyl',
                      'rates_s122', 'rates_s301', 'rates_s201', 'rates_other')
  for (key in authority_keys) {
    if (!is.null(overlay[[key]])) {
      base_config[[key]] <- merge_rate_tibble(base_config[[key]], overlay[[key]])
      message(sprintf('  Merged %s overlay: %s rows',
                      key, format(nrow(base_config[[key]]), big.mark = ',')))
    }
  }

  base_config
}
