# =============================================================================
# etr_engine.R
# =============================================================================
#
# Core tariff calculation functions. Takes parsed config data and import data,
# applies USMCA exemptions, metal content adjustment, MFN logic, target-total
# floors, and stacking rules to produce final tariff rates.
#
# Functions:
#   - calc_etrs_for_config():            Orchestrate calc for one config set
#   - calc_delta():                      Compute delta (counterfactual - baseline)
#   - calc_weighted_etr():               Main tariff calculation at HS10 × country level
#   - aggregate_countries_to_partners(): Aggregate to partner × GTAP level
#
# =============================================================================


#' Calculate tariff rates for a single config (one set of YAML files)
#'
#' Takes parsed config and pre-loaded shared data, runs calc_weighted_etr()
#' and aggregate_countries_to_partners(), returns both results.
#' MFN rates are loaded from the config (config$mfn_rates).
#'
#' @param config Named list from load_scenario_config()
#' @param hs10_by_country Import data (hs10, cty_code, gtap_code, imports)
#' @param country_mapping Country-to-partner mapping
#'
#' @return Named list with hs10_country_etrs and partner_etrs
calc_etrs_for_config <- function(config, hs10_by_country, country_mapping) {

  message('Calculating tariff rates at HS10 × country level...')

  # Load metal content shares for Section 232 derivative adjustment
  metal_content <- load_metal_content(
    metal_content_config = config$other_params$metal_content,
    import_data = hs10_by_country
  )

  hs10_country_etrs <- calc_weighted_etr(
    rates_s232              = if (!is.null(config$params_s232)) config$params_s232$rate_matrix else NULL,
    usmca_exempt_s232       = if (!is.null(config$params_s232)) config$params_s232$usmca_exempt else NULL,
    s232_target_total_rules = if (!is.null(config$params_s232)) config$params_s232$target_total_rules else NULL,
    rates_ieepa_reciprocal = config$rates_ieepa_reciprocal,
    rates_ieepa_fentanyl   = config$rates_ieepa_fentanyl,
    rates_s122             = config$rates_s122,
    rates_s301             = config$rates_s301,
    rates_s201             = config$rates_s201,
    rates_other            = config$rates_other,
    import_data            = hs10_by_country,
    usmca_product_shares   = config$usmca_product_shares,
    us_auto_content_share  = config$other_params$us_auto_content_share,
    auto_rebate            = config$other_params$auto_rebate_rate,
    us_assembly_share      = config$other_params$us_auto_assembly_share,
    ieepa_usmca_exempt     = config$other_params$ieepa_usmca_exception,
    s122_usmca_exempt      = config$other_params$s122_usmca_exception %||% 0,
    s301_usmca_exempt      = config$other_params$s301_usmca_exception %||% 0,
    metal_content          = metal_content,
    metal_programs         = config$other_params$metal_content$metal_programs %||% character(0),
    program_metal_types    = config$other_params$metal_content$program_metal_types %||% NULL,
    mfn_rates                    = config$mfn_rates,
    mfn_rates_by_product_country = config$mfn_rates_by_product_country,
    mfn_exemption_shares         = config$mfn_exemption_shares
  )

  message('Aggregating HS10×country results to partner×GTAP level...')
  partner_etrs <- aggregate_countries_to_partners(
    hs10_country_etrs = hs10_country_etrs,
    country_mapping   = country_mapping
  )

  list(
    hs10_country_etrs = hs10_country_etrs,
    partner_etrs      = partner_etrs
  )
}


#' Compute delta between counterfactual and baseline HS10×country results
#'
#' Joins on (hs10, cty_code) and computes delta = counterfactual level - baseline level.
#' The delta replaces the etr column (used for shocks and delta outputs).
#' The level column retains the counterfactual absolute tariff rate.
#'
#' @param baseline_hs10 HS10×country results from baseline config
#' @param counter_hs10 HS10×country results from counterfactual config
#'
#' @return counter_hs10 with etr replaced by delta (counter level - baseline level)
calc_delta <- function(baseline_hs10, counter_hs10) {
  counter_hs10 %>%
    left_join(
      baseline_hs10 %>% select(hs10, cty_code, level_base = level),
      by = c('hs10', 'cty_code')
    ) %>%
    mutate(
      level_base = if_else(is.na(level_base), 0, level_base),
      etr = level - level_base
    ) %>%
    select(-level_base)
}


#' Calculate weighted ETR changes at HS10 × country level using tabular config data
#'
#' Takes clean tabular rate data from config parsing and applies USMCA exemptions,
#' auto rebates, and stacking rules to produce final ETRs.
#'
#' @param rates_s232 Tibble with columns: hs10, cty_code, s232_[tariff]_rate (one per tariff)
#' @param usmca_exempt_s232 Named vector of USMCA exempt flags by tariff name
#' @param rates_ieepa_reciprocal Tibble with columns: hs10, cty_code, ieepa_reciprocal_rate
#' @param rates_ieepa_fentanyl Tibble with columns: hs10, cty_code, ieepa_fentanyl_rate
#' @param rates_s122 Tibble with columns: hs10, cty_code, s122_rate (or NULL if no s122.yaml)
#' @param rates_s301 Tibble with columns: hs10, cty_code, s301_rate (or NULL if no s301.yaml)
#' @param import_data Data frame with hs10, cty_code, gtap_code, imports
#' @param usmca_product_shares Tibble with columns: hts10, cty_code, usmca_share
#' @param us_auto_content_share Share of US content in auto assembly
#' @param auto_rebate Auto rebate rate
#' @param us_assembly_share Share of US assembly in autos
#' @param ieepa_usmca_exempt Apply USMCA exemption to IEEPA tariffs (1 = yes, 0 = no)
#' @param metal_content Tibble with columns: hs10, metal_share (from load_metal_content()).
#'   May also include steel_share, aluminum_share, copper_share, other_metal_share for detail mode.
#' @param metal_programs Character vector of 232 tariff names that are metal programs (e.g., 'steel', 'aluminum')
#' @param program_metal_types Named list: program_name -> metal type string (e.g., list(steel = 'steel', aluminum_base_articles = 'aluminum')).
#'   When provided with per-type share columns, each program is scaled by its type's share instead of aggregate metal_share.
#' @param mfn_rates Tibble with columns: hs8, mfn_rate (MFN baseline tariff rates)
#' @param mfn_exemption_shares Tibble with columns: hs2, cty_code, exemption_share (or NULL).
#'   When provided, MFN rates are adjusted: effective_mfn = mfn_rate * (1 - exemption_share).
#'   This captures FTA/GSP preferences that reduce the statutory MFN rate.
#'
#' @return Data frame with columns: hs10, cty_code, gtap_code, imports, etr, level, base_s232, base_ieepa, base_neither
calc_weighted_etr <- function(rates_s232,
                              usmca_exempt_s232,
                              s232_target_total_rules = NULL,
                              rates_ieepa_reciprocal,
                              rates_ieepa_fentanyl,
                              rates_s122 = NULL,
                              rates_s301 = NULL,
                              rates_s201 = NULL,
                              rates_other = NULL,
                              import_data,
                              usmca_product_shares,
                              us_auto_content_share,
                              auto_rebate,
                              us_assembly_share,
                              ieepa_usmca_exempt,
                              s122_usmca_exempt = 0,
                              s301_usmca_exempt = 0,
                              metal_content = NULL,
                              metal_programs = character(0),
                              program_metal_types = NULL,
                              mfn_rates = NULL,
                              mfn_rates_by_product_country = NULL,
                              mfn_exemption_shares = NULL) {

  # =============================================================================
  # Build complete rate matrix by joining config tables with import data
  # Config tables are now sparse (only non-zero rates), so NAs from joins = 0
  # NULL inputs (missing config files) produce zero-rate columns
  # =============================================================================

  rate_matrix <- import_data %>%
    select(hs10, cty_code, gtap_code, imports)

  # Join 232 rates if provided
  if (!is.null(rates_s232)) {
    rate_matrix <- rate_matrix %>%
      left_join(rates_s232, by = c('hs10', 'cty_code'))
  }

  # Helper: join an authority rate tibble, renaming target_total_rate to avoid
  # collisions when multiple authorities have target_total_rules.
  join_authority_rates <- function(matrix, rates, rate_col_name) {
    if (!is.null(rates)) {
      if ('target_total_rate' %in% names(rates)) {
        rates <- rates %>%
          rename(!!paste0('target_total_rate_', rate_col_name) := target_total_rate)
      }
      matrix %>% left_join(rates, by = c('hs10', 'cty_code'))
    } else {
      matrix %>% mutate(!!rate_col_name := 0)
    }
  }

  rate_matrix <- rate_matrix %>%
    join_authority_rates(rates_ieepa_reciprocal, 'ieepa_reciprocal_rate') %>%
    join_authority_rates(rates_ieepa_fentanyl, 'ieepa_fentanyl_rate') %>%
    join_authority_rates(rates_s122, 's122_rate') %>%
    join_authority_rates(rates_s301, 's301_rate') %>%
    join_authority_rates(rates_s201, 's201_rate') %>%
    join_authority_rates(rates_other, 'other_rate')

  # Get all 232 rate column names
  rate_s232_cols <- names(rate_matrix)[str_detect(names(rate_matrix), '^s232_.*_rate$')]

  # Replace NAs (indicates no tariff) with 0 for all rate columns
  all_rate_cols <- c(rate_s232_cols, 'ieepa_reciprocal_rate', 'ieepa_fentanyl_rate', 's122_rate', 's301_rate', 's201_rate', 'other_rate')
  rate_matrix <- rate_matrix %>%
    mutate(
      across(
        .cols = all_of(all_rate_cols),
        .fns  = ~ if_else(is.na(.), 0, .)
      )
    )

  # =============================================================================
  # Apply USMCA exemptions and auto rebates
  # =============================================================================

  # Join product-level USMCA shares on (hs10, cty_code)
  rate_matrix <- rate_matrix %>%
    left_join(
      usmca_product_shares %>% select(hs10 = hts10, cty_code, usmca_share),
      by = c('hs10', 'cty_code')
    ) %>%
    mutate(usmca_share = if_else(is.na(usmca_share), 0, usmca_share))

  # Rebate-eligible tariffs: only passenger auto and light truck programs.
  # The tracker applies the rebate only to auto_products (passenger vehicles,
  # light trucks, auto parts), NOT to MHD, buses, or other heading programs.
  rebate_tariffs <- c('automobiles_passenger_and_light_trucks', 'automobile_parts',
                       'autos_passenger', 'autos_light_trucks', 'auto_parts')

  # Vehicle tariffs that use adjusted USMCA share (usmca_share * us_auto_content_share).
  # Includes all vehicle/parts programs flagged s232_usmca_eligible in the tracker:
  # passenger autos, light trucks, auto parts, MHD vehicles, MHD parts, buses.
  vehicle_tariffs <- c('automobiles_passenger_and_light_trucks', 'automobile_parts', 'vehicles_completed_mhd',
                        'autos_passenger', 'autos_light_trucks', 'auto_parts',
                        'mhd_vehicles', 'mhd_parts', 'buses')

  # Get tariff names from rate column names
  tariff_names <- str_replace(rate_s232_cols, '^s232_(.*)_rate$', '\\1')

  # Apply auto rebate and USMCA exemptions to each 232 tariff
  for (tariff_name in tariff_names) {
    rate_col <- paste0('s232_', tariff_name, '_rate')

    # Apply auto rebate only to rebate-eligible (passenger auto) tariffs
    if (tariff_name %in% rebate_tariffs) {
      rate_matrix <- rate_matrix %>%
        mutate(
          !!rate_col := !!sym(rate_col) - (auto_rebate * us_assembly_share)
        )
    }

    # Apply USMCA exemption if enabled for this tariff
    if (usmca_exempt_s232[[tariff_name]] == 1) {

      # Vehicle tariffs use adjusted USMCA share
      if (tariff_name %in% vehicle_tariffs) {
        rate_matrix <- rate_matrix %>%
          mutate(
            adjusted_usmca_share = usmca_share * us_auto_content_share,
            !!rate_col := if_else(
              cty_code %in% USMCA_COUNTRIES,
              !!sym(rate_col) * (1 - adjusted_usmca_share),
              !!sym(rate_col)
            )
          ) %>%
          select(-adjusted_usmca_share)

      # Non-auto tariffs use standard USMCA share
      } else {
        rate_matrix <- rate_matrix %>%
          mutate(
            !!rate_col := if_else(
              cty_code %in% USMCA_COUNTRIES,
              !!sym(rate_col) * (1 - usmca_share),
              !!sym(rate_col)
            )
          )
      }
    }
  }

  # Apply USMCA exemption to IEEPA tariffs if enabled
  if (ieepa_usmca_exempt == 1) {
    rate_matrix <- rate_matrix %>%
      mutate(
        ieepa_reciprocal_rate = if_else(
          cty_code %in% USMCA_COUNTRIES,
          ieepa_reciprocal_rate * (1 - usmca_share),
          ieepa_reciprocal_rate
        ),
        ieepa_fentanyl_rate = if_else(
          cty_code %in% USMCA_COUNTRIES,
          ieepa_fentanyl_rate * (1 - usmca_share),
          ieepa_fentanyl_rate
        )
      )
  }

  # Apply USMCA exemption to Section 122 tariffs if enabled
  if (s122_usmca_exempt == 1) {
    rate_matrix <- rate_matrix %>%
      mutate(
        s122_rate = if_else(
          cty_code %in% USMCA_COUNTRIES,
          s122_rate * (1 - usmca_share),
          s122_rate
        )
      )
  }

  # Apply USMCA exemption to Section 301 tariffs if enabled
  if (s301_usmca_exempt == 1) {
    rate_matrix <- rate_matrix %>%
      mutate(
        s301_rate = if_else(
          cty_code %in% USMCA_COUNTRIES,
          s301_rate * (1 - usmca_share),
          s301_rate
        )
      )
  }

  # Join MFN rates early so reciprocal target-total rules can use Column 1.
  # Two paths: (1) hs10×country from CSV (tracker is source of truth),
  #            (2) hs8-level from YAML config with optional exemption shares.
  if (!is.null(mfn_rates_by_product_country)) {

    # CSV path: MFN at hs10×country level (statutory, pre-exemption)
    rate_matrix <- rate_matrix %>%
      left_join(mfn_rates_by_product_country, by = c('hs10', 'cty_code')) %>%
      mutate(
        mfn_rate = if_else(is.na(mfn_rate), 0, mfn_rate),
        statutory_mfn = mfn_rate
      )

    # Apply MFN exemption shares (same logic as YAML path)
    if (!is.null(mfn_exemption_shares)) {
      rate_matrix <- rate_matrix %>%
        mutate(hs2 = substr(hs10, 1, 2)) %>%
        left_join(mfn_exemption_shares, by = c('hs2', 'cty_code')) %>%
        mutate(
          exemption_share = if_else(is.na(exemption_share), 0, exemption_share),
          exemption_share = if_else(
            cty_code %in% USMCA_COUNTRIES, 0, exemption_share
          ),
          mfn_rate = mfn_rate * (1 - exemption_share)
        ) %>%
        select(-hs2, -exemption_share)
    }

    # Apply USMCA share reduction to MFN rate for CA/MX
    rate_matrix <- rate_matrix %>%
      mutate(
        mfn_rate = if_else(
          cty_code %in% USMCA_COUNTRIES,
          mfn_rate * (1 - usmca_share),
          mfn_rate
        )
      )

  } else if (!is.null(mfn_rates)) {

    # YAML path: MFN at hs8 level + optional exemption shares
    rate_matrix <- rate_matrix %>%
      mutate(hs8 = substr(hs10, 1, 8)) %>%
      left_join(mfn_rates, by = 'hs8')

    n_missing <- sum(is.na(rate_matrix$mfn_rate))
    n_total <- nrow(rate_matrix)
    if (n_missing > 0) {
      n_hs8_missing <- length(unique(rate_matrix$hs8[is.na(rate_matrix$mfn_rate)]))
      message(sprintf('MFN join: %d of %d rows (%d unique HS8 codes) have no MFN rate, defaulting to 0',
                      n_missing, n_total, n_hs8_missing))
    }

    rate_matrix <- rate_matrix %>%
      mutate(
        mfn_rate = if_else(is.na(mfn_rate), 0, mfn_rate),
        statutory_mfn = mfn_rate
      ) %>%
      select(-hs8)

    # Apply MFN exemption shares (FTA/GSP preferences)
    if (!is.null(mfn_exemption_shares)) {
      rate_matrix <- rate_matrix %>%
        mutate(hs2 = substr(hs10, 1, 2)) %>%
        left_join(mfn_exemption_shares, by = c('hs2', 'cty_code')) %>%
        mutate(
          exemption_share = if_else(is.na(exemption_share), 0, exemption_share),
          # USMCA product shares handle CA/MX directly; skip MFN exemptions
          # for those countries (tracker's exclude_usmca = TRUE behavior)
          exemption_share = if_else(
            cty_code %in% USMCA_COUNTRIES,
            0,
            exemption_share
          ),
          mfn_rate = mfn_rate * (1 - exemption_share)
        ) %>%
        select(-hs2, -exemption_share)
    }

    # Apply USMCA share reduction to MFN rate for CA/MX
    rate_matrix <- rate_matrix %>%
      mutate(
        mfn_rate = if_else(
          cty_code %in% USMCA_COUNTRIES,
          mfn_rate * (1 - usmca_share),
          mfn_rate
        )
      )

  } else {
    rate_matrix <- rate_matrix %>%
      mutate(mfn_rate = 0, statutory_mfn = 0)
  }

  # Clean up USMCA share column (all USMCA applications are done)
  rate_matrix <- rate_matrix %>%
    select(-usmca_share)

  # Generalized target-total (floor) rules:
  # For any authority with a target_total_rate_{authority} column, the effective
  # add-on = max(target_total - statutory_mfn, 0), applied where the authority rate > 0.
  # Uses statutory (pre-exemption, pre-USMCA) MFN to match tracker's order of operations.
  tt_cols <- names(rate_matrix)[str_detect(names(rate_matrix), '^target_total_rate_')]
  for (tt_col in tt_cols) {
    # Extract the rate column name (e.g., 'target_total_rate_ieepa_reciprocal_rate' -> 'ieepa_reciprocal_rate')
    rate_col <- str_replace(tt_col, '^target_total_rate_', '')
    if (rate_col %in% names(rate_matrix)) {
      rate_matrix <- rate_matrix %>%
        mutate(
          !!rate_col := if_else(
            !is.na(!!sym(tt_col)) & !!sym(rate_col) > 0,
            pmax(!!sym(rate_col), pmax(!!sym(tt_col) - statutory_mfn, 0)),
            !!sym(rate_col)
          )
        )
    }
  }

  # Optional 232 target-total floor logic (auto deal rates):
  # For each 232 program with target_total rules, the effective 232 add-on is
  # max(target_total - MFN, 0) for countries with a deal. Same mechanism as
  # IEEPA reciprocal target-total but applied per-program within 232.
  if (!is.null(s232_target_total_rules)) {
    for (program_name in names(s232_target_total_rules)) {
      col_name <- paste0('s232_', program_name, '_rate')
      if (col_name %in% names(rate_matrix)) {
        target_tibble <- s232_target_total_rules[[program_name]] %>%
          rename(s232_tt_rate = target_total_rate)
        rate_matrix <- rate_matrix %>%
          left_join(target_tibble, by = 'cty_code') %>%
          mutate(
            !!col_name := if_else(
              !is.na(s232_tt_rate) & !!sym(col_name) > 0,
              pmax(!!sym(col_name), pmax(s232_tt_rate - statutory_mfn, 0)),
              !!sym(col_name)
            )
          ) %>%
          select(-s232_tt_rate)
      }
    }
  }

  # Clean up statutory_mfn (only needed for target-total computations above)
  rate_matrix <- rate_matrix %>%
    select(-statutory_mfn)

  # =============================================================================
  # Apply metal content shares to Section 232 metal program rates
  # For derivative products, only the metal content share of the customs value
  # is subject to 232 tariffs; the non-metal portion receives IEEPA tariffs.
  # =============================================================================

  # Track whether per-type accumulation is active for nonmetal_share computation
  use_per_type_nonmetal <- FALSE

  if (!is.null(metal_content) && length(metal_programs) > 0) {

    # Join metal content shares (may include per-type columns)
    rate_matrix <- rate_matrix %>%
      left_join(metal_content, by = 'hs10') %>%
      mutate(metal_share = if_else(is.na(metal_share), 1.0, metal_share))

    # Check if per-type shares are available and program_metal_types is provided
    has_per_type <- !is.null(program_metal_types) &&
      'steel_share' %in% names(rate_matrix)

    # Map metal type names to column names
    type_col_map <- c(
      steel   = 'steel_share',
      aluminum = 'aluminum_share',
      copper  = 'copper_share',
      other   = 'other_metal_share'
    )

    if (has_per_type) {
      use_per_type_nonmetal <- TRUE
    }

    # Apply metal content share to metal program 232 tariffs
    # This scales the rate: effective_rate = statutory_rate * share
    for (tariff_name in tariff_names) {
      if (tariff_name %in% metal_programs) {
        rate_col <- paste0('s232_', tariff_name, '_rate')

        # Use per-type share if available, otherwise aggregate metal_share
        if (has_per_type && tariff_name %in% names(program_metal_types)) {
          metal_type <- program_metal_types[[tariff_name]]
          type_col <- type_col_map[[metal_type]]
          if (is.null(type_col)) {
            stop(sprintf('Unknown metal type "%s" for program "%s". Expected: %s',
                          metal_type, tariff_name, paste(names(type_col_map), collapse = ', ')))
          }
          rate_matrix <- rate_matrix %>%
            mutate(!!rate_col := !!sym(rate_col) * !!sym(type_col))
        } else {
          rate_matrix <- rate_matrix %>%
            mutate(!!rate_col := !!sym(rate_col) * metal_share)
        }
      }
    }

    # Compute per-type nonmetal share AFTER scaling.
    # Group programs by metal type and check if ANY program of that type is active
    # per row. Add each type's share at most once to avoid double-counting when
    # multiple programs map to the same type (e.g., aluminum_base + aluminum_derivative).
    if (has_per_type) {
      type_to_programs <- split(names(program_metal_types), unlist(program_metal_types))
      rate_matrix <- rate_matrix %>% mutate(.active_type_share = 0)

      for (metal_type in names(type_to_programs)) {
        progs_of_type <- intersect(type_to_programs[[metal_type]], tariff_names)
        type_col <- type_col_map[[metal_type]]
        rate_cols_for_type <- paste0('s232_', progs_of_type, '_rate')
        rate_cols_for_type <- rate_cols_for_type[rate_cols_for_type %in% names(rate_matrix)]

        if (length(rate_cols_for_type) > 0) {
          rate_matrix <- rate_matrix %>%
            mutate(.active_type_share = .active_type_share +
              if_else(pmax(!!!syms(rate_cols_for_type)) > 0, !!sym(type_col), 0))
        }
      }
    }

  } else {
    # No metal content adjustment — add column for downstream stacking logic
    rate_matrix <- rate_matrix %>%
      mutate(metal_share = 1.0)
  }


  # =============================================================================
  # Apply stacking rules to calculate final_rate
  # =============================================================================

  # Calculate effective 232 rate across all tariffs
  if (length(tariff_names) > 0) {

    if (use_per_type_nonmetal) {
      # Per-type mode: metal rates are already scaled by disjoint type shares,
      # so rates from different metal types are additive. Within a single metal
      # type, pmax dedupes overlapping programs (e.g., aluminum_base vs
      # aluminum_derivative). Non-metal 232 programs (autos) cover the full
      # product, so they dominate via pmax against the metal sum.
      nonmetal_s232_programs <- setdiff(tariff_names, metal_programs)
      nonmetal_s232_cols <- paste0('s232_', nonmetal_s232_programs, '_rate')
      nonmetal_s232_cols <- nonmetal_s232_cols[nonmetal_s232_cols %in% names(rate_matrix)]

      # Group metal programs by type and take pmax within each type
      type_to_programs <- split(names(program_metal_types), unlist(program_metal_types))
      type_max_exprs <- list()
      for (metal_type in names(type_to_programs)) {
        progs_of_type <- intersect(type_to_programs[[metal_type]], tariff_names)
        cols_for_type <- paste0('s232_', progs_of_type, '_rate')
        cols_for_type <- cols_for_type[cols_for_type %in% names(rate_matrix)]
        if (length(cols_for_type) > 0) {
          type_max_exprs[[metal_type]] <- rlang::expr(pmax(!!!syms(cols_for_type)))
        }
      }

      # Sum across metal types (disjoint value fractions are additive)
      if (length(type_max_exprs) > 0) {
        metal_sum_expr <- Reduce(function(a, b) rlang::expr(!!a + !!b), type_max_exprs)
      } else {
        metal_sum_expr <- rlang::expr(0)
      }

      # pmax of non-metal 232 (full-product) vs metal sum (partial fractions)
      if (length(nonmetal_s232_cols) > 0) {
        nonmetal_max_expr <- rlang::expr(pmax(!!!syms(nonmetal_s232_cols)))
        final_expr <- rlang::expr(pmax(!!nonmetal_max_expr, !!metal_sum_expr))
      } else {
        final_expr <- metal_sum_expr
      }

      rate_matrix <- rate_matrix %>%
        mutate(rate_s232_max = !!final_expr)

    } else {
      # Aggregate mode: single metal_share applied to all metal programs,
      # so pmax across all 232 columns is correct
      rate_matrix <- rate_matrix %>%
        mutate(rate_s232_max = pmax(!!!syms(rate_s232_cols)))
    }

  # No 232 tariffs - set max to 0
  } else {
    rate_matrix <- rate_matrix %>%
      mutate(rate_s232_max = 0)
  }

  # Compute non-metal share for IEEPA application on the non-metal portion of derivatives.
  # Only for products covered by metal 232 programs (not auto 232, etc.).
  # Per-type mode: nonmetal_share = 1 - sum(active type shares), so IEEPA fills
  # everything not claimed by the specific 232 programs covering each product.
  # Aggregate mode: nonmetal_share = 1 - metal_share (unchanged).
  metal_s232_cols <- paste0('s232_', intersect(tariff_names, metal_programs), '_rate')
  metal_s232_cols <- metal_s232_cols[metal_s232_cols %in% names(rate_matrix)]

  if (length(metal_s232_cols) > 0) {
    if (use_per_type_nonmetal) {
      # Per-type mode: IEEPA covers everything active 232 programs don't claim
      rate_matrix <- rate_matrix %>%
        mutate(
          nonmetal_share = if_else(pmax(!!!syms(metal_s232_cols)) > 0, 1 - .active_type_share, 0)
        )
    } else {
      # Aggregate mode: IEEPA covers the non-metal portion
      rate_matrix <- rate_matrix %>%
        mutate(
          nonmetal_share = if_else(pmax(!!!syms(metal_s232_cols)) > 0, 1 - metal_share, 0)
        )
    }
  } else {
    rate_matrix <- rate_matrix %>%
      mutate(nonmetal_share = 0)
  }

  # Clean up temporary accumulator column
  if ('.active_type_share' %in% names(rate_matrix)) {
    rate_matrix <- rate_matrix %>% select(-.active_type_share)
  }

  rate_matrix <- rate_matrix %>%
    mutate(

      # Stacking rules:
      # 1. IEEPA Reciprocal: Mutually exclusive with 232 (applies only to uncovered base)
      #    Exception: for metal 232 derivatives, IEEPA applies to the non-metal portion
      # 2. IEEPA Fentanyl:
      #    - China: STACKS on top of everything (232 + reciprocal + fentanyl)
      #    - Others: Only applies to base not covered by 232 or reciprocal
      # 3. Section 122: Excluded "to the extent the 232 tariff applies," meaning
      #    S122 applies only to the nonmetal_share of 232-covered products (same as IEEPA).
      #    For non-232 products, S122 applies in full.
      # 4. Section 301, Section 201, other: Unconditionally cumulative with all other
      #    authorities. Apply to full customs value (no metal content scaling).

      final_rate = mfn_rate + case_when(
        # China: Fentanyl stacks on top of normal 232-reciprocal logic
        # For metal 232 derivatives: reciprocal and s122 apply to non-metal portion
        # Section 301, 201, other always apply to full customs value
        cty_code == CTY_CHINA ~ if_else(
          rate_s232_max > 0,
          rate_s232_max + ieepa_reciprocal_rate * nonmetal_share
            + ieepa_fentanyl_rate + s122_rate * nonmetal_share
            + s301_rate + s201_rate + other_rate,
          ieepa_reciprocal_rate + ieepa_fentanyl_rate + s122_rate
            + s301_rate + s201_rate + other_rate
        ),

        # Everyone else: 232 takes precedence; reciprocal, fentanyl, and s122
        # apply to non-metal portion of derivatives (content-based split).
        # Section 301, 201, other always apply to full customs value.
        rate_s232_max > 0 ~
          rate_s232_max + ieepa_reciprocal_rate * nonmetal_share
            + ieepa_fentanyl_rate * nonmetal_share
            + s122_rate * nonmetal_share + s301_rate + s201_rate + other_rate,

        # Otherwise use all IEEPA + s122 + s301 + s201 + other
        TRUE ~ ieepa_reciprocal_rate + ieepa_fentanyl_rate + s122_rate
          + s301_rate + s201_rate + other_rate
      )
    )

  # =============================================================================
  # Calculate ETR and coverage tracking
  # =============================================================================

  hs10_country_etrs <- rate_matrix %>%
    mutate(
      etr = final_rate,

      # Coverage tracking (mutually exclusive categories for reporting)
      # Note: For China, fentanyl stacks on top, but for coverage we track primary authority.
      # The non-232 bucket includes IEEPA and Section 122.
      base_s232     = if_else(rate_s232_max > 0, imports, 0),
      base_ieepa   = if_else(
        rate_s232_max == 0 & (ieepa_reciprocal_rate > 0 | ieepa_fentanyl_rate > 0 | s122_rate > 0),
        imports, 0
      ),
      base_neither = if_else(
        rate_s232_max == 0 & ieepa_reciprocal_rate == 0 & ieepa_fentanyl_rate == 0 & s122_rate == 0,
        imports, 0
      )
    )

  hs10_country_etrs <- hs10_country_etrs %>%
    mutate(level = final_rate)

  hs10_country_etrs <- hs10_country_etrs %>%
    select(hs10, cty_code, gtap_code, imports, etr, level, base_s232, base_ieepa, base_neither)

  return(hs10_country_etrs)
}


#' Aggregate HS10×country ETRs to partner×GTAP level for GTAP output
#'
#' Takes HS10-level ETR data and aggregates to 8 partner groups × GTAP sectors
#' using import-weighted averaging. Also aggregates coverage bases for tariff
#' coverage reporting.
#'
#' @param hs10_country_etrs Data frame with columns: hs10, cty_code, gtap_code, imports, etr, level, base_s232, base_ieepa, base_neither
#' @param country_mapping Data frame with columns: cty_code, partner
#'
#' @return Data frame with columns: partner, gtap_code, etr, level, base_s232, base_ieepa, base_neither
aggregate_countries_to_partners <- function(hs10_country_etrs, country_mapping) {

  # Map countries to partners
  etrs_with_partner <- hs10_country_etrs %>%
    left_join(
      country_mapping %>% select(cty_code, partner) %>% distinct(),
      by = 'cty_code'
    ) %>%
    # Unmapped countries default to 'row'
    mutate(partner = if_else(is.na(partner), 'row', partner))

  # Aggregate to partner × GTAP level using import-weighted average
  # NOTE: weighted_etr and weighted_level must be computed BEFORE imports is overwritten
  # (dplyr summarise gotcha — later expressions see the scalar aggregate, not the vector)
  partner_etrs <- etrs_with_partner %>%
    group_by(partner, gtap_code) %>%
    summarise(
      total_imports = sum(imports),
      weighted_etr = sum(etr * imports),
      weighted_level = sum(level * imports),
      base_s232 = sum(base_s232),
      base_ieepa = sum(base_ieepa),
      base_neither = sum(base_neither),
      .groups = 'drop'
    ) %>%
    mutate(
      etr = if_else(total_imports > 0, weighted_etr / total_imports, 0),
      level = if_else(total_imports > 0, weighted_level / total_imports, 0)
    ) %>%
    select(partner, gtap_code, etr, level, base_s232, base_ieepa, base_neither)

  return(partner_etrs)
}
