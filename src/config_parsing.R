# =============================================================================
# config_parsing.R
# =============================================================================
#
# Functions for loading and parsing tariff configuration files.
#
# Functions:
#   - deduplicate_232_codes():    Deduplicate HTS codes across Section 232 tariffs
#   - load_232_rates():           Load Section 232 rates at country level with defaults
#   - load_ieepa_rates_yaml():    Load IEEPA rates at country level with hierarchical config
#
# =============================================================================




#' Load Section 232 tariff rates as complete HS10 × country tibble
#'
#' Returns a tibble with one row per HS10 × country combination and one column per tariff.
#' This is the complete rate matrix ready for calculations - no further processing needed.
#'
#' YAML format:
#'   tariff_name:
#'     base: [list of HTS codes]
#'     rates:
#'       default: 0.5
#'       '5700': 0.25  # China
#'     usmca_exempt: 0
#'
#' @param yaml_file Path to 232 YAML configuration file
#' @param crosswalk_file Path to HS10-GTAP crosswalk CSV (for HS10 universe)
#' @param census_codes_file Path to Census country codes CSV (for country universe)
#'
#' @return List with two elements:
#'   - rate_matrix: Tibble with columns hs10, cty_code, s232_[tariff]_rate (one per tariff)
#'   - usmca_exempt: Named vector of usmca_exempt flags by tariff name
load_232_rates <- function(yaml_file,
                           crosswalk_file = 'resources/hs10_gtap_crosswalk.csv',
                           census_codes_file = 'resources/census_codes.csv') {

  message('Loading Section 232 rates from YAML...')

  # Read YAML configuration
  params_232 <- read_yaml(yaml_file)

  # Read HS10 universe from crosswalk
  crosswalk <- read_csv(crosswalk_file, show_col_types = FALSE) %>%
    mutate(hs10 = as.character(hs10))
  hs10_codes <- unique(crosswalk$hs10)

  # Read country universe from Census codes
  census_codes <- read_csv(
    census_codes_file,
    col_types = cols(Code = col_character()),
    show_col_types = FALSE
  )
  all_country_codes <- as.character(census_codes$Code)

  # Initialize with complete HS10 × country combinations
  rate_matrix <- expand_grid(
    hs10 = hs10_codes,
    cty_code = all_country_codes
  )

  # Extract USMCA exempt flags
  usmca_exempt_flags <- sapply(names(params_232), function(t) params_232[[t]]$usmca_exempt)

  # For each tariff, build a sparse rate matrix (only non-zero rates)
  tariff_matrices <- list()

  for (tariff_name in names(params_232)) {

    # Get HTS codes that define coverage
    hts_codes <- params_232[[tariff_name]]$base

    # Get rates config
    rates_config <- params_232[[tariff_name]]$rates
    default_rate <- rates_config$default
    if (is.null(default_rate)) {
      stop(sprintf('Tariff %s is missing required "default" rate', tariff_name))
    }

    # Build regex pattern for coverage matching
    if (length(hts_codes) > 0) {
      pattern <- paste0('^(', paste(hts_codes, collapse = '|'), ')')
    } else {
      pattern <- '^$'  # Matches nothing
    }

    # Build country rates lookup (only countries with non-zero rates)
    country_rates_nonzero <- tibble(
      cty_code = all_country_codes,
      country_rate = sapply(all_country_codes, function(code) {
        if (!is.null(rates_config[[code]])) {
          rates_config[[code]]
        } else {
          default_rate
        }
      })
    ) %>%
      filter(country_rate > 0)

    # Build sparse rate matrix for this tariff
    if (nrow(country_rates_nonzero) > 0 && length(hts_codes) > 0) {
      # Get covered HS10 codes
      covered_hs10 <- hs10_codes[str_detect(hs10_codes, pattern)]

      if (length(covered_hs10) > 0) {
        rate_col_name <- paste0('s232_', tariff_name, '_rate')

        tariff_matrix <- expand_grid(
          hs10 = covered_hs10,
          cty_code = country_rates_nonzero$cty_code
        ) %>%
          left_join(country_rates_nonzero, by = 'cty_code') %>%
          rename(!!rate_col_name := country_rate)

        tariff_matrices[[tariff_name]] <- tariff_matrix
      }
    }
  }

  # Join all tariff matrices together
  if (length(tariff_matrices) > 0) {
    rate_matrix <- tariff_matrices[[1]]
    if (length(tariff_matrices) > 1) {
      for (i in 2:length(tariff_matrices)) {
        rate_matrix <- rate_matrix %>%
          full_join(tariff_matrices[[i]], by = c('hs10', 'cty_code'))
      }
    }
  } else {
    # No tariffs with non-zero rates - return empty tibble with correct structure
    rate_matrix <- tibble(
      hs10 = character(),
      cty_code = character()
    )
  }

  message(sprintf('Loaded 232 rates for %d tariffs across %s HS10 × country combinations',
                  length(params_232), format(nrow(rate_matrix), big.mark = ',')))

  return(list(
    rate_matrix = rate_matrix,
    usmca_exempt = usmca_exempt_flags
  ))
}


#' Load IEEPA rates as complete HS10 × country tibble
#'
#' Generic loader for any IEEPA-type tariff (reciprocal, fentanyl, etc.).
#' Returns a simple tibble with one row per HS10 × country combination.
#' Applies hierarchical rate structure from YAML:
#' 1. Headline rates: Default rate for each country
#' 2. Product rates: Override headline for specific HTS codes
#' 3. Product × country rates: Override everything for specific combinations
#'
#' @param yaml_file Path to IEEPA YAML configuration file
#' @param rate_col_name Name for the rate column in output tibble (e.g., 'ieepa_reciprocal_rate')
#' @param crosswalk_file Path to HS10-GTAP crosswalk CSV file
#' @param census_codes_file Path to Census country codes CSV file
#'
#' @return Tibble with columns: hs10, cty_code, [rate_col_name]
load_ieepa_rates_yaml <- function(yaml_file,
                                  rate_col_name = 'ieepa_rate',
                                  crosswalk_file = 'resources/hs10_gtap_crosswalk.csv',
                                  census_codes_file = 'resources/census_codes.csv') {

  message('Loading IEEPA rates from YAML...')

  # Read YAML configuration
  config <- read_yaml(yaml_file)

  # Read HS10 universe
  crosswalk <- read_csv(crosswalk_file, show_col_types = FALSE) %>%
    mutate(hs10 = as.character(hs10))
  hs10_codes <- unique(crosswalk$hs10)

  # Read country universe
  census_codes <- read_csv(
    census_codes_file,
    col_types = cols(Code = col_character()),
    show_col_types = FALSE
  )
  all_country_codes <- as.character(census_codes$Code)

  # Get default rate
  default_rate <- config$headline_rates$default
  if (is.null(default_rate)) {
    stop('IEEPA headline_rates is missing required "default" rate')
  }

  # ===========================
  # Step 1: Initialize with headline rates
  # ===========================

  rate_matrix <- tibble(cty_code = all_country_codes) %>%
    mutate(
      # Apply country-specific headline rates or default
      rate = sapply(cty_code, function(code) {
        country_rate <- config$headline_rates[[code]]
        if (!is.null(country_rate)) {
          return(country_rate)
        } else {
          return(default_rate)
        }
      })
    ) %>%
    expand_grid(hs10 = hs10_codes) %>%
    select(hs10, cty_code, rate)

  # ===========================
  # Step 2: Apply product-level rates (apply to ALL countries)
  # ===========================

  if (!is.null(config$product_rates) && length(config$product_rates) > 0) {

    # Build product rate lookup
    product_rates_expanded <- names(config$product_rates) %>%
      map_df(function(hts_code) {
        matching_hs10 <- hs10_codes[str_starts(hs10_codes, hts_code)]
        tibble(
          hs10 = matching_hs10,
          product_rate = config$product_rates[[hts_code]]
        )
      })

    # Apply product rate overrides
    rate_matrix <- rate_matrix %>%
      left_join(product_rates_expanded, by = 'hs10') %>%
      mutate(rate = if_else(!is.na(product_rate), product_rate, rate)) %>%
      select(-product_rate)
  }

  # ===========================
  # Step 3: Apply product × country rates
  # ===========================

  # TODO: Implement when needed

  # ===========================
  # Filter to sparse matrix (only non-zero rates) and return
  # ===========================

  rate_matrix <- rate_matrix %>%
    filter(rate > 0) %>%
    rename(!!rate_col_name := rate)

  message(sprintf('Loaded %s for %s HS10 × country combinations',
                  rate_col_name, format(nrow(rate_matrix), big.mark = ',')))

  return(rate_matrix)
}
