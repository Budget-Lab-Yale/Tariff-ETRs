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


#' Deduplicate HTS codes across 232 tariff proclamations
#'
#' Applies two deduplication rules to prevent double-counting:
#' 1. If a code appears at different HS levels across tariffs, keep only the higher-level (shorter) code
#' 2. If a code appears at the same HS level in multiple tariffs, keep the one with highest average rate
#'
#' @param params_232 List of 232 tariff parameters (from YAML)
#'
#' @return Modified params_232 list with deduplicated codes
deduplicate_232_codes <- function(params_232) {

  # Extract all codes with their tariff names, lengths, and average rates
  all_codes <- map_df(names(params_232), function(tariff_name) {
    codes <- params_232[[tariff_name]]$base
    rates <- params_232[[tariff_name]]$rate
    avg_rate <- mean(unlist(rates))

    tibble(
      tariff = tariff_name,
      code = as.character(codes),
      code_length = nchar(code),
      avg_rate = avg_rate
    )
  })

  # Rule 1: Remove child codes when parent exists at different HS level
  # For each code, check if any other code is a prefix (parent)
  codes_to_remove <- character(0)

  for (i in 1:nrow(all_codes)) {
    for (j in 1:nrow(all_codes)) {
      if (i == j) next
      if (all_codes$tariff[i] == all_codes$tariff[j]) next  # Same tariff is OK

      code_i <- all_codes$code[i]
      code_j <- all_codes$code[j]

      # If code_j is a parent of code_i (shorter and is prefix), mark code_i for removal
      if (nchar(code_j) < nchar(code_i) && str_starts(code_i, code_j)) {
        codes_to_remove <- c(codes_to_remove, paste(all_codes$tariff[i], code_i, sep = '::'))
      }
    }
  }

  # Rule 2: For same-length duplicates, keep highest average rate
  # Group by code to find duplicates
  duplicate_codes <- all_codes %>%
    group_by(code) %>%
    filter(n() > 1) %>%
    arrange(code, desc(avg_rate)) %>%
    group_by(code) %>%
    slice(-1) %>%  # Remove all but the first (highest rate)
    ungroup()

  # Add these to removal list
  codes_to_remove <- c(
    codes_to_remove,
    paste(duplicate_codes$tariff, duplicate_codes$code, sep = '::')
  )

  # Remove duplicates from list
  codes_to_remove <- unique(codes_to_remove)

  # Apply removals to params_232
  if (length(codes_to_remove) > 0) {
    message(sprintf('Deduplicating 232 codes: removing %d hierarchical/duplicate codes', length(codes_to_remove)))

    for (removal in codes_to_remove) {
      parts <- str_split(removal, '::', simplify = TRUE)
      tariff_name <- parts[1]
      code_to_remove <- parts[2]

      # Remove this code from the base list
      params_232[[tariff_name]]$base <- setdiff(params_232[[tariff_name]]$base, code_to_remove)
    }
  } else {
    message('No hierarchical/duplicate 232 codes found')
  }

  return(params_232)
}


#' Load Section 232 tariff rates with country-level rates and defaults
#'
#' Reads 232 tariff YAML configuration and expands rates to country level.
#' For each tariff, applies rates by country code with default fallback.
#'
#' New YAML format:
#'   tariff_name:
#'     base: [list of HTS codes]
#'     rates:
#'       default: 0.5
#'       '5700': 0.25  # China
#'       '1220': 0.10  # Canada
#'     usmca_exempt: 0
#'
#' @param yaml_file Path to 232 YAML configuration file
#' @param country_mapping_file Path to country-partner mapping CSV
#'
#' @return List with same structure as input, ready for downstream processing
load_232_rates <- function(yaml_file, country_mapping_file = 'resources/country_partner_mapping.csv') {

  message('Loading Section 232 rates from YAML...')

  # Read YAML configuration
  params_232 <- read_yaml(yaml_file)

  # Read country mapping to get all possible country codes
  country_mapping <- read_csv(
    country_mapping_file,
    col_types = cols(cty_code = col_character()),
    show_col_types = FALSE
  )

  # Get all unique country codes (as strings to match YAML keys)
  all_country_codes <- unique(country_mapping$cty_code)

  # Expand rates for each tariff to country level
  for (tariff_name in names(params_232)) {

    rates_config <- params_232[[tariff_name]]$rates

    # Get default rate
    default_rate <- rates_config$default
    if (is.null(default_rate)) {
      stop(sprintf('Tariff %s is missing required "default" rate in rates section', tariff_name))
    }

    # Build country-level rates
    country_rates <- list()

    for (cty_code in all_country_codes) {
      # Check if country has specific rate, otherwise use default
      if (!is.null(rates_config[[cty_code]])) {
        country_rates[[cty_code]] <- rates_config[[cty_code]]
      } else {
        country_rates[[cty_code]] <- default_rate
      }
    }

    # Add unmapped countries (those not in country_partner_mapping.csv)
    # They also get the default rate, and we need to handle them at runtime
    # For now, just store the default for use during calculations

    # Replace rates section with expanded country-level rates
    params_232[[tariff_name]]$rate <- country_rates
    params_232[[tariff_name]]$rates <- NULL  # Remove the config structure
    params_232[[tariff_name]]$default_rate <- default_rate  # Store default for unmapped countries
  }

  message(sprintf('Loaded 232 rates for %d tariffs', length(params_232)))

  return(params_232)
}


#' Load IEEPA rates from YAML configuration with hierarchical rate structure
#'
#' Reads IEEPA rates from YAML file with three levels of specificity:
#' 1. Headline rates: Default rate for each country across all HS10 codes
#' 2. Product rates: Override headline for specific HTS codes (variable length)
#' 3. Product x country rates: Override everything for specific HTS x country combinations
#'
#' @param yaml_file Path to IEEPA YAML configuration file
#' @param crosswalk_file Path to HS10-GTAP crosswalk CSV file
#' @param country_mapping_file Path to country-partner mapping CSV
#'
#' @return Tibble with columns: hs10, cty_code, rate (long format)
load_ieepa_rates_yaml <- function(yaml_file,
                                  crosswalk_file = 'resources/hs10_gtap_crosswalk.csv',
                                  country_mapping_file = 'resources/country_partner_mapping.csv') {

  message('Loading IEEPA rates from YAML...')

  # Read YAML configuration
  config <- read_yaml(yaml_file)

  # Read HS10-GTAP crosswalk
  crosswalk <- read_csv(crosswalk_file, show_col_types = FALSE) %>%
    mutate(hs10 = as.character(hs10))

  # Get unique HS10 codes
  hs10_codes <- unique(crosswalk$hs10)

  # Read country mapping to get all possible country codes
  country_mapping <- read_csv(
    country_mapping_file,
    col_types = cols(cty_code = col_character()),
    show_col_types = FALSE
  )

  # Get all unique country codes
  all_country_codes <- unique(country_mapping$cty_code)

  # Get default rate
  default_rate <- config$headline_rates$default
  if (is.null(default_rate)) {
    stop('IEEPA headline_rates is missing required "default" rate')
  }

  # ===========================
  # Step 1: Initialize with headline rates
  # ===========================

  # Create matrix with headline rates for all HS10 x country combinations
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
    select(hs10, cty_code, rate) %>% 
    arrange(hs10)
    

  # ===========================
  # Step 2: Apply product-level rates
  # ===========================

  if (!is.null(config$product_rates) && length(config$product_rates) > 0) {

    # Build expanded lookup table for all product rate overrides
    # Each product rate is a simple numeric value that applies to all countries
    product_rates_expanded <- names(config$product_rates) %>%
      map_df(function(hts_code) {
        # Find all HS10 codes matching this prefix
        matching_hs10 <- hs10_codes[str_starts(hs10_codes, hts_code)]

        tibble(
          hs10 = matching_hs10,
          rate_override = config$product_rates[[hts_code]]
        )
      })

    # Single join operation to apply all product rate overrides
    rate_matrix <- rate_matrix %>%
      left_join(product_rates_expanded, by = 'hs10') %>%
      mutate(rate = if_else(!is.na(rate_override), rate_override, rate)) %>%
      select(-rate_override)
  }


  # ===========================
  # Step 3: Apply product x country rates
  # ===========================

  # TODO
  
  # ===========================
  # Return long format with default rate for unmapped countries
  # ===========================

  message(sprintf('Loaded IEEPA rates for %d HS10 codes x %d countries',
                  length(hs10_codes), length(all_country_codes)))

  # Extract product-level defaults that apply to ALL countries (including unmapped)
  # These override the headline default_rate for specific HS10 codes
  product_defaults <- NULL
  if (!is.null(config$product_rates) && length(config$product_rates) > 0) {
    product_defaults <- names(config$product_rates) %>%
      map_df(function(hts_code) {
        matching_hs10 <- hs10_codes[str_starts(hs10_codes, hts_code)]
        tibble(
          hs10 = matching_hs10,
          product_default = config$product_rates[[hts_code]]
        )
      })
  }

  # Return list with rate_matrix, default_rate, and product_defaults
  # - rate_matrix: rates for mapped countries (in country_partner_mapping.csv)
  # - default_rate: headline default for unmapped countries with no product override
  # - product_defaults: product-level defaults that override headline default for specific HS10 codes
  return(list(
    rate_matrix = rate_matrix,
    default_rate = default_rate,
    product_defaults = product_defaults
  ))
}
