# =============================================================================
# config_parsing.R
# =============================================================================
#
# Functions for loading and parsing tariff configuration files.
#
# Functions:
#   - deduplicate_232_codes():    Deduplicate HTS codes across Section 232 tariffs
#   - load_ieepa_rates_yaml():    Load IEEPA rates from hierarchical YAML config
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


#' Load IEEPA rates from YAML configuration with hierarchical rate structure
#'
#' Reads IEEPA rates from YAML file with three levels of specificity:
#' 1. Headline rates: Default rate for each partner across all HS10 codes
#' 2. Product rates: Override headline for specific HTS codes (variable length)
#' 3. Product x country rates: Override everything for specific HTS x partner combinations
#'
#' @param yaml_file Path to IEEPA YAML configuration file
#' @param crosswalk_file Path to HS10-GTAP crosswalk CSV file
#'
#' @return Tibble with columns: hs10, partner, rate (long format)
load_ieepa_rates_yaml <- function(yaml_file, crosswalk_file = 'resources/hs10_gtap_crosswalk.csv') {

  message('Loading IEEPA rates from YAML...')

  # Read YAML configuration
  config <- read_yaml(yaml_file)

  # Read HS10-GTAP crosswalk (still needed for HS10 code list)
  crosswalk <- read_csv(crosswalk_file, show_col_types = FALSE) %>%
    mutate(hs10 = as.character(hs10))

  # Get unique HS10 codes
  hs10_codes <- unique(crosswalk$hs10)

  # Define partner list
  partners <- c('china', 'canada', 'mexico', 'uk', 'japan', 'eu', 'row', 'ftrow')

  # ===========================
  # Step 1: Initialize with headline rates
  # ===========================

  # Create matrix with headline rates for all HS10 x partner combinations
  rate_matrix <- expand.grid(
    hs10 = hs10_codes,
    partner = partners,
    stringsAsFactors = FALSE
  ) %>%
    as_tibble() %>%
    mutate(
      rate = case_when(
        partner == 'china'  ~ config$headline_rates$china,
        partner == 'canada' ~ config$headline_rates$canada,
        partner == 'mexico' ~ config$headline_rates$mexico,
        partner == 'uk'     ~ config$headline_rates$uk,
        partner == 'japan'  ~ config$headline_rates$japan,
        partner == 'eu'     ~ config$headline_rates$eu,
        partner == 'row'    ~ config$headline_rates$row,
        partner == 'ftrow'  ~ config$headline_rates$ftrow,
        TRUE ~ 0
      )
    )

  # ===========================
  # Step 2: Apply product-level rates
  # ===========================

  if (!is.null(config$product_rates) && length(config$product_rates) > 0) {

    # config$product_rates is a simple dict: HTS code (key) -> rate (value)
    # Apply each rate to ALL partners

    for (hts_code in names(config$product_rates)) {

      rate <- config$product_rates[[hts_code]]

      # Match HS10 codes using prefix matching
      pattern <- paste0('^', hts_code)

      # Update rate matrix for these HS10 codes (all partners)
      rate_matrix <- rate_matrix %>%
        mutate(
          rate = if_else(
            str_detect(hs10, pattern),
            !!rate,
            rate
          )
        )
    }
  }

  # ===========================
  # Step 3: Apply product x country rates
  # ===========================

  if (!is.null(config$product_country_rates) && length(config$product_country_rates) > 0) {

    for (prod_country_rate in config$product_country_rates) {

      hts_codes <- prod_country_rate$hts_codes
      target_partner <- prod_country_rate$partner
      rate <- prod_country_rate$rate

      # Match HS10 codes using prefix matching
      pattern <- paste0('^(', paste(hts_codes, collapse = '|'), ')')

      # Update rate matrix for these specific HS10 codes and partner
      rate_matrix <- rate_matrix %>%
        mutate(
          rate = if_else(
            str_detect(hs10, pattern) & partner == !!target_partner,
            !!rate,
            rate
          )
        )
    }
  }

  # ===========================
  # Return long format
  # ===========================

  message(sprintf('Loaded IEEPA rates for %d HS10 codes x %d partners',
                  length(hs10_codes), length(partners)))

  return(rate_matrix)
}
