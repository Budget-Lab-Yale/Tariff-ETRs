# =============================================================================
# config_parsing.R
# =============================================================================
#
# Functions for loading and parsing tariff configuration files.
#
# Functions:
#   - get_mnemonic_mapping():     Get mnemonic → country code mapping
#   - resolve_country_mnemonics(): Resolve mnemonics in rates config
#   - load_232_rates():           Load Section 232 rates at country level with defaults
#   - load_ieepa_rates_yaml():    Load IEEPA rates at country level with hierarchical config
#   - load_metal_content():       Load metal content shares for 232 derivative adjustment
#
# =============================================================================


#' Get mapping from mnemonics to Census country codes
#'
#' Returns a named list where names are mnemonics (e.g., 'china', 'eu') and
#' values are vectors of Census country codes. This allows config files to use
#' friendly names instead of numeric codes.
#'
#' @param country_partner_file Path to country_partner_mapping.csv
#'
#' @return Named list: mnemonic → character vector of country codes
get_mnemonic_mapping <- function(country_partner_file = 'resources/country_partner_mapping.csv') {

  # Read the partner mapping file
  mapping <- read_csv(country_partner_file, show_col_types = FALSE) %>%
    mutate(cty_code = as.character(cty_code))

  # Build the mnemonic → codes mapping
  # Group by partner and collect all country codes for each
  mnemonic_map <- mapping %>%
    group_by(partner) %>%
    summarise(codes = list(cty_code), .groups = 'drop') %>%
    deframe()

  return(mnemonic_map)
}


#' Resolve mnemonics in a rates configuration list
#'
#' Takes a rates config (named list of code → rate) and expands any mnemonics
#' (like 'china', 'eu', 'canada') to their constituent Census country codes.
#' Preserves 'default' and numeric country codes as-is.
#'
#' @param rates_config Named list with keys like 'default', '5700', 'china', 'eu'
#' @param mnemonic_map Named list from get_mnemonic_mapping()
#'
#' @return Named list with all mnemonics expanded to individual country codes
#' Expand variable-length HTS codes to matching HS10 codes (vectorized)
#'
#' Groups HTS codes by prefix length and uses vectorized `%in%` matching
#' instead of iterating one code at a time with str_starts. For 17K 10-digit
#' codes, this collapses to a single `%in%` operation vs 17K individual calls.
#'
#' @param hts_codes Character vector of HTS codes (4, 6, 8, or 10 digits)
#' @param hs10_universe Character vector of all valid HS10 codes
#'
#' @return Character vector of matching HS10 codes (deduplicated)
expand_hts_to_hs10 <- function(hts_codes, hs10_universe) {
  hts_codes <- as.character(hts_codes)
  hts_by_len <- split(hts_codes, nchar(hts_codes))
  matches <- unlist(lapply(names(hts_by_len), function(len) {
    len_int <- as.integer(len)
    hs10_universe[substr(hs10_universe, 1, len_int) %in% hts_by_len[[len]]]
  }))
  unique(matches)
}


resolve_country_mnemonics <- function(rates_config, mnemonic_map) {

  resolved <- list()
  valid_mnemonics <- names(mnemonic_map)

  for (key in names(rates_config)) {
    rate <- rates_config[[key]]

    if (key == 'default') {
      # Keep default as-is
      resolved[['default']] <- rate
    } else if (key %in% valid_mnemonics) {
      # This is a mnemonic - expand to all constituent country codes
      country_codes <- mnemonic_map[[key]]
      for (code in country_codes) {
        resolved[[code]] <- rate
      }
    } else if (grepl('^[0-9]+$', key)) {
      # Valid numeric country code
      resolved[[key]] <- rate
    } else {
      # Invalid key - not 'default', not a mnemonic, not numeric
      stop(
        'Invalid country identifier in config: "', key, '"\n',
        '  Must be one of:\n',
        '    - "default" for the default rate\n',
        '    - A valid mnemonic (lowercase): ', paste(valid_mnemonics, collapse = ', '), '\n',
        '    - A numeric Census country code (e.g., "5700" for China)\n',
        '  Note: Mnemonics are case-sensitive and must be lowercase.'
      )
    }
  }

  return(resolved)
}




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
#'       '5700': 0.25  # China (or use 'china' mnemonic)
#'       china: 0.25   # Mnemonic form
#'       eu: 0.15      # Expands to all 27 EU country codes
#'     usmca_exempt: 0
#'
#' Supported mnemonics: china, canada, mexico, uk, japan, eu, ftrow
#' (based on resources/country_partner_mapping.csv)
#'
#' @param yaml_file Path to 232 YAML configuration file
#' @param crosswalk_file Path to HS10-GTAP crosswalk CSV (for HS10 universe)
#' @param census_codes_file Path to Census country codes CSV (for country universe)
#' @param country_partner_file Path to country-partner mapping CSV (for mnemonic resolution)
#'
#' @return List with two elements:
#'   - rate_matrix: Tibble with columns hs10, cty_code, s232_[tariff]_rate (one per tariff)
#'   - usmca_exempt: Named vector of usmca_exempt flags by tariff name
load_232_rates <- function(yaml_file,
                           crosswalk_file = 'resources/hs10_gtap_crosswalk.csv',
                           census_codes_file = 'resources/census_codes.csv',
                           country_partner_file = 'resources/country_partner_mapping.csv') {

  message('Loading Section 232 rates from YAML...')

  # Read YAML configuration
  params_232 <- read_yaml(yaml_file)

  # Load mnemonic mapping for resolving friendly country names

  mnemonic_map <- get_mnemonic_mapping(country_partner_file)

  # Read HS10 universe from crosswalk
  crosswalk <- read_csv(crosswalk_file, show_col_types = FALSE) %>%
    mutate(hs10 = as.character(hs10))
  hs10_codes <- unique(crosswalk$hs10)

  # Read country universe from Census codes
  all_country_codes <- load_census_codes(census_codes_file)$cty_code

  # Extract USMCA exempt flags
  usmca_exempt_flags <- sapply(names(params_232), function(t) params_232[[t]]$usmca_exempt)

  # For each tariff, build a sparse rate matrix (only non-zero rates)
  tariff_matrices <- list()

  for (tariff_name in names(params_232)) {

    # Get HTS codes that define coverage
    hts_codes <- params_232[[tariff_name]]$base

    # Get rates config and resolve mnemonics (e.g., 'china' → '5700', 'eu' → 27 codes)
    rates_config <- resolve_country_mnemonics(params_232[[tariff_name]]$rates, mnemonic_map)
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
#' Optional:
#' - target_total_rules: Country-level "combined duty target" for reciprocal
#'   policies. When present, downstream calculations use max(target - MFN, 0)
#'   for rows with positive reciprocal coverage.
#'
#' Supported mnemonics in headline_rates: china, canada, mexico, uk, japan, eu, ftrow
#' (based on resources/country_partner_mapping.csv)
#'
#' @param yaml_file Path to IEEPA YAML configuration file
#' @param rate_col_name Name for the rate column in output tibble (e.g., 'ieepa_reciprocal_rate')
#' @param crosswalk_file Path to HS10-GTAP crosswalk CSV file
#' @param census_codes_file Path to Census country codes CSV file
#' @param country_partner_file Path to country-partner mapping CSV (for mnemonic resolution)
#'
#' @return Tibble with columns: hs10, cty_code, [rate_col_name], and optional
#'   target_total_rate when target_total_rules is provided.
load_ieepa_rates_yaml <- function(yaml_file,
                                  rate_col_name = 'ieepa_rate',
                                  crosswalk_file = 'resources/hs10_gtap_crosswalk.csv',
                                  census_codes_file = 'resources/census_codes.csv',
                                  country_partner_file = 'resources/country_partner_mapping.csv') {

  message('Loading IEEPA rates from YAML...')

  # Read YAML configuration
  config <- read_yaml(yaml_file)

  # Load mnemonic mapping for resolving friendly country names
  mnemonic_map <- get_mnemonic_mapping(country_partner_file)

  # Read HS10 universe
  crosswalk <- read_csv(crosswalk_file, show_col_types = FALSE) %>%
    mutate(hs10 = as.character(hs10))
  hs10_codes <- unique(crosswalk$hs10)

  # Read country universe
  all_country_codes <- load_census_codes(census_codes_file)$cty_code

  # Resolve mnemonics in headline_rates (e.g., 'china' → '5700', 'eu' → 27 codes)
  headline_rates <- resolve_country_mnemonics(config$headline_rates, mnemonic_map)

  # Resolve optional target_total_rules using the same country-id conventions.
  # This is used for reciprocal policies where legal text specifies a target
  # combined rate instead of a flat add-on.
  target_total_rules <- NULL
  if (!is.null(config$target_total_rules) && length(config$target_total_rules) > 0) {
    target_total_rules <- resolve_country_mnemonics(config$target_total_rules, mnemonic_map)
    if (!is.null(target_total_rules$default)) {
      stop('target_total_rules does not support "default"; provide explicit countries only')
    }
  }

  # Get default rate
  default_rate <- headline_rates$default
  if (is.null(default_rate)) {
    stop('IEEPA headline_rates is missing required "default" rate')
  }

  # ===========================
  # Step 1: Initialize with headline rates
  # ===========================

  # Build country-level headline rates (just ~240 rows)
  country_headline <- tibble(cty_code = all_country_codes) %>%
    mutate(
      rate = sapply(cty_code, function(code) {
        country_rate <- headline_rates[[code]]
        if (!is.null(country_rate)) country_rate else default_rate
      }),
      target_total_rate = sapply(cty_code, function(code) {
        if (!is.null(target_total_rules) && !is.null(target_total_rules[[code]])) {
          target_total_rules[[code]]
        } else {
          NA_real_
        }
      })
    )

  # Only build the full HS10 × country grid if there are non-zero headline rates
  # or product_rates that apply to all countries. Otherwise, product_country_rates
  # will build the sparse result directly (avoids creating millions of zero rows).
  has_product_rates <- !is.null(config$product_rates) && length(config$product_rates) > 0
  needs_full_grid <- any(country_headline$rate > 0) || has_product_rates

  if (needs_full_grid) {
    rate_matrix <- country_headline %>%
      expand_grid(hs10 = hs10_codes) %>%
      select(hs10, cty_code, rate, target_total_rate)
  } else {
    rate_matrix <- tibble(
      hs10 = character(), cty_code = character(),
      rate = numeric(), target_total_rate = numeric()
    )
  }

  # ===========================
  # Step 2: Apply product-level rates (apply to ALL countries)
  # ===========================

  if (!is.null(config$product_rates) && length(config$product_rates) > 0) {

    # Build product rate lookup (vectorized by prefix length)
    product_rate_lookup <- tibble(
      product_rate_prefix = names(config$product_rates),
      product_rate = as.numeric(config$product_rates),
      prefix_len = nchar(product_rate_prefix)
    )
    product_rates_expanded <- product_rate_lookup %>%
      split(.$prefix_len) %>%
      map_df(function(group) {
        len <- group$prefix_len[1]
        tibble(hs10 = hs10_codes, prefix = substr(hs10_codes, 1, len)) %>%
          inner_join(group %>% select(prefix = product_rate_prefix, product_rate),
                     by = 'prefix') %>%
          mutate(product_rate_prefix = prefix) %>%
          select(hs10, product_rate, product_rate_prefix)
      })

    # Guardrail: overlapping prefixes (e.g., 84 and 8407) would duplicate rows
    # in the join below and silently distort weighted calculations.
    dupes <- product_rates_expanded %>%
      group_by(hs10) %>%
      summarise(
        n = n(),
        prefixes = paste(sort(unique(product_rate_prefix)), collapse = ', '),
        .groups = 'drop'
      ) %>%
      filter(n > 1)
    if (nrow(dupes) > 0) {
      examples <- dupes %>%
        slice_head(n = 5) %>%
        transmute(example = sprintf('%s -> [%s]', hs10, prefixes)) %>%
        pull(example) %>%
        paste(collapse = '; ')
      stop(
        'Overlapping prefixes in product_rates produced duplicate HS10 matches for ',
        nrow(dupes), ' HS10 codes. ',
        'This would duplicate rows during joins. ',
        'Fix product_rates so each HS10 matches at most one prefix. ',
        'Examples: ', examples
      )
    }

    # Apply product rate overrides
    rate_matrix <- rate_matrix %>%
      left_join(
        product_rates_expanded %>% select(hs10, product_rate),
        by = 'hs10'
      ) %>%
      mutate(rate = if_else(!is.na(product_rate), product_rate, rate)) %>%
      select(-product_rate)
  }

  # ===========================
  # Step 3: Apply product × country rates
  # ===========================

  if (!is.null(config$product_country_rates) && length(config$product_country_rates) > 0) {

    # Build product×country rate lookup by iterating through each exemption
    product_country_overrides <- config$product_country_rates %>%
      map_df(function(exemption) {
        # Get one or more country identifiers and rate for this exemption.
        # Backward compatible:
        # - country: single string/code
        # - countries: vector/list of strings/codes
        country_ids <- exemption$countries
        if (is.null(country_ids)) {
          country_ids <- exemption$country
        }
        country_ids <- as.character(unlist(country_ids))
        if (length(country_ids) == 0) {
          stop(
            'product_country_rates entry is missing country/countries (in exemption "',
            exemption$name, '")'
          )
        }
        exemption_rate <- exemption$rate

        # Resolve each mnemonic/code to Census country codes
        country_codes <- map(country_ids, function(country_id) {
          if (country_id %in% names(mnemonic_map)) {
            mnemonic_map[[country_id]]
          } else if (grepl('^[0-9]+$', country_id)) {
            country_id
          } else {
            stop(
              'Invalid country identifier in product_country_rates: "', country_id, '"',
              ' (in exemption "', exemption$name, '")\n',
              '  Must be one of:\n',
              '    - A valid mnemonic (lowercase): ', paste(names(mnemonic_map), collapse = ', '), '\n',
              '    - A numeric Census country code (e.g., "5700" for China)\n',
              '  Note: Mnemonics are case-sensitive and must be lowercase.'
            )
          }
        }) %>%
          unlist() %>%
          unique()

        # Expand HTS codes to matching HS10 codes (vectorized)
        matching_hs10 <- expand_hts_to_hs10(unlist(exemption$hts), hs10_codes)

        # Create a tibble for this exemption (expanding across all country codes)
        if (length(matching_hs10) > 0) {
          expand_grid(
            hs10 = matching_hs10,
            cty_code = country_codes
          ) %>%
            mutate(product_country_rate = exemption_rate)
        } else {
          tibble(
            hs10 = character(),
            cty_code = character(),
            product_country_rate = numeric()
          )
        }
      })

    # Check for duplicate HS10 x country pairs in product_country_rates
    dupes <- product_country_overrides %>%
      group_by(hs10, cty_code) %>%
      filter(n() > 1)
    if (nrow(dupes) > 0) {
      stop(sprintf('Duplicate HS10 x country entries in product_country_rates: %d duplicates found',
                   nrow(dupes)))
    }

    # Apply product×country rate overrides (upsert: override existing + add new)
    # Uses anti_join + bind_rows instead of left_join + if_else so that
    # product_country_rates can add rows not in rate_matrix (sparse grid case).
    pc_with_target <- product_country_overrides %>%
      left_join(
        country_headline %>% select(cty_code, target_total_rate),
        by = 'cty_code'
      ) %>%
      rename(rate = product_country_rate)

    rate_matrix <- rate_matrix %>%
      anti_join(product_country_overrides, by = c('hs10', 'cty_code')) %>%
      bind_rows(pc_with_target)
  }

  # ===========================
  # Filter to sparse matrix (only non-zero rates) and return
  # ===========================

  rate_matrix <- rate_matrix %>%
    filter(rate > 0) %>%
    rename(!!rate_col_name := rate)

  # Keep output compact for non-target-total policies.
  if ('target_total_rate' %in% names(rate_matrix) && all(is.na(rate_matrix$target_total_rate))) {
    rate_matrix <- rate_matrix %>% select(-target_total_rate)
  }

  message(sprintf('Loaded %s for %s HS10 × country combinations',
                  rate_col_name, format(nrow(rate_matrix), big.mark = ',')))

  return(rate_matrix)
}


#' Load metal content shares for Section 232 derivative adjustment
#'
#' Supports three methods:
#' - 'flat': Uniform metal_share for all products (from flat_share param)
#' - 'bea': Industry-varying shares from pre-computed BEA I-O data
#'          Config option bea_table: 'domestic' (default) or 'total'
#'          Config option bea_granularity: 'gtap' (default), 'naics', or 'detail'
#'            - 'gtap': GTAP sector-level shares (~45 sectors)
#'            - 'naics': HS10-level shares via HS10->NAICS->BEA summary chain (~20K products)
#'            - 'detail': Per-metal-type shares (steel, aluminum, copper, other) via
#'              2017 BEA Detail IO table (~402 commodities, 10 metal sub-industries)
#' - 'cbo': Product-level shares from CBO bucket classification (high=0.75, low=0.25, copper=0.90)
#'
#' Primary products (chapters in primary_chapters) are forced to metal_share = 1.0
#' regardless of method, since the tariff applies to their full customs value.
#' In detail mode, primary chapters also get per-type shares set appropriately
#' (e.g., ch72/73 -> steel_share=1.0, ch76 -> aluminum_share=1.0).
#'
#' When metal_content_config is NULL (old configs without this block),
#' defaults to flat method with share = 1.0, producing identical behavior
#' to the pre-metal-content codebase.
#'
#' @param metal_content_config The metal_content block from other_params.yaml (or NULL)
#' @param import_data Tibble with hs10, gtap_code columns (for joining)
#'
#' @return Tibble with columns: hs10, metal_share (one row per unique hs10).
#'   In detail mode, also includes: steel_share, aluminum_share, copper_share, other_metal_share.
load_metal_content <- function(metal_content_config = NULL,
                               import_data) {

  method <- metal_content_config$method %||% 'flat'
  flat_share <- metal_content_config$flat_share %||% 1.0
  primary_chapters <- metal_content_config$primary_chapters %||% c('72', '73', '76')

  if (method == 'flat') {

    message(sprintf('Metal content: flat method (share = %.2f)', flat_share))
    shares <- import_data %>%
      distinct(hs10) %>%
      mutate(metal_share = flat_share)

  } else if (method == 'bea') {

    bea_table <- metal_content_config$bea_table %||% 'domestic'
    bea_granularity <- metal_content_config$bea_granularity %||% 'gtap'
    message(sprintf('Metal content: BEA I-O method (%s requirements, %s granularity)',
                    bea_table, bea_granularity))

    if (bea_granularity == 'naics') {

      # HS10-level shares via HS10 -> NAICS -> BEA summary code chain
      naics_shares_file <- sprintf('resources/metal_content_shares_naics_%s.csv', bea_table)
      if (!file.exists(naics_shares_file)) {
        stop(sprintf('NAICS shares file not found: %s\n  Run: Rscript scripts/build_metal_content_shares.R', naics_shares_file))
      }

      naics_shares <- read_csv(naics_shares_file, show_col_types = FALSE,
                               col_types = cols(hs10 = col_character())) %>%
        select(hs10, metal_share)

      # Also load GTAP-level shares as fallback for HS10 codes not in NAICS crosswalk
      gtap_shares_file <- sprintf('resources/metal_content_shares_%s.csv', bea_table)
      if (!file.exists(gtap_shares_file)) {
        stop(sprintf('GTAP shares file not found: %s\n  Run: Rscript scripts/build_metal_content_shares.R', gtap_shares_file))
      }

      gtap_shares <- read_csv(gtap_shares_file, show_col_types = FALSE) %>%
        mutate(gtap_code = tolower(gtap_code)) %>%
        select(gtap_code, metal_share_gtap = metal_share)

      shares <- import_data %>%
        distinct(hs10, gtap_code) %>%
        mutate(gtap_code = tolower(gtap_code)) %>%
        left_join(naics_shares, by = 'hs10') %>%
        left_join(gtap_shares, by = 'gtap_code') %>%
        # Use NAICS share if available, fall back to GTAP share, then 1.0
        mutate(metal_share = case_when(
          !is.na(metal_share)      ~ metal_share,
          !is.na(metal_share_gtap) ~ metal_share_gtap,
          TRUE                     ~ 1.0
        )) %>%
        select(hs10, metal_share)

      n_naics <- sum(!is.na(import_data %>% distinct(hs10) %>%
                             left_join(naics_shares, by = 'hs10') %>%
                             pull(metal_share)))
      n_total <- import_data %>% distinct(hs10) %>% nrow()
      message(sprintf('  NAICS coverage: %d of %d HS10 codes (%.1f%%), remainder use GTAP fallback',
                      n_naics, n_total, 100 * n_naics / n_total))

    } else if (bea_granularity == 'detail') {

      # HS10-level per-metal-type shares via HS10 -> NAICS -> BEA detail code chain
      detail_shares_file <- sprintf('resources/metal_content_shares_detail_hs10_%s.csv', bea_table)
      if (!file.exists(detail_shares_file)) {
        stop(sprintf('Detail shares file not found: %s\n  Run: Rscript scripts/build_metal_content_shares.R',
                      detail_shares_file))
      }

      detail_shares <- read_csv(detail_shares_file, show_col_types = FALSE,
                                 col_types = cols(hs10 = col_character())) %>%
        select(hs10, bea_detail_code, steel_share, aluminum_share, copper_share, other_metal_share, metal_share)

      # Also load GTAP-level aggregate shares as fallback for unmatched HS10 codes
      gtap_shares_file <- sprintf('resources/metal_content_shares_%s.csv', bea_table)
      if (!file.exists(gtap_shares_file)) {
        stop(sprintf('GTAP shares file not found: %s\n  Run: Rscript scripts/build_metal_content_shares.R',
                      gtap_shares_file))
      }

      gtap_shares <- read_csv(gtap_shares_file, show_col_types = FALSE) %>%
        mutate(gtap_code = tolower(gtap_code)) %>%
        select(gtap_code, metal_share_gtap = metal_share)

      shares <- import_data %>%
        distinct(hs10, gtap_code) %>%
        mutate(gtap_code = tolower(gtap_code)) %>%
        left_join(detail_shares, by = 'hs10') %>%
        left_join(gtap_shares, by = 'gtap_code') %>%
        mutate(
          # Unmatched: per-type shares = 0, total falls back to GTAP then 1.0
          steel_share       = if_else(is.na(steel_share), 0, steel_share),
          aluminum_share    = if_else(is.na(aluminum_share), 0, aluminum_share),
          copper_share      = if_else(is.na(copper_share), 0, copper_share),
          other_metal_share = if_else(is.na(other_metal_share), 0, other_metal_share),
          metal_share       = case_when(
            !is.na(metal_share)      ~ metal_share,
            !is.na(metal_share_gtap) ~ metal_share_gtap,
            TRUE                     ~ 1.0
          )
        ) %>%
        select(hs10, steel_share, aluminum_share, copper_share, other_metal_share, metal_share)

      n_detail <- import_data %>% distinct(hs10) %>%
        left_join(detail_shares %>% select(hs10, bea_detail_code), by = 'hs10') %>%
        filter(!is.na(bea_detail_code)) %>% nrow()
      n_total <- import_data %>% distinct(hs10) %>% nrow()
      message(sprintf('  Detail coverage: %d of %d HS10 codes (%.1f%%), remainder use GTAP fallback',
                      n_detail, n_total, 100 * n_detail / n_total))

    } else if (bea_granularity == 'gtap') {

      # Original GTAP sector-level shares
      shares_file <- sprintf('resources/metal_content_shares_%s.csv', bea_table)
      if (!file.exists(shares_file)) {
        stop(sprintf('BEA shares file not found: %s\n  Run: Rscript scripts/build_metal_content_shares.R', shares_file))
      }

      bea_shares <- read_csv(shares_file, show_col_types = FALSE) %>%
        mutate(gtap_code = tolower(gtap_code)) %>%
        select(gtap_code, metal_share)

      shares <- import_data %>%
        distinct(hs10, gtap_code) %>%
        mutate(gtap_code = tolower(gtap_code)) %>%
        left_join(bea_shares, by = 'gtap_code') %>%
        # Products with no GTAP match default to 1.0 (conservative)
        mutate(metal_share = if_else(is.na(metal_share), 1.0, metal_share)) %>%
        select(hs10, metal_share)

    } else {
      stop(sprintf('Unknown bea_granularity: "%s" (expected "gtap", "naics", or "detail")', bea_granularity))
    }

  } else if (method == 'cbo') {

    message('Metal content: CBO bucket method')

    # Read configurable share values (CBO defaults)
    high_share   <- metal_content_config$cbo_high_share   %||% 0.75
    low_share    <- metal_content_config$cbo_low_share     %||% 0.25
    copper_share <- metal_content_config$cbo_copper_share  %||% 0.90

    # Read CBO HTS lists
    cbo_high   <- read_csv('resources/cbo/alst_deriv_h.csv', show_col_types = FALSE)
    cbo_low    <- read_csv('resources/cbo/alst_deriv_l.csv', show_col_types = FALSE)
    cbo_copper <- read_csv('resources/cbo/copper.csv', show_col_types = FALSE)

    # Build HS10 -> metal_share lookup (priority: copper > high > low)
    # CBO lists have some overlaps and duplicates, so we bind in priority order
    # and keep the first match per hs10
    cbo_shares <- bind_rows(
      cbo_copper %>% transmute(hs10 = as.character(I_COMMODITY), metal_share = copper_share),
      cbo_high   %>% transmute(hs10 = as.character(I_COMMODITY), metal_share = high_share),
      cbo_low    %>% transmute(hs10 = as.character(I_COMMODITY), metal_share = low_share)
    ) %>%
      distinct(hs10, .keep_all = TRUE)

    # Join with import data universe
    shares <- import_data %>%
      distinct(hs10) %>%
      left_join(cbo_shares, by = 'hs10') %>%
      mutate(metal_share = if_else(is.na(metal_share), 1.0, metal_share))

  } else {
    stop(sprintf('Unknown metal_content method: "%s" (expected "flat", "bea", or "cbo")', method))
  }

  # Force primary chapters to 1.0 (tariff applies to full customs value)
  is_primary <- substr(shares$hs10, 1, 2) %in% primary_chapters
  shares <- shares %>%
    mutate(metal_share = if_else(is_primary, 1.0, metal_share))

  # For detail mode: set per-type shares for primary chapters
  if ('steel_share' %in% names(shares)) {
    shares <- shares %>%
      mutate(
        hts2 = substr(hs10, 1, 2),
        steel_share       = if_else(is_primary & hts2 %in% c('72', '73'), 1.0,
                            if_else(is_primary, 0, steel_share)),
        aluminum_share    = if_else(is_primary & hts2 == '76', 1.0,
                            if_else(is_primary, 0, aluminum_share)),
        copper_share      = if_else(is_primary & hts2 == '74', 1.0,
                            if_else(is_primary, 0, copper_share)),
        other_metal_share = if_else(is_primary, 0, other_metal_share)
      ) %>%
      select(-hts2)
  }

  message(sprintf('Metal content shares: %d products, mean = %.4f, range = [%.4f, %.4f]',
                  nrow(shares), mean(shares$metal_share),
                  min(shares$metal_share), max(shares$metal_share)))

  return(shares)
}
