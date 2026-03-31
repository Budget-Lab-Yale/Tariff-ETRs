# =============================================================================
# config_parsing.R
# =============================================================================
#
# Functions for loading and parsing tariff configuration files.
#
# Functions:
#   - get_mnemonic_mapping():     Get mnemonic → country code mapping
#   - resolve_country_mnemonics(): Resolve mnemonics in rates config
#   - load_s232_rates():           Load Section 232 rates at country level with defaults
#   - load_ieepa_rates_yaml():    Load IEEPA rates at country level with hierarchical config
#   - load_metal_content():       Load metal content shares for 232 derivative adjustment
#   - loader_context():            Shared context: HS10 universe, country codes, mnemonic map
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


#' Load shared context for YAML rate loaders
#'
#' Returns the HS10 universe, country code universe, and mnemonic mapping
#' that both load_s232_rates() and load_ieepa_rates_yaml() need.
#'
#' @param crosswalk_file Path to HS10-GTAP crosswalk CSV
#' @param census_codes_file Path to Census country codes CSV
#' @param country_partner_file Path to country-partner mapping CSV
#'
#' @return Named list with: hs10_codes, all_country_codes, mnemonic_map
loader_context <- function(crosswalk_file = 'resources/hs10_gtap_crosswalk.csv',
                           census_codes_file = 'resources/census_codes.csv',
                           country_partner_file = 'resources/country_partner_mapping.csv') {
  crosswalk <- read_csv(crosswalk_file, show_col_types = FALSE) %>%
    mutate(hs10 = as.character(hs10))

  list(
    hs10_codes         = unique(crosswalk$hs10),
    all_country_codes  = load_census_codes(census_codes_file)$cty_code,
    mnemonic_map       = get_mnemonic_mapping(country_partner_file)
  )
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
#' @param yaml_file Path to s232 YAML configuration file
#' @param crosswalk_file Path to HS10-GTAP crosswalk CSV (for HS10 universe)
#' @param census_codes_file Path to Census country codes CSV (for country universe)
#' @param country_partner_file Path to country-partner mapping CSV (for mnemonic resolution)
#'
#' @return List with two elements:
#'   - rate_matrix: Tibble with columns hs10, cty_code, s232_[tariff]_rate (one per tariff)
#'   - usmca_exempt: Named vector of usmca_exempt flags by tariff name
load_s232_rates <- function(yaml_file,
                           crosswalk_file = 'resources/hs10_gtap_crosswalk.csv',
                           census_codes_file = 'resources/census_codes.csv',
                           country_partner_file = 'resources/country_partner_mapping.csv') {

  message('Loading Section 232 rates from YAML...')

  # Read YAML configuration
  params_s232 <- read_yaml(yaml_file)

  # Load shared context (HS10 universe, country codes, mnemonic map)
  ctx <- loader_context(crosswalk_file, census_codes_file, country_partner_file)
  hs10_codes <- ctx$hs10_codes
  all_country_codes <- ctx$all_country_codes
  mnemonic_map <- ctx$mnemonic_map

  # Extract USMCA exempt flags
  usmca_exempt_flags <- sapply(names(params_s232), function(t) params_s232[[t]]$usmca_exempt)

  # For each tariff, build a sparse rate matrix (only non-zero rates)
  tariff_matrices <- list()
  target_total_rules <- list()

  for (tariff_name in names(params_s232)) {

    # Get HTS codes that define coverage
    hts_codes <- params_s232[[tariff_name]]$base

    # Get rates config and resolve mnemonics (e.g., 'china' → '5700', 'eu' → 27 codes)
    rates_config <- resolve_country_mnemonics(params_s232[[tariff_name]]$rates, mnemonic_map)
    default_rate <- rates_config$default
    if (is.null(default_rate)) {
      stop(sprintf('Tariff %s is missing required "default" rate', tariff_name))
    }

    # Extract optional target_total block (country-level floor rules)
    tt_config <- params_s232[[tariff_name]]$target_total
    if (!is.null(tt_config) && length(tt_config) > 0) {
      tt_resolved <- resolve_country_mnemonics(tt_config, mnemonic_map)
      tt_resolved[['default']] <- NULL  # target_total should not have a default
      if (length(tt_resolved) > 0) {
        target_total_rules[[tariff_name]] <- tibble(
          cty_code = names(tt_resolved),
          target_total_rate = as.numeric(tt_resolved)
        )
      }
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

  # Only include target_total_rules if any programs have them
  if (length(target_total_rules) == 0) {
    target_total_rules <- NULL
  }

  message(sprintf('Loaded s232 rates for %d tariffs across %s HS10 × country combinations',
                  length(params_s232), format(nrow(rate_matrix), big.mark = ',')))

  return(list(
    rate_matrix = rate_matrix,
    usmca_exempt = usmca_exempt_flags,
    target_total_rules = target_total_rules
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
                                  country_partner_file = 'resources/country_partner_mapping.csv',
                                  overlay_mode = FALSE) {

  message('Loading IEEPA rates from YAML...')

  # Read YAML configuration
  config <- read_yaml(yaml_file)

  # Load shared context (HS10 universe, country codes, mnemonic map)
  ctx <- loader_context(crosswalk_file, census_codes_file, country_partner_file)
  hs10_codes <- ctx$hs10_codes
  all_country_codes <- ctx$all_country_codes
  mnemonic_map <- ctx$mnemonic_map

  # Exclude exempt products if specified (blanket authority minus exemption list).
  # Used for authorities like IEEPA reciprocal that apply to ALL products except
  # a specific exemption list (e.g., Annex A). Supports variable-length HTS codes.
  if (!is.null(config$exempt_products)) {
    exempt_pattern <- paste0('^(', paste(config$exempt_products, collapse = '|'), ')')
    exempt_hs10 <- hs10_codes[str_detect(hs10_codes, exempt_pattern)]
    hs10_codes <- setdiff(hs10_codes, exempt_hs10)
    message(sprintf('  Excluded %d exempt products (%d HS10 codes)',
                    length(config$exempt_products), length(exempt_hs10)))
  }

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

  # Only build the full HS10 × country grid if there are non-zero headline rates,
  # product_rates that apply to all countries, or overlay_mode (where explicit zeros
  # must be preserved to suppress base rates).
  has_product_rates <- !is.null(config$product_rates) && length(config$product_rates) > 0
  needs_full_grid <- any(country_headline$rate > 0) || has_product_rates || overlay_mode

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

  # In overlay mode, keep zeros so they can suppress CSV baseline rates.
  # In normal mode, filter to sparse (non-zero) matrix.
  if (!overlay_mode) {
    rate_matrix <- rate_matrix %>%
      filter(rate > 0)
  }

  rate_matrix <- rate_matrix %>%
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

  method <- metal_content_config$method %||% 'bea'
  primary_chapters <- metal_content_config$primary_chapters %||% c('72', '73', '76')

  # Flat method: uniform share for all products (used for sensitivity analysis)
  if (method == 'flat') {
    flat_share <- metal_content_config$flat_share %||% 1.0
    message(sprintf('Metal content: flat method (share = %.2f)', flat_share))
    shares <- import_data %>%
      distinct(hs10) %>%
      mutate(metal_share = flat_share)

    is_primary <- substr(shares$hs10, 1, 2) %in% primary_chapters
    shares <- shares %>%
      mutate(metal_share = if_else(is_primary, 1.0, metal_share))

    message(sprintf('Metal content shares: %d products, mean = %.4f, range = [%.4f, %.4f]',
                    nrow(shares), mean(shares$metal_share),
                    min(shares$metal_share), max(shares$metal_share)))
    return(shares)
  }

  bea_table <- metal_content_config$bea_table %||% 'domestic'

  message(sprintf('Metal content: BEA Detail I-O method (%s requirements)', bea_table))

  # HS10-level per-metal-type shares via HS10 -> NAICS -> BEA detail code chain
  detail_shares_file <- sprintf('resources/metal_content_shares_detail_hs10_%s.csv', bea_table)
  if (!file.exists(detail_shares_file)) {
    stop(sprintf('Detail shares file not found: %s\n  Run: Rscript scripts/build_metal_content_shares.R',
                  detail_shares_file))
  }

  detail_shares <- read_csv(detail_shares_file, show_col_types = FALSE,
                             col_types = cols(hs10 = col_character())) %>%
    select(hs10, bea_detail_code, steel_share, aluminum_share, copper_share, other_metal_share, metal_share)

  flat_share <- metal_content_config$flat_share %||% 0.50

  shares <- import_data %>%
    distinct(hs10) %>%
    left_join(detail_shares, by = 'hs10') %>%
    mutate(
      # Unmatched: per-type shares = 0, total falls back to flat_share
      steel_share       = if_else(is.na(steel_share), 0, steel_share),
      aluminum_share    = if_else(is.na(aluminum_share), 0, aluminum_share),
      copper_share      = if_else(is.na(copper_share), 0, copper_share),
      other_metal_share = if_else(is.na(other_metal_share), 0, other_metal_share),
      metal_share       = if_else(is.na(metal_share), flat_share, metal_share)
    ) %>%
    select(hs10, steel_share, aluminum_share, copper_share, other_metal_share, metal_share)

  n_detail <- import_data %>% distinct(hs10) %>%
    left_join(detail_shares %>% select(hs10, bea_detail_code), by = 'hs10') %>%
    filter(!is.na(bea_detail_code)) %>% nrow()
  n_total <- import_data %>% distinct(hs10) %>% nrow()
  message(sprintf('  Detail coverage: %d of %d HS10 codes (%.1f%%), remainder use GTAP fallback',
                  n_detail, n_total, 100 * n_detail / n_total))

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


#' Load statutory rates from dense CSV (tracker interface)
#'
#' Reads a statutory_rates.csv.gz file produced by tariff-rate-tracker and returns
#' a config list matching the schema expected by calc_etrs_for_config().
#'
#' The CSV contains pre-USMCA, pre-metal-content statutory rates per authority.
#' Section 232 programs are discovered dynamically from column names matching ^s232_.
#' MFN rates are extracted from the mfn_rate column (tracker is source of truth).
#' Target-total floor columns use 1:1 naming: target_total_X maps to rate column X.
#'
#' @param csv_path Path to statutory_rates.csv.gz
#' @param other_params Parsed other_params.yaml list (for usmca_exempt, metal params, etc.)
#' @param usmca_product_shares USMCA product shares tibble (loaded by caller)
#'
#' @return Named list matching load_scenario_config() schema
load_statutory_csv <- function(csv_path, other_params, usmca_product_shares) {

  message(sprintf('Loading statutory rates from CSV: %s', csv_path))

  csv <- read_csv(csv_path, show_col_types = FALSE,
                  col_types = cols(hts10 = col_character(), cty_code = col_character()))

  message(sprintf('  CSV dimensions: %s rows × %d columns',
                  format(nrow(csv), big.mark = ','), ncol(csv)))

  # ---------------------------------------------------------------------------
  # Discover column groups
  # ---------------------------------------------------------------------------

  all_cols <- names(csv)
  s232_cols <- all_cols[str_detect(all_cols, '^s232_') & !str_detect(all_cols, '^target_total_')]
  tt_s232_cols <- all_cols[str_detect(all_cols, '^target_total_s232_')]
  authority_cols <- intersect(
    c('ieepa_reciprocal', 'ieepa_fentanyl', 's301', 's122', 's201', 'other'),
    all_cols
  )
  tt_authority_cols <- all_cols[str_detect(all_cols, '^target_total_') & !str_detect(all_cols, '^target_total_s232_')]

  message(sprintf('  Discovered %d s232 programs: %s',
                  length(s232_cols), paste(s232_cols, collapse = ', ')))

  # Validate s232 columns match metal_programs from other_params
  metal_programs <- other_params$metal_content$metal_programs %||% character(0)
  if (length(metal_programs) > 0 && length(s232_cols) > 0) {
    csv_programs <- str_replace(s232_cols, '^s232_', '')
    missing_from_csv <- setdiff(metal_programs, csv_programs)
    missing_from_metal <- setdiff(csv_programs, metal_programs)
    if (length(missing_from_csv) > 0) {
      warning(sprintf('metal_programs lists programs not in CSV: %s',
                      paste(missing_from_csv, collapse = ', ')))
    }
    if (length(missing_from_metal) > 0) {
      message(sprintf('  Note: CSV has s232 programs not in metal_programs (non-metal): %s',
                      paste(missing_from_metal, collapse = ', ')))
    }
  }

  # ---------------------------------------------------------------------------
  # Section 232: rate_matrix + usmca_exempt + target_total_rules
  # ---------------------------------------------------------------------------

  # Rename s232_X → s232_X_rate to match ETRs convention
  s232_rename <- setNames(s232_cols, paste0(s232_cols, '_rate'))

  s232_rate_col_names <- paste0(s232_cols, '_rate')

  rates_s232_matrix <- csv %>%
    select(hs10 = hts10, cty_code, all_of(s232_cols)) %>%
    rename(!!!s232_rename) %>%
    filter(if_any(all_of(s232_rate_col_names), ~ . > 0))

  # Extract s232 target_total_rules (named list of per-program tibbles)
  target_total_rules <- list()
  for (tt_col in tt_s232_cols) {
    # target_total_s232_autos_passenger → autos_passenger
    # (strip both target_total_ AND s232_ so calc_weighted_etr can reconstruct s232_X_rate)
    program_name <- str_replace(tt_col, '^target_total_s232_', '')
    rules <- csv %>%
      filter(!is.na(!!sym(tt_col))) %>%
      distinct(cty_code, target_total_rate = !!sym(tt_col))
    if (nrow(rules) > 0) {
      target_total_rules[[program_name]] <- rules
    }
  }
  if (length(target_total_rules) == 0) target_total_rules <- NULL

  # usmca_exempt flags from other_params
  usmca_exempt <- other_params$usmca_exempt

  params_s232 <- if (nrow(rates_s232_matrix) > 0) {
    list(
      rate_matrix = rates_s232_matrix,
      usmca_exempt = usmca_exempt,
      target_total_rules = target_total_rules
    )
  } else {
    NULL
  }

  message(sprintf('  s232: %s rows', format(nrow(rates_s232_matrix), big.mark = ',')))

  # ---------------------------------------------------------------------------
  # Other authorities: extract per-authority tibbles
  # ---------------------------------------------------------------------------

  extract_authority <- function(col_name, rate_col_name) {
    if (!(col_name %in% all_cols)) return(NULL)

    tt_col <- paste0('target_total_', col_name)
    has_tt <- tt_col %in% all_cols

    result <- csv %>%
      select(hs10 = hts10, cty_code, rate = !!col_name)

    if (has_tt) {
      result <- result %>%
        mutate(target_total_rate = csv[[tt_col]])
    }

    result <- result %>%
      filter(rate > 0) %>%
      rename(!!rate_col_name := rate)

    # Drop target_total_rate if all NA
    if (has_tt && all(is.na(result$target_total_rate))) {
      result <- result %>% select(-target_total_rate)
    }

    if (nrow(result) == 0) return(NULL)

    message(sprintf('  %s: %s rows', rate_col_name, format(nrow(result), big.mark = ',')))
    result
  }

  rates_ieepa_reciprocal <- extract_authority('ieepa_reciprocal', 'ieepa_reciprocal_rate')
  rates_ieepa_fentanyl   <- extract_authority('ieepa_fentanyl', 'ieepa_fentanyl_rate')
  rates_s122             <- extract_authority('s122', 's122_rate')
  rates_s301             <- extract_authority('s301', 's301_rate')
  rates_s201             <- extract_authority('s201', 's201_rate')
  rates_other            <- extract_authority('other', 'other_rate')

  # ---------------------------------------------------------------------------
  # MFN: extract at hs10 × country level (tracker is source of truth)
  # ---------------------------------------------------------------------------

  mfn_rates_by_product_country <- NULL
  if ('mfn_rate' %in% all_cols) {
    mfn_rates_by_product_country <- csv %>%
      select(hs10 = hts10, cty_code, mfn_rate) %>%
      filter(mfn_rate > 0)
    message(sprintf('  MFN: %s rows at hs10×country level',
                    format(nrow(mfn_rates_by_product_country), big.mark = ',')))
  }

  # ---------------------------------------------------------------------------
  # Return config list matching load_scenario_config() schema
  # ---------------------------------------------------------------------------

  list(
    params_s232                    = params_s232,
    rates_ieepa_reciprocal         = rates_ieepa_reciprocal,
    rates_ieepa_fentanyl           = rates_ieepa_fentanyl,
    rates_s122                     = rates_s122,
    rates_s301                     = rates_s301,
    rates_s201                     = rates_s201,
    rates_other                    = rates_other,
    other_params                   = other_params,
    mfn_rates                      = NULL,
    mfn_rates_by_product_country   = mfn_rates_by_product_country,
    mfn_exemption_shares           = NULL,
    usmca_product_shares           = usmca_product_shares
  )
}
