# =============================================================================
# functions.R
# =============================================================================
#
# This file contains helper functions for calculating effective tariff rate
# (ETR) changes on U.S. imports by trading partner and GTAP sector. All ETR
# values represent changes from an early 2025 baseline.
#
# Functions:
#   - load_imports_hs10_country():  Load HS10 x country x year x month import data
#   - do_scenario():                Run complete ETR analysis for a scenario
#   - calc_import_shares():         Calculate import shares for specific HS6 codes
#   - calc_weighted_etr():          Calculate weighted ETR changes by partner and sector
#   - write_shock_commands():       Write GTAP shock commands to output file
#   - calc_overall_etrs():          Calculate and print overall ETR changes by country
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


#' Load HS10 x country x year x month import data from Census IMP_DETL.TXT files
#'
#' Reads monthly import data from Census Merchandise Trade Imports files (IMDByymm.ZIP).
#' Each ZIP contains IMP_DETL.TXT with fixed-width format data. The function aggregates
#' imports over districts, returning HS10 x country x year x month totals.
#'
#' @param import_data_path Path to directory containing IMDByymm.ZIP files
#' @param year Year to filter data (e.g., 2024)
#' @param type Import value type: 'con' (consumption) or 'gen' (general imports)
#'
#' @return Tibble with columns: year, month, hs10, cty_code, value
load_imports_hs10_country <- function(import_data_path, year, type = c('con', 'gen')) {

  type <- match.arg(type)

  # Extract last two digits of year for file pattern matching
  yy <- substr(as.character(year), 3, 4)

  # Find all IMDByymm.ZIP files for the specified year
  # Pattern: IMDB + YY + MM + .ZIP (e.g., IMDB2401.ZIP for January 2024)
  file_pattern <- sprintf('IMDB%s\\d{2}\\.ZIP', yy)
  zip_files <- list.files(
    path = import_data_path,
    pattern = file_pattern,
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(zip_files) == 0) {
    stop(sprintf('No import files found at %s matching pattern %s', import_data_path, file_pattern))
  }

  message(sprintf('Found %d ZIP file(s) for year %d', length(zip_files), year))

  # Column positions for IMP_DETL.TXT (1-based, inclusive)
  col_positions <- fwf_positions(
    start     = c(1,  11,  23,  27,  74,   179),
    end       = c(10, 14,  26,  28,  88,   193),
    col_names = c('hs10', 'cty_code', 'year', 'month', 'con_val_mo', 'gen_val_mo')
  )

  # Process each ZIP file
  all_records <- map_df(zip_files, function(zip_path) {

    message(sprintf('Processing: %s', basename(zip_path)))

    # List contents of ZIP file
    zip_contents <- unzip(zip_path, list = TRUE)

    # Find IMP_DETL.TXT file (case-insensitive)
    detl_file <- zip_contents$Name[grepl('IMP_DETL\\.TXT$', zip_contents$Name, ignore.case = TRUE)]

    if (length(detl_file) == 0) {
      warning(sprintf('No IMP_DETL.TXT found in %s, skipping', basename(zip_path)))
      return(tibble())
    }

    if (length(detl_file) > 1) {
      warning(sprintf('Multiple IMP_DETL.TXT files found in %s, using first', basename(zip_path)))
      detl_file <- detl_file[1]
    }

    # Create temporary directory for extraction
    temp_dir <- tempdir()

    # Extract IMP_DETL.TXT to temp directory
    extracted_path <- unzip(zip_path, files = detl_file, exdir = temp_dir, overwrite = TRUE)

    # Read fixed-width file
    records <- read_fwf(
      file = extracted_path,
      col_positions = col_positions,
      col_types = cols(
        hs10        = col_character(),
        cty_code    = col_character(),
        year        = col_integer(),
        month       = col_integer(),
        con_val_mo  = col_double(),
        gen_val_mo  = col_double()
      ),
      progress = FALSE
    )

    # Clean up temp file
    file.remove(extracted_path)

    # Filter to specified year and select appropriate value column
    records %>%
      filter(year == !!year) %>%
      mutate(
        hs10 = str_pad(hs10, width = 10, side = 'left', pad = '0')
      ) %>%
      {if (type == 'con') mutate(., value = con_val_mo) else mutate(., value = gen_val_mo)} %>%
      select(year, month, hs10, cty_code, value)
  })

  # Aggregate over districts (sum duplicates by year, month, hs10, cty_code)
  result <- all_records %>%
    group_by(year, month, hs10, cty_code) %>%
    summarise(value = sum(value), .groups = 'drop')

  message(sprintf('Loaded %s records for year %d', format(nrow(result), big.mark = ','), year))

  return(result)
}


#' Run complete ETR analysis for a given scenario
#'
#' @param scenario Scenario name (corresponds to config/{scenario}/ directory)
#' @param import_data_path Path to Census Bureau import data files
#' @param use_cache Use cached HS10 data if available (default: TRUE)
#'
#' @return Returns ETR data invisibly
do_scenario <- function(scenario, import_data_path = 'C:/Users/jar335/Downloads', use_cache = TRUE) {

  message(sprintf('\n=========================================================='))
  message(sprintf('Running scenario: %s', scenario))
  message(sprintf('==========================================================\n'))

  #---------------------------
  # Load scenario parameters
  #---------------------------

  message('Loading scenario parameters...')

  # Section 232 tariffs
  params_232 <- read_yaml(sprintf('config/%s/232.yaml', scenario))

  # Deduplicate 232 codes to prevent double-counting
  params_232 <- deduplicate_232_codes(params_232)

  # IEEPA rates
  params_ieepa <- read_csv(sprintf('config/%s/ieepa_rates.csv', scenario), show_col_types = FALSE)

  # Other scenario parameters
  other_params <- read_yaml(sprintf('config/%s/other_params.yaml', scenario))

  # USMCA share of trade by sector
  usmca_shares <- read_csv('./resources/usmca_shares.csv', show_col_types = FALSE)

  # Adjust IEEPA rates: fold Korea into ftrow and Vietnam into row
  # Note: Korea and FTROW countries are directly classified via ftrow_codes in import data,
  # but IEEPA config has separate kr column that needs to be merged into ftrow rates
  params_ieepa <- params_ieepa %>%
    mutate(
      ftrow = ftrow * (1 - other_params$kr_share_ftrow) + kr * other_params$kr_share_ftrow,
      row   = row   * (1 - other_params$vn_share_row)   + vn * other_params$vn_share_row
    ) %>%
    select(-kr, -vn)

  #------------------
  # Read import data
  #------------------

  message('Reading import data...')

  # Load country-to-partner mapping
  country_mapping <- read_csv(
    'resources/country_partner_mapping.csv',
    col_types = cols(cty_code = col_character()),
    show_col_types = FALSE
  )

  # Read GTAP crosswalk (now HS10 -> GTAP)
  crosswalk <- read_csv('resources/hs10_gtap_crosswalk.csv', show_col_types = FALSE)

  # Define cache path
  cache_dir <- 'cache'
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  cache_file <- file.path(cache_dir, 'hs10_country_2024_con.rds')

  # Load HS10-level import data (from cache or raw files)
  if (use_cache && file.exists(cache_file)) {
    message(sprintf('Loading cached HS10 data from %s...', cache_file))
    hs10_raw <- readRDS(cache_file)
    message(sprintf('Loaded %s cached records', format(nrow(hs10_raw), big.mark = ',')))
  } else {
    if (use_cache) {
      message('No cache found, processing raw import files...')
    } else {
      message('Cache disabled, processing raw import files...')
    }

    # Load HS10-level import data using new function
    hs10_raw <- load_imports_hs10_country(
      import_data_path = import_data_path,
      year = 2024,
      type = 'con'
    )

    # Save to cache for next time
    message(sprintf('Saving processed data to cache at %s...', cache_file))
    saveRDS(hs10_raw, cache_file)
    message('Cache saved successfully')
  }

  # Build data: aggregate and map to partners and GTAP sectors
  hs10_by_country <- hs10_raw %>%

    # Tag each row with a partner group using mapping
    left_join(
      country_mapping %>% select(cty_code, partner),
      by = 'cty_code'
    ) %>%
    mutate(partner = if_else(is.na(partner), 'row', partner)) %>%

    # Get totals by HS10 and partner
    group_by(hs10, partner) %>%
    summarise(imports = sum(value), .groups = 'drop') %>%

    # Expand to include all HS10 x partner combinations (fill missing with 0)
    complete(
      hs10,
      partner = c('china', 'canada', 'mexico', 'uk', 'japan', 'eu', 'row', 'ftrow'),
      fill = list(imports = 0)
    ) %>%

    # Add GTAP code
    left_join(
      crosswalk %>%
        select(hs10, gtap_code),
      by = 'hs10'
    ) %>%
    mutate(gtap_code = str_to_lower(gtap_code)) %>%
    relocate(gtap_code, .after = hs10)

  #------------------------------
  # Calculate tax bases and ETRs
  #------------------------------

  message('Calculating tax bases and ETRs...')

  # Calculate tax bases for 232 -- and IEEPA as residual
  bases <- params_232 %>%
    names() %>%

    # Get bases for each 232 tariff
    map(~ {

      # Get HTS codes from the simplified base structure
      # Structure is now just: base: [list of codes]
      hts_codes <- params_232[[.x]]$base

      # Calculate share using variable-length code matching
      share <- calc_import_shares(hts_codes, data = hs10_by_country) %>%
        mutate(tariff = .x, .before = 1)

      return(share)
    }) %>%
    bind_rows() %>%

    # Add residual -- tax base uncovered by 232
    bind_rows(
      (.) %>%
        group_by(partner, gtap_code) %>%
        summarise(
          tariff = 'residual',
          share  = 1 - sum(share),
          .groups = 'drop'
        )
    )

  # Calculate ETRs
  etrs <- calc_weighted_etr(
    bases_data            = bases,
    params_data           = params_232,
    ieepa_data            = params_ieepa,
    usmca_data            = usmca_shares,
    us_auto_content_share = other_params$us_auto_content_share,
    auto_rebate           = other_params$auto_rebate_rate,
    us_assembly_share     = other_params$us_auto_assembly_share,
    ieepa_usmca_exempt    = other_params$ieepa_usmca_exception
  )

  #------------------
  # Write outputs
  #------------------

  message('Writing outputs...')

  # Write shock commands to file
  write_shock_commands(
    etr_data    = etrs,
    output_file = 'shocks.txt',
    scenario    = scenario
  )

  # Write sector x country ETR CSV
  write_sector_country_etrs(
    etr_data     = etrs,
    output_file  = 'etrs_by_sector_country.csv',
    scenario     = scenario
  )

  # Calculate and print overall ETRs with both GTAP and 2024 Census weights
  calc_overall_etrs(
    etr_data    = etrs,
    import_data = hs10_by_country,
    scenario    = scenario
  )

  message(sprintf('\nScenario %s complete!\n', scenario))

  # Return ETR data invisibly
  invisible(etrs)
}


#' Calculate import shares for a subset of HTS codes (arbitrary length: 4, 6, 8, or 10 digits)
#'
#' Given a vector of HTS codes of any length, this function calculates:
#' 1. Total imports for those codes (using prefix matching) by partner and GTAP sector
#' 2. Total imports for ALL codes by partner and GTAP sector
#' 3. The share (subset / total) by partner and GTAP sector
#'
#' Prefix matching: A code like "8703" matches all HS10 codes starting with "8703".
#' A code like "870322" matches all HS10 codes starting with "870322", and so on.
#'
#' @param hts_codes Character vector of HTS codes to analyze (4, 6, 8, or 10 digits)
#' @param data Data frame with columns: hs10, partner, gtap_code, imports
#'
#' @return Data frame with columns: partner, gtap_code, share
calc_import_shares <- function(hts_codes, data) {

  # Handle empty code list
  if (length(hts_codes) == 0 || is.null(hts_codes)) {
    return(
      data %>%
        group_by(partner, gtap_code) %>%
        summarise(share = 0, .groups = 'drop')
    )
  }

  # Convert codes to character and remove any whitespace
  hts_codes <- as.character(hts_codes) %>% str_trim()

  # Build regex pattern for prefix matching
  # Each code becomes a prefix: "^8703" matches anything starting with 8703
  pattern <- paste0('^(', paste(hts_codes, collapse = '|'), ')')

  # Calculate imports for codes matching the pattern by partner and GTAP
  subset_imports <- data %>%
    filter(str_detect(hs10, pattern)) %>%
    group_by(partner, gtap_code) %>%
    summarise(subset_imports = sum(imports), .groups = 'drop')

  # Calculate total imports by partner and GTAP
  total_imports <- data %>%
    group_by(partner, gtap_code) %>%
    summarise(total_imports = sum(imports), .groups = 'drop')

  # Join and calculate shares
  result <- total_imports %>%
    left_join(subset_imports, by = c('partner', 'gtap_code')) %>%
    mutate(share = replace_na(subset_imports / total_imports, 0)) %>%
    select(partner, gtap_code, share)

  return(result)
}


#' Calculate weighted ETR changes by partner and GTAP sector
#'
#' Calculates change in effective tariff rates from early 2025 baseline.
#'
#' @param bases_data Data frame with tariff bases
#' @param params_data Tariff parameters list
#' @param ieepa_data Data frame with IEEPA rates by partner and GTAP sector
#' @param usmca_data Data frame with USMCA shares by partner and GTAP sector
#' @param us_auto_content_share Share of US content in auto assembly
#' @param auto_rebate Auto rebate rate
#' @param us_assembly_share Share of US assembly in autos
#' @param ieepa_usmca_exempt Apply USMCA exemption to IEEPA tariffs (1 = yes, 0 = no)
#'
#' @return Data frame with columns: partner, gtap_code, etr (change from baseline)
calc_weighted_etr <- function(bases_data, params_data,
                              ieepa_data,
                              usmca_data,
                              us_auto_content_share,
                              auto_rebate,
                              us_assembly_share,
                              ieepa_usmca_exempt) {

  # Extract rates and usmca_exempt flags from params
  rates_and_exemptions <- map(names(params_data), ~ {
    tibble(
      tariff       = .x,
      partner      = names(params_data[[.x]]$rate),
      rate         = unlist(params_data[[.x]]$rate),
      usmca_exempt = params_data[[.x]]$usmca_exempt
    )
  }) %>%
    bind_rows()

  # Reshape USMCA shares long by country
  usmca_long <- usmca_data %>%
    pivot_longer(cols = -gtap_code, names_to = 'partner', values_to = 'usmca_share') %>%
    mutate(usmca_share = replace_na(usmca_share, 0))

  # Reshape IEEPA rates long by country
  ieepa_long <- ieepa_data %>%
    pivot_longer(cols = -gtap_code, names_to = 'partner', values_to = 'ieepa_rate') %>%
    mutate(ieepa_rate = replace_na(ieepa_rate, 0))

  # Join rates, exemptions, and USMCA shares to bases
  bases_data %>%
    left_join(rates_and_exemptions, by = c('tariff', 'partner')) %>%
    left_join(usmca_long, by = c('partner', 'gtap_code')) %>%
    left_join(ieepa_long, by = c('partner', 'gtap_code')) %>%
    mutate(
      rate = replace_na(rate, 0),
      usmca_exempt = replace_na(usmca_exempt, 0),
      usmca_share = replace_na(usmca_share, 0),
      ieepa_rate = replace_na(ieepa_rate, 0)
    ) %>%

    # Apply auto rebate adjustment for all countries
    mutate(
      rate = if_else(
        tariff %in% c('automobiles_passenger_and_light_trucks', 'automobile_parts', 'vehicles_completed_mhd'),
        rate - (auto_rebate * us_assembly_share),
        rate
      )
    ) %>%

    # Apply USMCA exemption logic
    mutate(
      adjusted_usmca_share = if_else(
        tariff %in% c('automobiles_passenger_and_light_trucks', 'automobile_parts', 'vehicles_completed_mhd'),
        usmca_share * us_auto_content_share,
        usmca_share
      ),
      adjusted_rate = if_else(
        usmca_exempt == 1 & partner %in% c('canada', 'mexico'),
        rate * (1 - adjusted_usmca_share),
        rate
      )
    ) %>%

    # Apply IEEPA rates to residual with USMCA exemption
    mutate(
      adjusted_rate = if_else(
        tariff == 'residual',
        if_else(
          ieepa_usmca_exempt == 1 & partner %in% c('canada', 'mexico'),
          ieepa_rate * (1 - adjusted_usmca_share),
          ieepa_rate
        ),
        adjusted_rate
      )
    ) %>%

    group_by(partner, gtap_code) %>%
    summarise(
      etr = sum(share * adjusted_rate),
      .groups = 'drop'
    )
}


#' Write shock commands to txt file for downstream model
#'
#' @param etr_data Data frame with columns: partner, gtap_code, etr
#' @param output_file Path to output file
#' @param scenario Scenario name for output directory
#'
#' @return Writes file and returns invisibly
write_shock_commands <- function(etr_data, output_file = 'shocks.txt', scenario) {

  # Create output directory if it doesn't exist
  output_dir <- sprintf('output/%s', scenario)
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # Build full output path
  output_path <- file.path(output_dir, output_file)

  # Map partner names to proper format
  partner_map <- c(
    'china'  = 'China',
    'canada' = 'Canada',
    'mexico' = 'Mexico',
    'row'    = 'ROW',
    'ftrow'  = 'FTROW',
    'japan'  = 'Japan',
    'eu'     = 'EU',
    'uk'     = 'UK'
  )

  # Define partner order for output
  partner_order <- c('China', 'ROW', 'FTROW', 'Canada', 'Mexico', 'Japan', 'EU', 'UK')

  # Prepare data
  shock_commands <- etr_data %>%
    mutate(
      partner_fmt = partner_map[partner],
      etr_pct = round(etr * 100, 1)
    ) %>%
    filter(etr_pct > 0) %>%
    arrange(match(partner_fmt, partner_order), gtap_code) %>%
    mutate(
      command = sprintf('Shock tms("%s","%s","USA") = %.1f;', gtap_code, partner_fmt, etr_pct)
    )

  # Write to file with blank lines between partners
  con <- file(output_path, 'w')

  for (p in partner_order) {
    partner_commands <- shock_commands %>% filter(partner_fmt == p)

    if (nrow(partner_commands) > 0) {
      writeLines(partner_commands$command, con)
      writeLines('', con)  # Blank line after each partner
    }
  }

  close(con)

  message(sprintf('Wrote %d shock commands to %s', nrow(shock_commands), output_path))
  invisible(shock_commands)
}


#' Write ETRs to CSV in sector (rows) x country (columns) format
#'
#' Values are written in percentage points (pp), i.e., multiplied by 100.
#'
#' @param etr_data Data frame with columns: partner, gtap_code, etr
#' @param output_file Path to output file (default: 'etrs_by_sector_country.csv')
#' @param scenario Scenario name for output directory
#'
#' @return Writes CSV file (in pp units) and returns invisibly
write_sector_country_etrs <- function(etr_data,
                                       output_file = 'etrs_by_sector_country.csv',
                                       scenario = NULL) {

  # Create output directory if needed
  if (!is.null(scenario)) {
    output_dir <- sprintf('output/%s', scenario)
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    output_path <- file.path(output_dir, output_file)
  } else {
    output_path <- output_file
  }

  # Define sector order
  sector_order <- c(
    'pdr', 'wht', 'gro', 'v_f', 'osd', 'c_b', 'pfb', 'ocr', 'ctl', 'oap',
    'rmk', 'wol', 'frs', 'fsh', 'coa', 'oil', 'gas', 'oxt', 'cmt', 'omt',
    'vol', 'mil', 'pcr', 'sgr', 'ofd', 'b_t', 'tex', 'wap', 'lea', 'lum',
    'ppp', 'p_c', 'chm', 'bph', 'rpp', 'nmm', 'i_s', 'nfm', 'fmp', 'ele',
    'eeq', 'ome', 'mvh', 'otn', 'omf', 'ely', 'gdt', 'wtr', 'cns'
  )

  # Define country order
  country_order <- c('china', 'canada', 'mexico', 'uk', 'japan', 'eu', 'row', 'ftrow')

  # Pivot ETRs to wide format and convert to percentage points
  etrs_wide <- etr_data %>%
    select(partner, gtap_code, etr) %>%
    pivot_wider(names_from = partner, values_from = etr, values_fill = 0) %>%
    select(gtap_code, any_of(country_order)) %>%
    mutate(across(-gtap_code, ~ .x * 100))

  # Apply sector ordering
  existing_sectors <- intersect(sector_order, etrs_wide$gtap_code)

  etrs_wide <- etrs_wide %>%
    filter(gtap_code %in% existing_sectors) %>%
    arrange(match(gtap_code, sector_order))

  # Write CSV file
  write_csv(etrs_wide, output_path)

  message(sprintf('Wrote ETRs by sector and country to %s (in pp units)', output_path))

  invisible(etrs_wide)
}


#' Calculate and print overall ETR changes by country and total using both GTAP and 2024 Census weights
#'
#' Calculates and prints change in effective tariff rates from early 2025 baseline.
#'
#' @param etr_data Data frame with columns: partner, gtap_code, etr (change from baseline)
#' @param import_data Data frame with columns: partner, gtap_code, imports (2024 import values)
#' @param weights_file Path to GTAP import weights CSV file
#' @param output_file Path to output text file (default: 'overall_etrs.txt')
#' @param scenario Scenario name for output directory
#'
#' @return Prints overall ETR changes and returns them invisibly
calc_overall_etrs <- function(etr_data, import_data = NULL,
                              weights_file = 'resources/gtap_import_weights.csv',
                              output_file = 'overall_etrs.txt',
                              scenario = NULL) {

  # ===========================
  # Calculate GTAP-weighted ETRs
  # ===========================

  # Read GTAP import weights
  gtap_weights <- read_csv(weights_file, show_col_types = FALSE)

  # Reshape weights to long format
  gtap_weights_long <- gtap_weights %>%
    pivot_longer(cols = -gtap_code, names_to = 'partner', values_to = 'import_weight') %>%
    filter(import_weight > 0)

  # Join ETRs with GTAP weights
  gtap_weighted_data <- etr_data %>%
    inner_join(gtap_weights_long, by = c('partner', 'gtap_code')) %>%
    mutate(weighted_etr = etr * import_weight)

  # Calculate overall ETR by country using GTAP weights
  gtap_country_etrs <- gtap_weighted_data %>%
    group_by(partner) %>%
    summarise(
      gtap_etr = sum(weighted_etr) / sum(import_weight),
      .groups = 'drop'
    )

  # Calculate total overall ETR using GTAP weights
  gtap_total_etr <- gtap_weighted_data %>%
    summarise(gtap_etr = sum(weighted_etr) / sum(import_weight))

  gtap_total_etr_value <- gtap_total_etr$gtap_etr

  # ===========================
  # Calculate 2024 Census-weighted ETRs
  # ===========================

  if (!is.null(import_data)) {
    # Calculate 2024 Census weights from import data
    census_weights <- import_data %>%
      group_by(partner, gtap_code) %>%
      summarise(import_weight = sum(imports), .groups = 'drop') %>%
      filter(import_weight > 0)

    # Join ETRs with Census weights
    census_weighted_data <- etr_data %>%
      inner_join(census_weights, by = c('partner', 'gtap_code')) %>%
      mutate(weighted_etr = etr * import_weight)

    # Calculate overall ETR by country using Census weights
    census_country_etrs <- census_weighted_data %>%
      group_by(partner) %>%
      summarise(
        census_etr = sum(weighted_etr) / sum(import_weight),
        .groups = 'drop'
      )

    # Calculate total overall ETR using Census weights
    census_total_etr <- census_weighted_data %>%
      summarise(census_etr = sum(weighted_etr) / sum(import_weight))

    census_total_etr_value <- census_total_etr$census_etr

    # Combine both sets of results
    country_etrs <- gtap_country_etrs %>%
      left_join(census_country_etrs, by = 'partner') %>%
      arrange(desc(gtap_etr))

  } else {
    # Only GTAP weights available
    country_etrs <- gtap_country_etrs %>%
      mutate(census_etr = NA) %>%
      arrange(desc(gtap_etr))
    census_total_etr_value <- NA
  }

  # ===========================
  # Print results
  # ===========================

  cat('\n')
  cat('Overall ETRs by Country (change from early 2025 baseline):\n')
  cat('==========================================================\n')
  if (!is.null(import_data)) {
    cat(sprintf('%-10s  %15s  %18s\n', '', 'GTAP Weights', '2024 Census Weights'))
    cat(sprintf('%-10s  %15s  %18s\n', 'Country', '', ''))
    cat(sprintf('%-10s  %15s  %18s\n', '-------', '------------', '-----------------'))
  } else {
    cat(sprintf('%-10s  %15s\n', '', 'GTAP Weights'))
    cat(sprintf('%-10s  %15s\n', 'Country', ''))
    cat(sprintf('%-10s  %15s\n', '-------', '------------'))
  }

  for (i in 1:nrow(country_etrs)) {
    if (!is.null(import_data)) {
      cat(sprintf('%-10s  %14.2f%%  %17.2f%%\n',
                  toupper(country_etrs$partner[i]),
                  country_etrs$gtap_etr[i] * 100,
                  country_etrs$census_etr[i] * 100))
    } else {
      cat(sprintf('%-10s  %14.2f%%\n',
                  toupper(country_etrs$partner[i]),
                  country_etrs$gtap_etr[i] * 100))
    }
  }
  cat('\n')
  if (!is.null(import_data)) {
    cat(sprintf('%-10s  %14.2f%%  %17.2f%%\n',
                'TOTAL',
                gtap_total_etr_value * 100,
                census_total_etr_value * 100))
  } else {
    cat(sprintf('%-10s  %14.2f%%\n',
                'TOTAL',
                gtap_total_etr_value * 100))
  }
  cat('\n')

  # ===========================
  # Write results to file
  # ===========================

  if (!is.null(scenario)) {
    output_dir <- sprintf('output/%s', scenario)
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    output_path <- file.path(output_dir, output_file)
  } else {
    output_path <- output_file
  }

  con <- file(output_path, 'w')

  writeLines('Overall ETRs by Country (change from early 2025 baseline):', con)
  writeLines('==========================================================', con)
  writeLines('', con)

  if (!is.null(import_data)) {
    writeLines(sprintf('%-10s  %15s  %18s', '', 'GTAP Weights', '2024 Census Weights'), con)
    writeLines(sprintf('%-10s  %15s  %18s', 'Country', '', ''), con)
    writeLines(sprintf('%-10s  %15s  %18s', '-------', '------------', '-----------------'), con)
  } else {
    writeLines(sprintf('%-10s  %15s', '', 'GTAP Weights'), con)
    writeLines(sprintf('%-10s  %15s', 'Country', ''), con)
    writeLines(sprintf('%-10s  %15s', '-------', '------------'), con)
  }

  for (i in 1:nrow(country_etrs)) {
    if (!is.null(import_data)) {
      writeLines(sprintf('%-10s  %14.2f%%  %17.2f%%',
                         toupper(country_etrs$partner[i]),
                         country_etrs$gtap_etr[i] * 100,
                         country_etrs$census_etr[i] * 100), con)
    } else {
      writeLines(sprintf('%-10s  %14.2f%%',
                         toupper(country_etrs$partner[i]),
                         country_etrs$gtap_etr[i] * 100), con)
    }
  }

  writeLines('', con)
  if (!is.null(import_data)) {
    writeLines(sprintf('%-10s  %14.2f%%  %17.2f%%',
                       'TOTAL',
                       gtap_total_etr_value * 100,
                       census_total_etr_value * 100), con)
  } else {
    writeLines(sprintf('%-10s  %14.2f%%',
                       'TOTAL',
                       gtap_total_etr_value * 100), con)
  }

  close(con)

  message(sprintf('Wrote overall ETRs to %s', output_path))

  # Return results invisibly
  invisible(list(
    by_country = country_etrs,
    gtap_total = gtap_total_etr_value,
    census_total = if (!is.null(import_data)) census_total_etr_value else NA
  ))
}
