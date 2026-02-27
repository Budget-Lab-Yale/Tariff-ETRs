# =============================================================================
# data_processing.R
# =============================================================================
#
# Functions for loading and processing raw import data.
#
# Functions:
#   - load_census_codes():          Load Census country codes with names
#   - load_imports_hs10_country():  Load HS10 x country x year x month import data
#   - load_mfn_rates():             Load MFN (most-favored-nation) baseline tariff rates
#   - load_mfn_exemption_shares():  Load MFN exemption shares (FTA/GSP discounts)
#
# =============================================================================


#' Load Census country codes with names
#'
#' @param file Path to census_codes.csv file
#'
#' @return Tibble with columns: cty_code, country_name
load_census_codes <- function(file = 'resources/census_codes.csv') {
  read_csv(file, col_types = cols(Code = col_character()), show_col_types = FALSE) %>%
    rename(cty_code = Code, country_name = Name)
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

  # Enforce complete annual coverage: exactly one ZIP for each month.
  zip_file_info <- tibble(
    zip_path = zip_files,
    file_name = basename(zip_files)
  ) %>%
    mutate(
      month = str_match(
        str_to_upper(file_name),
        sprintf('^IMDB%s(\\d{2})\\.ZIP$', yy)
      )[, 2],
      month = as.integer(month)
    )

  if (any(is.na(zip_file_info$month))) {
    bad_files <- zip_file_info %>%
      filter(is.na(month)) %>%
      pull(file_name)
    stop(sprintf('Could not parse month from import file name(s): %s',
                 paste(bad_files, collapse = ', ')))
  }

  invalid_months <- sort(setdiff(unique(zip_file_info$month), 1:12))
  if (length(invalid_months) > 0) {
    stop(sprintf('Found invalid month(s) in import file names: %s',
                 paste(sprintf('%02d', invalid_months), collapse = ', ')))
  }

  duplicate_months <- zip_file_info %>%
    count(month) %>%
    filter(n > 1)
  if (nrow(duplicate_months) > 0) {
    stop(sprintf('Multiple import ZIP files found for year %d month(s): %s',
                 year, paste(sprintf('%02d', duplicate_months$month), collapse = ', ')))
  }

  missing_months <- setdiff(1:12, sort(unique(zip_file_info$month)))
  if (length(missing_months) > 0) {
    stop(sprintf('Missing import ZIP files for year %d month(s): %s',
                 year, paste(sprintf('%02d', missing_months), collapse = ', ')))
  }

  # Process files in month order for deterministic behavior.
  zip_files <- zip_file_info %>%
    arrange(month) %>%
    pull(zip_path)

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
      stop(sprintf('No IMP_DETL.TXT found in %s', basename(zip_path)))
    }

    if (length(detl_file) > 1) {
      stop(sprintf('Multiple IMP_DETL.TXT files found in %s', basename(zip_path)))
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


#' Load MFN (most-favored-nation) baseline tariff rates at HS8 level
#'
#' Loads pre-built MFN rates from CSV. Specific-duty-only lines have mfn_rate = 0
#' (ad valorem equivalent not available). Coverage is ~93% of HS8 codes by count;
#' HS10 codes with no HS8 match will default to 0 at join time.
#'
#' @param file Path to MFN rates CSV file
#'
#' @return Tibble with columns: hs8 (character), mfn_rate (numeric)
load_mfn_rates <- function(file = 'resources/mfn_rates_2025.csv') {
  mfn <- read_csv(
    file,
    col_types = cols(hs8 = col_character(), mfn_rate = col_double()),
    show_col_types = FALSE
  ) %>%
    select(hs8, mfn_rate)

  message(sprintf('Loaded %s MFN rates from %s', format(nrow(mfn), big.mark = ','), file))
  mfn
}


#' Load MFN exemption shares at HS2 × country level
#'
#' MFN exemption shares capture the fraction of MFN tariff waived by FTAs, GSP,
#' and other duty-free provisions. Used to compute effective MFN rates:
#'   effective_mfn = mfn_rate * (1 - exemption_share)
#'
#' The CSV is sparse: only rows with exemption_share > 0 are included.
#' Missing HS2 × country combinations default to 0 (no exemption) at join time.
#'
#' @param file Path to MFN exemption shares CSV file
#'
#' @return Tibble with columns: hs2 (character), cty_code (character), exemption_share (numeric)
load_mfn_exemption_shares <- function(file) {
  shares <- read_csv(
    file,
    col_types = cols(hs2 = col_character(), cty_code = col_character(), exemption_share = col_double()),
    show_col_types = FALSE
  ) %>%
    select(hs2, cty_code, exemption_share)

  message(sprintf('Loaded %s MFN exemption shares from %s', format(nrow(shares), big.mark = ','), file))
  shares
}
