# =============================================================================
# data_processing.R
# =============================================================================
#
# Functions for loading and processing raw import data.
#
# Functions:
#   - load_census_codes():          Load Census country codes with names
#   - load_imports_hs10_country():  Load HS10 x country x year x month import data
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
