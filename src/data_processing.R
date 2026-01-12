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
#' @param months Optional integer vector of months to load (1-12). Default: all months found.
#' @param include_weight Logical; whether to attempt to extract net weight (kg). Default FALSE.
#' @param weight_method Weight proxy to extract when include_weight = TRUE:
#'   'shipping' uses air + vessel shipping weights from the Census layout (positions 239-253 and 284-298),
#'   summed to `net_weight_kg`; 'net' reads a single net weight field using net_weight_positions.
#' @param net_weight_positions Optional length-2 integer vector c(start, end) for a net weight
#'   field in IMP_DETL.TXT (1-based, inclusive). Required only if include_weight = TRUE
#'   and weight_method = 'net'.
#' @param include_raw_values Logical; whether to include con_val_mo and gen_val_mo columns
#'   in the returned data. Default FALSE.
#'
#' @return Tibble with columns: year, month, hs10, cty_code, value, and optionally
#'   net_weight_kg and/or raw value columns.
load_imports_hs10_country <- function(import_data_path,
                                      year,
                                      type = c('con', 'gen'),
                                      months = NULL,
                                      include_weight = FALSE,
                                      weight_method = c('shipping', 'net'),
                                      net_weight_positions = NULL,
                                      include_raw_values = FALSE) {

  type <- match.arg(type)
  weight_method <- match.arg(weight_method)

  if (!is.null(months)) {
    months <- as.integer(months)
    if (any(is.na(months)) || any(months < 1 | months > 12)) {
      stop('months must be an integer vector in 1..12')
    }
  }

  if (isTRUE(include_weight) && weight_method == 'net' && is.null(net_weight_positions)) {
    stop('net_weight_positions must be provided when include_weight = TRUE and weight_method = \"net\"')
  }

  # Extract last two digits of year for file pattern matching
  yy <- substr(as.character(year), 3, 4)

  # Find sources for the specified year:
  # - ZIPs: IMDBYYMM.ZIP
  # - Directories: IMDBYYMM (already unzipped)
  zip_pattern <- sprintf('^IMDB%s\\d{2}\\.ZIP$', yy)
  dir_pattern <- sprintf('^IMDB%s\\d{2}$', yy)

  zip_files <- list.files(
    path = import_data_path,
    pattern = zip_pattern,
    full.names = TRUE,
    ignore.case = TRUE
  )

  dir_paths <- list.files(
    path = import_data_path,
    pattern = dir_pattern,
    full.names = TRUE,
    ignore.case = TRUE
  )
  dir_paths <- dir_paths[file.info(dir_paths)$isdir %in% TRUE]

  sources <- tibble(path = c(dir_paths, zip_files)) %>%
    mutate(
      name = basename(path),
      month = suppressWarnings(as.integer(substr(name, 7, 8))),
      kind = if_else(file.info(path)$isdir %in% TRUE, 'dir', 'zip')
    ) %>%
    filter(!is.na(month))

  if (!is.null(months)) {
    sources <- sources %>% filter(month %in% months)
  }

  if (nrow(sources) == 0) {
    stop(sprintf('No import sources found at %s for year %d', import_data_path, year))
  }

  # Prefer directories when both directory and ZIP exist for the same month
  sources <- sources %>%
    group_by(month) %>%
    arrange(desc(kind == 'dir')) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    arrange(month)

  message(sprintf('Found %d source(s) for year %d', nrow(sources), year))

  # Column positions for IMP_DETL.TXT (1-based, inclusive)
  col_start <- c(1,  11,  23,  27,  74,   179)
  col_end <- c(10, 14,  26,  28,  88,   193)
  col_names <- c('hs10', 'cty_code', 'year', 'month', 'con_val_mo', 'gen_val_mo')

  if (isTRUE(include_weight)) {
    if (weight_method == 'shipping') {
      col_start <- c(col_start, 239, 284)
      col_end <- c(col_end, 253, 298)
      col_names <- c(col_names, 'air_wgt_mo', 'ves_wgt_mo')
    } else {
      col_start <- c(col_start, net_weight_positions[1])
      col_end <- c(col_end, net_weight_positions[2])
      col_names <- c(col_names, 'net_weight_kg')
    }
  }

  col_positions <- fwf_positions(
    start     = col_start,
    end       = col_end,
    col_names = col_names
  )

  # Process each source (directory or ZIP)
  all_records <- map_df(sources$path, function(source_path) {

    message(sprintf('Processing: %s', basename(source_path)))

    extracted_path <- NULL
    cleanup <- NULL

    if (file.info(source_path)$isdir %in% TRUE) {
      detl_paths <- list.files(
        path = source_path,
        pattern = '^IMP_DETL\\.TXT$',
        full.names = TRUE,
        ignore.case = TRUE
      )
      if (length(detl_paths) == 0) {
        warning(sprintf('No IMP_DETL.TXT found in %s, skipping', basename(source_path)))
        return(tibble())
      }
      extracted_path <- detl_paths[1]
    } else {
      zip_contents <- unzip(source_path, list = TRUE)
      detl_file <- zip_contents$Name[grepl('IMP_DETL\\.TXT$', zip_contents$Name, ignore.case = TRUE)]

      if (length(detl_file) == 0) {
        warning(sprintf('No IMP_DETL.TXT found in %s, skipping', basename(source_path)))
        return(tibble())
      }

      if (length(detl_file) > 1) {
        warning(sprintf('Multiple IMP_DETL.TXT files found in %s, using first', basename(source_path)))
        detl_file <- detl_file[1]
      }

      temp_dir <- tempdir()
      extracted_path <- unzip(source_path, files = detl_file, exdir = temp_dir, overwrite = TRUE)
      cleanup <- extracted_path
    }

    # Read fixed-width file
    records <- read_fwf(
      file = extracted_path,
      col_positions = col_positions,
      col_types = if (isTRUE(include_weight) && weight_method == 'shipping') {
        cols(
          hs10       = col_character(),
          cty_code   = col_character(),
          year       = col_integer(),
          month      = col_integer(),
          con_val_mo = col_double(),
          gen_val_mo = col_double(),
          air_wgt_mo = col_double(),
          ves_wgt_mo = col_double()
        )
      } else if (isTRUE(include_weight) && weight_method == 'net') {
        cols(
          hs10          = col_character(),
          cty_code      = col_character(),
          year          = col_integer(),
          month         = col_integer(),
          con_val_mo    = col_double(),
          gen_val_mo    = col_double(),
          net_weight_kg = col_double()
        )
      } else {
        cols(
          hs10       = col_character(),
          cty_code   = col_character(),
          year       = col_integer(),
          month      = col_integer(),
          con_val_mo = col_double(),
          gen_val_mo = col_double()
        )
      },
      progress = FALSE
    )

    if (!is.null(cleanup)) {
      file.remove(cleanup)
    }

    # Filter to specified year and select appropriate value column
    records %>%
      filter(year == !!year) %>%
      {if (!is.null(months)) filter(., month %in% months) else .} %>%
      mutate(
        hs10 = str_pad(hs10, width = 10, side = 'left', pad = '0')
      ) %>%
      {
        if (isTRUE(include_weight) && weight_method == 'shipping') {
          mutate(., net_weight_kg = air_wgt_mo + ves_wgt_mo)
        } else {
          .
        }
      } %>%
      {if (type == 'con') mutate(., value = con_val_mo) else mutate(., value = gen_val_mo)} %>%
      {
        out_cols <- c('year', 'month', 'hs10', 'cty_code', 'value')
        if (isTRUE(include_weight)) out_cols <- c(out_cols, 'net_weight_kg')
        if (isTRUE(include_raw_values)) out_cols <- c(out_cols, 'con_val_mo', 'gen_val_mo')
        select(., all_of(out_cols))
      }
  })

  # Aggregate over districts (sum duplicates by year, month, hs10, cty_code)
  sum_cols <- c('value')
  if (isTRUE(include_weight)) sum_cols <- c(sum_cols, 'net_weight_kg')
  if (isTRUE(include_raw_values)) sum_cols <- c(sum_cols, 'con_val_mo', 'gen_val_mo')

  result <- all_records %>%
    group_by(year, month, hs10, cty_code) %>%
    summarise(across(all_of(sum_cols), sum), .groups = 'drop')

  message(sprintf('Loaded %s records for year %d', format(nrow(result), big.mark = ','), year))

  return(result)
}
