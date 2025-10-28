library(tidyverse)

#----------------
# Load crosswalk
#----------------

crosswalk <- read_csv('resources/hs6_gtap_crosswalk.csv', show_col_types = FALSE)

#----------------
# Set parameters
#----------------

china_codes  <- c('5700')   # China
canada_codes <- c('1220')   # Canada
mexico_codes <- c('2010')   # Mexico
uk_codes     <- c('4120')   # United Kingdom (GB)
japan_codes  <- c('5880')   # Japan

# EU-27 cty_code values from your crosswalk (excludes UK)
eu_codes <- c(
  '4330', # Austria (AT)
  '4231', # Belgium (BE)
  '4870', # Bulgaria (BG)
  '4791', # Croatia (HR)
  '4910', # Cyprus (CY)
  '4351', # Czech Republic (CZ)
  '4099', # Denmark (DK) - 'except Greenland'
  '4470', # Estonia (EE)
  '4050', # Finland (FI)
  '4279', # France (FR)
  '4280', # Germany (DE)
  '4840', # Greece (GR)
  '4370', # Hungary (HU)
  '4190', # Ireland (IE)
  '4759', # Italy (IT)
  '4490', # Latvia (LV)
  '4510', # Lithuania (LT)
  '4239', # Luxembourg (LU)
  '4730', # Malta (MT)
  '4210', # Netherlands (NL)
  '4550', # Poland (PL)
  '4710', # Portugal (PT)
  '4850', # Romania (RO)
  '4359', # Slovakia (SK)
  '4792', # Slovenia (SI)
  '4700', # Spain (ES)
  '4010'  # Sweden (SE)
)


# Get list of all 2024 files
file_pattern <- 'dporths6ir24'
files_2024   <- list.files(path = 'C:/Users/jar335/Downloads', pattern = file_pattern, full.names = TRUE)


# Define column positions based on the file specification
col_positions <- fwf_positions(
  start     = c(1, 7, 11, 15, 19, 21),
  end       = c(6, 10, 14, 18, 20, 35),
  col_names = c('hs6_code', 'cty_code', 'port_code', 'year', 'month', 'value_mo')
)


# Build data
hs6_by_country <- files_2024 %>%
  
  # Read and combine all files
  map_df(~ read_fwf(
    file = .x,
    col_positions = col_positions,
    col_types = cols(
      hs6_code  = col_character(),
      cty_code  = col_character(),
      port_code = col_character(),
      year      = col_integer(),
      month     = col_integer(),
      value_mo  = col_double()
    )
  )) %>% 
  
  # Tag each row with a partner group
  mutate(
    partner = case_when(
      cty_code %in% china_codes  ~ 'china',
      cty_code %in% canada_codes ~ 'canada',
      cty_code %in% mexico_codes ~ 'mexico',
      cty_code %in% uk_codes     ~ 'uk',
      cty_code %in% japan_codes  ~ 'japan',
      cty_code %in% eu_codes     ~ 'eu',
      TRUE                       ~ 'row'
    )
  ) %>% 
  
  # Get totals
  group_by(hs6_code, partner) %>%
  summarise(imports = sum(value_mo), .groups = 'drop') %>% 
  
  # Add GTAP code
  left_join(
    crosswalk %>% 
      select(hs6_code, gtap_code), 
    by = 'hs6_code'
  ) %>% 
  mutate(gtap_code = str_to_lower(gtap_code)) %>%
  relocate(gtap_code, .after = hs6_code)


#----------------
# Functions
#----------------

#' Calculate import shares for a subset of HS6 codes
#'
#' Given a vector of HS6 codes, this function calculates:
#' 1. Total imports for those specific codes by partner and GTAP sector
#' 2. Total imports for ALL codes by partner and GTAP sector
#' 3. The share (subset / total) by partner and GTAP sector
#'
#' @param hs6_codes Character vector of HS6 codes to analyze
#' @param data Data frame with columns: hs6_code, partner, gtap_code, imports
#'
#' @return Data frame with columns: partner, gtap_code, subset_imports, total_imports, share
calc_import_shares <- function(hs6_codes, data = hs6_by_country) {

  # Calculate imports for the specified HS6 codes by partner and GTAP
  subset_imports <- data %>%
    filter(hs6_code %in% hs6_codes) %>%
    group_by(partner, gtap_code) %>%
    summarise(subset_imports = sum(imports), .groups = 'drop')

  # Calculate total imports by partner and GTAP
  total_imports <- data %>%
    group_by(partner, gtap_code) %>%
    summarise(total_imports = sum(imports), .groups = 'drop')

  # Join and calculate shares
  result <- total_imports %>%
    left_join(subset_imports, by = c('partner', 'gtap_code')) %>%
    mutate(
      subset_imports = replace_na(subset_imports, 0),
      share          = subset_imports / total_imports
    )

  return(result)
}




