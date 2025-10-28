library(tidyverse)
library(yaml)



#---------------------------
# Set tariff law parameters
#---------------------------

# Section 232 tariffs
params_232 <- read_yaml('config/232.yaml')

# IEEPA rates
params_ieepa = list()

# Other params
us_auto_assembly_share = 0.4

# USMCA share of trade by sector
usmca_shares = read_csv('./resources/usmca_shares.csv')


#----------------------
# Set other parameters
#----------------------

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


#------------------
# Read import data
#------------------

# Read GTAP crosswalk
crosswalk <- read_csv('resources/hs6_gtap_crosswalk.csv', show_col_types = FALSE)

# Get list of all 2024 import files
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
    mutate(share = replace_na(subset_imports / total_imports, 0)) %>%
    select(-subset_imports, -total_imports)

  return(result)
}


#' Calculate weighted ETR by partner and GTAP sector
#'
#' @param bases Data frame with tariff bases (default: bases_232)
#' @param params Tariff parameters list (default: params_232)
#' @param usmca_shares Data frame with USMCA shares by partner and GTAP sector
#' @param us_auto_assembly_share Share of US content in auto assembly
#'
#' @return Data frame with columns: partner, gtap_code, etr, etr_upper
calc_weighted_etr <- function(bases = bases_232, params = params_232,
                              usmca_shares = usmca_shares,
                              us_auto_assembly_share = us_auto_assembly_share) {

  # Extract rates and usmca_exempt flags from params
  rates_and_exemptions <- map(names(params), ~ {
    tibble(
      tariff       = .x,
      partner      = names(params[[.x]]$rate),
      rate         = unlist(params[[.x]]$rate),
      usmca_exempt = params[[.x]]$usmca_exempt
    )
  }) %>%
    bind_rows()

  # Reshape USMCA shares long by country
  usmca_long <- usmca_shares %>%
    pivot_longer(cols = -gtap_code, names_to = 'partner', values_to = 'usmca_share') %>%
    mutate(usmca_share = replace_na(usmca_share, 0))

  # Join rates, exemptions, and USMCA shares to bases
  bases %>%
    left_join(rates_and_exemptions, by = c('tariff', 'partner')) %>%
    left_join(usmca_long, by = c('partner', 'gtap_code')) %>%
    mutate(
      rate = replace_na(rate, 0),
      usmca_exempt = replace_na(usmca_exempt, 0),
      usmca_share = replace_na(usmca_share, 0)
    ) %>%
    
    # Apply USMCA exemption logic
    mutate(
      adjusted_usmca_share = if_else(
        tariff == 'automobiles_passenger_and_light_trucks',
        usmca_share * us_auto_assembly_share,
        usmca_share
      ),
      adjusted_rate = if_else(
        usmca_exempt == 1 & partner %in% c('canada', 'mexico'),
        rate * (1 - adjusted_usmca_share),
        rate
      )
    ) %>%
    group_by(partner, gtap_code) %>%
    summarise(
      etr       = sum(share * adjusted_rate),
      etr_upper = sum(share_upper * adjusted_rate),
      .groups = 'drop'
    )
}


#----------------------------------
# Calculate tax bases for 232 tariffs
#----------------------------------

bases <- params_232 %>%
  names() %>%

  # Get bases for each 232 tariff
  map(~ {

    full_codes <- params_232[[.x]]$base$full
    all_codes  <- c(params_232[[.x]]$base$full, params_232[[.x]]$base$partial)

    share       <- calc_import_shares(full_codes)
    share_upper <- calc_import_shares(all_codes) %>% rename(share_upper = share) 

    share %>%
      left_join(share_upper, by = c('partner', 'gtap_code')) %>%
      mutate(tariff = .x, .before = 1) %>%
      return()
  }) %>%
  bind_rows() %>%

  # Add residual -- tax base uncovered by 232
  bind_rows(
    (.) %>%
      group_by(partner, gtap_code) %>%
      summarise(
        tariff      = 'residual',
        share       = 1 - sum(share),
        share_upper = 1 - sum(share_upper),
        .groups = 'drop'
      )
  )

