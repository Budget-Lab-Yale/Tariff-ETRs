library(tidyverse)

#----------------
# Set parameters
#----------------

china_codes  <- c("5700")   # China
canada_codes <- c("1220")   # Canada
mexico_codes <- c("2010")   # Mexico
uk_codes     <- c("4120")   # United Kingdom (GB)
japan_codes  <- c("5880")   # Japan

# EU-27 cty_code values from your crosswalk (excludes UK)
eu_codes <- c(
  "4330", # Austria (AT)
  "4231", # Belgium (BE)
  "4870", # Bulgaria (BG)
  "4791", # Croatia (HR)
  "4910", # Cyprus (CY)
  "4351", # Czech Republic (CZ)
  "4099", # Denmark (DK) - "except Greenland"
  "4470", # Estonia (EE)
  "4050", # Finland (FI)
  "4279", # France (FR)
  "4280", # Germany (DE)
  "4840", # Greece (GR)
  "4370", # Hungary (HU)
  "4190", # Ireland (IE)
  "4759", # Italy (IT)
  "4490", # Latvia (LV)
  "4510", # Lithuania (LT)
  "4239", # Luxembourg (LU)
  "4730", # Malta (MT)
  "4210", # Netherlands (NL)
  "4550", # Poland (PL)
  "4710", # Portugal (PT)
  "4850", # Romania (RO)
  "4359", # Slovakia (SK)
  "4792", # Slovenia (SI)
  "4700", # Spain (ES)
  "4010"  # Sweden (SE)
)


# Get list of all 2024 files
file_pattern <- "dporths6ir24"
files_2024   <- list.files(path = "C:/Users/jar335/Downloads", pattern = file_pattern, full.names = TRUE)


# Define column positions based on the file specification
col_positions <- fwf_positions(
  start     = c(1, 7, 11, 15, 19, 21),
  end       = c(6, 10, 14, 18, 20, 35),
  col_names = c("commodity", "cty_code", "port_code", "year", "month", "value_mo")
)


hs6_by_country <- files_2024 %>%
  
  # Read and combine all files
  map_df(~ read_fwf(
    file = .x,
    col_positions = col_positions,
    col_types = cols(
      commodity = col_character(),
      cty_code = col_character(),
      port_code = col_character(),
      year = col_integer(),
      month = col_integer(),
      value_mo = col_double()
    )
  )) %>% 
  
  # Tag each row with a partner group
  mutate(
    partner = case_when(
      cty_code %in% china_codes  ~ "China",
      cty_code %in% canada_codes ~ "Canada",
      cty_code %in% mexico_codes ~ "Mexico",
      cty_code %in% uk_codes     ~ "United Kingdom",
      cty_code %in% japan_codes  ~ "Japan",
      cty_code %in% eu_codes     ~ "European Union (EU27)",
      TRUE                       ~ "ROW"
    )
  ) %>% 
  
  # Get totals
  group_by(commodity, partner) %>%
  summarise(total_value_2024 = sum(value_mo), .groups = "drop")

