# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an R-based data analysis project for processing U.S. import trade data. The project analyzes imports by HS10 commodity codes and trading partners using fixed-width format (FWF) data files from the Census Bureau.

## Data Architecture

**Input Data:**
- Census Bureau monthly ZIP files (IMDByymm.ZIP) containing IMP_DETL.TXT fixed-width format files
- Files are read from `C:/Users/jar335/Downloads`
- HS10-level import data (10-digit Harmonized System codes)
- Columns extracted from IMP_DETL.TXT:
  - `hs10` (positions 1-10): 10-digit HS code
  - `cty_code` (positions 11-14): Census country code
  - `year` (positions 23-26): Year
  - `month` (positions 27-28): Month
  - `con_val_mo` (positions 74-88): Consumption imports value
  - `gen_val_mo` (positions 179-193): General imports value

**Country Code Mappings:**
The project uses Census Bureau country codes (not ISO codes). Country-to-partner mapping used for final aggregation:
- China: 5700 → `china`
- Canada: 1220 → `canada`
- Mexico: 2010 → `mexico`
- UK: 4120 → `uk`
- Japan: 5880 → `japan`
- EU-27: 27 individual country codes → `eu` (see country_partner_mapping.csv)
- Free Trade ROW: Australia, Korea, Singapore, Chile, etc. → `ftrow`
- All others → `row` (Rest of World - 183 unmapped countries)

**IMPORTANT**: Tariff rates are specified at the COUNTRY level in config files, calculations happen at country level throughout, and only aggregate to partners at the final output stage for GTAP compatibility.

**GTAP Mapping:**
- HS10 codes are mapped to GTAP sectors using resources/hs10_gtap_crosswalk.csv
- This allows aggregation to GTAP sector level for economic modeling

**Caching:**
- Processed HS10×country×GTAP data is cached in cache/hs10_by_country_gtap_2024_con.rds
- Cache is used by default to avoid re-processing large import files
- Set `use_cache = FALSE` in main.R to force re-processing

**Output:**
- GTAP shock commands (output/{scenario}/shocks.txt)
- Sector×country ETR matrix (output/{scenario}/etrs_by_sector_country.csv)
- Overall ETRs by country (output/{scenario}/overall_etrs.txt)

## Development Commands

**Run the analysis:**
```r
source("src/main.R")
```

**Interactive development in RStudio:**
- Open `Tariff-ETRs.Rproj` to load the project with correct settings
- The project uses 2 spaces for indentation (configured in .Rproj file)

## Configuration Files

Each scenario in `config/{scenario}/` requires:

1. **232.yaml** - Section 232 tariffs with country-level rates and defaults:
   ```yaml
   tariff_name:
     base: [list of variable-length HTS codes: 4, 6, 8, or 10 digits]
     rates:
       default: 0.5          # Default rate for all countries
       '4120': 0.25          # UK-specific override (Census code)
       '5700': 0.3           # China-specific override
     usmca_exempt: 0         # 1 = apply USMCA exemptions, 0 = no exemption
   ```

2. **ieepa_rates.yaml** - IEEPA rates with country-level hierarchical structure:
   ```yaml
   headline_rates:
     default: 0.1            # Default rate for unmapped countries
     '5700': 0.2             # China (Census code 5700)
     '1220': 0.35            # Canada
     # ... other country codes

   product_rates:            # Optional: HTS-specific overrides
     '8703':                 # Can be simple rate (applies to all countries)
       default: 0.15         # Or dict with country-specific rates
       '5700': 0.25

   product_country_rates:    # Optional: Most specific overrides
     - hts_codes: ['870322', '870323']
       cty_code: '5700'
       rate: 0.30
   ```

3. **other_params.yaml** - USMCA parameters, auto rebate rates, etc.

## Key Implementation Notes

**Architecture: Multi-Authority Rate Matrix**

The codebase uses a clean separation between config parsing and calculations:

*Config Parsing → Tabular Data:*
- `load_232_rates()`: Returns complete HS10×country tibble with one column per tariff (`s232_[tariff]_rate`)
- `load_ieepa_rates_yaml()`: Returns complete HS10×country tibble with single column (`ieepa_rate`)
- Both functions handle the full universe of HS10 codes × 240 countries
- No nested lists - just clean tibbles ready for joining

*Calculations → Stacking Rules:*
- `calc_weighted_etr()`: Joins config tibbles with import data, applies USMCA/rebates, then applies stacking rules
- Stacking rules are centralized and easy to modify (calculations.R:345-360)
- Current rule: `final_rate = max(all 232 rates) OR ieepa_rate` (mutually exclusive)
- Future: Can easily add stacking (e.g., `final_rate = max(all 232 rates) + ieepa_fent_rate`)

**Core Functions:**

*src/config_parsing.R:*
- `load_232_rates()`: Loads 232 YAML, expands to complete HS10×country tibble with coverage and country-specific rates
- `load_ieepa_rates_yaml()`: Loads IEEPA YAML, applies hierarchical rate logic (headline → product → product×country), returns complete tibble

*src/data_processing.R:*
- `load_imports_hs10_country()`: Reads Census ZIP files, extracts IMP_DETL.TXT, returns HS10×country×month data

*src/calculations.R:*
- `calc_import_shares()`: Variable-length HTS code matching using regex prefix matching (still used internally by config parsing)
- `calc_weighted_etr()`: Joins tabular config data, applies USMCA exemptions/auto rebates, applies stacking rules, calculates final ETR
- `aggregate_countries_to_partners()`: Aggregates country-level ETRs to 8 partner groups using import-weighted averaging
- `do_scenario()`: Main orchestrator - loads tabular config data, calculates country-level ETRs, aggregates to partners, writes outputs

**Variable-Length HTS Matching:**
- Both 232 and IEEPA tariffs use prefix matching: '8703' matches all HS10 codes starting with 8703
- Supports 4-digit (chapter), 6-digit (subheading), 8-digit, and 10-digit (full HTS) codes
- Implemented via regex: `^(code1|code2|...)` pattern

**Country-Level Architecture:**
- Tariff rates are specified at country level (Census codes) in YAML configs with `default` rates
- ALL calculations (import shares, coverage, ETRs) happen at HS10×country level
- Country codes are mapped to 8 partner groups using resources/country_partner_mapping.csv (51 countries mapped)
- Unmapped countries (183 total) use default rates from config files and aggregate to 'row' partner
- Only the final aggregation step converts country-level ETRs → partner-level ETRs for GTAP output
- This ensures accurate country-specific tariff modeling while maintaining GTAP compatibility

**File Paths:**
- Import data path is hardcoded to `C:/Users/jar335/Downloads` in main.R
- Update this for different environments

## Style
- never use na.rm = T or any other kind of na filters (including preemptive replace_na() calls). if we have missings that's a sign that something is wrong and it should break accordingly!
- use single quotes for strings
- **prefer tabular data structures over loops**: when processing config data or doing complex transformations, convert list structures to tibbles early and use joins/vectorized operations instead of loops. This is more readable, more performant, and more idiomatic R. Example: instead of looping through a list to match values, convert the list to a tibble and use `left_join()`.
- please review the readme file each time you make a commit and determine whether you should update that file accordingly. 
