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
The project uses specific Census Bureau country codes (not ISO codes) mapped to 8 partner groups:
- China: 5700 → `china`
- Canada: 1220 → `canada`
- Mexico: 2010 → `mexico`
- UK: 4120 → `uk`
- Japan: 5880 → `japan`
- EU-27: 27 individual country codes → `eu` (see country_partner_mapping.csv)
- Free Trade ROW: Australia, Korea, Singapore, Chile, etc. → `ftrow`
- All others → `row` (Rest of World, including Vietnam)

**GTAP Mapping:**
- HS10 codes are mapped to GTAP sectors using resources/hs10_gtap_crosswalk.csv
- This allows aggregation to GTAP sector level for economic modeling

**Caching:**
- Processed HS10×partner×GTAP data is cached in cache/hs10_by_partner_gtap_2024_con.rds (567KB)
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

1. **232.yaml** - Section 232 tariffs with variable-length HTS codes (4, 6, 8, or 10 digits)
2. **ieepa_rates.yaml** - IEEPA rates with hierarchical structure:
   - `headline_rates`: Default rate for each partner (applied to all GTAP sectors)
   - `product_rates`: HTS-based overrides with partner lists (optional)
   - `product_country_rates`: Most specific HTS×partner overrides (optional)
3. **other_params.yaml** - USMCA parameters, auto rebate rates, etc.

## Key Implementation Notes

**Core Functions (src/functions.R):**
- `load_imports_hs10_country()`: Reads Census ZIP files, extracts IMP_DETL.TXT, returns HS10×country×month data
- `load_ieepa_rates_yaml()`: Loads IEEPA YAML config and applies hierarchical rate logic (headline → product → product×country)
- `deduplicate_232_codes()`: Prevents double-counting when HTS codes overlap across 232 tariffs
- `calc_import_shares()`: Variable-length HTS code matching using regex prefix matching
- `calc_weighted_etr()`: Calculates ETR changes with USMCA exemptions and auto rebates
- `do_scenario()`: Main orchestrator - loads config, processes data, calculates ETRs, writes outputs

**Variable-Length HTS Matching:**
- Both 232 and IEEPA tariffs use prefix matching: '8703' matches all HS10 codes starting with 8703
- Supports 4-digit (chapter), 6-digit (subheading), 8-digit, and 10-digit (full HTS) codes
- Implemented via regex: `^(code1|code2|...)` pattern

**Country Partner Mapping:**
- Country codes are mapped to partners using resources/country_partner_mapping.csv
- 8 partner groups: china, canada, mexico, uk, japan, eu, row, ftrow
- Unmapped countries default to 'row'

**File Paths:**
- Import data path is hardcoded to `C:/Users/jar335/Downloads` in main.R
- Update this for different environments

## Style
- never use na.rm = T or any other kind of na filters. if we have missings that's a sign that something is wrong and it should break accordingly!
- use single quotes for strings
- please review the readme file each time you make a commit and determine whether you should update that file accordingly. 
