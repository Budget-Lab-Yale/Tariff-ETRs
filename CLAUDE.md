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

**Tariff Types:**
- **Section 232**: Product-specific tariffs (steel, aluminum, autos, etc.)
- **IEEPA Reciprocal**: Mutually exclusive with 232 (applies only to uncovered base)
- **IEEPA Fentanyl**:
  - China: STACKS on top of 232 + reciprocal
  - Others: Only applies to base not covered by 232 or reciprocal
- **Section 122** (optional): Stacks on top of IEEPA always; stacking on 232 controlled by `s122_stacks_on_232` flag

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

**Output (static scenarios):**
- GTAP shock commands (output/{scenario}/shocks.txt)
- Sector×country ETR matrix (output/{scenario}/etrs_by_sector_country.csv)
- Overall ETRs by country (output/{scenario}/overall_etrs.txt)

**Output (time-varying scenarios):**
- Per-date shock commands (output/{scenario}/{date}/shocks.txt)
- Stacked CSVs with `date` as first column (output/{scenario}/etrs_by_sector_country.csv)
- Combined overall ETRs with per-date sections (output/{scenario}/overall_etrs.txt)

## Development Commands

**Run the analysis:**
```r
source("src/main.R")
```

**Interactive development in RStudio:**
- Open `Tariff-ETRs.Rproj` to load the project with correct settings
- The project uses 2 spaces for indentation (configured in .Rproj file)

## Configuration Files

### Scenario Types

**Static scenarios** have config files directly in `config/{scenario}/`.

**Time-varying scenarios** use YYYY-MM-DD dated subfolders, each containing a complete config set:
```
config/tariff-timeline/
  2025-02-04/
    232.yaml
    ieepa_reciprocal.yaml
    ieepa_fentanyl.yaml
    s122.yaml              # optional
    other_params.yaml
  2025-04-02/
    232.yaml
    ...
```
Detection is automatic based on subdirectory names. Each date subfolder must have a complete set of config files (no inheritance/fallback). The same 2024 import weights are reused across all dates.

### Config Files

Each scenario (or each date subfolder) requires:

1. **232.yaml** - Section 232 tariffs with country-level rates and defaults:
   ```yaml
   tariff_name:
     base: [list of variable-length HTS codes: 4, 6, 8, or 10 digits]
     rates:
       default: 0.5          # Default rate for all countries
       uk: 0.25              # UK-specific override (mnemonic)
       china: 0.3            # China-specific override (mnemonic)
       # Or use Census codes directly:
       '4120': 0.25          # UK (Census code)
       '5700': 0.3           # China (Census code)
     usmca_exempt: 0         # 1 = apply USMCA exemptions, 0 = no exemption
   ```

2. **ieepa_reciprocal.yaml** - IEEPA reciprocal tariffs (mutually exclusive with 232):
   ```yaml
   headline_rates:
     default: 0.1            # Default rate for unmapped countries
     china: 0.2              # China (mnemonic - expands to code 5700)
     canada: 0.35            # Canada (mnemonic)
     eu: 0.15                # EU-27 (expands to all 27 country codes!)
     # ... other countries/mnemonics

   product_rates:            # Optional: HTS-specific overrides
     '8703': 0.15            # Simple rate (applies to all countries)
   ```

3. **ieepa_fentanyl.yaml** - IEEPA fentanyl tariffs (STACKS for China, mutually exclusive otherwise):
   ```yaml
   headline_rates:
     default: 0.1            # Default rate for unmapped countries
     china: 0.1              # China - STACKS on top of 232 + reciprocal
     # ... other countries/mnemonics

   product_rates:            # Optional: HTS-specific overrides
     '2939': 0.2             # Fentanyl precursors
   ```

**Country Mnemonics:** Config files support friendly names that expand to Census codes:
- `china` → 5700
- `canada` → 1220
- `mexico` → 2010
- `uk` → 4120
- `japan` → 5880
- `eu` → all 27 EU country codes (Austria, Belgium, Bulgaria, etc.)
- `ftrow` → all free-trade ROW countries (Australia, Korea, Singapore, etc.)

You can mix mnemonics and Census codes in the same config file.

4. **s122.yaml** (optional) - Section 122 balance-of-payments tariffs:
   ```yaml
   headline_rates:
     default: 0.05           # Default rate for all countries
     china: 0.10             # China-specific override
     # ... other countries/mnemonics

   product_rates:            # Optional: HTS-specific overrides
     '8703': 0.08            # Autos get different rate

   product_country_rates:    # Optional: HTS×country overrides
     - hts: ['87032201']
       country: '5700'
       rate: 0.20
   ```
   Uses the same hierarchical format and `load_ieepa_rates_yaml()` loader as IEEPA configs.
   When absent, Section 122 rates default to zero.

5. **other_params.yaml** - USMCA parameters, auto rebate rates, metal content, s122 flags:
   ```yaml
   # USMCA and auto sector parameters
   us_auto_content_share: 0.4
   us_auto_assembly_share: 0.33
   auto_rebate_rate: 0.0375
   ieepa_usmca_exception: 1      # Apply USMCA exemption to IEEPA (1 = yes)

   # Section 122 flags
   s122_usmca_exception: 0       # Apply USMCA exemption to s122 (1 = yes, default 0)
   s122_stacks_on_232: 1         # s122 stacks on 232 (1 = yes, default 1)

   # Metal content shares (optional, see load_metal_content() docstring)
   metal_content:
     method: 'cbo'               # 'flat', 'bea', or 'cbo'
     primary_chapters: ['72', '73', '76']
     metal_programs: [steel, aluminum_base_articles, ...]
   ```

## Key Implementation Notes

**Architecture: Multi-Authority Rate Matrix**

The codebase uses a clean separation between config parsing and calculations:

*Config Parsing → Tabular Data:*
- `load_232_rates()`: Returns complete HS10×country tibble with one column per tariff (`s232_[tariff]_rate`)
- `load_ieepa_rates_yaml()`: Generic loader - returns complete HS10×country tibble with configurable column name
  - Used for reciprocal, fentanyl, and Section 122 tariffs
  - Handles hierarchical rate structure (headline → product → product×country)
- Both functions handle the full universe of HS10 codes × 240 countries
- No nested lists - just clean tibbles ready for joining

*Calculations → Stacking Rules:*
- `calc_weighted_etr()`: Joins config tibbles with import data, applies USMCA/rebates, metal content adjustment, then stacking rules
- Stacking rules are centralized in calculations.R
- Current rules (with metal content adjustment):
  - **China (5700)**:
    - 232 > 0: `rate_232 + reciprocal * nonmetal_share + fentanyl + s122 * s122_stacks_on_232`
    - No 232: `reciprocal + fentanyl + s122`
  - **Others**:
    - 232 > 0: `rate_232 + (reciprocal + fentanyl) * nonmetal_share + s122 * s122_stacks_on_232`
    - No 232: `reciprocal + fentanyl + s122`
  - **Metal 232 derivatives**: 232 rate scaled by `metal_share`; IEEPA applies to non-metal portion (`nonmetal_share = 1 - metal_share`)
  - **Section 122**: Always stacks on IEEPA; stacking on 232 controlled by `s122_stacks_on_232` flag (default 1)
  - **USMCA**: s122 has its own USMCA exemption flag (`s122_usmca_exception`, default 0)
- Easy to modify for new tariff types or stacking logic

**Core Functions:**

*src/config_parsing.R:*
- `get_mnemonic_mapping()`: Loads country_partner_mapping.csv and returns mnemonic → country codes mapping
- `resolve_country_mnemonics()`: Expands mnemonics (e.g., 'eu') to individual Census country codes in rates config
- `load_232_rates()`: Loads 232 YAML, expands to complete HS10×country tibble with coverage and country-specific rates
- `load_ieepa_rates_yaml()`: Generic IEEPA loader with configurable column name - applies hierarchical rate logic (headline → product → product×country)
- `load_metal_content()`: Loads metal content shares (flat, BEA, or CBO method) for Section 232 derivative adjustment

*src/data_processing.R:*
- `load_imports_hs10_country()`: Reads Census ZIP files, extracts IMP_DETL.TXT, returns HS10×country×month data

*src/calculations.R:*
- `detect_scenario_type()`: Auto-detects static vs time-varying scenarios by looking for YYYY-MM-DD subfolders
- `load_scenario_config()`: Loads all config YAMLs (232, IEEPA reciprocal, IEEPA fentanyl, optional s122, other_params) from a single directory into a named list
- `calc_etrs_for_config()`: Runs calc_weighted_etr() + aggregate_countries_to_partners() for one config set
- `do_scenario()`: Main orchestrator/dispatcher - loads shared data, detects scenario type, dispatches to static or time-varying
- `do_scenario_static()`: Runs a single-config scenario (original behavior)
- `do_scenario_time_varying()`: Loops over dated configs, writes per-date shocks + stacked CSVs
- `calc_weighted_etr()`: Joins tabular config data, applies USMCA exemptions/auto rebates, metal content adjustment, and stacking rules, calculates final ETR
- `aggregate_countries_to_partners()`: Aggregates country-level ETRs to 8 partner groups using import-weighted averaging
- `prepare_*()` functions: Pure computation (no I/O) for sector_country, country_level, country_hts2, and overall ETR data
- `write_*()` functions: Delegate to prepare_* then write files
- `write_*_stacked()` functions: Stack per-date prepare_* results with date column for time-varying scenarios

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
