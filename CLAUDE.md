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
- **Section 122** (optional): Excluded to the extent 232 applies (scales by `nonmetal_share` on 232-covered products; full rate on non-232 products)

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
- Sector×country delta matrix (output/{scenario}/deltas_by_sector_country.csv)
- Overall deltas by country (output/{scenario}/overall_deltas.txt)
- Sector×country tariff levels (output/{scenario}/levels_by_sector_country.csv)
- Overall tariff levels by country (output/{scenario}/overall_levels.txt)

**Output (time-varying scenarios):**
- Per-date shock commands (output/{scenario}/{date}/shocks.txt)
- Stacked CSVs with `date` as first column (output/{scenario}/deltas_by_sector_country.csv)
- Combined overall deltas with per-date sections (output/{scenario}/overall_deltas.txt)
- Stacked levels CSV with `date` as first column (output/{scenario}/levels_by_sector_country.csv)
- Combined overall tariff levels with per-date sections (output/{scenario}/overall_levels.txt)

**MFN Baseline Rates:**
- MFN rates at HS8 level loaded from path specified in `other_params.yaml` (`mfn_rates` key)
- Each config (baseline and counterfactual) specifies its own MFN rates path
- Tariff levels = MFN baseline + policy tariffs; deltas = counterfactual level - baseline level
- Specific-duty-only lines (~7% of HS8 codes) have mfn_rate = 0 (ad valorem equivalent not available)

**Baseline Architecture:**
- A single shared baseline lives at `config/baseline/` (not per-scenario)
- Baseline requires at least `other_params.yaml`
- Tariff YAML files (232.yaml, ieepa_*.yaml, s122.yaml) are optional; missing = zero rates
- Deltas are computed as: counterfactual tariff level - baseline tariff level
- For MFN-only baseline: omit all tariff YAMLs → all policy rates = 0 → delta = policy rate
- Baseline can be static (files in baseline/) or time-varying (YYYY-MM-DD subfolders within baseline/)

## Development Commands

**Run the analysis:**
```r
source("src/main.R")
```

**Interactive development in RStudio:**
- Open `Tariff-ETRs.Rproj` to load the project with correct settings
- The project uses 2 spaces for indentation (configured in .Rproj file)

## Configuration Files

### Scenario Structure

All scenarios share a single baseline at `config/baseline/`. Counterfactual configs live in each scenario directory, either at the root (static) or in YYYY-MM-DD dated subfolders (time-varying):

```
config/
  baseline/                    # Shared baseline config (required)
    other_params.yaml          # Must include mfn_rates pointer
    232.yaml                   # Optional (missing = zero rates)
    ieepa_reciprocal.yaml      # Optional
    ieepa_fentanyl.yaml        # Optional
  {scenario}/                  # Counterfactual configs only
    2026-01-01/                # Counterfactual date (time-varying)
      other_params.yaml
      232.yaml
      ieepa_reciprocal.yaml
      ...
```

Detection is automatic based on subdirectory names. Each date subfolder must have a complete config set (no inheritance/fallback). The same 2024 import weights are reused across all dates.

The baseline itself can be static (files directly in `baseline/`) or time-varying (YYYY-MM-DD subfolders within `baseline/`). For time-varying counterfactuals with a time-varying baseline, each counterfactual date is matched to the most recent baseline date <= counter date.

### Config Files

Each config directory (baseline or counterfactual date) requires `other_params.yaml`. Tariff YAMLs are optional (missing = zero rates):

1. **232.yaml** (optional) - Section 232 tariffs with country-level rates and defaults:
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

2. **ieepa_reciprocal.yaml** (optional) - IEEPA reciprocal tariffs (mutually exclusive with 232):
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

3. **ieepa_fentanyl.yaml** (optional) - IEEPA fentanyl tariffs (STACKS for China, mutually exclusive otherwise):
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

5. **other_params.yaml** (required) - MFN rates pointer, USMCA parameters, auto rebate rates, etc.
   Must include: `mfn_rates: 'resources/mfn_rates_2025.csv'`

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
- Current rules:
  - **China (5700)**: `final_rate = max(232, reciprocal) + fentanyl + s122` (fentanyl always stacks)
  - **Others**: `final_rate = (232 > 0 ? 232 : reciprocal + fentanyl) + s122`
  - Section 122: excluded "to the extent the 232 tariff applies" — on 232-covered products, S122 applies only to `nonmetal_share` (same as IEEPA); on non-232 products, full S122 rate
  - **Metal 232 derivatives**: 232 rate scaled by `metal_share`; IEEPA and S122 apply to non-metal portion (`nonmetal_share = 1 - metal_share`)
- Easy to modify for new tariff types or stacking logic

**Core Functions:**

*src/config_parsing.R:*
- `get_mnemonic_mapping()`: Loads country_partner_mapping.csv and returns mnemonic → country codes mapping
- `resolve_country_mnemonics()`: Expands mnemonics (e.g., 'eu') to individual Census country codes in rates config
- `load_232_rates()`: Loads 232 YAML, expands to complete HS10×country tibble with coverage and country-specific rates
- `load_ieepa_rates_yaml()`: Generic IEEPA loader with configurable column name - applies hierarchical rate logic (headline → product → product×country)
- `load_metal_content()`: Loads metal content shares (flat, BEA, or CBO method) for Section 232 derivative adjustment. BEA method supports `bea_granularity: 'gtap'` (sector-level), `'naics'` (HS10-level via NAICS chaining), or `'detail'` (per-metal-type shares via 2017 BEA Detail IO table)

*src/data_processing.R:*
- `load_imports_hs10_country()`: Reads Census ZIP files, extracts IMP_DETL.TXT, returns HS10×country×month data
- `load_mfn_rates()`: Loads MFN baseline tariff rates at HS8 level from resources/mfn_rates_2025.csv

*src/calculations.R:*
- `detect_baseline_structure()`: Detects shared baseline structure (static or time-varying)
- `detect_scenario_structure()`: Detects counterfactual structure (counter dates)
- `load_scenario_config()`: Loads all config YAMLs from a single directory into a named list. Tariff files (232, IEEPA, s122) are optional; missing = NULL = zero rates. MFN rates loaded from `other_params$mfn_rates` path.
- `calc_etrs_for_config()`: Runs calc_weighted_etr() + aggregate_countries_to_partners() for one config set. MFN rates come from config (no separate parameter).
- `calc_delta()`: Computes delta = counterfactual level - baseline level at HS10×country level
- `do_scenario()`: Main orchestrator - loads shared data, detects structure, processes baseline, processes counterfactual, computes deltas
- `do_scenario_static()`: Processes a static counterfactual with baseline delta computation
- `do_scenario_time_varying()`: Loops over counterfactual dates, matches each to baseline, computes deltas
- `match_baseline()`: Matches a counterfactual date to the appropriate baseline result (static or carry-forward)
- `calc_weighted_etr()`: Joins tabular config data, applies USMCA exemptions/auto rebates, metal content adjustment, and stacking rules. Handles NULL tariff inputs (missing config = zero rates). Also joins MFN rates and computes tariff levels.
- `aggregate_countries_to_partners()`: Aggregates country-level results to 8 partner groups using import-weighted averaging
- `prepare_*_deltas()` functions: Pure computation (no I/O) for sector_country, country_level, country_hts2, and overall delta data
- `write_*_deltas()` functions: Delegate to prepare_* then write delta files
- `write_*_deltas_stacked()` functions: Stack per-date prepare_* results with date column for time-varying scenarios
- `prepare_levels_by_sector_country()`, `write_levels_by_sector_country()`: Tariff level outputs
- `calc_overall_levels_data()`, `format_level_table()`, `write_overall_levels()`: Overall tariff level summaries
- `write_levels_by_sector_country_stacked()`, `write_overall_levels_combined()`: Stacked/combined level outputs for time-varying scenarios

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
