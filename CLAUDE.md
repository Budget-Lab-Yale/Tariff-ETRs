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
- **Section 301** (optional): Product-specific tariffs (primarily China). Unconditionally cumulative with all other authorities; applies to full customs value (no metal content scaling)
- **Section 201** (optional): Safeguard duties. Unconditionally cumulative; applies to full customs value.
- **Other** (optional): Miscellaneous Chapter 99 duties. Unconditionally cumulative; applies to full customs value.

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
- Sector×partner delta matrix (output/{scenario}/gtap_deltas_by_sector_country.csv)
- Census country deltas (output/{scenario}/deltas_by_census_country.csv)
- Census country×HTS2 deltas (output/{scenario}/deltas_by_census_country_hts2.csv)
- Overall deltas by country (output/{scenario}/overall_deltas.txt)
- Sector×partner tariff levels (output/{scenario}/gtap_levels_by_sector_country.csv)
- Census country tariff levels (output/{scenario}/levels_by_census_country.csv)
- Census country×HTS2 tariff levels (output/{scenario}/levels_by_census_country_hts2.csv)
- Overall tariff levels by country (output/{scenario}/overall_levels.txt)

**Output (time-varying scenarios):**
- Per-date shock commands (output/{scenario}/{date}/shocks.txt)
- Stacked deltas CSVs with `date`, `valid_from`, `valid_until` columns:
  - output/{scenario}/gtap_deltas_by_sector_country.csv
  - output/{scenario}/deltas_by_census_country.csv
  - output/{scenario}/deltas_by_census_country_hts2.csv
- Combined overall deltas with per-date sections (output/{scenario}/overall_deltas.txt)
- Stacked levels CSVs with `date`, `valid_from`, `valid_until` columns:
  - output/{scenario}/gtap_levels_by_sector_country.csv
  - output/{scenario}/levels_by_census_country.csv
  - output/{scenario}/levels_by_census_country_hts2.csv
- Combined overall tariff levels with per-date sections (output/{scenario}/overall_levels.txt)
- Validity intervals computed from sorted counter dates; last date uses `series_horizon` from scenario.yaml

**MFN Rates:**
- MFN rates at HS8 level loaded from path specified in `other_params.yaml` (`mfn_rates` key)
- Each config (baseline and counterfactual) specifies its own MFN rates path
- MFN is folded into the stacking rules as an unconditionally additive component of `final_rate`
- Specific-duty-only lines (~7% of HS8 codes) have mfn_rate = 0 (ad valorem equivalent not available)
- Optional MFN exemption shares (`mfn_exemption_shares` key in `other_params.yaml`) adjust for FTA/GSP preferences:
  `effective_mfn = mfn_rate * (1 - exemption_share)` at HS2×country granularity.
  Generated by `analysis/generate_mfn_exemption_shares.R` using Census calculated duty data.

**Baseline Architecture:**
- Baseline is a pointer to a historical date in `config/historical/` (defined in scenario.yaml)
- Historical configs are generated by tariff-rate-tracker and contain `statutory_rates.csv.gz` + `other_params.yaml`
- Deltas are computed as: counterfactual tariff level - baseline tariff level

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

Each scenario is defined by a `scenario.yaml` file in `config/scenarios/{name}/`. Historical tariff snapshots (generated by tariff-rate-tracker) live in `config/historical/{date}/`. Reform overlays (YAML diffs) live alongside the scenario.yaml.

```
config/
  historical/                          # Tracker-generated (git-lfs for .csv.gz)
    2025-01-01/                        # Pre-inauguration baseline
      statutory_rates.csv.gz
      other_params.yaml
    2026-01-01/                        # Post-inauguration
      statutory_rates.csv.gz
      other_params.yaml
    2026-02-24/                        # Reciprocal kicks in
      statutory_rates.csv.gz
      other_params.yaml

  scenarios/
    2-21_temp/
      scenario.yaml                    # Defines baseline + counterfactual
      reforms/
        s122/
          other_params.yaml            # s122_usmca_exception: 1
          s122.yaml                    # Section 122 overlay
```

### scenario.yaml Schema

```yaml
baseline: '2025-01-01'                 # Historical date = the "before" world

counterfactual:
  - '2026-01-01'                       # String = use historical as-is
  - date: '2026-02-24'                 # Object = historical + reform overlay
    reform: 'reforms/s122'             # Path relative to scenario dir
  - '2026-07-24'

series_horizon: '2026-12-31'
```

Entry normalization:
- String `'2026-01-01'` → `{date: '2026-01-01', historical: '2026-01-01', reform: NULL}`
- Object with `date` + `reform` → reform YAML overlays applied on top of historical CSV
- Object with `date` + `historical` + `reform` → use a different historical snapshot (rare)

### Reform Directories

Reform dirs contain optional files that modify the historical config:
- `other_params.yaml` — shallow-merged over historical via `modifyList()`
- Tariff YAML files (`s232.yaml`, `s122.yaml`, etc.) — merged via overlay logic
- Multiple dates can point to the same reform dir for reuse

### CSV Mode: `statutory_rates.csv.gz`

Generated by tariff-rate-tracker's `generate_etrs_config.R`. Contains pre-USMCA, pre-metal-content statutory rates per authority per HS10×country. ETRs applies all adjustments (USMCA, metal content, stacking).

**Key columns:**
- `hts10`, `cty_code` — product × country identifiers
- `mfn_rate` — MFN rate (tracker is source of truth; replaces separate mfn_rates.csv)
- `s232_*` — one column per 232 program (dynamic, discovered by `^s232_` prefix)
- `ieepa_reciprocal`, `ieepa_fentanyl`, `s301`, `s122`, `s201`, `other` — per-authority rates
- `target_total_*` — floor rates (1:1 naming convention with rate column)

**YAML overlay**: When YAML files coexist with the CSV, they act as counterfactual modifications:
- YAML rates overwrite CSV baseline for matching `(hs10, cty_code)` pairs
- New products/authorities in YAML are added to the baseline
- Explicit zeros in YAML suppress positive CSV rates (loaders use `overlay_mode = TRUE`)

### YAML Overlay Files (Reform Overlays)

YAML files in reform directories use the same hierarchical format as before. They are merged onto the CSV baseline at the `(hs10, cty_code)` level:
- Matching rows: overlay replaces CSV rate
- Explicit zeros: suppress positive CSV rates
- New rows: added to the baseline
- S232 overlays use column-level merge (only touches the programs defined in the overlay)

Supported overlay files: `s232.yaml`, `ieepa_reciprocal.yaml`, `ieepa_fentanyl.yaml`, `s122.yaml`, `s301.yaml`, `s201.yaml`, `other.yaml`

**Country Mnemonics** in YAML files: `china` → 5700, `canada` → 1220, `mexico` → 2010, `uk` → 4120, `japan` → 5880, `eu` → 27 EU codes, `ftrow` → free-trade ROW countries.

**other_params.yaml** (generated by tracker in historical/, overrideable in reforms):
   Contains: `mfn_rates`, `mfn_exemption_shares`, `usmca_product_shares`, `metal_content`, auto rebate params, USMCA exception flags.
   Note: `series_horizon` now lives in `scenario.yaml`, not `other_params.yaml`.

## Key Implementation Notes

**Architecture: Multi-Authority Rate Matrix**

The codebase uses a clean separation between config parsing and calculations:

*Config Parsing → Tabular Data:*
- `load_232_rates()`: Returns complete HS10×country tibble with one column per tariff (`s232_[tariff]_rate`)
- `load_ieepa_rates_yaml()`: Generic loader - returns complete HS10×country tibble with configurable column name
  - Used for reciprocal, fentanyl, Section 122, and Section 301 tariffs
  - Handles hierarchical rate structure (headline → product → product×country)
  - Supports `exempt_products` key: list of HTS codes (variable-length) excluded from coverage via prefix matching. Used for blanket authorities like IEEPA reciprocal that apply to all products except an exemption list.
- Both functions handle the full universe of HS10 codes × 240 countries
- No nested lists - just clean tibbles ready for joining

*Calculations → Stacking Rules:*
- `calc_weighted_etr()`: Joins config tibbles with import data, applies USMCA/rebates, metal content adjustment, then stacking rules
- Stacking rules are centralized in calculations.R
- Current rules (MFN is unconditionally additive in all branches):
  - **China (5700)**: `final_rate = mfn + max(232, reciprocal) + fentanyl + s122 + s301 + s201 + other` (fentanyl always stacks)
  - **Others**: `final_rate = mfn + (232 > 0 ? 232 : reciprocal + fentanyl) + s122 + s301 + s201 + other`
  - Section 122: excluded "to the extent the 232 tariff applies" — on 232-covered products, S122 applies only to `nonmetal_share` (same as IEEPA); on non-232 products, full S122 rate
  - **Section 301, Section 201, other**: Unconditionally cumulative — apply to full customs value regardless of 232/IEEPA coverage (no `nonmetal_share` scaling)
  - **Metal 232 derivatives**: 232 rate scaled by `metal_share`; IEEPA and S122 apply to non-metal portion (`nonmetal_share = 1 - metal_share`); S301/S201/other apply to full value
- Easy to modify for new tariff types or stacking logic

**Core Functions:**

*src/config_parsing.R:*
- `get_mnemonic_mapping()`: Loads country_partner_mapping.csv and returns mnemonic → country codes mapping
- `resolve_country_mnemonics()`: Expands mnemonics (e.g., 'eu') to individual Census country codes in rates config
- `load_s232_rates()`: Loads s232 YAML, expands to complete HS10×country tibble with coverage and country-specific rates. Returns `list(rate_matrix, usmca_exempt, target_total_rules)` where `target_total_rules` is a named list of per-program tibbles (cty_code, target_total_rate) for countries with floor rules, or NULL if none.
- `load_ieepa_rates_yaml()`: Generic IEEPA loader with configurable column name - applies hierarchical rate logic (headline → product → product×country). Supports `overlay_mode = TRUE` to preserve explicit zeros for CSV overlay merge.
- `load_metal_content()`: Loads metal content shares (flat, BEA, or CBO method) for Section 232 derivative adjustment. BEA method supports `bea_granularity: 'gtap'` (sector-level), `'naics'` (HS10-level via NAICS chaining), or `'detail'` (per-metal-type shares via 2017 BEA Detail IO table)
- `load_statutory_csv()`: Reads `statutory_rates.csv.gz` from tracker, returns config list matching `load_scenario_config()` schema. Discovers s232 programs dynamically from `^s232_` columns. Extracts MFN at hs10×country level. Validates s232 columns against `metal_programs`.

*src/data_processing.R:*
- `load_imports_hs10_country()`: Reads Census ZIP files, extracts IMP_DETL.TXT, returns HS10×country×month data
- `load_mfn_rates()`: Loads MFN baseline tariff rates at HS8 level from resources/mfn_rates_2025.csv
- `load_mfn_exemption_shares()`: Loads MFN exemption shares (FTA/GSP preferences) at HS2×country level

*src/calculations.R:*
- `load_scenario_definition()`: Reads `scenario.yaml`, normalizes counterfactual entries to `{date, historical, reform}` structure, validates historical dates exist
- `load_scenario_config()`: Loads CSV config from a directory (requires `statutory_rates.csv.gz`). Applies co-located YAML overlays if present.
- `load_config_with_reform()`: Loads historical config + optional reform overlay (other_params merge + YAML rate overlays)
- `merge_other_params()`: Shallow-merges reform other_params over historical via `modifyList()`
- `merge_s232_rate_matrix()`: Column-level merge for S232 rate matrix (only updates program columns present in overlay)
- `load_yaml_overlay()`: Scans config directory for YAML tariff files and loads them with `overlay_mode=TRUE` (preserving explicit zeros). Returns NULL if no YAML files found.
- `merge_overlay()`: Merges YAML overlay onto CSV baseline config. Single-column authorities use row-level upsert (anti_join + bind_rows). S232 uses column-level merge via `merge_s232_rate_matrix()`.
- `calc_etrs_for_config()`: Runs calc_weighted_etr() + aggregate_countries_to_partners() for one config set. MFN rates come from config (no separate parameter).
- `calc_delta()`: Computes delta = counterfactual level - baseline level at HS10×country level
- `do_scenario()`: Main orchestrator - reads scenario.yaml, loads baseline from historical/, builds counterfactual configs (historical + reform), computes deltas
- `do_scenario_static()`: Processes a single counterfactual entry
- `do_scenario_time_varying()`: Processes multiple counterfactual entries with date intervals
- `calc_weighted_etr()`: Joins tabular config data, applies USMCA exemptions/auto rebates, metal content adjustment, MFN exemption shares, generalized target_total floor logic, and stacking rules. MFN comes from CSV at hs10×country level (tracker is source of truth).
- `aggregate_countries_to_partners()`: Aggregates country-level results to 8 partner groups using import-weighted averaging
- `prepare_*_deltas()` / `prepare_*_levels()` functions: Pure computation (no I/O) for sector_country, country_level, country_hts2, and overall data
- `write_*_deltas()` / `write_*_levels()` functions: Delegate to prepare_* then write files
- `write_*_deltas_stacked()` / `write_*_levels_stacked()` functions: Stack per-date results with date column for time-varying scenarios
- `calc_overall_levels_data()`, `format_level_table()`, `write_overall_levels()`: Overall tariff level summaries
- `write_overall_levels_combined()`, `write_overall_deltas_combined()`: Combined text outputs for time-varying scenarios

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
