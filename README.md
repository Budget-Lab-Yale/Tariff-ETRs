# Tariff Delta Calculator

Calculate tariff deltas (counterfactual - baseline) and absolute tariff levels on U.S. imports by trading partner and GTAP sector under various tariff policy scenarios. **Delta values represent the change from a configurable baseline scenario.**

## Overview

This project analyzes U.S. import trade data to compute tariff changes incorporating:
- **Section 232 tariffs**: Steel, aluminum, softwood lumber, furniture, automobiles, and auto parts
- **IEEPA Reciprocal tariffs**: Broad-based tariffs (mutually exclusive with Section 232)
- **IEEPA Fentanyl tariffs**: Targeted tariffs that stack for China, mutually exclusive otherwise
- **Section 122 tariffs**: Balance-of-payments tariffs that stack on top of all other authorities
- **Section 301 tariffs**: Product-specific tariffs (primarily China) that stack unconditionally on full customs value
- **USMCA exemptions**: Trade agreement provisions with content requirements
- **Metal content adjustment**: Section 232 derivative products taxed on metal content share only
- **Baseline scenarios**: Any scenario can serve as a baseline; deltas are computed as counterfactual level - baseline level

## Repository Structure

```
Tariff-ETRs/
├── src/
│   ├── main.R              # Main execution script
│   ├── config_parsing.R    # Config loading and rate expansion
│   ├── data_processing.R   # Census data processing
│   └── calculations.R      # Delta/level calculations and aggregation
├── config/
│   ├── baseline/                 # Shared baseline config
│   │   ├── other_params.yaml     # Baseline parameters (MFN rates pointer, metal content)
│   │   └── s232.yaml             # Pre-2025 Section 232 tariffs (optional)
│   └── {scenario}/               # Counterfactual configs only
│       ├── 2026-01-01/           # Counterfactual date (time-varying)
│       │   ├── s232.yaml
│       │   ├── ieepa_reciprocal.yaml
│       │   ├── ieepa_fentanyl.yaml
│       │   ├── s122.yaml
│       │   ├── s301.yaml
│       │   └── other_params.yaml
│       └── 2026-02-24/
│           └── ...
├── resources/
│   ├── hs10_gtap_crosswalk.csv       # HTS10 to GTAP sector mapping
│   ├── country_partner_mapping.csv   # Census country code → partner group
│   ├── census_codes.csv              # Census country codes and names
│   ├── usmca_shares.csv              # USMCA-qualifying trade shares
│   ├── gtap_import_weights.csv       # Import weights for aggregation
│   ├── mfn_rates_2025.csv            # MFN baseline tariff rates (HS8 level)
│   ├── mfn_exemption_shares.csv     # MFN exemption shares (HS2 × country)
│   ├── gtap_bea_crosswalk.csv        # GTAP → BEA industry mapping (for metal content)
│   ├── metal_content_shares_*.csv    # Pre-computed metal content shares (GTAP/NAICS/detail)
│   ├── naics_bea_*_crosswalk.csv     # NAICS → BEA crosswalks (summary and detail)
│   ├── bea/                          # BEA I-O source data
│   └── cbo/                          # CBO HTS derivative product lists
├── cache/
│   └── hs10_by_country_gtap_2024_con.rds  # Cached import data
├── output/
│   ├── baseline/                                # Baseline tariff levels (no deltas/shocks)
│   │   ├── gtap_levels_by_sector_country.csv    # Tariff levels (sector × partner)
│   │   ├── levels_by_census_country.csv         # Tariff levels by census country
│   │   ├── levels_by_census_country_hts2.csv    # Tariff levels by country × HTS chapter
│   │   └── overall_levels.txt                   # Summary: total tariff levels
│   ├── {static-scenario}/
│   │   ├── shocks.txt                           # GTAP shock commands
│   │   ├── gtap_deltas_by_sector_country.csv         # Delta matrix (sector × partner)
│   │   ├── deltas_by_census_country.csv         # Deltas by census country
│   │   ├── deltas_by_census_country_hts2.csv    # Deltas by country × HTS chapter
│   │   ├── overall_deltas.txt                   # Summary: delta statistics
│   │   ├── gtap_levels_by_sector_country.csv         # Tariff levels (sector × partner)
│   │   ├── levels_by_census_country.csv         # Tariff levels by census country
│   │   ├── levels_by_census_country_hts2.csv    # Tariff levels by country × HTS chapter
│   │   └── overall_levels.txt                   # Summary: total tariff levels
│   └── {time-varying-scenario}/
│       ├── 2026-01-01/shocks.txt                # Per-date shock commands
│       ├── 2026-02-24/shocks.txt
│       ├── gtap_deltas_by_sector_country.csv         # Stacked CSV (date column first)
│       ├── deltas_by_census_country.csv         # Stacked CSV (date column first)
│       ├── deltas_by_census_country_hts2.csv    # Stacked CSV (date column first)
│       ├── overall_deltas.txt                   # Combined with per-date sections
│       ├── gtap_levels_by_sector_country.csv         # Stacked levels (date column first)
│       ├── levels_by_census_country.csv         # Stacked levels (date column first)
│       ├── levels_by_census_country_hts2.csv    # Stacked levels (date column first)
│       └── overall_levels.txt                   # Combined levels with per-date sections
└── README.md
```

## Requirements

- R (version 4.0+)
- R packages: `tidyverse`, `yaml`
- U.S. import data files (fixed-width format, 2024)

## Data Sources

**Import Data:**
- Source: [U.S. Census Bureau - Merchandise Trade Imports](https://www.census.gov/foreign-trade/data/IMDB.html)
- Format: Monthly ZIP files (IMDByymm.ZIP) containing IMP_DETL.TXT fixed-width format files
- Location: Set `import_data_path` in `main.R` or via `--import-data-path` CLI flag (environment-specific; no universal default)

**Country Codes:**
The project uses Census Bureau country codes. Tariff rates are specified at the country level, with calculations at HTS10×country level before aggregating to 8 partner groups:

| Partner | Countries |
|---------|-----------|
| china | 5700 |
| canada | 1220 |
| mexico | 2010 |
| japan | 5880 |
| uk | 4120 |
| eu | 27 EU member state codes |
| ftrow | 18 FTA partners (Australia, Korea, Singapore, Chile, Colombia, Peru, Costa Rica, Guatemala, Honduras, Nicaragua, Panama, El Salvador, Dominican Republic, Bahrain, Israel, Jordan, Oman, Morocco) |
| row | All unmapped countries default to this group |

See `resources/country_partner_mapping.csv` for the complete mapping.

**GTAP Crosswalk:**
HTS10 codes map to GTAP sectors via a crosswalk derived from [Angel Aguiar's 6-digit crosswalk](https://www.gtap.agecon.purdue.edu/resources/res_display.asp?RecordID=5111).

## Usage

### Running Scenarios

1. Set scenarios in `src/main.R`:
   ```r
   scenarios <- c('2-21_perm', '2-21_temp')
   ```

2. Ensure config files exist in `config/{scenario}/` and a shared baseline exists at `config/baseline/`

3. Run:
   ```r
   source('src/main.R')
   ```

### Creating a New Scenario

1. Ensure the shared baseline exists at `config/baseline/` (with at least `other_params.yaml`)

2. Create scenario directory:
   ```bash
   mkdir -p config/my_scenario
   ```

3. Add counterfactual config files (date subfolders for time-varying, or files at root for static)

4. Add to `scenarios` in `main.R` and run

### Scenario Structure

All scenarios share a single baseline at `config/baseline/`. The baseline defines the reference point; deltas are computed as `counterfactual_level - baseline_level`.

```
config/
  baseline/                    # Shared baseline config
    other_params.yaml          # Must include mfn_rates pointer
    s232.yaml                  # Optional (missing = zero 232 rates)
    ieepa_reciprocal.yaml      # Optional (missing = zero rates)
    ieepa_fentanyl.yaml        # Optional (missing = zero rates)
    s122.yaml                  # Optional (missing = zero rates)
  {scenario}/                  # Counterfactual configs only
    2026-01-01/                # Counterfactual date (time-varying)
      other_params.yaml
      s232.yaml
      ...
```

For a simple MFN-only baseline, just `baseline/other_params.yaml` is needed (all tariff YAMLs omitted = zero policy rates). The current `2-21_perm` and `2-21_temp` scenarios use a pre-2025 baseline that includes original Section 232 steel/aluminum tariffs (Proclamations 9705/9704) and 2020 derivative products (Proclamation 9980) at the rates and country exemptions in effect as of January 2025. The baseline itself can be time-varying by adding YYYY-MM-DD subfolders within `baseline/`.

### MFN Rates

Each `other_params.yaml` must include a pointer to the MFN rates file:

```yaml
mfn_rates: 'resources/mfn_rates_2025.csv'
```

### MFN Exemption Shares (optional)

Many countries don't pay the full statutory MFN rate due to FTAs, GSP, and other duty-free provisions. To account for this, specify an MFN exemption shares file in `other_params.yaml`:

```yaml
mfn_exemption_shares: 'resources/mfn_exemption_shares.csv'
```

When present, MFN rates are adjusted: `effective_mfn = mfn_rate * (1 - exemption_share)`. This affects tariff levels and the reciprocal target-total rule. Exemption shares are at HS2 × country granularity; missing combinations default to 0 (no exemption).

To regenerate: `Rscript analysis/generate_mfn_exemption_shares.R` (requires Census API access and cached import data).

### Time-Varying Scenarios

To model tariff rates that change over time, create dated subfolders (YYYY-MM-DD) for the counterfactual configs. Each subfolder must contain a complete set of config files:

```bash
config/baseline/                  # Shared baseline
  other_params.yaml
config/tariff-timeline/           # Scenario counterfactuals
  2025-02-04/
    s232.yaml
    ieepa_reciprocal.yaml
    ieepa_fentanyl.yaml
    s122.yaml              # optional
    s301.yaml              # optional
    other_params.yaml
  2025-04-02/
    s232.yaml
    ieepa_reciprocal.yaml
    ieepa_fentanyl.yaml
    s122.yaml              # optional
    s301.yaml              # optional
    other_params.yaml
```

Detection is automatic: if a scenario directory contains YYYY-MM-DD subfolders, it runs as time-varying. The same 2024 import weights are reused across all dates. Outputs:
- **Shock commands**: Per-date subfolders (`output/scenario/2025-02-04/shocks.txt`)
- **CSV files**: Single stacked file with `date` as the first column
- **overall_deltas.txt**: Combined file with per-date sections

## Configuration

### Section 232 Tariffs (`s232.yaml`)

Defines tariff rates and product coverage using variable-length HTS codes (4, 6, 8, or 10 digits) with prefix matching. **Optional**: if missing, all Section 232 rates are zero.

```yaml
steel:
  base:
    - '73012010'  # 8-digit: matches HTS10 codes starting with 73012010
    - '7307'      # 4-digit: matches all HTS10 codes starting with 7307
  rates:
    default: 0.50   # Default rate for all countries
    '4120': 0.25    # UK-specific override
  target_total:     # Optional: country-level "combined duty floor"
    japan: 0.15     # Effective 232 add-on = max(0.15 - MFN, 0)
    eu: 0.15        # Expands to all 27 EU country codes
  usmca_exempt: 0   # 1 = USMCA exemption applies
```

### IEEPA Rates (`ieepa_reciprocal.yaml`, `ieepa_fentanyl.yaml`)

Hierarchical rate structure with three levels of specificity. **Optional**: if missing, all IEEPA rates are zero.

```yaml
headline_rates:
  default: 0.10    # Default for unmapped countries
  '5700': 0.10     # China
  '1220': 0.00     # Canada (exempt)

product_rates:
  '8703': 0.0      # Exempt autos for all countries

product_country_rates:
  - hts: ['87032201', '87032301']
    country: '5700'
    rate: 0.50

target_total_rules:          # Optional: reciprocal "combined duty target"
  uk: 0.10                   # Effective add-on = max(target - MFN, 0)
  eu: 0.15
```

`product_country_rates` also supports `countries` (list) to apply one HTS block to multiple countries.

**Stacking Rules:**
All tariff authorities are combined via a unified stacking formula. MFN is unconditionally additive in all branches:
- **MFN**: Always additive — included in `final_rate` for every product×country combination
- **Section 232**: Takes precedence over IEEPA when applicable. For metal derivative products, the 232 rate is scaled by `metal_share`; IEEPA applies to the non-metal portion.
- **IEEPA Reciprocal**: Applies to imports not covered by Section 232 (or to the non-metal portion of 232 derivatives)
- **IEEPA Fentanyl**:
  - *China*: Stacks on top of everything (232 + reciprocal + fentanyl)
  - *Others*: Only applies to base not covered by 232 or reciprocal (or to the non-metal portion of 232 derivatives)
- **Section 122**: Stacks on top of all other tariffs. Stacking on 232 controlled by `s122_stacks_on_232` flag in `other_params.yaml`.
- **Section 301**: Unconditionally cumulative with all other authorities. Applies to full customs value (no metal content scaling).

### Section 122 Tariffs (`s122.yaml`, optional)

Balance-of-payments tariffs that stack on top of all other authorities. Uses the same hierarchical format as IEEPA:

```yaml
headline_rates:
  default: 0.0           # Default for all countries
  china: 0.10            # China-specific override

product_rates:
  '8703': 0.05           # Product-specific override (all countries)

product_country_rates:
  - hts: ['87032201']
    country: '5700'
    rate: 0.20
```

When `s122.yaml` is absent from a scenario, Section 122 rates default to zero.

### Section 301 Tariffs (`s301.yaml`, optional)

Product-specific tariffs (Trade Act of 1974, administered by USTR) that stack unconditionally on full customs value. Uses the same hierarchical format as IEEPA:

```yaml
headline_rates:
  default: 0.0           # Default for all countries
  china: 0.25            # China-specific rate

product_rates:
  '8541': 0.50           # Product-specific override (all countries)

product_country_rates:
  - hts: ['85414000']
    country: '5700'
    rate: 1.00
```

When `s301.yaml` is absent from a scenario, Section 301 rates default to zero.

### Metal Content Shares (`other_params.yaml`)

Section 232 tariffs on derivative products (outside HTS Chapters 72/73/76) legally apply only to the metal content share of the product, not the full customs value. The non-metal portion receives IEEPA tariffs instead.

```yaml
metal_content:
  method: 'bea'                      # 'flat', 'bea', or 'cbo'
  flat_share: 1.0                     # Used when method = 'flat'
  primary_chapters: ['72', '73', '76'] # Forced to share = 1.0
  bea_table: 'domestic'               # BEA: 'domestic' or 'total'
  bea_granularity: 'gtap'             # BEA: 'gtap', 'naics', or 'detail'
  metal_programs:                     # 232 tariff names that are metal programs
    - steel
    - aluminum_base_articles
    - aluminum_derivative_9903_85_04
    - copper_derivatives
  program_metal_types:                # Per-metal-type scaling (detail mode)
    steel: 'steel'
    aluminum_base_articles: 'aluminum'
    aluminum_derivative_9903_85_04: 'aluminum'
    copper_derivatives: 'copper'
  # CBO-specific parameters (optional, defaults match CBO's config):
  cbo_high_share: 0.75               # Used when method = 'cbo'
  cbo_low_share: 0.25                # Used when method = 'cbo'
  cbo_copper_share: 0.90             # Used when method = 'cbo'
```

**Methods:**
- `flat`: Uniform metal share for all derivative products (e.g., 1.0 = full value, 0.5 = TPC assumption, 0.0 = lower bound)
- `bea`: Industry-varying shares computed from BEA Input-Output requirements (primary metals inputs / total industry output). Three granularity levels:
  - `gtap`: Shares at the GTAP sector level (~45 sectors)
  - `naics`: Shares at the HS10 level via HS10 → NAICS → BEA summary chaining (~20K products), falling back to GTAP-level for unmatched codes
  - `detail`: Per-metal-type shares (steel, aluminum, copper, other) via the 2017 BEA Detail IO table (~402 commodities, 10 metal sub-industries). When combined with `program_metal_types`, each 232 program is scaled by its own metal type's share rather than the aggregate. Falls back to GTAP-level aggregate for unmatched codes.
- `cbo`: Product-level shares from the [CBO conventional tariff analysis model](https://github.com/US-CBO/conventional-tariff-analysis-model). Classifies Section 232 derivatives into three buckets: high metal content (75%, 168 products), low metal content (25%, 735 products), and copper derivatives (90%, 118 products). Products not in any CBO list default to 100%. HTS lists are stored in `resources/cbo/`.

**Per-Metal-Type Scaling (`program_metal_types`):** When `bea_granularity: 'detail'` is used with `program_metal_types`, each 232 program is scaled by the share of its specific metal type (e.g., a steel program uses `steel_share`, an aluminum program uses `aluminum_share`). Valid types: `steel`, `aluminum`, `copper`, `other`. Programs not in `program_metal_types` fall back to aggregate `metal_share`. Without `program_metal_types`, all programs use aggregate `metal_share` (backward compatible).

**When `metal_content` is absent** from `other_params.yaml`, defaults to `flat` with `share = 1.0` (identical to pre-metal-content behavior).

**To regenerate BEA shares:** Run `python scripts/build_naics_crosswalks.py` then `Rscript scripts/build_metal_content_shares.R` from the project root.

## Output

### Shock Commands (`shocks.txt`)

GTAP-formatted commands:
```
Shock tms("i_s","China","USA") = 50.0;
Shock tms("nfm","China","USA") = 50.0;
```

### Delta Matrix (`gtap_deltas_by_sector_country.csv`)

Wide-format CSV with tariff deltas in percentage points. Rows are GTAP sectors, columns are partner groups. Values represent the change from baseline scenario.

### Country-Level Deltas (`deltas_by_census_country.csv`)

Overall delta for each census country, weighted by 2024 imports:

```csv
cty_code,country_name,etr
5700,China,24.02
1220,Canada,8.45
```

Sorted by delta descending.

### Country × HTS Chapter Deltas (`deltas_by_census_country_hts2.csv`)

Deltas by country and 2-digit HTS chapter. Countries as rows, chapters (01-97) as columns.

### Summary Statistics (`overall_deltas.txt`)

Overall deltas by partner using both GTAP weights and 2024 Census import weights, plus tariff coverage statistics.

### Tariff Levels (`gtap_levels_by_sector_country.csv`)

Total tariff levels (MFN + policy tariffs) in percentage points. Same format as `gtap_deltas_by_sector_country.csv` but represents absolute tariff rates. MFN is included in the stacking formula as an unconditionally additive component; rates are specified per-config via the `mfn_rates` pointer in `other_params.yaml`.

### Country-Level Tariff Levels (`levels_by_census_country.csv`)

Total tariff level for each census country, weighted by 2024 imports. Same format as `deltas_by_census_country.csv` but represents absolute tariff rates.

### Country × HTS Chapter Tariff Levels (`levels_by_census_country_hts2.csv`)

Tariff levels by country and 2-digit HTS chapter. Same format as `deltas_by_census_country_hts2.csv` but represents absolute tariff rates.

### Overall Tariff Levels (`overall_levels.txt`)

Overall tariff levels by partner using both GTAP weights and 2024 Census import weights.

## Trading Partner Groups

| Partner | Description |
|---------|-------------|
| china | Mainland China |
| canada | Canada |
| mexico | Mexico |
| japan | Japan |
| uk | United Kingdom |
| eu | European Union (27 members) |
| ftrow | Free Trade Rest of World |
| row | Rest of World |

## How It Works

1. **Load Config**: Parse YAML files into HTS10×country rate tables (232, IEEPA reciprocal, IEEPA fentanyl, Section 122, Section 301)
2. **Load Imports**: Read Census IMP_DETL.TXT files, aggregate by HTS10×country
3. **Process Baseline**: Compute tariff levels for baseline config and write levels to `output/baseline/`
4. **Process Counterfactual**: Compute tariff levels for counterfactual config(s)
5. **Compute Deltas**: Delta = counterfactual level - baseline level
6. **Aggregate**: Roll up to partner×GTAP level using import-weighted averaging
7. **Output**: Write shock commands, CSVs, and summary statistics

## Contact

Email: john.ricco@yale.edu
Twitter: @riccoja
