# Tariff ETR Calculator

Calculate effective tariff rate (ETR) changes on U.S. imports by trading partner and GTAP sector under various tariff policy scenarios. **All ETR values represent changes from an early 2025 baseline.**

## Overview

This project analyzes U.S. import trade data to compute effective tariff rate changes incorporating:
- **Section 232 tariffs**: Steel, aluminum, softwood lumber, furniture, automobiles, and auto parts
- **IEEPA Reciprocal tariffs**: Broad-based tariffs (mutually exclusive with Section 232)
- **IEEPA Fentanyl tariffs**: Targeted tariffs that stack on top of other authorities for China, mutually exclusive otherwise
- **USMCA exemptions**: Trade agreement provisions with content requirements

The analysis produces sector-level ETR changes by trading partner and aggregated overall ETR changes. Overall ETRs are calculated using two weighting methods: (1) GTAP import weights and (2) 2024 Census import totals.

## Repository Structure

```
Tariff-ETRs/
├── src/
│   ├── main.R              # Main execution script
│   ├── config_parsing.R    # Config loading and rate expansion
│   ├── data_processing.R   # Census data processing
│   └── calculations.R      # ETR calculations and aggregation
├── config/
│   └── {scenario}/         # Scenario-specific configuration
│       ├── 232.yaml        # Section 232 tariff parameters (country-level)
│       ├── ieepa_reciprocal.yaml  # IEEPA reciprocal tariffs (country-level, hierarchical)
│       ├── ieepa_fentanyl.yaml    # IEEPA fentanyl tariffs (country-level, hierarchical)
│       └── other_params.yaml # Other scenario parameters
├── resources/
│   ├── hs10_gtap_crosswalk.csv       # HTS10 to GTAP sector mapping
│   ├── country_partner_mapping.csv   # Country → partner group mapping (51 countries)
│   ├── usmca_shares.csv              # USMCA-qualifying trade shares
│   └── gtap_import_weights.csv       # Import weights for aggregation
├── cache/
│   └── hs10_by_country_gtap_2024_con.rds  # Cached country-level import data
├── output/
│   └── {scenario}/         # Scenario-specific output
│       ├── shocks.txt      # GTAP shock commands
│       ├── etrs_by_sector_country.csv  # ETR matrix (sector × partner)
│       ├── etrs_by_census_country.csv  # Overall ETRs by census country code (2-column CSV)
│       └── overall_etrs.txt # Overall ETRs by country (both weighting methods)
└── README.md
```

## Requirements

- R (version 4.0+)
- R packages:
  - `tidyverse`
  - `yaml`
- U.S. import data files (fixed-width format, 2024)

## Data Sources

**Import Data:**
- Source: [U.S. Census Bureau - Merchandise Trade Imports](https://www.census.gov/foreign-trade/data/IMDB.html)
- Format: Monthly ZIP files (IMDByymm.ZIP) containing IMP_DETL.TXT fixed-width format files
- Location: Download files and place in a local directory, then update the `import_data_path` parameter in `do_scenario()` calls (default: `C:/Users/{username}/Downloads/`)
- Structure:
  - Columns: HTS10 code (10-digit), country code, year, month, import value (consumption and general)
  - Aggregated by HTS10 commodity and trading partner

**Country Codes and Architecture:**
The project uses Census Bureau country codes (not ISO codes). **Tariff rates are specified at the country level**, and all calculations happen at the HS10×country level before aggregating to 8 partner groups for GTAP output:

- China: `5700` → `china`
- Canada: `1220` → `canada`
- Mexico: `2010` → `mexico`
- Japan: `5880` → `japan`
- UK: `4120` → `uk`
- EU-27: 27 individual country codes → `eu` (see `resources/country_partner_mapping.csv`)
- Free Trade ROW: Australia, Korea, Singapore, Chile, etc. → `ftrow`
- All unmapped countries (183 total, including Vietnam, India, Brazil): → `row` (Rest of World)

**Important**: Config files specify rates at the country level with `default` rates for efficiency. Unmapped countries automatically receive the default rate specified in each config file.

**GTAP-HTS Crosswalk**
We map HTS10 codes to GTAP sectors using a crosswalk derived from the [6-digit crosswalk developed by Angel Aguiar](https://www.gtap.agecon.purdue.edu/resources/res_display.asp?RecordID=5111). Each HTS10 code inherits its GTAP sector from its 6-digit prefix. 


## Usage

### Running Scenarios

1. **Set the scenarios** in `src/main.R`:
   ```r
   scenarios <- c('10_30', '10_30_ex_ieepa') 
   ```

2. **Ensure config files exist** in `config/{scenario}/`:
   - `232.yaml` - Section 232 tariff rates and HTS code definitions (variable length: 4, 6, 8, or 10 digits)
   - `ieepa_reciprocal.yaml` - IEEPA reciprocal tariffs with hierarchical structure
   - `ieepa_fentanyl.yaml` - IEEPA fentanyl tariffs with hierarchical structure

3. **Run the analysis**:
   ```r
   source('src/main.R')
   ```

### Creating a New Scenario

1. Copy an existing scenario folder:
   ```bash
   cp -r config/baseline config/my_scenario
   ```

2. Modify the config files in `config/my_scenario/`

3. Update `scenarios` variable in `main.R`:
   ```r
   scenario <- 'my_scenario'
   ```

4. Run the analysis

## Configuration

### Section 232 Tariffs (`232.yaml`)

Defines tariff rates and product coverage for each Section 232 tariff at the **country level** using Census Bureau country codes. Codes can be specified at any level of granularity (4, 6, 8, or 10 digits) and use prefix matching - e.g., `'8703'` matches all HS10 codes starting with 8703.

```yaml
steel:
  base:
    - '73012010'  # 8-digit code: matches all HTS10 codes starting with 73012010
    - '73012050'
    - '73023000'
    - '7307'      # 4-digit code: matches all HTS10 codes starting with 7307
    # ... additional codes
  rates:
    default: 0.50    # Default rate for all countries (including 183 unmapped countries)
    '4120': 0.25     # UK-specific override (Census country code 4120)
    # Only specify countries that differ from default
  usmca_exempt: 0    # 1 = USMCA exemption applies, 0 = no exemption
```

### IEEPA Rates (`ieepa_reciprocal.yaml` and `ieepa_fentanyl.yaml`)

Both IEEPA tariff types use identical YAML structure with hierarchical rates at the **country level**:

1. **Headline rates**: Default rate for each country (applied to all HTS10 codes)
2. **Product rates**: Override headline rates for specific HTS codes
3. **Product × country rates**: Most specific override for HTS code and country combinations

```yaml
headline_rates:
  default: 0.1     # Default for unmapped countries (183 countries)
  '5700': 0.2      # China (Census code 5700)
  '1220': 0.35     # Canada
  '2010': 0.25     # Mexico
  # ... other country codes from country_partner_mapping.csv

product_rates:
  '8703': 0.0     # Simple rate: applies to all countries
  '2939': 0.15    # Fentanyl precursors

product_country_rates:
  - hts_codes: ['87032201', '87032301']
    cty_code: '5700'
    rate: 0.50
```

**Stacking Logic**:
- **Reciprocal**: Mutually exclusive with Section 232 (applies only to uncovered base)
- **Fentanyl**:
  - China (5700): STACKS on top of whichever is higher (232 OR reciprocal) + fentanyl
    - Formula: `max(232, reciprocal) + fentanyl`
    - Example: If 232=50%, reciprocal=10%, fentanyl=10% → Final rate = 50% + 10% = 60%
  - All others: Only applies to base not covered by 232 or reciprocal (mutually exclusive)

**HTS Code Matching**: Uses prefix matching like Section 232 tariffs - `'8703'` matches all HST10 codes starting with 8703. Codes can be 4, 6, 8, or 10 digits.

**Default Rates**: Unmapped countries (those not in `country_partner_mapping.csv`) automatically receive the `default` rate at each level of the hierarchy.


## Output

### Shock Commands (`output/{scenario}/shocks.txt`)

GTAP-formatted shock commands for economic modeling:

```
Shock tms("steel","China","USA") = 50.0;
Shock tms("steel","Canada","USA") = 45.2;
...
```

### ETR Matrix by Sector and Partner (`output/{scenario}/etrs_by_sector_country.csv`)

Wide-format CSV with ETRs (in percentage points) for each GTAP sector (rows) and partner group (columns). Partner groups are the 8 aggregated regions: china, canada, mexico, japan, uk, eu, row, ftrow.

### Overall ETRs by Census Country (`output/{scenario}/etrs_by_census_country.csv`)

Simple two-column CSV with overall ETR for each Census Bureau country code, weighted by 2024 census imports:

```
cty_code,etr
5700,25.30
1220,10.15
2010,8.45
...
```

- **Column 1**: Census country code (5700 = China, 1220 = Canada, etc.)
- **Column 2**: Overall ETR in percentage points (aggregated across all sectors)
- **Weights**: 2024 census import values
- **Sorting**: Descending by ETR (highest tariff countries first)

### Overall ETRs (`output/{scenario}/overall_etrs.txt`)

Overall ETR changes by country using both GTAP weights and 2024 Census import weights (change from early 2025 baseline):

```
Overall ETRs by Country (change from early 2025 baseline):
==========================================================

Country     GTAP Weights  2024 Census Weights
-------     ------------  -------------------
CHINA           25.30%              26.15%
ROW             18.50%              17.85%
FTROW           12.40%              11.92%
...

TOTAL           20.45%              20.78%
```

This output is both displayed in the console and saved to `output/{scenario}/overall_etrs.txt`.

## Trading Partner Groups

- **China**: Mainland China
- **Canada**: Canada
- **Mexico**: Mexico
- **Japan**: Japan
- **UK**: United Kingdom
- **EU**: European Union (27 member states, excluding UK)
- **FTROW**: Free Trade Rest of World (countries with whom US has a FTA)
- **ROW**: Rest of World (all other countries)


## How It Works

The calculation pipeline uses a **multi-authority rate matrix** approach for flexibility and extensibility:

1. **Load Configuration**: Config parsers return clean tabular data:
   - Section 232: Complete HTS10×country tibble with one column per tariff (`s232_steel_rate`, `s232_aluminum_rate`, etc.)
   - IEEPA: Complete HTS10×country tibble with single column (`ieepa_rate`)
   - All rates fully expanded for complete universe of HST10 codes × 240 countries

2. **Process Import Data**: Read Census Bureau IMP_DETL.TXT files from monthly ZIP archives and aggregate by HTS10 × country

3. **Build Rate Matrix**: Join config tables with import data to create master rate matrix with all tariff authority columns

4. **Apply Adjustments**: Apply USMCA exemptions and auto rebates to each rate column independently

5. **Apply Stacking Rules**: Calculate final rate using authority-specific logic:
   - Current rule: `final_rate = max(all 232 rates) OR ieepa_rate` (mutually exclusive)
   - Future: Easy to modify for stacking tariffs (e.g., "IEEPA fentanyl stacks on 232")

6. **Calculate ETRs**: ETR = final_rate at HTS10×country level (relative to early 2025 baseline)

7. **Aggregate to Partners**: Convert country-level ETRs to 8 partner groups using import-weighted averaging

8. **Generate Output**: Write GTAP shock commands and calculate overall ETR changes

## Key Features

- **Multi-Authority Rate Matrix**: Clean separation between config parsing (returns tabular data) and calculations (applies stacking rules)
- **Flexible Tariff Stacking**: Easy to modify stacking rules to support multiple IEEPA types or tariffs that stack on top of each other
- **Country-Level Architecture**: Tariff rates specified at country level (Census codes) with default rates for efficiency
- **Automatic Default Handling**: 183 unmapped countries automatically receive default rates from config files
- **Tabular Config Interface**: Config parsers return complete HTS10×country tibbles, not nested lists - calculations consume clean tables
- **Scenario-based**: Easy comparison of different tariff policy configurations
- **Hierarchical IEEPA Configuration**: Flexible rate specification with headline, product, and product×country levels
- **Variable-Length HTS Matching**: Supports 4-, 6-, 8-, and 10-digit HTS codes with prefix matching for both 232 and IEEPA tariffs
- **Modular Code**: Separate files for config parsing, data processing, and calculations
- **USMCA Compliance**: Automatically adjusts rates based on qualifying trade shares
- **Auto Sector Logic**: Special handling for automobile and auto parts tariffs (including rebates)
- **Dual Weighting**: Overall ETRs calculated using both GTAP weights and 2024 Census import totals for comparison
- **GTAP Compatible**: Country-level calculations aggregate to 8 partner groups for economic modeling

## Contact

Email: john.ricco@yale.edu
Twitter: @riccoja 
