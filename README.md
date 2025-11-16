# Tariff ETR Calculator

Calculate effective tariff rate (ETR) changes on U.S. imports by trading partner and GTAP sector under various tariff policy scenarios. **All ETR values represent changes from an early 2025 baseline.**

## Overview

This project analyzes U.S. import trade data to compute effective tariff rate changes incorporating:
- **Section 232 tariffs**: Steel, aluminum, softwood lumber, furniture, automobiles, and auto parts
- **IEEPA tariffs**: Residual catch-all rates for imports not covered by Section 232
- **USMCA exemptions**: Trade agreement provisions with content requirements
- **Country-specific adjustments**: Korea and Vietnam receive separate treatment within broader country groupings

The analysis produces sector-level ETR changes by trading partner and aggregated overall ETR changes. Overall ETRs are calculated using two weighting methods: (1) GTAP import weights and (2) 2024 Census import totals.

## Repository Structure

```
Tariff-ETRs/
├── src/
│   ├── main.R              # Main execution script
│   └── functions.R         # Helper functions
├── config/
│   └── {scenario}/         # Scenario-specific configuration
│       ├── 232.yaml        # Section 232 tariff parameters
│       ├── ieepa_rates.yaml # IEEPA tariff rates (hierarchical)
│       └── other_params.yaml # Other scenario parameters
├── resources/
│   ├── hs10_gtap_crosswalk.csv     # HS10 to GTAP sector mapping
│   ├── usmca_shares.csv            # USMCA-qualifying trade shares
│   └── gtap_import_weights.csv    # Import weights for aggregation
├── output/
│   └── {scenario}/         # Scenario-specific output
│       ├── shocks.txt      # GTAP shock commands
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
  - Columns: HS10 code (10-digit), country code, year, month, import value (consumption and general)
  - Aggregated by HS10 commodity and trading partner

**Country Codes:**
The project uses Census Bureau country codes (not ISO codes):
- China: `5700`
- Canada: `1220`
- Mexico: `2010`
- Japan: `5880`
- UK: `4120`
- EU-27: 27 individual country codes (see `main.R` for complete list)
- All others: Classified as "ROW" (Rest of World)

**GTAP-HS Crosswalk**
We map HS10 codes to GTAP sectors using a crosswalk derived from the [6-digit crosswalk developed by Angel Aguiar](https://www.gtap.agecon.purdue.edu/resources/res_display.asp?RecordID=5111). Each HS10 code inherits its GTAP sector from its 6-digit prefix. 


## Usage

### Running Scenarios

1. **Set the scenarios** in `src/main.R`:
   ```r
   scenarios <- c('10_30', '10_30_ex_ieepa') 
   ```

2. **Ensure config files exist** in `config/{scenario}/`:
   - `232.yaml` - Section 232 tariff rates and HTS code definitions (variable length: 4, 6, 8, or 10 digits)
   - `ieepa_rates.yaml` - IEEPA rates with hierarchical structure (headline, product, product×country)

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

Defines tariff rates and product coverage for each Section 232 tariff. Codes can be specified at any level of granularity (4, 6, 8, or 10 digits) and use prefix matching - e.g., `'8703'` matches all HS10 codes starting with 8703.

```yaml
steel:
  base:
    - '73012010'  # 8-digit code: matches all HS10 codes starting with 73012010
    - '73012050'
    - '73023000'
    - '7307'      # 4-digit code: matches all HS10 codes starting with 7307
    # ... additional codes
  rate:
    china: 0.50
    canada: 0.50
    mexico: 0.50
    japan: 0.50
    uk: 0.25
    eu: 0.50
    row: 0.50
    ftrow: 0.50
  usmca_exempt: 0  # 1 = USMCA exemption applies, 0 = no exemption
```

### IEEPA Rates (`ieepa_rates.yaml`)

Specifies IEEPA tariff rates using a hierarchical structure with three levels of specificity:

1. **Headline rates**: Default rate for each trading partner (applied to all GTAP sectors)
2. **Product rates**: Override headline rates for specific HTS codes
3. **Product × country rates**: Most specific override for HTS code and partner combinations

```yaml
headline_rates:
  china: 0.2
  canada: 0.35
  mexico: 0.25
  japan: 0.15
  uk: 0.1
  eu: 0.15
  row: 0.1
  ftrow: 0.1

product_rates:
  - hts_codes: ['8703', '8704']  # Vehicles - variable length like 232 tariffs
    rate: 0.30
    partners: all  # Can be 'all' or list like [china, eu, row]

  - hts_codes: ['7601']  # Aluminum
    rate: 0.15
    partners: [canada, mexico]

product_country_rates:
  - hts_codes: ['8703']  # Passenger vehicles
    partner: china
    rate: 0.50

  - hts_codes: ['270900']  # Petroleum oils
    partner: canada
    rate: 0.00
```

**HTS Code Matching**: Uses prefix matching like Section 232 tariffs - `'8703'` matches all HS10 codes starting with 8703. Codes can be 4, 6, 8, or 10 digits.

**Precedence**: Product × country rates override product rates, which override headline rates.


## Output

### Shock Commands (`output/{scenario}/shocks.txt`)

GTAP-formatted shock commands for economic modeling:

```
Shock tms("steel","China","USA") = 50.0;
Shock tms("steel","Canada","USA") = 45.2;
...
```

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
- **ROW**: Rest of World (all other countries, including Vietnam)
- **FTROW**: Free Trade Rest of World (subset of ROW, including Korea)

## How It Works

1. **Load Configuration**: Read scenario-specific tariff parameters from `config/{scenario}/`
2. **Process Import Data**: Read Census Bureau IMP_DETL.TXT files from monthly ZIP archives and aggregate by HS10 and trading partner
3. **Map to GTAP**: Convert HS10 codes to GTAP sectors using crosswalk
4. **Calculate Tax Bases**: Determine import shares subject to each tariff by partner and sector using prefix matching on variable-length HTS codes
5. **Compute ETR Changes**: Apply tariff rates with USMCA exemptions, auto rebates, and country-specific adjustments (relative to early 2025 baseline)
6. **Generate Output**: Write GTAP shock commands and calculate overall ETR changes

## Key Features

- **Scenario-based**: Easy comparison of different tariff policy configurations
- **Hierarchical IEEPA Configuration**: Flexible rate specification with headline, product, and product×country levels
- **Variable-Length HTS Matching**: Supports 4-, 6-, 8-, and 10-digit HTS codes with prefix matching for both 232 and IEEPA tariffs
- **Modular Code**: Separate functions file for maintainability
- **USMCA Compliance**: Automatically adjusts rates based on qualifying trade shares
- **Auto Sector Logic**: Special handling for automobile and auto parts tariffs
- **Dual Weighting**: Overall ETRs calculated using both GTAP weights and 2024 Census import totals for comparison

## Contact

Email: john.ricco@yale.edu
Twitter: @riccoja 
