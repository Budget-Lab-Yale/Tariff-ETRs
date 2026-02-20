# Tariff ETR Calculator

Calculate effective tariff rate (ETR) changes on U.S. imports by trading partner and GTAP sector under various tariff policy scenarios. **All ETR values represent changes from an early 2025 baseline.**

## Overview

This project analyzes U.S. import trade data to compute effective tariff rate changes incorporating:
- **Section 232 tariffs**: Steel, aluminum, softwood lumber, furniture, automobiles, and auto parts
- **IEEPA Reciprocal tariffs**: Broad-based tariffs (mutually exclusive with Section 232)
- **IEEPA Fentanyl tariffs**: Targeted tariffs that stack for China, mutually exclusive otherwise
- **USMCA exemptions**: Trade agreement provisions with content requirements

## Repository Structure

```
Tariff-ETRs/
├── src/
│   ├── main.R              # Main execution script
│   ├── config_parsing.R    # Config loading and rate expansion
│   ├── data_processing.R   # Census data processing
│   └── calculations.R      # ETR calculations and aggregation
├── config/
│   ├── {scenario}/         # Static scenario configuration
│   │   ├── 232.yaml              # Section 232 tariff parameters
│   │   ├── ieepa_reciprocal.yaml # IEEPA reciprocal tariffs
│   │   ├── ieepa_fentanyl.yaml   # IEEPA fentanyl tariffs
│   │   └── other_params.yaml     # USMCA and auto rebate parameters
│   └── {scenario}/         # Time-varying scenario (dated subfolders)
│       ├── 2025-02-04/           # Each date has a complete config set
│       │   ├── 232.yaml
│       │   ├── ieepa_reciprocal.yaml
│       │   ├── ieepa_fentanyl.yaml
│       │   └── other_params.yaml
│       └── 2025-04-02/
│           └── ...
├── resources/
│   ├── hs10_gtap_crosswalk.csv       # HTS10 to GTAP sector mapping
│   ├── country_partner_mapping.csv   # Census country code → partner group
│   ├── census_codes.csv              # Census country codes and names
│   ├── usmca_shares.csv              # USMCA-qualifying trade shares
│   └── gtap_import_weights.csv       # Import weights for aggregation
├── cache/
│   └── hs10_by_country_gtap_2024_con.rds  # Cached import data
├── output/
│   ├── {static-scenario}/
│   │   ├── shocks.txt                     # GTAP shock commands
│   │   ├── etrs_by_sector_country.csv     # ETR matrix (sector × partner)
│   │   ├── etrs_by_census_country.csv     # Overall ETRs by census country
│   │   ├── etrs_by_census_country_hts2.csv # ETRs by country × HTS chapter
│   │   └── overall_etrs.txt               # Summary statistics
│   └── {time-varying-scenario}/
│       ├── 2025-02-04/shocks.txt          # Per-date shock commands
│       ├── 2025-04-02/shocks.txt
│       ├── etrs_by_sector_country.csv     # Stacked CSV (date column first)
│       ├── etrs_by_census_country.csv     # Stacked CSV (date column first)
│       ├── etrs_by_census_country_hts2.csv # Stacked CSV (date column first)
│       └── overall_etrs.txt               # Combined with per-date sections
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
- Location: Update `import_data_path` in `main.R` (default: `C:/Users/jar335/Downloads`)

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
| ftrow | FTA partners (Australia, Korea, Singapore, etc.) |
| row | All unmapped countries (183 total) |

See `resources/country_partner_mapping.csv` for the complete mapping.

**GTAP Crosswalk:**
HTS10 codes map to GTAP sectors via a crosswalk derived from [Angel Aguiar's 6-digit crosswalk](https://www.gtap.agecon.purdue.edu/resources/res_display.asp?RecordID=5111).

## Usage

### Running Scenarios

1. Set scenarios in `src/main.R`:
   ```r
   scenarios <- c('10-30', '11-17')
   ```

2. Ensure config files exist in `config/{scenario}/`

3. Run:
   ```r
   source('src/main.R')
   ```

### Creating a New Scenario

1. Copy an existing scenario:
   ```bash
   cp -r config/11-17 config/my_scenario
   ```

2. Modify the config files in `config/my_scenario/`

3. Add to `scenarios` in `main.R` and run

### Time-Varying Scenarios

To model tariff rates that change over time, create dated subfolders (YYYY-MM-DD) inside a scenario directory. Each subfolder must contain a complete set of config files:

```bash
config/tariff-timeline/
  2025-02-04/
    232.yaml
    ieepa_reciprocal.yaml
    ieepa_fentanyl.yaml
    other_params.yaml
  2025-04-02/
    232.yaml
    ieepa_reciprocal.yaml
    ieepa_fentanyl.yaml
    other_params.yaml
```

Detection is automatic: if a scenario directory contains YYYY-MM-DD subfolders, it runs as time-varying. The same 2024 import weights are reused across all dates. Outputs:
- **Shock commands**: Per-date subfolders (`output/scenario/2025-02-04/shocks.txt`)
- **CSV files**: Single stacked file with `date` as the first column
- **overall_etrs.txt**: Combined file with per-date sections

## Configuration

### Section 232 Tariffs (`232.yaml`)

Defines tariff rates and product coverage using variable-length HTS codes (4, 6, 8, or 10 digits) with prefix matching.

```yaml
steel:
  base:
    - '73012010'  # 8-digit: matches HTS10 codes starting with 73012010
    - '7307'      # 4-digit: matches all HTS10 codes starting with 7307
  rates:
    default: 0.50   # Default rate for all countries
    '4120': 0.25    # UK-specific override
  usmca_exempt: 0   # 1 = USMCA exemption applies
```

### IEEPA Rates (`ieepa_reciprocal.yaml`, `ieepa_fentanyl.yaml`)

Hierarchical rate structure with three levels of specificity:

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
```

**Stacking Rules:**
- **Section 232**: Takes precedence over IEEPA when applicable
- **IEEPA Reciprocal**: Applies to imports not covered by Section 232
- **IEEPA Fentanyl**:
  - *China*: Stacks on top (232 or reciprocal, whichever applies, plus fentanyl)
  - *Others*: Only applies if neither 232 nor reciprocal covers the import

## Output

### Shock Commands (`shocks.txt`)

GTAP-formatted commands:
```
Shock tms("i_s","China","USA") = 50.0;
Shock tms("nfm","China","USA") = 50.0;
```

### ETR Matrix (`etrs_by_sector_country.csv`)

Wide-format CSV with ETRs in percentage points. Rows are GTAP sectors, columns are partner groups.

### Country-Level ETRs (`etrs_by_census_country.csv`)

Overall ETR for each census country, weighted by 2024 imports:

```csv
cty_code,country_name,etr
5700,China,24.02
1220,Canada,8.45
```

Sorted by ETR descending.

### Country × HTS Chapter ETRs (`etrs_by_census_country_hts2.csv`)

ETRs by country and 2-digit HTS chapter. Countries as rows, chapters (01-97) as columns.

### Summary Statistics (`overall_etrs.txt`)

Overall ETRs by partner using both GTAP weights and 2024 Census import weights, plus tariff coverage statistics.

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

1. **Load Config**: Parse YAML files into HTS10×country rate tables
2. **Load Imports**: Read Census IMP_DETL.TXT files, aggregate by HTS10×country
3. **Build Rate Matrix**: Join config tables with import data
4. **Apply Adjustments**: USMCA exemptions and auto rebates
5. **Apply Stacking**: Calculate final rate per HTS10×country using authority precedence
6. **Aggregate**: Roll up to partner×GTAP level using import-weighted averaging
7. **Output**: Write shock commands, CSVs, and summary statistics

## Contact

Email: john.ricco@yale.edu
Twitter: @riccoja
