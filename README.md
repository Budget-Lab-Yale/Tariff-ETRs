# Tariff ETR Calculator

Calculate effective tariff rates (ETRs) on U.S. imports by trading partner and GTAP sector under various tariff policy scenarios.

## Overview

This project analyzes U.S. import trade data to compute effective tariff rates incorporating:
- **Section 232 tariffs**: Steel, aluminum, softwood lumber, furniture, automobiles, and auto parts
- **IEEPA tariffs**: Residual catch-all rates for imports not covered by Section 232
- **USMCA exemptions**: Trade agreement provisions with content requirements
- **Country-specific adjustments**: Korea and Vietnam receive separate treatment within broader country groupings

The analysis produces sector-level ETRs by trading partner and aggregated overall ETRs weighted by GTAP import shares.

## Repository Structure

```
Tariff-ETRs/
├── src/
│   ├── main.R              # Main execution script
│   └── functions.R         # Helper functions
├── config/
│   └── {scenario}/         # Scenario-specific configuration
│       ├── 232.yaml        # Section 232 tariff parameters
│       └── ieepa_rates.csv # IEEPA tariff rates by sector
├── resources/
│   ├── hs6_gtap_crosswalk.csv      # HS6 to GTAP sector mapping
│   ├── usmca_shares.csv            # USMCA-qualifying trade shares
│   └── gtap_import_weights.csv    # Import weights for aggregation
├── output/
│   └── {scenario}/         # Scenario-specific output
│       └── shocks.txt      # GTAP shock commands
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
- Source: U.S. Census Bureau trade data
- Format: Fixed-width format files matching pattern `dporths6ir24*`
- Location: Place files in `C:/Users/{username}/Downloads/` or update path in `main.R`
- Structure:
  - Columns: HS6 code, country code, port code, year, month, import value
  - Aggregated by HS6 commodity and trading partner

**Country Codes:**
The project uses Census Bureau country codes (not ISO codes):
- China: `5700`
- Canada: `1220`
- Mexico: `2010`
- Japan: `5880`
- UK: `4120`
- EU-27: 27 individual country codes (see `main.R` for complete list)
- All others: Classified as "ROW" (Rest of World)

**GTAP-HS6 Crosswalk**
We map six-digit HS systems using the [crosswalk developed by Angel Aguiar](https://www.gtap.agecon.purdue.edu/resources/res_display.asp?RecordID=5111). 


## Usage

### Running a Scenario

1. **Set the scenario** in `src/main.R`:
   ```r
   scenario <- 'baseline'  # or '10_29', etc.
   ```

2. **Ensure config files exist** in `config/{scenario}/`:
   - `232.yaml` - Section 232 tariff rates and HS6 code definitions
   - `ieepa_rates.csv` - IEEPA rates by GTAP sector and trading partner

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

3. Update `scenario` variable in `main.R`:
   ```r
   scenario <- 'my_scenario'
   ```

4. Run the analysis

## Configuration

### Section 232 Tariffs (`232.yaml`)

Defines tariff rates and product coverage for each Section 232 tariff. Since official proclamations specify policy at the 10-digit level, there is necessarily some degree of judgement involved in configuring policy. We welcome feedback on our choices here. 

```yaml
steel:
  base:
    definite: [730120, 730230, ...]  # HS6 codes definitely covered
    maybe: [730721, 730722, ...]     # HS6 codes possibly covered
  rate:
    china: 0.50
    canada: 0.50
    mexico: 0.50
    japan: 0.50
    uk: 0.25
    eu: 0.50
    row: 0.50
  usmca_exempt: 0  # 1 = USMCA exemption applies, 0 = no exemption
```

### IEEPA Rates (`ieepa_rates.csv`)

Specifies IEEPA tariff rates by GTAP sector and trading partner:

```csv
gtap_code,china,canada,mexico,japan,uk,eu,row,ftrow,kr,vn
b_t,0.3,0.35,0.25,0.15,0.1,0.15,0.1,0.1,0.151,0.273
...
```

**Note**: Korea (`kr`) and Vietnam (`vn`) rates are folded into `ftrow` and `row` respectively using trade share parameters.

### Parameters

Key parameters in `main.R`:

```r
us_auto_content_share  = 0.4    # US content requirement for auto USMCA exemption
us_auto_assembly_share = 0.33   # Share of US assembly in total auto value
auto_rebate_rate       = 0.0375 # Auto assembly rebate rate
ieepa_usmca_exception  = 1      # Apply USMCA exemption to IEEPA tariffs
kr_share_ftrow         = 0.57   # Korea's share of FTROW trade
vn_share_row           = 0.94   # Vietnam's share of ROW trade
```

## Output

### Shock Commands (`output/{scenario}/shocks.txt`)

GTAP-formatted shock commands for economic modeling:

```
Shock tms("steel","China","USA") = 50.0;
Shock tms("steel","Canada","USA") = 45.2;
...
```

### Console Output

Overall ETRs by country and total:

```
Overall ETRs by Country:
========================
CHINA     : 25.30%
ROW       : 18.50%
FTROW     : 12.40%
...

Total Overall ETR: 20.45%
```

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
2. **Process Import Data**: Read Census Bureau fixed-width format files and aggregate by HS6 and trading partner
3. **Map to GTAP**: Convert HS6 codes to GTAP sectors using crosswalk
4. **Calculate Tax Bases**: Determine import shares subject to each tariff by partner and sector
5. **Compute ETRs**: Apply tariff rates with USMCA exemptions, auto rebates, and country-specific adjustments
6. **Generate Output**: Write GTAP shock commands and calculate overall ETRs

## Key Features

- **Scenario-based**: Easy comparison of different tariff policy configurations
- **Modular Code**: Separate functions file for maintainability
- **USMCA Compliance**: Automatically adjusts rates based on qualifying trade shares
- **Auto Sector Logic**: Special handling for automobile and auto parts tariffs
- **Country Folding**: Korea and Vietnam rates incorporated into broader groupings
- **Weighted Aggregation**: Overall ETRs calculated using GTAP import weights

## Contact

Email: john.ricco@yale.edu
Twitter: @riccoja 
