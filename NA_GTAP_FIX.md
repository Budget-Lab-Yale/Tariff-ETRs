# Fix for NA gtap_code in shocks.txt

## Problem

The shocks.txt file contained an invalid line:
```
Shock tms("NA","Canada","USA") = 35.0;
```

This occurred because some HS10 codes in the import data don't have corresponding GTAP sector mappings in the crosswalk file.

## Root Cause

1. Import data contains HS10 codes that aren't in the `hs10_gtap_crosswalk.csv` file
2. When joining import data with the crosswalk (calculations.R:113-119), these codes get `NA` for `gtap_code`
3. These NA values propagated through all calculations
4. The `write_shock_commands()` function filtered for `etr_pct > 0` but didn't filter out NA gtap_codes
5. Result: `sprintf()` at line 521 output "NA" as a literal string in the GTAP command

## Solution

Applied three-layer defense:

### 1. Filter at data loading (calculations.R:89, 125)
Added filter to remove rows with NA gtap_code when loading cached data or building from raw:
```r
filter(!is.na(gtap_code))
```

This prevents unmapped HS10 codes from entering calculations at all.

### 2. Filter at output (calculations.R:518)
Added NA check when generating shock commands:
```r
filter(etr_pct > 0, !is.na(gtap_code))
```

This provides a safety net in case any NA values slip through.

### 3. Existing filter in write_sector_country_etrs (calculations.R:588)
The CSV output function already filters to only known GTAP sectors, so it was unaffected.

## Impact

- Unmapped HS10 codes are now excluded from all ETR calculations
- Import value from unmapped codes is effectively dropped
- This is the correct behavior: if we don't know what GTAP sector an HS10 code belongs to, we can't include it in sector-level ETR calculations

## Testing

Run `test_na_fix.R` to verify:
1. No NA values in shocks.txt
2. No NA gtap_codes in output data

Note: You'll need to delete the cache file (`cache/hs10_by_country_gtap_2024_con.rds`) or run with `use_cache = FALSE` for the data loading filters to take effect on existing cached data.
