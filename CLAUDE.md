# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an R-based data analysis project for processing U.S. import trade data. The project analyzes imports by HS6 commodity codes and trading partners using fixed-width format (FWF) data files from the Census Bureau.

## Data Architecture

**Input Data:**
- Fixed-width format files matching pattern `dporths6ir24` (2024 data)
- Files are read from `C:/Users/jar335/Downloads`
- Column structure:
  - `commodity` (positions 1-6): HS6 commodity code
  - `cty_code` (positions 7-10): Country code
  - `port_code` (positions 11-14): Port of entry code
  - `year` (positions 15-18): Year
  - `month` (positions 19-20): Month
  - `value_mo` (positions 21-35): Monthly trade value in dollars

**Country Code Mappings:**
The project uses specific Census Bureau country codes (not ISO codes) to identify trading partners:
- China: 5700
- Canada: 1220
- Mexico: 2010
- UK: 4120
- Japan: 5880
- EU-27: 27 individual country codes (see main.R:14-42 for complete list)
- All others classified as "ROW" (Rest of World)

**Output:**
- Aggregated data by HS6 commodity and trading partner
- Total import values for 2024

## Development Commands

**Run the analysis:**
```r
source("main.R")
```

**Interactive development in RStudio:**
- Open `Tariff-ETRs.Rproj` to load the project with correct settings
- The project uses 2 spaces for indentation (configured in .Rproj file)

## Key Implementation Notes

- The main processing pipeline uses tidyverse patterns: `map_df()` to read and combine multiple files, then `mutate()` and `group_by()` for aggregation
- Country groupings are defined at the top of main.R and should be updated there if partner definitions change
- The `partner` classification uses a cascading `case_when()` that checks codes in order (lines 76-84), so order matters
- File path is currently hardcoded to a specific Windows user directory and will need updating for different environments

## Style
- never use na.rm = T or any other kind of na filters. if we have missings that's a sign that som
thing is wrong and it shoudl break accordingly!
- use single quotes for strings
