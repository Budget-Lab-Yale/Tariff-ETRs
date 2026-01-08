# =============================================================================
# extract_hts_from_pdf.R
# =============================================================================
#
# Extract HTS codes from Federal Register PDF documents.
#
# Usage:
#   Rscript scripts/extract_hts_from_pdf.R <pdf_path> <output_dir>
#
# Example:
#   Rscript scripts/extract_hts_from_pdf.R docs/FR_2025-21940.pdf output/hts_lists/
#
# Output:
#   - civil_aircraft_hts.csv: Civil aircraft HTS codes
#   - civil_aircraft_hts.yaml: YAML-ready block for config files
#
# Dependencies:
#   install.packages('pdftools')
#   install.packages('stringr')
# =============================================================================

library(pdftools)
library(stringr)
library(yaml)

# -----------------------------------------------------------------------------
# Parse command line arguments
# -----------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 2) {
  message('Usage: Rscript extract_hts_from_pdf.R <pdf_path> <output_dir>')
  message('')
  message('This script extracts HTS codes from Federal Register PDF documents.')
  message('The PDF should contain HTS codes in a recognizable format (e.g., 8802.11.00)')
  quit(status = 1)
}

pdf_path <- args[1]
output_dir <- args[2]

if (!file.exists(pdf_path)) {
  stop(sprintf('PDF file not found: %s', pdf_path))
}

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

message(sprintf('Processing PDF: %s', pdf_path))
message(sprintf('Output directory: %s', output_dir))

# -----------------------------------------------------------------------------
# Extract text from PDF
# -----------------------------------------------------------------------------
message('\nExtracting text from PDF...')
pdf_text <- pdf_text(pdf_path)
full_text <- paste(pdf_text, collapse = '\n')

message(sprintf('  Extracted %d pages', length(pdf_text)))

# -----------------------------------------------------------------------------
# Extract HTS codes using regex patterns
# -----------------------------------------------------------------------------
# Pattern matches various HTS formats:
# - 8802.11.00 (with dots)
# - 8802110000 (10-digit no dots)
# - 880211 (6-digit)
# - 8802 (4-digit heading)

extract_hts_codes <- function(text, pattern_name, pattern) {
  matches <- str_extract_all(text, pattern)[[1]]
  matches <- unique(matches)

  # Normalize: remove dots and leading zeros, keep consistent format
  normalized <- gsub('\\.', '', matches)
  normalized <- unique(normalized)

  message(sprintf('  %s: found %d unique codes', pattern_name, length(normalized)))

  return(normalized)
}

# Pattern for HTS codes (various formats)
hts_pattern <- '\\b[0-9]{4}(\\.?[0-9]{2}){0,3}\\b'

all_hts <- extract_hts_codes(full_text, 'All HTS codes', hts_pattern)

# -----------------------------------------------------------------------------
# Filter for civil aircraft (Chapter 88) and engines (8411)
# -----------------------------------------------------------------------------
message('\nFiltering for civil aircraft codes...')

# Civil aircraft headings:
# 8802 - Aircraft (except military/unmanned)
# 8803 - Parts of aircraft
# 8805 - Flight simulators
# 8411 - Turbojets, turbopropellers (aircraft engines)
# NOT 8806 - Unmanned aircraft (explicitly excluded)

civil_aircraft_prefixes <- c('8802', '8803', '8805', '8411')
excluded_prefixes <- c('8806')  # Unmanned aircraft

is_civil_aircraft <- function(hts) {
  # Check if starts with civil aircraft prefix
  is_aircraft <- any(sapply(civil_aircraft_prefixes, function(p) startsWith(hts, p)))
  # Exclude unmanned aircraft
  is_excluded <- any(sapply(excluded_prefixes, function(p) startsWith(hts, p)))

  return(is_aircraft && !is_excluded)
}

civil_aircraft_hts <- all_hts[sapply(all_hts, is_civil_aircraft)]
civil_aircraft_hts <- sort(unique(civil_aircraft_hts))

message(sprintf('  Found %d civil aircraft HTS codes', length(civil_aircraft_hts)))

if (length(civil_aircraft_hts) == 0) {
  warning('No civil aircraft HTS codes found! Check if PDF contains the expected annexes.')
}

# -----------------------------------------------------------------------------
# Output results
# -----------------------------------------------------------------------------
message('\nWriting output files...')

# CSV output
csv_path <- file.path(output_dir, 'civil_aircraft_hts.csv')
write.csv(
  data.frame(hts = civil_aircraft_hts),
  csv_path,
  row.names = FALSE
)
message(sprintf('  Wrote: %s', csv_path))

# YAML-ready output
yaml_path <- file.path(output_dir, 'civil_aircraft_hts.yaml')
yaml_content <- list(
  name = 'korea_civil_aircraft_extracted',
  country = '5800',
  rate = 0,
  hts = civil_aircraft_hts
)
write_yaml(yaml_content, yaml_path)
message(sprintf('  Wrote: %s', yaml_path))

# Summary text output
txt_path <- file.path(output_dir, 'extraction_summary.txt')
writeLines(c(
  sprintf('HTS Extraction Summary'),
  sprintf('======================'),
  sprintf('PDF: %s', pdf_path),
  sprintf('Date: %s', Sys.time()),
  sprintf(''),
  sprintf('Total HTS codes found: %d', length(all_hts)),
  sprintf('Civil aircraft codes: %d', length(civil_aircraft_hts)),
  sprintf(''),
  sprintf('Civil aircraft prefixes searched: %s', paste(civil_aircraft_prefixes, collapse = ', ')),
  sprintf('Excluded prefixes: %s', paste(excluded_prefixes, collapse = ', ')),
  sprintf(''),
  sprintf('Extracted HTS codes:'),
  paste('  -', civil_aircraft_hts)
), txt_path)
message(sprintf('  Wrote: %s', txt_path))

message('\nExtraction complete!')
message(sprintf('Civil aircraft HTS codes: %d', length(civil_aircraft_hts)))
