# =============================================================================
# build_metal_232_reform.R
# =============================================================================
#
# Parses the annex text file from the Section 232 metal content overhaul EO
# (April 6, 2026) and generates the reform s232.yaml.
#
# The EO restructures 232 tariffs into three tiers applied to full customs value:
#   Annex I-A: 50% (primary metals + promoted derivatives)
#   Annex I-B: 25% (remaining derivatives)
#   Annex III: Transitional floor of 15% total (MFN + s232 >= 15%), expires Dec 31, 2027
#   Annex II:  Removed from 232 scope
#
# UK gets reduced rates: 25% for I-A, 15% for I-B.
#
# Usage:
#   Rscript scripts/build_metal_232_reform.R [annex_text_path] [output_dir] [historical_csv]
#
# Default annex path: C:/Users/jar335/Downloads/annex.txt
# Default output dir: config/scenarios/metal_232_overhaul/reforms/metal_232
# Default historical CSV: config/historical/2026-02-20/statutory_rates.csv.gz
# =============================================================================

library(stringr)
library(yaml)
library(readr)
library(dplyr)
library(rlang)

# -----------------------------------------------------------------------------
# Paths
# -----------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
annex_path <- if (length(args) >= 1) args[1] else 'C:/Users/jar335/Downloads/annex.txt'
output_dir <- if (length(args) >= 2) args[2] else 'config/scenarios/metal_232_overhaul/reforms/metal_232'
# Historical CSV for extracting old 232 product scope (used for legacy zeroing)
historical_csv <- if (length(args) >= 3) args[3] else 'config/historical/2026-02-20/statutory_rates.csv.gz'
mfn_path <- 'resources/mfn_rates_2025.csv'

if (!file.exists(annex_path)) {
  stop(sprintf('Annex text file not found: %s', annex_path))
}

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------------
# Read the text file
# -----------------------------------------------------------------------------
message(sprintf('Reading annex text from: %s', annex_path))
lines <- readLines(annex_path, warn = FALSE)
message(sprintf('  %d lines', length(lines)))

# -----------------------------------------------------------------------------
# Section boundary detection
# -----------------------------------------------------------------------------
# We scan for annex headers and metal-type sub-headers to classify each line.

message('\nDetecting section boundaries...')

# State tracking
current_annex <- NA_character_
current_type <- NA_character_
line_sections <- vector('list', length(lines))

for (i in seq_along(lines)) {
  line <- lines[i]

  # Annex headers (take priority)
  if (str_detect(line, 'Annex I-A.*50%.*Section 232')) {
    current_annex <- 'ia'
    current_type <- NA_character_
    message(sprintf('  Line %d: Annex I-A (50%%)', i))
  } else if (str_detect(line, 'Annex I-B.*25%.*Section 232')) {
    current_annex <- 'ib'
    current_type <- NA_character_
    message(sprintf('  Line %d: Annex I-B (25%%)', i))
  } else if (str_detect(line, 'Annex II.*Removed from Scope')) {
    current_annex <- 'ii'
    current_type <- NA_character_
    message(sprintf('  Line %d: Annex II (Removed)', i))
  } else if (str_detect(line, 'Annex III.*Temporary Reduction')) {
    current_annex <- 'iii'
    current_type <- NA_character_
    message(sprintf('  Line %d: Annex III (Transitional)', i))
  } else if (str_detect(line, '^Annex IV')) {
    current_annex <- 'iv'
    current_type <- NA_character_
    message(sprintf('  Line %d: Annex IV (stop)', i))
  }

  # Metal-type sub-headers
  # Pattern: line that says just "Copper" or "Copper\nArticles" or "Steel\nDerivatives"
  # These appear as table headers: "Steel Description" or "Steel\nDerivatives Description"
  trimmed <- str_trim(line)

  # Copper Articles (only in I-A and I-B)
  if (trimmed == 'Copper') {
    # Check next line for "Articles"
    next_trimmed <- if (i < length(lines)) str_trim(lines[i + 1]) else ''
    if (str_detect(next_trimmed, '^Articles')) {
      current_type <- 'copper'
      message(sprintf('  Line %d: -> Copper Articles', i))
    }
  }

  # Aluminum primary: "Aluminum Description" (one line)
  if (str_detect(trimmed, '^Aluminum Description$') ||
      (trimmed == 'Aluminum' &&
       i < length(lines) &&
       str_detect(str_trim(lines[i + 1]), '^Description$'))) {
    # Distinguish from "Aluminum\nDerivative" by checking next line
    next_trimmed <- if (i < length(lines)) str_trim(lines[i + 1]) else ''
    if (!str_detect(next_trimmed, '^Derivative')) {
      current_type <- 'aluminum'
      message(sprintf('  Line %d: -> Aluminum (primary)', i))
    }
  }

  # Aluminum Derivatives: "Aluminum\nDerivatives" or "Aluminum\nDerivative Description"
  if (trimmed == 'Aluminum' &&
      i < length(lines) &&
      str_detect(str_trim(lines[i + 1]), '^Derivative')) {
    current_type <- 'aluminum_derivatives'
    message(sprintf('  Line %d: -> Aluminum Derivatives', i))
  }

  # Steel primary: "Steel Description" (one line, at start of annex)
  if (str_detect(trimmed, '^Steel Description$')) {
    current_type <- 'steel'
    message(sprintf('  Line %d: -> Steel (primary)', i))
  }

  # Steel Derivatives: "Steel\nDerivatives"
  if (trimmed == 'Steel' &&
      i < length(lines) &&
      str_detect(str_trim(lines[i + 1]), '^Derivative')) {
    current_type <- 'steel_derivatives'
    message(sprintf('  Line %d: -> Steel Derivatives', i))
  }

  line_sections[[i]] <- list(annex = current_annex, type = current_type)
}

# -----------------------------------------------------------------------------
# Extract HTS codes from each section
# -----------------------------------------------------------------------------
message('\nExtracting HTS codes...')

# HTS code pattern: line starting with a code (digits with optional dots)
# Must be 4+ digits. Exclude standalone page numbers (1-3 digits alone on a line).
hts_pattern <- '^\\s*([0-9]{4,10}(?:\\.[0-9]{2}){0,3})\\b'

section_codes <- list()

for (i in seq_along(lines)) {
  sec <- line_sections[[i]]
  if (is.na(sec$annex) || sec$annex == 'iv') next
  if (is.na(sec$type)) next

  match <- str_match(lines[i], hts_pattern)
  if (is.na(match[1, 2])) next

  code <- gsub('\\.', '', match[1, 2])

  # Skip page numbers (bare numbers < 4 digits that slipped through)
  if (nchar(code) < 4) next
  # Skip years
  if (code %in% c('2025', '2026', '2027', '2028', '2029')) next

  # Skip heading references in multi-line descriptions.
  # When a description wraps across lines, a heading reference like "heading\n8427"
  # can place a bare chapter number at the start of a line, fooling the regex.
  # Detect by checking if the previous line ends with "heading" or "headings".
  if (i > 1) {
    prev_trimmed <- str_trim(lines[i - 1])
    if (str_detect(prev_trimmed, '(?i)headings?\\s*$')) next
    # Also skip range patterns ("8701 to 8705") that appear in descriptions
    if (str_detect(lines[i], '^\\s*\\d{4}\\s+to\\s+\\d{4}')) next
    # Skip codes embedded in description text: if previous line is a description
    # (starts with a letter and is long) AND this line has "heading" somewhere,
    # the code is a cross-reference, not a product
    if (str_detect(prev_trimmed, '^[A-Za-z]') && nchar(prev_trimmed) > 20 &&
        str_detect(lines[i], '(?i)heading')) next
  }

  key <- paste(sec$annex, sec$type, sep = '_')
  if (is.null(section_codes[[key]])) {
    section_codes[[key]] <- character(0)
  }
  section_codes[[key]] <- c(section_codes[[key]], code)
}

# Deduplicate within each section
for (key in names(section_codes)) {
  section_codes[[key]] <- unique(section_codes[[key]])
}

# Report
message('\nExtraction summary:')
total <- 0
for (key in sort(names(section_codes))) {
  n <- length(section_codes[[key]])
  message(sprintf('  %-35s %4d codes', key, n))
  total <- total + n
}
message(sprintf('  %-35s %4d codes', 'TOTAL', total))

# -----------------------------------------------------------------------------
# Build program definitions
# -----------------------------------------------------------------------------
message('\nBuilding programs...')

UK <- '4120'

build_program <- function(codes, default_rate, uk_rate = NULL,
                          target_total_rate = NULL) {
  if (length(codes) == 0) return(NULL)

  prog <- list(
    base = sort(codes),
    rates = list(default = default_rate),
    usmca_exempt = 0
  )

  if (!is.null(target_total_rate)) {
    # Epsilon rate triggers the target_total floor logic
    prog$rates$default <- 0.0001
    if (!is.null(uk_rate)) prog$rates[[UK]] <- 0.0001
    prog$target_total <- list(default = target_total_rate)
    if (!is.null(uk_rate)) prog$target_total[[UK]] <- target_total_rate
  } else {
    if (!is.null(uk_rate)) prog$rates[[UK]] <- uk_rate
  }

  prog
}

programs <- list()

# --- Annex I-A: 50% (UK: 25% for steel/aluminum only, NOT copper) ---
ia_steel_alum_keys <- list(
  steel_ia              = 'ia_steel',
  steel_derivatives_ia  = 'ia_steel_derivatives',
  aluminum_ia           = 'ia_aluminum',
  aluminum_derivatives_ia = 'ia_aluminum_derivatives'
)
for (prog_name in names(ia_steel_alum_keys)) {
  codes <- section_codes[[ia_steel_alum_keys[[prog_name]]]]
  if (!is.null(codes) && length(codes) > 0) {
    programs[[prog_name]] <- build_program(codes, 0.50, uk_rate = 0.25)
  }
}
# Copper: no UK reduction per EO (UK discount is only for aluminum smelted/cast
# or steel melted/poured in UK)
codes <- section_codes[['ia_copper']]
if (!is.null(codes) && length(codes) > 0) {
  programs[['copper_ia']] <- build_program(codes, 0.50)
}

# --- Annex I-B: 25% (UK: 15% for steel/aluminum only, NOT copper) ---
ib_steel_alum_keys <- list(
  steel_derivatives_ib    = 'ib_steel_derivatives',
  aluminum_derivatives_ib = 'ib_aluminum_derivatives'
)
for (prog_name in names(ib_steel_alum_keys)) {
  codes <- section_codes[[ib_steel_alum_keys[[prog_name]]]]
  if (!is.null(codes) && length(codes) > 0) {
    programs[[prog_name]] <- build_program(codes, 0.25, uk_rate = 0.15)
  }
}
codes <- section_codes[['ib_copper']]
if (!is.null(codes) && length(codes) > 0) {
  programs[['copper_ib']] <- build_program(codes, 0.25)
}

# --- Annex III: Transitional (target_total = 15%) ---
iii_keys <- list(
  steel_derivatives_iii    = 'iii_steel_derivatives',
  aluminum_derivatives_iii = 'iii_aluminum_derivatives'
)
for (prog_name in names(iii_keys)) {
  codes <- section_codes[[iii_keys[[prog_name]]]]
  if (!is.null(codes) && length(codes) > 0) {
    programs[[prog_name]] <- build_program(codes, 0.0001, uk_rate = 0.0001,
                                           target_total_rate = 0.15)
  }
}

# --- Annex II exclusions ---
# The EO lists codes at mixed granularity: a 6-digit code in Annex I-B (e.g., 850300)
# means "all HS8 sub-codes under 8503.00". But some specific HS8 sub-codes under
# that prefix are explicitly removed via Annex II (e.g., 85030045, 85030090).
# Annex II takes precedence. We handle this by expanding both the active program
# codes and Annex II codes to HS10, removing overlaps, and rebuilding.

annex_ii_codes <- c(
  section_codes[['ii_steel_derivatives']] %||% character(0),
  section_codes[['ii_aluminum_derivatives']] %||% character(0)
)

if (length(annex_ii_codes) > 0) {
  message('\nApplying Annex II exclusions...')

  # Load HS10 universe from historical CSV to expand prefixes
  hs10_universe <- hist_data_raw <- read_csv(
    historical_csv, show_col_types = FALSE,
    col_select = 'hts10',
    col_types = cols(hts10 = col_character())
  ) %>% pull(hts10) %>% unique()

  # Expand Annex II codes to HS10 set
  ii_hs10 <- character(0)
  for (code in annex_ii_codes) {
    ii_hs10 <- c(ii_hs10, hs10_universe[str_starts(hs10_universe, code)])
  }
  ii_hs10 <- unique(ii_hs10)
  message(sprintf('  Annex II covers %d HS10 codes', length(ii_hs10)))

  # For each active program, check for Annex II overlaps
  active_progs <- c(
    'steel_ia', 'steel_derivatives_ia', 'aluminum_ia', 'aluminum_derivatives_ia',
    'copper_ia', 'steel_derivatives_ib', 'aluminum_derivatives_ib', 'copper_ib',
    'steel_derivatives_iii', 'aluminum_derivatives_iii'
  )

  for (prog_name in active_progs) {
    if (is.null(programs[[prog_name]])) next
    codes <- programs[[prog_name]]$base

    # Expand program codes to HS10
    prog_hs10 <- character(0)
    for (code in codes) {
      prog_hs10 <- c(prog_hs10, hs10_universe[str_starts(hs10_universe, code)])
    }
    prog_hs10 <- unique(prog_hs10)

    # Find overlap with Annex II
    overlap <- intersect(prog_hs10, ii_hs10)
    if (length(overlap) == 0) next

    message(sprintf('  %s: %d HS10 codes overlap with Annex II', prog_name, length(overlap)))

    # Identify which prefix codes have partial overlap (some sub-codes removed)
    # For each original code, check if ALL its expansions are in Annex II (full remove)
    # or only some (partial — need to split into specific HS10 codes)
    new_codes <- character(0)
    for (code in codes) {
      code_hs10 <- hs10_universe[str_starts(hs10_universe, code)]
      code_overlap <- intersect(code_hs10, ii_hs10)

      if (length(code_overlap) == 0) {
        # No overlap — keep original code as-is
        new_codes <- c(new_codes, code)
      } else if (length(code_overlap) == length(code_hs10)) {
        # Entire prefix removed by Annex II — drop this code
        message(sprintf('    Dropping %s (fully in Annex II)', code))
      } else {
        # Partial overlap — replace prefix with specific HS10 codes not in Annex II
        kept <- setdiff(code_hs10, ii_hs10)
        message(sprintf('    Splitting %s: %d kept, %d removed',
                        code, length(kept), length(code_overlap)))
        new_codes <- c(new_codes, kept)
      }
    }

    programs[[prog_name]]$base <- sort(unique(new_codes))
  }
}

# --- Suppress legacy baseline programs ---
# The overlay adds NEW annex-tier programs but doesn't touch OLD program columns
# (steel, aluminum, aluminum_derivatives, steel_derivatives). Without explicit
# zeros, legacy rates survive via pmax and Annex II/III products get wrong rates.
#
# Fix: extract ALL HS10 codes with positive rates from the historical CSV for
# each old program. This is the only reliable way to ensure complete zeroing —
# the old scope spans dozens of chapters beyond 72-76 and cannot be captured
# by a fixed set of chapter prefixes.

message('\nExtracting old 232 product scope from historical CSV...')
if (!file.exists(historical_csv)) {
  stop(sprintf('Historical CSV not found: %s\n  Provide path as 3rd argument.', historical_csv))
}

hist_header <- names(read_csv(historical_csv, n_max = 0, show_col_types = FALSE))
old_s232_cols <- hist_header[str_detect(hist_header, '^s232_')]
message(sprintf('  Found %d old 232 columns: %s',
                length(old_s232_cols), paste(old_s232_cols, collapse = ', ')))

# Map old column names to legacy program names
# Column s232_steel_rate -> program 'steel', etc.
# Only zero out metal programs (steel, aluminum, and their derivatives).
# Non-metal programs (autos, softwood, etc.) are untouched by the metal overhaul.
legacy_metal_programs <- c('steel', 'steel_derivatives', 'aluminum', 'aluminum_derivatives')
old_metal_cols <- paste0('s232_', legacy_metal_programs)
old_metal_cols <- old_metal_cols[old_metal_cols %in% old_s232_cols]

if (length(old_metal_cols) == 0) {
  stop('No old metal 232 columns found in historical CSV!')
}

# Read only the hts10 column + old metal columns (skip everything else)
# Use col_select for memory efficiency on large CSVs
hist_data <- read_csv(historical_csv, show_col_types = FALSE,
                       col_select = c('hts10', all_of(old_metal_cols)),
                       col_types = cols(hts10 = col_character(), .default = col_double()))

# For each old program, get all unique HS10 codes with positive rates
for (old_prog in legacy_metal_programs) {
  old_col <- paste0('s232_', old_prog)
  if (!(old_col %in% names(hist_data))) {
    message(sprintf('  %s: column not in CSV, skipping', old_prog))
    next
  }

  old_hs10 <- hist_data %>%
    filter(!!sym(old_col) > 0) %>%
    pull(hts10) %>%
    unique() %>%
    sort()

  message(sprintf('  %s: %d products with positive rates in historical CSV',
                  old_prog, length(old_hs10)))

  if (length(old_hs10) > 0) {
    programs[[old_prog]] <- list(
      base = old_hs10,
      rates = list(default = 0),
      usmca_exempt = 0
    )
  }
}

rm(hist_data)  # free memory

# -----------------------------------------------------------------------------
# Verify Annex III MFN rates
# -----------------------------------------------------------------------------
message('\nVerifying Annex III MFN rates...')

annex_iii_codes <- c(
  section_codes[['iii_steel_derivatives']] %||% character(0),
  section_codes[['iii_aluminum_derivatives']] %||% character(0)
)

if (length(annex_iii_codes) > 0 && file.exists(mfn_path)) {
  mfn <- read_csv(mfn_path, show_col_types = FALSE,
                   col_types = cols(hs8 = col_character()))

  annex_iii_hs8 <- unique(substr(annex_iii_codes, 1, 8))
  mfn_match <- mfn %>% filter(hs8 %in% annex_iii_hs8)

  if (nrow(mfn_match) > 0) {
    high_mfn <- mfn_match %>% filter(mfn_rate >= 0.15)
    if (nrow(high_mfn) > 0) {
      warning(sprintf('%d Annex III products have MFN >= 15%%:', nrow(high_mfn)))
      for (r in seq_len(nrow(high_mfn))) {
        message(sprintf('    %s: %.1f%%', high_mfn$hs8[r], high_mfn$mfn_rate[r] * 100))
      }
    } else {
      message(sprintf('  All %d matched HS8 codes have MFN < 15%% -- target_total is safe',
                      nrow(mfn_match)))
    }
    message(sprintf('  MFN range: %.1f%% - %.1f%%',
                    min(mfn_match$mfn_rate) * 100, max(mfn_match$mfn_rate) * 100))
  }
}

# -----------------------------------------------------------------------------
# Write s232.yaml
# -----------------------------------------------------------------------------
message('\nWriting s232.yaml...')

yaml_path <- file.path(output_dir, 's232.yaml')

header <- c(
  '# Section 232 Metal Content Overhaul -- Reform Overlay',
  '# EO: "Strengthening Actions Taken to Adjust Imports of Aluminum, Steel, and Copper"',
  '# Effective: April 6, 2026',
  '#',
  '# Annex I-A: 50% on full customs value (UK: 25%)',
  '# Annex I-B: 25% on full customs value (UK: 15%)',
  '# Annex III: Transitional floor of 15% total (through Dec 31, 2027)',
  '# Annex II:  Removed from 232 scope (rate = 0)',
  '#',
  '# Generated by scripts/build_metal_232_reform.R',
  sprintf('# Generated on: %s', Sys.time()),
  ''
)

yaml_body <- as.yaml(programs, indent = 2, indent.mapping.sequence = TRUE)
writeLines(c(header, yaml_body), yaml_path)

message(sprintf('  Wrote: %s', yaml_path))

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------
message('\n=== SUMMARY ===')
grand_total <- 0
for (name in sort(names(programs))) {
  n <- length(programs[[name]]$base)
  rate <- programs[[name]]$rates$default
  if (!is.null(programs[[name]]$target_total)) {
    rate_desc <- sprintf('target_total=%.0f%%', programs[[name]]$target_total$default * 100)
  } else if (rate == 0) {
    rate_desc <- 'REMOVED'
  } else {
    rate_desc <- sprintf('rate=%.0f%%', rate * 100)
  }
  message(sprintf('  %-30s %4d codes  (%s)', name, n, rate_desc))
  grand_total <- grand_total + n
}
message(sprintf('\n  Total: %d HTS codes across %d programs', grand_total, length(programs)))
message('Done!')
