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
#   Rscript scripts/build_metal_232_reform.R [annex_text_path]
#
# Default annex path: C:/Users/jar335/Downloads/annex.txt
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
output_dir <- 'config/scenarios/metal_232_overhaul/reforms/metal_232'
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

# --- Suppress legacy baseline programs ---
# The overlay adds NEW annex-tier programs but doesn't touch OLD program columns
# (steel, aluminum, aluminum_derivatives, steel_derivatives). Without explicit
# zeros, legacy rates survive via pmax and Annex II/III products get wrong rates.
# Fix: zero out old programs for ALL annex products so only new programs apply.

all_annex_codes <- unique(unlist(section_codes[grep('^(ia|ib|ii|iii)_', names(section_codes))]))

# The EO terminates old HTSUS headings (Annex IV item 11), so ALL products in
# old 232 programs that aren't on a new annex list lose coverage. We must zero
# out the old programs with broad prefixes (not just annex codes) to catch
# products like 7201-7205 that are in the old baseline but not on any annex.
legacy_suppressor_codes <- sort(unique(c(
  '72', '73',     # All primary steel (covers 7201-7306 and anything the tracker had)
  '76',           # All primary aluminum
  '74',           # All copper
  all_annex_codes # All derivative codes from annexes I-A, I-B, II, III
)))
message(sprintf('  Suppressing legacy programs with %d codes (incl. chapter prefixes)',
                length(legacy_suppressor_codes)))

for (old_prog in c('steel', 'aluminum', 'aluminum_derivatives', 'steel_derivatives')) {
  programs[[old_prog]] <- list(
    base = legacy_suppressor_codes,
    rates = list(default = 0),
    usmca_exempt = 0
  )
}

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
