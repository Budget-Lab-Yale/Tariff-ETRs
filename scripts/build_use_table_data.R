# =============================================================================
# build_use_table_data.R
# =============================================================================
#
# One-time script to extract BEA use table data from xlsx files to CSVs.
#
# Input (from tariff-impacts/input/):
#   BEA - Import Matrix, Before Redefinitions - Summary - 2024.xlsx
#   BEA - The Use of Commodities by Industry - Summary - 2024.xlsx
#   BEA - Commodity-by-Commodity Total Requirements - Summary - 2024.xlsx
#
# Output (to resources/io/):
#   bea_use_import.csv          (CxI import use matrix)
#   bea_use_domestic.csv        (CxI domestic use = total - import)
#   bea_pce_by_commodity.csv    (bea_code, pce)
#   bea_industry_output.csv     (bea_code, output)
#   bea_cxc_total_requirements.csv (CxC Leontief inverse)
#
# Excel layout (both Import and Use tables):
#   Row 6: BEA codes as column headers
#   Row 7: Description names
#   Row 8+: Data (bea_code, name, then numeric values)
#
# =============================================================================

library(tidyverse)
library(readxl)

INPUT_DIR <- 'C:/Users/jar335/Documents/Repositories/tariff-impacts/input'
OUTPUT_DIR <- 'resources/io'
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)


# =============================================================================
# Helper: parse a BEA CxI matrix from xlsx
# Uses row 6 as BEA code headers (A6:CP6 or A6:CQ6)
# Data starts at row 8
# =============================================================================

parse_bea_matrix <- function(path) {
  # Read with row 6 as header (BEA codes)
  # Use range starting at row 6 so codes become column names
  raw <- read_excel(path, sheet = 'Table', skip = 5, col_names = TRUE)

  # First two columns are commodity code and name
  names(raw)[1:2] <- c('bea_code', 'bea_name')
  raw <- raw %>% mutate(bea_code = str_trim(as.character(bea_code)))

  # Skip the descriptions row (row 7 in Excel, row 1 in our data)
  # It has NA or blank in bea_code and 'Name' or description text
  raw <- raw %>% filter(!is.na(bea_code), bea_code != '')

  return(raw)
}


# =============================================================================
# 1. Import Matrix
# =============================================================================

message('Parsing Import Matrix...')
import_raw <- parse_bea_matrix(
  file.path(INPUT_DIR, 'BEA - Import Matrix, Before Redefinitions - Summary - 2024.xlsx')
)

# Identify industry columns (BEA codes, not totals/final demand)
all_cols <- names(import_raw)[3:ncol(import_raw)]

# Industry codes: exclude T (totals), F (final demand), V0 (value added)
industry_cols <- all_cols[!str_detect(all_cols, '^T|^F|^V0')]
message(sprintf('  Industry columns: %d', length(industry_cols)))

# Filter to commodity rows only (no T0/V0/VA totals)
import_matrix <- import_raw %>%
  filter(!str_detect(bea_code, '^T0|^V0|^VA')) %>%
  select(bea_code, all_of(industry_cols)) %>%
  mutate(across(-bea_code, ~ {
    x <- as.character(.)
    x <- if_else(x == '---' | is.na(x), '0', x)
    as.numeric(x)
  }))

write_csv(import_matrix, file.path(OUTPUT_DIR, 'bea_use_import.csv'))
message(sprintf('bea_use_import.csv: %d commodities x %d industries',
                nrow(import_matrix), ncol(import_matrix) - 1))


# =============================================================================
# 2. Use Table + PCE + Industry Output
# =============================================================================

message('\nParsing Use Table...')
use_raw <- parse_bea_matrix(
  file.path(INPUT_DIR, 'BEA - The Use of Commodities by Industry - Summary - 2024.xlsx')
)

# ---- Extract F010 (PCE) column ----
if (!'F010' %in% names(use_raw)) {
  stop('Could not find F010 (PCE) column in Use Table')
}

pce <- use_raw %>%
  filter(!str_detect(bea_code, '^T0|^V0|^VA')) %>%
  transmute(
    bea_code,
    pce = {
      x <- as.character(F010)
      x <- if_else(x == '---' | is.na(x), '0', x)
      as.numeric(x)
    }
  )

write_csv(pce, file.path(OUTPUT_DIR, 'bea_pce_by_commodity.csv'))
message(sprintf('bea_pce_by_commodity.csv: %d commodities, total PCE = %.0f',
                nrow(pce), sum(pce$pce)))

# ---- Extract T018 (Total industry output) row ----
t018_row <- use_raw %>% filter(bea_code == 'T018')
if (nrow(t018_row) == 0) stop('Could not find T018 row')

# Get industry output values (same industry columns as import matrix)
industry_output <- tibble(
  bea_code = industry_cols,
  output = map_dbl(industry_cols, ~ {
    x <- as.character(t018_row[[.x]])
    x <- if_else(x == '---' | is.na(x), '0', x)
    as.numeric(x)
  })
)

write_csv(industry_output, file.path(OUTPUT_DIR, 'bea_industry_output.csv'))
message(sprintf('bea_industry_output.csv: %d industries, total output = %.0f',
                nrow(industry_output), sum(industry_output$output)))

# ---- Extract CxI use matrix (same rows/cols as import) ----
use_matrix <- use_raw %>%
  filter(!str_detect(bea_code, '^T0|^V0|^VA')) %>%
  select(bea_code, all_of(industry_cols)) %>%
  mutate(across(-bea_code, ~ {
    x <- as.character(.)
    x <- if_else(x == '---' | is.na(x), '0', x)
    as.numeric(x)
  }))


# =============================================================================
# 3. Domestic Use = Total Use - Import Use
# =============================================================================

message('\nDeriving Domestic Use matrix...')

# Ensure same row order
common_rows <- intersect(import_matrix$bea_code, use_matrix$bea_code)

import_aligned <- import_matrix %>%
  filter(bea_code %in% common_rows) %>%
  arrange(match(bea_code, common_rows))

use_aligned <- use_matrix %>%
  filter(bea_code %in% common_rows) %>%
  arrange(match(bea_code, common_rows))

domestic_matrix <- use_aligned
domestic_matrix[, industry_cols] <- use_aligned[, industry_cols] - import_aligned[, industry_cols]

write_csv(domestic_matrix, file.path(OUTPUT_DIR, 'bea_use_domestic.csv'))
message(sprintf('bea_use_domestic.csv: %d rows x %d cols (min = %.1f)',
                nrow(domestic_matrix), length(industry_cols),
                min(domestic_matrix[, industry_cols])))


# =============================================================================
# 4. CxC Total Requirements (Leontief inverse)
# =============================================================================

message('\nParsing CxC Total Requirements...')

tr_raw <- read_excel(
  file.path(INPUT_DIR, 'BEA - Commodity-by-Commodity Total Requirements - Summary - 2024.xlsx'),
  sheet = 1, col_names = FALSE
)
message(sprintf('  Raw: %d rows x %d cols', nrow(tr_raw), ncol(tr_raw)))

# Auto-detect data start: first row where col 1 matches BEA code pattern
bea_strict <- '^[0-9]{2,3}[A-Z0-9]*$'
bea_broad <- '^[A-Za-z0-9]{2,6}$'
data_start <- NA
for (i in 1:min(20, nrow(tr_raw))) {
  val <- str_trim(as.character(tr_raw[[1]][i]))
  if (!is.na(val) && str_detect(val, bea_strict)) {
    data_start <- i
    break
  }
}
stopifnot(!is.na(data_start))

# Find data end
data_end <- data_start
for (i in (data_start + 1):min(nrow(tr_raw), data_start + 100)) {
  val <- str_trim(as.character(tr_raw[[1]][i]))
  if (is.na(val) || val == '' || str_detect(val, '^(Legend|Note|Source|Total)')) break
  data_end <- i
}

# Find column code row
code_row <- NA
for (i in max(1, data_start - 5):(data_start - 1)) {
  vals <- str_trim(as.character(tr_raw[i, ]))
  n_matches <- sum(str_detect(vals[3:length(vals)], bea_broad), na.rm = TRUE)
  if (n_matches >= 10) { code_row <- i; break }
}

row_codes <- str_trim(as.character(tr_raw[[1]][data_start:data_end]))

# Determine last data column
last_col <- ncol(tr_raw)
test_r <- if (!is.na(code_row)) code_row else data_start
for (j in ncol(tr_raw):3) {
  val <- as.character(tr_raw[[j]][test_r])
  if (!is.na(val) && str_trim(val) != '' && str_trim(val) != '---') {
    last_col <- j; break
  }
}

col_codes <- if (!is.na(code_row)) {
  str_trim(as.character(tr_raw[code_row, 3:last_col]))
} else {
  row_codes[1:min(length(row_codes), last_col - 2)]
}
valid_cols <- !is.na(col_codes) & col_codes != ''
col_codes <- col_codes[valid_cols]

# Build numeric matrix
n_rows <- length(row_codes)
n_cols <- length(col_codes)
mat <- matrix(0, nrow = n_rows, ncol = n_cols)
for (j in seq_len(n_cols)) {
  col_idx <- which(valid_cols)[j] + 2
  vals <- as.character(tr_raw[[col_idx]][data_start:data_end])
  vals[is.na(vals) | vals == '---' | vals == '...'] <- '0'
  mat[, j] <- suppressWarnings(as.numeric(vals))
  mat[is.na(mat[, j]), j] <- 0
}

cxc_result <- as_tibble(mat, .name_repair = 'minimal')
names(cxc_result) <- col_codes
cxc_result <- bind_cols(tibble(bea_code = row_codes), cxc_result)

write_csv(cxc_result, file.path(OUTPUT_DIR, 'bea_cxc_total_requirements.csv'))
message(sprintf('bea_cxc_total_requirements.csv: %d x %d (diagonal range: %.3f - %.3f)',
                n_rows, n_cols,
                min(diag(mat)), max(diag(mat))))

message('\nDone. All outputs written to ', OUTPUT_DIR)
