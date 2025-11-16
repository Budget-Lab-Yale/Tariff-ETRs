# Test script for load_imports_hs10_country() function
# This script tests the new HS10 data loading function

library(tidyverse)

# Source the functions
source('src/functions.R')

# Test loading 2024 data
cat('Testing load_imports_hs10_country()...\n\n')

# Load consumption imports for 2024
result <- load_imports_hs10_country(
  import_data_path = 'C:/Users/jar335/Downloads',
  year = 2024,
  type = 'con'
)

# Display summary statistics
cat('\n=== Summary Statistics ===\n')
cat(sprintf('Total records: %s\n', format(nrow(result), big.mark = ',')))
cat(sprintf('Unique HS10 codes: %s\n', format(n_distinct(result$hs10), big.mark = ',')))
cat(sprintf('Unique countries: %s\n', format(n_distinct(result$cty_code), big.mark = ',')))
cat(sprintf('Months covered: %s\n', paste(sort(unique(result$month)), collapse = ', ')))
cat(sprintf('Total import value: $%s\n', format(round(sum(result$value) / 1e9, 2), big.mark = ',')), 'billion')

# Show sample of data
cat('\n=== Sample Records (first 10) ===\n')
print(head(result, 10))

# Check HS10 code padding
cat('\n=== HS10 Code Length Check ===\n')
code_lengths <- result %>%
  mutate(code_length = nchar(hs10)) %>%
  count(code_length)
print(code_lengths)

# Show top 10 HS10 codes by value
cat('\n=== Top 10 HS10 Codes by Import Value ===\n')
top_codes <- result %>%
  group_by(hs10) %>%
  summarise(total_value = sum(value), .groups = 'drop') %>%
  arrange(desc(total_value)) %>%
  head(10) %>%
  mutate(total_value_millions = round(total_value / 1e6, 1))
print(top_codes)

cat('\nTest complete!\n')
