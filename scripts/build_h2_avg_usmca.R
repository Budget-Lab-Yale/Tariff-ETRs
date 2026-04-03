# Build H2-average USMCA product shares from tracker monthly files
# Averages Jul-Dec 2025 monthly shares, compares to current annual file,
# and writes the result to resources/usmca_product_shares_2025.csv

tracker_dir <- 'C:/Users/jar335/Documents/Repositories/tariff-rate-tracker/resources'
months <- sprintf('%02d', 7:12)
files <- sprintf('%s/usmca_product_shares_2025_%s.csv', tracker_dir, months)

# Read and stack all H2 monthly files
dfs <- lapply(files, read.csv, colClasses = c('character', 'character', 'numeric'))
h2 <- do.call(rbind, dfs)

# Average across months
h2_avg <- aggregate(usmca_share ~ hts10 + cty_code, data = h2, FUN = mean)

# Load current annual file
current <- read.csv('resources/usmca_product_shares_2025.csv',
  colClasses = c('character', 'character', 'numeric'))

# Load import weights
imports <- readRDS('cache/hs10_by_country_gtap_2024_con.rds')
imports$cty_code <- as.character(imports$cty_code)
# Aggregate imports to hs10 x cty_code (drop gtap dimension)
imp_agg <- aggregate(imports ~ hs10 + cty_code, data = imports, FUN = sum)

# --- Trade-weighted comparison ---
weighted_mean <- function(shares_df, col_name, imp_df, country) {
  sub <- shares_df[shares_df$cty_code == country, ]
  names(sub)[names(sub) == 'hts10'] <- 'hs10'
  m <- merge(sub, imp_df, by = c('hs10', 'cty_code'))
  sum(m[[col_name]] * m$imports) / sum(m$imports)
}

cat('=== H2 Average (trade-weighted) ===\n')
cat(sprintf('Rows: %d\n', nrow(h2_avg)))
cat(sprintf('Wtd share (Canada): %.4f\n', weighted_mean(h2_avg, 'usmca_share', imp_agg, '1220')))
cat(sprintf('Wtd share (Mexico): %.4f\n', weighted_mean(h2_avg, 'usmca_share', imp_agg, '2010')))

cat('\n=== Current Annual (trade-weighted) ===\n')
cat(sprintf('Rows: %d\n', nrow(current)))
cat(sprintf('Wtd share (Canada): %.4f\n', weighted_mean(current, 'usmca_share', imp_agg, '1220')))
cat(sprintf('Wtd share (Mexico): %.4f\n', weighted_mean(current, 'usmca_share', imp_agg, '2010')))

# Merged comparison
comp_h2 <- h2_avg
names(comp_h2)[names(comp_h2) == 'hts10'] <- 'hs10'
comp_h2 <- merge(comp_h2, imp_agg, by = c('hs10', 'cty_code'))
comp_cur <- current
names(comp_cur)[names(comp_cur) == 'hts10'] <- 'hs10'
comp_cur <- merge(comp_cur, imp_agg, by = c('hs10', 'cty_code'))

cat('\n=== Difference (trade-weighted) ===\n')
for (cty in c('1220', '2010')) {
  label <- if (cty == '1220') 'Canada' else 'Mexico'
  h2_sub <- comp_h2[comp_h2$cty_code == cty, ]
  cur_sub <- comp_cur[comp_cur$cty_code == cty, ]
  h2_wtd <- sum(h2_sub$usmca_share * h2_sub$imports) / sum(h2_sub$imports)
  cur_wtd <- sum(cur_sub$usmca_share * cur_sub$imports) / sum(cur_sub$imports)
  cat(sprintf('%s: annual %.4f -> h2_avg %.4f (diff %+.4f)\n', label, cur_wtd, h2_wtd, h2_wtd - cur_wtd))
}

# Write the h2_avg to the output location
write.csv(h2_avg, 'resources/usmca_product_shares_2025_h2avg.csv', row.names = FALSE)
cat('\nWrote h2_avg to resources/usmca_product_shares_2025_h2avg.csv\n')
cat('To activate: rename to resources/usmca_product_shares_2025.csv\n')
