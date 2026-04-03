cache <- readRDS('cache/hs10_by_country_gtap_2024_con.rds')
cat('Columns:', paste(names(cache), collapse=', '), '\n')
cat('Rows:', nrow(cache), '\n')
print(head(cache, 3))
