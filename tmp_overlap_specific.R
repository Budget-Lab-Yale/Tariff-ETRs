library(tidyverse)
library(yaml)
imp <- read_csv('resources/hs10_gtap_crosswalk.csv', show_col_types = FALSE, col_types = cols(hs10=col_character(), gtap_code=col_character())) %>% select(hs10, gtap_code)
other <- read_yaml('config/2-21_temp_mc-detail-types/2026-01-01/other_params.yaml')
cfg232 <- read_yaml('config/2-21_temp_mc-detail-types/2026-01-01/232.yaml')
expand_prefixes <- function(prefixes, hs10_codes) {
  if (is.null(prefixes) || length(prefixes)==0) return(character())
  unique(unlist(lapply(prefixes, function(p) hs10_codes[str_starts(hs10_codes, as.character(p))])))
}
rows <- list()
for (p in other$metal_content$metal_programs) {
  hs <- expand_prefixes(cfg232[[p]]$base, imp$hs10)
  rows[[length(rows)+1]] <- tibble(hs10=hs, program=p)
}
map <- bind_rows(rows) %>% distinct()
print(map %>% filter(hs10=='7616995190'))
