# https://tutorials.quanteda.io/statistical-analysis/collocation/


require(quanteda)
install.packages("quanteda")
library(quanteda)

news_corp <- readRDS("/Users/rohanalexander/Downloads/data_corpus_guardian.rds")
ndoc(news_corp)
range(docvars(news_corp, 'date'))

news_toks <- ?tokens(news_corp, remove_punct = TRUE)
cap_col <- tokens_select(news_toks, '^[A-Z]', valuetype = 'regex', case_insensitive = FALSE, padding = TRUE) %>% 
  textstat_collocations(min_count = 100)
head(cap_col, 20)