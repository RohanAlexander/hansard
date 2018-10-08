# https://tutorials.quanteda.io/statistical-analysis/collocation/

# install.packages("quanteda")
library(quanteda)
library(tidyverse)

# Read the data
file_names <-
  list.files(
    path = "outputs/hansard/hansard_csv_files",
    pattern = "*.csv",
    recursive = TRUE,
    full.names = TRUE
  ) # The full set of files

text_data <- map_dfr(file_names, read_csv)

text_data <- text_data %>% 
  mutate(docid_field = as.integer(rownames(text_data))) 
class(text_data)
head(text_data)

data <- corpus(text_data)

# news_corp <- readRDS("/Users/rohanalexander/Downloads/data_corpus_guardian.rds")
# ndoc(news_corp)
# range(docvars(news_corp, 'date'))

news_toks <- tokens(data, remove_punct = TRUE)
cap_col <-
  tokens_select(
    news_toks,
    '^[A-Z]',
    valuetype = 'regex',
    case_insensitive = FALSE,
    padding = TRUE
  ) %>%
  textstat_collocations(min_count = 10, size = 3)
head(cap_col, 20)


write_csv(cap_col, "ngrams3.csv")