#### Preamble ####
# Purpose: We don't really need everything to be spelt correctly, but fixing some common spelling issues would be great. This goes through Hansard files and identifies words that are wrong so they can be fixed. It's not going to pick up words that are correct, but wrong in that context.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 8 September 2018
# Prerequisites: You need to have Hansard in a csv or text file.


#### Set up workspace ####
# Load libraries
library(hunspell)
library(tictoc)
library(tidytext)
library(tidyverse)
# update.packages()


#### Load data ####
csv_file_names <-
  list.files(
    path = "outputs/hansard/hansard_csv_files",
    pattern = "*.csv",
    recursive = TRUE,
    full.names = TRUE
  )
set.seed(12345)
csv_file_names <- sample(csv_file_names, 100) # Just get 100 of them


#### Create ignore lists ####
# Create list of words to ignore - mostly names of electorates and Australian places:

electorate_names.csv
place_names.csv
this_is_fine.csv

ignore_these_politician_names <- read_csv("inputs/politicians/politicians_by_individuals.csv", col_types = cols()) %>% 
  select(surname) %>% 
  unique() %>% 
  pull()

custom_stop_words <- bind_rows(
  stop_words, # The default list
  data_frame(
    word = c(ignore_these_area_names, ignore_these_electorate_names, ignore_these_politician_names),
    lexicon = rep("custom", length(word))
  )
)

#### Finde wroong spelings ####
# Get a dataset of all the words used in the statements see: https://cran.r-project.org/web/packages/tidytext/vignettes/tidytext.html
finde_wroong_spelings <- function(file_name){
  # file_name <- csv_file_names[1] # for testing
  hansard_words <- read_csv(file_name, col_types = cols()) %>% 
    select(theText) %>% 
    unnest_tokens(word, theText, to_lower = FALSE) %>%  # break the lines into each word
    anti_join(custom_stop_words, by = "word") # ignore stop words
}

all_words <- map_dfr(csv_file_names, ~finde_wroong_spelings(.x)) # Not instant


possible_wrong_words <- all_words %>% 
  count(word, sort = TRUE) %>% # Get counts of each word
  mutate(right_spelling = hunspell_check(word, dict = dictionary('en_GB'))) %>% # Run the spell check using Hunspell
  filter(right_spelling == FALSE) %>% # Filter to just wrong words 
  select(word)

add_to_dictionary <- c("Labor")


# You can add a column of suggestions - takes a while - if you want using something like the next line, but it doesn't seem to get you much in this case
#   mutate(recommended_word = hunspell_suggest(word))
# wrong_words_in_statements <- wrong_words_in_statements[,1:3] # If you included the column of suggestions then you need this if you want to write.table
write_csv(possible_wrong_words, "wrong.csv")


#### Examine issues ####
# The above provides a bunch of possible issues and if want to examine the context use:
rows_of_statements_that_contain_string <- all_hansard %>%
  filter(str_detect(statement, " connexion "))


#### Clean up ####
# Save
TBD

# Remove
rm(wrong_words_in_statements, words_in_statements)