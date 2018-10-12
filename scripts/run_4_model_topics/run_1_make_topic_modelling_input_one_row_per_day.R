#### Preamble ####
# Purpose: This file takes Australian Hansard csv files and it converts them to a tidied tibble of text that can be analysed. The words are lowered and punctuation is removed, but stop words are not removed and they are not stemmed here.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 8 October 2018
# Prerequisites: You need a folder of csv files e.g. the output of parse_each_pdf_and_save_csv_pdftools.R 
# To do:
# - 


#### Set up workspace ####
library(furrr)
library(hunspell)
library(lubridate)
library(stringi)
library(tictoc)
library(tidytext)
library(tidyverse)
# update.packages()

# Read in multiword expression concatenation list e.g. new zealand to new_zealand
multiword_expressions <- read_csv2("inputs/misc/multiwords_with_corrections.csv", col_types = cols()) %>% 
  mutate(numberOfCharacters = nchar(original)) %>%
  arrange(desc(numberOfCharacters)) %>%
  select(-numberOfCharacters)




#### Create the lists of CSV filenames to read in ####
# Change the path as required:
# use_this_path_to_get_csvs  <- "outputs/hansard/hansard_csv_files" # For testing
use_this_path_to_get_csvs  <- "outputs/big_files_do_not_push/hansard_csv" # For testing
# use_this_path_to_get_csvs  <- "/Volumes/Backup/temp"

file_names <-
  list.files(
    path = use_this_path_to_get_csvs,
    pattern = "*.csv",
    recursive = TRUE,
    full.names = TRUE
  )
file_names <- file_names %>% sample() # Change the order so that it doesn't run chronologically
file_names

# file_names <- file_names[1:2] # for testing


#### Create the function that will be passed to the purrr function ####
tidy_the_hansard_csv_files <-
  function(name_of_input_csv_file) {
    
    # Read in the text column of each CSV
    # name_of_input_csv_file <- "outputs/hansard/hansard_csv_files/1997-02-05.csv" # for testing
    csv_file_as_parsed_PDF <- read_csv(name_of_input_csv_file, col_types = cols())

    # Get the date - it's needed later
    date_of_hansard <- basename(name_of_input_csv_file) %>% str_replace(".csv", "") %>% ymd()
    
    # Remove numbers from text
    csv_file_as_parsed_PDF$text <- str_replace_all(csv_file_as_parsed_PDF$text, "[:digit:]+", "") # The plus means look for one or more occurances - see stringr docs

    # Remove punctuation
    csv_file_as_parsed_PDF$text <- str_replace_all(csv_file_as_parsed_PDF$text, "[:punct:]+", "")
    # I don't know why, but punct isn't picking up $, so do that explicitly
    csv_file_as_parsed_PDF$text <- str_replace_all(csv_file_as_parsed_PDF$text, "\\$+", "")

    # Remove change all to lower
    csv_file_as_parsed_PDF$text <- str_to_lower(csv_file_as_parsed_PDF$text)
    
    # Combine multiword expressions by _
    csv_file_as_parsed_PDF$text <-
      stri_replace_all_regex(
        csv_file_as_parsed_PDF$text,
        "\\b" %s+% multiword_expressions$original %s+% "\\b",
        multiword_expressions$corrected,
        vectorize_all = FALSE
      )

    # Convert theText column to just one word per row in a column called word
    words_from_text_file <- csv_file_as_parsed_PDF %>%
      select(text) %>% 
      unnest_tokens(word, text)

    all_words_on_day <- paste(words_from_text_file$word, collapse = ", ")
    
    # Push the date and the words together as a tibble
    days_words <-
      tibble(date = date_of_hansard, words = all_words_on_day)
}


#### Use the function ####
# Set up furrr plan
plan(multiprocess)

# tic("Normal purrr")
# all_hansard_words <- map_dfr(file_names, tidy_the_hansard_csv_files, .id = NULL)
# toc()

tic("Furrr")
all_hansard_words <- future_map_dfr(file_names, tidy_the_hansard_csv_files, .id = NULL, .progress = TRUE)
toc()

# object.size(all_hansard_words)

head(all_hansard_words)

#### Save and clean up ####

save(all_hansard_words, file = "outputs/big_files_do_not_push/all_hansard_words_by_date.Rda")

rm(all_hansard_words, file_names, use_this_path_to_get_csvs, tidy_the_hansard_csv_files)

