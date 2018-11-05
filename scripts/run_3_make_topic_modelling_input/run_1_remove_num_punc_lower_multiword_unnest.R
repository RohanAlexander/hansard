#### Preamble ####
# Purpose: This file takes Australian Hansard csv files and it converts them to a tidied tibble of text that can be analysed. The words are lowered and punctuation is removed, but stop words are not removed and they are not stemmed here.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 14 October 2018
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



#### Create lists of CSVs to read ####
# Change the path as required:
# use_this_path_to_get_csvs  <- "/Volumes/Backup/temp"
# use_this_path_to_get_csvs  <- "outputs/hansard/temp/testing"
# use_this_path_to_get_csvs  <- "outputs/big_files_do_not_push/hansard_csv"
use_this_path_to_get_csvs <- "/Volumes/Hansard/parsed/federal/hor"

# Get list of Hansard csvs that have been parsed from PDFs and had front matter removed
file_names <-
  list.files(
    path = use_this_path_to_get_csvs,
    pattern = "*.csv",
    recursive = FALSE,
    full.names = TRUE
  )

file_names <- file_names %>% sample() # Randomise the order

# use_this_path_to_save_csvs  <- "outputs/hansard/temp/testing/test"
use_this_path_to_save_csvs <- "/Volumes/Hansard/for_topics/federal/hor"
# use_this_path_to_save_csvs  <- "/Volumes/Backup/senate_topic_modelling_input"
# use_this_path_to_save_csvs  <- "/Volumes/Backup/temp"
save_names <- file_names %>%
  str_replace(use_this_path_to_get_csvs, use_this_path_to_save_csvs)



#### Create the function that will be passed to the purrr function ####
tidy_the_hansard_csv_files <-
  function(name_of_input_csv_file,
           name_of_output_csv_file) {

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

    # Save file
    write_csv(words_from_text_file, name_of_output_csv_file)
    
    print(paste0("Done with ", name_of_output_csv_file, " at ", Sys.time()))
}


#### Use the function ####
# Set up furrr plan
plan(multiprocess)

# tic("Normal purrr")
# all_hansard_words <- map_dfr(file_names, tidy_the_hansard_csv_files, .id = NULL)
# toc()




safely_tidy_the_hansard_csv_files <- safely(tidy_the_hansard_csv_files)

tic("Furrr walk2 stringr")
future_walk2(file_names,
             save_names,
             ~ safely_tidy_the_hansard_csv_files(.x, .y),
             .progress = TRUE)
toc()


#### Clean up ####
rm(all_hansard_words, file_names, use_this_path_to_get_csvs, tidy_the_hansard_csv_files)

