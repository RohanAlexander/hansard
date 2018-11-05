#### Preamble ####
# Purpose: This file takes Australian Hansard csv files and it converts them to a tidied tibble of text that can be analysed. The words are lowered and punctuation is removed, but stop words are not removed and they are not stemmed here.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 17 October 2018
# Prerequisites: You need a folder of csv files e.g. the output of parse_each_pdf_and_save_csv_pdftools.R
# To do:
# -


#### Set up workspace ####
library(furrr)
library(hunspell)
library(lubridate)
library(quanteda)
library(stringi)
library(stm)
library(tictoc)
library(tidytext)
library(tidyverse)
# update.packages()

# Read in politicians names
politicians_surname <-
  read_csv("inputs/politicians/politicians_by_individuals.csv") %>%
  select(surname) %>%
  mutate(surname = str_to_lower(surname)) %>%
  unique() %>%
  pull()

politicians_first_name <-
  read_csv("inputs/politicians/politicians_by_individuals.csv") %>%
  select(firstName) %>%
  mutate(firstName = str_to_lower(firstName)) %>%
  unique() %>%
  pull()

# Bind custom list of stopwords to the default list
custom_stop_words <- bind_rows(stop_words, # The default list
                               data_frame(
                                 word = c(
                                   politicians_surname,
                                   politicians_first_name,
                                   "act",
                                   "amendment",
                                   "australia",
                                   "australian",
                                   "bill",
                                   "commonwealth",
                                   "esq",
                                   "gentleman",
                                   "government",
                                   "honorable",
                                   "honourable",
                                   "house",
                                   "labor",
                                   "labour",
                                   "legislation",
                                   "liberal",
                                   "madam",
                                   "minister",
                                   "motion",
                                   "mp",
                                   "opposition",
                                   "people",
                                   "senator",
                                   "sir",
                                   "speaker"
                                 ),
                                 lexicon = rep("custom", length(word))
                               ))


#### Create lists of CSVs to read ####
# Change the path as required:
# use_this_path_to_get_csvs  <- "outputs/hansard/temp/testing"
use_this_path_to_get_csvs <-
  "/Volumes/Hansard/for_topics/federal/hor"

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
use_this_path_to_save_csvs  <-
  "/Volumes/Hansard/for_topics/federal/hor"
# use_this_path_to_save_csvs  <- "/Volumes/Backup/temp"
save_names <- file_names %>%
  str_replace(use_this_path_to_get_csvs, use_this_path_to_save_csvs)



#### Create the function that will be passed to the purrr function ####
tidy_the_hansard_csv_files <-
  function(name_of_input_csv_file,
           name_of_output_csv_file) {
    # Read in the text column of each CSV
    # name_of_input_csv_file <- "/Volumes/Backup/senate_topic_modelling_input/1980-12-03.csv" # for testing
    hansard <- read_csv(name_of_input_csv_file, col_types = cols())
    
    # Remove stop words
    hansard <- hansard %>%
      mutate(line = row_number()) %>%
      anti_join(custom_stop_words)
    
    # Only keep words that appear at least five times
    hansard <- hansard %>%
      count(word, sort = TRUE) %>% # Create counts of how many times each word appears
      filter(n > 5) %>% # Filter to only those words that appear at least 10 times in a day
      uncount(n)
    
    # Save file
    write_csv(hansard, name_of_output_csv_file)
    print(paste0("Done with ", name_of_output_csv_file, " at ", Sys.time()))
  }


#### Use the function ####
# Set up furrr plan
plan(multiprocess)

# tic("Normal purrr")
# all_hansard_words <- map_dfr(file_names, tidy_the_hansard_csv_files, .id = NULL)
# toc()

safely_tidy_the_hansard_csv_files <-
  safely(tidy_the_hansard_csv_files)

tic("Furrr walk2 stringr")
future_walk2(file_names,
             save_names,
             ~ safely_tidy_the_hansard_csv_files(.x, .y),
             .progress = TRUE)
toc()


#### Clean up ####
rm(
  file_names,
  use_this_path_to_get_csvs,
  tidy_the_hansard_csv_files,
  safely_tidy_the_hansard_csv_files,
  custom_stop_words,
  politicians_first_name,
  politicians_surname
)
