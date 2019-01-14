# !diagnostics off
#### Preamble ####
# Purpose: This file takes Australian Hansard CSV files that are all in one column and one row and applies a brutal, replacement-based, custom-dictionary, spell-checker.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 11 October 2018
# Prerequisites: You need to have done all the other parsing steps. For testing purposes there should be some in the /outputs/hansard folder.
# To do:


#### Set up workspace ####
# devtools::install_github("DavisVaughan/furrr")
library(furrr)
library(lubridate)
# library(pdftools)
library(stringi)
library(tidyverse)
library(tictoc)
library(tm)
# update.packages()
# Set up furrr
plan(multiprocess)
# Get the spell checker
fix_wrong_spellings <-
  read_csv2("inputs/misc/misspelt_words_with_corrections.csv") %>%
  mutate(numberOfCharacters = nchar(original)) %>%
  arrange(desc(numberOfCharacters)) %>%
  select(-numberOfCharacters)


#### Create lists of CSVs to read ####
# Change the path as required:
use_this_path_to_get_csvs  <- "outputs/hansard/run_5_output"
# use_this_path_to_get_csvs <- "/Volumes/Hansard/parsed/federal/hortest"

# Get list of Hansard csvs that have been parsed from PDFs and had front matter removed
file_names <-
  list.files(
    path = use_this_path_to_get_csvs,
    pattern = "*.csv",
    recursive = FALSE,
    full.names = TRUE
  )

file_names <- file_names %>% sample() # Randomise the order

use_this_path_to_save_csvs  <- "outputs/hansard/run_6_output"
# use_this_path_to_save_csvs <- "/Volumes/Hansard/parsed/federal/hor"
# use_this_path_to_save_csvs <- "/Volumes/Hansard/parsed/federal/hortest"

save_names <- file_names %>%
  str_replace(use_this_path_to_get_csvs, use_this_path_to_save_csvs)


#### Create the function that will be applied to the files ####
fix_spelling <-
  function(name_of_input_csv_file,
           name_of_output_csv_file) {
    # Read in the csv, based on the filename list
    # name_of_input_csv_file <- "/Volumes/Backup/temp/1971-10-05.csv" # uncomment for testing
    
    csv_to_clean <-
      read_csv(name_of_input_csv_file,
               trim_ws = FALSE,
               col_types = cols())
    
    #Fix the spelling
    csv_to_clean$text <-
      stri_replace_all_regex(
        csv_to_clean$text,
        "\\b" %s+% fix_wrong_spellings$original %s+% "\\b",
        fix_wrong_spellings$corrected,
        vectorize_all = FALSE
      )
    
    # Save file
    write_csv(csv_to_clean, name_of_output_csv_file)
    
    print(paste0("Done with ", name_of_output_csv_file, " at ", Sys.time()))
  }

safely_fix_spelling <- safely(fix_spelling)


#### Walk through the lists and parse the PDFs ####
# Normal walk2
tic("Normal walk2")
walk2(file_names,
      save_names,
      ~ safely_fix_spelling(.x, .y)
      )
toc()

# # Furrr walk2
# tic("Furrr walk2")
# future_walk2(file_names,
#              save_names,
#              ~ safely_fix_spelling(.x, .y),
#              .progress = TRUE)
# toc()
