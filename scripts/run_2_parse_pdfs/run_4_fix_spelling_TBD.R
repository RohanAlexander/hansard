# !diagnostics off
#### Preamble ####
# Purpose: This file takes Australian Hansard CSV files and it splits the ones that were in two column format. The PDFs until (not including) 2013-11-12 are arranged as two columns on each page and so most rows are two different speeches and those columns need to be separated.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 9 October 2018
# Prerequisites: You need to have downloaded the PDFs from the parliament's website. There are many GBs of PDFs and they are saved on an external drive - have fun finding that future-Rohan - and also the Berkeley Demography server. For testing purposes there should be some in the /inputs/for_testing_hansard_pdf folder.
# To do:


#### Set up workspace ####
# devtools::install_github("DavisVaughan/furrr")
library(furrr)
library(lubridate)
library(pdftools)
library(stringi)
library(tidyverse)
library(tictoc)
library(tm)
# update.packages()
# Set up furrr
plan(multiprocess)
# Get the spell checker
fix_wrong_spellings <- read_csv2("inputs/misc/misspelt_words_with_corrections.csv") %>% 
  mutate(numberOfCharacters = nchar(original)) %>% 
  arrange(desc(numberOfCharacters)) %>% 
  select(-numberOfCharacters)



#### Create lists of CSVs to read ####
# Change the path as required:
use_this_path_to_get_csvs  <- "/Volumes/Backup/temp"
# use_this_path_to_get_csvs  <- "outputs/hansard/temp"

# Get list of Hansard csvs that have been parsed from PDFs and had front matter removed
file_names <-
  list.files(
    path = use_this_path_to_get_csvs,
    pattern = "*.csv",
    recursive = FALSE,
    full.names = TRUE
  )

file_names <- file_names %>% sample() # Randomise the order

# use_this_path_to_save_csvs  <- "outputs/hansard/temp/testing"
use_this_path_to_save_csvs  <- "/Volumes/Backup/temp"
save_names <- file_names %>%
  str_replace(use_this_path_to_get_csvs, use_this_path_to_save_csvs)


#### Create the function that will be applied to the files ####
split_columns <-
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


#### Walk through the lists and parse the PDFs ####
# tic("Normal walk2")
# walk2(file_names, save_names, ~ get_text_from_PDFs(.x, .y))
# toc()

safely_split_columns <- safely(split_columns)

# file_names <- file_names[1:10]
# save_names <- save_names[1:10]
# file_names <- file_names[1:(length(file_names)/2)]
# save_names <- save_names[1:(length(save_names)/2)]


tic("Furrr walk2 stringr")
future_walk2(file_names,
             save_names,
             ~ safely_split_columns(.x, .y),
             .progress = TRUE)
toc()

