# !diagnostics off
#### Preamble ####
# Purpose: This file takes Australian Hansard CSV files and combines everything onto one row. The reason for this is that some words have been split across lines e.g. Moni- ca, and so they are treated as two words, but really ought to be combined.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 11 October 2018
# Prerequisites: You need to have downloaded the PDFs, read them into CSVs and the removed the two column format. For testing purposes there should be some in the /outputs/hansard/ folder.
# To do:


#### Set up workspace ####
# devtools::install_github("DavisVaughan/furrr")
library(furrr)
# library(lubridate)
# library(pdftools)
library(stringi)
library(tidyverse)
library(tictoc)
library(tm)
# update.packages()
# Set up furrr
plan(multiprocess)


#### Create lists of CSVs to read ####
# Change the path as required:
# use_this_path_to_get_csvs  <- "/Volumes/Backup/temp"
# use_this_path_to_get_csvs  <- "outputs/hansard/temp/testing"
# use_this_path_to_get_csvs  <- "outputs/big_files_do_not_push/hansard_csv"
use_this_path_to_get_csvs <- "/Volumes/Backup/senate_split"


# Get list of Hansard csvs that have been parsed from PDFs and had front matter removed
file_names <-
  list.files(
    path = use_this_path_to_get_csvs,
    pattern = "*.csv",
    recursive = FALSE,
    full.names = TRUE
  )

file_names <- file_names %>% sample() # Randomise the order

# Seems unnecessary, but sometimes useful to separate input and output
# use_this_path_to_save_csvs  <- "outputs/hansard/temp/testing"
# use_this_path_to_save_csvs  <- "outputs/big_files_do_not_push/hansard_csv"
use_this_path_to_save_csvs <- "/Volumes/Backup/senate_combine"
# use_this_path_to_save_csvs  <- "/Volumes/Backup/temp/testing"
save_names <- file_names %>%
  str_replace(use_this_path_to_get_csvs, use_this_path_to_save_csvs)


#### Create the function that will be applied to the files ####
split_columns <-
  function(name_of_input_csv_file,
           name_of_output_csv_file) {
    # Read in the csv, based on the filename list
    # name_of_input_csv_file <- "outputs/hansard/temp/1971-10-05.csv" # uncomment for testing
    csv_with_rows_to_combine <-
      read_csv(name_of_input_csv_file,
               trim_ws = FALSE,
               col_types = cols())
    
    # Remove any extra whitespace i.e. two or more spaces and spaces at either end. Yes, I know that I turned this off in the read_csv, but it's important to be explicit because the whitespcae is useful sometimes.
    csv_with_rows_to_combine$text <-
      str_squish(csv_with_rows_to_combine$text)
    
    # Put everything onto one line, which allows us to rejoin words that have been split across two lines e.g. Mon- ica
    csv_with_rows_to_combine <- csv_with_rows_to_combine %>%
      mutate(text = str_replace_all(text, "-$", "MONICA"))
    
    full_days_hansard <- csv_with_rows_to_combine %>%
      select(-pageNumbers) %>%
      summarise(text = paste(text, collapse = " ")) %>%
      mutate(text = str_replace_all(text, "MONICA ", "")) %>%
      mutate(text = str_replace_all(text, "[:space:]-(?=[:alpha:])", " - "))
    
    rm(csv_with_rows_to_combine)
    
    write_csv(full_days_hansard, name_of_output_csv_file)
    
    print(paste0("Done with ", name_of_output_csv_file, " at ", Sys.time()))
  }


#### Walk through the lists and parse the PDFs ####
# tic("Normal walk2")
# walk2(file_names, save_names, ~ get_text_from_PDFs(.x, .y))
# toc()

safely_split_columns <- safely(split_columns)


tic("Furrr walk2 stringr")
future_walk2(file_names,
             save_names,
             ~ safely_split_columns(.x, .y),
             .progress = TRUE)
toc()
