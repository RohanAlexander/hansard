# !diagnostics off
#### Preamble ####
# Purpose: This file takes Australian Hansard CSV files and attemps to split them into different lines for each speech.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 23 November 2018
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


#### Create lists of CSVs to read ####
# Change the path as required:
use_this_path_to_get_csvs  <- "outputs/hansard/temp"
# use_this_path_to_get_csvs <- "/Volumes/Hansard/parsed/federal/hor"

# Get list of Hansard csvs
file_names <-
  list.files(
    path = use_this_path_to_get_csvs,
    pattern = "*.csv",
    recursive = FALSE,
    full.names = TRUE
  )

file_names <- file_names %>% sample() # Randomise the order

use_this_path_to_save_csvs  <- "outputs/hansard/tempp"
# use_this_path_to_save_csvs <- "/Volumes/Hansard/parsed/federal/hor"
save_names <- file_names %>%
  str_replace(use_this_path_to_get_csvs, use_this_path_to_save_csvs)


#### Create the function that will be applied to the files ####
split_lines <-
  function(name_of_input_csv_file,
           name_of_output_csv_file) {
    # Read in the csv, based on the filename list
    # name_of_input_csv_file <- "/Volumes/Backup/temp/1971-10-05.csv" # uncomment for testing
    # name_of_input_csv_file <- "outputs/hansard/temp/1915-07-07.csv" # uncomment for testing
    
    csv_to_clean <-
      read_csv(name_of_input_csv_file,
               trim_ws = FALSE,
               col_types = cols())
    
    csv_to_clean <- separate_rows(csv_to_clean, text, sep = "METHESTART- (?=Mr [:upper:]{2,})", convert = FALSE)
    
    # write_csv(csv_to_clean, "test.csv")
    
    
    
    # Save file
    write_csv(csv_to_clean, name_of_output_csv_file)
    
    print(paste0("Done with ", name_of_output_csv_file, " at ", Sys.time()))
  }


#### Walk through the lists and parse the PDFs ####
# tic("Normal walk2")
# walk2(file_names, save_names, ~ get_text_from_PDFs(.x, .y))
# toc()

safely_fix_spelling <- safely(fix_spelling)



tic("Furrr walk2 stringr")
future_walk2(file_names,
             save_names,
             ~ safely_fix_spelling(.x, .y),
             .progress = TRUE)
toc()
