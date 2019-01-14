# !diagnostics off
#### Preamble ####
# Purpose: This file takes Australian Hansard CSV files and it splits the ones that were in two column format. The PDFs until (not including) 2013-11-12 are arranged as two columns on each page and so most rows are two different speeches and those columns need to be separated.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 17 October 2018
# Prerequisites: You need to have downloaded the PDFs and read them from PDF to a CSV. For testing purposes there should be some in the /outputs/hansard/ folder.
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
use_this_path_to_get_csvs  <- "outputs/hansard/run_1_output"
# use_this_path_to_get_csvs <- "/Volumes/Hansard/parsed/federal/hor"

# Get list of Hansard csvs that have been parsed from PDFs and had front matter removed
file_names <-
  list.files(
    path = use_this_path_to_get_csvs,
    pattern = "*.csv",
    recursive = FALSE,
    full.names = TRUE
  )

file_names <- file_names %>% sample() # Randomise the order

# The two columns is only an issue up to 12 November 2013 so remove the file if it is after 2013-11-12
file_names_tibble <- tibble(file_name = file_names, file_date = file_names) %>% 
  mutate(file_date = str_replace(file_date, ".csv", ""),
         file_date = str_replace(file_date, "outputs/hansard/run_1_output/hor-", ""),
         file_date = str_replace(file_date, "outputs/hansard/run_1_output/senate-", ""),
         file_date = ymd(file_date)) %>% 
  filter(file_date > "2013-11-12")

file_names <- file_names_tibble$file_name
rm(file_names_tibble)

#Sometimes it's useful to seperate the input and the output, but otherwise might prefer to overwrite - if you split it make sure to get the one column ones into the new folder
use_this_path_to_save_csvs  <- "outputs/hansard/run_2_output"
# use_this_path_to_save_csvs  <- "/Volumes/Hansard/parsed/federal/hor"
save_names <- file_names %>%
  str_replace(use_this_path_to_get_csvs, use_this_path_to_save_csvs)


#### Create the function that will be applied to the files ####
split_columns <-
  function(name_of_input_csv_file,
           name_of_output_csv_file) {
    # Read in the csv, based on the filename list
    # name_of_input_csv_file <- "outputs/hansard/temp/1971-03-30.csv" # uncomment for testing
    # name_of_input_csv_file <- "senate.csv" # uncomment for testing
    
    csv_to_split <-
      read_csv(name_of_input_csv_file,
               trim_ws = FALSE,
               col_types = cols())
    
    write_csv(csv_to_split, name_of_output_csv_file)
    
    print(paste0("Done with ", name_of_output_csv_file, " at ", Sys.time()))
    
    
  }

safely_split_columns <- safely(split_columns)


#### Walk through the lists and parse the PDFs ####
tic("Normal walk2")
walk2(file_names,
      save_names,
      ~ safely_split_columns(.x, .y))
toc()


#
# tic("Furrr walk2")
# future_walk2(file_names,
#              save_names,
#              ~ safely_split_columns(.x, .y),
#              .progress = TRUE)
# toc()
