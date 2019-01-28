# !diagnostics off
#### Preamble ####
# Purpose: This file takes Australian Hansard CSV files and combines everything onto one row, by speaker. The reason for this is that some words have been split across lines e.g. Moni- ca, and so they are treated as two words, but really ought to be combined.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 5 December 2018
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
# install.packages('tm')
library(tm)
# update.packages()
# Set up furrr
plan(multiprocess)


#### Create lists of CSVs to read ####
# Change the path as required:
# use_this_path_to_get_csvs  <- "outputs/hansard/run_4_output"
use_this_path_to_get_csvs  <- "/Volumes/Hansard/parsed/federal/for_zoe/run_4_output"
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

# Seems unnecessary, but sometimes useful to separate input and output
# use_this_path_to_save_csvs  <- "outputs/hansard/run_5_output"
use_this_path_to_save_csvs  <- "/Volumes/Hansard/parsed/federal/for_zoe/run_5_output"
# use_this_path_to_save_csvs <- "/Volumes/Hansard/parsed/federal/hor"
# use_this_path_to_save_csvs <- "/Volumes/Hansard/parsed/federal/hortest"
save_names <- file_names %>%
  str_replace(use_this_path_to_get_csvs, use_this_path_to_save_csvs)


#### Create the function that will be applied to the files ####
combine_rows <-
  function(name_of_input_csv_file,
           name_of_output_csv_file) {
    # Read in the csv, based on the filename list
    # name_of_input_csv_file <- "outputs/hansard/run_4_output/hor-2004-11-29.csv" # uncomment for testing
    
    csv_with_rows_to_combine <-
      read_csv(name_of_input_csv_file,
               trim_ws = FALSE,
               col_types = cols())
    
    # Remove any extra whitespace i.e. two or more spaces and spaces at either end. Yes, I know that I turned this off in the read_csv, but it's important to be explicit because the whitespcae is useful sometimes.
    csv_with_rows_to_combine$Text <-
      str_squish(csv_with_rows_to_combine$Text)
    
    csv_with_rows_to_combine$Title <-
      str_squish(csv_with_rows_to_combine$Title)
    
    # # Put everything onto one line, which allows us to rejoin words that have been split across two lines e.g. Mon- ica
    csv_with_rows_to_combine <- csv_with_rows_to_combine %>%
      mutate(Text = str_replace_all(Text, "-$", "MONICA"))

    # full_days_hansard <- csv_with_rows_to_combine
    
    csv_with_rows_to_combine$speakerCount <-
      if_else(is.na(csv_with_rows_to_combine$Speaker), 0, 1)
    # Some of the speakers were hard to distinguish - this just adds a maximum that we're going to get
    # csv_with_rows_to_combine <- csv_with_rows_to_combine %>%
    #   mutate(text = if_else(Speaker == 1, paste(text, "MONICA"), text))
    csv_with_rows_to_combine <- csv_with_rows_to_combine %>%
      mutate(speakerGroups = cumsum(speakerCount)) %>% 
      select(-speakerCount)
    # Thanks to Mark Needham for this: https://markhneedham.com/blog/2015/06/27/r-dplyr-squashing-multiple-rows-per-group-into-one/
    csv_with_rows_to_combine <- csv_with_rows_to_combine %>%
      group_by(speakerGroups) %>%
      mutate(Title = replace_na(Title, "Unknown")) %>% 
      summarise(Speaker = first(Speaker),
                Title = first(Title),
                text = paste(Text, collapse = " "),
                pageNumbers = min(pageNumbers)
                )
      # summarise(text = paste(text, collapse = " "),
      #           pageNumbers = min(pageNumbers),
      #           title = first(title))
    
    full_days_hansard <- csv_with_rows_to_combine %>%
      # select(-pageNumbers) %>%
      # summarise(text = paste(text, collapse = " ")) %>%
      mutate(text = str_replace_all(text, "MONICA ", "")) %>%
      mutate(text = str_replace_all(text, "[:space:]-(?=[:alpha:])", " - "))

    rm(csv_with_rows_to_combine)
    
    # write_csv(csv_with_rows_to_combine, "text.csv")
    
    write_csv(full_days_hansard, name_of_output_csv_file)
    
    print(paste0("Done with ", name_of_output_csv_file, " at ", Sys.time()))
  }

safely_combine_rows <- safely(combine_rows)


#### Walk through the lists and parse the PDFs ####
# Normal walk2
# tic("Normal walk2")
# walk2(file_names,
#       save_names,
#       ~ safely_combine_rows(.x, .y))
# toc()




# Furrr walk2
tic("Furrr walk2")
future_walk2(file_names,
             save_names,
             ~ safely_combine_rows(.x, .y),
             .progress = TRUE)
toc()
