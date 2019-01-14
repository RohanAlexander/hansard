# Misc other:
# Motion (by Mr OHIFUBY). agreed .to-
# Honorable members interjecting,
# Sir Arthur Fadden interjecting,
# Sitting suspended from 5.59 to 8 p.m.
# Mrs Sudmalis interjecting-


# There's a contents pages at the end of 2001-08-09. WTF.
# 2003-02-13.pdf - Not there
# 2001-08-09.pdf - There
# 1998-04-09.pdf - Not there


# !diagnostics off
#### Preamble ####
# Purpose: This file takes Australian Hansard CSV files and isolates the name of the person speaking into a separate column.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 18 December 2018
# Prerequisites: 1) Download the PDFs; 2) read them into CSVs; 3) get rid of the front matter; 4) split the columns. For testing purposes there should be some Hansard PDFs in the /outputs/hansard/ folder.
# To do:


#### Set up workspace ####
# devtools::install_github("DavisVaughan/furrr")
library(furrr)
# install.packages('lubridate')
library(lubridate)
library(stringi)
library(tidyverse)
# install.packages('tictoc')
library(tictoc)
# library(tm)
# update.packages()
# Set up furrr
plan(multiprocess)


#### Create lists of CSVs to read ####
# Change the path as required:
use_this_path_to_get_csvs  <- "outputs/hansard/run_3_output"
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
use_this_path_to_save_csvs  <- "outputs/hansard/run_4_output"
# use_this_path_to_save_csvs <- "/Volumes/Hansard/parsed/federal/hor"
save_names <- file_names %>%
  str_replace(use_this_path_to_get_csvs, use_this_path_to_save_csvs)



#### Create the function that will be applied to the files
split_titles <-
  function(name_of_input_csv_file,
           name_of_output_csv_file) {
    # Read in the csv, based on the filename list
    # name_of_input_csv_file <- "outputs/hansard/temp/1901-07-19.csv" # uncomment for testing
    name_of_input_csv_file <- "outputs/hansard/run_3_output/hor-1913-12-17.csv" # uncomment for testing
    
    csv_with_titles_to_split <-
      read_csv(name_of_input_csv_file,
               trim_ws = FALSE,
               col_types = cols())
    
    # Remove any extra whitespace, just in case.
    csv_with_titles_to_split$Text <-
      str_squish(csv_with_titles_to_split$Text)
    
    full_days_hansard <- csv_with_titles_to_split
    
    # Clean the text a little more - toward making it possible to identify the politicians
    full_days_hansard <- replace_na(full_days_hansard, list(Text = "EmptyHere"))
    
    # Looking for stage directions
    
    regex_for_directions <-
      paste("^Opposition members interjecting",
            "^Sitting suspended from ",
            "^Honourable members interjecting-$",
            "^Government members interjecting-$"
            # "(?<=(^[:^lower:]$))",
            sep = "|")
    
    # str_replace("QUESTIONSWITHOUTNOTICE", "[:upper:]", "")
    
    full_days_hansard <- full_days_hansard %>% 
      mutate(isDirection = if_else(str_detect(Text, regex_for_directions), 1,0),
             Speaker = ifelse(isDirection == 1, "Deus ex machina", Speaker)) %>% 
      select(-isDirection)
    
    
    
    
    
    # Now looks for titles
    regex_for_titles <- "^[[:upper:][:space:]]{4,}$"
      # paste("^[[:upper:][:space:]]{4,}$",
      # 
      #       sep = "|")
    
    # str_replace("QUESTIONSWITHOUTNOTICE", "[:upper:]", "")
    
  
    full_days_hansard <- full_days_hansard %>% 
      mutate(isTitle = if_else(str_detect(Text, regex_for_titles), 1, 0),
             Title = ifelse(isTitle == 1, Text, NA),
             Text = ifelse(isTitle == 1, NA, Text)
             ) %>% 
      select(-isTitle)
    
    
    full_days_hansard <- fill(full_days_hansard, Title)
    
    
    
    # write_csv(full_days_hansard, "test.csv") # Just for testing
    write_csv(full_days_hansard, name_of_output_csv_file)
    
    print(paste0("Done with ", name_of_output_csv_file, " at ", Sys.time()))
  }

safely_split_titles <- safely(split_titles)


#### Walk through the lists and parse the PDFs ####
tic("Normal walk2")
walk2(file_names,
      save_names,
      ~ safely_split_titles(.x, .y))
toc()


# tic("Furrr walk2")
# future_walk2(file_names,
#              save_names,
#              ~ safely_split_columns(.x, .y),
#              .progress = TRUE)
# toc()
