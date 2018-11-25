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
use_this_path_to_get_csvs  <- "outputs/hansard/temp"
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
use_this_path_to_save_csvs  <- "outputs/hansard/tempp"
# use_this_path_to_save_csvs <- "/Volumes/Hansard/parsed/federal/hor"
save_names <- file_names %>%
  str_replace(use_this_path_to_get_csvs, use_this_path_to_save_csvs)


#### Create the function that will be applied to the files ####
split_columns <-
  function(name_of_input_csv_file,
           name_of_output_csv_file) {
    # Read in the csv, based on the filename list
    # name_of_input_csv_file <-
    #   "outputs/hansard/temp/1971-10-05.csv" # uncomment for testing
    csv_with_rows_to_combine <-
      read_csv(name_of_input_csv_file,
               trim_ws = FALSE,
               col_types = cols())
    
    # Remove any extra whitespace i.e. two or more spaces and spaces at either end. Yes, I know that I turned this off in the read_csv, but it's important to be explicit because the whitespcae is useful sometimes.
    csv_with_rows_to_combine$text <-
      str_squish(csv_with_rows_to_combine$text)
    
    full_days_hansard <- csv_with_rows_to_combine
    
    # Clean the text a little more - toward making it possible to identify the politicians
    full_days_hansard$text <-
      str_replace_all(
        full_days_hansard$text,
        "M[:space:]?r[:punct:]+[:space:]",
        "Mr ") # Looks for "Mr. ", or "M r. " (the full stop is any punctuation) and replaces it with "Mr "
    full_days_hansard$text <-
      str_replace_all(full_days_hansard$text,
                      "M[:space:]?r[:space:]*[:punct:]*[:space:]",
                      "Mr ") # Change, "Mr. " to "Mr "
    full_days_hansard$text <-
      str_replace_all(full_days_hansard$text, 
                      "S i r", 
                      "Sir")
    full_days_hansard$text <-
      str_replace_all(full_days_hansard$text, 
                      "Mc(?=[:upper:]+)", 
                      "MC") # Change McDONALD to MCDONALD
    full_days_hansard$text <-
      str_replace_all(full_days_hansard$text, 
                      "Mac(?=[:upper:]+)", 
                      "MAC") # Change MacDONALD to MACDONALD
    full_days_hansard$text <-
      str_replace_all(full_days_hansard$text, 
                      "(?<=\\S)Mr", 
                      " Mr") # If there is not a space to the left of Mr then put one there
    full_days_hansard$text <-
      str_replace_all(full_days_hansard$text, 
                      "(?<=\\S)Sir", 
                      " Sir") # Again, if there is not a space to the left of Sir then put one there
    # Finally, some special cases
    full_days_hansard$text <-
      str_replace_all(full_days_hansard$text,
                      "WILLIAMM c MILLAN",
                      "WILLIAM MCMILLAN")
    full_days_hansard$text <-
      str_replace_all(full_days_hansard$text,
                      "FitzPATRICK",
                      "FITZPATRICK")
    full_days_hansard$text <-
      str_replace_all(full_days_hansard$text,
                      "Mr Reynolds-",
                      "Mr REYNOLDS-")
    full_days_hansard$text <-
      str_replace_all(full_days_hansard$text,
                      "Mr DEPUTY SPEAKER ( Mr Drury)-",
                      "Mr DEPUTY SPEAKER-")
    full_days_hansard$text <-
      str_replace_all(full_days_hansard$text,
                      "Mr Sinclair-",
                      "Mr SINCLAIR-")
    full_days_hansard$text <-
      str_replace_all(full_days_hansard$text,
                      "Mr BEAZLEY (Fremantle) (4.43)-",
                      "Mr BEAZLEY-")
    full_days_hansard$text <-
      str_replace_all(full_days_hansard$text,
                      "MTHAMER - 1",
                      "Mr HAMMER - I")
    full_days_hansard$text <-
      str_replace_all(full_days_hansard$text,
                      "Mr Foster-",
                      "Mr FOSTER-")
    full_days_hansard$text <-
      str_replace_all(full_days_hansard$text,
                      "WHITLA M",
                      "WHITLAM")
    full_days_hansard$text <-
      str_replace_all(full_days_hansard$text,
                      "^Mr James-",
                      "Mr JAMES-")
    full_days_hansard$text <-
      str_replace_all(full_days_hansard$text,
                      "^Mr Collard-",
                      "Mr COLLARD-")
    full_days_hansard$text <-
      str_replace_all(full_days_hansard$text,
                      "^Mr Cope-",
                      "Mr COPE-")
    full_days_hansard$text <-
      str_replace_all(full_days_hansard$text,
                      "^Mr Bryant-",
                      "Mr BRYANT-")
    full_days_hansard$text <-
      str_replace_all(full_days_hansard$text,
                      "^Mr Whitlam-",
                      "Mr WHITLAM-")
    full_days_hansard$text <-
      str_replace_all(full_days_hansard$text,
                      "^Mr Malcolm Fraser-",
                      "Mr MALCOLM FRASER-")


    
    
    

    # separate(full_days_hansard, text, into = c("Speaker", "Text"), sep = "^[(?=Mr [:upper:]{2,}) -]", fill = "right", remove = FALSE)
    full_days_hansard <- separate(
      full_days_hansard,
      text,
      into = c("Speaker", "Text"),
      sep = "(?<=(Mr [:upper:]{2,30}[:blank:]?))-|(?<=(Mr [:upper:]{2,30}[:blank:][:upper:]{2,30}))-|(?<=(Dr [:upper:]{2,30}[:blank:]?))-|(?<=(Mr [:upper:]{2,30}[:blank:]?(\\(.{1,20})\\)))-|(?<=(Dr [:upper:]{2,30}[:blank:]?(\\(.{1,20})\\)))-|(?<=(Mr [:upper:]{2,30}[:blank:]?))\\(|(?<=(Mr [:upper:]{2,30}[:blank:][:upper:]{2,30}[:blank:]?))\\(", # The third one takes care of "Mr HAYDEN (Oxley)-", the fifth one takes care of "Mr SINCLAIR (New England", the sith one takes care of "Mr DEPUTY SPEAKER ( Mr Drury"
      extra = "merge",
      fill = "left",
      remove = TRUE
    )
    
    
    # 
    # # str_detect("WOOLSHED", "^(^[:lower:])$")
    # ## titles
    # 
    # heh <- separate(
    #   full_days_hansard,
    #   Text,
    #   into = c("Title", "Text"),
    #   sep = "^[^a-z]*$",
    #   extra = "merge",
    #   fill = "left",
    #   remove = FALSE
    # )
    # 
    
    
    # write_csv(full_days_hansard, "test.csv")
    
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
