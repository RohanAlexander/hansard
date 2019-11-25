# !diagnostics off
#### Preamble ####
# Purpose: This file takes Australian Hansard CSV files and isolates the name of the person speaking into a separate column.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 5 December 2018
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

# Get the name fixer
fix_wrong_names <-
  read_csv2("inputs/misc/wrong_names_with_corrections.csv") %>%
  mutate(numberOfCharacters = nchar(original)) %>%
  arrange(desc(numberOfCharacters)) %>%
  select(-numberOfCharacters)


#### Create lists of CSVs to read ####
# Change the path as required:
use_this_path_to_get_csvs  <- "/Volumes/Hansard/parsed/federal/hor/run_2_output"
# use_this_path_to_get_csvs  <- "/Volumes/Hansard/parsed/federal/senate/run_2_output"

# Get list of Hansard csvs that have been parsed from PDFs and had front matter removed
file_names <-
  list.files(
    path = use_this_path_to_get_csvs,
    pattern = "*.csv",
    recursive = FALSE,
    full.names = TRUE
  )

file_names <- file_names %>% sample() # Randomise the order

# file_names <- "/Volumes/Hansard/parsed/federal/hor/run_2_output/2015-02-09.csv"

# Just use this to filter if needed
file_tibble <- tibble(filename = file_names)
file_tibble <- file_tibble %>%
  mutate(the_year = filename,
         the_year = str_replace(the_year, "/Volumes/Hansard/parsed/federal/senate/run_2_output/", ""),
         the_year = str_replace(the_year, "/Volumes/Hansard/parsed/federal/hor/run_2_output/", ""),
         the_year = str_replace(the_year, ".csv", ""),
         the_year = ymd(the_year)) %>%
  # filter(year(the_year) < 1981) %>%
  filter(year(the_year) > 2015)
file_names <- file_tibble$filename
rm(file_tibble)


# use_this_path_to_save_csvs  <- "/Volumes/Hansard"
# Seems unnecessary, but sometimes useful to separate input and output
# use_this_path_to_save_csvs  <- "/Volumes/Hansard/parsed/federal/senate/run_3_output"
use_this_path_to_save_csvs <- "/Volumes/Hansard/parsed/federal/hor/run_3_output"
save_names <- file_names %>%
  str_replace(use_this_path_to_get_csvs, use_this_path_to_save_csvs)



#### Create the function that will be applied to the files
split_columns <-
  function(name_of_input_csv_file,
           name_of_output_csv_file) {
    # Read in the csv, based on the filename list
    # name_of_input_csv_file <- "/Volumes/Hansard/parsed/federal/hor/run_2_output/2015-02-09.csv" # uncomment for testing
    # name_of_input_csv_file <- "outputs/hansard/run_2_output/hor-2016-11-23.csv" # uncomment for testing
    
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
      str_replace_all(
        full_days_hansard$text,
        "M[:space:]?r[:space:]?s[:punct:]?+[:space:]",
        "Mrs ") # Looks for "Mrs. ", or "M rs. ", etc, (the full stop is any punctuation) and replaces it with "Mrs "
    full_days_hansard$text <-
      str_replace_all(
        full_days_hansard$text,
        "M[:space:]?r[:space:]*[:punct:]*[:space:]",
        "Mr ") # Change, "Mr. " to "Mr "
    full_days_hansard$text <-
      str_replace_all(
        full_days_hansard$text, 
        "S i r", 
        "Sir")
    full_days_hansard$text <-
      str_replace_all(
        full_days_hansard$text, 
        "Mc(?=[:upper:]+)", 
        "MC") # Change McDONALD to MCDONALD
    full_days_hansard$text <-
      str_replace_all(
        full_days_hansard$text, 
        "M c (?=[:upper:]+)", 
        "MC") # Change M c DOUGALL to MCDONALD
    full_days_hansard$text <-
      str_replace_all(
        full_days_hansard$text, 
        "Mac(?=[:upper:]+)", 
        "MAC") # Change MacDONALD to MACDONALD
    full_days_hansard$text <-
      str_replace_all(
        full_days_hansard$text, 
        "(?<=\\S)Mr", 
        " Mr") # If there is not a space to the left of Mr then put one there
    full_days_hansard$text <-
      str_replace_all(
        full_days_hansard$text, 
        "O'KEEFE", 
        "OKEEFE") 
    full_days_hansard$text <-
      str_replace_all(
        full_days_hansard$text, 
        "(?<=\\S)Sir", 
        " Sir") # Again, if there is not a space to the left of Sir then put one there
    full_days_hansard$text <-
      str_replace_all(
        full_days_hansard$text,
        "D[:space:]?r[:space:]*[:punct:]",
        "Dr") # Change, "Dr." to "Dr"
    full_days_hansard$text <-
      str_replace_all(
        full_days_hansard$text,
        "Mi-\\.",
        "Mr")   
    
    # These are less general, but have a semi colon in the middle which was annoying the csv of the other less general ones
    full_days_hansard$text <-
      str_replace_all(
        full_days_hansard$text,
        "Mr SYDNEY; SMITH",
        "Mr SYDNEY SMITH") 
    full_days_hansard$text <-
      str_replace_all(
        full_days_hansard$text,
        "to encourage such industries; Mr JONES",
        "Mr JONES") 
    full_days_hansard$text <-
      str_replace_all(
        full_days_hansard$text,
        "PAIRS; Mr SPEAKER",
        "Mr SPEAKER") 
    
    full_days_hansard$text <-
      str_replace_all(
        full_days_hansard$text,
        "Ms O'NEIL:",
        "Ms ONEIL:") 
    
    full_days_hansard$text <-
      str_replace_all(
        full_days_hansard$text,
        "Ms O'Neil:",
        "Ms ONEIL:") 
    
    full_days_hansard$text <-
      str_replace_all(
        full_days_hansard$text,
        "Ms Butler:",
        "Ms BUTLER:") 
    
    full_days_hansard$text <-
      str_replace_all(
        full_days_hansard$text,
        "Ms Henderson:",
        "Ms HENDERSON:")
    

    # Sir GRANVILLE RYRIE;-; Sir GRENVILLE RYRIE -
    
    #Fix the names
    full_days_hansard$text <-
      stri_replace_all_regex(
        full_days_hansard$text,
        # "Mr PI",
        fix_wrong_names$original,
        fix_wrong_names$corrected,
        vectorize_all = FALSE
      )
    
    on_the_regular <- paste(
      "(?<=(^Mr [:upper:]{1,30}[:blank:]?))-",
      "(?<=(^Mr [:upper:]{1,30}[:blank:][:upper:]{1,30}[:blank:]?))-",
      "(?<=(^Mr [:upper:]{1,30}[:blank:]?))\\(",
      "(?<=(^Mr [:upper:]{1,30}[:blank:][:upper:]{1,30}[:blank:]?))\\(",
      "(?<=(^Mr [:upper:]{1,30}[:blank:]?(\\(.{1,20})\\)))-",
      "(?<=(^Mrs [:upper:]{1,30}[:blank:]?))-",
      "(?<=(^Mrs [:upper:]{1,30}[:blank:][:upper:]{1,30}[:blank:]?))-",
      "(?<=(^Mrs [:upper:]{1,30}[:blank:]?))\\(",
      "(?<=(^Mrs [:upper:]{1,30}[:blank:][:upper:]{1,30}[:blank:]?))\\(",
      "(?<=(^Ms [:upper:]{1,30}[:blank:]?))-",
      "(?<=(^Ms [:upper:]{1,30}[:blank:][:upper:]{1,30}[:blank:]?))-",
      "(?<=(^Ms [:upper:]{1,30}[:blank:]?))\\(",
      "(?<=(^Ms [:upper:]{1,30}[:blank:][:upper:]{1,30}[:blank:]?))\\(",
      "(?<=(^Dr [:upper:]{1,30}[:blank:]?))-",
      "(?<=(^Dr [:upper:]{1,30}[:blank:][:upper:]{1,30}[:blank:]?))-",
      "(?<=(^Dr [:upper:]{1,30}[:blank:]?))\\(",
      "(?<=(^Dr [:upper:]{1,30}[:blank:]?(\\(.{1,20})\\)))-",
      "(?<=(^Dr [:upper:]{1,30}[:blank:][:upper:]{1,30}[:blank:]?))\\(",
      "(?<=(^Sir [:upper:]{1,30}[:blank:]?))-",
      "(?<=(^Madam [:upper:]{1,30}[:blank:]?))-",
      "(?<=(^Sir [:upper:]{1,30}[:blank:][:upper:]{1,30}[:blank:]?))-",
      "(?<=(^Sir [:upper:]{1,30}[:blank:][:upper:]{1,30}[:blank:]?))\\(",
      "(?<=(^Mr [:upper:]{1,30}[:blank:]?))(asked)",
      "(?<=(^Sir [:upper:]{1,30}[:blank:][:upper:]{1,30}[:blank:]?))asked",
      "(?<=(^Mr [:upper:]{1,30}[:blank:][:upper:]{1,30}[:blank:]?))asked",
      "(?<=(^The CHAIRMAN ))-",
      "(?<=(^An HONOURABLE MEMBER[:blank:]?))-",
      "(?<=(^GOVERNMENT SUPPORTERS[:blank:]?))-",
      "(?<=(^Colonel [:upper:]{1,30}[:blank:]?))-",
      "(?<=(^Colonel [:upper:]{1,30}[:blank:]?))asked",
      "(?<=(^Dr [:upper:]{1,30}[:blank:]?))asked",
      "(?<=(^Dame [:upper:]{1,30}[:blank:][:upper:]{1,30}[:blank:]?))-",
      "(?<=(^The [:upper:]{1,30}[:blank:][:upper:]{1,30}[:blank:]?))-",
      "(?<=(^The [:upper:]{1,30}[:blank:][:upper:]{1,30}[:blank:]?))\\(",
      "(?<=(^The [:upper:]{1,30}[:blank:]?))-",
      "(?<=(^The [:upper:]{1,30}[:blank:]?))\\(",
      "(?<=(^Senator [:upper:]{1,30}[:blank:]?))-",
      "(?<=(^Senator [:lower:]{1,30}[:blank:]?))-",
      "(?<=(^Senator [:lower:]{1,30}[:blank:][:lower:]{1,30}[:blank:]?))-",
      "(?<=(^Senator [:lower:]{1,30}[:blank:]?))-",
      "(?<=(^Senator [:upper:]{1,30}[:blank:]?))\\(",
      "(?<=(^Senator [:upper:]{1,30}[:blank:]?))asked",
      "(?<=(^Senator Sir [:upper:]{1,30}[:blank:]?))-",
      "(?<=(^Senator Sir [:upper:]{1,30}[:blank:][:upper:]{1,30}[:blank:]))\\(",
      "(?<=(^The Clerk))-",
      "(?<=(^The ACTING DEPUTY PRESIDENT ))\\(",
      sep = "|")
    
    on_the_regular_since_may_2011 <- paste(
      "(?<=(^Dr [:upper:]{1,30}[:blank:]?)):",
      "(?<=(^Dr [:upper:]{1,30}[:blank:]?))\\(",
      "(?<=(^Mr [:alpha:]{1,30}[:blank:]?)):",
      "(?<=(^Mr [:upper:]{1,30}[:blank:]?)):",
      "(?<=(^Mr [:upper:]{1,30}[:blank:]?))\\(",
      "(?<=(^Mr [:upper:]{1,30}[:blank:][:upper:]{1,30}[:blank:]?)):",
      "(?<=(^Mr [:upper:]{1,30}[:blank:][:upper:]{1,30}[:blank:]?))\\(",
      "(?<=(^Mrs [:upper:]{1,30}[:blank:]?)):",
      "(?<=(^Mrs [:upper:]{1,30}[:blank:]?))\\(",
      "(?<=(^Mrs [:upper:]{1,30}[:blank:][:upper:]{1,30}[:blank:]?))\\(",
      "(?<=(^Ms [:upper:]{1,30}[:blank:]?)):",
      "(?<=(^Ms [:upper:]{1,30}[:blank:]?))\\(",
      "(?<=(^Ms [:upper:]{1,30}[:blank:][:upper:]{1,30}[:blank:]?)):",
      "(?<=(^Ms [:upper:]{1,30}[:blank:][:upper:]{1,30}[:blank:]?))\\(",
      "(?<=(^Senator [:lower:]{1,30}[:blank:]?)):",
      "(?<=(^Senator [:lower:]{1,30}[:blank:]?)):",
      "(?<=(^Senator [:lower:]{1,30}[:blank:][:lower:]{1,30}[:blank:]?)):",
      "(?<=(^Senator [:upper:]{1,30}[:blank:]?)):",
      "(?<=(^Senator [:upper:]{1,30}[:blank:]?))\\(",
      "(?<=(^Senator [:upper:]{1,30}[:blank:][:upper:]{1,30}[:blank:]?))\\(",
      "(?<=(^Senator Sir [:upper:]{1,30}[:blank:][:upper:]{1,30}[:blank:]?))\\(",
      "(?<=(^The [:upper:]{1,30}[:blank:]?)):",
      "(?<=(^The [:upper:]{1,30}[:blank:]?))\\(",
      "(?<=(^The [:upper:]{1,30}[:blank:][:upper:]{1,30}[:blank:]?)):",
      "(?<=(^The [:upper:]{1,30}[:blank:][:upper:]{1,30}[:blank:]?))\\(",
      "(?<=(^The ACTING DEPUTY PRESIDENT ))\\(",
      "(?<=(^A government member)):",
      sep = "|")

    # From 2011-05-10 the way that the speakers are split becomes :.
    date_of_file <-
      basename(name_of_input_csv_file) %>% str_replace(".csv", "") %>% ymd()
    
    if (date_of_file < "2011-05-10") {
      full_days_hansard <- separate(
        full_days_hansard,
        text,
        into = c("Speaker", "Text"),
        sep = on_the_regular,
        extra = "merge",
        fill = "left",
        remove = TRUE
      )
    } else {
      full_days_hansard <- separate(
        full_days_hansard,
        text,
        into = c("Speaker", "Text"),
        sep = on_the_regular_since_may_2011,
        extra = "merge",
        fill = "left",
        remove = TRUE
      )
    }
    
    
    # write_csv(full_days_hansard, "test.csv") # Just for testing
    write_csv(full_days_hansard, name_of_output_csv_file)
    
    print(paste0("Done with ", name_of_output_csv_file, " at ", Sys.time()))
  }

safely_split_columns <- safely(split_columns)


#### Walk through the lists and parse the PDFs ####
# tic("Normal walk2")
# walk2(file_names,
#       save_names,
#       ~ safely_split_columns(.x, .y))
# toc()

tic("Furrr walk2")
future_walk2(file_names,
             save_names,
             ~ safely_split_columns(.x, .y),
             .progress = TRUE)
toc()
