#### Preamble ####
# Purpose: This file takes Australian Hansard text file CSVs and mashes them all together.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 28 August 2018
# Prerequisites: You a folder of csvs e.g. the outputs of make_text_Tibble_from_text_files.R
# To do:


#### Set up workspace ####
library(tidyverse)
# update.packages()


#### Create the lists of filenames ####
# Get list of Hansard PDF filenames
file_names <-
  list.files(
    #path = "/Volumes/SanDisk/hansard_words/csv", # Use this for all of them, comment to just run on test group
    path = "outputs/for_testing_hansard_cleaned_text_files",
    # Use this for testing, comment to get all
    pattern = "*.csv",
    recursive = TRUE,
    full.names = TRUE
  )
file_names

list_of_csvs <- map(file_names, read.csv)

csv_dates <- file_names %>% 
  str_replace(".csv", "") %>% 
  # str_replace("/Volumes/SanDisk/hansard_words/", "")
  str_replace("outputs/for_testing_hansard_cleaned_text_files/", "")


#### Create the function ####
# Read each dataframe into that list
masher <- function(csv_to_add,
                   date_of_hansard_for_that_csv) {
  one_days_data <- read_csv(file = csv_to_add, col_types = "cc")
  all_words_in_that_day <-
    paste(one_days_data$stem_word, collapse = ", ")
  days_words <-
    tibble(date = date_of_hansard_for_that_csv, words = all_words_in_that_day)
  days_words
}


#### Use the function ####
# Initialise a list that will hold them
my_data <- list()

# Use the function
my_data <- map2(file_names, csv_dates, ~ masher(.x, .y))


#### Bind the elements and save them ####
# Bind all the dataframes together
words_in_hansard_by_date <- bind_rows(my_data)

# Save the big dataframe
save(words_in_hansard_by_date, file = "outputs/words.Rda")
