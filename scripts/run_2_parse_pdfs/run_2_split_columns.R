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


#### Create lists of CSVs to read ####
# Change the path as required:
# use_this_path_to_get_csvs  <- "/Volumes/Backup/hansard_pdfs"
use_this_path_to_get_csvs  <- "outputs/hansard/temp"

# Get list of Hansard csvs that have been parsed from PDFs and had front matter removed
file_names <-
  list.files(
    path = use_this_path_to_get_csvs,
    pattern = "*.csv",
    recursive = FALSE,
    full.names = TRUE
  )

file_names <- file_names %>% sample() # Randomise the order

# Seems as though this is only an issue up to 12 November 2013
# Remove the file if it is after 2013-11-12
file_names_tibble <- tibble(file_name = file_names)
file_names_tibble <- file_names_tibble %>% 
  mutate(date = basename(file_names) %>% 
           str_replace(".csv", "") %>%
           ymd()) %>% 
  filter(date < "2013-11-12")

file_names <- file_names_tibble$file_name
rm(file_names_tibble)

use_this_path_to_save_csvs  <- "outputs/hansard/temp/testing"
save_names <- file_names %>%
  str_replace(use_this_path_to_get_csvs, use_this_path_to_save_csvs)


#### Create the function that will be applied to the files ####
split_columns <-
  function(name_of_input_csv_file,
           name_of_output_csv_file) {
    # Read in the csv, based on the filename list
    # name_of_input_csv_file <- "outputs/hansard/temp/1971-10-05.csv" # uncomment for testing
    csv_to_split <-
      read_csv(name_of_input_csv_file,
               trim_ws = FALSE,
               col_types = cols())
    
    # Work out whether a line has two columns in it or only the left or only the right.
    # This is done based on the number of characters in the column.
    # The benefit of the doubt is given to having both.
    csv_to_split <- csv_to_split %>%
      mutate(
        line_type = case_when(
          str_detect(text, "^\\s{26,}") == TRUE ~ "secondColumnOnly",
          # If there is at least 26 spaces in a row at the start then assume there is only content in the second column.
          # 26 seems to be the minimum, but could fine-tune this.
          nchar(text) < 48 ~ "firstColumnOnly",
          # If there is less than 48 characters then assume there is only content in the first column.
          # We do another parse later on.
          # But, again, could fine-tune this.
          TRUE ~ "both" # Otherwise assume there is content in both columns for now
        )
      )
    
    # Add a counter column so that the order can be reconstructed later.
    csv_to_split <- csv_to_split %>%
      mutate(ordering = 1:nrow(csv_to_split))
    
    # First look at the lines that we think have two columns
    lines_with_both_columns <- csv_to_split %>%
      filter(line_type == "both")
    # When there is at least two spaces towards the middle of the row then add the splitter there
    lines_with_both_columns <- lines_with_both_columns %>%
      mutate(
        text = str_replace(text, "(?<=(.){37,60})[:space:]{2,}", "MONICAHOWLETT"),
        split = str_detect(text, "MONICAHOWLETT")
      )
    
    # When there is not at least two spaces then search for the space that is closest to the middle and add the splitter there
    # First get the length of each line
    lines_with_both_columns_middle_space <-
      lines_with_both_columns %>%
      filter(split == FALSE) %>%
      mutate(
        length = nchar(text),
        bit_to_the_left_of_half = as.integer(length / 2 - 3),
        bit_to_the_left_of_half_plus_10 = bit_to_the_left_of_half + 10,
        reg_pattern = paste0(
          "(?<=(.){",
          bit_to_the_left_of_half,
          ",",
          bit_to_the_left_of_half_plus_10,
          "})[:space:]"
        )
      ) %>%
      select(-bit_to_the_left_of_half,
             -bit_to_the_left_of_half_plus_10)
    # Looking at the histogram - ggplot(lines_with_both_columns_middle_space, aes(x = length)) + geom_bar() - and then the rows, when there is less than 60 it seems to be just left column. Change the type to be first column only and remove them
    # If it's less than 60 then change the type to be first column only
    lines_with_both_columns_middle_space <-
      lines_with_both_columns_middle_space %>%
      mutate(line_type = if_else(length < 60, "firstColumnOnly", line_type))
    # Need to deal with these later
    lines_with_left_only_but_thought_both <-
      lines_with_both_columns_middle_space %>%
      filter(line_type == "firstColumnOnly")
    lines_with_both_columns_middle_space <-
      lines_with_both_columns_middle_space %>%
      filter(line_type != "firstColumnOnly")
    # Then add the splitter
    lines_with_both_columns_middle_space <-
      lines_with_both_columns_middle_space %>%
      mutate(
        text = str_replace(
          text,
          lines_with_both_columns_middle_space$reg_pattern,
          "MONICAHOWLETT"
        ),
        split = str_detect(text, "MONICAHOWLETT")
      ) %>%
      select(-reg_pattern,-length)
    
    # Push those back together
    lines_with_both_columns <-
      rbind(lines_with_both_columns,
            lines_with_both_columns_middle_space)
    
    # Now split based on the splitter
    lines_with_both_columns <- lines_with_both_columns %>%
      separate(
        text,
        c("first_column", "second_column"),
        sep = "MONICAHOWLETT",
        remove = FALSE,
        extra = "merge"
      ) %>%
      select(first_column, second_column, pageNumbers, ordering)
    
    # Now deal with the lines that we think only have a second column
    lines_with_only_second_columns <- csv_to_split %>%
      filter(line_type == "secondColumnOnly") %>%
      mutate(first_column = NA, second_column = text) %>%
      select(first_column, second_column, pageNumbers, ordering)
    
    # Now deal with the lines that we think only have a first column
    lines_with_only_first_columns <- csv_to_split %>%
      filter(line_type == "firstColumnOnly") %>%
      mutate(first_column = text, second_column = NA) %>%
      select(first_column, second_column, pageNumbers, ordering)
    
    # Now deal with the lines that we think only have a first column but that were hiding in the both dataset
    lines_with_left_only_but_thought_both  <-
      lines_with_left_only_but_thought_both %>%
      mutate(first_column = text, second_column = NA) %>%
      select(first_column, second_column, pageNumbers, ordering)
    
    # head(lines_with_only_first_columns)
    # head(lines_with_only_second_columns)
    # head(lines_with_left_only_but_thought_both)
    
    # Merge them all into one
    all_one_column <-
      bind_rows(
        lines_with_both_columns,
        lines_with_only_first_columns,
        lines_with_left_only_but_thought_both,
        lines_with_only_second_columns
      ) %>%
      arrange(ordering)
    
    rm(
      lines_with_both_columns,
      lines_with_only_first_columns,
      lines_with_left_only_but_thought_both,
      lines_with_only_second_columns
    )
    
    # head(all_one_column)
    
    # Now the columns of the PDF are split into two columns and we need to put them back together in order; within each page: first column on top then second column
    all_one_column <- all_one_column %>%
      select(-ordering) %>%
      gather(position, textInPosition, -c(pageNumbers)) %>%
      mutate(counter = 1:n()) %>%
      arrange(pageNumbers, counter)
    
    # Check for completely empty rows and remove them
    all_one_column <- all_one_column %>%
      mutate(emptyCell = if_else(textInPosition == "" |
                                   is.na(textInPosition), 1, 0)) %>%
      filter(emptyCell == 0) %>%
      select(-emptyCell, -position, -counter) %>%
      rename(text = textInPosition)
    
    # Remove any extra whitespace i.e. two or more spaces and spaces at either end
    all_one_column$text <-
      str_squish(all_one_column$text)
    
    # Finally, put everything onto one line, which allows us to rejoin words that have been split across two lines e.g. Mon- ica
    all_one_column <- all_one_column %>%
      mutate(text = str_replace_all(text, "-$", "MONICA"))
    
    full_days_hansard <- all_one_column %>%
      select(-pageNumbers) %>%
      summarise(text = paste(text, collapse = " ")) %>%
      mutate(text = str_replace_all(text, "MONICA ", "")) %>%
      mutate(text = str_replace_all(text, "[:space:]-(?=[:alpha:])", " - "))
    
    write_csv(full_days_hansard, name_of_output_csv_file)
    
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
