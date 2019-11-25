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
# use_this_path_to_get_csvs  <- "/Volumes/Hansard/parsed/federal/senate/run_6_output"
use_this_path_to_get_csvs <- "/Volumes/Hansard/parsed/federal/hor/run_6_output"

# Get list of Hansard csvs
file_names <-
  list.files(
    path = use_this_path_to_get_csvs,
    pattern = "*.csv",
    recursive = FALSE,
    full.names = TRUE
  )

file_names <- file_names %>% sample() # Randomise the order

# Just use this to filter if needed
file_tibble <- tibble(filename = file_names)
file_tibble <- file_tibble %>%
  mutate(the_year = filename,
         the_year = str_replace(the_year, "/Volumes/Hansard/parsed/federal/senate/run_6_output/", ""),
         the_year = str_replace(the_year, "/Volumes/Hansard/parsed/federal/hor/run_6_output/", ""),
         the_year = str_replace(the_year, ".csv", ""),
         the_year = ymd(the_year)) %>%
  # filter(year(the_year) < 1981) %>%
  filter(year(the_year) == 2015)
file_names <- file_tibble$filename
rm(file_tibble)


# use_this_path_to_save_csvs  <- "outputs/hansard/run_7_output"
# use_this_path_to_save_csvs  <- "/Volumes/Hansard/parsed/federal/senate/run_7_output"
use_this_path_to_save_csvs <- "/Volumes/Hansard/parsed/federal/hor/run_7_output"
save_names <- file_names %>%
  str_replace(use_this_path_to_get_csvs, use_this_path_to_save_csvs)


#### Create the function that will be applied to the files ####
general_clean_up <-
  function(name_of_input_csv_file,
           name_of_output_csv_file) {
    # Read in the csv, based on the filename list
    # name_of_input_csv_file <- "/Volumes/Hansard/parsed/federal/for_zoe/run_6_output/2016-09-14.csv" # uncomment for testing
    # name_of_input_csv_file <- "outputs/hansard/temp/hor-2016-11-23.csv" # uncomment for testing
    
    csv_to_clean <-
      read_csv(name_of_input_csv_file,
               trim_ws = FALSE,
               col_types = cols())
    
    # Based on hor-2017-10-26:
    # First look for ones like: "Dawson) (10:42):"
    csv_to_clean$text <- str_remove(csv_to_clean$text, "^.{0,60}\\)[:space:]\\([:digit:]{2}:[:digit:]{2}\\)\\:")
    # Then for ones like: "Mr Rob Mitchell):"
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^.{0,30}\\)\\:", "")
    # Bit specific, but Dan Tehan has the longest title I've ever seen: Wannon-Minister for Veterans' Affairs, Minister Assisting the Prime Minister for the Centenary of ANZAC, Minister Assisting the Prime Minister for Cyber Security and Minister for Defence Personnel) (12:04):
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Wannon-Minister for Veterans\\' Affairs, Minister Assisting the Prime Minister for the Centenary of ANZAC, Minister Assisting the Prime Minister for Cyber Security and Minister for Defence Personnel\\)[:space:]\\([:digit:]{2}:[:digit:]{2}\\):", "")

    #Based on hor-2016:
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Stirling-Minister for Justice and Minister Assisting the Prime Minister for Counter-Terrorism\\)[:space:]\\([:digit:]{2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Farrer-Minister for Sport and Minister for Health and Aged Care\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^New England-Leader of the National Party of Australia\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Higgins-Minister for Small Business and Assistant Treasurer\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Sturt-Leader of the House and Minister for Education and Training\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Paterson-Parliamentary Secretary to the Minister for the Environment\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Wide Bay-Deputy Prime Minister and Minister for Infrastructure and Regional Development\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Moncrieff-Parliamentary Secretary to the Minister for Foreign Affairs and ParliamentarySecretary to the Minister for Trade and Investment\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Gippsland-Parliamentary Secretary to the Minister for Defence\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Cowper-Deputy Leader of the House and Assistant Minister for Employment\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Cowper-Deputy Leader of the House and Assistant Minister for Employment\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^MCPherson-Parliamentary Secretary to the Minister for Industry and Science\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Farrer-Minister for Health, Minister for Sport and Minister for Aged Care\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^MCPherson-Parliamentary Secretary to the Minister for Industry and Science\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Forde\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Wide Bay-Deputy Prime Minister and Minister for Infrastructure and Regional Development\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Fadden-Minister for Veterans' Affairs, Minister for Human Services and Minister Assisting the Prime Minister for the Centenary of ANZAC\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Sturt-Leader of the House, Minister for Industry, Innovation and Science\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Sturt-Leader of the House, Minister for Industry and Innovation and Science\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Sturt-Leader of the House and Minister for Industry, Innovation and Science\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Stirling-Minister for Justice and Minister Assisting the Prime Minister on Counter-Terrorism\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Stirling-Minister for Justice and Minister Assisting the Prime Minister on Counter-Terrorism\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Cowper-Minister for Vocational Education and Skills and Deputy Leader of the House\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Cowper-Deputy Leader of the House and Assistant Minister for Employment\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Cowper-Minister for Vocational Education and Skills and Deputy Leader of the House\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Cowper-Deputy Leader of the House and Assistant Minister for Employment\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Kooyong-Minister for Resources, Energy and Northern Australia\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Bradfield-Parliamentary Secretary to the Minister for Communications\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Bradfield-Parliamentary Secretary to the Minister for Communications\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Bradfield-Minister for Territories, Local Government and Major Projects\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Moncrieff-Minister for International Development and the Pacific\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Moncrieff-Parliamentary Secretary to the Minister for Foreign Affairs and ParliamentarySecretary to the Minister for Trade and Investment\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Moncrieff-Parliamentary Secretary to the Minister for Foreign Affairs and Parliamentary Secretary to the Minister for Trade and Investment\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Moncrieff-Minister for International Development and the Pacific\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Gippsland-Parliamentary Secretary to the Minister for Defence\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Gippsland-Parliamentary Secretary to the Minister for Defence\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Fisher-Minister for Defence Materiel and Science and Special Minister of State\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Fisher-Minister for Defence Materiel and Science and Special Minister of State\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Mayo-Assistant Minister for Infrastructure and Regional Development\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Mayo-Assistant Minister for Infrastructure and Regional Development\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Paterson-Parliamentary Secretary to the Minister for the Environment\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^Paterson-Parliamentary Secretary to the Minister for the Environment\\)[:space:]\\([:digit:]{1,2}:[:digit:]{2}\\):", "")

    
    # Similarly, but earlier dates used - not :" also Calwell) (9.52 a.m.)- dots with the am or pm.
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^.{0,60}\\)[:space:]\\([:digit:]{1,2}\\.[:digit:]{1,2}[:space:]?[:letter:]\\.m\\.\\)[:space:]?\\-", "")
    
    # Then for ones like: "Mr Rob Mitchell):"
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^.{0,30}\\)\\:", "")
    
    # Wannon-Prime these Minister)- 
    # or 
    # Macquarie) -
    # or 
    # North Sydney) - 
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^.{0,60}\\)[:space:]?\\-", "")
    
    
    
    # Gwydu-Minister for Health) (2.21 ) - 
    csv_to_clean$text <- str_replace(csv_to_clean$text, "^.{0,60}\\)[:space:]\\([:digit:]{1,2}\\.[:digit:]{1,2}[:space:]?\\)[:space:]?\\-", "")
    
    # Clean up from earlier
    csv_to_clean$text <- str_replace_all(csv_to_clean$text, "EMPTYHERE", "")
    csv_to_clean$text <- str_replace_all(csv_to_clean$text, "EmptyHere", "")
    csv_to_clean$text <- str_replace_all(csv_to_clean$text, "MONICA", "")
    
    
    
    # Clean the names of whitespace
    csv_to_clean$Speaker <-
      str_squish(csv_to_clean$Speaker)
    
    
    # Clean the names of whitespace
    csv_to_clean$text <-
      str_replace_all(csv_to_clean$text, "\\( ", "\\(")
    
    # csv_to_clean <- csv_to_clean %>% 
    #   select(-speakerGroups)
    # 
    # write_csv(csv_to_clean, "2016-09-14.csv")
    
    # Need to deal with interjecting....  
    # "Mr Brendan O'Connor interjectingMONICA"
    # Come back to hor 2016 - something weird going on with Sharkie.
    
    
    
    # Save file
    write_csv(csv_to_clean, name_of_output_csv_file)
    
    print(paste0("Done with ", name_of_output_csv_file, " at ", Sys.time()))
  }

safely_general_clean_up <- safely(general_clean_up)


#### Walk through the lists and parse the PDFs ####
# tic("Normal walk2")
# walk2(file_names,
#       save_names,
#       ~ safely_general_clean_up(.x, .y))
# toc()


tic("Furrr walk2")
future_walk2(file_names,
             save_names,
             ~ safely_general_clean_up(.x, .y),
             .progress = TRUE)
toc()
