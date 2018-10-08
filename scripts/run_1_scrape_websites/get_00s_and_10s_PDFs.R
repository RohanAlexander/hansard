#### Preamble ####
# Purpose: This file gets Australian Hansard PDF files from the parliament website for the dates that they only make PDFs available.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 27 August 2018
# Prerequisites: 
# To do:


#### Set up workspace ####
# Load libraries
library(tidyverse)
# update.packages()


#### Set up data to scrape ####
data_to_scrape <- read_csv("scripts/get_90s_forgotten_csv_of_URLs.csv")

# the_dates <- data_to_scrape[1:2,] # Change this as appropriate. Have done 1:2 inclusive.
the_dates <- data_to_scrape[3:nrow(data_to_scrape),] # Change this as appropriate. Have done 1:2 inclusive.



#### Scraping ####
for (i in 1:nrow(the_dates)) {
  the_URL <- the_dates$URL[i]
  
  save_here <- paste0("data/PDFs/", the_dates$file_name[i])
  
  download.file(the_URL, save_here)
  
  print(paste(
    "Done with file",
    i,
    "of",
    nrow(the_dates),
    "-",
    the_dates$file_name[i],
    "-",
    "at",
    Sys.time()
  ))  # Helpful so that you know progress when running it on all the records
  
  Sys.sleep(30) # Space out requests by 30 seconds
  
}

