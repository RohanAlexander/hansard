#### Preamble ####
# Purpose: This file gets Australian Hansard PDF files for the senate from the parliament website.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 10 September 2018
# Prerequisites: Need a list of csvs to walk through - URLs_for_1901-2017_PDFs_senate.csv
# To do: -


#### Set up workspace ####
# Load libraries
library(tidyverse)
# update.packages()


#### Create lists of URLs to scrape and file names to save the PDF as ####
data_to_scrape <- read_csv("scripts/to_scrape_websites/addresses/URLs_for_1981_and_1960.csv")

data_to_scrape <- data_to_scrape %>% 
  filter(file_name %in% c("1960-12-06.pdf", "1960-12-01.pdf", "1960-11-30.pdf", "1960-11-29.pdf", "1960-11-24.pdf", "1960-11-23.pdf", "1960-11-22.pdf", "1960-11-17.pdf", "1960-11-16.pdf", "1960-11-15.pdf", "1960-11-10.pdf", "1960-11-09.pdf", "1960-11-08.pdf", "1960-04-28.pdf", "1960-04-27.pdf", "1960-04-07.pdf", "1960-09-22.pdf", "1960-09-21.pdf", "1960-09-20.pdf"))

address_to_visit <- data_to_scrape$URL
save_name <- data_to_scrape$file_name

save_name <- paste0("pdfs/", save_name)

dir.create("pdfs") # Creates the directory - throws warning but does not overwrite if there's already a directory


#### Create the function that will visit address_to_visit and save to save_name files ####
visit_address_and_save_PDF <-
  function(name_of_address_to_visit,
           name_of_where_to_save) {
    download.file(name_of_address_to_visit, name_of_where_to_save)
    print(paste("Done with", name_of_where_to_save, "at", Sys.time()))  # Helpful so that you know progress when running it on all the records
    Sys.sleep(sample(15:30, 1)) # Space out requests by somewhere between 15 and 30 seconds each
  }

safe_visit_address_and_save_PDF <- safely(visit_address_and_save_PDF)


#### Walk through the lists and get the PDFs ####
# save_name <- save_name[1:2] # testing it works
# address_to_visit <- address_to_visit[1:2] # testing it works
# save_name <- save_name[3:length(save_name)] # main run
# address_to_visit <- address_to_visit[3:length(address_to_visit)] # main run

walk2(address_to_visit, save_name, ~ safe_visit_address_and_save_PDF(.x, .y))

