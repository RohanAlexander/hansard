#### Preamble ####
# Purpose: This file gets NSW Hansard PDF files from the parliament website.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 3 October 2018
# Prerequisites: Need a list of csvs to walk through - NSW_addresses.csv
# To do: -


#### Set up workspace ####
# Load libraries
library(tidyverse)
# update.packages()


#### Create lists of URLs to scrape and file names to save the PDF as ####
data_to_scrape <- read_csv("scripts/to_scrape_websites/addresses/NSW_addresses.csv")

# Randomise the order
set.seed(1234)
data_to_scrape <- data_to_scrape %>% 
  mutate(orderByThisRandom = sample(1:nrow(data_to_scrape))) %>% 
  arrange(orderByThisRandom) %>% 
  select(-orderByThisRandom)

address_to_visit <- data_to_scrape$URL
save_name <- data_to_scrape$file_name

save_name <- paste0("pdfs/", save_name)

dir.create("pdfs")


#### Create the function that will visit address_to_visit and save to save_name files ####
visit_address_and_save_PDF <-
  function(name_of_address_to_visit,
           name_of_where_to_save) {
    download.file(name_of_address_to_visit, name_of_where_to_save)
    print(paste("Done with", name_of_where_to_save, "at", Sys.time()))  # Helpful so that you know progress when running it on all the records
    Sys.sleep(sample(20:30, 1)) # Space out requests by somewhere between 20 and 30 seconds each
  }

safe_visit_address_and_save_PDF <- safely(visit_address_and_save_PDF)


#### Walk through the lists and get the PDFs ####
# save_name <- save_name[1:2] # testing it works
# address_to_visit <- address_to_visit[1:2] # testing it works
# save_name <- save_name[3:length(save_name)] # main run
# address_to_visit <- address_to_visit[3:length(address_to_visit)] # main run

walk2(address_to_visit, save_name, ~ safe_visit_address_and_save_PDF(.x, .y))
