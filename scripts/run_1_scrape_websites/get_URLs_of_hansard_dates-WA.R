#### Preamble ####
# Purpose: I threw a wish (initial scraper) in the well (Hansard website), Don't ask me I'll never tell (because my code is embarrasing), I looked to you as it fell (threw errors), And now you're (Hansard website) in my way (why don't you just have a bulk downloader?), I'd trade my soul for a wish (bulk downloader), Pennies and dimes for a kiss (bulk downloader), I wasn't looking for this (scraping expedition), But now you're in my way (and a conference deadline is imminent).
# The Hansard website has some tres changeable urls for the PDFs so it seems less uncertain to just to get the explicit address of the PDFs instead of using a rule. This scraper gets those URLs. This isn't pretty, but it works.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 18 September 2018
# Prerequisites: You need the pages of the search results.
# To do: 


#### Set up workspace ####
library(tidyverse)
library(rvest)


#### Pages of search results that list the PDFs ####

pages_to_scrape <- read_csv("scripts/to_scrape_websites/addresses/wa_urls_to_get_links.csv")


#### Write a function that rests, then finds the URL of the PDFs on the search result page ####
getter <- function(search_result) {
  Sys.sleep(20)
  read_html(search_result) %>% 
    html_nodes("font a") %>%
    html_attr('href') %>% 
    as_tibble()
}

safe_getter <- safely(getter)

#### Map ####
# Initialise an empty list to hold the results
my_data <- list()

# Use the function - it applies the 'getter' function to each element of the pages list and stores the result of each element in an element of the my_data list.
pages_to_scrape <- pages_to_scrape[1:2,] # for testing
# pages <- pages[3:length(pages)]

my_data <- map(pages_to_scrape$URL, ~ safe_getter(.x))

my_data_transpose <- transpose(my_data)  

# Combine all the results and write to a csv
wa_hansard_pdfs <- bind_rows(my_data_transpose$result)

write_csv(wa_hansard_pdfs, "links.csv")






# Need to get 1995 on...

