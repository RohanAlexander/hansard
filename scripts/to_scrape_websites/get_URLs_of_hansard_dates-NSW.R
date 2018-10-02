#### Preamble ####
# Purpose: To get the URLs for the NSW PDFs
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 2 October 2018
# Prerequisites: -
# To do: 


#### Set up workspace ####
library(jsonlite)
library(lubridate)
library(tidyverse)
library(rvest)
# update.packages()


#### Pages of search results that list the PDFs ####
years <- c(1879:2017)
pages_to_GET <- paste0("https://api.parliament.nsw.gov.au/api/hansard/search/year/", years)
# pages_to_GET <- pages_to_GET[1]


# Thanks to https://tclavelle.github.io/blog/r_and_apis/ and https://data.metinyazici.org/2017/10/working-with-web-data-in-r.html and https://stackoverflow.com/questions/43127114/sys-sleep-using-function-possibly-from-purrr-package and https://tclavelle.github.io/blog/r_and_apis/
# pages_to_GET <- pages_to_GET[1:3]
fromJSON_responses <- map(pages_to_GET, ~{
  # pb$tick()$print()
  Sys.sleep(20)
  fromJSON(txt = .x)
}) 

fromJSON_responses

nsw_hansard_tibble <- bind_rows(fromJSON_responses) %>% 
  unnest(Events) %>% 
  as_tibble()

nsw_hansard_tibble <- nsw_hansard_tibble %>% 
  mutate(date = ymd(date)) %>%
  mutate(chamber_type = case_when(
    Chamber == "Legislative Assembly" ~ "A",
    Chamber == "Legislative Council" ~ "C",
    TRUE ~ "Unsure"
  )) %>% 
  select(-style, -tooltip, -TocDocId, -Uncorrected)

nsw_hansard_tibble <- nsw_hansard_tibble %>%
  mutate(URL = case_when(
    year(date) >= 1991 ~ paste0("https://api.parliament.nsw.gov.au/api/hansard/search/daily/pdf/", PdfDocId),
    year(date) < 1991 ~ paste0("https://api.parliament.nsw.gov.au/api/hansard/search/daily/searchablepdf/", PdfDocId),
    TRUE ~ "Unsure"
  ))

nsw_hansard_tibble <- nsw_hansard_tibble %>%
  mutate(save_name = paste0("NSW-", date, "-", chamber_type, ".pdf"))

head(nsw_hansard_tibble)
tail(nsw_hansard_tibble)

write_csv(nsw_hansard_tibble, "scripts/to_scrape_websites/addresses/NSW_addresses.csv")