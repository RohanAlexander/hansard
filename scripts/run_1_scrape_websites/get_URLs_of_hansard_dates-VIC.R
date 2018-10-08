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


#### Load a batch of them ####
pages <- c(
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2938-hansard-1856-9",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2939-hansard-1860-1",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2940-hansard-1861-4",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2941-hansard-1864-5",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2864-hansard-1866-7",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2865-hansard-1868-70",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2866-hansard-1871-3",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2867-hansard-1874-6",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2868-hansard-1876-80",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2869-hansard-1880-1",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2870-hansard-1881-2",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2871-hansard-1882-5",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2872-hansard-1886-8",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2873-hansard-1889-91",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2874-hansard-1892-4",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2875-hansard-1894-7",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2876-hansard-1897-1900",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2877-hansard-1901-3",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2878-hansard-1903-5",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2879-hansard-1906-7",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2880-hansard-1908",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2881-hansard-1909-11",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2882-hansard-1911-14",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2883-hansard-1914-17",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2884-hansard-1917-20",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2885-hansard-1920-1",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2887-hansard-1921-4",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2888-hansard-1924-6",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2889-hansard-1927-9",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2890-hansard-1927-9",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2891-hansard-1932-4",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2892-hansard-1935-7",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2893-hansard-1937-40",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2894-hansard-1941-3",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2895-hansard-1943-5",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2896-hansard-1945-7",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2897-hansard-1947-9",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2899-hansard-1950-2",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2900-hansard-1952-6",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2901-hansard-1956-8",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2902-hansard-1959-61",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2903-hansard-1961-4",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2904-hansard-1964-7",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2905-hansard-1967-70",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2906-hansard-1970-3",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2907-hansard-1973-5",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2908-hansard-1976-8",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2909-hansard-1979-81",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2910-hansard-1982-84",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2911-hansard-1985-8",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2912-hansard-1988-92",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2942-hansard-1992-6",
  "https://www.parliament.vic.gov.au/hansard/26-hansard/articles/2943-hansard-1996-9"
)



#### Write a function that rests, then finds the URL of the PDFs on the search result page ####
getter <- function(search_result) {
  Sys.sleep(20)
  read_html(search_result) %>% 
    html_nodes("td a") %>%
    html_attr('href') %>% 
    as_tibble()
}


#### Map ####
# Initialise an empty list to hold the results
my_data <- list()

# Use the function - it applies the 'getter' function to each element of the pages list and stores the result of each element in an element of the my_data list.
# pages <- pages[1:2] # for testing
# pages <- pages[3:length(pages)]

my_data <- map(pages, ~ getter(.x))


# Combine all the results and write to a csv
hansard_pdfs <- bind_rows(my_data)

write_csv(hansard_pdfs, "links.csv")





#### Get from 1999 ####
pages <- c(
  "https://www.parliament.vic.gov.au/hansard/daily-hansard/3389-council-2017",
  "https://www.parliament.vic.gov.au/hansard/daily-hansard/26-hansard/articles/2914-council-2016",
  "https://www.parliament.vic.gov.au/hansard/daily-hansard/26-hansard/articles/2413-council-2015",
  "https://www.parliament.vic.gov.au/hansard/daily-hansard/26-hansard/articles/2237-council-2014",
  "https://www.parliament.vic.gov.au/hansard/daily-hansard/26-hansard/articles/2026-council-2013",
  "https://www.parliament.vic.gov.au/hansard/daily-hansard/26-hansard/articles/1735-legislative-council-february-to-december-2012",
  "https://www.parliament.vic.gov.au/hansard/daily-hansard/26-hansard/articles/1319-legislative-council-february-to-december-2011",
  "https://www.parliament.vic.gov.au/hansard/daily-hansard/26-hansard/articles/843-legislative-council-february-to-october-2010",
  "https://www.parliament.vic.gov.au/hansard/daily-hansard/26-hansard/articles/842-legislative-council-february-to-december-2009",
  "https://www.parliament.vic.gov.au/hansard/daily-hansard/26-hansard/articles/841-legislative-council-february-to-december-2008",
  "https://www.parliament.vic.gov.au/hansard/daily-hansard/26-hansard/articles/840-legislative-council-february-to-december-2007",
  "https://www.parliament.vic.gov.au/hansard/daily-hansard/26-hansard/articles/839-legislative-council-february-to-december-2006",
  "https://www.parliament.vic.gov.au/hansard/daily-hansard/26-hansard/articles/838-legislative-council-february-to-november-2005",
  "https://www.parliament.vic.gov.au/hansard/daily-hansard/26-hansard/articles/837-legislative-council-march-to-december-2004",
  "https://www.parliament.vic.gov.au/hansard/daily-hansard/26-hansard/articles/836-legislative-council-february-to-december-2003",
  "https://www.parliament.vic.gov.au/hansard/daily-hansard/26-hansard/articles/835-legislative-council-march-to-october-2002",
  "https://www.parliament.vic.gov.au/hansard/daily-hansard/26-hansard/articles/834-legislative-council-march-to-december-2001",
  "https://www.parliament.vic.gov.au/hansard/daily-hansard/26-hansard/articles/833-legislative-council-february-to-nov-2000",
  "https://www.parliament.vic.gov.au/hansard/daily-hansard/26-hansard/articles/832-legislative-council-november-and-december-1999",
  "https://www.parliament.vic.gov.au/hansard/daily-hansard/3388-assembly-2017",
  "https://www.parliament.vic.gov.au/hansard/daily-hansard/26-hansard/articles/2913-assembly-2016",
  "https://www.parliament.vic.gov.au/hansard/daily-hansard/26-hansard/articles/2415-assembly-2015",
  "https://www.parliament.vic.gov.au/hansard/daily-hansard/26-hansard/articles/2238-assembly-2014",
  "https://www.parliament.vic.gov.au/hansard/daily-hansard/26-hansard/articles/2027-assembly-2013",
  "https://www.parliament.vic.gov.au/hansard/daily-hansard/26-hansard/articles/1736-february-to-december-2012",
  "https://www.parliament.vic.gov.au/hansard/daily-hansard/26-hansard/articles/1318-february-to-december-2011",
  "https://www.parliament.vic.gov.au/hansard/daily-hansard/26-hansard/articles/819-february-to-december-2010",
  "https://www.parliament.vic.gov.au/hansard/daily-hansard/26-hansard/articles/820-legislative-assembly-february-to-december-2009",
  "https://www.parliament.vic.gov.au/hansard/daily-hansard/26-hansard/articles/822-legislative-assembly-february-to-december-2008",
  "https://www.parliament.vic.gov.au/hansard/daily-hansard/26-hansard/articles/823-legislative-assembly-february-to-december-2007",
  "https://www.parliament.vic.gov.au/hansard/daily-hansard/26-hansard/articles/824-legislative-assembly-february-to-december-2006",
  "https://www.parliament.vic.gov.au/hansard/daily-hansard/26-hansard/articles/825-legislative-assembly-february-to-december-2005",
  "https://www.parliament.vic.gov.au/hansard/daily-hansard/26-hansard/articles/826-legislative-assembly-february-to-december-2004",
  "https://www.parliament.vic.gov.au/hansard/daily-hansard/26-hansard/articles/827-legislative-assembly-february-to-december-2003",
  "https://www.parliament.vic.gov.au/hansard/daily-hansard/26-hansard/articles/828-legislative-assembly-february-to-december-2002",
  "https://www.parliament.vic.gov.au/hansard/daily-hansard/26-hansard/articles/829-legislative-assembly-february-to-december-2001",
  "https://www.parliament.vic.gov.au/hansard/daily-hansard/26-hansard/articles/830-legislative-assembly-february-to-december-2000",
  "https://www.parliament.vic.gov.au/hansard/daily-hansard/26-hansard/articles/831-legislative-assembly-february-to-december-1999"
)



#### Write a function that rests, then finds the URL of the PDFs on the search result page ####
getter <- function(search_result) {
  Sys.sleep(20)
  read_html(search_result) %>% 
    html_nodes("a") %>%
    html_attr('href') %>% 
    as_tibble()
}


#### Map ####
# Initialise an empty list to hold the results
my_data <- list()

# Use the function - it applies the 'getter' function to each element of the pages list and stores the result of each element in an element of the my_data list.
# pages <- pages[1:2] # for testing
# pages <- pages[3:length(pages)]

my_data <- map(pages, ~ getter(.x))


# Combine all the results and write to a csv
hansard_pdfs <- bind_rows(my_data)

write_csv(hansard_pdfs, "from1999onward.csv")
