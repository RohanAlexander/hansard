#### Set up workspace ####
library(tidyverse)
library(rvest)


pages_to_scrape <- c("https://en.wikipedia.org/wiki/Main_Page", "asdfasfasf")



unstable_function <- function(the_page) {
  read_html(the_page)
}

safe_function <- safely(unstable_function)

map(pages_to_scrape, ~ safe_function(.x)) %>%
  map("result") %>%
  compact() 

