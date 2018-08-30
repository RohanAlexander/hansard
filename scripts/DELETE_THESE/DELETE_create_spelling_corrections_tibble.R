#### Preamble ####
# Purpose: Create a tibble of two columns where the first is the mistake and the second is the correction.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 29 July 2018
# Prerequisites: 


#### Set up workspace ####
# Load libraries
library(tidytext)
library(tidyverse)
# update.packages()


#### Create table ####
corrections_table <- read_csv("data/corrections_table.csv", col_names = TRUE)



dat_orig <- tibble(TEXT = c(
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit.",
  "Fusce nec quam ut tortor interdum pulvinar id vitae magna.",
  "Curabitur commodo consequat arcu et lacinia.",
  "Proin at diam vitae lectus dignissim auctor nec dictum lectus.",
  "Fusce venenatis eros congue velit feugiat, ac aliquam ipsum gravida."
))

head(corrections_table)




