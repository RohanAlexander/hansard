#### Preamble ####
# Purpose: 
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 26 August 2018
# Prerequisites: 
# Issues:


#### Workspace set-up ####
# Load libraries
library(tictoc)
library(tidyverse)
# update.packages()

# Load spelling corrections
load("outputs/corrections.RData")

# Load Hansard statements
load("outputs/hansard/all_hansard.Rda") # Takes a while


#### Collapsing ####
# Comment next few lines for full run
# Get a sample of 100,000 statements 
# set.seed(123)
# some_random_rows <- sample(1:nrow(all_hansard), 100000)
# all_hansard <- all_hansard[some_random_rows, ]
# rm(some_random_rows)

all_hansard <- all_hansard %>% 
  select(date, statement)

hansard_text <- all_hansard %>% 
  group_by(date) %>% 
  summarise(allTextForDate = toString(statement)) %>%
  ungroup() %>% 
  as_tibble()


#### Fix spellings ####
tic()
hansard_text$allTextForDate <- str_replace_all(hansard_text$allTextForDate, corrections)
toc()

#### Save ####
save(hansard_text, file = "outputs/hansard_text.Rda")

