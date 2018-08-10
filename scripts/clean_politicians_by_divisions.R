#### Preamble ####
# Purpose: The dataset about all the divisions of the politicians in the data folder, and want to fix the classes and fill downwards and save it in outputs.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 10 August 2018
# Prerequisites: You need to have the dataset of politicians divisions based on (but modified from) the Parliamentary Handbook saved as a CSV (see data/politicians/politicians_by_division.csv).


#### Set up workspace ####
# Load libraries
library(lubridate)
library(tidyverse)
# update.packages()


#### Load data ####
politicians_by_division <-
  read_csv("data/politicians/politicians_by_division.csv",
           col_names = TRUE)
head(politicians_by_division)


#### Remove the superfluous columns ####
politicians_by_division <- politicians_by_division %>% 
  select(-starts_with("DELETE_"))


#### Classes ####
politicians_by_division <- politicians_by_division %>% 
  mutate(electionDate = ymd(electionDate),
         ceasedToBeMemberDate = ymd(ceasedToBeMemberDate))


#### Fill ####
politicians_by_division <- politicians_by_division %>% 
  fill(uniqueID)
head(politicians_by_division)

#### Save ####
save(politicians_by_division, file = "outputs/politicians_by_division.Rda")
write_csv(politicians_by_division, path = "outputs/politicians_by_division.csv")
