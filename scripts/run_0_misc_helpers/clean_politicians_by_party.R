#### Preamble ####
# Purpose: The dataset about all the parties of the politicians in the data folder, and want to fix the classes and fill downwards and save it in outputs.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 12 August 2018
# Prerequisites: You need to have the dataset of politicians divisions based on (but modified from) the Parliamentary Handbook saved as a CSV (see data/politicians/politicians_by_party.csv).


#### Set up workspace ####
# Load libraries
library(lubridate)
library(tidyverse)
# update.packages()


#### Load data ####
politicians_by_party <-
  read_csv("data/politicians/politicians_by_party.csv",
           col_names = TRUE)
head(politicians_by_party)


#### Remove the superfluous columns ####
politicians_by_party <- politicians_by_party %>% 
  select(-starts_with("DELETE_"))


#### Classes ####
politicians_by_party <- politicians_by_party %>% 
  mutate(partyFrom = dmy(partyFrom),
         partyTo = dmy(partyTo))


#### Fill ####
politicians_by_party <- politicians_by_party %>% 
  fill(uniqueID)
head(politicians_by_party)

test <- politicians_by_party %>% 
  filter(is.na(partyTo)) 

#### Save ####
save(politicians_by_party, file = "outputs/politicians_by_party.Rda")
write_csv(politicians_by_party, path = "outputs/politicians_by_party.csv")
