#### Preamble ####
# Purpose: A list of everyone who won an election is available in the Parliamentary Handbook (add link: HERE). This script cleans the data to make it comparable with the Hansard data. It can then be used to check the speakers.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 1 August 2018
# Prerequisites: You need to have the table from the Parliamentary Handbook saved as a CSV (see data/parliamentary_handbook.csv).


#### Set up workspace ####
# Load libraries
# install.packages("genderdata", repos = "http://packages.ropensci.org")
library(gender)
library(lubridate)
library(naniar)
library(tidyverse)
# update.packages()


#### Load data ####
parliamentary_handbook <- read_csv("data/parliamentary_handbook.csv", col_names = TRUE)


#### Change the dates ####
# Change classes to dates
parliamentary_handbook$dateOfElection <- dmy(parliamentary_handbook$dateOfElection)
parliamentary_handbook$dateCeasedToBeMember <- dmy(parliamentary_handbook$dateCeasedToBeMember)
# Some of the dates for DOB have day, month and year, but some only have the year so need both
parliamentary_handbook <- parliamentary_handbook %>% 
  mutate(birthDate = dmy(born),
         yearBorn = if_else(is.na(birthDate), year(parse_date_time(born, "Y")), year(birthDate)))
parliamentary_handbook$died <- dmy(parliamentary_handbook$died)
head(parliamentary_handbook)

# Add year of election/birth/death
parliamentary_handbook <- parliamentary_handbook %>% 
  mutate(yearDied = year(died),
         yearElected = year(dateOfElection)
         )

#### Amend the names ####
# Split out first name
parliamentary_handbook <- parliamentary_handbook %>% 
  separate(col = otherNames, into = c("firstName", "middleNames"), sep = " ", extra = "merge", fill = "right", remove = FALSE) %>% 
  rename(nonSurnames = otherNames)

# Add Hansard name
parliamentary_handbook <- parliamentary_handbook %>% 
  mutate(hansardName = paste0(toupper(surname), ", ", firstName)) %>% 
  replace_with_na(replace = list(hansardName = "NA, NA"))


#### Add gender ####
# The data is split into using one source - method = "ipums" - for pre-1930 births and another source - method = "ssa" - for 1930 and onwards births.
# Earlier births
pre_1930_results <- parliamentary_handbook %>% 
  filter(yearBorn < 1930) %>% 
  mutate(min_year = rep(1789, n()),
         max_year = rep(1930, n())) %>% 
  gender_df(name_col = "firstName", year_col = c("min_year", "max_year"), method = "ipums") 
# More recent births
from_1930_results <- parliamentary_handbook %>% 
  filter(yearBorn >= 1930) %>% 
  mutate(min_year = rep(1932, n()),
         max_year = rep(2012, n())) %>% 
  gender_df(name_col = "firstName", year_col = c("min_year", "max_year"), method = "ssa") 
# Combine them
gender_based_on_name <- rbind(pre_1930_results, from_1930_results) %>% 
  distinct(name, .keep_all = TRUE)
check_changes <- gender_based_on_name %>% group_by(name) %>% summarise(n_distinct(gender))
# Push guessed gender back into the main dataset
parliamentary_handbook <- parliamentary_handbook %>% 
  left_join(gender_based_on_name, by = c("firstName" = "name"))
# Check the names where the proportion male is between 0.1 and 0.9
check_these <- parliamentary_handbook %>% 
  filter(proportion_male < 0.9 & proportion_male > 0.1)
# Make the changes
parliamentary_handbook$gender[parliamentary_handbook$hansardName %in% c("BARTLETT, Kerry",
                                                                        "BLAND, Francis",
                                                                        "BRIGGS, Jamie",
                                                                        "CHAPMAN, Hedley",
                                                                        "CULPIN, Millice",
                                                                        "FALKINER, Franc",
                                                                        "FERGUSON, Laurie",
                                                                        "FORDE, Francis",
                                                                        "GARLAND, Ransley",
                                                                        "HUSIC, Edham",
                                                                        "INNES, Urquhart",
                                                                        "SCIACCA, Concetto",
                                                                        "SCOTT, Leslie",
                                                                        "SOLOMON, Vaiben",
                                                                        "WALLIS, Laurie")] <- "male"
parliamentary_handbook$gender[parliamentary_handbook$hansardName %in% c("BROWNBILL, Kay",
                                                                        "ELSON, Kay",
                                                                        "HULL, Kay",
                                                                        "KELLY, De-Anne",
                                                                        "REA, Kerry")] <- "female"
# Clean up
rm(pre_1930_results, from_1930_results, gender_based_on_name, check_these)
parliamentary_handbook <- parliamentary_handbook %>% 
  select (-c(proportion_female, proportion_male, year_min, year_max))
# check <- parliamentary_handbook %>% filter(is.na(gender))


#### Save ####
save(parliamentary_handbook, file = "outputs/parliamentary_handbook.Rda")