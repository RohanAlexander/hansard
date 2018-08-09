#### Preamble ####
# Purpose: The dataset about all the politicians in the data folder, and want to fix the classes and add genders to it and save it in outputs.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 9 August 2018
# Prerequisites: You need to have the dataset of politicians based on (but modified from) the Parliamentary Handbook saved as a CSV (see data/politicians/politicians_by_individuals.csv).


#### Set up workspace ####
# Load libraries
# install.packages("genderdata", repos = "http://packages.ropensci.org")
library(gender)
library(lubridate)
library(naniar)
library(tidyverse)
# update.packages()


#### Load data ####
politicians_by_individuals <-
  read_csv("data/politicians/politicians_by_individuals.csv",
           col_names = TRUE)


#### Update classes ####
# Some of the dates for date of birth (birthDate) and date of death (deathDate) have year-month-day, but some only have the year so need to account for both
politicians_by_individuals <- politicians_by_individuals %>%
  mutate(birthDate = ymd(birthDate),
         deathDate = ymd(deathDate))
head(politicians_by_individuals)


#### Amend the names ####
# # Add Hansard name
# politicians_by_individuals <- politicians_by_individuals %>%
#   mutate(useThisName = if_else(!is.na(commonName), commonName, firstName)) %>%
#   mutate(hansardName = paste0(toupper(surname), ", ", useThisName)) %>%
#   replace_with_na(replace = list(hansardName = "NA, NA")) %>%
#   select(-useThisName)


#### Add gender ####
# The data is split into using one source - method = "ipums" - for pre-1930 births and another source - method = "ssa" - for 1930 and onwards births.
# Earlier births
pre_1930_results <- politicians_by_individuals %>%
  filter(birthYear < 1930) %>%
  gender_df(name_col = "firstName",
            year_col = "birthYear",
            method = "ipums")
# More recent births
from_1930_results <- politicians_by_individuals %>%
  filter(birthYear >= 1930) %>%
  gender_df(name_col = "firstName",
            year_col = "birthYear",
            method = "ssa")
# Combine them
gender_based_on_name <-
  rbind(pre_1930_results, from_1930_results)
# Push guessed gender back into the main dataset
politicians_by_individuals <- politicians_by_individuals %>%
  left_join(gender_based_on_name,
            by = c("firstName" = "name", "birthYear" = "year_min"))
# Correct mistakes - NA or female to male
politicians_by_individuals$gender[politicians_by_individuals$uniqueKey %in% c(
  "batcheloregerton1901",
  "beazleykim1945",
  "blainadair1934",
  "blandfrancis1951",
  "bowennigel1964",
  "bridgesmaxwellcrawford1964",
  "briggsjamie2008",
  "browngeoffrey1949",
  "cameroneoin1993",
  "cameronewen1977",
  "campbellgraeme1980",
  "chapmangrant1975",
  "coonjabez1906",
  "crossmanfred1961",
  "culpinmillice1903",
  "dickmilton2016",
  "enderbykep1970",
  "fergusonlaurie1990",
  "fordefrancis1922",
  "fosterfrancis1906",
  "garlandransley1969",
  "georgioupetro1994",
  "hansenbrendan1961",
  "holtenrendle1958",
  "hurrygeoffry1922",
  "husiced2010",
  "innested1972",
  "jonesewen2010",
  "keonstandish1949",
  "leemervyn1966",
  "lindsayeamon1983",
  "mackinnonewen1949",
  "millarclarrie1974",
  "munrodugald1966",
  "oconnorgavan1993",
  "phillipspharez1901",
  "richardsonkym2004",
  "robinsonian1963",
  "sciaccacon1987",
  "scottles1988",
  "sinclairian1963",
  "smithwarwick1984",
  "solomonvaiben1901",
  "spurrlancelot1939",
  "streetgeoffrey1934",
  "townleyathol1949",
  "wallislaurie1969",
  "websteralasdair1984",
  "wilkiekim1998"
)] <- "male"
# Correct mistakes - NA or male to female
politicians_by_individuals$gender[politicians_by_individuals$uniqueKey %in% c(
  "brodtmanngai2010",
  "brownbillkay1966",
  "jacksonsharryn2001",
  "johnstonricky1996",
  "kearneyged2018",
  "kellydeanne1996",
  "leysussan2001",
  "mactiernanalannah2013",
  "reakerry2007",
  "sharkierebekha2016"
)] <- "female"
# Clean up
rm(pre_1930_results,
   from_1930_results,
   gender_based_on_name)
politicians_by_individuals <- politicians_by_individuals %>%
  select (-c(proportion_female, proportion_male, year_max)) %>%
  select(
    uniqueKey,
    surname,
    allOtherNames,
    firstName,
    commonName,
    earlierOrLaterNames,
    title,
    gender,
    everything()
  )

#### Save ####
save(politicians_by_individuals, file = "outputs/politicians_by_individuals.Rda")
write_csv(politicians_by_individuals, path = "outputs/politicians_by_individuals.csv")
