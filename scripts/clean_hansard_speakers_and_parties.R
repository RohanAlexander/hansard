#### Preamble ####
# Purpose: We need the speakers names and details to be consistent.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 31 July 2018
# Prerequisites: You need to have the Hansard in a data frame - see get_data_from_xml_to_dataframe.


#### Set up workspace ####
# Load libraries
library(lubridate)
library(tictoc)
library(tidytext)
library(tidyverse)
# update.packages()


#### Load data ####
# Load Hansard data
load("outputs/all_hansard.Rda") # Takes a minute
# Create a working sample of 100,000 statements 
# set.seed(123) # Comment if you want to run on full dataset
# some_random_rows <- sample(1:nrow(all_hansard), 100000) # Comment if you want to run on full dataset
# all_hansard <- all_hansard[some_random_rows, ] # Comment if you want to run on full dataset
# rm(some_random_rows) # Comment if you want to run on full dataset

# Load Parliamentary Handbook parliamentarians data
load("outputs/parliamentary_handbook.Rda") 


#### Get speakers? 'Hear, hear!'####
# Reduce the dataset to just the variables related to the speakers
all_hansard <- all_hansard %>% 
  select(speakerID, speaker_name_meta, speaker_name_display, electorate, party, role, date) 

length(unique(all_hansard$speakerID))
length(unique(all_hansard$speaker_name_meta))
length(unique(all_hansard$speaker_name_display))

# We have a list of accepted names from the House so first match those to the speaker_name_meta list
speakers_by_name_and_ID <- all_hansard %>%
  select(speakerID, speaker_name_meta) %>% 
  unique() %>% 
  add_count(speakerID, sort = TRUE)

speakers_by_name_and_ID <- speakers_by_name_and_ID %>% 
  mutate(in_handbook = if_else(speaker_name_meta %in% parliamentary_handbook$Hansard_name, 1, 0))





 %>% 
  filter(n>1)
# speaker_name_meta = "CHANEY, Fred+D3309" needs to be looked at

speakers_by_speakerID <- all_hansard %>%
  select(speakerID, speaker_name_display) %>% 
  unique() %>% 
  add_count(speakerID, sort = TRUE) %>% 
  filter(n>1)
# Many issues here

speakers_by_speakerID <- all_hansard %>%
  select(speaker_name_meta, speaker_name_display) %>% 
  unique() %>% 
  add_count(speaker_name_meta, sort = TRUE) %>% 
  filter(n>1)
# Many issues here too



aggregate(speakerID ~ speaker_name_meta, all_hansard, FUN=length)

  ?expand(nesting(speakerID, speaker_name_meta)) %>% 
  group_by(speakerID) %>% 
  mutate(counts = count(speaker_name_meta))

?count
names(all_hansard)

unique(all_hansard$speakerID)
length(unique(all_hansard$speakerID))
length(unique(all_hansard$speaker_name_meta))
length(unique(all_hansard$speaker_name_display))

unique(all_hansard$dates)

test <- all_hansard %>%
  group_by(speakerID) %>%
  mutate(number_of_names = length(unique(speaker_name_meta))) %>%
  filter(number_of_names >1) %>%
  distinct(speaker_name_meta) %>%
  arrange(speakerID)


#### Get political parties ####
# Reduce the dataset to just the variables related to the speakers
parties <- all_hansard %>% 
  select(speakerID, party, speaker_name_meta) %>% 
  count(party,speakerID, speaker_name_meta, sort = TRUE)

