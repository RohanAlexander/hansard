#### Preamble ####
# Purpose: We need the speakers names and details to be consistent.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 23 July 2018
# Prerequisites: You need to have the Hansard in a data frame - see get_data_from_xml_to_dataframe.


#### Set up workspace ####
# Load libraries
library(tictoc)
library(tidytext)
library(tidyverse)
# update.packages()


#### Load data ####
load("outputs/all_hansard.Rda") # Takes a minute
# Create a working sample of 100,000 statements 
set.seed(123) # Comment if you want to run on full dataset
some_random_rows <- sample(1:nrow(all_hansard), 100000) # Comment if you want to run on full dataset
all_hansard <- all_hansard[some_random_rows, ] # Comment if you want to run on full dataset
rm(some_random_rows) # Comment if you want to run on full dataset


#### Get speakers? 'Hear, hear!'####
# Reduce the dataset to just the variables related to the speakers
all_hansard <- all_hansard %>% 
  select(speakerID, speaker_name_meta, speaker_name_display, electorate, party, role, date) 

length(unique(all_hansard$speakerID))
length(unique(all_hansard$speaker_name_meta))
length(unique(all_hansard$speaker_name_display))

speakers_by_speakerID <- all_hansard %>%
  select(speakerID, speaker_name_meta) %>% 
  unique() %>% 
  add_count(speakerID, sort = TRUE) %>% 
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
  select(party) %>% 
  count(party, sort = TRUE)

