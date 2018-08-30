#### Preamble ####
# Purpose: We need the speakers names and details to be consistent. We have a list based on the Parliamentary Handbook, and want to make the Hansard data consistent with that.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 6 August 2018
# Prerequisites: You need to have the Hansard in a data frame - see get_data_from_xml_to_dataframe.R. You need to have the corrected and updated Parliamentary Handbook data - see clean_parliamentary_handbook.R.


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

# Focus on the speaker ID and the name 
speakers_by_name_and_ID <- all_hansard %>%
  select(speakerID, speaker_name_meta) %>% 
  unique() %>% 
  add_count(speakerID, sort = TRUE) %>% 
  filter(n > 1)

# Look for speaker_name_meta = "CHANEY, Fred+D3309", likely replace with "CHANEY, Fred"
# Look for "KNM" which is both "HARRISON, Eli" and "HARRISON, Eli James", likely replace one with the other
# Same for "KNX" which is both "HARRISON, Eric" and "HARRISON, Eric John"
# Same for "LLN" which is both "EDWARDS, Harold Raymond" and "EDWARDS, Harold"

# Check which way to change
follow_ups <- all_hansard %>%
  select(speakerID, speaker_name_meta) %>% 
  filter(speakerID %in% c("KNM", "KNX", "LLN")) %>% 
  add_count(speaker_name_meta, sort = TRUE) %>% 
  unique()

# Make the changes
all_hansard$speaker_name_meta[all_hansard$speaker_name_meta == "HARRISON, Eli James" &
                                all_hansard$speakerID == "KNM"] <-
  "HARRISON, Eli"
all_hansard$speaker_name_meta[all_hansard$speaker_name_meta == "CHANEY, Fred+D3309" &
                                all_hansard$speakerID == "JWV"] <-
  "CHANEY, Fred"
all_hansard$speaker_name_meta[all_hansard$speaker_name_meta == "HARRISON, Eric John" &
                                all_hansard$speakerID == "KNX"] <-
  "HARRISON, Eric"
all_hansard$speaker_name_meta[all_hansard$speaker_name_meta == "EDWARDS, Harold" &
                                all_hansard$speakerID == "LLN"] <-
  "EDWARDS, Harold Raymond"
rm(follow_ups)



# We have a list of accepted names from the House so first match those to the speaker_name_meta list
all_hansard$speaker_name_meta[all_hansard$speaker_name_meta == "ATKINSON, Llewelyn"] <- "ATKINSON, Llewellyn"
all_hansard$speaker_name_meta[all_hansard$speaker_name_meta == "BATE, Henry"] <- "BATE, Jeff"
all_hansard$speaker_name_meta[all_hansard$speaker_name_meta == "EDWARDS, Harold Raymond"] <- "EDWARDS, Harold"
all_hansard$speaker_name_meta[all_hansard$speaker_name_meta == "EVATT, Herbert Vere"] <- "EVATT, Herbert"
all_hansard$speaker_name_meta[all_hansard$speaker_name_meta == "FADDEN, Arthur William"] <- "FADDEN, Arthur"
all_hansard$speaker_name_meta[all_hansard$speaker_name_meta == "FALSTEIN, Sydney"] <- "FALSTEIN, Max"
all_hansard$speaker_name_meta[all_hansard$speaker_name_meta == "FORDE, Francis Michael"] <- "FORDE, Francis"
all_hansard$speaker_name_meta[all_hansard$speaker_name_meta == "GUY, James Allan"] <- "GUY, James"
all_hansard$speaker_name_meta[all_hansard$speaker_name_meta == "HUGHES, William Morris"] <- "HUGHES, William"
all_hansard$speaker_name_meta[all_hansard$speaker_name_meta == "INNES, Urquhart"] <- "INNES, Ted"
all_hansard$speaker_name_meta[all_hansard$speaker_name_meta == "JOHNSON, Leslie"] <- "JOHNSON, Les"
all_hansard$speaker_name_meta[all_hansard$speaker_name_meta == "LANG, John"] <- "LANG, Jack"
all_hansard$speaker_name_meta[all_hansard$speaker_name_meta == "LYONS, Joseph Aloysius"] <- "LYONS, Joseph"
all_hansard$speaker_name_meta[all_hansard$speaker_name_meta == "MCGUREN, Francis"] <- "MCGUREN, Frank"
all_hansard$speaker_name_meta[all_hansard$speaker_name_meta == "McKENZIE, David Charles"] <- "McKENZIE, David"
all_hansard$speaker_name_meta[all_hansard$speaker_name_meta == "MCLEAY, John Elden"] <- "MCLEAY, John"
all_hansard$speaker_name_meta[all_hansard$speaker_name_meta == "SCULLIN, James Henry"] <- "SCULLIN, Jim"
all_hansard$speaker_name_meta[all_hansard$speaker_name_meta == "STEWART, Francis"] <- "STEWART, Frank"
all_hansard$speaker_name_meta[all_hansard$speaker_name_meta == "WATSON, John Christian"] <- "WATSON, John"
all_hansard$speaker_name_meta[all_hansard$speaker_name_meta == "WENTWORTH, William Charles"] <- "WENTWORTH, William"
all_hansard$speaker_name_meta[all_hansard$speaker_name_meta == "WEST, John Edward"] <- "WEST, John"
all_hansard$speaker_name_meta[all_hansard$speaker_name_meta == "WILSON, John Gratton"] <- "WILSON, John"
all_hansard$speaker_name_meta[all_hansard$speaker_name_meta == "WILSON, Keith Cameron"] <- "WILSON, Keith"

# Come back and check this one. Maybe just a capitalisation issue?
all_hansard$speaker_name_meta[all_hansard$speaker_name_meta == "McKENZIE, David"] <- "MCKENZIE, David"

speakers_by_name_and_ID <- all_hansard %>%
  select(speakerID, speaker_name_meta) %>% 
  unique() %>% 
  add_count(speakerID, sort = TRUE)

speakers_by_name_and_ID <- speakers_by_name_and_ID %>% 
  mutate(in_handbook = if_else(speaker_name_meta %in% parliamentary_handbook$hansardName, 1, 0)) %>% 
  filter(!(speakerID %in% c("ME4", "2U4", "K6F", "JQR", "JUH", "JXR", "8G4", "KMX", "9V4", "C7D", "KPG", "KTA", "5U4", "KTZ", "KUJ", "KVK", "1L5", "JYA", "K1F", "CAK", "K5H", "BJ4", "KAS", "CJO", "KBC", "DV4", "KBL", "KBW")))
# These are all senators - they got to speak on 6/7 August 1974 during the joint sitting. See: https://en.wikipedia.org/wiki/Joint_Sitting_of_the_Australian_Parliament_of_1974

write.csv(speakers_by_name_and_ID, "speakers_by_name_and_ID.csv")

# Need to save the modified Hansard






#### Get political parties ####
# Reduce the dataset to just the variables related to the speakers
parties <- all_hansard %>% 
  select(speakerID, party, speaker_name_meta) %>% 
  count(party,speakerID, speaker_name_meta, sort = TRUE)

