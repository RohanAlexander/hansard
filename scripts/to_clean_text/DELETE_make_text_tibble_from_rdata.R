#### Preamble ####
# Purpose: 
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 29 August 2018
# Prerequisites: 
# To do:


#### Set up workspace ####
# library(stringi)
# library(stringr)
# library(corpus)
library(SnowballC)
library(tidytext)
library(tidyverse)
library(tm)
# update.packages()


#### Stop words ####
# Fix this
# Bind custom list of stopwords to the default list
# custom_stop_words <- bind_rows(
#   stop_words, # The default list
#   data_frame(
#     word = c(
#       "act", "amendment", "amount", "australia", "australian", "bill", "board", "cent", "clause", "commission", "committee", "commonwealth", "countries", "country", "day", "deal", "debate", "department", "desire", "duty", "gentleman", "government", "honorable", "honourable", "house", "increase", "labor", "labour", "leader", "legislation", "matter", "minister", "money", "national", "opposition", "parliament", "party", "people", "policy", "position", "power", "prime", "proposed", "public", "question", "regard", "report", "service", "situation", "south", "speaker", "statement", "support", "system", "time", "united", "vote", "wales"
#     ),
#     lexicon = rep("custom", length(word))
#   )
# )


#### Create the lists of filenames ####
# Get list of Hansard PDF filenames
file_names <-
  list.files(
    #path = "/Volumes/SanDisk/hansard_txt", # Use this for all of them, comment to just run on test group
    # path = "outputs/for_testing_individual_hansard",
    path = "outputs/hansard/each_hansard",
    # Use this for testing, comment to get all
    pattern = "*.Rda",
    recursive = TRUE,
    full.names = TRUE
  )
file_names

dir.create("outputs/hansard/each_hansard_txt/") # Thanks Hadley! Creates a folder, just gives a warning if already exists

# file_names <- file_names[1:3]


#### Create the function that will be passed to walk2 ####
tidy_the_hansard_text_files <-
  function(name_of_input_text_file) {
    # Read in the text file
    # For testing:
    # load("outputs/hansard/each_hansard/1901-05-09.Rda")
    load(name_of_input_text_file)
    
    date <- this_hansard_as_tibble$date[1]
    
    this_hansard_as_tibble <- this_hansard_as_tibble %>% 
      select(statement)
    
    # Remove numbers
    this_hansard_as_tibble <- removeNumbers(this_hansard_as_tibble$statement)
    
    # Convert to a tibble
    this_hansard_as_tibble <- data_frame(text = this_hansard_as_tibble)
    
    # Convert the column text to just one word per row in a column called word, additionally: 1) make all words lower case; and 2) remove punctuation
    this_hansard_as_tibble <- this_hansard_as_tibble %>%
      unnest_tokens(word, text, to_lower = TRUE, strip_punct = TRUE)
    
    # Remove common words
    this_hansard_as_tibble <- this_hansard_as_tibble %>%
      anti_join(stop_words)
    
    #stem words
    parsed_text_file <- this_hansard_as_tibble %>%
      mutate(stem_word = wordStem(word, language = "english"))
    
    save_here <- paste0("outputs/hansard/each_hansard_txt/", date, ".csv")
    
    # Save the CSV
    write_csv(parsed_text_file, path = save_here)
    
    print(paste0("Done with ", save_here, " at ", Sys.time()))
    
  }


#### Run walk2 ####
walk(file_names, ~tidy_the_hansard_text_files(.x))






