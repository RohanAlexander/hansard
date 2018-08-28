#### Preamble ####
# Purpose: This file takes Australian Hansard text files and it converts them to a terrific tidied text tibble that can be analysed.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 28 August 2018
# Prerequisites: You a folder of txt files e.g. the outputs of get_data_from_pdfs.R 
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
    path = "outputs/for_testing_hansard_txt", # Use this for testing, comment to get all
    pattern = "*.txt",
    recursive = TRUE,
    full.names = TRUE
  )
file_names

# Make list of the names that we'll save it all with
csv_dates <-
  list.files(
    #path = "/Volumes/SanDisk/hansard_txt", # Use this for all of them, comment to just run on test group
    path = "outputs/for_testing_hansard_txt", # Use this for testing, comment to get all
    pattern = "*.txt",
    recursive = TRUE,
    full.names = FALSE
  ) %>%
  str_replace(".txt", ".csv")
csv_dates

dir.create("outputs/for_testing_hansard_cleaned_text_files") # Thanks Hadley! Creates a folder, just gives a warning if already exists
save_names <- paste0("outputs/for_testing_hansard_cleaned_text_files/", csv_dates)
# dir.create("/Volumes/SanDisk/hansard_cleaned_text_files") # Thanks Hadley! Creates a folder
# save_names <- paste0("/Volumes/SanDisk/hansard_cleaned_text_files/", save_names)


#### Create the function that will be passed to walk2 ####
tidy_the_hansard_text_files <-
  function(name_of_input_text_file,
           name_of_output_text_file) {
    
    # Read in the text file
    parsed_text_file <- read_lines(name_of_input_text_file)
    # parsed_text_file <- read_lines("outputs/for_testing_hansard_txt/2011-02-28.txt")
    
    # Remove numbers
    parsed_text_file <- removeNumbers(parsed_text_file)
    
    # Convert to a tibble
    parsed_text_file <- data_frame(text = parsed_text_file)
    
    # Convert the column text to just one word per row in a column called word, additionally: 1) make all words lower case; and 2) remove punctuation
    parsed_text_file <- parsed_text_file %>%
      unnest_tokens(word, text, to_lower = TRUE, strip_punct = TRUE)
    
    # Remove common words
    parsed_text_file <- parsed_text_file %>%
      anti_join(stop_words)
    
    #stem words
    parsed_text_file <- parsed_text_file %>%
      mutate(stem_word = wordStem(word, language="english"))
    
    # Save the CSV
    write_csv(parsed_text_file, path = name_of_output_text_file)
    
    print(paste0("Done with ", name_of_output_text_file, " at ", Sys.time()))
    
  }
    

#### Run walk2 ####
walk2(file_names, save_names, ~tidy_the_hansard_text_files(.x, .y))












dates_spoken <- str_replace(csv_dates, ".csv", "")
dates_spoken

# Collapse the stemmed words into one row and save that too

data <- read.csv("outputs/for_testing_hansard_cleaned_text_files/2011-02-28.csv")
name <- "2011-02-28"
all <- paste(data$stem_word, collapse = ", ")
days_words <- tibble(date = name, words = all)



