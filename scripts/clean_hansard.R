#### Preamble ####
# Purpose: 
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au 
# Last updated: 17 July 2018
# Prerequisites: Uses all_hansard_titles_and_dates.Rda which is an output of get_list_of_titles.R
# Issues: 


#### Workspace set-up ####
# Load libraries
library(stringr) # Needed for the regular expressions search within statements
library(tidyverse)
# update.packages()
# Load data
load("outputs/all_hansard_text.Rda") # Takes a while
# Reduce the data to a tractable size
some_hansard_text <- all_hansard_text[all_hansard_text$dates == "1949-02-09", ]


#### Rows that are statements but are not well attributed ####
# Some rows have no information about the speaker, but the name of the speaker is in the statement. The issue seems to have been introduced during the process of converting the PDFs to XML etc.
# We are looking for statements that start with Mr. or Dr. and we don't want the statement type to be a quote - TODO are there any women?
# x <- c("Mr.", "Mr", "and Mr.", "Mr. Jones", "mr.")
# str_detect(x, "^Mr\\.") # ^ anchors the search to the start of the string; \\. looks for .; https://xkcd.com/208/
some_hansard_text$flagMr <- str_detect(some_hansard_text$statement, "^Mr\\.")
some_hansard_text$flagDr <- str_detect(some_hansard_text$statement, "^Dr\\.")
some_hansard_text$flag <- some_hansard_text$flagMr | some_hansard_text$flagDr
some_hansard_text$flagMr <- NULL
some_hansard_text$flagDr <- NULL

some_hansard_text$flag <-
  if_else(
    some_hansard_text$statement_type == "quote",
    FALSE,
    some_hansard_text$flag
  )

# TODO: Add some code that pulls out the name and other details.


#### Rows that are quotes ####
# Sometimes when a speaker quotes someone it has been pushed onto its own row. This incorrectly adds to the number of statements in Hansard because the quote should be part of the statement
# Create a column of row numbers
some_hansard_text$rowNumbers <- 2:nrow(some_hansard_text)
# Iterate through the rows
for (i in 2:nrow(some_hansard_text)){ # Start at 2 because otherwise 1-1 is 0 and that row doesn't exist
  if (is.na(some_hansard_text$speakerID[i])){
    # First we are looking to combine rows where both rows are quotes, but they've been separated into two rows. We want to combine these and then surround the whole statement with "".
    # If statement_type is quote and next one is quote, then give them the same number
    if (some_hansard_text$statement_type[i-1] == "quote"){
      if (some_hansard_text$statement_type[i] == "quote"){
        some_hansard_text$rowNumbers[i] <- some_hansard_text$rowNumbers[i-1]
      }
    } 
  }
}

for (i in 2:nrow(some_hansard_text)){  
  if (some_hansard_text$rowNumbers[i] == some_hansard_text$rowNumbers[i-1]){
    some_hansard_text$statement[i-1] <- 
      paste(some_hansard_text$statement[i-1], some_hansard_text$statement[i], sep = " ")
  }
}


# Then we want to push the quote into the statement.
# TODO


rm(i)
some_hansard_text$check <- 1:nrow(some_hansard_text)
some_hansard_text$check <- if_else(some_hansard_text$check == some_hansard_text$rowNumbers, 1, 0)


#### Statements that have been separated over two rows ####
# Sometimes the one statement has been pushed across more than one row. Sometimes this seems to have been the case when the PDF has a page break in teh middle of the statement. This incorrectly adds to the number of statements in Hansard and makes analysis more difficult.
# TODO


#### Spelling mistakes ####
# There's a bunch of spelling mistakes. They mostly seem to have occred when the files were converted from PDFs.
# TODO


#### Spelling inconsistencies ####
# TODO




for (i in 2:nrow(some_hansard_text)){
  if (is.na(some_hansard_text$speakerID[i])){
    # If statement_type is talk.start and next one is para, then give them the same number e.g. line 1400225
    if (some_hansard_text$statement_type[i-1] == "talk.start"){
      if (some_hansard_text$statement_type[i] == "para"){
        some_hansard_text$counter[i] <- some_hansard_text$counter[i-1]
      }
    } # Errors: This is picking up some stage directions e.g. line 1400176
    # If statement_type is para and next one is para, then give them the same number
    # Line 1400029 is a good example of this
    if (some_hansard_text$statement_type[i-1] == "para"){
      if (some_hansard_text$statement_type[i] == "para"){
        some_hansard_text$counter[i] <- some_hansard_text$counter[i-1]
      }
    } # Errors: 
    # If statement_type is quote and next one is quote, then give them the same number
    # Line 1400029 is a good example of this
    if (some_hansard_text$statement_type[i-1] == "quote"){
      if (some_hansard_text$statement_type[i] == "quote"){
        some_hansard_text$counter[i] <- some_hansard_text$counter[i-1]
      }
    } # Errors: 
    
  }
}
rm(i)

some_hansard_text$checker <- 1:nrow(some_hansard_text)
some_hansard_text$highlighter <- if_else(some_hansard_text$counter == some_hansard_text$checker, 1, 0)

  # If statement_type is continue 
  # then find the most recent, earlier, talk.start and give those two the same number (later we could get the speaker details from that and potentially even link them) 
  # If the statement has 'The CHAIRMAN' at the start of it (e.g. 1131445) 
  # then need to adjust all other fields to fill in the details if that's not already done.
  



