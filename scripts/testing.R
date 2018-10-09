library(furrr)
library(lubridate)
library(pdftools)
library(stringi)
library(tidyverse)
library(tictoc)
library(tm)
# update.packages()


name_of_input_PDF_file <- "inputs/for_testing_hansard_pdf/1971-10-05.pdf" # uncomment for testing
pdf_document <- pdf_text(name_of_input_PDF_file)

# Get the date based on the filename
date_of_doc <- basename(name_of_input_PDF_file) %>% # basename gets rid of the path to the file
  str_replace(".pdf", "") %>% 
  ymd()

# Convert to tibble so that tidyverse can be used
pdf_document_tibble <- tibble(text = pdf_document)
rm(pdf_document)

# Each row is now a page of the PDF so adding a column of the row numbers allows you to keep track of the page numbers later on
pdf_document_tibble$pageNumbers <- 1:nrow(pdf_document_tibble)

# Separate each line (of each page) into it's own row
pdf_document_tibble <-
  separate_rows(pdf_document_tibble, text, sep = "\\n")

# Fix some minor punctuation issues to make the analysis easier
# str_count(pdf_document_tibble$text, "(?<=[:lower:])I(?=[:space:])") %>% sum() # Use this if you suspect an issue and it'll give you a count of how many rows are affected
# which(str_detect(pdf_document_tibble$text, "(?<=[:lower:])I(?=[:space:])")==TRUE) # Use this to tell you what line it's on
pdf_document_tibble$text <-
  str_replace_all(pdf_document_tibble$text, "’", "'") 
pdf_document_tibble$text <-
  str_replace_all(pdf_document_tibble$text, "•", "-") # Some hyphens are being read in as big dots
pdf_document_tibble$text <-
  str_replace_all(pdf_document_tibble$text, "\\b- \\b", "") # Hyphens are being retained improperly e.g. Roh- an and that would affect the words analysis so needs to be fixed

pdf_document_tibble$text <-
  str_replace_all(pdf_document_tibble$text, "—", "-") # I'm as pedantic as the next person about dash, hyphen, em dash usage, but this is not the place.
pdf_document_tibble$text <-
  str_replace_all(pdf_document_tibble$text, "(?<=[:space:][:upper:])[:space:](?=[:upper:][:space:])", "") # This one is picking up annoying spaces e.g. R O H A N should be Rohan. The regular expression is a bit wild, but each bit in round brackets is looking either side of the space and then removing that space as appropriate - see 'Look Arounds' in the stringr cheatsheet. There's a function - kerning - in the textclean package which does a similar task, but it seems to have an error that binds it to the next word if that's capitalised. Thanks Monica and also https://stackoverflow.com/questions/31280327/remove-extra-white-space-from-between-letters-in-r-using-gsub
# These next ones are just to make search and replacements work more consistently
pdf_document_tibble$text <-
  str_replace_all(pdf_document_tibble$text, "-[:space:]{2}", "- ") # A dash followed by two spaces should be changed to a dash followed by one space
pdf_document_tibble$text <-
  str_replace_all(pdf_document_tibble$text, "\\.[:space:]*-", " -")
pdf_document_tibble$text <-
  str_replace_all(pdf_document_tibble$text, "~", "-")

# Fix some other issues that just make everything a bit easier
pdf_document_tibble$text <-
  str_replace_all(pdf_document_tibble$text, "M[:space:]?r[:punct:]+[:space:]|Mr[:space:]*[:punct:]+", "Mr ")
# str_detect("Mr . THOMAS ", "M[:space:]?r[:punct:]+[:space:]|Mr[:space:]*[:punct:]+")
pdf_document_tibble$text <-
  str_replace_all(pdf_document_tibble$text, "M[:space:]?r[:space:]*[:punct:]*[:space:]", "Mr ")
pdf_document_tibble$text <-
  str_replace_all(pdf_document_tibble$text, "DE-ANNE", "DEANNE")
pdf_document_tibble$text <-
  str_replace_all(pdf_document_tibble$text, "- BRITIS H", "BRITISH")
pdf_document_tibble$text <-
  str_replace_all(pdf_document_tibble$text, "McDONALD", "MCDONALD")
pdf_document_tibble$text <-
  str_replace_all(pdf_document_tibble$text, "thatAn", "that An")
pdf_document_tibble$text <-
  str_replace_all(pdf_document_tibble$text, "(?<=\\S)Sir", " Sir")
pdf_document_tibble$text <-
  str_replace_all(pdf_document_tibble$text, "S i r", "Sir")
pdf_document_tibble$text <-
  str_replace_all(pdf_document_tibble$text, "EDWARDBRADDON", "EDWARD BRADDON")
pdf_document_tibble$text <-
  str_replace_all(pdf_document_tibble$text, "WILLIAMM c MILLAN", "WILLIAM MCMILLAN")
pdf_document_tibble$text <-
  str_replace_all(pdf_document_tibble$text, "Mc(?=[:upper:]+)", "MC")
pdf_document_tibble$text <-
  str_replace_all(pdf_document_tibble$text, "(?<=\\S)Mr", " Mr")

# Fix related to the Speaker taking the chair
pdf_document_tibble$text <-
  str_replace_all(pdf_document_tibble$text, "SPEAKEB", "SPEAKER")
pdf_document_tibble$text <-
  str_replace_all(pdf_document_tibble$text, "ohair", "chair")
pdf_document_tibble$text <-
  str_replace_all(pdf_document_tibble$text, "SruAKEK", "SPEAKER")
pdf_document_tibble$text <-
  str_replace_all(pdf_document_tibble$text, "SJ-BAKJSB", "SPEAKER")
pdf_document_tibble$text <-
  str_replace_all(pdf_document_tibble$text, "Mx: SI-BAKER", "MR SPEAKER")
pdf_document_tibble$text <-
  str_replace_all(pdf_document_tibble$text, "Mr SFEAKEB.", "MR SPEAKER")
pdf_document_tibble$text <-
  str_replace_all(pdf_document_tibble$text, "took the. chair", "took the chair")
pdf_document_tibble$text <-
  str_replace_all(pdf_document_tibble$text, "took tlie chair", "took the chair")
pdf_document_tibble$text <-
  str_replace_all(pdf_document_tibble$text, "took tiie chair", "took the chair")
pdf_document_tibble$text <-
  str_replace_all(pdf_document_tibble$text, "took 'the chair", "took the chair")

## Identify page headers and footers and remove them
pdf_document_tibble <- pdf_document_tibble %>%
  group_by(pageNumbers) %>%
  mutate(lineNumber = 1:n()) %>% # This gives you a line numbering for each page
  mutate(lastLine = n()) %>% # This tells you the number of the last line in each page
  ungroup()

# There are some footers in the 2017 ones - they are rows that just have 'CHAMBER' and some 'FEDERATION CHAMBER'
pdf_document_tibble <- pdf_document_tibble %>%
  filter(lineNumber != 1) %>% # Remove the first line of each page
  filter(text %>% str_trim != "CHAMBER") %>% # Remove the footer when it exists
  filter(text %>% str_trim != "FEDERATION CHAMBER") %>% # Remove the footer when it exists
  select(-lineNumber, -lastLine)


## Identify front matter and remove it
# Primarily identify the start of talking based on the first occurence of 'Mr SPEAKER'. It seems pretty common, but there are some misses (e.g. 1991-01-22). As a backup look for first occurence of "JOINT HOUSE" and then pick the next page. And look for 'took the chair' and pick that page.
pdf_document_tibble <- pdf_document_tibble %>%
  mutate(firstSpeakerRow = str_detect(text, "SPEAKER"),
         firstJointHouseRow = str_detect(text, "JOINT HOUSE"),
         firstTookTheChairRow = str_detect(text, "took the chair"),
         firstTheChairAtRow = str_detect(text, "the chair at"))  
pdf_document_tibble$firstSpeakerRow[pdf_document_tibble$firstSpeakerRow == FALSE] <- NA
pdf_document_tibble$firstJointHouseRow[pdf_document_tibble$firstJointHouseRow == FALSE] <- NA
pdf_document_tibble$firstTookTheChairRow[pdf_document_tibble$firstTookTheChairRow == FALSE] <- NA
pdf_document_tibble$firstTheChairAtRow[pdf_document_tibble$firstTheChairAtRow == FALSE] <- NA

# Get the row and corresponding page and then filter to only pages from that page
row_of_first_SPEAKER <-
  pdf_document_tibble$firstSpeakerRow[pdf_document_tibble$firstSpeakerRow == TRUE] %>% which() %>% first()
row_of_first_JOINTHOUSE <- 
  pdf_document_tibble$firstJointHouseRow[pdf_document_tibble$firstJointHouseRow == TRUE] %>% which() %>% first()
row_of_first_TookTheChair <- 
  pdf_document_tibble$firstTookTheChairRow[pdf_document_tibble$firstTookTheChairRow == TRUE] %>% which() %>% first()
row_of_first_TheChairAt <- 
  pdf_document_tibble$firstTheChairAtRow[pdf_document_tibble$firstTheChairAtRow == TRUE] %>% which() %>% first()

first_page_of_interest_SPEAKER <-
  pdf_document_tibble[row_of_first_SPEAKER, "pageNumbers"] %>% as.integer()
first_page_of_interest_JOINTHOUSE <-
  pdf_document_tibble[row_of_first_JOINTHOUSE, "pageNumbers"] %>% as.integer()
first_page_of_interest_TookTheChair <-
  pdf_document_tibble[row_of_first_TookTheChair, "pageNumbers"] %>% as.integer()
first_page_of_interest_TheChairAt <-
  pdf_document_tibble[row_of_first_TheChairAt, "pageNumbers"] %>% as.integer()

first_page_of_interest_JOINTHOUSE <- (first_page_of_interest_JOINTHOUSE + 1)  %>% as.integer()

filter_from_here <- case_when(!is.na(first_page_of_interest_TookTheChair) ~ first_page_of_interest_TookTheChair,
                              !is.na(first_page_of_interest_TheChairAt) ~ first_page_of_interest_TheChairAt,
                              !is.na(first_page_of_interest_SPEAKER) ~ first_page_of_interest_SPEAKER,
                              TRUE ~ first_page_of_interest_JOINTHOUSE
)

pdf_document_tibble <- pdf_document_tibble %>%
  filter(pageNumbers >= filter_from_here) %>%
  select(-firstSpeakerRow, -firstJointHouseRow, -firstTookTheChairRow)
rm(first_page_of_interest_SPEAKER, row_of_first_SPEAKER, row_of_first_TookTheChair)




#### PROBABLY SHOULD SPLIT IT HERE ####

# write_csv(pdf_document_tibble, "test.csv") Just for testing

## The PDFs until (not including) 2013-11-12 are arranged as two columns on each page and so most rows are two different speeches and those columns need to be separated
if (date_of_doc < 2013-11-12) {
  # Split it based on the number of characters - can probably finetune this.
  pdf_document_tibble <- pdf_document_tibble %>%
    mutate(
      line_type = case_when(
        str_detect(text, "^\\s{26,}") == TRUE ~ "secondColumnOnly", # If there is at least 26 spaces in a row at the start then it's only got content in the second column. 26 is the minimum to make 
        nchar(text) < 48 ~ "firstColumnOnly", # If there is less than 48 characters then it's only got content in the first column
        TRUE ~ "both" # Otherwise there is content in both columns
      )
    )
  # write_csv(pdf_document_tibble, "test.csv") # Just for testing
  
  # # 13 September - TRY NOT DOING THIS    
  # pdf_document_tibble <- pdf_document_tibble %>%
  #   mutate(text = if_else(
  #     line_type %in% c("both", "firstColumnOnly"),
  #     str_trim(text, side = c("left")),
  #     text
  #   )) # Just remove whitespace at the left of the string. Couldn't do it until here because the existence of whitespace on the left of the string was how the right column only lines were identified.
  
  # Right, let's try this - don't @ me - it should work well enough and my supervisor is breathing down my neck
  # Come back here - there are issues with the parsing here that are affecting the specifics of the statements. It's fit for purpose, but not ideal. 
  pdf_document_tibble <- pdf_document_tibble %>% 
    mutate(ordering = 1:nrow(pdf_document_tibble)) # This is important so that the order can be reconstructed later
  
  # Come back and make this to work a little better - throwing away a lot - extra = "merge" - hides them so get rid of that then come back here.
  ## PLAYGROUND START
  pdf_document_tibble_both <- pdf_document_tibble %>%
    filter(line_type == "both") %>%
    mutate(text = str_replace(text, "(?<=(.){37,60})[:space:]{2,}", "MONICAHOWLETT"), # The start needs to be at most 37 because of 1901-06-27. The end needs to be pushed over because of 1971-10-05.
           text = if_else(str_detect(text, "MONICAHOWLETT"), text, str_replace(text, "(?<=(.){42,50})[:space:]", "MONICAHOWLETT"))) %>% 
    separate(
      text,
      c("first_column", "second_column"),
      # sep = "[:space:]{2,}",
      sep = "MONICAHOWLETT", # Trying this
      remove = FALSE,
      extra = "merge"
    )
  
  pdf_document_tibble_secondColumnOnly <- pdf_document_tibble %>% 
    filter(line_type == "secondColumnOnly") %>% 
    mutate(first_column = NA, second_column = text)
  
  pdf_document_tibble_firstColumnOnly <- pdf_document_tibble %>% 
    filter(line_type == "firstColumnOnly") %>% 
    mutate(first_column = text, second_column = NA)
  
  pdf_document_tibble <-
    bind_rows(
      pdf_document_tibble_both,
      pdf_document_tibble_secondColumnOnly,
      pdf_document_tibble_firstColumnOnly
    ) %>%
    arrange(ordering)
  
  rm(
    pdf_document_tibble_both,
    pdf_document_tibble_secondColumnOnly,
    pdf_document_tibble_firstColumnOnly
  )
  
  # Now the columns of the PDF are split into two columns and we need to put them back together in order - first column on top then second column
  pdf_document_tibble <- pdf_document_tibble %>%
    select(-c(text, line_type, ordering)) %>%
    gather(position, textInPosition, -c(pageNumbers)) %>%
    mutate(counter = 1:n()) %>%
    arrange(pageNumbers, counter)
  
  # Check for empty rows and remove them
  pdf_document_tibble <- pdf_document_tibble %>%
    mutate(emptyCell = if_else(textInPosition == "" | is.na(textInPosition), 1, 0)) %>%
    filter(emptyCell == 0) %>%
    select(-emptyCell, -position, -counter) %>%
    rename(text = textInPosition)
}

## The next task is to split out the topic names and the names of the speakers
# Remove any extra whitespace i.e. two or more spaces and spaces at either end
pdf_document_tibble$text <-
  str_squish(pdf_document_tibble$text)


# Check for empty rows and remove them
pdf_document_tibble <- pdf_document_tibble %>%
  mutate(emptyCell = if_else(text == "" | is.na(text), 1, 0)) %>%
  filter(emptyCell == 0) %>%
  select(-emptyCell)

#START # Just use this to get everything into one line
pdf_document_tibble <- pdf_document_tibble %>%
  mutate(text = str_replace_all(text, "-$", "MONICA"))

pdf_document_tibble <- pdf_document_tibble %>%
  select(-pageNumbers) %>% 
  # group_by(speakerGroups) %>%
  summarise(text = paste(text, collapse = " ")) %>% 
  mutate(text = str_replace_all(text, "MONICA ", "")) %>% 
  mutate(text = str_replace_all(text, "[:space:]-(?=[:alpha:])", " - "))

