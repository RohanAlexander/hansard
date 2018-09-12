# !diagnostics off
#### Preamble ####
# Purpose: This file takes Australian Hansard PDF files and it converts them to CSVs of text data that can be analysed.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 12 September 2018
# Prerequisites: You need to have downloaded the PDFs from the parliament's website. There are many GBs of PDFs and they are saved on an external drive - have fun finding that future-Rohan - and also the Berkeley Demography server. For testing purposes there should be some in the /inputs/for_testing_hansard_pdf folder.
# To do:
# - Check which footers need to be replaced
# - Better identify which page talking starts at
# - Deal with two columns better - e.g. line 162
# - Better identify speakers and look within cells


#### Set up workspace ####
library(furrr)
# devtools::install_github("DavisVaughan/furrr")
library(lubridate)
library(pdftools)
library(tidyverse)
library(tictoc)
library(tm)
# update.packages()

# Get the spell checker
load("outputs/misc/corrections.RData")

# Set up furrr plan
plan(multiprocess)


#### Create lists of PDFs to read and file names to save text as ####
# Get list of Hansard PDF filenames
# Change the path as required:
# use_this_path_to_get_pdfs  <- "/Volumes/Backup/hansard_pdfs"
use_this_path_to_get_pdfs  <- "inputs/for_testing_hansard_pdf"

use_this_path_to_save_csv_files  <- "outputs/hansard/hansard_csv_files"
# use_this_path_to_save_csv_files  <- "/Volumes/Backup/hansard_csv"

file_names <-
  list.files(
    path = use_this_path_to_get_pdfs,
    pattern = "*.pdf",
    recursive = TRUE,
    full.names = TRUE
  )
file_names <- file_names %>% sample()
file_names

# file_names <- sample(file_names, 500)  # Just get 500 of them
# get_these <- paste0("/Volumes/Backup/hansard_pdfs/", relevant_dates$allDates, ".pdf")
# file_names <- file_names[file_names %in% get_these] 

# parsed_these_already <- read_csv("parsed_these.csv") %>% pull
# head(parsed_these_already)
# file_names <- file_names[!file_names %in% parsed_these_already]

save_names <- file_names %>%
  str_replace(use_this_path_to_get_pdfs, "") %>%
  str_replace(".pdf", ".csv")

save_names <- paste0(use_this_path_to_save_csv_files, save_names)
save_names

# which(save_names == "/Volumes/Backup/hansard_csv/1915-04-23.csv")
# didntwork <- save_names[118]

# file_names <- file_names[119:length(file_names)]
# save_names <- save_names[119:length(save_names)]

# write_lines(file_names, "parsed_these.csv")


#### Create the function that will be applied to the files ####
get_text_from_PDFs <-
  function(name_of_input_PDF_file,
           name_of_output_csv_file) {
    
    # Read in the document, based on the filename list, and general tidying
    # name_of_input_PDF_file <- "inputs/for_testing_hansard_pdf/1998-06-02.pdf" # for testing
    # name_of_input_PDF_file <- "inputs/for_testing_hansard_pdf/1907-08-14.pdf" # for testing
    
    pdf_document <-
      pdf_text(name_of_input_PDF_file)
    
    # Get the date - it's needed later in the single or double columns
    date_of_doc <- basename(name_of_input_PDF_file) %>% # basename gets rid of the path to the file
      str_replace(".pdf", "") %>% 
      ymd()
    
    # Convert to tibble so that tidyverse can be used
    pdf_document_tibble <- tibble(text = pdf_document)
    rm(pdf_document)

    # Now each row is a page of the PDF so adding a column of the row numbers allows you to keep track of the page numbers later on
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
      str_replace_all(pdf_document_tibble$text, "—", "-") # I'm as pedantic as the next person about dash, hyphen, em dash usage, but this is not the place.
    pdf_document_tibble$text <-
      str_replace_all(pdf_document_tibble$text, "\\b- \\b", "") # Hyphens are being retained improperly e.g. Roh- an and that would affect the words analysis so needs to be fixed
    pdf_document_tibble$text <-
      str_replace_all(pdf_document_tibble$text, "(?<=[:space:][:upper:])[:space:](?=[:upper:][:space:])", "") # This one is picking up annoying spaces e.g. R O H A N should be Rohan. The regular expression is a bit wild, but each bit in round brackets is looking either side of the space and then removing that space as appropriate - see 'Look Arounds' in the stringr cheatsheet. There's a function - kerning - in the textclean package which does a similar task, but it seems to have an error that binds it to the next word if that's capitalised. Thanks Monica, also https://stackoverflow.com/questions/31280327/remove-extra-white-space-from-between-letters-in-r-using-gsub
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
             firstTookTheChairRow = str_detect(text, "took the chair"))   # UPDATE THIS SO IT IGNORES CASE
    pdf_document_tibble$firstSpeakerRow[pdf_document_tibble$firstSpeakerRow == FALSE] <- NA
    pdf_document_tibble$firstJointHouseRow[pdf_document_tibble$firstJointHouseRow == FALSE] <- NA
    pdf_document_tibble$firstTookTheChairRow[pdf_document_tibble$firstTookTheChairRow == FALSE] <- NA
    
    # Get the row and corresponding page and then filter to only pages from that page
    row_of_first_SPEAKER <-
      pdf_document_tibble$firstSpeakerRow[pdf_document_tibble$firstSpeakerRow == TRUE] %>% which() %>% first()
    row_of_first_JOINTHOUSE <- 
      pdf_document_tibble$firstJointHouseRow[pdf_document_tibble$firstJointHouseRow == TRUE] %>% which() %>% first()
    row_of_first_TookTheChair <- 
      pdf_document_tibble$firstTookTheChairRow[pdf_document_tibble$firstTookTheChairRow == TRUE] %>% which() %>% first()
    
    first_page_of_interest_SPEAKER <-
      pdf_document_tibble[row_of_first_SPEAKER, "pageNumbers"] %>% as.integer()
    first_page_of_interest_JOINTHOUSE <-
      pdf_document_tibble[row_of_first_JOINTHOUSE, "pageNumbers"] %>% as.integer()
    first_page_of_interest_TookTheChair <-
      pdf_document_tibble[row_of_first_TookTheChair, "pageNumbers"] %>% as.integer()
    pdf_document_tibble <- pdf_document_tibble %>%
      filter(pageNumbers >= min(first_page_of_interest_SPEAKER, first_page_of_interest_JOINTHOUSE+1, first_page_of_interest_TookTheChair, na.rm = TRUE)) %>%
      select(-firstSpeakerRow, -firstJointHouseRow, -firstTookTheChairRow)
    rm(first_page_of_interest_SPEAKER, row_of_first_SPEAKER, row_of_first_TookTheChair)
    
    # write_csv(pdf_document_tibble, "text.csv")
    ## The PDFs until (not including) 2013-11-12 are arranged as two columns on each page and so most rows are two different speeches and those columns need to be separated
    if (date_of_doc < 2013-11-12) {
    # Split it based on the number of characters - can probably finetune this.
    pdf_document_tibble <- pdf_document_tibble %>%
      # mutate(line_number = 1:nrow(pdf_document_tibble)) %>%
      mutate(
        line_type = case_when(
          str_detect(text, "^\\s{40,}") == TRUE ~ "secondColumnOnly", # If there is at least 40 spaces in a row at the start then it's only got content in the second column
          nchar(text) < 48 ~ "firstColumnOnly", # If there is less than 48 characters then it's only got content in the first column
          TRUE ~ "both" # Otherwise there is content in both columns
        )
      )
    # write_csv(pdf_document_tibble, "testing.csv") # Use this while testing if you want to see what it's looking like
    
    pdf_document_tibble <- pdf_document_tibble %>%
      mutate(text = if_else(
        line_type %in% c("both", "firstColumnOnly"),
        str_trim(text, side = c("left")),
        text
      )) # Just remove whitespace at the left of the string. Couldn't do it until here because the existence of whitespace on the left of the string was how the right column only lines were identified.
    
    # Right, let's try this - don't @ me - it should work well enough and my supervisor is breathing down my neck
    # Come back here - there are issues with the parsing here that are affecting the specifics of the statements. It's fit for purpose, but not ideal. 
    pdf_document_tibble <- pdf_document_tibble %>% 
      mutate(ordering = 1:nrow(pdf_document_tibble)) # This is important so that the order can be reconstructed later
    
    # Come back and make this to work a little better - throwing away a lot - extra = "merge" - hides them so get rid of that then come back here.
    ## PLAYGROUND START
    pdf_document_tibble_both <- pdf_document_tibble %>%
      filter(line_type == "both") %>%
      mutate(text = str_replace(text, "(?<=(.){38,50})[:space:]{2,}", "MONICAHOWLETT"),
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
    
    pdf_document_tibble_colapse <- pdf_document_tibble %>%
      select(-pageNumbers) %>% 
      # group_by(speakerGroups) %>%
      summarise(text = paste(text, collapse = " ")) %>% 
      mutate(text = str_replace_all(text, "MONICA ", "")) %>% 
      mutate(text = str_replace_all(text, "[:space:]-(?=[:alpha:])", " - "))
      

    
    # write_csv(pdf_document_tibble_colapse, "lookat.csv")
    #END #
    
    # pdf_document_tibble$possibleSpeaker <-
    #   ifelse(pdf_document_tibble$possibleSpeaker == TRUE, 1, 0)
    # # Some of the speakers were hard to distinguish - this just adds a maximum that we're going to get
    # pdf_document_tibble <- pdf_document_tibble %>% 
    #   mutate(text = if_else(possibleSpeaker == 1, paste(text, "MONICA"), text))
    # pdf_document_tibble <- pdf_document_tibble %>%
    #   mutate(speakerGroups = cumsum(possibleSpeaker))
    # # Thanks to Mark Needham for this: https://markhneedham.com/blog/2015/06/27/r-dplyr-squashing-multiple-rows-per-group-into-one/
    # pdf_document_tibble <- pdf_document_tibble %>%
    #   group_by(speakerGroups) %>%
    #   summarise(text = paste(text, collapse = " "),
    #             pageNumbers = min(pageNumbers),
    #             title = first(title))
    

    # #### Undoing this bit for now COME BACK AND IMPROVE ####
    # 
    # # Identify speakers - THIS CAN BE IMPROVED e.g. does it look within a string?
    # # This is the way we identify them - amend as appropriate:
    # # findSpeakersBasedOnThis <-
    # #   c("Dr\\.*[:alpha:]{3,}\\.*"
    # #     "Mr [:upper:] ([:alpha::punct:])-"
    # #   )
    # 
    # # str_detect("Mr KEATING(Blaxland-Prime Minister)-", "Mr [:upper:]+[:punct:][:alpha::punct:][:punct:]-")
    # 
    # findSpeakersBasedOnThis <-
    #   c(
    #     "Dr [:upper:]{3,}-",
    #     "Dr [:alpha:]{3,}-",
    #     "Ms [:upper:]{3,}-",
    #     "Mr [:alpha:]{3,}[:space:]*-",
    #     "Mr [:alpha:]{3,}.-",
    #     "Mr [:upper:]{3,}",
    #     "Mr [:upper:]\\. [:upper:]{3,}",
    #     "Mr. [:upper:]{3,} .",
    #     "Mr. [:upper:]{3,}.",
    #     "Mr.[:upper:]{3,}.",
    #     "Mr. [:upper:]{3,}[:space:][:upper:]{3,}.",
    #     "Dr. [:upper:]{3,}.",
    #     "Sir [:upper:]{3,}[:space:][:upper:]{3,}.",
    #     "Colonel [:upper:]{3,}[:punct:]",
    #     "The Clerk-",
    #     "The CHAIRMAN",
    #     "The DEPUTY SPEAKER",
    #     "The SPEAKER",
    #     "An Honorable MEMBER",
    #     "Mrs DEANNE KELLY",
    #     "Opposition members interjecting-",
    #     "Honourable members interjecting-",
    #     "Mr Leo McLeay-",
    #     "Ms CHESTERS ",
    #     "' Mr Calwell"
    #   )
    # findSpeakersBasedOnThis <-
    #   paste(findSpeakersBasedOnThis, collapse = "|")
    # 
    # # Identify names of debates
    # findTitlesBasedOnThis <-
    #   c("^[:upper:]{5,}")
    # findTitlesBasedOnThis <-
    #   paste(findTitlesBasedOnThis, collapse = "|")
    # 
    # # Add exceptions here
    # capitalisedButNotTitles <- c("RAAF VIP jet", "QUEEN VICTORIA BUILDING NSW 1230", "HEPP", "RDAF", "BARBECURE", "EFTPOS", "COSBOA", "NDIS", "AFP", "ABS", "CSIRO", "ALRC", "ASX", "OECD", "GST", "GDP", "ASB", "ABC", "MPI", "ALP")
    # capitalisedButNotTitles <-
    #   paste(capitalisedButNotTitles, collapse = "|")
    # 
    # pdf_document_tibble <- pdf_document_tibble %>%
    #   mutate(
    #     possibleSpeaker = str_detect(text, findSpeakersBasedOnThis),
    #     possibleTitle = str_detect(text, findTitlesBasedOnThis),
    #     possibleTitle = if_else(str_detect(text, capitalisedButNotTitles), FALSE, possibleTitle),
    #     possibleTitle = if_else(possibleSpeaker == TRUE & possibleTitle == TRUE, FALSE, possibleTitle),
    #     possibleTitle = ifelse(possibleTitle == TRUE, 'possibleTitle', 'probablyNotATitle')
    #   )
    # 
    # # Want the titles to be in their own column
    # pdf_document_tibble <- pdf_document_tibble %>%
    #   mutate(counter = 1:n()) %>%
    #   spread(possibleTitle, text)
    # 
    # # Some of the titles are spread over multiple rows. We need to be able to push those into just one row. It's not pretty, but it works.
    # pdf_document_tibble <- pdf_document_tibble %>%
    #   mutate(
    #     lagPossibleTitle = lag(possibleTitle),
    #     titleNumber = if_else(!is.na(possibleTitle) & is.na(lagPossibleTitle), 1, 0)) %>%
    #   mutate(
    #     titleNumberCumulative = cumsum(titleNumber),
    #     titleNumberCumulative = ifelse(is.na(possibleTitle), NA, titleNumberCumulative)) %>%
    #   select(-lagPossibleTitle)
    # 
    # # This is the titles properly fomatted
    # combined_titles <- pdf_document_tibble %>%
    #   filter(!is.na(titleNumberCumulative)) %>%
    #   group_by(titleNumberCumulative) %>%
    #   summarise(possibleTitle = paste(possibleTitle, collapse = " "))
    # 
    # # Get rid of the rows that are just the debris of the titles that we don't want
    # pdf_document_tibble <- pdf_document_tibble %>%
    #   filter(is.na(titleNumberCumulative) | titleNumber != 0) %>%
    #   select(-possibleTitle)
    # 
    # # Join the titles back into the main tibble
    # pdf_document_tibble <- pdf_document_tibble %>%
    #   left_join(combined_titles, by = "titleNumberCumulative")
    # rm(combined_titles)
    # 
    # pdf_document_tibble <- pdf_document_tibble %>%
    #   mutate(possibleTitle = lag(possibleTitle)) %>%
    #   select(-counter, -titleNumber, -titleNumberCumulative) %>% 
    #   rename(text = probablyNotATitle, title = possibleTitle)
    # 
    # pdf_document_tibble$title[1] <- "SETUP"
    # 
    # pdf_document_tibble <- pdf_document_tibble %>% 
    #   filter(!is.na(text)) %>% 
    #   mutate(title = str_replace_all(title, '[:punct:]', " "),
    #          title = str_squish(title),
    #          title = str_to_title(title)) %>% 
    #   fill(title) 
    # 
    # 
    # 
    # ## Deal with the speakers
    # pdf_document_tibble$possibleSpeaker <-
    #   ifelse(pdf_document_tibble$possibleSpeaker == TRUE, 1, 0)
    # # Some of the speakers were hard to distinguish - this just adds a maximum that we're going to get
    # pdf_document_tibble <- pdf_document_tibble %>% 
    #   mutate(text = if_else(possibleSpeaker == 1, paste(text, "MONICA"), text))
    # pdf_document_tibble <- pdf_document_tibble %>%
    #   mutate(speakerGroups = cumsum(possibleSpeaker))
    # # Thanks to Mark Needham for this: https://markhneedham.com/blog/2015/06/27/r-dplyr-squashing-multiple-rows-per-group-into-one/
    # pdf_document_tibble <- pdf_document_tibble %>%
    #   group_by(speakerGroups) %>%
    #   summarise(text = paste(text, collapse = " "),
    #             pageNumbers = min(pageNumbers),
    #             title = first(title))
    # 
    # 
    # 
    # 
    # 
    # pdf_document_tibble <- pdf_document_tibble %>%
    #   separate(text, c("speaker", "theText"), sep = "-|\\):|MONICA|(?<=[:upper:]{2,10})[:]", extra = "merge") %>%
    #   mutate(speaker = str_squish(speaker),
    #          speaker = str_to_title(speaker))
    # 
    # pdf_document_tibble <- pdf_document_tibble %>% 
    #   mutate(theText = str_replace_all(theText, "- ", ""), #Deal with hyphens that were across lines
    #          theText = str_squish(theText),
    #          theText = str_replace_all(theText, "(?<=[:upper:][:lower:]{1,7})[:space:](?=[^a:lower:][:space:])", ""),
    #          speaker = str_squish(speaker),
    #          title = str_squish(title))
    # 
    # # Remove the just-in-case speaker distinguisher
    # pdf_document_tibble$theText <- str_replace_all(pdf_document_tibble$theText, "[:space:]?MONICA[:space:]?", "")
    # # Undo the MCM change
    # pdf_document_tibble$theText <- str_replace_all(pdf_document_tibble$theText, "MC(?=[:upper:]+)", "Mc")
    # # Sometimes in the more recent Hansard the title is being included
    # # pdf_document_tibble$theTexttest <- str_replace_all(pdf_document_tibble$theText, "^([:graph:]+[:space:]){3,}\\(..:..\\):", "")
    # 
    #### 


    # Fix spelling
    # pdf_document_tibble$theText <- str_replace_all(pdf_document_tibble$theText, corrections)
    # pdf_document_tibble$speaker <- str_replace_all(pdf_document_tibble$speaker, corrections)
    # pdf_document_tibble$title <- str_replace_all(pdf_document_tibble$title, corrections)
    
    #Fix the spelling while it's cut down
    pdf_document_tibble$text <-  str_replace_all(pdf_document_tibble$text, corrections)

    # write_csv(pdf_document_tibble, "testing.csv") # Use this while testing if you want to see what it's looking like

    # Save file
    # write_csv(doc_tibble, "name_of_output_csv_file")
    write_csv(pdf_document_tibble, name_of_output_csv_file)
    
    print(paste0("Done with ", name_of_output_csv_file, " at ", Sys.time()))
  }


#### Walk through the lists and parse the PDFs ####
# tic("Normal walk2")
# walk2(file_names, save_names, ~ get_text_from_PDFs(.x, .y))
# toc()

safely_get_text_from_PDFs <- safely(get_text_from_PDFs)

tic("Furrr walk2 progress")
future_walk2(file_names, save_names, ~ safely_get_text_from_PDFs(.x, .y), .progress = TRUE)
toc()

