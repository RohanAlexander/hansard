# !diagnostics off
#### Preamble ####
# Purpose: This file takes Australian Hansard PDF files and it converts them to CSVs of text data that can be analysed.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 3 September 2018
# Prerequisites: You need to have downloaded the PDFs from the parliament's website. There are many GBs of PDFs and they are saved on an external drive - have fun finding that future-Rohan - and also the Berkeley Demography server. For testing purposes there should be some in the /data folder.
# To do:
# - Check if footers need to be replaced
# - Better identify which page talking starts at
# - Deal with two columns better - e.g. line 162
# - Better identify speakers and look within cells


#### Set up workspace ####
library(furrr)
# devtools::install_github("DavisVaughan/furrr")
library(pdftools)
library(tidyverse)
library(tictoc)
library(tm)
# update.packages()

# Get the spell checker
load("outputs/corrections.RData")

# Set up furrr plan
plan(multiprocess)


#### Create lists of PDFs to read and file names to save text as ####
# Get list of Hansard PDF filenames
# Change the path as required:
use_this_path_to_get_pdfs  <- "data/for_testing_hansard_pdf"
# use_this_path_to_get_pdfs  <- "/Volumes/SanDisk/hansard_pdfs"

use_this_path_to_save_csv_files  <-
  "outputs/hansard/hansard_csv_files"
# use_this_path_to_save_csv_files  <- "/Volumes/SanDisk/hansard_csv"

file_names <-
  list.files(
    path = use_this_path_to_get_pdfs,
    pattern = "*.pdf",
    recursive = TRUE,
    full.names = TRUE
  )
file_names

save_names <- file_names %>%
  str_replace(use_this_path_to_get_pdfs, "") %>%
  str_replace(".pdf", ".csv")

save_names <- paste0(use_this_path_to_save_csv_files, save_names)
save_names


#### Create the function that will be applied to the files ####
get_text_from_PDFs <-
  function(name_of_input_PDF_file,
           name_of_output_csv_file) {
    ## Read in the document, based on the filename list, and general tidying
    # pdf_document <-
    #   pdf_text(name_of_input_PDF_file)
    pdf_document <-
      pdf_text("data/for_testing_hansard_pdf/1997-02-05.pdf") # for testing
    
    # Convert to tibble so that tidyverse can be used
    pdf_document_tibble <- tibble(text = pdf_document)
    rm(pdf_document)

    # Add the moment each row is a page of the PDF so adding a column of the row numbers allows you to keep track of the page numbers later on
    pdf_document_tibble$pageNumbers <- 1:nrow(pdf_document_tibble)
    
    # Separate each line (of each page) into it's own row
    pdf_document_tibble <-
      separate_rows(pdf_document_tibble, text, sep = "\\n")
    
    # Fix some minor issues in how the PDF has been read
    # str_count(pdf_document_tibble$text, "\\b- \\b") %>% sum() # Use this if you suspect an issue and it'll give you a count of how many rows are affected
    pdf_document_tibble$text <-
      str_replace_all(pdf_document_tibble$text, "•", "-") # Some hyphens are being read in as big dots
    pdf_document_tibble$text <-
      str_replace_all(pdf_document_tibble$text, "\\b- \\b", "") # Hyphens are being retained improperly e.g. Roh- an and that would affect the words analysis so needs to be fixed
    pdf_document_tibble$text <-
      str_replace_all(
        pdf_document_tibble$text,
        "(?<=[:space:][:upper:])[:space:](?=[:upper:][:space:])",
        ""
      ) # This one is picking up annoying spaces e.g. R O H A N should be Rohan. The regular expression is a bit wild, but each bit in round brackets is looking either side of the space and then removing that space as appropriate - see 'Look Arounds' in the stringr cheatsheet. There's a function - kerning - in the textclean package which does a similar task, but it seems to have an error that binds it to the next word if that's capitalised. Thanks Monica, also https://stackoverflow.com/questions/31280327/remove-extra-white-space-from-between-letters-in-r-using-gsub
    # These next ones are just to make search and replacements work more consistently
    pdf_document_tibble$text <-
      str_replace_all(pdf_document_tibble$text, "—", "— ")
    pdf_document_tibble$text <-
      str_replace_all(pdf_document_tibble$text, "—  ", "— ")
    pdf_document_tibble$text <-
      str_replace_all(pdf_document_tibble$text, ".—", " —")
    pdf_document_tibble$text <-
      str_replace_all(pdf_document_tibble$text, "(?<=[:alpha:])—", " —")
    pdf_document_tibble$text <-
      str_replace_all(pdf_document_tibble$text, "Mr.. ", "Mr ")
    pdf_document_tibble$text <-
      str_replace_all(pdf_document_tibble$text, "Mrs DE-ANNE KELLY", "Mrs DEANNE KELLY")
    pdf_document_tibble$text <-
      str_replace_all(pdf_document_tibble$text, "— BRITIS H APPLICANTS", "BRITISH APPLICANTS")
    pdf_document_tibble$text <-
      str_replace_all(pdf_document_tibble$text, "—(?=[:alpha:])", "— ")
    # str_count(pdf_document_tibble$text, "—(?=[:alpha:])") %>% sum() 
    
    # Fix spelling for the first time
    pdf_document_tibble$text <-
      str_replace_all(pdf_document_tibble$text, corrections)
    
    
    ## Identify page headers and footers and remove them
    pdf_document_tibble <- pdf_document_tibble %>%
      group_by(pageNumbers) %>%
      mutate(lineNumber = 1:n()) %>% # This gives you a line numbering for each page
      mutate(lastLine = n()) %>% # This tells you the number of the last line in each page
      ungroup()
    
    # There are some footers in the 2017 ones - they are rows that just have 'CHAMBER'
    pdf_document_tibble$text[6424] %>% str_trim()
    
    
    pdf_document_tibble <- pdf_document_tibble %>%
      filter(lineNumber != 1) %>% # Remove the first line of each page
      filter(text %>% str_trim != "CHAMBER") %>% # Remove the footer
      filter(text %>% str_trim != "FEDERATION CHAMBER") %>% # Remove the footer
      select(-lineNumber, -lastLine)
    
    # Identify footers and remove them
    # Not being done yet, but use the lastLine and the lineNumber from above if you need to do it.
    
    
    ## Identify front matter and remove it
    # Primarily identify the start of talking based on the first occurence of 'Mr SPEAKER'. It seems pretty common, but there are some misses (e.g. 1991-01-22). As a backup look for first occurence of "JOINT HOUSE" and then pick the next page.
    pdf_document_tibble <- pdf_document_tibble %>%
      mutate(firstSpeakerRow = str_detect(text, "SPEAKER"),
             firstJointHouseRow = str_detect(text, "JOINT HOUSE")) 
    pdf_document_tibble$firstSpeakerRow[pdf_document_tibble$firstSpeakerRow == FALSE] <- NA
    pdf_document_tibble$firstJointHouseRow[pdf_document_tibble$firstJointHouseRow == FALSE] <- NA
    # Get the row and corresponding page and then filter to only pages from that page
    row_of_first_SPEAKER <-
      pdf_document_tibble$firstSpeakerRow[pdf_document_tibble$firstSpeakerRow == TRUE] %>% which() %>% first()
    row_of_first_JOINTHOUSE <- 
      pdf_document_tibble$firstJointHouseRow[pdf_document_tibble$firstJointHouseRow == TRUE] %>% which() %>% first()
    first_page_of_interest_SPEAKER <-
      pdf_document_tibble[row_of_first_SPEAKER, "pageNumbers"] %>% as.integer()
    first_page_of_interest_JOINTHOUSE <-
      pdf_document_tibble[row_of_first_JOINTHOUSE, "pageNumbers"] %>% as.integer()
    pdf_document_tibble <- pdf_document_tibble %>%
      filter(pageNumbers >= min(first_page_of_interest_SPEAKER, first_page_of_interest_JOINTHOUSE+1, na.rm = TRUE)) %>%
      select(-firstSpeakerRow, -firstJointHouseRow)
    rm(first_page_of_interest_SPEAKER, row_of_first_SPEAKER)
    

    ## Most of the PDFs are arranged as two columns on each page and so most rows are two different speeches and those columns need to be separated
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
    # Come back here - there are issues with the parsing here that are affecting the specifics of the statements. It's fit for purpose, but not ideal. Should probably pre-process the text e.g. if letter then two spaces then letter in the first 40 spots then probably change to letter one space letter. Things like that.
    
    pdf_document_tibble <- pdf_document_tibble %>% 
      mutate(ordering = 1:nrow(pdf_document_tibble)) # This is important so that the order can be reconstructed later
    
    # Come back and make this to work a little better - throwing away a lot - extra = "merge" - hides them so get rid of that then come back here.
    pdf_document_tibble_both <- pdf_document_tibble %>%
      filter(line_type == "both") %>%
      separate(
        text,
        c("first_column", "second_column"),
        sep = "[:space:]{2,}",
        remove = FALSE,
        extra = "merge"
      )
    # write_csv(pdf_document_tibble_both, "testing.csv") # Use this while testing if you want to see what it's looking like
    
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
      select(-emptyCell, -position) %>% # Even though it seems superfluous, don't get rid of counter, because you need it, or something like it, later in the spread
      rename(text = textInPosition)
    
    
    ## The next task is to split out the topic names and the names of the speakers
    # Remove any extra whitespace i.e. two or more spaces and spaces at either end
    pdf_document_tibble$text <-
      str_squish(pdf_document_tibble$text)
    
    # Fix spelling, again
    pdf_document_tibble$text <-
      str_replace_all(pdf_document_tibble$text, corrections)
    
    # Identify speakers - THIS CAN BE IMPROVED e.g. does it look within a string?
    # This is the way we identify them - amend as appropriate:
    findSpeakersBasedOnThis <-
      c(
        "Dr [:upper:]{3,}-",
        "Dr [:alpha:]{3,}-",
        "Ms [:upper:]{3,}-",
        "Mr [:alpha:]{3,}-",
        "Mr [:alpha:]{3,}.—",
        "Mr [:upper:]{3,}",
        "Mr.McEWEN.",
        "Mr. McEWEN.",
        "Mr. [:upper:]{3,} .",
        "Mr. [:upper:]{3,}.",
        "Mr.[:upper:]{3,}.",
        "Mr. [:upper:]{3,}[:space:][:upper:]{3,}.",
        "Dr. [:upper:]{3,}.",
        "Sir [:upper:]{3,}[:space:][:upper:]{3,}.",
        "Colonel [:upper:]{3,}[:punct:]",
        "The Clerk-",
        "Mrs DEANNE KELLY",
        "Opposition members interjecting-",
        "Honourable members interjecting-",
        "Mr Leo McLeay-",
        "' Mr Calwell"
      )
    findSpeakersBasedOnThis <-
      paste(findSpeakersBasedOnThis, collapse = "|")

    # Identify names of debates
    findTitlesBasedOnThis <-
      c("^[:upper:]{3,}")
    findTitlesBasedOnThis <-
      paste(findTitlesBasedOnThis, collapse = "|")
    
    pdf_document_tibble <- pdf_document_tibble %>%
      mutate(
        possibleSpeaker = str_detect(text, findSpeakersBasedOnThis),
        possibleTitle = str_detect(text, findTitlesBasedOnThis),
        possibleTitle = if_else(possibleSpeaker == TRUE & possibleTitle == TRUE, FALSE, possibleTitle),
        possibleTitle = ifelse(possibleTitle == TRUE, 'possibleTitle', 'probablyNotATitle')
      )

    # Want the titles to be in their own column
    pdf_document_tibble <- pdf_document_tibble %>%
      spread(possibleTitle, text)
    
    # Some of the titles are spread over multiple rows. We need to be able to push those into just one row. It's not pretty, but it works.
    pdf_document_tibble <- pdf_document_tibble %>%
      mutate(
        lagPossibleTitle = lag(possibleTitle),
        titleNumber = if_else(!is.na(possibleTitle) & is.na(lagPossibleTitle), 1, 0)) %>%
      mutate(
        titleNumberCumulative = cumsum(titleNumber),
        titleNumberCumulative = ifelse(is.na(possibleTitle), NA, titleNumberCumulative)) %>%
      select(-lagPossibleTitle)
    
    # This is the titles properly fomatted
    combined_titles <- pdf_document_tibble %>%
      filter(!is.na(titleNumberCumulative)) %>%
      group_by(titleNumberCumulative) %>%
      summarise(possibleTitle = paste(possibleTitle, collapse = " "))
    
    # Get rid of the rows that are just the debris of the titles that we don't want
    pdf_document_tibble <- pdf_document_tibble %>%
      filter(is.na(titleNumberCumulative) | titleNumber != 0) %>%
      select(-possibleTitle)
    
    # Join the titles back into the main tibble
    pdf_document_tibble <- pdf_document_tibble %>%
      left_join(combined_titles, by = "titleNumberCumulative")
    rm(combined_titles)
    
    pdf_document_tibble <- pdf_document_tibble %>%
      mutate(possibleTitle = lag(possibleTitle)) %>%
      select(-counter, -titleNumber, -titleNumberCumulative) %>% 
      rename(text = probablyNotATitle, title = possibleTitle)
    
    pdf_document_tibble$title[1] <- "SETUP"
    
    pdf_document_tibble <- pdf_document_tibble %>% 
      filter(!is.na(text)) %>% 
      mutate(title = str_replace_all(title, '[:punct:]', " "),
             title = str_squish(title),
             title = str_to_title(title)) %>% 
      fill(title) 
    
    
    ## Deal with the speakers
    pdf_document_tibble$possibleSpeaker <-
      ifelse(pdf_document_tibble$possibleSpeaker == TRUE, 1, 0)
    
    pdf_document_tibble <- pdf_document_tibble %>%
      mutate(speakerGroups = cumsum(possibleSpeaker))
    
    # Thanks to Mark Needham for this: https://markhneedham.com/blog/2015/06/27/r-dplyr-squashing-multiple-rows-per-group-into-one/
    pdf_document_tibble <- pdf_document_tibble %>%
      group_by(speakerGroups) %>%
      summarise(text = paste(text, collapse = " "),
                pageNumbers = min(pageNumbers),
                title = first(title))
    
    #Deal with hyphens that were across lines
    pdf_document_tibble$text <- str_replace_all(pdf_document_tibble$text, "- ", "")
    
    # Fix spelling, again
    pdf_document_tibble$text <- str_replace_all(pdf_document_tibble$text, corrections)
    
    pdf_document_tibble <- pdf_document_tibble %>% 
      mutate(text = str_squish(text),
             text = str_replace_all(text, "(?<=[:upper:][:lower:]{1,7})[:space:](?=[^a:lower:][:space:])", ""))

    # Split out speakers
    pdf_document_tibble <- pdf_document_tibble %>%
      separate(text, c("speaker", "theText"), sep = "—|\\):|-", extra = "merge") %>% 
      mutate(speaker = str_squish(speaker),
             speaker = str_to_title(speaker))
    
    write_csv(pdf_document_tibble, "testing.csv") # Use this while testing if you want to see what it's looking like

    # Save file
    # write_csv(doc_tibble, "name_of_output_csv_file")
    write_csv(pdf_document_tibble, name_of_output_csv_file)
    
    print(paste0("Done with ", name_of_output_csv_file, " at ", Sys.time()))
  }


#### Walk through the lists and parse the PDFs ####
tic("Normal walk2")
walk2(file_names, save_names, ~ get_text_from_PDFs(.x, .y))
toc()

tic("Furrr walk2")
future_walk2(file_names, save_names, ~ get_text_from_PDFs(.x, .y))
toc()

tic("Furrr walk2 progress no print")
future_walk2(file_names, save_names, ~ get_text_from_PDFs(.x, .y), .progress = TRUE)
toc()