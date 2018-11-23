# !diagnostics off
#### Preamble ####
# Purpose: This file takes Australian Hansard PDF files and it converts them to CSVs of text data that can be analysed.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 16 October 2018
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
library(stringi)
library(tidyverse)
library(tictoc)
library(tm)
# update.packages()
# Set up furrr
plan(multiprocess)


#### Create lists of PDFs to read and file names to save text as ####
# Change the path as required:
# use_this_path_to_get_pdfs  <- "/Volumes/Hansard/pdfs/federal/hor"
use_this_path_to_get_pdfs  <- "inputs/for_testing_hansard_pdf"

# use_this_path_to_save_csv_files  <- "/Volumes/Hansard/parsed/federal/hor"
use_this_path_to_save_csv_files  <- "outputs/hansard/temp"

# Get list of Hansard PDF filenames
file_names <-
  list.files(
    path = use_this_path_to_get_pdfs,
    pattern = "*.pdf",
    recursive = TRUE,
    full.names = TRUE
  )
file_names <- file_names %>% sample() # Randomise the order
# file_names <- file_names %>% sample(500) # Get 500

# Use this to get and remove the ones already done
# file_names_already_done <-
#   list.files(
#     path = use_this_path_to_save_csv_files,
#     pattern = "*.csv",
#     recursive = TRUE,
#     full.names = FALSE
#   ) %>%
#   str_replace(".csv", ".pdf")
# file_names_already_done <-  paste0(use_this_path_to_get_pdfs, "/", file_names_already_done)
# file_names <- file_names[!(file_names %in% file_names_already_done)]
# rm(file_names_already_done)

save_names <- file_names %>%
  str_replace(use_this_path_to_get_pdfs, "") %>%
  str_replace(".pdf", ".csv")

save_names <- paste0(use_this_path_to_save_csv_files, save_names)


#### Create the function that will be applied to the files ####
get_text_from_PDFs <-
  function(name_of_input_PDF_file,
           name_of_output_csv_file) {
    # Read in the document, based on the filename list
    # name_of_input_PDF_file <- "/Volumes/Backup/hansard_pdfs/1953-10-21.pdf"
    # name_of_input_PDF_file <- "inputs/for_testing_hansard_pdf/1971-10-05.pdf" # uncomment for testing
    # name_of_input_PDF_file <- "inputs/for_testing_senate_pdf/1901-06-06.pdf" # uncomment for testing
    pdf_document <- pdf_text(name_of_input_PDF_file)
    
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
      str_replace_all(
        pdf_document_tibble$text,
        "(?<=[:space:][:upper:])[:space:](?=[:upper:][:space:])",
        ""
      ) # This one is picking up annoying spaces e.g. R O H A N should be Rohan. The regular expression is a bit wild, but each bit in round brackets is looking either side of the space and then removing that space as appropriate - see 'Look Arounds' in the stringr cheatsheet. There's a function - kerning - in the textclean package which does a similar task, but it seems to have an error that binds it to the next word if that's capitalised. Thanks Monica and also https://stackoverflow.com/questions/31280327/remove-extra-white-space-from-between-letters-in-r-using-gsub
    # These next ones are just to make search and replacements work more consistently
    pdf_document_tibble$text <-
      str_replace_all(pdf_document_tibble$text, "-[:space:]{2}", "- ") # A dash followed by two spaces should be changed to a dash followed by one space
    pdf_document_tibble$text <-
      str_replace_all(pdf_document_tibble$text, "\\.[:space:]*-", " -")
    pdf_document_tibble$text <-
      str_replace_all(pdf_document_tibble$text, "~", "-")
    
    # Fix some other issues that just make everything a bit easier
    pdf_document_tibble$text <-
      str_replace_all(
        pdf_document_tibble$text,
        "M[:space:]?r[:punct:]+[:space:]|Mr[:space:]*[:punct:]+",
        "Mr "
      )
    # str_detect("Mr . THOMAS ", "M[:space:]?r[:punct:]+[:space:]|Mr[:space:]*[:punct:]+")
    pdf_document_tibble$text <-
      str_replace_all(pdf_document_tibble$text,
                      "M[:space:]?r[:space:]*[:punct:]*[:space:]",
                      "Mr ")
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
      str_replace_all(pdf_document_tibble$text,
                      "WILLIAMM c MILLAN",
                      "WILLIAM MCMILLAN")
    pdf_document_tibble$text <-
      str_replace_all(pdf_document_tibble$text, "Mc(?=[:upper:]+)", "MC")
    pdf_document_tibble$text <-
      str_replace_all(pdf_document_tibble$text, "(?<=\\S)Mr", " Mr")
    
    # Fix related to the Speaker taking the chair
    pdf_document_tibble$text <-
      str_replace_all(pdf_document_tibble$text, "ohair|ehair", "chair")
    pdf_document_tibble$text <-
      str_replace_all(pdf_document_tibble$text, "SruAKEK|Sr-EAKEB|SI-KAXEE|SFJSAKEK|SPEAKBB|SI'BAKBR|SPBAKSK|SPKAKKR", "SPEAKER")
    pdf_document_tibble$text <-
      str_replace_all(pdf_document_tibble$text, "SPKAKKB|SPJSAKEB|SPKAKKB|SPJSAKEB|SrEAKKK|SFBAKBK", "SPEAKER")
    pdf_document_tibble$text <-
      str_replace_all(pdf_document_tibble$text, "SPSAKSB|SPEAKKKSi'RaKKR|SPBAKBR|SI'EAKBB|SPKAXBB", "SPEAKER")
    pdf_document_tibble$text <-
      str_replace_all(pdf_document_tibble$text, "SrEAKBii|SPEAKEK|SPBAKES|SI'KAKER|SPBAKKB|SI'HAKER", "SPEAKER")
    pdf_document_tibble$text <-
      str_replace_all(pdf_document_tibble$text, "SPEAXJDB|SPEAKEE|SVJSAKEK|SrBAKER|SriiAKKic|SPBAXBR", "SPEAKER")
    pdf_document_tibble$text <-
      str_replace_all(pdf_document_tibble$text, "SPEAXXX|SPEAXEE|SJ'JSAKJSK|SPKAKXI!|SI'EAKEH|SPBAKEE", "SPEAKER")
    pdf_document_tibble$text <-
      str_replace_all(pdf_document_tibble$text, "Si-EAKEit|SPBAKEK|SFJSAKKJ|SPHAKEE|SPKAKEE|SPEAKBE", "SPEAKER")
    pdf_document_tibble$text <-
      str_replace_all(pdf_document_tibble$text, "Mr DEPDTT SPKAKEB", "Mr DEPUTY SPEAKER")
    pdf_document_tibble$text <-
      str_replace_all(pdf_document_tibble$text, "SPXAKKB|SPEAKEB|SJ-BAKJSB|SFEAKEB.", "SPEAKER")
    pdf_document_tibble$text <-
      str_replace_all(pdf_document_tibble$text, "Mx: SI-BAKER", "MR SPEAKER")
    pdf_document_tibble$text <-
      str_replace_all(pdf_document_tibble$text, "took the. chair", "took the chair")
    pdf_document_tibble$text <-
      str_replace_all(pdf_document_tibble$text, "took tho chair", "took the chair")
    pdf_document_tibble$text <-
      str_replace_all(pdf_document_tibble$text, "took tlie chair", "took the chair")
    pdf_document_tibble$text <-
      str_replace_all(pdf_document_tibble$text, "took tiie chair", "took the chair")
    pdf_document_tibble$text <-
      str_replace_all(pdf_document_tibble$text, "took 'the chair|took the nhair", "took the chair")
    
    pdf_document_tibble$text <-
      str_replace_all(pdf_document_tibble$text, "PBESIEENT|FREBIDKKT|PBUSIOTNT|PBEaroBNT", "PRESIDENT")
    

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
      select(-lineNumber,-lastLine)
    
    
    ## Identify front matter and remove it
    # Primarily identify the start of talking based on the first occurence of 'Mr SPEAKER'. It seems pretty common, but there are some misses (e.g. 1991-01-22). As a backup look for first occurence of "JOINT HOUSE" and then pick the next page. And look for 'took the chair' and pick that page.
    pdf_document_tibble <- pdf_document_tibble %>%
      mutate(
        firstSpeakerRow = str_detect(text, "SPEAKER"),
        firstPresidentRow = str_detect(text, "PRESIDENT"),
        firstJointHouseRow = str_detect(text, "JOINT HOUSE"),
        firstTookTheChairRow = str_detect(text, "took the chair"),
        firstTheChairAtRow = str_detect(text, "the chair at")
      )
    pdf_document_tibble$firstSpeakerRow[pdf_document_tibble$firstSpeakerRow == FALSE] <-
      NA
    pdf_document_tibble$firstPresidentRow[pdf_document_tibble$firstPresidentRow == FALSE] <-
      NA
    pdf_document_tibble$firstJointHouseRow[pdf_document_tibble$firstJointHouseRow == FALSE] <-
      NA
    pdf_document_tibble$firstTookTheChairRow[pdf_document_tibble$firstTookTheChairRow == FALSE] <-
      NA
    pdf_document_tibble$firstTheChairAtRow[pdf_document_tibble$firstTheChairAtRow == FALSE] <-
      NA
    
    # Get the row and corresponding page and then filter to only pages from that page
    row_of_first_SPEAKER <-
      pdf_document_tibble$firstSpeakerRow[pdf_document_tibble$firstSpeakerRow == TRUE] %>% which() %>% first()
    row_of_first_PRESIDENT <-
      pdf_document_tibble$firstPresidentRow[pdf_document_tibble$firstPresidentRow == TRUE] %>% which() %>% first()
    row_of_first_JOINTHOUSE <-
      pdf_document_tibble$firstJointHouseRow[pdf_document_tibble$firstJointHouseRow == TRUE] %>% which() %>% first()
    row_of_first_TookTheChair <-
      pdf_document_tibble$firstTookTheChairRow[pdf_document_tibble$firstTookTheChairRow == TRUE] %>% which() %>% first()
    row_of_first_TheChairAt <-
      pdf_document_tibble$firstTheChairAtRow[pdf_document_tibble$firstTheChairAtRow == TRUE] %>% which() %>% first()
    
    first_page_of_interest_SPEAKER <-
      pdf_document_tibble[row_of_first_SPEAKER, "pageNumbers"] %>% as.integer()
    first_page_of_interest_PRESIDENT <-
      pdf_document_tibble[row_of_first_PRESIDENT, "pageNumbers"] %>% as.integer()
    first_page_of_interest_JOINTHOUSE <-
      pdf_document_tibble[row_of_first_JOINTHOUSE, "pageNumbers"] %>% as.integer()
    first_page_of_interest_TookTheChair <-
      pdf_document_tibble[row_of_first_TookTheChair, "pageNumbers"] %>% as.integer()
    first_page_of_interest_TheChairAt <-
      pdf_document_tibble[row_of_first_TheChairAt, "pageNumbers"] %>% as.integer()
    
    first_page_of_interest_JOINTHOUSE <-
      (first_page_of_interest_JOINTHOUSE + 1)  %>% as.integer()
    
    filter_from_here <-
      case_when(
        !is.na(first_page_of_interest_TookTheChair) ~ first_page_of_interest_TookTheChair,
        !is.na(first_page_of_interest_TheChairAt) ~ first_page_of_interest_TheChairAt,
        !is.na(first_page_of_interest_SPEAKER) ~ first_page_of_interest_SPEAKER,
        !is.na(first_page_of_interest_PRESIDENT) ~ first_page_of_interest_PRESIDENT,
        TRUE ~ first_page_of_interest_JOINTHOUSE
      )


    pdf_document_tibble <- pdf_document_tibble %>%
      filter(pageNumbers >= filter_from_here) %>%
      select(-firstSpeakerRow,
             -firstPresidentRow,
             -firstJointHouseRow,
             -firstTookTheChairRow,
             -firstTheChairAtRow)
    
    rm(
      row_of_first_SPEAKER,
      row_of_first_TookTheChair,
      row_of_first_PRESIDENT,
      row_of_first_JOINTHOUSE,
      row_of_first_TheChairAt,
      first_page_of_interest_SPEAKER,
      first_page_of_interest_PRESIDENT,
      first_page_of_interest_JOINTHOUSE,
      first_page_of_interest_TookTheChair,
      first_page_of_interest_TheChairAt,
      filter_from_here
    )
    
    
    # Save file
    write_csv(pdf_document_tibble, name_of_output_csv_file)
    
    print(paste0("Done with ", name_of_output_csv_file, " at ", Sys.time()))
  }


#### Walk through the lists and parse the PDFs ####
# tic("Normal walk2")
# walk2(file_names, save_names, ~ get_text_from_PDFs(.x, .y))
# toc()

safely_get_text_from_PDFs <- safely(get_text_from_PDFs)

# file_names <- file_names[1:10]
# save_names <- save_names[1:10]
# file_names <- file_names[1:(length(file_names)/2)]
# save_names <- save_names[1:(length(save_names)/2)]


tic("Furrr walk2 stringr")
future_walk2(file_names,
             save_names,
             ~ safely_get_text_from_PDFs(.x, .y),
             .progress = TRUE)
toc()

