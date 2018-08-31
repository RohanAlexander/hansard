# !diagnostics off
#### Preamble ####
# Purpose: This file takes Australian Hansard PDF files and it converts them to tidied text data that can be analysed.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 30 August 2018
# Prerequisites: You need to have downloaded the PDFs from the parliament's website, e.g. get_80s_and_90s_PDFs.R. There are many GBs of PDFs and they are saved on an external drive - have fun finding that future-Rohan. For testing purposes there should be some in the /data folder.
# To do:
# - Talk to Matt
# - Identify and remove front matter
# - Identify and remove headers/footers
# - Deal with two columns better
# - Identify speakers


#### Set up workspace ####
library(stringi)
library(stringr)
library(tidyverse)
library(tm)
# update.packages()

# Get the spell checker
load("outputs/corrections.RData")


#### Read in PDFs ####
read <-
  readPDF(engine = c("xpdf")) # Despite the name this affects the options for how to read PDFs, it doesn't actually read the PDFs. -layout asks it to maintain the layout as best as possible.
# Handy for background: https://data.library.virginia.edu/reading-pdf-files-into-r-for-text-mining/
read <-
  readPDF(engine = c("xpdf"), control = list(info = "-f")) # I don't know why, but some of the more recent ones don't work with the above, but do work with this one
# An alternative is here that tries to maintain the layout:
# read <- readPDF(engine = c("xpdf"), control = list(text = "-layout"))
# Engine options: c("pdftools", "xpdf", "Rpoppler", "ghostscript", "Rcampdf", "custom")
# If you use pdftools then you need to split the lines - there's some code below in a playground to get started
# Handy example of UN speeches: https://medium.com/@CharlesBordet/how-to-extract-and-clean-data-from-pdf-files-in-r-da11964e252e
# The issue with this is that the PDFs are two column - the version without trying to maintain layout just seems to put the second column under the first one, which is what we'd want.


#### Create lists of PDFs to read and file names to save text as ####
# Get list of Hansard PDF filenames
# Change the path as required:
# use_this_path_to_get_pdfs  <- "data/for_testing_hansard_pdf"
use_this_path_to_get_pdfs  <- "/Volumes/SanDisk/hansard_pdfs"

# use_this_path_to_save_csv_files  <- "outputs/hansard/hansard_csv_files"
use_this_path_to_save_csv_files  <- "/Volumes/SanDisk/hansard_csv"

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
    # Read in the document, based on the filename list
    document <-
      Corpus(URISource(name_of_input_PDF_file),
             readerControl = list(reader = read))
    # document <-
    #   Corpus(
    #     URISource("data/for_testing_hansard_pdf/1996-04-30.pdf"),
    #     readerControl = list(reader = read)
    #   ) # for testing
    doc <- content(document[[1]]) # Unsure why this has to be done
    rm(document)
    
    # For some reason the encoding is latin1 - maybe an option in readPDF(?), but it's easier to work with UTF-8 - god help me how long it took for me to work this out
    doc <- iconv(doc, from = "latin1", to = "UTF-8")
    # doc[400:800]
    
    # Convert to tibble
    doc_tibble <- tibble(doc)
    names(doc_tibble) <- c("text")
    
    # Fix some minor issues
    doc_tibble$text <-
      str_replace_all(doc_tibble$text, "·", "-") # This seems to be coming about because of an issue with the PDF reader - for some reason hyphens are being read in as dots
    doc_tibble$text <-
      str_replace_all(doc_tibble$text, "\\b- \\b", "") # Hyphens are being retained improperly e.g. Roh- an and that would affect the words analysis so needs to be fixed
    
    # Identify headers and remove them
    doc_tibble <- doc_tibble %>%
      mutate(headerRow = str_detect(text, "^\\f"),
             headerRow5Below = lag(headerRow, 5)) %>%
      mutate(
        headerRow = ifelse(headerRow == TRUE, "Remove", NA),
        headerRow = ifelse(is.na(headerRow) &
                             headerRow5Below == TRUE, "Keep", headerRow)
      ) %>%
      fill(headerRow) %>%
      filter(headerRow == "Keep") %>%
      select(-headerRow, -headerRow5Below)
    
    # Identify front matter and remove it
    doc_tibble <- doc_tibble %>%
      mutate(firstSpeakerRow = str_detect(text, "SPEAKER"))
    doc_tibble$firstSpeakerRow[doc_tibble$firstSpeakerRow == FALSE] <-
      NA
    doc_tibble <- doc_tibble %>%
      fill(firstSpeakerRow) %>%
      filter(firstSpeakerRow == TRUE) %>%
      select(-firstSpeakerRow)
    
    # Identify speakers - THIS CAN BE IMPROVED e.g. it doesn't look within a string.
    # This is way we identify them - amend as appropriate:
    find_based_on_this <-
      c(
        "Mr [:upper:]{3,}",
        "The Clerk-",
        "Honourable members interjecting-",
        "Mr Leo McLeay-",
        "^[:^lower:]{3,}$"
      )
    find_based_on_this <- paste(find_based_on_this, collapse = "|")
    
    doc_tibble <- doc_tibble %>%
      mutate(speakerName = str_detect(text, find_based_on_this))
    doc_tibble$speakerName[doc_tibble$speakerName == FALSE] <- 0
    doc_tibble$speakerName[doc_tibble$speakerName == TRUE] <- 1
    doc_tibble <- doc_tibble %>%
      mutate(speakerGroups = cumsum(speakerName))
    # Thanks to Mark Needham for this: https://markhneedham.com/blog/2015/06/27/r-dplyr-squashing-multiple-rows-per-group-into-one/
    doc_tibble <- doc_tibble %>%
      group_by(speakerGroups) %>%
      summarise(text_spoken = paste(text, collapse = " "))
    
    
    # Combine all text into one bundle - WHY?
    # just_the_text <- c(doc_tibble$text)
    
    # Fix spelling
    doc_tibble$text_spoken <-
      str_replace_all(doc_tibble$text_spoken, corrections)
    
    # Save file
    # write_csv(doc_tibble, "name_of_output_csv_file")
    write_csv(doc_tibble, name_of_output_csv_file)
    
    print(paste0("Done with ", name_of_output_csv_file, " at ", Sys.time()))
    
  }


#### Walk through the lists and parse the PDFs ####
walk2(file_names, save_names, ~ get_text_from_PDFs(.x, .y))




#### Alternative ####
# COME BACK HERE AND MAKE THE PARSER BETTER BUT JOHN IS ON THE WAR PATH AND THERE'S NO TIME
# install.packages('textreadr')
# library(textreadr)
# library(tidyverse)
#
# pdf_doc <- ?system.file("data/some_hansard_pdfs/1981-02-24.pdf", package = "textreadr")
#
# test <- read_pdf("data/some_hansard_pdfs/1981-02-24.pdf")





### TESTING DEBRIS


read <-
  readPDF(engine = c("xpdf"),
          control = list(info = "-f", text = "-layout"))

document <-
  Corpus(
    URISource("/Volumes/SanDisk/hansard_pdfs/2011-05-11.pdf"),
    readerControl = list(reader = read)
  )
doc <- content(document[[1]]) # Unsure why this has to be done
rm(document)

# For some reason the encoding is latin1 - maybe an option in readPDF(?), but it's easier to work with UTF-8 - god help me how long it took for me to work this out
doc <- iconv(doc, from = "latin1", to = "UTF-8")

# Convert to tibble
doc_tibble <- tibble(doc)
names(doc_tibble) <- c("text")

doc_tibble$text <-
  str_replace_all(doc_tibble$text, "·", "-") # This seems to be coming about because of an issue with the PDF reader - for some reason hyphens are being read in as dots
doc_tibble$text <-
  str_replace_all(doc_tibble$text, "\\b- \\b", "") # Hyphens are being retained improperly e.g. Roh- an and that would affect the words analysis so needs to be fixed

# Identify headers - should remove them?
doc_tibble <- doc_tibble %>%
  mutate(headerRow = if_else(grepl("^\\f", doc_tibble$text), 1, 0)) # \f is used by the PDF parser to signal page breaks

# Identify speakers - TBD


# Combine all text into one bundle
just_the_text <- c(doc_tibble$text)

# Fix spelling
just_the_text <- str_replace_all(just_the_text, corrections)
