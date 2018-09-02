# !diagnostics off
#### Preamble ####
# Purpose: This file takes Australian Hansard PDF files and it converts them to tidied text data that can be analysed.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 1 September 2018
# Prerequisites: You need to have downloaded the PDFs from the parliament's website, e.g. get_80s_and_90s_PDFs.R. There are many GBs of PDFs and they are saved on an external drive - have fun finding that future-Rohan. For testing purposes there should be some in the /data folder.
# To do:
# - Identify and remove front matter
# - Identify and remove headers/footers
# - Deal with two columns better
# - Identify speakers


#### Set up workspace ####
# library(stringi)
# library(stringr)
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
    # Read in the document, based on the filename list
    document <-
      Corpus(URISource(name_of_input_PDF_file),
             readerControl = list(reader = read))
    # document <-
    #   Corpus(
    #     URISource("data/for_testing_hansard_pdf/1946-11-07.pdf"),
    #     readerControl = list(reader = read)
    #   ) # for testing
    doc <- content(document[[1]]) # Unsure why this has to be done
    rm(document)
    
    # For some reason the encoding is latin1 - maybe an option in readPDF(?), but it's easier to work with UTF-8 - god help me how long it took for me to work this out
    doc <- iconv(doc, from = "latin1", to = "UTF-8")
    doc[1:500]
    
    # Convert to tibble
    doc_tibble <- tibble(doc)
    names(doc_tibble) <- c("text")
    
    # Fix some minor issues
    doc_tibble$text <-
      str_replace_all(doc_tibble$text, "·", "-") # This seems to be coming about because of an issue with the PDF reader - for some reason hyphens are being read in as dots
    doc_tibble$text <-
      str_replace_all(doc_tibble$text, "\\b- \\b", "") # Hyphens are being retained improperly e.g. Roh- an and that would affect the words analysis so needs to be fixed
    doc_tibble$text <-
      str_replace_all(doc_tibble$text,
                      "(?<=[:upper:])[:space:](?=[:upper:][:space:])",
                      "")
    doc_tibble$text <-
      str_replace_all(doc_tibble$text, "--", "-- ")
    doc_tibble$text <-
      str_replace_all(doc_tibble$text, "--  ", "-- ")
    doc_tibble$text <-
      str_replace_all(doc_tibble$text, ".--", ". --")
    
    # Fix spelling
    doc_tibble$text <-
      str_replace_all(doc_tibble$text, corrections)
    
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
    
    # Identify speakers - THIS CAN BE IMPROVED e.g. does it look within a string?
    # This is the way we identify them - amend as appropriate:
    find_based_on_this <-
      c(
        "Mr [:upper:]{3,}",
        "Mr. McEWEN.",
        "The Clerk-",
        "Honourable members interjecting-",
        "Mr Leo McLeay-",
        "^[:^lower:]{3,}$",
        "Mr. [:upper:]{3,} .",
        "Mr. [:upper:]{3,}.",
        "Mr. [:upper:]{3,}[:space:][:upper:]{3,}.",
        "Dr. [:upper:]{3,}.",
        "Sir [:upper:]{3,}[:space:][:upper:]{3,}.",
        "Ms [:upper:]{3,}-",
        "Dr [:upper:]{3,}-",
        "Dr [:alpha:]{3,}-",
        "Mr [:alpha:]{3,}-"
      )
    find_based_on_this <- paste(find_based_on_this, collapse = "|")
    
    # str_replace("Government Sir JOHN FORREST. -- I desire", "Sir [:upper:]{3,}[:space:][:upper:]{3,}.", "HEH")
    
    
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
    
    # Save file
    # write_csv(doc_tibble, "name_of_output_csv_file")
    write_csv(doc_tibble, name_of_output_csv_file)
    
    print(paste0("Done with ", name_of_output_csv_file, " at ", Sys.time()))
    
  }


#### Walk through the lists and parse the PDFs ####
walk2(file_names, save_names, ~ get_text_from_PDFs(.x, .y))












#### Alternative using PDFtexttools ####
# COME BACK HERE AND MAKE THE PARSER BETTER BUT JOHN IS ON THE WAR PATH AND THERE'S NO TIME
# install.packages('textreadr')
library(pdftools)
# library(textreadr)
# install.packages('textclean')
library(textclean)
library(tidyverse)
library(tictoc)
# library(tm)

# Get the spell checker
load("outputs/corrections.RData")


#
# pdf_doc <- ?system.file("data/some_hansard_pdfs/1981-02-24.pdf", package = "textreadr")
#
# test_textreader <- read_pdf("data/for_testing_hansard_pdf/1984-02-28.pdf")
test_pdftools <-
  pdf_text("data/for_testing_hansard_pdf/1946-11-07.pdf")
# test_tm_options <- readPDF(control = list(text = "-layout"))
# test_tm <- Corpus(URISource("data/for_testing_hansard_pdf/1984-02-28.pdf"), readerControl = list(reader = test_tm_options))
# test_tm <- content(test_tm[[1]])
# head(test_tm)
# write_csv(test_textreader, "test_textreader.csv")
# write_lines(test_pdftools, "test_pdftools.csv")
# write_lines(test_tm, "test_tm.csv")

test_textreader$text[13]

test_tibble_pdftools <- tibble(text = test_pdftools)

test_tibble_pdftools$page_num <- 1:nrow(test_tibble_pdftools)

test_tibble_pdftools <-
  separate_rows(test_tibble_pdftools, text, sep = "\\n")


# UP TO HERE

test_tibble_pdftools$text_mod <- test_tibble_pdftools$text

# Fix some minor issues
test_tibble_pdftools$text_mod <-
  str_replace_all(test_tibble_pdftools$text_mod, "·", "-") # This seems to be coming about because of an issue with the PDF reader - for some reason hyphens are being read in as dots
test_tibble_pdftools$text_mod <-
  str_replace_all(test_tibble_pdftools$text_mod, "•", "-") # This seems to be coming about because of an issue with the PDF reader - for some reason hyphens are being read in as dots
test_tibble_pdftools$text_mod <-
  str_replace_all(test_tibble_pdftools$text_mod, "\\b- \\b", "") # Hyphens are being retained improperly e.g. Roh- an and that would affect the words analysis so needs to be fixed

# test_tibble_pdftools$text_mod <- replace_kern(test_tibble_pdftools$text_mod)
test_tibble_pdftools$text_mod <-
  str_replace_all(
    test_tibble_pdftools$text_mod,
    "(?<=[:space:][:upper:])[:space:](?=[:upper:][:space:])",
    ""
  )

test_tibble_pdftools$text_mod <-
  str_replace_all(test_tibble_pdftools$text_mod, "--", "-- ")
test_tibble_pdftools$text_mod <-
  str_replace_all(test_tibble_pdftools$text_mod, "--  ", "-- ")
test_tibble_pdftools$text_mod <-
  str_replace_all(test_tibble_pdftools$text_mod, ".--", ". --")

# Fix spelling
tic('Not fixed')
test_tibble_pdftools$text_mod <-
  str_replace_all(test_tibble_pdftools$text_mod, corrections)
toc()


# Identify headers and remove them
test_tibble_pdftools <- test_tibble_pdftools %>%
  group_by(page_num) %>%
  mutate(lineNumber = 1:n()) %>%
  mutate(lastLine = n()) %>%
  ungroup()

test_tibble_pdftools <- test_tibble_pdftools %>%
  filter(lineNumber != 1) %>%
  select(-lineNumber)

# # Identify footers and remove them
# test_tibble_pdftools <- test_tibble_pdftools %>%
#   group_by(page_num) %>%
#   mutate(lastLine = n()) %>%
#   ungroup()
#
# test_tibble_pdftools <- test_tibble_pdftools %>%
#   filter(lineNumber != 1) %>%
#   select(-lineNumber)


# Identify front matter and remove it
test_tibble_pdftools <- test_tibble_pdftools %>%
  mutate(firstSpeakerRow = str_detect(text_mod, "SPEAKER"))
test_tibble_pdftools$firstSpeakerRow[test_tibble_pdftools$firstSpeakerRow == FALSE] <-
  NA

rowOfFirstSPEAKER <-
  test_tibble_pdftools$firstSpeakerRow[test_tibble_pdftools$firstSpeakerRow == TRUE] %>% which() %>% first()
firstPageOfInterest <-
  test_tibble_pdftools[rowOfFirstSPEAKER, "page_num"] %>% as.integer()

test_tibble_pdftools <- test_tibble_pdftools %>%
  filter(page_num >= firstPageOfInterest) %>%
  select(-firstSpeakerRow)

test_tibble_pdftools[1:100,]

test_tibble_pdftools <- test_tibble_pdftools %>%
  select(-text)


test_tibble_pdftools <- test_tibble_pdftools %>%
  mutate(line_number = 1:nrow(test_tibble_pdftools)) %>%
  mutate(
    line_type = case_when(
      str_detect(text_mod, "^\\s{40,}") == TRUE ~ "secondColumnOnly",
      nchar(text_mod) < 48 ~ "firstColumnOnly",
      TRUE ~ "both"
    )
  )

write_csv(test_tibble_pdftools, "TEST.csv")

test_tibble_pdftools <- test_tibble_pdftools %>%
  mutate(text_mod = if_else(
    line_type %in% c("both", "firstColumnOnly"),
    str_trim(text_mod, side = c("left")),
    text_mod
  ))


# doc_tibble <- doc_tibble %>%
#   mutate(line_type = if_else(text == "", "lineBreak", line_type))

# Right, let's try this - don't @ me - it should work well enough and my supervisor is breathing down my neck
# Come back here - there are issues with the parsing here that are affecting the specifics of the statements. It's fit for purpose, but not ideal. Should probably pre-process the text e.g. if letter then two spaces then letter in the first 40 spots then probably change to letter one space letter. Things like that.

test_tibble_pdftools <-
  test_tibble_pdftools %>% mutate(ordering = 1:nrow(test_tibble_pdftools))


# Sunday - come back and tune this to work a little better - throwing away a lot - extra = "merge" - hides them so get rid of that then come back here.
test_tibble_pdftools_both <- test_tibble_pdftools %>%
  filter(line_type == "both") %>%
  separate(
    text_mod,
    c("first_column", "second_column"),
    sep = "[:space:]{2,}",
    remove = FALSE,
    extra = "merge"
  )

test_tibble_pdftools_secondColumnOnly <-
  test_tibble_pdftools %>% filter(line_type ==
                                    "secondColumnOnly") %>% mutate(first_column = NA, second_column = text_mod)

test_tibble_pdftools_firstColumnOnly <-
  test_tibble_pdftools %>% filter(line_type ==
                                    "firstColumnOnly") %>% mutate(first_column = text_mod, second_column = NA)


test_tibble_pdftools <-
  bind_rows(
    test_tibble_pdftools_both,
    test_tibble_pdftools_secondColumnOnly,
    test_tibble_pdftools_firstColumnOnly
  ) %>%
  arrange(ordering)

rm(
  test_tibble_pdftools_both,
  test_tibble_pdftools_secondColumnOnly,
  test_tibble_pdftools_firstColumnOnly
)

test_tibble_pdftools_test <-
  test_tibble_pdftools %>%
  select(-c(text_mod, lastLine, line_type, ordering, line_number)) %>%
  gather(position, text_in_position,-c(page_num)) %>%
  mutate(counter = 1:n()) %>%
  arrange(page_num, counter)


names(test_tibble_pdftools)
