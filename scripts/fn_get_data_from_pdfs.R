#### Preamble ####
# Purpose: This file takes Australian Hansard PDF files and it converts them to tidied text data that can be analysed.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 26 August 2018
# Prerequisites: You need to have downloaded the PDFs from the parliament's website - get_80s_and_90s_PDFs.R. There are many GBs of PDFs and they are saved on an external drive - have fun finding that future-Rohan.
# To do:


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
# An alternative is here that tries to maintain the layout:
# read <- readPDF(engine = c("xpdf"), control = list(text = "-layout"))
# Engine options: c("pdftools", "xpdf", "Rpoppler", "ghostscript", "Rcampdf", "custom")
# If you use pdftools then you need to split the lines - there's some code below in a playground to get started
# Handy example of UN speeches: https://medium.com/@CharlesBordet/how-to-extract-and-clean-data-from-pdf-files-in-r-da11964e252e
# The issue with this is that the PDFs are two column - the version without trying to maintain layout just seems to put the second column under the first one, which is what we'd want.

# Get list of Hansard PDF filenames
file_names <-
  list.files(
    path = "/Volumes/SanDisk/hansard_pdfs",
    # Change this as required
    pattern = "*.pdf",
    recursive = TRUE,
    full.names = TRUE
  )
file_names

# Make list of the text names that we'll save it all with
save_names <-
  list.files(
    path = "/Volumes/SanDisk/hansard_pdfs",
    # Change this as required
    pattern = "*.pdf",
    recursive = TRUE,
    full.names = FALSE
  ) %>%
  str_replace(".pdf", ".txt")

# save_names <- paste0("outputs/hansard_text_files/", save_names)
save_names <- paste0("/Volumes/SanDisk/hansard_txt/", save_names)

save_names <- save_names[995:length(save_names)]
file_names <- file_names[995:length(file_names)]


get_text_from_PDFs <-
  function(name_of_input_PDF_file,
           name_of_output_txt_file) {
    # Iterate over files from here
    document <-
      Corpus(URISource(name_of_input_PDF_file),
             readerControl = list(reader = read))
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
    
    # Save file
    write(just_the_text, file = name_of_output_txt_file)
    
    print(paste0("Done with ", name_of_output_txt_file, " at ", Sys.time()))
    
  }


walk2(file_names, save_names, ~get_text_from_PDFs(.x, .y))




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


read <- readPDF(engine = c("xpdf"), control = list(info = "-f", text = "-layout"))

document <-
  Corpus(URISource("/Volumes/SanDisk/hansard_pdfs/2011-05-11.pdf"),
         readerControl = list(reader = read))
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
