#### Preamble ####
# Purpose: This file takes Australian Hansard PDF files and it converts them to tidied text data that can be analysed.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 20 August 2018
# Prerequisites: You need to have downloaded the PDFs from the parliament's website  - get_80s_and_90s_PDFs.R. There are many GBs of PDFs and they are saved on an external drive - have fun finding that future-Rohan.
# To do:


#### To delete start ####
# install.packages("pdftools")
# library(pdftools)
# 
# text <- pdf_text("71_PV.62.pdf")
# text2 <- strsplit(text, "\n")
# head(text2[[1]])
#### To delete end ####


#### Set up workspace ####
library(stringi)
library(stringr)
library(tidyverse)
library(tm)
# update.packages()

read <- readPDF(engine = c("xpdf"), control = list(text = "-layout")) # Despite the name this affects the options for how to read PDFs, it doesn't actually read the PDFs


#### Read in data ####
# Will need to come back here to update ahead of the main read through
document <- Corpus(URISource("scripts/1990-12-21.pdf"), readerControl = list(reader = read))
doc <- content(document[[1]]) # Unsure why this has to be done
# doc[1:600]

# For some reason the encoding is latin1 - maybe an option in readPDF(?), but it's easier to work with UTF-8
doc <- iconv(doc, from = "latin1", to = "UTF-8")

# Remove the top content - come back here to get the data that's important
joint_house_row <- grep("JOINT HOUSE", doc)[1]
doc <- doc[(joint_house_row): length(doc)]
head(doc)
doc[1:100]

# Fix some errors
# doc <- str_replace(doc, "^ ", "")
# doc <- str_trim(doc, side = c("left")) # Don't do this - some of the line have a whole bunch of spacing at the front because they are in teh second column and this removes that.
doc <- str_replace_all(doc, "·", "-")

# nchar(doc[85]) # Max length of a line seems to be 48. Could use this to do the split?
# doc_split <- strsplit(doc, "\\.{48}")

# Remove header
# header_rows <- grep("^\\f", doc) # Remember: \f are for page breaks
# doc[header_rows] <- "page" # I put a marker here that will be useful later
# doc <- doc[- (header_rows - 1)]

# Deal with the two columns
page_breaks <- grep("\\f", doc) # Need to know where the page breaks are to know where to put the right column

# doc_split <- strsplit(doc, "\\s{3,}")
# doc_split <- strsplit(doc, "\\s{2,}\\S")


# strsplit("asfef", "e")
doc[1:20]
doc_split[1:20]

####
doc_tibble <- tibble(doc)
names(doc_tibble) <- c("text")
head(doc_tibble)
doc_tibble[1:50, ]

nchar(doc_tibble$text[10])

doc_tibble <- doc_tibble %>%
  mutate(line_number = 1:nrow(doc_tibble)) %>%
  mutate(
    line_type = case_when(
      line_number %in% page_breaks ~ "pageBreak",
      str_detect(text, "^\\s{48,}") == TRUE ~ "secondColumnOnly",
      nchar(text) < 48 ~ "firstColumnOnly",
      TRUE ~ "both"
    )
  ) %>%
  select(-line_number)

doc_tibble <- doc_tibble %>%
  mutate(text = if_else(line_type %in% c("both", "firstColumnOnly"), str_trim(text, side = c("left")), text))

doc_tibble <- doc_tibble %>%
  mutate(line_type = if_else(text == "", "lineBreak", line_type))

doc_tibble <-
  doc_tibble[3:nrow(doc_tibble), ] # Just get rid of that top bit for now - work out how to do this properly

doc_tibble <- doc_tibble %>%
  mutate(
    split_string = case_when(
      line_type == "secondColumnOnly" ~ str_split(text, pattern = "^\\s{48,}"),
      line_type == "both" ~ str_split(text, pattern = "\\s{2,}"),
      line_type == "firstColumnOnly" ~ str_split(text, pattern = "\\s{2,}")
    )
  )


unlist(c("Friday, 21 December 1990", "Second Reading"))
unlist("asdfasd")

doc_tibble <- doc_tibble %>%
  separate()

head(doc_tibble)
class(doc_tibble$split_string[[1]])

doc_tibble <- doc_tibble %>%
  mutate_if(line_type = "secondColumnOnly", 


str_detect("                                                  where an acquisition was made by an-", "^\\s{48,}")

doc_tibble <- doc_tibble %>% 
  str_split(text, pattern = "\\s{2,}")
####

# https://stackoverflow.com/questions/42541849/extract-text-from-two-column-pdf-with-r
# https://medium.com/@CharlesBordet/how-to-extract-and-clean-data-from-pdf-files-in-r-da11964e252e
doc_split <- lapply(doc_split, function(x) {
  # For each element, extract:
  #    - doc1 that is the first column. 
  #    - doc2 that is the second column.
  doc1 <- x[1:8][x[1:8] != ""][1] # The first piece of text that's not empty
  if (is.na(doc1)) doc1 <- ""
  # doc2 takes the next non-empty piece of text
  doc2 <- x[x != ""] 
  if (doc1 != "") doc2 <- doc2[-1]
  if (length(doc2) == 0) doc2 <- ""
  # Sometimes there is more text needed to be extracted. 
  # I try to give it to either doc1 or doc2 depending on the size of it.
  # while (sum(nchar(doc2)) > 65) {
  #   doc1 <- paste(doc1, doc2[1], collapse = " ")
  #   doc2 <- doc2[-1]
  # }
  # Clean it before returning it
  doc2 <- paste(doc2, collapse = " ")
  doc1 <- str_trim(doc1) # stringr::str_trim trim the spaces before/after
  doc2 <- str_trim(doc2)
  list(doc1 = doc1, doc2 = doc2)
})
doc1 <- sapply(doc_split, `[[`, 1) # First column
doc2 <- sapply(doc_split, `[[`, 2) # Second column

# Vector of the page breaks coordinates:
pages_rows <- c(0, which(doc1 == "page"), length(doc1))
doc <- c()
# Page by page, we fill a new vector:
for (i in 1:(length(pages_rows) - 1)) {
  doc <- c(doc, c(doc1[(pages_rows[i] + 1):pages_rows[i + 1]],
                  doc2[(pages_rows[i] + 1):pages_rows[i + 1]]))
}
doc <- doc[doc != "page"]

doc[1:40]