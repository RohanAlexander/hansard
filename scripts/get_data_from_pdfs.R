# install.packages("pdftools")
library(pdftools)

text <- pdf_text("71_PV.62.pdf")
text2 <- strsplit(text, "\n")
head(text2[[1]])


library(tidyverse)
library(stringi)
library(stringr)
library(tm)

read <- readPDF(engine = c("xpdf"), control = list(text = "-layout"))
document <- Corpus(URISource("1990-12-21.pdf"), readerControl = list(reader = read))
doc <- content(document[[1]])
doc[1:600]

# Remove the top content - come back here to get the data that's important
joint_house_row <- grep("JOINT HOUSE", doc)[1]
doc <- doc[(joint_house_row): length(doc)]
head(doc)
doc[1:100]

# For some reason the encoding the latin1, but it's easier to work with UTF-8
doc <- iconv(doc, from = "latin1", to = "UTF-8")

# Fix some errors
# doc <- str_replace(doc, "^ ", "")
doc <- str_replace_all(doc, "·", "-")


# nchar(doc[85]) # Max length of a line seems to be 44.

# Remove header and footer
# TBD

#  Deal with the two columns

# doc <- str_trim(doc, side = c("left"))

page_breaks <- grep("\\f", doc)

# doc_split <- strsplit(doc, "\\s{3,}")
# doc_split <- strsplit(doc, "\\s{2,}\\S")
doc_split <- strsplit(doc, "\\.{48}")

# strsplit("asfef", "e")
doc[1:20]
doc_split[1:20]

####
doc_tibble <- tibble(doc_split)
names(doc_tibble) <- c("text")
head(doc_tibble)
doc_tibble <- doc_tibble %>% 
  str_split(text, pattern = "\\s{3,}")
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