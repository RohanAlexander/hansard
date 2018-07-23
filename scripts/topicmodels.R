#### Preamble ####
# Purpose: ...
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au 
# Date: 13 September 2017
# Prerequisites: Uses all_hansard_titles_and_dates.Rda which is an output of get_list_of_titles.R
# Issues: 


#### Workspace set-up ####
#install.packages('cleanNLP')
#install.packages('reticulate')
#library(cleanNLP)

http://tidytextmining.com/topicmodeling.html

#install.packages('topicmodels')
library(topicmodels)
#install.packages('tidytext')
library(tidytext)


load("outputs/all_hansard_titles_and_dates.Rda")

chapters_dtm <- word_counts %>%
  cast_dtm(document, word, n)

