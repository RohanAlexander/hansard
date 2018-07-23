#### Preamble ####
# Purpose: Associate a group, based on similar topics, for every statement in Hansard.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 13 June 2018
# Prerequisites: Uses all_hansard_titles_and_dates.Rda which is an output of get_list_of_titles.R
# Issues:


#### Workspace set-up ####
# Load libraries
library(dplyr)
library(tidytext)
library(tidyverse)
library(tm)
library(topicmodels)

# Load data
load("outputs/all_hansard_text.Rda") # Takes a while

# Bind custom list of stopwords to the default list
custom_stop_words <- bind_rows(
  stop_words, # The default list
  data_frame(
    word = c(
      "1", "2", "act", "amendment", "amount", "australia", "australian", "bill", "board", "cent", "clause", "commission", "committee", "commonwealth", "countries", "country", "day", "deal", "debate", "department", "desire", "duty", "gentleman", "government", "honorable", "honourable", "house", "increase", "labor", "labour", "leader", "legislation", "matter", "minister", "money", "national", "opposition", "parliament", "party", "people", "policy", "position", "power", "prime", "proposed", "public", "question", "regard", "report", "service", "situation", "south", "speaker", "statement", "support", "system", "time", "united", "vote", "wales"
    ),
    lexicon = rep("custom", length(word))
  )
)


#### Create a working sample of data ####
# Get a sample of 100,000 statements
set.seed(123)
some_random_rows <- sample(1:nrow(all_hansard_text), 100000)
some_hansard_text <- all_hansard_text[some_random_rows, ]
rm(all_hansard_text, some_random_rows)


#### Topic modelling at word level ####
# Tokenize the statements to words and remove stopwords
cleaned_hansard <- some_hansard_text %>% # Start with the 100,000 sample
  unnest_tokens(word, statement) %>% # Splits the statement column into a column of tokens (words in this case, but could be changed) called word
  anti_join(custom_stop_words) %>% # Remove the stop words
  count(parliament.no, word, sort = TRUE) %>% # Get counts of each word, by each parliament - doesn't really make sense with the sample, but useful for the full dataset
  rename(document = parliament.no, term = word, count = n)

# Create DocumentTermMatrix
hansard_dtm <- cleaned_hansard %>% # Take the tokenised dataset
  cast_dtm(document, term, count) # Create DocumentTermMatrix

# Conduct LDA to find groups of topics
chapters_lda <- LDA(hansard_dtm, k = 5, control = list(seed = 1234)) # Change k to get a different number of groups. Should go through a process to find the optimal k.

# Look at the words that make up the topic-groups
Terms <- terms(chapters_lda, 30)
Terms[, 1:3]





# Worked example for text

text <- tibble(
  statement = c(
    "The Labour party believes in immigration, preferably of people of the British race. It is no reflection upon Southern Europeans to say that this country, having been pioneered and developed by the British people, must be retained for them, and any Government should protect our own kith and kin against the possibility of being displaced by foreigners.",
    "But the influx of the Chinese has been checked to some extent, though the legislation which has been passed does not go far enough. While it checked the influx of the Chinese, it did not check the influx of the Japanese, or Malays, or Javanese, or a host of others who are as great a danger to the working classes, the artisan classes, and the trading classes of Australia as the Chinese themselves.",
    "That passage makes it clear how it is that France, when in need of making an appeal to her children, has her loans oversubscribed within a few weeks. In no country in the world is wealth more evenly divided than in France - if we except Australia.",
    "While saving money is extemely important in the current economic and budgetary circumstances, I urge the ABC to have a close look at the situation in cost-benefit terms. It could well be that the benefits lost to the community of Adelaide and the ABC in that community far outweigh the relatively minor cost saving in this instance."
  ),
  parliament.no = c(1, 1, 1, 1)
)



text_example <- text %>%
  unnest_tokens(word, statement) %>%
  anti_join(custom_stop_words) %>% # Remove the stop words
  count(parliament.no, word, sort = TRUE) %>%
  rename(document = parliament.no, term = word, count = n)

# Create DocumentTermMatrix
text_example_dtm <- text_example %>% # Take the tokenised dataset
  cast_dtm(document, term, count) # Create DocumentTermMatrix

# Conduct LDA to find groups of topics
example_lda <- LDA(text_example_dtm, k = 2, control = list(seed = 1234)) # Change k to get a different number of groups. Should go through a process to find the optimal k.

# Look at the words that make up the topic-groups
Terms <- terms(example_lda, 10)
Terms[, 1:2]