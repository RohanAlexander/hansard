#### Preamble ####
# Purpose: Associate a group, based on similar topics, for every statement in Hansard.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 29 August 2018
# Prerequisites: 
# Issues:


#### Workspace set-up ####
# Load libraries
# library(dplyr)
# library(modelr) # Used in the kfolds section
library(tictoc)
library(tidytext)
library(tidyverse)
# library(tm)
library(topicmodels)

# Load Hansard statements
load("outputs/words.Rda") # Takes a while

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


#### Topic modelling at word level ####
# Get a sample of 100,000 statements
set.seed(123)
some_random_rows <- sample(1:nrow(words_in_hansard_by_date), 100)
some_hansard_text <- words_in_hansard_by_date[some_random_rows, ]
rm(words_in_hansard_by_date, some_random_rows)

# Tokenize the statements to words and remove stopwords
cleaned_hansard <- some_hansard_text %>% # Start with the 100,000 sample
  unnest_tokens(word, words) %>% # Splits the statement column into a column of tokens (words in this case, but could be changed) called word
  anti_join(custom_stop_words) %>% # Remove the stop words
  count(date, word, sort = TRUE) %>% # Get counts of each word, by each parliament - doesn't really make sense with the sample, but useful for the full dataset
  rename(document = date, term = word, count = n)

head(cleaned_hansard)

# Create DocumentTermMatrix
hansard_dtm <- cleaned_hansard %>% # Take the tokenised dataset
  cast_dtm(document, term, count) # Create DocumentTermMatrix

# Conduct LDA to find groups of topics
# Be careful running this. Takes a while. Comment out to avoid accidently running it.
tic('Running LDA model')
# chapters_lda <- LDA(hansard_dtm, k = 25, control = list(seed = 1234)) # Will need to go through a process to find the optimal k.
toc()

# Look at the words that make up the topic-groups
Terms <- terms(chapters_lda, 10) # Look at the top ten words
Terms[, 1:25] # For all twenty-five topics

# Get the spread of topics over each parliament
each_days_topics <- tidy(chapters_lda, matrix = "gamma")
save(each_days_topics, file = "outputs/each_days_topics.Rda")



#### Worked example ####
text <- tibble(
  statement = c(
    "The Labour party believes in immigration, preferably of people of the British race. It is no reflection upon Southern Europeans to say that this country, having been pioneered and developed by the British people, must be retained for them, and any Government should protect our own kith and kin against the possibility of being displaced by foreigners.",
    "But the influx of the Chinese has been checked to some extent, though the legislation which has been passed does not go far enough. While it checked the influx of the Chinese, it did not check the influx of the Japanese, or Malays, or Javanese, or a host of others who are as great a danger to the working classes, the artisan classes, and the trading classes of Australia as the Chinese themselves.",
    "That passage makes it clear how it is that France, when in need of making an appeal to her children, has her loans oversubscribed within a few weeks. In no country in the world is wealth more evenly divided than in France - if we except Australia.",
    "While saving money is extemely important in the current economic and budgetary circumstances, I urge the ABC to have a close look at the situation in cost-benefit terms. It could well be that the benefits lost to the community of Adelaide and the ABC in that community far outweigh the relatively minor cost saving in this instance."
  ),
  parliament.no = c(1, 1, 1, 1)
)

# Get the list of words
text_example <- text %>%
  unnest_tokens(word, statement) %>%
  anti_join(custom_stop_words) %>% # Remove the stop words
  count(parliament.no, word, sort = TRUE) %>%
  rename(document = parliament.no, term = word, count = n)

# Create DocumentTermMatrix
text_example_dtm <- text_example %>% # Take the tokenised dataset
  cast_dtm(document, term, count) # Create DocumentTermMatrix

# Conduct LDA to find groups of topics
example_lda <- LDA(text_example_dtm, k = 2, control = list(seed = 1234)) # 2 topics have been specified

# Look at the words that make up the topic-groups
Terms <- terms(example_lda, 10)
Terms[, 1:2]


#### With k-fold cross validation ####
# The main issue is that it's not clear how many topics is appropriate to specify. One way to solve this is cross-validation (*insert Peter's Hastie/Tibshirani joke here*).
# Essentially what is going to happen is that we'll implement LDA with a test/train setup and a bunch of possible numbers of topics and then compare the results using perplexity. 
# Based on https://stackoverflow.com/questions/21355156/topic-models-cross-validation-with-loglikelihood-or-perplexity

folds <- 5
n <- nrow(hansard_dtm)
splitfolds <- sample(1:folds, n, replace = TRUE)
candidate_k <- c(2, 3, 4, 5, 10, 20, 30, 40, 50, 75, 100, 200, 300) # candidates for how many topics


cv1 <- crossv_kfold(hansard_dtm, 5)
cv1$train[1]
cv1$test[1]
models <- map(cv1$train, ~ lm(mpg ~ wt, data = .))
errs <- map2_dbl(models, cv1$test, rmse)
hist(errs)

cv1 <- crossv_kfold(mtcars, 5)
cv1

library(purrr)
cv2 <- crossv_mc(mtcars, 100)
models <- map(cv2$train, ~ lm(mpg ~ wt, data = .))
errs <- map2_dbl(models, cv2$test, rmse)
hist(errs)



# Alternative from https://cran.r-project.org/web/packages/ldatuning/vignettes/topics.html

# install.packages("ldatuning")
library(ldatuning)

data("AssociatedPress", package="topicmodels")
dtm <- AssociatedPress[1:10, ]

result <- FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to = 15, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)
FindTopicsNumber_plot(result)




result <- FindTopicsNumber(
  hansard_dtm,
  topics = seq(from = 2, to = 100, by = 5),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)

FindTopicsNumber_plot(result)

example_lda_12 <- LDA(hansard_dtm, k = 12, control = list(seed = 1234)) # 2 topics have been specified
Sys.time()
example_lda_20 <- LDA(hansard_dtm, k = 20, control = list(seed = 1234)) # 2 topics have been specified
Sys.time()
example_lda_50 <- LDA(hansard_dtm, k = 50, control = list(seed = 1234)) # 2 topics have been specified
Sys.time()

perplexity(example_lda_12)
perplexity(example_lda_20)
perplexity(example_lda_50)