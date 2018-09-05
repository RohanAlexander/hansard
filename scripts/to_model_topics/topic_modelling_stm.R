#### Preamble ####
# Purpose: Associate a group, based on similar topics, for every statement in Hansard.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 5 September 2018
# Prerequisites: 
# Issues:


#### Workspace set-up ####
# Load libraries
# devtools::install_github("dgrtwo/drlib")
library(drlib)
# library(dplyr)
library(lubridate)
# library(modelr) # Used in the kfolds section
# install.packages("stm")
library(stm)
# citation('stm')
library(tictoc)
library(tidytext)
library(tidyverse)
# library(tm)
# library(topicmodels)
library(viridis)
# update.packages()

# Load Hansard statements
load("outputs/hansard/all_hansard_words_by_date.Rda") # Takes a while


#### Get a sample of the days ####
set.seed(123)
some_random_rows <- sample(1:nrow(all_hansard_words), 2000) # Change the integer to a larger number on the full run through if you want
some_days_hansard_words <- all_hansard_words[some_random_rows, ]
rm(all_hansard_words, some_random_rows)
# Here - what you want to do for the graph is to aggregate the days by years then run it

# housethat -> house that
# tbe -> the
# there seems to be lots of 'tile' and 'tlie' - are these percentile errors?


#### Temp fix for dodgy words
# Temp fix for getting rid of politicians names
politicians <- read_csv("data/politicians/politicians_by_individuals.csv") %>% 
  select(surname) %>% 
  mutate(surname = str_to_lower(surname)) %>% 
  unique() %>% 
  pull()

# Bind custom list of stopwords to the default list
custom_stop_words <- bind_rows(
  stop_words, # The default list
  data_frame(
    word = c(politicians, "bhe", "tbe", 'tile', 'tlie', "duncanhughes", "fche", "honorahle", "madam", "brucepage", "archie",
"showeth",
"act", "amendment", "amount", "australia", "australian", "bill", "board", "cent", "clause", "commission", "committee", "commonwealth", "countries", "country", "day", "deal", "debate", "department", "desire", "duty", "gentleman", "government", "honorable", "honourable", "house", "increase", "labor", "labour", "leader", "legislation", "matter", "minister", "national", "opposition", "parliament", "party", "people", "policy", "position", "power", "prime", "proposed", "public", "question", "regard", "statement", "support", "system", "time"
    ),
    lexicon = rep("custom", length(word))
  )
)




## Sweet graph from https://juliasilge.com/blog/sherlock-holmes-stm/
tidy_hansard <- some_days_hansard_words %>%
  mutate(line = row_number()) %>%
  mutate(year = year(date)) %>% 
  mutate(decade = case_when(
    year <= 1909 ~ 1900,
    year <= 1919 ~ 1910,
    year <= 1929 ~ 1920,
    year <= 1939 ~ 1930,
    year <= 1949 ~ 1940,
    year <= 1959 ~ 1950,
    year <= 1969 ~ 1960,
    year <= 1979 ~ 1970,
    year <= 1989 ~ 1980,
    year <= 1999 ~ 1990,
    year <= 2009 ~ 2000,
    year <= 2019 ~ 2010
  )) %>%  # Thanks http://derekogle.com/fishR/2018-03-30-Collapsing_Values
  unnest_tokens(word, words) %>%
  anti_join(custom_stop_words) 

head(tidy_hansard)


tidy_hansard %>%
  count(word, sort = TRUE)


# By day
hansard_tf_idf <- tidy_hansard %>%
  count(date, word, sort = TRUE) %>% # Construct them by day
  bind_tf_idf(word, date, n) %>%
  arrange(-tf_idf) %>%
  group_by(date) %>%
  top_n(10) %>%
  ungroup


graph_these <- c("1902-06-17", "1903-07-09", "1904-06-02", "1908-04-02", "1926-02-05", "1928-05-15", "1930-06-20", "1950-09-27", "1965-03-25", "1970-10-28", "1996-05-09", "2002-06-25", "2009-06-24")
graph_these <- graph_these %>% ymd()
class(graph_these[1])

# some_random_dates <- sample(some_days_hansard_words$date %in% of_interest, 12)
# some_days_hansard_words$date %>% pull()
# some_random_dates

hansard_tf_idf %>%
  mutate(word = reorder_within(word, tf_idf, date)) %>% 
  filter(date %in% graph_these) %>%
  ggplot(aes(word, tf_idf, fill = "date")) +
  theme_minimal() +
  geom_col(alpha = 0.8, show.legend = FALSE, aes(fill = as.factor(date))) +
  facet_wrap(~ date, scales = "free", ncol = 3) +
  scale_x_reordered() +
  coord_flip() +
  theme(strip.text=element_text(size=20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size=20)) +
  # labs(x = NULL, y = "tf-idf", title = "Highest tf-idf words in Hansard") +
  labs(x = NULL, y = "tf-idf") +
  scale_fill_viridis_d()
  

# By year
hansard_tf_idf_year <- tidy_hansard %>%
  count(year, word, sort = TRUE) %>% # Construct them by year
  bind_tf_idf(word, year, n) %>%
  arrange(-tf_idf) %>%
  group_by(year) %>%
  top_n(10) %>%
  ungroup

some_random_years <- sample(c(1901:2017), 12)

hansard_tf_idf_year %>%
  mutate(word = reorder_within(word, tf_idf, year)) %>% 
  filter(year %in% some_random_years) %>%
  ggplot(aes(word, tf_idf, fill = "year")) +
  theme_minimal() +
  geom_col(alpha = 0.8, show.legend = FALSE, aes(fill = as.factor(year))) +
  facet_wrap(~ year, scales = "free", ncol = 3) +
  scale_x_reordered() +
  coord_flip() +
  theme(strip.text=element_text(size=11)) +
  labs(x = NULL, y = "tf-idf",
       title = "Highest tf-idf words in Hansard") +
  scale_fill_viridis_d()


# By decade
hansard_tf_idf_year <- tidy_hansard %>%
  count(decade, word, sort = TRUE) %>% # Construct them by year
  bind_tf_idf(word, decade, n) %>%
  arrange(-tf_idf) %>%
  group_by(decade) %>%
  top_n(10) %>%
  ungroup

# some_random_years <- sample(c(1901:2017), 12)

hansard_tf_idf_year %>%
  mutate(word = reorder_within(word, tf_idf, decade)) %>% 
  # filter(decade %in% some_random_years) %>%
  ggplot(aes(word, tf_idf, fill = "decade")) +
  theme_minimal() +
  geom_col(alpha = 0.8, show.legend = FALSE, aes(fill = as.factor(decade))) +
  facet_wrap(~ decade, scales = "free", ncol = 3) +
  scale_x_reordered() +
  coord_flip() +
  theme(strip.text=element_text(size=11)) +
  labs(x = NULL, y = "tf-idf",
       title = "Highest tf-idf words in Hansard") +
  scale_fill_viridis_d()



# head(some_days_hansard_words)
some_years_hansard_words <- some_days_hansard_words %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarise(theWords = toString(words)) %>%
  ungroup()

# head(some_years_hansard_words)


processed <- textProcessor(some_days_hansard_words$words, metadata = some_days_hansard_words)

processed <- textProcessor(some_years_hansard_words$theWords, metadata = some_years_hansard_words)

plotRemoved(processed$documents, lower.thresh = seq(1, 200, by = 100))
out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 15)

docs <- out$documents
vocab <- out$vocab
meta <-out$meta

hansardPrevFit <-
  stm(
    documents = out$documents,
    vocab = out$vocab,
    K = 30,
    max.em.its = 75,
    data = out$meta,
    init.type = "Spectral"
  )


##Sweet graph of probabilities that each word is generated from each topic.
td_beta <- tidy(hansardPrevFit)

td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")



## Sweet graph of probability each document is generated from each topic
td_gamma <- tidy(hansardPrevFit, matrix = "gamma",                    
                 document_names = some_days_hansard_words$date)

ggplot(td_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 3) +
  labs(title = "Distribution of document probabilities for each topic",
       subtitle = "Each topic is associated with 1-3 stories",
       y = "Number of stories", x = expression(gamma))



write_csv(td_gamma, path = "outputs/td_gamma.csv")














hansardSelect <-
  selectModel(
    out$documents,
    out$vocab,
    K = 20,
    max.em.its = 75,
    data = out$meta,
    runs = 20,
    seed = 8458159 # Thank you to the authors of STM
  )

plotModels(hansardSelect, pch=c(1,2,3,4), legend.position="bottomright") # Want a model with points in top right

selectedModel <- hansardSelect$runout[[2]]

storage <-
  searchK(
    out$documents,
    out$vocab,
    K = c(5, 10, 15, 20, 25, 30),
    data = meta
  )

labelTopics(selectedModel, c(3, 7, 20))

thoughts3 <- findThoughts(selectedModel, texts = some_days_hansard_words$words, n = 2, topics = 3)$docs[[1]]
thoughts20 <- findThoughts(selectedModel, texts = some_days_hansard_words$words, n = 2, topics = 20)$docs[[1]]

# par(mfrow = c(1, 2),mar = c(.5, .5, 1, .5))
# plotQuote(thoughts3, width = 30, main = "Topic 3")
# plotQuote(thoughts20, width = 30, main = "Topic 20")

plot.STM(selectedModel,type = "summary")




prep <- estimateEffect(1:3 ~ treatment, selectedModel, some_days_hansard_words)
plot(prep, "treatment", model=gadarianFit,
     method="pointestimate")

?plot.estimateEffect

plot(mod.out.corr)

draws <- thetaPosterior(selectedModel)



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

## MISC start
# Remove common words
parsed_text_file <- parsed_text_file %>%
  anti_join(stop_words)

#stem words
parsed_text_file <- parsed_text_file %>%
  mutate(stem_word = wordStem(word, language="english"))
## MISC end


#### Topic modelling at word level ####

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