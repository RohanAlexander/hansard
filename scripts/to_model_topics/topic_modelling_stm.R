#### Preamble ####
# Purpose: Associate a group, based on similar topics, for every statement in Hansard.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 6 September 2018
# Prerequisites: 
# Issues:


#### Workspace set-up ####
# Load libraries
# devtools::install_github("dgrtwo/drlib")
library(drlib)
library(lubridate)
# library(modelr) # Used in the kfolds section
library(stm)
library(tictoc)
library(tidytext)
library(tidyverse)
library(viridis)
# update.packages()

# Load Hansard words by day
load("outputs/for_topic_models/all_hansard_words_by_date.Rda") # Takes a while


#### Take a sample from the days ####
set.seed(123)
some_random_rows <- sample(1:nrow(all_hansard_words), 1000) # Change the integer to a larger number through if you want
some_days_hansard_words <- all_hansard_words[some_random_rows, ]
rm(all_hansard_words, some_random_rows)


#### Temp fix for dodgy words ####
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
    word = c(politicians,"na", "tax", "ment", "south", "wales", "na", "bhe", "tbe", 'tile', 'tlie', "duncanhughes", "fche", "honorahle", "madam", "brucepage", "archie",
"showeth", "report", "sir",
"act", "amendment", "amount", "australia", "australian", "bill", "board", "cent", "clause", "commission", "committee", "commonwealth", "countries", "country", "day", "deal", "debate", "department", "desire", "duty", "gentleman", "government", "honorable", "honourable", "house", "increase", "labor", "labour", "leader", "legislation", "matter", "minister", "national", "opposition", "parliament", "party", "people", "policy", "position", "power", "prime", "proposed", "public", "question", "regard", "statement", "support", "system", "time", "industry", "million"
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


graph_these <- c("1902-06-17", "1904-06-02", "1908-04-02", "1926-02-05", "1928-05-15", "1930-06-20", "1950-09-27", "1965-03-25", "1970-10-28", "1996-05-09", "2002-06-25", "2009-06-24")
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
  geom_col(alpha = 0.8,
           show.legend = FALSE,
           aes(fill = as.factor(date))) +
  facet_wrap( ~ date, scales = "free", ncol = 3) +
  scale_x_reordered() +
  coord_flip() +
  theme(
    strip.text = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(size = 12)
  ) +
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



# head(tidy)
# some_years_hansard_words <- some_days_hansard_words %>% 
#   mutate(year = year(date)) %>% 
#   group_by(year) %>% 
#   summarise(theWords = toString(words)) %>%
#   ungroup()

head(tidy_hansard)

# tidy_hansard$year %>% unique %>% sort

tidy_hansard <- tidy_hansard %>% 
  select(year, word)

hansard_dfm <- tidy_hansard %>%
  count(year, word, sort = TRUE) %>%
  cast_dfm(year, word, n)

hansardPrevFit <-
  stm(
    hansard_dfm,
    K = 30,
    init.type = "Spectral"
  )



hansardPrevFit20 <-
  stm(
    hansard_dfm,
    K = 15,
    init.type = "Spectral"
  )

hansardPrevFit <- hansardPrevFit20



head(hansard_dfm)

# processed <- textProcessor(some_days_hansard_words$words, metadata = some_days_hansard_words)
# 
# processed <- textProcessor(some_years_hansard_words$theWords, metadata = some_years_hansard_words)

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
                 document_names = rownames(hansard_dfm))

ggplot(td_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 3) +
  labs(title = "Distribution of document probabilities for each topic",
       subtitle = "Each topic is associated with 1-3 stories",
       y = "Number of stories", x = expression(gamma))



write_csv(td_gamma, path = "outputs/td_gamma.csv")






