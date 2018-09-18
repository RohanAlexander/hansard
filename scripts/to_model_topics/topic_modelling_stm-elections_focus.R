# !diagnostics off
#### Preamble ####
# Purpose: Associate a group, based on similar topics, for every statement in Hansard that is either the last two weeks before an electin or the first two weeks.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 17 September 2018
# Prerequisites: 
# Issues:


#### Workspace set-up ####
# Load libraries
# devtools::install_github("dgrtwo/drlib")
library(drlib)
library(furrr)
library(lubridate)
# library(modelr) # Used in the kfolds section
library(stm)
library(tictoc)
library(tidytext)
library(tidyverse)
library(viridis)
# update.packages()

plan(multiprocess)

# Load Hansard words by day
load("outputs/big_files_do_not_push/all_hansard_words_by_date.Rda") # Takes 30 sec or so
# Load the election dates
election_dates <- read_csv("inputs/misc/misc_elections_data.csv")
# Load the Hansard dates
hansard_dates <- read_csv("inputs/misc/hansard_dates.csv")


#### Temp fix for dodgy words ####
# Temp fix for getting rid of politicians names
politicians <- read_csv("inputs/politicians/politicians_by_individuals.csv") %>% 
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


#### Prepare for topic modelling ####
# Put each word on its own row - from https://juliasilge.com/blog/sherlock-holmes-stm/
# Best to split it up so you don't have memory issues
all_hansard_words <- all_hansard_words %>% 
  mutate(grouper = sample(1:10, nrow(all_hansard_words), rep = TRUE))

tidy_hansard_pieces <- function(name_of_input_tibble){
  name_of_input_tibble %>% # takes a minute or two
  mutate(line = row_number()) %>%
  unnest_tokens(word, words) %>%
  anti_join(custom_stop_words) 
}

tidy_hansard <- all_hansard_words %>% 
  split(.$grouper) %>% 
  map_dfr(tidy_hansard_pieces, .id = NULL)

rm(all_hansard_words, tidy_hansard_pieces)

# Construct the TF-IDF measures on a day basis
# By day
# This takes about 10 minutes or so depending on the specifics - BYO book
hansard_tf_idf <- tidy_hansard %>%
  count(date, word, sort = TRUE) %>% # Construct them by day
  bind_tf_idf(word, date, n) %>%
  arrange(-tf_idf) %>%
  group_by(date) %>%
  top_n(100) %>%
  ungroup

head(hansard_tf_idf)
# Can graph that if you want
rm(hansard_tf_idf)


#### Topic modelling ####
head(tidy_hansard)
tidy_hansard <- tidy_hansard %>% 
  select(date, word)

hansard_dfm <- tidy_hansard %>%
  count(date, word, sort = TRUE) %>%
  filter(n > 100) %>% # Here we remove any word that doesn't occur at least 100 times
  cast_dfm(date, word, n)
  
many_models <-
  data_frame(K = c(20, 40, 60, 80, 100)) %>% 
  mutate(topic_model = future_map(K, ~ stm(hansard_dfm, K = ., verbose = TRUE)))

heldout <- make.heldout(hansard_dfm)

k_result <- many_models %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, hansard_dfm), eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, hansard_dfm),
         bound = map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)), lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

k_result


k_result %>% transmute(K, `Lower bound` = lbound, Residuals = map_dbl(residual, "dispersion"),
  `Semantic coherence` = map_dbl(semantic_coherence, mean),
  `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")
) %>%
  gather(Metric, Value,-K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5,
            alpha = 0.7,
            show.legend = FALSE) +
  facet_wrap( ~ Metric, scales = "free_y") +
  labs(
    x = "K (number of topics)",
    y = NULL,
    title = "Model diagnostics by number of topics"
    # subtitle = "These diagnostics indicate that a good number of topics would be around 60"
  )


k_result %>%
  select(K, exclusivity, semantic_coherence) %>%
  # filter(K %in% c(20, 60, 100)) %>%
  unnest() %>%
  mutate(K = as.factor(K)) %>%
  ggplot(aes(semantic_coherence, exclusivity, color = K)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence",
       subtitle = "Models with fewer topics have higher semantic coherence for more topics, but lower exclusivity")

topic_model <- k_result %>% 
  filter(K == 80) %>% 
  pull(topic_model) %>% 
  .[[1]]



#### Just run a quick one here for the results for the draft - DELETE START
# Identify which hansard date is the first after an election
all_dates <- tibble(allDates = seq(ymd('1901-01-01'), ymd('2017-12-31'), by = 'days')) # First make a list of all the dates from Federation
all_dates <- all_dates %>%
  mutate(
    electionDate = if_else(allDates %in% election_dates$electionDate, 1, 0),
    hansardDate = if_else(allDates %in% all_hansard_words$date, 1, 0)
  ) %>%
  filter(electionDate == 1 | hansardDate == 1) %>%  # Reduce that of all dates since Federation to only those that we have a Hansard for or that had an election
  mutate(firstHansardAfterElection = lag(electionDate)) %>% 
  select(allDates, firstHansardAfterElection) %>% 
  filter(firstHansardAfterElection == 1)

all_hansard_words <- all_hansard_words %>% 
  left_join(all_dates, by = c("date" = "allDates"))

all_hansard_words$firstHansardAfterElection[is.na(all_hansard_words$firstHansardAfterElection)] <- 0
  
processed_Hansard <- textProcessor(all_hansard_words$words, metadata = all_hansard_words)

# HERE

out <- prepDocuments(processed_Hansard$documents, processed_Hansard$vocab, processed_Hansard$meta, lower.thresh = 15)





tidy_hansard <- tidy_hansard %>%
  count(date, word, sort = TRUE) %>%
  filter(n > 100)  # Here we remove any word that doesn't occur at least 100 times
head(tidy_hansard)
library(quanteda)
?quanteda



justOne <- stm(hansard_dfm, K = 100, verbose = TRUE)
topic_model <- justOne
rm(justOne)

#### DELETE END


topic_model

td_beta <- tidy(topic_model)
td_beta

td_gamma <- tidy(topic_model, matrix = "gamma",
                 document_names = rownames(hansard_dfm))
td_gamma
# write_csv(td_gamma, "test_gammas.csv")

library(ggthemes)

top_terms <- td_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

gamma_terms %>%
  select(topic, gamma, terms) %>%
  knitr::kable(digits = 3, 
        col.names = c("Topic", "Expected topic proportion", "Top 7 terms"))


## Sweet graph of probability each document is generated from each topic
td_gamma$document <- ymd(td_gamma$document)
head(td_gamma)

td_gamma %>% 
  filter(gamma >= 0.001) %>% # Get rid of the zeros while testing
  ggplot(aes(x = document, y = gamma)) +
  geom_point() +
  theme_classic() +
  labs(title = "Distribution of topic probabilities for each day",
       y = "Date", 
       x = "Probability")

ggsave("outputs/figures/td_gamma.pdf")

# Try to get summary stats by whether before or after election
td_gamma$document <- td_gamma$document %>% ymd()

td_gamma <- td_gamma %>% 
  left_join(relevant_dates, by = c("document" = "allDates"))

gamme_summarises <- td_gamma %>% 
  group_by(electionNumber, beforeAfter, topic) %>% 
  summarise(ave_gamme = mean(gamma))

gamme_summarises_diff <- gamme_summarises %>% 
  spread(beforeAfter, ave_gamme) %>% 
  rename("afterElection" = `1`, "beforeElection" = `0`) %>% 
  filter(!is.na(afterElection)) %>% 
  filter(!is.na(beforeElection)) %>% 
  mutate(diff = abs(afterElection - beforeElection))

gamme_summarises_diff_summary <- gamme_summarises_diff %>% 
  group_by(electionNumber) %>% 
  summarise(average = mean(diff))



election_dates <- election_dates %>% 
  filter(!is.na(electionDate)) %>% 
  mutate(lastElectionWinner = lag(electionWinner)) %>% 
  mutate(changedPartyOfGovernment = if_else(electionWinner == lastElectionWinner, 0, 1)) %>% 
  mutate(electionNumber = 1:n()) %>% 
  select(-year, -election, -seatsTotalNumber, -electionWinner, -comment, -lastElectionWinner)


gamme_summarises_diff_summary <- gamme_summarises_diff_summary %>% 
  left_join(election_dates, by = c("electionNumber" = "electionNumber"))


library(ggrepel)
class(gamme_summarises_diff_summary$changedPartyOfGovernment.x)
gamme_summarises_diff_summary$changedPartyOfGovernment.x <- as.character(gamme_summarises_diff_summary$changedPartyOfGovernment.x)

ggplot(data = gamme_summarises_diff_summary, aes(x = electionDate.x, y = average)) +
  geom_point() +
  geom_text(aes(label = electionDate, color = changedPartyOfGovernment.x))














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













#### Identify the dates that are the 10 days of sitting immediately before and after an election ####
all_dates <- tibble(allDates = seq(ymd('1901-01-01'), ymd('2017-12-31'), by = 'days')) # First make a list of all the dates from Federation
all_dates <- all_dates %>%
  mutate(
    electionDate = if_else(allDates %in% election_dates$electionDate, 1, 0),
    hansardDate = if_else(allDates %in% all_hansard_words$date, 1, 0)
  ) %>%
  filter(electionDate == 1 | hansardDate == 1) %>% # Reduce that of all dates since Federation to only those that we have a Hansard for or that had an election
  mutate(
    tenAfterElectionDay = lag(electionDate, n = 10),
    elevenBeforeElectionDay = lead(electionDate, n = 11)
  ) %>% # Use eleven because we're going to get rid of 11 onward in order to get the ten
  mutate(
    elevenBeforeElectionDay = na_if(elevenBeforeElectionDay, 0),
    tenAfterElectionDay = na_if(tenAfterElectionDay, 0)
  )

all_dates$elevenBeforeElectionDay[all_dates$elevenBeforeElectionDay == 1] <- 2

relevant_dates <- all_dates %>%
  mutate(tenAfterElectionDay = if_else(
    !is.na(elevenBeforeElectionDay),
    elevenBeforeElectionDay,
    tenAfterElectionDay
  )) %>%
  fill(tenAfterElectionDay, .direction = "up") %>%
  mutate(tenAfterElectionDay = na_if(tenAfterElectionDay, 2)) %>%
  mutate(dateOfInterest = tenAfterElectionDay) %>%
  select(-tenAfterElectionDay,-elevenBeforeElectionDay,-hansardDate) %>%
  filter(electionDate == 0) %>%
  filter(dateOfInterest == 1) %>%
  mutate(beforeAfter = c(rep(c(
    rep(1, 10), rep(0, 10)
  ), 44), rep(1, 10))) %>%  # this is a bit dodgy just making it 0 if before an election and 1 if the ten after the election
  mutate(electionNumber = c(c(1, 0,0,0,0,0,0,0,0,0), rep(c(1, 0,0,0,0,0,0,0,0,0, 0, 0,0,0,0,0,0,0,0,0), 44))) %>% 
  mutate(electionNumber = cumsum(electionNumber))

head(relevant_dates)



# write_csv(relevant_dates, "test.csv")
rm(all_dates, election_dates, hansard_dates)
relevant_dates$allDates

# Filter to just the dates that are the date nearest each side of an election
head(all_hansard_words)
some_days_hansard_words <- all_hansard_words[which(all_hansard_words$date %in% relevant_dates$allDates), ]
rm(all_hansard_words)
