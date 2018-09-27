# install.packages("rstan")
# install.packages("rstanarm")
# install.packages("arm")

library("rstan")
library("rstanarm")
library("arm")
library(tidyverse)
library(lubridate)
# options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)




# Load the election dates
election_dates <- read_csv("inputs/misc/misc_elections_data.csv")
# Load topics
topics <- read_csv("outputs/big_files_do_not_push/gammas_model_prevalence_is_spline.csv")
head(election_dates)
head(topics)


all_dates <-
  tibble(allDates = seq(ymd('1901-01-01'), ymd('2017-12-31'), by = 'days')) %>%  # Make a column of all the dates from Federation
  mutate(electionDate = if_else(allDates %in% election_dates$electionDate, 1, 0),
         electionDate = cumsum(electionDate)) %>% 
  rename(electionCounter = electionDate)

head(all_dates)

topics <- topics %>% 
  left_join(all_dates, by = c("document" = "allDates"))

head(topics)

topics_dates <- tibble(date = topics$document %>% unique())
topics_dates <- topics_dates %>% 
  arrange(date) %>% 
  mutate(counter = 1:nrow(topics_dates))

topics <- topics %>% 
  left_join(topics_dates, by = c("document" = "date"))

head(topics)

topics_test <- topics %>% 
  # filter(electionCounter %in% c(1, 2)) %>%
  mutate(gamma = gamma * 1000000000,
         gamma = as.integer(gamma)) %>% 
  # filter(!topic %in% c(1, 2)) %>%
  mutate(electionCounter = as.integer(electionCounter))

head(topics_test)
?scale


# fit_1 <- stan_glmer(gamma ~ (1 + counter | topic) + counter + electionCounter, data=topics_test)
# Allow varying slope and intercept for topics and, allow slope of counter to vary by topic

fit_1 <-
  lmer(
    gamma ~ (1 | topic) + counter + electionCounter,
    data = topics_test
  )
# Each topic gets a different intercept

summary(fit_1)
plot(fit_1)
display(fit_1)
ranef(fit_1)
?fit


# install.packages("merTools")
# library(merTools)
# m1 <- lmer(gamma ~ (1 | topic) + counter + electionCounter,
#            data = topics_test)
# shinyMer(m1, simData = topics_test[1:100, ]) # just try the first 100 rows of data


topics_test_rescale <- transform(topics_test,
                                 gamma=scale(gamma),
                                 counter=scale(counter),
                                 electionCounter=scale(electionCounter))
head(topics_test_rescale)

fit_1 <- lmer(gamma ~ (1 + electionCounter | topic) + counter + electionCounter, data=topics_test_rescale)
summary(fit_1)
plot(fit_1)
display(fit_1)
ranef(fit_1)
logLik(fit_1)
coef(summary(fit_1))