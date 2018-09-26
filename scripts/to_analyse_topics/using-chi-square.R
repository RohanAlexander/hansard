# !diagnostics off
#### Preamble ####
# Purpose: Analyse topics using chi-square tests for differences
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 24 September 2018
# Prerequisites:
# Issues:


#### Workspace set-up ####
# Load libraries
library(lubridate)
# library(quanteda)
# library(stm)
# library(tictoc)
# library(tidytext)
library(tidyverse)
# library(viridis)
# update.packages()

# Load the election dates
election_dates <- read_csv("inputs/misc/misc_elections_data.csv")
# Load topics
topics <- read_csv("outputs/big_files_do_not_push/test_gammas.csv")
head(election_dates)
head(topics)


all_dates <-
  tibble(allDates = seq(ymd('1901-01-01'), ymd('2017-12-31'), by = 'days')) %>%  # Make a column of all the dates from Federation
  mutate(electionDate = if_else(allDates %in% election_dates$electionDate, 1, 0),
         electionDate = cumsum(electionDate)) %>% 
  rename(electionCounter = electionDate)

topics <- topics %>% 
  left_join(all_dates, by = c("document" = "allDates"))

rm(all_dates, election_dates)


topics_test <- topics %>% 
  filter(electionCounter %in% c(1, 2)) %>% 
  group_by(electionCounter, topic) %>% 
  summarise(ave_gamma = mean(gamma)) %>% 
  mutate(ave_gamma = as.integer(ave_gamma*100000))

topics_test <- topics_test %>% 
  spread(electionCounter, ave_gamma) %>% 
  rename(year1 = `1`, year2 = `2`) 

topics_test$topic <- NULL
topics_test[1:5,]

library(MASS)       # load the MASS package 
tbl = table(survey$Smoke, survey$Exer) 
tbl %>% class()

ggplot(topics_test, aes(x = topic, y = ave_gamma, colour = electionCounter))+
  geom_point()


chisq.test(topics_test[1:90,])