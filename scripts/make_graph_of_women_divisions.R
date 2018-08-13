library(lubridate)
library(tidyverse)

load("outputs/politicians_by_division.Rda")
load("outputs/politicians_by_individuals.Rda")

head(politicians_by_individuals)
head(politicians_by_division)

both <- left_join(politicians_by_individuals, politicians_by_division)

test <- both %>% 
  arrange(division, electionDate) %>% 
  select(uniqueID, division, electionDate, gender) %>%
  mutate(previousMembersGender = lag(gender))

test_Test <- test %>% 
  filter(gender == "female") %>%
  group_by(uniqueID) %>%
  filter(electionDate == min(electionDate))
         
head(test_Test)

check <- test_Test %>% 
  # filter(year(electionDate) > 2000) %>% 
  group_by(electionDate, previousMembersGender) %>% 
  summarise(number = n())

