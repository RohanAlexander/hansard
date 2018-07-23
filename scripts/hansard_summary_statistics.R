#### Preamble ####
# Purpose: TBD
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au 
# Date: 17 January 2018
# Prerequisites: ...
# Issues: ...


#### Workspace set-up ####
library(tidyverse)
library(lubridate)

setwd("~/Documents/projects/gov_spending_determinants")

load("outputs/all_hansard_text.Rda")

# Get only unique debate titles and add a column with the count of how many times that debate title has been used. Filter so that only those with more than 1,000 uses show and then arrange in descending order.
unique_debate_titles_and_counts <- all_hansard_text %>%
  group_by(debate_title) %>%
  mutate(count = n()) %>%
  filter(count > 1000) %>%
  arrange(desc(count)) %>%
  distinct(debate_title, .keep_all = TRUE)




tester <- all_hansard_text %>%
  filter(speakerID %in% c("L07"))

?distinct




# Look at the Gouglas measures
politicians_by_year <- all_hansard_text %>% 
  group_by(speakerID) %>% 
  summarise(first = min(year(dates)),
            last = max(year(dates))) %>%
  mutate(check = last - first)

politicians_by_year$check <- NULL

politicians_by_start_year <- politicians_by_year %>% 
  group_by(first) %>% 
  summarise(Count = n()) %>%
  rename(Year = first)

parliament_size <- read_csv("data/parliament_size.csv")

newly_elected_politicians_talk <- read_csv("data/politicians_elected_would_talk.csv")

years <- data.frame(Year = c(1901:1980))

parliament_size <- left_join(years, parliament_size, by = "Year") %>%
  fill(Seats)

rm(years)

parliament_size <- left_join(parliament_size, newly_elected_politicians_talk, by = "Year") %>%
  rename(important = Election)

rm(newly_elected_politicians_talk)

gouglas_data <- left_join(parliament_size, politicians_by_start_year, by = "Year") %>%
  rename(new_hansard_speakers = count, seats = Seats, year = Year) %>%
  replace_na(list(new_hansard_speakers = 0)) %>%
  mutate(gouglas_new = new_hansard_speakers / seats)

gouglas_data <- gouglas_data[2:nrow(gouglas_data),] #Get rid of 1901 because that was the first one

ggplot(data = gouglas_data, mapping = aes(x = year, y = gouglas_new)) + 
  geom_point() +
  geom_smooth(method='lm') + 
  theme_classic()

gouglas_data <- gouglas_data %>%
  mutate(gouglas_new_important = ifelse(important == 1, gouglas_new, NA))

ggplot(data = gouglas_data, mapping = aes(x = year, y = gouglas_new_important)) + 
  geom_point() +
  geom_smooth(method='lm') + 
  theme_classic()

write_csv(gouglas_data, path = "outputs/gouglas_data.csv")





# Look at number of questions compared with other
unique(all_hansard_text$debate_type)
count(all_hansard_text, debate_type, sort = TRUE)
?count




# Misc code


ggplot(data = politicians_by_year) + 
  geom_segment(mapping = aes(x = first, xend = last, y = speakerID, yend = speakerID))


#People lasting a lot less time
# number of speakers, parties, average statement length..


