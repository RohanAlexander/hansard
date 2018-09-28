
# Load libraries
library(lubridate)
library(tidyverse)

# Load the gammas from the topic model
td_gamma <- read_csv("outputs/big_files_do_not_push/gammas_model_prevalence_is_spline.csv", col_types = cols())
head(td_gamma)

# Load the metadata that will define groups
election_dates <- read_csv("inputs/misc/misc_elections_data.csv", col_types = cols())
governmentChanges <- read_csv("inputs/misc/change_of_pm.csv", col_types = cols())
keyEconomicDates <- read_csv("inputs/misc/key_dates-economic.csv", col_types = cols())
keyOtherDates <- read_csv("inputs/misc/key_dates-other.csv", col_types = cols())


# Join the metadata to the gammas data
all_dates <-
  tibble(allDates = seq(ymd('1901-01-01'), ymd('2017-12-31'), by = 'days')) %>%  # Make a column of all the dates from Federation
  mutate(electionDate = if_else(allDates %in% election_dates$electionDate, 1, 0),
         electionDate = cumsum(electionDate),
         governmentChangeDate = if_else(allDates %in% governmentChanges$end, 1, 0),
         governmentChangeDate = cumsum(governmentChangeDate),
         keyEconomicChange = if_else(allDates %in% keyEconomicDates$theDate, 1, 0),
         keyEconomicChange = cumsum(keyEconomicChange),
         keyOtherChange = if_else(allDates %in% keyOtherDates$theDate, 1, 0),
         keyOtherChange = cumsum(keyOtherChange)) %>% 
  rename(electionCounter = electionDate)
head(all_dates)

td_gamma <- td_gamma %>% 
  left_join(all_dates, by = c("document" = "allDates"))
head(td_gamma)


td_gamma_reduced <- td_gamma %>% 
  filter(topic %in% c(1:20)) %>% 
  mutate(grouper = case_when(topic %in% c(1,5,9,13,17) ~ "Topics 1, 5, 9, 13, 17",
                             topic %in% c(2,6,10,14,18) ~ "Topics 2, 6, 10, 14, 18",
                             topic %in% c(3,7,11,15,19) ~ "Topics 3, 7, 11, 15, 19",
                             TRUE ~ "Topics 4, 8, 12, 16, 20"
                             ),
         Topic = as.factor(topic))


# Change in election
ggplot(data = td_gamma_reduced, aes(x = electionCounter, y = gamma, color = Topic)) +
  geom_point() +
  theme_classic() +
  facet_wrap(vars(grouper), nrow = 4) +
  labs(x = "Election",
       y = "Gamma") +
  theme(strip.background = element_blank()) +
  scale_colour_viridis_d()

ggsave(
    "outputs/figures/topics_separated_by_election.pdf",
     height = 8,
     width = 6,
     units = "in"
  )

# Change in government
ggplot(data = td_gamma_reduced, aes(x = governmentChangeDate, y = gamma, color = Topic)) +
  geom_point() +
  theme_classic() +
  facet_wrap(vars(grouper), nrow = 4) +
  labs(x = "Government change",
       y = "Gamma") +
  theme(strip.background = element_blank()) +
  scale_colour_viridis_d()

ggsave(
  "outputs/figures/topics_separated_by_government_change.pdf",
  height = 8,
  width = 6,
  units = "in"
)

# Change in economic conditions
ggplot(data = td_gamma_reduced, aes(x = keyEconomicChange, y = gamma, color = Topic)) +
  geom_point() +
  theme_classic() +
  facet_wrap(vars(grouper), nrow = 4) +
  labs(x = "Changed economic conditions",
       y = "Gamma") +
  theme(strip.background = element_blank()) +
  scale_colour_viridis_d()

ggsave(
  "outputs/figures/topics_separated_by_economic_changes.pdf",
  height = 8,
  width = 6,
  units = "in"
)


# Changed other conditions
ggplot(data = td_gamma_reduced, aes(x = keyOtherChange, y = gamma, color = Topic)) +
  geom_point() +
  theme_classic() +
  facet_wrap(vars(grouper), nrow = 4) +
  labs(x = "Changed other conditions",
       y = "Gamma") +
  theme(strip.background = element_blank()) +
  scale_colour_viridis_d()

ggsave(
  "outputs/figures/topics_separated_by_other_changes.pdf",
  height = 8,
  width = 6,
  units = "in"
)

  