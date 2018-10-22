library(tidyverse)
update.packages()

mu_election <- read_csv("scripts/from_monica/results/mu_election.csv")

mu_election %>% 
  filter(election %in% c(38, 39, 40, 41)) %>% 
  filter(!topic %in% c(1, 16)) %>% 
  ggplot(aes(x = as.factor(election), y = median, group = as.factor(topic), colour = as.factor(topic), label = as.factor(topic))) +
  geom_line() +
  geom_text(check_overlap = TRUE) +
  theme_classic()

ggsave(
  "outputs/figures/mu_election-howard.pdf",
  height = 10,
  width = 8,
  units = "in"
)



mu_election %>% 
  filter(election %in% c(25, 26)) %>% 
  # filter(!topic %in% c(1, 16)) %>% 
  ggplot(aes(x = as.factor(election), y = median, group = as.factor(topic), colour = as.factor(topic), label = as.factor(topic))) +
  geom_line() +
  geom_text(check_overlap = TRUE) +
  theme_classic()

ggsave(
  "outputs/figures/mu_election-finalmenzies-firstholt.pdf",
  height = 10,
  width = 8,
  units = "in"
)




mu_election %>% 
  filter(election %in% c(28, 29)) %>% 
  # filter(!topic %in% c(1, 16)) %>% 
  ggplot(aes(x = as.factor(election), y = median, group = as.factor(topic), colour = as.factor(topic), label = as.factor(topic))) +
  geom_line() +
  geom_text(check_overlap = TRUE) +
  theme_classic()

ggsave(
  "outputs/figures/mu_election-whitlamelections.pdf",
  height = 10,
  width = 8,
  units = "in"
)


mu_election %>% 
  filter(election %in% c(28, 29)) %>% 
  # filter(!topic %in% c(1, 16)) %>% 
  gather(elections, medians, election:median) %>% 
  mutate(diff = median[election == 29] - median[election == 28])
ggplot(aes(x = as.factor(election), y = median, group = as.factor(topic), colour = as.factor(topic), label = as.factor(topic))) +
  geom_line() +
  geom_text(check_overlap = TRUE) +
  theme_classic()