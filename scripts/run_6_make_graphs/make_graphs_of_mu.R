library(tidyverse)

mu_election <- read_csv("scripts/from_monica/results/mu_election.csv")

mu_election %>% 
  ggplot(aes(x = election, y = median)) +
  geom_point() +
  facet_wrap(vars(topic)) 
  
mu_election %>% 
  filter(topic %in% c(12, 13, 17, 22, 23, 24, 29, 30, 32, 33, 35, 39)) %>%
  ggplot(aes(x = election, y = median)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  facet_wrap(vars(topic), nrow = 4, ncol = 3) +
  # facet_wrap(vars(topic)) +
  theme_minimal()

ggsave(
  "outputs/figures/mu_election.pdf",
  height = 10,
  width = 8,
  units = "in"
)


mu_election %>% 
  filter(topic %in% c(12, 13, 17, 22, 23, 24, 29, 30, 32, 33, 35, 39)) %>%
  ggplot(aes(x = election, y = median)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  facet_wrap(vars(topic), nrow = 4, ncol = 3) +
  # facet_wrap(vars(topic)) +
  theme_minimal() +
  theme(text = element_text(size = 20)) +
  theme(
    panel.background = element_rect(fill = "transparent") # bg of the panel
    , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
    , panel.grid.major = element_blank() # get rid of major grid
    , panel.grid.minor = element_blank() # get rid of minor grid
    , legend.background = element_rect(fill = "transparent") # get rid of legend bg
    , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  ) +
  labs(x = "Election",
       y = "Median")

ggsave(
  "outputs/figures/mu_election-slides.pdf",
  height = 8,
  width = 10,
  units = "in"
)







mu_gov <- read_csv("scripts/from_monica/results/mu_gov.csv")

mu_gov %>% 
  ggplot(aes(x = government, y = median)) +
  geom_point() +
  facet_wrap(vars(topic)) 

mu_gov %>% 
  filter(!government %in% c(19, 32)) %>%
  filter(topic %in% c(12, 13, 17, 22, 23, 24, 29, 30, 32, 33, 35, 39)) %>% 
  ggplot(aes(x = government, y = median)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  facet_wrap(vars(topic), nrow = 4, ncol = 3) +
  theme_minimal()

ggsave(
  "outputs/figures/mu_gov.pdf",
  height = 10,
  width = 8,
  units = "in"
)



mu_gov %>% 
  filter(!government %in% c(19, 32)) %>%
  filter(topic %in% c(12, 13, 17, 22, 23, 24, 29, 30, 32, 33, 35, 39)) %>% 
  ggplot(aes(x = government, y = median)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  facet_wrap(vars(topic), nrow = 3, ncol = 4) +
  theme_minimal() +
  theme(text = element_text(size = 20)) +
  theme(
    panel.background = element_rect(fill = "transparent") # bg of the panel
    , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
    , panel.grid.major = element_blank() # get rid of major grid
    , panel.grid.minor = element_blank() # get rid of minor grid
    , legend.background = element_rect(fill = "transparent") # get rid of legend bg
    , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  ) +
  labs(x = "Government",
       y = "Median")


ggsave(
  "outputs/figures/mu_gov-slides.pdf",
  height = 8,
  width = 10,
  units = "in"
)