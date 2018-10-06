# Just a quick chart as an example for the paper

library(lubridate)
library(tidyverse)

td_gamma <- read_csv("outputs/topic_models_and_gammas/gammas_model_80.csv")
dates <- read_csv("outputs/topic_models_and_gammas/dates.csv")

head(td_gamma)

# td_gamma$document %>% unique() %>% length()

td_gamma$date <- rep(dates$dates, 80)

set.seed(12345)
td_gamma <- td_gamma %>% 
  sample_frac(0.05) # reduce to 5 per cent of the dataset so that it doesn't take an age to load

td_gamma_reduced <- td_gamma %>% 
  filter(topic %in% c(17))

ggplot(data = td_gamma, aes(x = date, y = gamma, color = as.factor(topic))) +
  geom_point(color = "grey", alpha = 0.5) +
  geom_point(data = td_gamma_reduced) +
  theme_classic() +
  labs(x = "Date",
       y = "Gamma") +
  theme(legend.position="none") +
  scale_colour_viridis_d()

ggsave(
    "outputs/figures/topics_example.pdf",
     height = 6,
     width = 8,
     units = "in"
  )
