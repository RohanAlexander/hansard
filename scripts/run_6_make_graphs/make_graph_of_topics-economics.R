# Just a quick chart as an example for the paper

library(lubridate)
library(tidyverse)

td_gamma <- read_csv("outputs/results-topic_models_and_gammas/gammas_model_80.csv")
dates <- read_csv("outputs/results-topic_models_and_gammas/dates.csv")

head(td_gamma)

td_gamma <- td_gamma %>% 
  separate(document, into = c("chamber", "date"), sep = "-", extra = "merge")

td_gamma$chamber[td_gamma$chamber == "hor"] <- "House of Representatives"
td_gamma$chamber[td_gamma$chamber == "senate"] <- "Senate"


td_gamma$date <- ymd(td_gamma$date)

# td_gamma$document %>% unique() %>% length()

# td_gamma$date <- rep(dates$dates, 60)

# set.seed(12345)
# td_gamma <- td_gamma %>% 
#   sample_frac(0.05) # reduce to 5 per cent of the dataset so that it doesn't take an age to load


# For paper
td_gamma_reduced <- td_gamma %>% 
  filter(topic %in% c(17, 22, 25, 44, 51, 55, 59, 71, 72, 74, 76, 77))

ggplot(data = td_gamma, aes(x = date, y = gamma, color = as.factor(topic))) +
  # geom_point(color = "grey", alpha = 0.5, size = 0.5) +
  # geom_point(data = td_gamma_reduced, size = 0.5) +
  geom_smooth(data = td_gamma_reduced, method = loess) +
  theme_minimal() +
  labs(x = "Date",
       y = "Gamma") +
  facet_wrap(vars(chamber), nrow = 2) +
  # theme(legend.position="none") +
  scale_colour_viridis_d(name = "Topics")

ggsave(
    "outputs/figures/topics_example.pdf",
     height = 8,
     width = 6,
     units = "in"
  )


# For slides
td_gamma_reduced <- td_gamma %>% 
  filter(topic %in% c(4, 26, 28, 30, 66))

ggplot(data = td_gamma, aes(x = date, y = gamma, color = as.factor(topic))) +
  geom_point(color = "grey", alpha = 0.5) +
  geom_point(data = td_gamma_reduced, size = 4) +
  theme_classic() +
  labs(x = "Date",
       y = "Gamma") +
  theme(legend.position="none") +
  # facet_wrap(vars(topic)) +
  scale_colour_viridis_d() + 
  theme(text = element_text(size = 20)) +
  theme(
    panel.background = element_rect(fill = "transparent") # bg of the panel
    , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
    , panel.grid.major = element_blank() # get rid of major grid
    , panel.grid.minor = element_blank() # get rid of minor grid
    , legend.background = element_rect(fill = "transparent") # get rid of legend bg
    , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )

ggsave(
  "outputs/figures/topics_example-slides.pdf",
  height = 6,
  width = 8,
  units = "in"
)
