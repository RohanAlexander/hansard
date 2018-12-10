# Just a quick chart as an example for the paper

library(lubridate)
library(tidyverse)

td_gamma <- read_csv("outputs/results-topic_models_and_gammas/gammas_model_80.csv")
dates <- read_csv("outputs/results-topic_models_and_gammas/dates.csv")
elections <- read_csv("inputs/misc/misc_elections_data.csv") %>% 
  filter(election == 1)
pms <- read_csv("inputs/misc/change_of_pm.csv") 


td_gamma <- td_gamma %>% 
  separate(document, into = c("chamber", "date"), sep = "-", extra = "merge")

td_gamma$chamber[td_gamma$chamber == "hor"] <- "House of Representatives"
td_gamma$chamber[td_gamma$chamber == "senate"] <- "Senate"

td_gamma$date <- ymd(td_gamma$date)

td_gamma <- td_gamma %>% 
  filter(chamber == "House of Representatives")


# For paper
td_gamma_reduced <- td_gamma %>% 
  mutate(
    example = case_when(
             between(date, ymd("1958-01-01"), ymd("1959-12-31")) ~ "1. Menzies",
             between(date, ymd("1982-01-01"), ymd("1983-12-31")) ~ "2. Fraser, Hawke",
             between(date, ymd("2007-09-01"), ymd("2010-10-01")) ~ "3. Rudd",
             TRUE ~ "0"
           )
         ) %>% 
  filter(example != "0") %>% 
  filter(chamber == "House of Representatives")

ggplot() +
  geom_point(data = td_gamma_reduced, 
             size = 0.5, 
             aes(x = date, 
                 y = gamma)) +
  geom_vline(xintercept = elections$electionDate, linetype = "dashed") +
  geom_vline(xintercept = pms$start, linetype = "dotted") +
  theme_minimal() +
  labs(x = "Date",
       y = "Topic share") +
  facet_wrap(vars(example), nrow = 3, scales = "free_x") +
  theme_classic()

  
ggsave(
    "outputs/figures/topics_examples_three.pdf",
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
