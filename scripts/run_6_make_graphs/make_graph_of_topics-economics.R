# Just a quick chart as an example for the paper

library(lubridate)
library(tidyverse)

td_gamma <- read_csv("outputs/results-topic_models_and_gammas/gammas_model_80.csv")
# dates <- read_csv("outputs/results-topic_models_and_gammas/dates.csv")

head(td_gamma)

td_gamma <- td_gamma %>%
  separate(document, into = c("chamber", "date"), sep = "-", extra = "merge")

td_gamma$chamber[td_gamma$chamber == "hor"] <- "House of Representatives"
td_gamma$chamber[td_gamma$chamber == "senate"] <- "Senate"

td_gamma$date <- ymd(td_gamma$date)


# td_gamma$date %>% unique() %>% length()

# td_gamma$date <- rep(dates$dates, 60)

# set.seed(12345)
# td_gamma <- td_gamma %>% 
#   sample_frac(0.05) # reduce to 5 per cent of the dataset so that it doesn't take an age to load


# Great Depression
td_gamma_reduced <- td_gamma %>% 
  filter(topic %in% c(17, 22, 25, 44, 51, 55, 59, 71, 72, 74, 76, 77)) 
# %>% 
#   filter(topic %in% c(51, 55, 59))

td_gamma_reduced <- td_gamma_reduced %>% 
  filter(date >= "1928-01-01") %>% 
  filter(date <= "1932-12-31") 

labels <- tibble(
  date = c(ymd("1929-10-29"), ymd("1931-06-11")),
  label_with_this = c("Black Tuesday", "Premiers' Plan"),
  height = c(0.65, 0.60)
)


ggplot(data = td_gamma_reduced, 
       aes(x = date, 
           y = gamma, 
           color = as.factor(topic))
       ) +
  geom_jitter(size = 0.5) +
  geom_smooth() +
  geom_vline(data = labels, aes(xintercept = date), linetype = "dashed") +
  geom_label(data = labels, aes(x = date, y = height, label = label_with_this, color = "black"), color = "black") +
  theme_classic() +
  labs(x = "Date",
       y = "Proportion") +
  facet_wrap(vars(chamber), ncol = 2) +
  # theme(legend.position="none") +
  scale_colour_viridis_d(name = "Topics")

ggsave(
    "outputs/figures/topics_example-GreatD.pdf",
     height = 6,
     width = 8,
     units = "in"
  )





# Financial criss

# For paper
td_gamma_reduced <- td_gamma %>% 
  filter(topic %in% c(17, 22, 25, 44, 51, 55, 59, 71, 72, 74, 76, 77)) 
# %>% 
#   filter(topic %in% c(51, 55, 59))

td_gamma_reduced <- td_gamma_reduced %>% 
  filter(date >= "2007-01-01") %>% 
  filter(date <= "2010-12-31") 

labels <- tibble(
  date = c(ymd("2008-09-15")),
  label_with_this = c("Lehman Brothers"),
  height = c(0.60)
)



ggplot(data = td_gamma_reduced, 
       aes(x = date, 
           y = gamma, 
           color = as.factor(topic))
) +
  geom_jitter(size = 0.5) +
  geom_smooth() +
  geom_vline(data = labels, aes(xintercept = date), linetype = "dashed") +
  geom_label(data = labels, aes(x = date, y = height, label = label_with_this, color = "black"), color = "black") +
  theme_classic() +
  labs(x = "Date",
       y = "Proportion") +
  facet_wrap(vars(chamber), ncol = 2) +
  # theme(legend.position="none") +
  scale_colour_viridis_d(name = "Topics")

ggsave(
  "outputs/figures/topics_example-GFC.pdf",
  height = 6,
  width = 8,
  units = "in"
)




# 80s

# For paper
td_gamma_reduced <- td_gamma %>% 
  filter(topic %in% c(17, 22, 25, 44, 51, 55, 59, 71, 72, 74, 76, 77)) 
# %>%
#   filter(topic %in% c(76, 77))

td_gamma_reduced <- td_gamma_reduced %>% 
  filter(date >= "1981-01-01") %>% 
  filter(date <= "2000-12-31") 

labels <- tibble(
  date = c(ymd("1983-12-12", "1987-10-20")),
  label_with_this = c("Float dollar", "Black Tuesday"),
  height = c(0.50, 0.45)
)


ggplot(data = td_gamma_reduced, 
       aes(x = date, 
           y = gamma, 
           color = as.factor(topic))
) +
  geom_jitter(size = 0.5) +
  geom_smooth() +
  geom_vline(data = labels, aes(xintercept = date), linetype = "dashed") +
  geom_label(data = labels, aes(x = date, y = height, label = label_with_this, color = "black"), color = "black") +
  theme_classic() +
  labs(x = "Date",
       y = "Proportion") +
  facet_wrap(vars(chamber), ncol = 2) +
  # theme(legend.position="none") +
  scale_colour_viridis_d(name = "Topics")

ggsave(
  "outputs/figures/topics_example-the80s.pdf",
  height = 6,
  width = 8,
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
