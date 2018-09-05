library(ggrepel)
library(lubridate)
library(tidyverse)


td_gamma <- read_csv("outputs/td_gamma.csv")
misc_data <- read_csv("data/misc/key_dates.csv")
misc_elections_data <- read_csv("data/misc/misc_elections_data.csv")


td_gamma$document <- ymd(td_gamma$document)
td_gamma$topic %>% class()
misc_elections_data$electionDate <- dmy(misc_elections_data$electionDate)

# each_days_topics <- each_days_topics %>% 
#   arrange(document, topic) 

# Get rid of zeros in the graph if you want
# each_days_topics$gamma[each_days_topics$gamma < 0.01] <- NA


ggplot() +
  geom_point(data = td_gamma, mapping = aes(x = document, y = gamma, colour = topic, group = topic)) +
  geom_smooth(data = td_gamma, mapping = aes(x = document, y = gamma, colour = topic, group = topic), se = FALSE) +
  # geom_vline(data = misc_elections_data, aes(xintercept = electionDate), linetype = "dashed") +
  geom_vline(data = misc_data, aes(xintercept = theDate), linetype = "dashed") + 
  ?geom_text_repel(data = misc_data, aes(x = theDate, y = 1, label = event)) +
  # geom_line() +
  theme_classic() +
  scale_colour_viridis_c()
