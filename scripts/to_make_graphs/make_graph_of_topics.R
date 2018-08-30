library(lubridate)
library(tidyverse)

load("outputs/each_days_topics.Rda")

class(each_days_topics$document)
each_days_topics$document <- ymd(each_days_topics$document)

each_days_topics <- each_days_topics %>% 
  arrange(document, topic) 

# Get rid of zeros in the graph if you want
# each_days_topics$gamma[each_days_topics$gamma < 0.01] <- NA

ggplot(data = each_days_topics, mapping = aes(x = document, y = gamma, colour = topic, group = topic)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  scale_colour_viridis_c()
