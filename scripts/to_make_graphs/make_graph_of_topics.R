library(ggrepel)
library(lubridate)
library(tidyverse)


td_gamma <- read_csv("outputs/td_gamma.csv")
misc_data <- read_csv("data/misc/key_dates_reduced.csv")
misc_elections_data <- read_csv("data/misc/misc_elections_data.csv")
migration_data <- read_csv("data/non-political_data/migration.csv")

td_gamma <- td_gamma %>% arrange(topic, document, gamma)
td_gamma <- td_gamma %>% 
  mutate(type = 1)

# td_gamma$document <- ymd(td_gamma$document)
td_gamma$topic %>% class()
misc_elections_data$electionDate <- dmy(misc_elections_data$electionDate)
misc_data$year <- year(misc_data$theDate)
# migration_data$Year <- y(migration_data$Year)

migration_data <- migration_data %>% 
  mutate(document = Year, 
         topic = NA,
         gamma = NOM, 
         type = 2
         ) %>% 
  select(document, topic, gamma, type)


# each_days_topics <- each_days_topics %>% 
#   arrange(document, topic) 

# Get rid of zeros in the graph if you want
# each_days_topics$gamma[each_days_topics$gamma < 0.01] <- NA


ggplot() +
  # geom_point(data = td_gamma, mapping = aes(x = document, y = gamma, colour = topic, group = topic)) +
  geom_smooth(data = td_gamma, mapping = aes(x = document, y = gamma, colour = factor(topic), group = factor(topic), label = factor(topic)), se = FALSE) +
  # geom_vline(data = misc_elections_data, aes(xintercept = electionDate), linetype = "dashed") +
  geom_vline(data = misc_data, aes(xintercept = year), linetype = "dashed") +
  geom_text_repel(data = misc_data, aes(x = year, y = 0.45, label = event)) +
  geom_label() + 
  # geom_text() +
  # geom_line() +
  theme_classic() +
  theme(legend.position="none") +
  # scale_colour_viridis_d() +
  coord_cartesian(ylim = c(0, 0.5)) 




##
ggplot() +
  # geom_point(data = td_gamma, mapping = aes(x = document, y = gamma, colour = topic, group = topic)) +
  geom_smooth(data = td_gamma, mapping = aes(x = document, y = gamma, group = factor(topic), label = factor(topic)), colour = "grey90", se = FALSE) +
  # geom_vline(data = misc_elections_data, aes(xintercept = electionDate), linetype = "dashed") +
  geom_vline(data = misc_data, aes(xintercept = year), linetype = "dashed", color = "grey50") +
  geom_text_repel(data = misc_data, aes(x = year, y = 0.40, label = event), nudge_x = 5, size = 5) +
  geom_smooth(data = td_gamma[td_gamma$topic %in% c(6, 14, 16, 17, 20),], mapping = aes(x = document, y = gamma, colour = factor(topic), group = factor(topic), label = factor(topic)), se = FALSE, size = 2) +
  # geom_text() +
  # geom_line() +
  theme_classic() +
  guides(color = guide_legend(title = "Topic")) +
  theme(    text = element_text(size=20),
    axis.title.y = element_blank(),
            axis.title.x = element_blank()
  ) +
  scale_colour_viridis_d() +
  coord_cartesian(ylim = c(0, 0.4)) 

##




both_data <- rbind(td_gamma, migration_data)


ggplot() +
  geom_col(data = both_data[both_data$type == 2, ], mapping = aes(x = document, y = gamma)) +
  theme_classic() +
  theme(
    strip.text = element_text(size = 20),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    # axis.title.y = element_blank(),
    # axis.text.y = element_blank()
    # axis.ticks.y = element_blank(),
    # axis.title.x = element_blank(),
    # axis.text.x = element_blank(),
    # axis.ticks.x = element_blank(),
    # text = element_text(size = 20)
  ) +
  # labs(x = NULL, y = "tf-idf", title = "Highest tf-idf words in Hansard") +
  labs(x = NULL, y = "Net overseas migration")
