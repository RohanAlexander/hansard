library(tidyverse)
# update.packages()

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



all_differerences <- mu_election %>% 
  select(election, topic, median) %>% 
  mutate(previousElection = lag(median, 40)) %>% 
  mutate(diff = abs(median - previousElection))

mu_election %>% 
  select(election, topic, median) %>% 
  mutate(previousElection = lag(median, 40)) %>% 
  mutate(diff = abs(median - previousElection)) %>% 
  mutate(grouper = case_when(election %in% c(26) ~ "Menzies-Holt",
                             election %in% c(29) ~ "Whitlam",
                             election %in% c(39, 40, 41) ~ "Howard",
                             TRUE ~ "Other")) %>% 
  filter(election %in% c(26, 29, 39, 40, 41)) %>%
  ggplot(aes(x = topic, y = diff, color = as.factor(election))) +
  geom_point(data = all_differerences, aes(x = topic, y = diff), color = "grey90") +
  geom_point() +
  labs(y = "Absolute difference with previous", 
       x = "Topic") +
  coord_flip() +
  scale_color_viridis_d(name = "Election") +
  facet_wrap(vars(grouper)) +
  theme_minimal()
  
ggsave(
  "outputs/figures/mu_election-differences.pdf",
  height = 10,
  width = 8,
  units = "in"
)


# library(rmarkdown)
# draft("myslides.Rmd", template="metropolis", package="binb", edit=FALSE)
# setwd("myslides")  ## template creates a new subdir
# render("myslides.Rmd")



all_differerences <- mu_election %>% 
  select(election, topic, median) %>% 
  mutate(previousElection = lag(median, 40)) %>% 
  mutate(diff = abs(median - previousElection))

slides_data <- mu_election %>% 
  select(election, topic, median) %>% 
  mutate(previousElection = lag(median, 40)) %>% 
  mutate(diff = abs(median - previousElection)) %>% 
  mutate(grouper = case_when(election %in% c(26) ~ "Menzies-Holt",
                             election %in% c(29) ~ "Whitlam",
                             election %in% c(39, 40, 41) ~ "Howard",
                             TRUE ~ "Other")) %>% 
  filter(election %in% c(26, 29, 39, 40, 41))

slides_data %>% 
  filter(election %in% c(26)) %>% 
  ggplot(aes(x = topic, y = diff, color = as.factor(election))) +
  geom_point(data = all_differerences, aes(x = topic, y = diff), color = "grey90") +
  geom_point() +
  labs(y = "Absolute difference with previous", 
       x = "Topic") +
  scale_color_viridis_d(name = "Election") +
  theme_minimal()

ggsave(
  "outputs/figures/mu_election-differences-menziesholt.pdf",
  height = 8,
  width = 10,
  units = "in"
)



slides_data %>% 
  filter(election %in% c(29)) %>% 
  ggplot(aes(x = topic, y = diff, color = as.factor(election))) +
  geom_point(data = all_differerences, aes(x = topic, y = diff), color = "grey90") +
  geom_point() +
  labs(y = "Absolute difference with previous", 
       x = "Topic") +
  scale_color_viridis_d(name = "Election") +
  theme_minimal()

ggsave(
  "outputs/figures/mu_election-differences-whitlam.pdf",
  height = 8,
  width = 10,
  units = "in"
)



slides_data %>% 
  filter(election %in% c(26)) %>% 
  ggplot(aes(x = topic, y = diff, color = as.factor(election))) +
  geom_point(data = all_differerences, aes(x = topic, y = diff), color = "grey90") +
  geom_point() +
  labs(y = "Absolute difference with previous", 
       x = "Topic") +
  scale_color_viridis_d(name = "Election") +
  theme_minimal()

ggsave(
  "outputs/figures/mu_election-differences-menziesholt.pdf",
  height = 8,
  width = 10,
  units = "in"
)



slides_data %>% 
  filter(election %in% c(39, 40, 41)) %>% 
  ggplot(aes(x = topic, y = diff, color = as.factor(election))) +
  geom_point(data = all_differerences, aes(x = topic, y = diff), color = "grey90") +
  geom_point() +
  labs(y = "Absolute difference with previous", 
       x = "Topic") +
  scale_color_viridis_d(name = "Election") +
  theme_minimal()

ggsave(
  "outputs/figures/mu_election-differences-howard.pdf",
  height = 8,
  width = 10,
  units = "in"
)





slides_data %>% 
  filter(election %in% c(29)) %>% 
  ggplot(aes(x = topic, y = diff, color = as.factor(election))) +
  geom_point(data = all_differerences, aes(x = topic, y = diff), color = "grey90") +
  geom_point(size = 4) +
  labs(y = "Absolute difference with previous", 
       x = "Topic") +
  scale_color_viridis_d(name = "Election") +
  theme_minimal() +
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
  "outputs/figures/mu_election-differences-whitlam-slides.pdf",
  height = 8,
  width = 10,
  units = "in"
)



slides_data %>% 
  filter(election %in% c(26)) %>% 
  ggplot(aes(x = topic, y = diff, color = as.factor(election))) +
  geom_point(data = all_differerences, aes(x = topic, y = diff), color = "grey90") +
  geom_point(size = 4) +
  labs(y = "Absolute difference with previous", 
       x = "Topic") +
  scale_color_viridis_d(name = "Election") +
  theme_minimal() +
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
  "outputs/figures/mu_election-differences-menziesholt-slides.pdf",
  height = 8,
  width = 10,
  units = "in"
)



slides_data %>% 
  filter(election %in% c(39, 40, 41)) %>% 
  ggplot(aes(x = topic, y = diff, color = as.factor(election))) +
  geom_point(data = all_differerences, aes(x = topic, y = diff), color = "grey90") +
  geom_point(size = 4) +
  labs(y = "Absolute difference with previous", 
       x = "Topic") +
  scale_color_viridis_d(name = "Election") +
  theme_minimal() +
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
  "outputs/figures/mu_election-differences-howard-slides.pdf",
  height = 8,
  width = 10,
  units = "in"
)

