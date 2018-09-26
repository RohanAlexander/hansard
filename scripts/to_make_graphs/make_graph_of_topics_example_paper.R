# library(ggrepel)
library(lubridate)
library(tidyverse)

td_gamma <- read_csv("outputs/big_files_do_not_push/gammas_model_prevalence_is_spline.csv")

head(td_gamma)

td_gamma_test <- td_gamma %>% 
  mutate(decade = case_when(
    document < 1909-12-31 ~ "1900s",
    document < 1919-12-31 ~ "1910s",
    document < 1929-12-31 ~ "1920s",
    document < 1939-12-31 ~ "1930s",
    document < 1949-12-31 ~ "1940s",
    document < 1959-12-31 ~ "1950s",
    document < 1969-12-31 ~ "1960s",
    document < 1979-12-31 ~ "1970s",
    document < 1989-12-31 ~ "1980s",
    document < 1999-12-31 ~ "1990s",
    document < 2009-12-31 ~ "2000s",
    TRUE ~ "2010s"
  )) %>% 
  group_by(decade, topic) %>% 
  summarise(ave_gamma = mean(gamma))

td_gamma_test <- td_gamma %>% 
  mutate(topic = case_when(
    topic <= 10 ~ 1,
    topic <= 20 ~ 2,
    topic <= 30 ~ 3,
    topic <= 40 ~ 4,
    topic <= 50 ~ 5,
    topic <= 60 ~ 6,
    topic <= 70 ~ 7,
    topic <= 80 ~ 8,
    topic <= 90 ~ 9,
    TRUE ~ 10
  )) %>% 
  group_by(document, topic) %>% 
  summarise(ave_gamma = sum(gamma))

td_gamma_test[1:20,]

td_gamma_test_reduced <- td_gamma_test %>% 
  filter(topic %in% c(3, 4, 10))

ggplot(data = td_gamma_test, aes(x = document, y = ave_gamma, color = as.factor(topic))) +
  geom_point(color = "grey", alpha = 0.5) +
  geom_point(data = td_gamma_test_reduced) +
  theme_classic() +
  labs(x = "Date",
       y = "Gamma") +
  scale_colour_viridis_d()

ggsave(
    "outputs/figures/topics_example.pdf",
     height = 6,
     width = 8,
     units = "in"
  )
