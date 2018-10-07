library(ggplot2)
library(tidyverse)

# Read data
mu_res <- read_csv("scripts/from_monica/governments/mu_res.csv")
sigma_res <- read_csv("scripts/from_monica/governments/sigma_res.csv")
alpha_res <- read_csv("scripts/from_monica/governments/alpha_res.csv")


# Make changes to data


# Make graphs
head(mu_res)
mu_res$government %>% unique()
mu_res %>%
  filter(topic %in% 1:20) %>%
  ggplot(aes(government, median, color = factor(government))) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  facet_wrap(vars(topic)) +
  theme(legend.position="none")

ggsave(
  "outputs/figures/mu_topic1-20.pdf",
  height = 6,
  width = 8,
  units = "in"
)

mu_res %>%
  filter(topic %in% 21:40) %>%
  ggplot(aes(government, median, color = factor(government))) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  facet_wrap(vars(topic)) +
  theme(legend.position="none")

ggsave(
  "outputs/figures/mu_topic21-40.pdf",
  height = 6,
  width = 8,
  units = "in"
)

mu_res %>%
  filter(topic %in% 41:60) %>%
  ggplot(aes(government, median, color = factor(government))) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  facet_wrap(vars(topic)) +
  theme(legend.position="none")

ggsave(
  "outputs/figures/mu_topic41-60.pdf",
  height = 6,
  width = 8,
  units = "in"
)




head(sigma_res)
sigma_res %>% 
  filter(median<0.3) %>% 
  ggplot(aes(government, median)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = lower, ymax = upper))

head(alpha_res)
alpha_res %>% 
  filter(median<0.3) %>% 
  ggplot(aes(government, median)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = lower, ymax = upper))




ggsave(
  "outputs/figures/topics_example.pdf",
  height = 6,
  width = 8,
  units = "in"
)