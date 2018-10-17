library(tidyverse)

sigma <- read_csv("scripts/from_monica/results/sigma.csv")

sigma %>% 
  filter(topic %in% c(4:15)) %>% 
  ggplot(aes(x = election, y = sigma_median)) +
  geom_point() +
  facet_wrap(vars(topic), nrow = 4, ncol = 3) +
  theme_minimal()

ggsave(
  "outputs/figures/sigma.pdf",
  height = 10,
  width = 8,
  units = "in"
)
