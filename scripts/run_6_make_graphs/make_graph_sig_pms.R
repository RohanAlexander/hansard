library(tidyverse)
library(lubridate)


pms <- read_csv("inputs/misc/change_of_pm.csv")
sig_pms <- read_csv("inputs/misc/test_sig.csv")

elections <- read_csv("inputs/misc/misc_elections_data.csv")
sig_elections <- read_csv("inputs/misc/test_elections_sig.csv")

elections <- elections %>% 
  left_join(sig_elections)

pms$end <- ymd(pms$end)

pms <- pms %>% 
  left_join(sig_pms)

pms_sig <- pms %>% filter(sig == 1)

elections <- elections %>% filter(!is.na(electionDate))
elections_sig <- elections %>% filter(sig == 1)

all <- tibble(dates = elections$electionDate, type = "Elections")
all <- rbind(all, tibble(dates = pms$start, type = "PMs"))

sig <- tibble(dates = elections_sig$electionDate, type = "Elections")
sig <- rbind(sig, tibble(dates = pms_sig$start, type = "PMs"))

ggplot() +
  geom_point(data = all, aes(x = dates, y = 0.5), alpha = 0.1, size = 6) + 
  geom_point(data = sig, aes(x = dates, y = 0.5), alpha = 1, size = 6) + 
  facet_wrap(vars(type), nrow = 2) +
  # geom_point(data = pms_sig, aes(x = end, y = 0.75)) +
  # geom_point(data = elections, aes(x = electionDate, y = 0.25), alpha = 0.1) + 
  # geom_point(data = elections_sig, aes(x = electionDate, y = 0.25)) + 
  # ylim(0,1) +
  theme_bw() +
  theme_classic() +
  theme(axis.title.y=element_blank(),
        axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        text = element_text(size=48))

ggsave("outputs/figures/sig.pdf", height = 210, width = 297, units = "mm")