library(lubridate)
library(tidyverse)
library(ggrepel)

hansard_dates <- read_csv("inputs/misc/hansard_dates.csv")
election_dates <- read_csv("inputs/misc/misc_elections_data.csv") %>% filter(!is.na(electionDate))

hansard_dates$sitting <- 1

# hansard_dates_test <- hansard_dates[6000:7500,]
# hansard_dates$class <- c(rep(0, nrow(hansard_dates)/2), rep(1, nrow(hansard_dates)/2))

hansard_dates %>%
  ggplot(mapping = aes(x = hansardDates)) +
  geom_bar() +
  # geom_vline(
  #   data = election_dates,
  #   aes(xintercept = electionDate),
  #   linetype = "dashed",
  #   color = "grey50"
  # ) +
  theme_classic() +
  theme(axis.title.y = element_blank(), # GET RID OF THE Y AXIS LABELS AND TICKS
        axis.ticks.y =  element_blank()) +
  labs(x = "Date",
       title = "Sitting dates and election years",
       caption = "Data based on available Hansard records.") +
  theme_classic()

# Maybe split the data by 30 year panels?


hansard_dates_by_year <- hansard_dates %>%
  mutate(year = year(hansardDates)) %>%
  group_by(year) %>%
  summarise(countByYear = n())

write_csv(hansard_dates_by_year, "counts_by_year.csv")

ggsave(
  "outputs/figures/age_at_election.pdf",
  height = 8.27,
  width = 11.69,
  units = "in"
)





hansard_dates %>% 
  filter(year(hansardDates) <= 1925) %>%
  ggplot(mapping = aes(x = yday(hansardDates))) +
  geom_bar() +
  # facet_wrap(vars(year(hansardDates)), ncol = 10) +
  facet_wrap(vars(year(hansardDates)), ncol = 5) +
  theme_classic() +
  theme(axis.title.y = element_blank(),# GET RID OF THE Y AXIS LABELS
        axis.text.y=element_blank(),
        # axis.ticks.y = element_blank(), # GET RID OF THE Y AXIS TICKS
        axis.ticks.y =  element_blank()) +
  labs(x = "Day of year",
       # title = "Sitting dates and election years",
       caption = "Data based on Hansard PDF records available at https://parlinfo.aph.gov.au.")

# install.packages("ggrepel")
library(ggrepel)

test <- hansard_dates %>% 
  mutate(diff = hansardDates - lag(hansardDates, n = 8),
         hansardDatesSelect  = if_else(diff >= 200, hansardDates, as.Date(NA))) %>%
  filter(diff >= 200) %>% 
  ggplot(mapping = aes(x = diff)) +
  geom_bar() +
  geom_label(aes(y = 20, label = hansardDatesSelect))

  