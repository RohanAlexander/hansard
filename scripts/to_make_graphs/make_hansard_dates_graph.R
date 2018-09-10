library(lubridate)
library(tidyverse)
library(ggrepel)

hansard_dates <- read_csv("inputs/misc/hansard_dates.csv")
election_dates <- read_csv("inputs/misc/misc_elections_data.csv") %>% filter(!is.na(electionDate))

hansard_dates$sitting <- 1

hansard_dates_test <- hansard_dates[6000:7500,]
hansard_dates$class <- c(rep(0, nrow(hansard_dates)/2), rep(1, nrow(hansard_dates)/2))


ggplot(data = hansard_dates,
       mapping = aes(x = hansardDates)) +
  geom_bar() +
  geom_vline(
    data = election_dates,
    aes(xintercept = electionDate),
    linetype = "dashed",
    color = "grey50"
  ) +
  theme_classic() +
  theme(axis.title.y = element_blank(), # GET RID OF THE Y AXIS LABELS AND TICKS
        axis.ticks.y =  element_blank()) +
  labs(x = "Date",
       title = "Sitting dates and election years",
       caption = "Data based on available Hansard records.") +
  theme_classic() 

# Maybe split the data by 30 year panels?





  

 
  



ggsave(
  "outputs/figures/age_at_election.pdf",
  height = 8.27,
  width = 11.69,
  units = "in"
)
