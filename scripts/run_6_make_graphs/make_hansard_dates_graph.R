# Load the libraries
library(lubridate)
library(tidyverse)
library(ggrepel)

# Read in the data
hansard_dates <- read_csv("inputs/misc/hansard_dates.csv")
election_dates <- read_csv("inputs/misc/misc_elections_data.csv") %>% filter(!is.na(electionDate))

hansard_dates$chamber[hansard_dates$chamber == "HoR"] <- "House of Representatives"

# Barcode plots
## All dates
hansard_dates %>%
  ggplot(mapping = aes(x = hansardDates)) +
  geom_bar() +
  facet_wrap(vars(chamber), nrow = 2) +
  theme_classic() +
  theme(axis.title.y = element_blank(), # Remove y axis title
        axis.ticks.y =  element_blank(), # Remove y axis ticks
        axis.text.y =  element_blank()) +
  labs(x = "Date",
       caption = "Data based on available Hansard records.") 

## 1920 and before
hansard_dates %>%
  filter(year(hansardDates) < 1920) %>% 
  ggplot(mapping = aes(x = hansardDates)) +
  geom_bar() +
  facet_wrap(vars(chamber), nrow = 2) +
  theme_classic() +
  theme(axis.title.y = element_blank(), # Remove y axis title
        axis.ticks.y =  element_blank(), # Remove y axis ticks
        axis.text.y =  element_blank()) +
  labs(x = "Year"
       # title = "Sitting dates and election years",
       # caption = "Data based on available Hansard records."
       ) 

ggsave(
  "outputs/figures/sitting_dates_before_1920.pdf",
  height = 4,
  width = 6,
  units = "in"
)

## 2000 and after
hansard_dates %>%
  filter(year(hansardDates) > 1999) %>% 
  ggplot(mapping = aes(x = hansardDates)) +
  geom_bar() +
  facet_wrap(vars(chamber), nrow = 2) +
  theme_classic() +
  theme(axis.title.y = element_blank(), # Remove y axis title
        axis.ticks.y =  element_blank(), # Remove y axis ticks
        axis.text.y =  element_blank()) +
  labs(x = "Year"
       # title = "Sitting dates and election years",
       # caption = "Data based on available Hansard records."
       ) 

ggsave(
  "outputs/figures/sitting_dates_from_2000.pdf",
  height = 4,
  width = 6,
  units = "in"
)


# 
# hansard_dates %>%
#   filter(year(hansardDates) <= 1925) %>%
#   ggplot(mapping = aes(x = yday(hansardDates))) +
#   geom_bar() +
#   facet_wrap(vars(year(hansardDates)), ncol = 5) +
#   # facet_grid(vars(chamber), vars(year(hansardDates))) +
#   theme_classic() +
#   theme(
#     axis.title.y = element_blank(), # GET RID OF THE Y AXIS LABELS
#     axis.text.y = element_blank(), # axis.ticks.y = element_blank(), # GET RID OF THE Y AXIS TICKS
#     axis.ticks.y =  element_blank()
#   ) +
#   labs(x = "Day of year",
#        # title = "Sitting dates and election years",
#        caption = "Data based on Hansard PDF records available at https://parlinfo.aph.gov.au.")
# 


hansard_dates <- hansard_dates %>% 
  mutate(
    sitting = case_when(
      chamber == "House of Representatives" ~ 1,
      chamber == "Senate" ~ 0.5,
      TRUE ~ 0
    )
  )


hansard_dates %>%
  filter(year(hansardDates) <= 1940) %>%
  ggplot(mapping = aes(x = yday(hansardDates))) +
  # geom_bar() +
  geom_point(aes(y = sitting, color = chamber)) +
  # geom_step(direction = "hv") +
  # ?facet_wrap(vars(year(hansardDates), chamber), ncol = 10) +
  # facet_wrap(vars(year(hansardDates), chamber), labeller = "label_both") +
  facet_wrap(vars(year(hansardDates)), ncol = 5) +
  # facet_grid(vars(chamber), vars(year(hansardDates))) +
  theme_classic() +
  theme(
    axis.title.y = element_blank(), # GET RID OF THE Y AXIS LABELS
    axis.text.y = element_blank(), # axis.ticks.y = element_blank(), # GET RID OF THE Y AXIS TICKS
    axis.ticks.y =  element_blank()
  ) +
  ylim(0.25,1.25) +
  labs(x = "Day of year",
       # title = "Sitting dates and election years",
       caption = "Data based on Hansard PDF records available at https://parlinfo.aph.gov.au.") +
  scale_color_manual(values = c("green", "red"))





test <- hansard_dates %>% 
  group_by(chamber) %>% 
  mutate(diff = hansardDates - lag(hansardDates, n = 8)) %>%
  count(diff)


  ggplot(mapping = aes(x = diff)) + 
  geom_bar() +
  scale_y_continuous(trans = "log")

