#### Preamble ####
# Purpose: Create graphs and tables that summarise the Hansard data for the Appendix by election and PM
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 8 December 2018
# Prerequisites:
# Issues:


#### Workspace set-up ####
# library(ggrepel)
library(lubridate)
# library(stringr)
library(tidyverse)

# Get the names of the PDFs
# Senate
senate_dates <-
  list.files(path = "/Volumes/Hansard/pdfs/federal/senate") %>%
  str_replace(".pdf", "") %>%
  ymd()
senate_dates <- tibble(pdf_date = senate_dates, chamber = "senate")
# Reps
reps_dates <-
  list.files(path = "/Volumes/Hansard/pdfs/federal/hor") %>%
  str_replace(".pdf", "") %>%
  ymd()
reps_dates <- tibble(pdf_date = reps_dates, chamber = "hor")

hansard_dates <- rbind(reps_dates, senate_dates)
rm(reps_dates, senate_dates)

# Get the PM periods
pm_dates <- read_csv("inputs/misc/change_of_pm.csv")

# hansard_dates <- read_csv("inputs/misc/hansard_dates.csv")


sitting_days_counts_by_gov <- hansard_dates %>%
  mutate(
    government =  case_when(
      pdf_date < "1903-09-24" ~ "Barton",
      pdf_date < "1904-04-27" ~ "Deakin 1",
      pdf_date < "1904-08-18" ~ "Watson",
      pdf_date < "1905-07-05" ~ "Reid",
      pdf_date < "1908-11-13" ~ "Deakin 2",
      pdf_date < "1909-06-02" ~ "Fisher 1",
      pdf_date < "1910-04-29" ~ "Deakin 3",
      pdf_date < "1913-06-24" ~ "Fisher 2",
      pdf_date < "1914-09-17" ~ "Cook",
      pdf_date < "1915-10-27" ~ "Fisher 3",
      pdf_date < "1923-02-09" ~ "Hughes",
      pdf_date < "1929-10-22" ~ "Bruce",
      pdf_date < "1932-01-06" ~ "Scullin",
      pdf_date < "1939-04-07" ~ "Lyons",
      pdf_date < "1939-04-26" ~ "Page",
      pdf_date < "1941-08-28" ~ "Menzies 1",
      pdf_date < "1941-10-07" ~ "Fadden",
      pdf_date < "1945-07-05" ~ "Curtin",
      pdf_date < "1945-07-13" ~ "Forde",
      pdf_date < "1949-12-19" ~ "Chifley",
      pdf_date < "1966-01-26" ~ "Menzies 2",
      pdf_date < "1967-12-19" ~ "Holt",
      pdf_date < "1968-01-10" ~ "McEwen",
      pdf_date < "1971-03-10" ~ "Gorton",
      pdf_date < "1972-12-05" ~ "McMahon",
      pdf_date < "1975-11-11" ~ "Whitlam",
      pdf_date < "1983-03-11" ~ "Fraser",
      pdf_date < "1991-12-20" ~ "Hawke",
      pdf_date < "1996-03-11" ~ "Keating",
      pdf_date < "2007-12-03" ~ "Howard",
      pdf_date < "2010-06-24" ~ "Rudd 1",
      pdf_date < "2013-06-27" ~ "Gillard",
      pdf_date < "2013-09-18" ~ "Rudd 2",
      pdf_date < "2015-09-15" ~ "Abbott",
      pdf_date < "2018-08-24" ~ "Turnbull",
      TRUE ~ "Morrison"
    )
  ) %>% 
  count(government, chamber)

sitting_days_counts_by_gov <- sitting_days_counts_by_gov %>% 
  spread(chamber, n)

pm_dates <- 
  pm_dates %>% 
  left_join(sitting_days_counts_by_gov) %>% 
  replace_na(list(hor = 0, senate = 0))

rm(sitting_days_counts_by_gov)

write_csv(pm_dates, "pm_dates.csv")



# Get the elections
elections_dates <- read_csv("inputs/misc/misc_elections_data.csv") %>% 
  filter(election == 1)

sitting_days_counts_by_election <- hansard_dates %>%
  mutate(
    election =  case_when(
pdf_date < "1903-12-16" ~ 1,
pdf_date < "1906-12-12" ~ 2,
pdf_date < "1910-04-13" ~ 3,
pdf_date < "1913-05-31" ~ 4,
pdf_date < "1914-09-05" ~ 5,
pdf_date < "1917-05-05" ~ 6,
pdf_date < "1919-12-13" ~ 7,
pdf_date < "1922-12-16" ~ 8,
pdf_date < "1925-11-14" ~ 9,
pdf_date < "1928-11-17" ~ 10,
pdf_date < "1929-10-12" ~ 11,
pdf_date < "1931-12-19" ~ 12,
pdf_date < "1934-09-15" ~ 13,
pdf_date < "1937-10-23" ~ 14,
pdf_date < "1940-09-21" ~ 15,
pdf_date < "1943-08-21" ~ 16,
pdf_date < "1946-09-28" ~ 17,
pdf_date < "1949-12-10" ~ 18,
pdf_date < "1951-08-28" ~ 19,
pdf_date < "1954-05-29" ~ 20,
pdf_date < "1955-12-10" ~ 21,
pdf_date < "1958-11-22" ~ 22,
pdf_date < "1961-12-09" ~ 23,
pdf_date < "1963-11-30" ~ 24,
pdf_date < "1966-11-26" ~ 25,
pdf_date < "1969-10-25" ~ 26,
pdf_date < "1972-12-02" ~ 27,
pdf_date < "1974-05-18" ~ 28,
pdf_date < "1975-12-13" ~ 29,
pdf_date < "1977-12-10" ~ 30,
pdf_date < "1980-10-18" ~ 31,
pdf_date < "1983-03-05" ~ 32,
pdf_date < "1984-12-01" ~ 33,
pdf_date < "1987-07-11" ~ 34,
pdf_date < "1990-03-24" ~ 35,
pdf_date < "1993-03-13" ~ 36,
pdf_date < "1996-03-02" ~ 37,
pdf_date < "1998-10-03" ~ 38,
pdf_date < "2001-11-10" ~ 39,
pdf_date < "2004-10-09" ~ 40,
pdf_date < "2007-11-24" ~ 41,
pdf_date < "2010-08-21" ~ 42,
pdf_date < "2013-09-07" ~ 43,
pdf_date < "2016-07-02" ~ 44,
TRUE ~ 45
)
) %>% 
  count(election, chamber)

sitting_days_counts_by_election <- sitting_days_counts_by_election %>% 
  spread(chamber, n)

write_csv(sitting_days_counts_by_election, "sitting_days_elections.csv")







#### Graphs ####
# Graph of counts by year
counts_by_year %>%
  group_by(year) %>%
  select(year, house_number, senate_number) %>%
  rename(House = house_number,
         Senate = senate_number,
         Year = year) %>%
  gather(key = "Chamber", value = "Number", House:Senate) %>%
  ungroup() %>%
  ggplot(aes(
    x = Year,
    y = Number,
    fill = Chamber,
    group = Chamber
  )) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_classic() +
  scale_fill_viridis_d()

ggsave(
  "outputs/figures/counts_by_year.pdf",
  height = 6,
  width = 8,
  units = "in"
)

# Graph of difference with the parliament website claim
counts_by_year %>%
  group_by(year) %>%
  select(year, rep_diff, senate_diff) %>%
  rename(House = rep_diff,
         Senate = senate_diff,
         Year = year) %>%
  gather(key = "Chamber", value = "Difference", House:Senate) %>%
  ungroup() %>%
  mutate(label_the_year = ifelse(abs(Difference) > 1, Year, NA)) %>%
  ggplot(aes(x = Year, y = Difference)) +
  geom_point() +
  geom_label_repel(aes(label = label_the_year)) +
  facet_wrap(vars(Chamber), nrow = 2) +
  theme_classic() +
  scale_fill_viridis_d() +
  theme(text = element_text(size = 12))

ggsave(
  "outputs/figures/differences_by_year.pdf",
  height = 6,
  width = 8,
  units = "in"
)
