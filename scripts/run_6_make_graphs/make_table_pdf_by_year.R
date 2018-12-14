#### Preamble ####
# Purpose: Create graphs and tables that summarise the Hansard data for the Appendix
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 5 November 2018
# Prerequisites:
# Issues:


#### Workspace set-up ####
library(ggrepel)
library(lubridate)
library(stringr)
library(tidyverse)

# Get the names of the PDFs
# Senate
senate_dates <- list.files(path = "/Volumes/Hansard/pdfs/federal/senate") %>% 
  str_replace(".pdf", "") %>% 
  ymd() 
senate_dates <- tibble(pdf_date = senate_dates)
# Reps
reps_dates <- list.files(path = "/Volumes/Hansard/pdfs/federal/hor") %>% 
  str_replace(".pdf", "") %>% 
  ymd()
reps_dates <- tibble(pdf_date = reps_dates)


#### Create the dataset ####
# Make the counts by each year for the PDFs that we have
counts_by_year_senate <- senate_dates %>% 
  group_by(year(pdf_date)) %>% 
  count()
  
names(counts_by_year_senate) <- c("year", "senate_number")

counts_by_year_reps <- reps_dates %>% 
  group_by(year(pdf_date)) %>% 
  count() 

names(counts_by_year_reps) <- c("year", "house_number")

counts_by_year <- counts_by_year_reps %>% 
  left_join(counts_by_year_senate)

rm(counts_by_year_senate, counts_by_year_reps)

# Compare that to the counts that were on the parliament website at 
# https://www.aph.gov.au/Parliamentary_Business/Statistics/Senate_StatsNet/General/sittingdaysyear
counts_from_website <- read_csv("inputs/misc/number_of_sitting_days.csv")

counts_by_year <- counts_by_year %>% 
  left_join(counts_from_website)

counts_by_year <- counts_by_year %>% 
  mutate(rep_diff = reps - house_number, 
         senate_diff = senate - senate_number)

# write_csv(counts_by_year, "outputs/misc/missing_dates.csv")


#### Graphs ####
# Graph of counts by year
counts_by_year %>% 
  group_by(year) %>% 
  select(year, house_number, senate_number) %>% 
  rename(House = house_number, Senate = senate_number, Year = year) %>% 
  gather(key = "Chamber", value = "Number", House:Senate) %>% 
  ungroup() %>% 
  ggplot(aes(x = Year, y = Number, fill = Chamber, group = Chamber)) +
  # geom_bar(stat = "identity", position = "dodge") +
  geom_point(aes(color = Chamber), size = 2) +
  theme_classic() +
  # scale_fill_viridis_d() +
  scale_colour_viridis_d() +
  ylim(0, 125)

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
  rename(House = rep_diff, Senate = senate_diff, Year = year) %>% 
  gather(key = "Chamber", value = "Difference", House:Senate) %>% 
  ungroup() %>% 
  mutate(label_the_year = ifelse(abs(Difference) > 1, Year, NA)) %>% 
  ggplot(aes(x = Year, y = Difference)) +
    geom_point() +
    geom_label_repel(aes(label = label_the_year)) + 
    facet_wrap(vars(Chamber), nrow = 2) +
    theme_classic() +
    scale_fill_viridis_d() + 
    theme(text = element_text(size=12))

ggsave(
  "outputs/figures/differences_by_year.pdf",
  height = 6,
  width = 8,
  units = "in"
)

