# library(broom)
library(ggrepel)
library(lubridate)
library(tidyverse)

politicians <- read_csv("data/politicians_by_division.csv")

# Change classes to dates
politicians$dateOfElection <-
  dmy(politicians$dateOfElection)

politicians$dateCeasedToBeMember <-
  dmy(politicians$dateCeasedToBeMember)

head(politicians)

politicians <- politicians %>% 
  filter(byElection == "No") %>% 
  group_by(uniqueKey) %>% 
  summarise(yearEnteredParliament = min(dateOfElection)) %>% 
  mutate(yearEnteredParliament = year(yearEnteredParliament))

new_politicians_year_count <- politicians %>% 
  group_by(yearEnteredParliament) %>% 
  summarise(numberOfNewOnes = n())

new_politicians_year_count$winner <- c("Not labor", "Not labor", "Not labor", "Labor", "Not labor", "Labor", "Not labor", "Not labor", "Not labor", "Not labor", "Not labor", "Labor", "Not labor", "Not labor", "Not labor", "Not labor", "Labor", "Labor", "Not labor", "Not labor", "Not labor", "Not labor", "Not labor", "Not labor", "Not labor", "Not labor", "Not labor", "Labor", "Labor", "Not labor", "Not labor", "Not labor", "Labor", "Labor", "Labor", "Labor", "Labor", "Not labor", "Not labor", "Not labor", "Not labor", "Labor", "Labor", "Not labor", "Not labor")

new_politicians_year_count$comment <- c("", "", "", "", "", "Fisher '14", "", "", "", "", "", "", "Lyons '31", "", "", "", "Curtin '43", "", "Menzies '49", "", "", "", "", "", "", "", "", "", "", "Fraser '75", "", "", "", "", "", "", "", "Howard '96", "", "", "", "Kevin '07", "", "", "")

data_for_graph <- tibble(year = 1901:2018)

data_for_graph <-
  left_join(data_for_graph, new_politicians_year_count, by = c("year" = "yearEnteredParliament"))

colo <- c("Labor" = "#f00011", "Not labor" = "#050e47")

head(data_for_graph)

data_for_graph <- data_for_graph %>% 
  filter(!is.na(winner))

ggplot(data = data_for_graph,
       mapping = aes(x = year, y = numberOfNewOnes, color = winner, label = comment)) +
  geom_point(na.rm = TRUE, size = 6) +
  geom_text(size = 6, vjust = -0.8, nudge_y = 0.5) +
  annotate("text", label = "(First parliament)", x = 1908, y = 72, size = 6, colour = "Black") +
  annotate("text", label = "(No. of seats increases)", x = 1949, y = 65, size = 6, colour = "Black") +
  labs(
    y = "Number",
    x = "Year",
    color = "Election winner",
    title = "Newly elected House of Representatives Members, by election",
    caption = "By-elections removed. Data from Parliamentary Handbook as modified by Rohan Alexander."
  ) +
  theme_classic() +
  theme(text = element_text(size = 18)) +
  scale_colour_manual(values = colo)

ggsave(
  "outputs/newly_elected_by_party.pdf",
  height = 8.27,
  width = 11.69,
  units = "in"
)
