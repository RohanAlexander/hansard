library(lubridate)
library(tidyverse)
library(ggrepel)

load("outputs/politicians_by_individuals.Rda")
load("outputs/politicians_by_division.Rda")
election_dates <- read_csv("data/misc/misc_elections_data.csv")

politicians_by_division <- politicians_by_division %>%
  group_by(uniqueID) %>%
  slice(which.min(electionDate))

politicians_by_individuals <- politicians_by_individuals %>%
  left_join(politicians_by_division, by = "uniqueID") %>%
  select(
    "uniqueID",
    "surname",
    "allOtherNames",
    "firstName",
    "commonName",
    "gender",
    "birthDate",
    "birthYear",
    "electionDate",
    "wasEverPrimeMinister"
  )

rm(politicians_by_division)

politicians_by_individuals <- politicians_by_individuals %>%
  mutate(
    ageAtFirstElection = as.integer(floor((
      electionDate - birthDate
    ) / 365)),
    electionYear = ifelse(is.na(birthDate), (as.integer(year(
      electionDate
    ))), NA),
    ageInYears = electionYear - birthYear,
    ageAtFirstElection = if_else(is.na(ageAtFirstElection), ageInYears, ageAtFirstElection)
  ) %>%
  select(-electionYear,-ageInYears)

politicians_by_individuals <- politicians_by_individuals %>%
  mutate(firstElectionYear = year(electionDate))

politicians_by_age_at_election <- politicians_by_individuals %>%
  group_by(firstElectionYear) %>%
  summarise(averageAge = mean(ageAtFirstElection))

head(politicians_by_age_at_election)

data_for_graph <- election_dates %>%
  left_join(politicians_by_age_at_election,
            by = c("year" = "firstElectionYear")) %>%
  select(year, election, averageAge, comment)

data_for_graph <- data_for_graph %>%
  mutate(averageAge = ifelse(is.na(election), NA, averageAge)) 

data_for_graph$comment[data_for_graph$comment == "Lyons '31"] <- NA
data_for_graph$comment[data_for_graph$comment == "Menzies '49"] <- NA
data_for_graph$comment[data_for_graph$comment == "Howard '96"] <- NA

head(data_for_graph)
head(politicians_by_individuals)
politicians_by_individuals$ageAtFirstElection <- as.double(politicians_by_individuals$ageAtFirstElection)

just_prime_ministers <- politicians_by_individuals %>% 
  filter(wasEverPrimeMinister == 1) %>% 
  mutate(commonName = if_else(is.na(commonName), firstName, commonName)) %>% 
  unite(useName, commonName, surname, remove = FALSE, sep = " ")


ggplot(data = data_for_graph,
       mapping = aes(x = year, y = averageAge)) +
  geom_point(data = politicians_by_individuals, colour = "grey", mapping = aes(x = firstElectionYear, y = ageAtFirstElection)) +
  geom_point(na.rm = TRUE, shape = 21) +
  geom_point(data = just_prime_ministers, mapping = aes(x = firstElectionYear, y = ageAtFirstElection)) +
  geom_smooth(data = politicians_by_individuals, linetype="dashed", colour = "black", mapping = aes(x = firstElectionYear, y = ageAtFirstElection)) +
  geom_label_repel(data = just_prime_ministers, mapping = aes(x = firstElectionYear, y = ageAtFirstElection, label = useName), segment.size  = 0.2, segment.color = "grey50", repel = 15) +
  labs(
    y = "Age",
    x = "Year",
    title = "Age first elected to House of Representatives",
    caption = "Data based on Parliamentary Handbook."
  ) +
  theme_classic() +
  theme(text = element_text(size = 18))



ggsave(
  "outputs/figures/age_at_election.pdf",
  height = 8.27,
  width = 11.69,
  units = "in"
)
