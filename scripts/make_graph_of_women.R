library(broom)
library(tidyverse)

load("outputs/parliamentary_handbook.Rda")

# list_of_women <- c("LYONS, Enid", "BLACKBURN, Doris", "BROWNBILL, Kay", "CHILD, Joan", "DARLING, Elaine", "KELLY, Ros", "FATIN, Wendy", "MCHUGH, Jeannette", "MAYER, Helen", "JAKOBSEN, Carolyn", "SULLIVAN, Kathy", "CRAWFORD, Mary", "HARVEY, Elizabeth", "BAILEY, Fran", "CROSIO, Janice", "GALLUS, Christine", "DEAHM, Maggie", "EASSON, Mary", "HENZELL, Marjorie", "MOYLAN, Judi", "SMITH, Silvia", "WORTH, Trish", "LAWRENCE, Carmen", "BISHOP, Bronwyn", "DRAPER, Trish", "ELLIS, Annette", "ELSON, Kay", "GAMBARO, Teresa", "GASH, Joanna", "GRACE, Elizabeth", "HANSON, Pauline", "JEANES, Susan", "JOHNSTON, Ricky", "KELLY, De-Anne", "KELLY, Jackie", "MACKLIN, Jenny", "STONE, Sharman", "VALE, Danna", "WEST, Andrea", "BISHOP, Julie", "BURKE, Anna", "GERICK, Jane", "GILLARD, Julia", "HALL, Jill", "HOARE, Kelly", "HULL, Kay", "IRWIN, Julia", "KERNOT, Cheryl", "LIVERMORE, Kirsten", "MCFARLANE, Jann", "MAY, Margaret", "O'BYRNE, Michelle", "PLIBERSEK, Tanya", "ROXON, Nicola", "CORCORAN, Ann", "SHORT, Leonie", "GEORGE, Jennie", "GRIERSON, Sharon", "JACKSON, Sharryn", "KING, Catherine", "LEY, Sussan", "MIRABELLA, Sophie", "VAMVAKINOU, Maria", "BIRD, Sharon", "ELLIOT, Justine", "ELLIS, Kate", "MARKUS, Louise", "OWENS, Julie", "CAMPBELL, Jodie", "COLLINS, Julie", "D'ATH, Yvette", "MARINO, Nola", "MCKEW, Maxine", "NEAL, Belinda", "PARKE, Melissa", "REA, Kerry", "RISHWORTH, Amanda", "SAFFIN, Janelle", "O'DWYER, Kelly", "ANDREWS, Karen", "BRODTMANN, Gai", "GRIGGS, Natasha", "O'NEILL, Deborah", "PRENTICE, Jane", "ROWLAND, Michelle", "SMYTH, Laura", "CHESTERS, Lisa", "CLAYDON, Sharon", "HENDERSON, Sarah", "LANDRY, Michelle", "MACTIERNAN, Alannah", "MCGOWAN, Cathy", "MCNAMARA, Karen", "O'NEIL, Clare", "PRICE, Melissa", "RYAN, Joanne", "SCOTT, Fiona", "SUDMALIS, Ann", "WICKS, Lucy", "BUTLER, Terri", "ALY, Anne", "BANKS, Julia", "BURNEY, Linda", "FLINT, Nicolle", "HUSAR, Emma", "KEAY, Justine", "KING, Madeleine", "LAMB, Susan", "MCBRIDE, Emma", "O'TOOLE, Cathy", "SHARKIE, Rebekha", "STANLEY, Anne", "SWANSON, Meryl", "TEMPLEMAN, Susan", "KEARNEY, Ged")

# parliamentary_handbook <- parliamentary_handbook %>%
#   mutate(in_or_out = if_else(hansardName %in% list_of_women, 1, 0))

counts_by_gender <- parliamentary_handbook %>%
  filter(!is.na(surname)) %>%
  group_by(yearElected, gender) %>%
  summarise(n()) %>%
  rename(number = `n()`)

counts_by_gender <- counts_by_gender %>%
  group_by(gender) %>%
  mutate(cumsum = cumsum(number))

data_for_graph <- tibble(year = 1901:2018)
data_for_graph <-
  left_join(data_for_graph, counts_by_gender, by = c("year" = "yearElected"))

data_for_graph <- data_for_graph %>%
  select(-number) %>%
  spread(key = gender, value = cumsum) %>%
  select(year, female, male) %>%
  fill(female, male, .direction = c("down"))

data_for_graph <- data_for_graph %>%
  gather(male, female, key = "gender", value = "cumulative")

ggplot(data = data_for_graph,
       mapping = aes(x = year, y = cumulative, color = gender)) +
  geom_point() +
  # geom_smooth(data=subset(data_for_graph, year > 1990), method=lm , se=FALSE) +
  scale_colour_viridis_d() +
  labs(
    colour = "Gender",
    y = "Cumulative number",
    x = "Year",
    title = "Members of the House of Representatives, cumulative number, by gender",
    caption = "Data based on the Parliamentary Handbook, as modified by the author - all errors by Rohan Alexander."
  ) +
  theme_classic() +
  theme(text = element_text(size = 18))

ggsave(
  "outputs/cumulative_number_by_sex.pdf",
  height = 8.27,
  width = 11.69,
  units = "in"
)
