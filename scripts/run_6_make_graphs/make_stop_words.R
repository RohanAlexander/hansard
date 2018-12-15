library(lubridate)
library(purrr)
library(stringr)
library(tidyverse)

str_count("Hey the Malcolm the ex PM", "the")
str_count("Hey the Malcolm the ex PM", " ")


# Get the data
# House
# From: https://github.com/STAT545-UBC/Discussion/issues/398
reps_files <- list.files(path = "outputs/hansard/tempp", full.names = TRUE)

names(reps_files) <- list.files(path = "outputs/hansard/tempp") %>% 
  gsub(pattern = ".csv$", replacement = "")

all_data <- purrr::map_df(reps_files, readr::read_csv, .id = "Date")

# Check the
all_data$the_count <- str_count(all_data$text, " the ")
all_data$and_count <- str_count(all_data$text, " and ")
all_data$be_count <- str_count(all_data$text, " be ")
all_data$to_count <- str_count(all_data$text, " to ")
all_data$of_count <- str_count(all_data$text, " of ")

all_data$word_count <- str_count(all_data$text, " ")

all_data$Date <- ymd(all_data$Date)

stop_words_data <- all_data %>%
  group_by(Date) %>% 
  summarise(the = sum(the_count),
            be = sum(be_count),
            to = sum(to_count),
            of = sum(of_count),
            and = sum(and_count),
            all_words = sum(word_count))

stop_words_data %>% 
  gather(key = "Word", value = "Count", the, be, to, of, and) %>% 
  mutate(Proportion = Count / all_words) %>% 
    ggplot(aes(x = Date, y = Proportion, color = Word)) +
    geom_point() +
    ylim(0, 0.10) +
    theme_classic() +
  scale_color_viridis_d()


ggsave(
  "outputs/figures/stop_words.pdf",
  height = 6,
  width = 8,
  units = "in"
)
