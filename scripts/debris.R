





use_this_path  <- "outputs/hansard/tempp"


file_names <-
  list.files(
    path = use_this_path,
    pattern = "*.csv",
    recursive = FALSE,
    full.names = TRUE
  )

all_data <- purrr::map_df(file_names, readr::read_csv, .id = "id")

names(all_data)

speakers <- all_data$Speaker %>% count()
count(all_data, Speaker, sort = TRUE)

write_csv(all_data, "test.csv")