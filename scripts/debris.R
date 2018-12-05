





use_this_path  <- "outputs/hansard/tempp"


file_names <-
  list.files(
    path = use_this_path,
    pattern = "*.csv",
    recursive = FALSE,
    full.names = TRUE
  )

names(file_names) <- list.files(use_this_path) %>% 
  str_replace(".csv", "")
  
all_data <- purrr::map_df(file_names, readr::read_csv, .id = "date")

names(all_data)

speakers <- count(all_data, Speaker, sort = TRUE)

write_csv(speakers, "speakers.csv")

write_csv(all_data, "test.csv")