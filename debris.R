# Get the name fixer
fix_wrong_names <-
  read_csv2("inputs/misc/wrong_names_with_corrections.csv") %>%
  mutate(numberOfCharacters = nchar(original)) %>%
  arrange(desc(numberOfCharacters)) %>%
  select(-numberOfCharacters)



#### Create lists of CSVs to read ####
# Change the path as required:
use_this_path_to_get_csvs  <- "outputs/hansard/temp"
# use_this_path_to_get_csvs <- "/Volumes/Hansard/parsed/federal/hor"

# Get list of Hansard csvs that have been parsed from PDFs and had front matter removed
file_names <-
  list.files(
    path = use_this_path_to_get_csvs,
    pattern = "*.csv",
    recursive = FALSE,
    full.names = TRUE
  )

file_names <- file_names %>% sample() # Randomise the order

# Seems unnecessary, but sometimes useful to separate input and output
use_this_path_to_save_csvs  <- "outputs/hansard/tempp"
# use_this_path_to_save_csvs <- "/Volumes/Hansard/parsed/federal/hor"
save_names <- file_names %>%
  str_replace(use_this_path_to_get_csvs, use_this_path_to_save_csvs)




file_names <-
  list.files(
    path = use_this_path_to_save_csvs,
    pattern = "*.csv",
    recursive = FALSE,
    full.names = TRUE
  )

d <- purrr::map_df(file_names, readr::read_csv, .id = "id")

names(d)

speakers <- d$Speaker %>% unique()

write_csv(d, "test.csv")