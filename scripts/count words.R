library(quanteda)
library(tidyverse)


function()



ntoken(hansard$text, remove_punct = TRUE) %>% class()




#### Create the function that will be applied to the files ####
count_tokens <-
  function(name_of_input_csv_file) {
    # Read in the csv, based on the filename list
    # name_of_input_csv_file <- "outputs/hansard/run_7_output/hor-1901-07-19.csv" # uncomment for testing
    
           hansard <-
             read_csv(name_of_input_csv_file,
                      col_types = cols())
           
           numbers <- tibble(number = ntoken(hansard$text, remove_punct = TRUE))

    # Save file
    return(numbers)
    
    print(paste0("Done with ", name_of_input_csv_file, " at ", Sys.time()))
  }


files <- c("outputs/hansard/run_7_output/hor-1901-07-19.csv"), "outputs/hansard/run_7_output/hor-1903-06-25.csv")

use_this_path_to_get_csvs  <- "/Volumes/Hansard/parsed/federal/for_zoe/run_7_output"
# use_this_path_to_get_csvs <- "/Volumes/Hansard/parsed/federal/hor"

# Get list of Hansard csvs that have been parsed from PDFs and had front matter removed
file_names <-
  list.files(
    path = use_this_path_to_get_csvs,
    pattern = "*.csv",
    recursive = FALSE,
    full.names = TRUE
  )


test <- map_dfr(files, count_tokens)

sum(test$number)

8 times wiht both

2 times with just the first

6 times wiht just the second