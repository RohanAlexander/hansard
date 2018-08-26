#### Preamble ####
# Purpose: This is a function that will us regular expressions to fix some spelling. It will be called a bunch of text files.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 25 August 2018
# Prerequisites: 1) a bunch of text files; 2) a two column csv with the misspelling and the replacement (generate that last one by using clean_hansard_spelling.R or whatever)


#### Set up workspace ####
# Load libraries
library(tidyverse)
# update.packages()


# Load data
corrections_table <- read_csv2("scripts/misspelt_words_with_corrections.csv")
head(corrections_table)
# str_replace("you're insane 1 rise to say", corrections_table$original[1], corrections_table$corrected[1])

# Update data
# The \\b \\b works to isolate just the word - if we didn't include these before and after the search term then searching for, say, "Mon" and replacing it with "Monica", would also change all "Monica" to "Monicaica". https://xkcd.com/208/.
corrections_table <- corrections_table %>% 
  mutate(original = paste0("\\b", original, "\\b"))
# str_replace("Mon1heica", corrections_table$original[2], corrections_table$corrected[2])
# str_replace("Mon 1he ica", corrections_table$original[2], corrections_table$corrected[2])
original <- corrections_table$original
corrections <- corrections_table$corrected

names(corrections) <- original
# Save the named vector?

# Do the replacement
# str_replace_all(text$text, corrections)
str_replace_all("1 rise in 1he house", corrections)

save(corrections, file = "outputs/corrections.RData")









library(readr)
library(stringr)
some_text <- read_lines("outputs/some_text_files/1981-03-12.txt")

load("outputs/corrections.RData")
some_text_mod <- str_replace_all(some_text, corrections)

some_text_mod[542]

write(some_text_mod, file = "hehehehee.txt")