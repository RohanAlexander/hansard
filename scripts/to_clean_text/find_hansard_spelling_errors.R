#### Preamble ####
# Purpose: We don't really need everything to be spelt correctly, but fixing some common spelling issues would be great. This goes through Hansard files and identifies words that are wrong so they can be fixed. It's not going to pick up words that are correct, but wrong in that context.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 7 September 2018
# Prerequisites: You need to have Hansard in a csv or text file.


#### Set up workspace ####
# Load libraries
library(hunspell)
library(tictoc)
library(tidytext)
library(tidyverse)
# update.packages()


#### Load data ####
load("outputs/big_files_do_not_push/all_hansard_words_by_date.Rda") # Takes a minute
# Create a working sample of 100 days statements - comment these lines if you want to run on full dataset
set.seed(12345)
some_random_rows <- sample(1:nrow(all_hansard_words), 100)
all_hansard_words <- all_hansard_words[some_random_rows, ]
rm(some_random_rows)
head(all_hansard_words)

#### Finde wroong spelings ####
# Get a dataset of all the words used in the statements see: https://cran.r-project.org/web/packages/tidytext/vignettes/tidytext.html
words_in_statements <- all_hansard_words %>% 
  select(words) %>% 
  unnest_tokens(word, words, to_lower = FALSE)

# Remove stopwords and transform the words so there is a count of how many each is used
words_in_statements <- words_in_statements %>% 
  anti_join(get_stopwords()) %>% 
  count(word, sort = TRUE) 
head(words_in_statements)

# Create list of words to ignore - mostly names of electorates and Australian places:
ignore_these_electorate_names <- c("Angas", "Australian Capital Territory", "Balaclava", "Barrier", "Bland", "Bonython", "Bourke", "Burke", "Burke", "Canobolas", "Charlton", "Cook", "Coolgardie", "Corinella", "Corinella", "Dalley", "Dampier", "Darebin", "Darling", "Darling Downs", "Darwin", "Diamond Valley", "Dundas", "East Sydney ", "Echuca", "Evans", "Fawkner", "Fraser", "Grampians", "Gwydir", "Hawker", "Henty", "Higinbotham", "Hoddle", "Illawarra", "Isaacs", "Kalgoorlie", "Laanecoorie", "Lang", "Lawson", "Lowe", "Martin", "Mernda", "Moira", "Namadgi", "Nepean", "Northern Melbourne", "Northern Territory", "Oxley ", "Parkes", "Phillip", "Prospect", "Riverina-Darling", "Scullin", "South Australia", "South Sydney", "Southern Melbourne", "St George", "Streeton ", "Tasmania", "Throsby", "Watson", "West Sydney", "Wilmot", "Wimmera", "Yarra", "Warringah", "Grayndler", "Bennelong", "Cowan", "McPherson", "Menzies", "Melbourne", "Chisholm", "Cunningham", "Curtin", "McMahon", "Mallee", "McMillan", "Canberra", "Wright", "Watson", "Barton", "Port Adelaide", "Griffith", "Holt", "Rankin", "Wakefield", "Gippsland", "Bendigo", "Dawson", "Moncrieff", "Blaxland", "Newcastle", "Banks", "Franklin", "Shortland", "Parkes", "Dunkley", "Melbourne Ports", "Oxley", "Isaacs", "Murray", "Dickson", "Richmond", "Adelaide", "Leichhardt", "Brisbane", "Mackellar", "Batman", "Hunter", "Bradfield", "Boothby", "Macarthur", "Kooyong", "Calare", "Hindmarsh", "Scullin", "Lyne", "Moore", "Solomon", "Perth", "Bass", "Cowper", "Canning", "Mitchell", "Fowler", "Corangamite", "Bruce", "Page", "Petrie", "Flinders", "Lindsay", "Chifley", "Swan", "Whitlam", "New England", "Kennedy", "Batman", "Braddon", "Stirling", "Hughes", "Eden-Monaro", "Burt", "Wills", "Ballarat", "Brand", "Longman", "Bowman", "Capricornia", "Reid", "Berowra", "Fenner", "Farrer", "Maranoa", "Jagajaga", "Forrest", "Corio", "Dobell", "Riverina", "Indi", "Groom", "Lyons", "McEwen", "Cook", "Tangney", "Blair", "Wide Bay", "Fairfax", "Gorton", "Flynn", "Higgins", "Hotham", "Herbert", "Parramatta", "Barker", "Moreton", "Hinkler", "Sydney", "Pearce", "Ryan", "Durack", "Sturt", "Grey", "Kingston", "Fadden", "Greenway", "Lalor", "Mayo", "Maribyrnong", "Casey", "Lingiari", "Werriwa", "Gilmore", "Deakin", "Lilley", "Paterson", "Hume", "Wannon", "Macquarie", "Kingsford Smith", "Aston", "Wentworth", "Calwell", "Forde", "Bonner", "Fisher", "Gellibrand", "Robertson", "Denison", "Fremantle", "O'Connor", "Goldstein", "La Trobe", "Hasluck", "Makin", "North Sydney")

ignore_these_area_names <- c("Sydney", "Melbourne", "Brisbane", "Perth", "Adelaide", "Gold Coast", "Tweed Heads", "Newcastle", "Maitland", "Canberra", "Queanbeyan", "Sunshine Coast", "Wollongong", "Geelong", "Hobart", "Townsville", "Cairns", "Darwin", "Toowoomba", "Ballarat", "Bendigo", "Albury", "Wodonga", "Mackay", "Rockhampton", "Launceston", "Bunbury", "Bundaberg", "Coffs Harbour", "Hervey Bay", "Wagga Wagga", "Tamworth", "Shepparton", "Mooroopna", "Port Macquarie", "Gladstone", "Tannum Sands", "Mildura", "Traralgon", "Morwell", "Orange", "Bowral", "Mittagong", "Busselton", "Geraldton", "Dubbo", "Nowra", "Bomaderry", "Warragul", "Drouin", "Bathurst", "Warrnambool", "Albany", "Kalgoorlie", "Boulder", "Devonport", "Mount Gambier", "Lismore", "Nelson Bay", "Maryborough", "Burnie", "Wynyard", "Alice Springs", "Victor Harbor", "Goolwa", "Taree", "Ballina", "Morisset", "Cooranbong", "Armidale", "Goulburn", "Whyalla", "Gympie", "Forster", "Tuncurry", "Echuca", "Moama", "Griffith", "Grafton", "Wangaratta", "St Georges Basin", "Sanctuary Point", "Yeppoon", "Mount Isa", "Murray Bridge", "Broken Hill", "Camden Haven", "Moe", "Newborough", "Karratha", "Singleton", "Batemans Bay", "Horsham", "Port Lincoln", "Ulladulla", "Warwick", "Kempsey", "Bairnsdale", "Sale", "Broome", "Ulverstone", "Port Hedland", "Port Pirie", "Emerald", "Port Augusta", "Lithgow", "Colac", "Muswellbrook", "Esperance", "Mudgee", "Parkes", "Swan Hill", "Portland", "Kingaroy", "Sydney", "Melbourne", "Brisbane", "Perth", "Adelaide", "Gold Coast", "Tweed Heads", "Canberra", "Queanbeyan", "Newcastle", "Central Coast", "Wollongong", "Sunshine Coast", "Hobart", "Townsville", "Geelong", "Cairns", "Darwin", "Toowoomba", "Ballarat", "Bendigo", "Mandurah", "Albury", "Wodonga", "Maitland", "Mackay", "Launceston", "Bunbury", "Rockhampton", "Melton", "Hervey Bay", "Bundaberg", "Wagga Wagga", "Coffs Harbour", "Shepparton", "Mooroopna", "Port Macquarie", "Orange", "Mildura", "Buronga", "Sunbury", "Dubbo", "Tamworth", "Bathurst", "Gladstone", "Geraldton", "Nowra–Bomaderry", "Warrnambool", "Kalgoorlie", "Boulder", "Albany", "Lismore", "Gawler", "Mount Gambier", "Traralgon", "Busselton")

ignore_list <- c(ignore_these_area_names, ignore_these_electorate_names)

# Run the spell check using Hunspell: https://cran.r-project.org/web/packages/hunspell/vignettes/intro.html
words_in_statements$right_spelling <- hunspell_check(words_in_statements$word, dict = dictionary('en_GB', add_words = ignore_list))

# Filter to just wrong words
wrong_words_in_statements <- words_in_statements %>% 
  filter(right_spelling == FALSE) %>% 
  filter(n >= 2) %>% 
  select(-right_spelling, -n)
# %>%  # You can add a column of suggestions - takes a while - if you want, by pipe to the next row, but it doesn't seem to get you much in this case
#   mutate(recommended_word = hunspell_suggest(word))
# wrong_words_in_statements <- wrong_words_in_statements[,1:3] # If you included the column of suggestions then you need this if you want to write.table
write_csv(wrong_words_in_statements, "wrong.csv")


#### Examine issues ####
# The above provides a bunch of possible issues and if want to examine the context use:
rows_of_statements_that_contain_string <- all_hansard %>%
  filter(str_detect(statement, " connexion "))


#### Clean up ####
# Save
TBD

# Remove
rm(wrong_words_in_statements, words_in_statements)