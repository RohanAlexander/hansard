#### Preamble ####
# Purpose: We don't really need everything to be spelt correctly, but fixing some common spelling issues would be great.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 23 July 2018
# Prerequisites: You need to have the Hansard in a data frame - see get_data_from_xml_to_dataframe.


#### Set up workspace ####
# Load libraries
# install.packages("hunspell")
library(hunspell)
library(tictoc)
library(tidytext)
library(tidyverse)
# update.packages()


#### Load data ####
load("outputs/all_hansard.Rda") # Takes a minute
# Create a working sample of 100,000 statements - Comment these lines if you want to run on full dataset
set.seed(123)
some_random_rows <- sample(1:nrow(all_hansard), 100000)
some_hansard <- all_hansard[some_random_rows, ]
rm(all_hansard, some_random_rows)


#### Finde wroong spelings ####
# Get a dataset of all the words used in the statements see: https://cran.r-project.org/web/packages/tidytext/vignettes/tidytext.html
words_in_statements <- some_hansard %>% 
  select(statement) %>% 
  unnest_tokens(word, statement, to_lower = FALSE)

# Remove stopwords and transform the words so there is a count of how many each is used
words_in_statements <- words_in_statements %>% 
  anti_join(get_stopwords()) %>% 
  count(word, sort = TRUE) 

# Run the spell check using Hunspell: https://cran.r-project.org/web/packages/hunspell/vignettes/intro.html
words_in_statements$right_spelling <- hunspell_check(words_in_statements$word, dict = 'en_GB')

# Filter to just wrong words and add a column of suggestions - takes a while
wrong_words_in_statements <- words_in_statements %>% 
  filter(right_spelling == FALSE) %>% 
  mutate(recommended_word = hunspell_suggest(word))
  

#### Fix issues ####
??? "by" <- "bv"
??? "endorse" <- "indorse"
??? "endorsement" <- "indorsement"
??? "has" <- "ha3"
??? "Hon" <- "hon"
??? "I'm" <- "lm"
??? "it" <- "lt"
??? "they" <- "thev"
??? "Tory" <- "tory"
??? "very" <- "verv"
??? "we" <- "ve"
???? "00m"
????? "tralia"
"Address inReply" <- "AddressinReply"
"Attorney General"<- "AttorneyGeneral"
"Attorney General's" <- "AttorneyGeneral's"
"Australian" <- "Austraiian"
"central" <- "cental"
"connection" <- "connexion"
"do not" <- "donot"
"Eden Monaro" <- "EdenMonaro"
"employee" <- "employe"
"employes"
"ex-servicemen" <- "exservicemen"
"favorable"
"free trade" <- "freetrade"
"Governor General" <- "GovernorGeneral" 
"GovernorGeneral's"
"have been" <- "havebeen"
"honorable member" <- "honorablemember"
"honorable"
"Honorable"
"in the" <- "inthe"
"ing"
"It" <- "Tt"
"Kanakas" <- "kanakas"
"lias"
"Ministry" <- "Mini stry"
"not" <- "npt"
"notice paper" <- "noticepaper"
"of the" <- "ofthe"
"of" <- "o'f"
"ot"
"pf"
"Postmaster General" <- "PostmasterGeneral"
"Postmaster General's" <- "PostmasterGeneral's"
"self government" <- "selfgovernment"
"th"
"tha"
"thai"
"that the" <- "thatthe"
"the Government" <- "theGovernment"
"the honorable" <- "thehonorable"
"the honorable" <- "thehonorable"
"this" <- "thi3"
"tho"
"tion"
"tlie"
"to the" <- "tothe"
"Vice President" <- "VicePresident"
"we" <- "wc"
"wheat growers" <- "wheatgrowers"
"will be" <- "willbe"
"wool growers" <- "woolgrowers"
"would be" <- "wouldbe"



#### Clean up ####
# Save
TBD

# Remove
rm(wrong_words_in_statements, words_in_statements)