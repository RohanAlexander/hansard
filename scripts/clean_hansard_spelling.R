#### Preamble ####
# Purpose: We don't really need everything to be spelt correctly, but fixing some common spelling issues would be great.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 24 July 2018
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
wrong_words_in_statements <- wrong_words_in_statements[,1:3]
write.table(wrong_words_in_statements, "wrong.txt", sep="\t", row.names=FALSE)

#### Fix issues ####

""
# The above provides a bunch of possible issues and if want to examine the context use:
rows_of_statements_that_contain_string <- some_hansard %>%
  filter(str_detect(statement, " pf "))

# Based on above we have the following issues and corrections
corrections <- c(
  " 1 rise " = " I rise ",
  " 1he " =  " the ", 
  " actionwas " = " action was ",
  " AddressinReply " = " Address in Reply ",
  " Al! " = " All ",
  " aswas " = " as was ",
  " AttorneyGeneral " = " Attorney General ",
  " AttorneyGeneral's " = " Attorney General's ",
  " Aus tralia " = " Australia ",
  " Austraiian " = " Australian ",
  " bv " =  " by ",
  " cental " = " central ",
  " connexion " = " connection ",
  " donot " = " do not ",
  " EdenMonaro " = " Eden Monaro ",
  " employe " = " employee ",
  " employes " = " employees ",
  " exservicemen " = " ex-servicemen ",
  " freetrade " = " free trade ",
  " GovernorGeneral"  = " Governor General ",
  " GovernorGeneral's " = " Governor General's ",
  " ha ve " = " have ",
  " ha3 " =  " has ",
  " havebeen " = " have been ",
  " hon orable " =  " honorable ",
  " honorablemember " = " honorable member ",
  " i3 " = " is ",
  " isthat " = " is that ",
  " Ilansard " = " Hansard ", 
  " indorse " = " endorse ", 
  " indorsement " = " endorsement ",
  " inthe " = " in the ",
  " kanakas " = " Kanakas ",
  " LabourParty " = " Labour Party ",
  " lias " = " has ",
  " longterm " = " long term ",
  " maintaina " = " maintain a ",
  " Mini stry " = " Ministry ",
  " noticepaper " = " notice paper ",
  " npt " = " not ",
  " npt " = " not ",
  " o'f " = " of ",
  " ofthe " = " of the ",
  " OppositionI " = " Opposition I ",
  " Parlia- ment " = " Parliament ",
  " PostmasterGeneral " = " Postmaster General ",
  " PostmasterGeneral's " = " Postmaster General's ",
  " pre,viously " = " previously ",
  " selfgovernment " = " self government ",
  " thatthe " = " that the ",
  " theexpression " = "the expression",
  " thefreight " = " the freight ",
  " theGovernment " = " the Government ",
  " thehonorable " = " the honorable ",
  " thev " = " they ",
  " thi3 " = " this ",
  " tothe " = " to the ",
  " Tt " = " It ",
  " twomore " = " two more ",
  " verv " =  " very ",
  " VicePresident " = " Vice President ",
  " wc " = " we ",
  " wheatgrowers " = " wheat growers ",
  " willbe " = " will be ",
  " woolgrowers " = " wool growers ",
  " wouldbe " = " would be "
)

# Do the replacement
some_hansard$statement <- str_replace_all(some_hansard$statement, corrections)
rm(corrections)


#### Clean up ####
# Save
TBD

# Remove
rm(wrong_words_in_statements, words_in_statements)