#### Preamble ####
# Purpose: Convert our topics to the CAP
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 23 March 2019
# Prerequisites:
# Issues:


#### Workspace set-up ####
library(tidyverse)
# update.packages()


#### Load data  ####  
the_gammas <- read_csv("outputs/results-topic_models_and_gammas/gammas_model_80.csv")
the_conversion <- read_csv2("outputs/results-topic_models_and_gammas/top_words_80_comparative_agendas.csv")

the_conversion <- rename(the_conversion, topic = Topic, CAP_number = `CAP number`)

the_conversion <- the_conversion %>% 
  select(topic, CAP_number)



the_gammas <- the_gammas %>% 
  left_join(the_conversion)


write_csv(the_gammas, "outputs/results-topic_models_and_gammas/gammas_model_80-with-cap.csv")