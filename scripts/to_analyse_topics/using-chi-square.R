# !diagnostics off
#### Preamble ####
# Purpose: Analyse topics using chi-square tests for differences
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 24 September 2018
# Prerequisites:
# Issues:


#### Workspace set-up ####
# Load libraries
# library(lubridate)
# library(quanteda)
# library(stm)
# library(tictoc)
library(tidytext)
# library(tidyverse)
# library(viridis)
# update.packages()

# Load the election dates
election_dates <- read_csv("inputs/misc/misc_elections_data.csv")
# Load topics
election_dates <- read_csv(td_gamma, "test_gammas.csv")