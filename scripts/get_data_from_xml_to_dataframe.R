#### Preamble ####
# Purpose: This file goes through Australian Hansard XML data and creates rectangular datasets, by date.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 23 July 2018
# Prerequisites: Download the zipped record of Hansard put together by Tim Sherratt (about 2GB). Unzip it (about 6GB...). Delete the zipped versions of the files (not sure why they are there too). Link to write up of this is: https://timsherratt.org/research-notebook/projects/historic-hansard/
# To do:
# - Remove the paras that are not speeches, but rather instructions.
# - Remove the embarrasing function calls


#### Set up workspace ####
# Load libraries
library(tictoc)
library(tidyverse)
library(XML)
library(xml2)
# update.packages()


#### Create functions ####
# Check for the existence of the field and if not then make it worth something - used to make the data frame work
checkExistence <- function(field_of_interest) {
  if (length(field_of_interest) == 0) {
    field_of_interest <- NA
  } else {
    field_of_interest <- field_of_interest
  }
  return(field_of_interest)
}

# Add the date and other session details to the data - used toward the end to grab one piece of information per Hansard file
addHansardDetailsAsColumn <- function(variable_of_interest) {
  search_for <- paste0(".//", variable_of_interest)
  column_of_interest <- xml_find_first(this_hansard_as_xml, search_for) %>%
    xml_text() %>%
    as.character() %>%
    rep(nrow(this_hansard_as_tibble))
  return(column_of_interest)
}


#### Load data ####
# Load list of file names
# file_names <- 
#   list.files(
#     path = "data/hansard_test_data/1949",
#     pattern = "*.xml",
#     recursive = TRUE,
#     full.names = TRUE
#   ) # Use this one while testing - fewer files
file_names <-
  list.files(
    path = "data/hansard-xml-master/hofreps",
    pattern = "*.xml",
    recursive = TRUE,
    full.names = TRUE
  ) # The full set of files
file_names <- file_names[c(1000:length(file_names))] # Further reduce the number of files to one while getting it working

#### Make dataset of all statements ####
tic("Looping") # Start timer

list_of_wrong_file_numbers <- list()

for (i in 1:length(file_names)) { # Loop into each file
  this_hansard_as_xml <- read_xml(file_names[i]) # Read file i as XML, change i to 1 while testing
  
  this_hansard_as_tibble <- 
    xml_find_all(this_hansard_as_xml, ".//talk.start") %>%   # Find all the speeches in this file - xml_find_all. Based on https://stackoverflow.com/questions/40654667/r-convert-complex-xml-to-dataframe and https://stackoverflow.com/questions/30279204/convert-xml-with-repeated-nodes-to-r-data-frame
      map_df(function(x) {
        # This imposes a function on a list and outputs a dataframe - the list is each of the speech nodesets and the function is just picking out pieces from the XML.
        
        debate_title <-
          xml_find_all(x, "ancestor::debate") %>% # For more on how ancestor works: https://www.w3schools.com/xml/xpath_axes.asp
          xml_find_first(".//title") %>% xml_text() %>% as.character()
        
        debate_page.no <-
          xml_find_all(x, "ancestor::debate") %>%
          xml_find_first(".//page.no") %>% xml_text() %>% as.character()
        
        debate_type <-
          xml_find_all(x, "ancestor::debate") %>%
          xml_find_first(".//type") %>% xml_text() %>% as.character()
        
        subdebate_title <-
          xml_find_all(x, "ancestor::subdebate.1") %>%
          xml_find_first(".//title") %>% xml_text() %>% as.character()
        
        subdebate_page.no <-
          xml_find_all(x, "ancestor::subdebate.1") %>%
          xml_find_first(".//page.no") %>% xml_text() %>% as.character() 
        
        speakerID <-
          xml_find_first(x, ".//name.id") %>% xml_text() %>% as.character()
        
        speaker_name_meta <-
          xml_find_first(x, ".//name[contains(@role, 'metadata')]") %>% xml_text() %>% as.character()
        
        speaker_name_display <-
          xml_find_first(x, ".//name[contains(@role, 'display')]") %>% xml_text() %>% as.character()
        
        page_number <-
          xml_find_first(x, ".//page.no") %>% xml_text() %>% as.character()
        
        electorate <-
          xml_find_first(x, ".//electorate") %>% xml_text() %>% as.character()
        
        party <-
          xml_find_first(x, ".//party") %>% xml_text() %>% as.character()
        
        role <-
          xml_find_first(x, ".//role") %>% xml_text() %>% as.character()
        
        in_gov <-
          xml_find_first(x, ".//in.gov") %>% xml_text() %>% as.character()
        
        first_speech <-
          xml_find_first(x, ".//first.speech") %>% xml_text() %>% as.character()
        
        statement <-
          xml_find_all(x, ".//para") %>% xml_text() %>% as.character()
        
        # Sometimes the statement continues outside of that talk.start. The following are needed to gather those situations.
        # Following https://stackoverflow.com/questions/7600960/how-to-check-for-the-immediate-following-sibling-of-parents-in-xpath and https://www.w3schools.com/xml/xpath_axes.asp 
        neighbours_check <-
          xml_find_all(x, "./following-sibling::*[position()=1][name()='para' or name()='quote']")
          # xml_find_all(x, "./following-sibling::talk.start[1]/preceding-sibling::para[preceding-sibling::talk.start]")
        
        # neighbours_check_second <-
        #     xml_find_all(x, "./following-sibling::*[position()=2][name()='para' or name()='quote']")
        
        uncle_check <-
          xml_find_all(x, "../following-sibling::*[position()=1][name()='para' or name()='quote']")

        # xml_find_all("..//following-sibling::*[name()!='parav'][1]/preceding-sibling::parav[preceding-sibling::para]")
                
        if (!is_empty(neighbours_check) & !is_empty(uncle_check)) {
          statement <-
            paste(
              statement,
              neighbours_check %>% xml_text() %>% as.character(),
              uncle_check %>% xml_text() %>% as.character()
            )
        # } else if (!is_empty(neighbours_check) & !is_empty(neighbours_check_second)) {
        #   statement <-
        #     paste(statement,
        #           neighbours_check %>% xml_text() %>% as.character(),
        #           neighbours_check_second %>% xml_text() %>% as.character())
        } else if (!is_empty(neighbours_check)) {
          statement <-
            paste(statement,
                  neighbours_check %>% xml_text() %>% as.character())
        } else if (!is_empty(uncle_check)) {
          statement <-
            paste(statement, uncle_check %>% xml_text() %>% as.character())
        } else {
          statement <- statement
        }
        
        statement_type <-
          xml_parent(x) %>% xml_name() %>% as.character()
        
        # Come back here and fix this embarrasment
        debate_title <- checkExistence(debate_title)
        debate_page.no <- checkExistence(debate_page.no)
        debate_type <- checkExistence(debate_type)
        subdebate_title <- checkExistence(subdebate_title)
        subdebate_page.no <- checkExistence(subdebate_page.no)
        speakerID <- checkExistence(speakerID)
        speaker_name_meta <- checkExistence(speaker_name_meta)
        speaker_name_display <- checkExistence(speaker_name_display)
        page_number <- checkExistence(page_number)
        electorate <- checkExistence(electorate)
        party <- checkExistence(party)
        role <- checkExistence(role)
        in_gov <- checkExistence(in_gov)
        first_speech <- checkExistence(first_speech)
        statement <- checkExistence(statement)
        statement_type <- checkExistence(statement_type)

        # Pull it all together into a dataframe - if you add another type of field then make sure you add it to the list here
        tibble(
          debate_title,
          debate_page.no,
          debate_type,
          subdebate_title,
          subdebate_page.no,
          speakerID,
          speaker_name_meta,
          speaker_name_display,
          page_number,
          electorate,
          party,
          role,
          in_gov,
          first_speech,
          statement,
          statement_type
        )
      })
  
  
  if (is_empty(this_hansard_as_tibble)) {
    list_of_wrong_file_numbers <- c(list_of_wrong_file_numbers, i)
    next # skip iteration and go to next iteration if there's an error
  }
  
    
  # Add the date and other session details
  # Come back here and fix this embarrasment
  this_hansard_as_tibble$date <- addHansardDetailsAsColumn("date")
  this_hansard_as_tibble$parliament.no <- addHansardDetailsAsColumn("parliament.no")
  this_hansard_as_tibble$session.no <- addHansardDetailsAsColumn("session.no")
  this_hansard_as_tibble$period.no <- addHansardDetailsAsColumn("period.no")
  this_hansard_as_tibble$chamber <- addHansardDetailsAsColumn("chamber")


  # Change the class
  this_hansard_as_tibble <- this_hansard_as_tibble %>%
    mutate(
      page_number = as.integer(page_number),
      in_gov = as.integer(in_gov),
      first_speech = as.integer(first_speech),
      parliament.no = as.integer(parliament.no),
      session.no = as.integer(session.no),
      period.no = as.integer(period.no),
      date = as.Date(date)
    )
  
  
  #### Save ####
  # Save as R data 
  this_hansards_date <- xml_find_first(this_hansard_as_xml, ".//date") %>%
    xml_text() %>%
    as.character() 
  file_name <- paste0("outputs/each_hansard/", this_hansards_date, ".Rda")
  save(this_hansard_as_tibble, file = file_name)

  
  ### Clean up ###
  rm(this_hansard_as_tibble, this_hansard_as_xml)

  
  print(paste(
    "Done with file number",
    i,
    "of",
    length(file_names),
    "at",
    Sys.time()
  ))  # Helpful so that you know progress when running it on all the records
}

toc()


# Clean up
list_of_wrong_file_numbers


#### Create one big dataframe #### 
# Follows https://stackoverflow.com/questions/17499013/how-do-i-make-a-list-of-data-frames

# Get a list of all the Rda files created above
file_names <-
  list.files(
    path = "outputs/each_hansard",
    pattern = "*.Rda",
    recursive = TRUE,
    full.names = TRUE
  ) # The full set of files

# Initialise a list that will hold them
my_data <- list()

# Read each dataframe into that list
for (i in seq_along(file_names)) {
  load(file = file_names[i])
  my_data[[i]] <- this_hansard_as_tibble
}

# Bind all the dataframes together
tic("binding")
all_hansard <- bind_rows(my_data)
toc("binding")

# Save the big dataframe
save(all_hansard, file = "outputs/all_hansard.Rda")






















#### Testing Xpath ####
test_hansard <- read_xml(
  "
<speech>
    <debate>
      <debateinfo>
        <title>You're NOT the one that I want</title>
        <talk.start>
          <talker>
            <page.no>229</page.no>
          </talker>
        </talk.start>
      </debateinfo>
    <parav>Please don't find this</parav>
    </debate>
  <continue>
  <parav>Test</parav>
  <debate>
      <debateinfo>
        <title>You're the one that I want</title>
      </debateinfo>
    <talk.start>
      <talker>
        <page.no>229</page.no>
      </talker>
      <para>I should be Prime Minister</para>
    </talk.start>
    <parav>Neighbour Government is the worst, apart from all the others</parav>
  </debate>
  <parav>Government is the worst, apart from all the others</parav>
  <parav>Second government is the worst, apart from all the others</parav>
  <parav>You are</parav>
  <debate>
    <parav>How about those apples</parav>
  </debate>
  <parav>How about them apples</parav>
  <parav>How about these apples</parav>
  </continue>
  </speech>
  ")

the_meaning <- "42"

# Following https://stackoverflow.com/questions/7600960/how-to-check-for-the-immediate-following-sibling-of-parents-in-xpath
uncle_check <-
  xml_find_all(test_hansard, ".//talk.start") %>%
  xml_find_all("../following-sibling::*[name()!='parav'][1]preceding-sibling::parav[preceding-sibling::*]")

neighbours_check <-
  xml_find_all(test_hansard, ".//talk.start") %>%
  # xml_find_all("./following-sibling::*[name()='parav']")
  xml_find_all("./following-sibling::*[name()!='parav'][1]/preceding-sibling::parav[preceding-sibling::*]")

# That . controls how far up it is looking.

if (!is_empty(neighbours_check) & !is_empty(uncle_check)) {
  the_meaning <- "both"
} else if (!is_empty(neighbours_check)) {
  the_meaning <- "neighbour"
} else if (!is_empty(uncle_check)) {
  the_meaning <- "uncle"
} else {
  the_meaning <- "nothing"
}

the_meaning


ancestor_check <-
  xml_find_all(test_hansard, ".//talk.start") %>%
  xml_find_all(".//ancestor::*[position()=1][name()='debate']") %>%
  xml_find_first(".//title") %>% xml_text() %>% as.character()

ancestor_check


####