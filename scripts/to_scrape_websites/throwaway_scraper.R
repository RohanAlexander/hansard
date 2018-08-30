library(tidyverse)
library(rvest)

lego_movie <- read_html("http://parlinfo.aph.gov.au/parlInfo/search/summary/summary.w3p;adv=yes;orderBy=_fragment_number,doc_date-rev;page=0;query=Dataset%3Ahansardr,hansardr80%20Decade%3A%222000s%22;resCount=Default")



data <- tibble()

lego_movie_1 <- read_html("http://parlinfo.aph.gov.au/parlInfo/search/summary/summary.w3p;adv=yes;orderBy=_fragment_number,doc_date-rev;page=0;query=Dataset%3Ahansardr,hansardr80%20Decade%3A%222000s%22;resCount=Default")


data <- lego_movie %>%
  html_nodes("#results a") %>%
  html_attr('href') %>% 
  as_tibble() %>% 
  rbind(data)



pages <- c("http://parlinfo.aph.gov.au/parlInfo/search/summary/summary.w3p;adv=yes;orderBy=_fragment_number,doc_date-rev;page=2;query=Dataset%3Ahansardr,hansardr80%20Decade%3A%222010s%22;resCount=Default",
           "http://parlinfo.aph.gov.au/parlInfo/search/summary/summary.w3p;adv=yes;orderBy=_fragment_number,doc_date-rev;page=3;query=Dataset%3Ahansardr,hansardr80%20Decade%3A%222010s%22;resCount=Default",
           "http://parlinfo.aph.gov.au/parlInfo/search/summary/summary.w3p;adv=yes;orderBy=_fragment_number,doc_date-rev;page=4;query=Dataset%3Ahansardr,hansardr80%20Decade%3A%222010s%22;resCount=Default",
           "http://parlinfo.aph.gov.au/parlInfo/search/summary/summary.w3p;adv=yes;orderBy=_fragment_number,doc_date-rev;page=5;query=Dataset%3Ahansardr,hansardr80%20Decade%3A%222010s%22;resCount=Default",
           "http://parlinfo.aph.gov.au/parlInfo/search/summary/summary.w3p;adv=yes;orderBy=_fragment_number,doc_date-rev;page=6;query=Dataset%3Ahansardr,hansardr80%20Decade%3A%222010s%22;resCount=Default",
           "http://parlinfo.aph.gov.au/parlInfo/search/summary/summary.w3p;adv=yes;orderBy=_fragment_number,doc_date-rev;page=7;query=Dataset%3Ahansardr,hansardr80%20Decade%3A%222010s%22;resCount=Default",
           "http://parlinfo.aph.gov.au/parlInfo/search/summary/summary.w3p;adv=yes;orderBy=_fragment_number,doc_date-rev;page=8;query=Dataset%3Ahansardr,hansardr80%20Decade%3A%222010s%22;resCount=Default",
           "http://parlinfo.aph.gov.au/parlInfo/search/summary/summary.w3p;adv=yes;orderBy=_fragment_number,doc_date-rev;page=9;query=Dataset%3Ahansardr,hansardr80%20Decade%3A%222010s%22;resCount=Default",
           "http://parlinfo.aph.gov.au/parlInfo/search/summary/summary.w3p;adv=yes;orderBy=_fragment_number,doc_date-rev;page=10;query=Dataset%3Ahansardr,hansardr80%20Decade%3A%222010s%22;resCount=Default",
           "http://parlinfo.aph.gov.au/parlInfo/search/summary/summary.w3p;adv=yes;orderBy=_fragment_number,doc_date-rev;page=11;query=Dataset%3Ahansardr,hansardr80%20Decade%3A%222010s%22;resCount=Default",
           "http://parlinfo.aph.gov.au/parlInfo/search/summary/summary.w3p;adv=yes;orderBy=_fragment_number,doc_date-rev;page=12;query=Dataset%3Ahansardr,hansardr80%20Decade%3A%222010s%22;resCount=Default",
           "http://parlinfo.aph.gov.au/parlInfo/search/summary/summary.w3p;adv=yes;orderBy=_fragment_number,doc_date-rev;page=13;query=Dataset%3Ahansardr,hansardr80%20Decade%3A%222010s%22;resCount=Default",
           "http://parlinfo.aph.gov.au/parlInfo/search/summary/summary.w3p;adv=yes;orderBy=_fragment_number,doc_date-rev;page=14;query=Dataset%3Ahansardr,hansardr80%20Decade%3A%222010s%22;resCount=Default",
           "http://parlinfo.aph.gov.au/parlInfo/search/summary/summary.w3p;adv=yes;orderBy=_fragment_number,doc_date-rev;page=15;query=Dataset%3Ahansardr,hansardr80%20Decade%3A%222010s%22;resCount=Default",
           "http://parlinfo.aph.gov.au/parlInfo/search/summary/summary.w3p;adv=yes;orderBy=_fragment_number,doc_date-rev;page=16;query=Dataset%3Ahansardr,hansardr80%20Decade%3A%222010s%22;resCount=Default",
           "http://parlinfo.aph.gov.au/parlInfo/search/summary/summary.w3p;adv=yes;orderBy=_fragment_number,doc_date-rev;page=17;query=Dataset%3Ahansardr,hansardr80%20Decade%3A%222010s%22;resCount=Default",
           "http://parlinfo.aph.gov.au/parlInfo/search/summary/summary.w3p;adv=yes;orderBy=_fragment_number,doc_date-rev;page=18;query=Dataset%3Ahansardr,hansardr80%20Decade%3A%222010s%22;resCount=Default",
           "http://parlinfo.aph.gov.au/parlInfo/search/summary/summary.w3p;adv=yes;orderBy=_fragment_number,doc_date-rev;page=19;query=Dataset%3Ahansardr,hansardr80%20Decade%3A%222010s%22;resCount=Default",
           "http://parlinfo.aph.gov.au/parlInfo/search/summary/summary.w3p;adv=yes;orderBy=_fragment_number,doc_date-rev;page=20;query=Dataset%3Ahansardr,hansardr80%20Decade%3A%222010s%22;resCount=Default",
           "http://parlinfo.aph.gov.au/parlInfo/search/summary/summary.w3p;adv=yes;orderBy=_fragment_number,doc_date-rev;page=21;query=Dataset%3Ahansardr,hansardr80%20Decade%3A%222010s%22;resCount=Default",
           "http://parlinfo.aph.gov.au/parlInfo/search/summary/summary.w3p;adv=yes;orderBy=_fragment_number,doc_date-rev;page=22;query=Dataset%3Ahansardr,hansardr80%20Decade%3A%222010s%22;resCount=Default",
           "http://parlinfo.aph.gov.au/parlInfo/search/summary/summary.w3p;adv=yes;orderBy=_fragment_number,doc_date-rev;page=23;query=Dataset%3Ahansardr,hansardr80%20Decade%3A%222010s%22;resCount=Default",
           "http://parlinfo.aph.gov.au/parlInfo/search/summary/summary.w3p;adv=yes;orderBy=_fragment_number,doc_date-rev;page=24;query=Dataset%3Ahansardr,hansardr80%20Decade%3A%222010s%22;resCount=Default",
           "http://parlinfo.aph.gov.au/parlInfo/search/summary/summary.w3p;adv=yes;orderBy=_fragment_number,doc_date-rev;page=25;query=Dataset%3Ahansardr,hansardr80%20Decade%3A%222010s%22;resCount=Default",
           "http://parlinfo.aph.gov.au/parlInfo/search/summary/summary.w3p;adv=yes;orderBy=_fragment_number,doc_date-rev;page=26;query=Dataset%3Ahansardr,hansardr80%20Decade%3A%222010s%22;resCount=Default",
           "http://parlinfo.aph.gov.au/parlInfo/search/summary/summary.w3p;adv=yes;orderBy=_fragment_number,doc_date-rev;page=27;query=Dataset%3Ahansardr,hansardr80%20Decade%3A%222010s%22;resCount=Default",
           "http://parlinfo.aph.gov.au/parlInfo/search/summary/summary.w3p;adv=yes;orderBy=_fragment_number,doc_date-rev;page=28;query=Dataset%3Ahansardr,hansardr80%20Decade%3A%222010s%22;resCount=Default",
           "http://parlinfo.aph.gov.au/parlInfo/search/summary/summary.w3p;adv=yes;orderBy=_fragment_number,doc_date-rev;page=29;query=Dataset%3Ahansardr,hansardr80%20Decade%3A%222010s%22;resCount=Default",
           "http://parlinfo.aph.gov.au/parlInfo/search/summary/summary.w3p;adv=yes;orderBy=_fragment_number,doc_date-rev;page=30;query=Dataset%3Ahansardr,hansardr80%20Decade%3A%222010s%22;resCount=Default",
           "http://parlinfo.aph.gov.au/parlInfo/search/summary/summary.w3p;adv=yes;orderBy=_fragment_number,doc_date-rev;page=31;query=Dataset%3Ahansardr,hansardr80%20Decade%3A%222010s%22;resCount=Default",
           "http://parlinfo.aph.gov.au/parlInfo/search/summary/summary.w3p;adv=yes;orderBy=_fragment_number,doc_date-rev;page=32;query=Dataset%3Ahansardr,hansardr80%20Decade%3A%222010s%22;resCount=Default",
           "http://parlinfo.aph.gov.au/parlInfo/search/summary/summary.w3p;adv=yes;orderBy=_fragment_number,doc_date-rev;page=33;query=Dataset%3Ahansardr,hansardr80%20Decade%3A%222010s%22;resCount=Default",
           "http://parlinfo.aph.gov.au/parlInfo/search/summary/summary.w3p;adv=yes;orderBy=_fragment_number,doc_date-rev;page=34;query=Dataset%3Ahansardr,hansardr80%20Decade%3A%222010s%22;resCount=Default",
           "http://parlinfo.aph.gov.au/parlInfo/search/summary/summary.w3p;adv=yes;orderBy=_fragment_number,doc_date-rev;page=35;query=Dataset%3Ahansardr,hansardr80%20Decade%3A%222010s%22;resCount=Default")

pages <- pages[1:2]
pages <- pages[3:length(pages)]

for (i in seq_along(pages)){
  Sys.sleep(10)
  data <- read_html(pages[i]) %>% 
  html_nodes("#results a") %>%
  html_attr('href') %>% 
  as_tibble() %>% 
  rbind(data)
}

write_csv(data, "links2.csv")