# Analysis of Australian Hansard

This contains the scripts and related material that I am putting together for a text analysis of Australian Hansard. The Hansard dataset doesn't seem to be able to be included in this repo, but please get in touch if you are interested.

To start, download the XML files of Hansard from 1901 to 1980. Unpack them, and then run the get_data_from_xml_to_dataframe.R file to convert to a dataframe. After this you need to try to fix as many of the typos, mistakes, and transcription errors in Hansard as possible, by running clean_hansard_spelling.R. 

The data associated with the Hansard statements (politician name, party, etc) is also full of issues. I found it easier to start with the tables provided in the list of historical members in the Parliamentary Handbook and to fix the mistakes in that, instead of fixing the list implied by Hansard. So grab that list from parliament website and split it into constituent parts, then you can run clean_politicians_by_divisions.R, clean_politicians_by_individuals.R and clean_politicians_by_party.R. The result of all that what seems to be a reasonably accurate list of all the politicians, divisions and parties.

After this, you can try some topic modelling, using topic_modelling_LDA.R.



Notes:

- In the list of parties, the acronym LIB had been used for the Commonwealth Liberal Party of Deakin, Cook etc in 1910s. This was also the acronym used for the Liberal Party of Australia of Menzies etc from the 1940s. Although the parties are related, it's a bit confusing. Has been left as it was, but might be worth changing to avoid confusion?

To update:

- Turnbull/Wentworth/etc.
- Fix the way that clean_hansard_spelling works so that it is looking up a table instead of all in the script.
- ABD says Frederick William Holder was an independent and resigned from his party. Has been left as it was, but maybe check vote records for 1903 and 1906 and update the list of parties?
- A bunch of the parties didn't have exact dates for the change, especially in the 1900s and 1910s. Need to go back to them and update. Look for years that have day and month of 1 January.
- The parties are especially dodgy and need to be checked.
- Can we split out date of announcing retirement/resignation and the actual date of not longer being the member. Otherwise we just get a bunch leaving at the election, even if they announced it much earlier. Also the treatment of this seems to be inconsistent so would be good to check/fix.

Next election:

- Retirements:
  + Kate Ellis (Adelaide) announced 9 March 2017
  + Wayne Swan (Lilley) announced 10 February 2018
  + Michael Danby (Melbourne Ports) announced 5 July 2018
  + Jenny Macklin (Jagajaga) announced 6 July 2018
  + Emma Husar (Lindsay) announced 8 August 2018
  + Luke Hartsuyker (Cowper) announced 8 August 2018
  + Gai Brodtmann (Canberra) announced 13 August 2018
- Division changes:
  + Victoria gets another - Fraser
  + ACT gets another - Bean
  + South Australia reduced - Port Adelaide removed
- Divisions renaming:
  + Batman -> Cooper
  + McMillan -> Monash
  + Melbourne Ports -> Macnamara
  + Murray -> Nicholls
  + Wakefield -> Spence
  + Denison -> Clark