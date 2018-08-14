# Analysis of Australian Hansard

This contains the scripts and related material that I am putting together for a text analysis of Australian Hansard. The Hansard dataset doesn't seem to be able to be included in this repo, but please get in touch if you are interested.

To start, download the XML files of Hansard from 1901 to 1980. Unpack them, and then run the get_data_from_xml_to_dataframe.R file to convert to a dataframe. After this you need to try to fix as many of the typos, mistakes, and transcription errors in Hansard as possible, by running clean_hansard_spelling.R. 

The data associated with the Hansard statements (politician name, party, etc) is also full of issues. I found it easier to start with the tables provided in the list of historical members in the Parliamentary Handbook and to fix the mistakes in that, instead of fixing the list implied by Hansard. So grab that list from parliament website and split it into constituent parts, then you can run clean_politicians_by_divisions.R, clean_politicians_by_individuals.R and clean_politicians_by_party.R. The result of all that what seems to be a reasonably accurate list of all the politicians, divisions and parties.

After this, you can try some topic modelling, using topic_modelling_LDA.R.



Notes:

- In the list of parties, the acronym LIB had been used for the Commonwealth Liberal Party of Deakin, Cook etc in 1910s. This was also the acronym used for the Liberal Party of Australia of Menzies etc from the 1940s. Although the parties are related, it's a bit confusing. Has been left as it was, but might be worth changing to avoid confusion?

To update:

- ABD says Frederick William Holder was an independent and resigned from his party. Has been left as it was, but maybe check vote records for 1903 and 1906 and update the list of parties?
- A bunch of the parties didn't have exact dates for the change, especially in the 1900s and 1910s. Need to go back to them and update. Look for years that have day and month of 1 January.
- The parties are especially dodgy and need to be checked.
- Fix the way that clean_hansard_spelling works so that it is looking up a table instead of all in the script.
- Update Patrick Gorman's birthday.
- Michael Danby to retire at next election - update that division at next election.
- Emma Husar to retire at next election - update that division at next election.
- Wayne Swan to retire at next election - update that division at next election.
- Gai Brodtmann to retire at next election - update that division at next election.
- ACT redistribution - from 2 to 3 seats - update those divisions at next election.
- Can we split out date of announcing retirement/resignation and the actual date of not longer being the member. Otherwise we just get a bunch leaving at the election, even if they announced it much earlier. Also the treatment of this seems to be inconsistent so would be good to check/fix.
