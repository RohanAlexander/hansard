# Analysis of Australian Hansard

This contains the scripts and related material that I am putting together for a text analysis of Australian Hansard. The data doesn't seem to be able to be included in this repo, but please get in touch if you are interested.

To start, download the XML files of Hansard from 1901 to 1980. Unpack them, and then run the get_data_from_xml_to_dataframe.R file to convert to a dataframe. 

Notes:

- In the list of parties, the acronym LIB had been used for the Commonwealth Liberal Party of Deakin, Cook etc in 1910s. This was also the acronym used for the Liberal Party of Australia of Menzies etc from the 1940s. Although the parties are related, it's a bit confusing. Has been left as it was, but might be worth changing to avoid confusion?
- ABD says Frederick William Holder was an independent and resigned from his party. Has been left as it was, but maybe check vote records for 1903 and 1906 and update the list of parties?
- A bunch of the parties didn't have exact dates for the change, especially in the 1900s and 1910s. Need to go back to them and update. Look for years that have day and month of 1 January.
- The parties are especially dodgy and need to be checked.


To update:

- Update Patrick Gorman's birthday.
- Michael Danby to retire at next election - update that division at next election.
- Emma Husar to retire at next election - update that division at next election.
- Wayne Swan to retire at next election - update that division at next election.
- ACT redistribution - from 2 to 3 seats - update those divisions at next election.
