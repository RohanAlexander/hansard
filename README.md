# Analysis of Australian Hansard

TEST

This contains data, scripts, and related material for analysing Australia's Hansard.

## Getting started
To start, download the XML files of Hansard from 1901 to 1980 from Tim Sherratt at University of Canberra. Unpack them, and then run the get_data_from_xml_to_dataframe.R file to convert to a dataframe. After this you need to try to fix as many of the typos, mistakes, and transcription errors in Hansard as possible, by running clean_hansard_spelling.R. 

The data associated with the Hansard statements (politician name, party, etc) is also full of issues. I found it easier to start with the tables provided in the list of historical members in the Parliamentary Handbook and to fix the mistakes in that, instead of fixing the list implied by Hansard. So grab that list from parliament website and split it into constituent parts, then you can run clean_politicians_by_divisions.R, clean_politicians_by_individuals.R and clean_politicians_by_party.R. The result of all that what seems to be a reasonably accurate list of all the politicians, divisions and parties - available in the outputs folder. Just from this you can make some nice graphs, for instance:

[Age at first election to House of Reps](outputs/figures/age_at_election.pdf)

After this, you can try some topic modelling, using topic_modelling_LDA.R.

## Next steps
Once you've gotten started you'll probably want the complete period. Unfortunately, the 1981 - 1997 records aren't available in XML (someone at Hansard said it's because the XML doesn't exist for those years). You can get the 1997 onwards XML from a combination of Open Australia and Andrew Turpin at University of Melbourne. So one option is to just have a gap in your analysis. But fortunately, the PDFs are available. So scrape those from the Hansard website using get_80s_and_90s_PDFs.R. Then parse the PDFs using parse_each_pdf_and_save_csv_tm.R and get some text output. You can then run topic modelling on the whole period using topic_modelling.R.

The issue is that each data source is wrong in it's own way. Also, neither was really designed for topic analysis. In some sense, it might be better to just start with the universe of Australia Hansard and then do everything. So get all of the Hansard PDFs using get_1901-1980_PDFs.R (use get_URLs_of_hansard_PDFs.R to help you get the csv that you need to walk2 over). That's going to take a long weekend or so and many GB in storage.

Then you can parse the PDFs with parse_each_pdf_and_save_csv_pdftools.R. Then create a dataframe with the words in each days Hansard using make_topic_modelling_text_input.R.



## Notes
- HoR voting: ACT only split out from 1949 election, and NT only split out from 1922 election.
- Maybe this PDF is corrupted for Commonwealth - no CSV output - 1918-06-14.
- Graph of the dates is interesting - in the early days they seemed to sit in a quite bunched up way. But these days spread it out more.
- Replace the PDF for: 1985-08-23.pdf, 1992-09-10.pdf, 1996-12-13.pdf, 1938-11-23.pdf
- Add a graph of the stop words over time - looking for no change over time
- Look at network of call outs between politicians. Does it change over time? Does it change at elections? Has it become more partisan (could be done with sentiment analysis of the call out)?
- Look at whether the politicians have become more national - did WA politicians used to concentrate on WA issues and do that less now?
- Victoria: From 49th Parliament, volume 366, September 1982 LC and LA are split out. Before this they are in same PDF - sigh.
- Victoria: Have really just focused on the main content - not the questions on notice and stuff - this becomes relevant for later years.
- Check that airline crash in the 40s is being accurately accounted for in the politicians data.
- In the list of parties, the acronym LIB had been used for the Commonwealth Liberal Party of Deakin, Cook etc in 1910s. This was also the acronym used for the Liberal Party of Australia of Menzies etc from the 1940s. Although the parties are related, it's a bit confusing. Has been left as it was, but might be worth changing to avoid confusion?

To update:
- Turnbull/Wentworth/etc.
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
  + Julia Banks (Chisholm) announced 29 August 2018
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
