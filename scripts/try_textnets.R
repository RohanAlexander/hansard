library(textnets)


data("sotu")

sotu_firsts <- sotu %>% group_by(president) %>% slice(1L)

sotu_firsts_nouns <- PrepText(sotu_firsts, groupvar = "president", textvar = "sotu_text", node_type = "groups", tokenizer = "words", pos = "nouns", remove_stop_words = TRUE, compound_nouns = TRUE)

# sotu_firsts_sentiment <- PrepTextSent(sotu_firsts, groupvar = "president", textvar = "sotu_text", node_type = "groups", tokenizer = "words", sentiment_lexicon = "afinn", language = "english", udmodel_lang = udmodel_lang, remove_numbers = NULL, compound_nouns = TRUE)

sotu_firsts_network <- CreateTextnet(sotu_firsts_nouns)

VisTextNet(sotu_firsts_network, label_degree_cut = 0)