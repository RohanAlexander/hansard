library(stm)
library(tidyverse)

data <- read_csv("scripts/poliblogs2008.csv")

data$randomJumps <- sample(c(0, 1), nrow(data), replace = TRUE)
data$jumpAfterFirstHalf <- c(rep(0,nrow(data)/2), rep(1,nrow(data)/2))
data$counter <- 

processed <- textProcessor(data$documents, metadata = data)

out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

docs <- out$documents
vocab <- out$vocab
meta <- out$meta


poliblogPrevFit <- stm(documents = out$documents, vocab = out$vocab, K = 20, prevalence =~ randomJumps + jumpAfterFirstHalf + s(day), max.em.its = 75, data = out$meta, init.type = "Spectral")

poliblogPrevFit_nos <- stm(documents = out$documents, vocab = out$vocab, K = 20, prevalence =~ randomJumps + jumpAfterFirstHalf + day, max.em.its = 75, data = out$meta, init.type = "Spectral")


labelTopics(poliblogPrevFit, c(3, 7, 20))

thoughts3 <- findThoughts(poliblogPrevFit, texts = data$documents, n = 2, topics = 3)$docs[[1]]

out$meta$rating <- as.factor(out$meta$rating)

prep <- estimateEffect(1:20 ~ randomJumps + jumpAfterFirstHalf + s(day), poliblogPrevFit, meta = out$meta, uncertainty = "Global")

summary(prep, topics=1)



prep_nos <- estimateEffect(1:20 ~ randomJumps + jumpAfterFirstHalf + day, poliblogPrevFit_nos, meta = out$meta, uncertainty = "Global")

summary(prep_nos, topics=1)
