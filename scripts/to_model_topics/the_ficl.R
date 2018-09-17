install.packages("vars")
library(vars)

gammas <- read_csv("/Users/rohanalexander/Desktop/test_gammas.csv")

data("Canada")
head(Canada)

gammas_test <- spread(gammas, topic, gamma)

head(gammas_test)
tail(gammas_test)

head(Canada)

VAR(gammas_test, p = 2, type = "none")



install.packages("nnet")
library(nnet)



glm.fit = multinom(direccion~., data=datos)
summary(glm.fit)
#Prediction
predict(glm.fit, newdata, "probs")