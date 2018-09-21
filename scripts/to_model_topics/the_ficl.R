install.packages("vars")
library(vars)

gammas <- read_csv("/Users/rohanalexander/Desktop/test_gammas.csv")

data("Canada")
head(Canada)

gammas_test <- spread(gammas, topic, gamma)

head(gammas_test)
tail(gammas_test)

head(Canada)

?VAR(gammas_test, p = 2, type = "none")



#### 
install.packages("betareg")
library(betareg)

test <- betareg()




####

install.packages("nnet")
library(nnet)
glm.fit = ?multinom(direccion~., data=datos)
summary(glm.fit)
#Prediction
predict(glm.fit, newdata, "probs")






####
install.packages("mlogit")
library(mlogit)
gammas <- read_csv("/Users/rohanalexander/Desktop/test_gammas.csv")
head(gammas)
gamma_data <- mlogit.data(gammas, 
                          choice = gamma,
                          shape = long,
                          varying = 
                          id.var = 
                            
                            varying = c(2:9), 
                          shape = "wide", choice =
              "mode")

f <- mFormula(choice ~ vcost | income + size | travel)