base <- read.csv("credit-data.csv")

base$clientid <- NULL

summary(base)

idade_invalida <- base[base$age < 0 & !is.na(base$age),]

mean(base$age[base$age > 0], na.rm = TRUE)

#ajustar base inconsistente com média do valor
base$age <- ifelse(base$age < 0, 40.92, base$age)

#ajusta valores faltantes com a média
base$age <- ifelse(is.na(base$age), mean(base$age, na.rm = TRUE) ,base$age)

#normalização
# x = (x - min(x)) / (max(x) - min(x))

#padronização - mais recomendado por tratar os outliers
# x = (x - media(x)) / (desvio_padrao(x)) 

base[, 1:3] <- scale(base[, 1:3]) 

#install.packages("caTools")
library(caTools)

set.seed(1)

divisao <- sample.split(base$default, SplitRatio = 0.75)

base_treinamento <- subset(base, divisao == TRUE)
base_teste <- subset(base, divisao == FALSE)

#install.packages("h2o")

library(h2o)

h2o.init(nthreads = -1) 

classificador <- h2o.deeplearning(y = "default", 
                                  training_frame = as.h2o(base_treinamento),
                                  activation = "Rectifier",
                                  hidden = c(100),
                                  epochs = 1000)

previsoes <- h2o.predict(classificador, newdata = as.h2o(base_teste[-4]))

previsoes <- (previsoes > 0.5)

matriz_confusao <- table(as.vector(previsoes), base_teste[,4])

library(caret)

confusionMatrix(matriz_confusao) #99.2%
