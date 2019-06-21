base <- read.csv("credit-data.csv")

#remove a coluna cliente
base$clientid <- NULL

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

#install.packages('class')
library(class)

previsoes = knn(train = base_treinamento[, -4], test = base_teste[, -4], cl = base_treinamento$default, k = 5)

matriz_confusao = table(base_teste$default, previsoes)

library(caret)
confusionMatrix(matriz_confusao) #sem escalonamento #83.4% , com escalonamento = 97.4%
