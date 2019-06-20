library(rpart)
library(caTools)

base <- read.csv("credit-data.csv")

base$clientid <- NULL

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

#transforma o classificador default em factor
base$default = factor(base$default, levels = c(0, 1), labels = c(0, 1))

divisao <- sample.split(base$default, SplitRatio = 0.75)

#dividir a base em treinamento e teste
base_treinamento <- subset(base, divisao == TRUE)
base_teste <- subset(base, divisao == FALSE)


#install.packages("RoughSets")
library(RoughSets)

#é necessário criar uma decision table

dt_treinamento = SF.asDecisionTable(dataset = base_treinamento, decision.attr = 4)
dt_teste = SF.asDecisionTable(dataset = base_teste, decision.attr = 4)

'discretização, criar faixas de valores parao uso no algoritmo'
intervalos = D.discretization.RST(dt_treinamento, nOfIntervals = 4)
dt_treinamento = SF.applyDecTable(dt_treinamento, intervalos)
dt_teste = SF.applyDecTable(dt_teste, intervalos)

classificador <- RI.CN2Rules.RST(dt_treinamento, K = 5)

print(classificador)

previsoes <- predict(classificador, newdata = dt_teste[-4])

matriz_confusao <- table(dt_teste[,4], unlist(previsoes))

library(caret)

confusionMatrix(matriz_confusao)# 92.4%
