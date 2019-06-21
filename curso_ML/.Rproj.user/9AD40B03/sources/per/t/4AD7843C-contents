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

#classificador do algoritmo de regressao logistica
classificador <- glm(formula = default ~ ., family = binomial, data = base_treinamento )


probabilidades <- predict(classificador, type = "response", newdata = base_teste[, -4])
previsoes <- ifelse(probabilidades > 0.5, 1, 0)

matriz_confusao <- table(base_teste[,4], previsoes)

library(caret)

confusionMatrix(matriz_confusao) #95%
