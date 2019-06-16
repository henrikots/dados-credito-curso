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

classificador_ad_credit_data <- rpart(formula = default ~ ., base_treinamento)
  
previsoes_ad_credit_data <- predict(classificador_ad_credit_data, newdata = base_teste[-4])

previsoes_ad_credit_data <- predict(classificador_ad_credit_data, newdata = base_teste[-4], type = "class")


#cria uma matriz de confusao para fazer um comparativo
matriz_confusao = table(base_teste[, 4], previsoes_ad_credit_data) #93.6% de acerto

library(caret)
confusionMatrix(matriz_confusao)
