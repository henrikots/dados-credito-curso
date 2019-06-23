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


library(e1071)
classificador = svm(formula = default ~ ., data = base_treinamento, type = "C-classification", 
                    kernel = "radial", cost = 8)

previsoes = predict(classificador, newdata = base_teste[-4])


matriz_confusao = table(base_teste[,4], previsoes)

library(caret)
confusionMatrix(matriz_confusao)

#linear = 94.8%
#polinomyal = 96.5%
#radial = 97.8%
#sigmoid = 84.6% 

#ajuste de custo para o radial
#0.2 = 97.2%
#5 = 98.8%
#8 = 99%