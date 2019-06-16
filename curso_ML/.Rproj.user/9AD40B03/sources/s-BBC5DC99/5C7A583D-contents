base_nb <- read.csv("risco-credito.csv")

#instalar o pacoto com o algoritmo do naive-bayes
#install.packages("e1071")

library(e1071) #o algoritmo já vem com a correção laplaciana implementado

classificador_nb = naiveBayes(x = base_nb[1:5], y = base_nb$risco)

#historia 1 : boa, alta, nenhuma, acima_35
historia <- c("boa")
divida <- c("alta")
garantias <- c("nenhuma")
renda <- c("acima_35")

historia_1 <- data.frame(historia, divida, garantias, renda)

previsao_nb <- predict(classificador_nb, newdata = historia_1) #resultado = baixo

historia <- c("ruim")
divida <- c("alta")
garantias <- c("adequada")
renda <- c("0_15")

historia_2 <- data.frame(historia, divida, garantias, renda)

previsao_nb <- predict(classificador_nb, newdata = historia_2, "raw") #resultado = alto



