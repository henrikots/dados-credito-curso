base_rc <- read.csv("risco-credito.csv")

#instalar o pacote com o algoritmo da arvore de decisao
#install.packages("rpart")

library(rpart) 

classificador_ad =rpart(formula = risco ~ ., data = base_rc, control = rpart.control(minbucket = 1))

plot(classificador_ad)
text(classificador_ad)

#install.packages("rpart.plot")  
library(rpart.plot)

rpart.plot(classificador_ad)

#historia 1 : boa, alta, nenhuma, acima_35
historia <- c("boa", "ruim")
divida <- c("alta", "alta")
garantias <- c("nenhuma", "adequada")
renda <- c("acima_35", "0_15")

historia_1 <- data.frame(historia, divida, garantias, renda)

previsao_ad <- predict(classificador_ad, newdata = historia_1) #resultado = baixo

print(previsao_ad)


