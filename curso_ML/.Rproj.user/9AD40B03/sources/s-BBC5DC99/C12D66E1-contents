base = read.csv("risco-credito.csv")

#install.packages("OneR")
library(OneR)

classificador <- OneR(base)

print(classificador)

#historia 1 : boa, alta, nenhuma, acima_35
historia <- c("boa", "ruim")
divida <- c("alta", "alta")
garantias <- c("nenhuma", "adequada")
renda <- c("acima_35", "0_15")

historia_1 <- data.frame(historia, divida, garantias, renda)

previsao_or <- predict(classificador, newdata = historia_1)
print(previsao_or)
