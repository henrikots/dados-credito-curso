base <- read.csv("risco-credito.csv")

base <- base[base$risco != "moderado",]

#utilizar o pacote já presente no R
classificador = glm(formula = risco ~ ., family = binomial, data = base)

#historia 1 : boa, alta, nenhuma, acima_35
historia <- c("boa", "ruim")
divida <- c("alta", "alta")
garantias <- c("nenhuma", "adequada")
renda <- c("acima_35", "0_15")

historia_1 <- data.frame(historia, divida, garantias, renda)

probabilidades <- predict(classificador, type = 'response', newdata = historia_1)
respostas <- ifelse(probabilidades > 0.5, "baixa", "alta")
