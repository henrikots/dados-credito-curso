base <- read.csv("credit-data.csv")

base$clientid <- NULL

summary(base)

idade_invalida <- base[base$age < 0 & !is.na(base$age),]

mean(base$age[base$age > 0], na.rm = TRUE)

#ajustar base inconsistente com média do valor
base$age <- ifelse(base$age < 0, 40.92, base$age)

#ajusta valores faltantes com a média
base$age <- ifelse(is.na(base$age), mean(base$age) ,base$age)