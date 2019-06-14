basec <- read.csv("census.csv")
basec <- basec[-1]

#busca os valores categoricos existentes na coluna
table(basec$sex)

#troca os atributos de texto por numeros
basec$sex <- factor(basec$sex, levels = c(" Female", " Male"), labels = c(0, 1))

basec$workclass <- factor(basec$workclass, levels = unique(basec$workclass), labels = c(1:ifelse(length(unique(basec$workclass)) > 2, length(unique(basec$workclass)), 2)))
basec$education <- factor(basec$education, levels = unique(basec$education), labels = c(1:ifelse(length(unique(basec$education)) > 2, length(unique(basec$education)), 2)))
basec$marital.status <- factor(basec$marital.status, levels = unique(basec$marital.status), labels = c(1:ifelse(length(unique(basec$marital.status)) > 2, length(unique(basec$marital.status)), 2)))
basec$occupation <- factor(basec$occupation, levels = unique(basec$occupation), labels = c(1:ifelse(length(unique(basec$occupation)) > 2, length(unique(basec$occupation)), 2)))
basec$relationship <- factor(basec$relationship, levels = unique(basec$relationship), labels = c(1:ifelse(length(unique(basec$relationship)) > 2, length(unique(basec$relationship)), 2)))
basec$race <- factor(basec$race, levels = unique(basec$race), labels = c(1:ifelse(length(unique(basec$race)) > 2, length(unique(basec$race)), 2)))
basec$native.country <- factor(basec$native.country, levels = unique(basec$native.country), labels = c(1:ifelse(length(unique(basec$native.country)) > 2, length(unique(basec$native.country)), 2)))
basec$income <- factor(basec$income, levels = unique(basec$income), labels = c(1:length( unique(basec$income))))
