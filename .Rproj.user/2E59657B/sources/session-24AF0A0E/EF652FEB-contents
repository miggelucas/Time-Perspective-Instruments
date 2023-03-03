library(psych)
library(readr)
library(lavaan)
library(dplyr)


df <- read_csv("Database/ATI_Brazilian.csv")


colunas_aatas <- grep("^aatas", names(df))
df_aatas <- df[, colunas_aatas]

colSums(is.na(df_aatas))
# a contagem de valores NA indica que são raros
# substituindo os valores pela mediana de cada coluna

replace_na_median <- function(x) {
  median_value <- median(x, na.rm = TRUE)
  x[is.na(x)] <- median_value
  return(x)
}

df_aatas_fix <- apply(df_aatas, 2, replace_na_median)

summary(df_aatas_fix )

# Medidas de adequação para EFA

result_bartlett <- cortest.bartlett(df_aatas_fix)
print(result_bartlett$p)
## Significativo para bartlett

result_kmo <- KMO(df_aatas_fix)
print(result_kmo$MSA)
## KMO = 0.919


fa_parrallel <- fa.parallel(df_aatas_fix,
                            cor="poly",
                            n.iter = 100,
                            main = "Análises Paralelas",
                            fa= "fa")
fa_parrallel$fa.values
# scree, eingenvalue e teoria indicam 3 fatores, dados simulados indicam 5; 


efa <- fa(df_aatas_fix, nfactors = 3, n.iter = 25, cor = "poly", rotate = "oblimin", fm = "minres")
fa.diagram(efa)
print(efa)


### itens que compoem o primeiro fator
itens_fa1 <- names(efa$loadings[,1][abs(efa$loadings[,1]) > 0.3])
itens_fa1

## itens que compoem o segundor fator
itens_fa2 <- names(efa$loadings[,2][abs(efa$loadings[,2]) > 0.3])
itens_fa2

## itens que compoem o terceiro fator
itens_fa3 <- names(efa$loadings[,3][abs(efa$loadings[,3]) > 0.3])
itens_fa3

f1_model <- paste(itens_fa1, collapse = ' + ')
f2_model <- paste(itens_fa2, collapse = ' + ')
f3_model <- paste(itens_fa3, collapse = ' + ')

f1_model
f2_model
f3_model

modelo_cfa <- 'f1 =~ aatas2 + aatas5 + aatas8 + aatas11 + aatas14 + aatas17 + aatas20 + aatas23 + aatas26 + aatas29
               f2 =~ aatas3 + aatas6 + aatas9 + aatas12 + aatas15 + aatas18 + aatas21 + aatas24 + aatas27 + aatas30
               f3 =~ aatas1 + aatas7 + aatas10 + aatas13 + aatas16 + aatas19 + aatas22 + aatas28'

cfa <- cfa(df_aatas_fix, model = modelo_cfa)
summary(cfa, fit.measures = TRUE, standardized = TRUE)




      