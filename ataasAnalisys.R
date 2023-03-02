library(psych)
library(readr)
library(lavaan)
library(dplyr)


df <- read_csv("ATI_Brazilian.csv")


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

