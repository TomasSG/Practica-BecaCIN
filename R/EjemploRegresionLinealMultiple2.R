# --------------------------------------Bibliotecas------------------------------------

library(readxl)
library(dplyr)

# --------------------------------------Datos------------------------------------

datos_crudos <- read.csv("./data/datasets_383055_741735_CarPrice_Assignment.csv")

# Convertimos en factor las variables categoricas

datos <- datos_crudos %>% 
  mutate(fueltype = as.factor(fueltype))


# --------------------------------------RLM-----------------------------------

# La idea es practicar el uso de variables categoricas asi que en principio no
# vamos a usar todas las variables

