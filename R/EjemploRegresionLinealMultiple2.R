# --------------------------------------Bibliotecas------------------------------------

library(readxl)
library(dplyr)
library(ggcorrplot) #ggcorrplot

# --------------------------------------Datos------------------------------------

datos_crudos <- read.csv("./data/datasets_383055_741735_CarPrice_Assignment.csv")

# Convertimos en factor las variables categoricas

datos <- datos_crudos %>% 
  mutate(fueltype = as.factor(fueltype))


# --------------------------------------RLM-----------------------------------

# La idea es practicar el uso de variables categoricas asi que en principio no
# vamos a usar todas las variables

# Empezamos analizando si existe relación lineal entre nuestras variables

datos %>% 
  select(carlength, carwidth, carheight, horsepower, wheelbase, curbweight, enginesize, 
         boreratio, stroke, compressionratio, peakrpm, citympg, highwaympg, price) %>% 
  cor(method = "pearson") %>% 
  ggcorrplot(lab = TRUE)

ggplot(datos, aes(fueltype, price, fill = fueltype)) +
  geom_boxplot() +
  geom_jitter(width = .1)

# Genearmos el modelo con las variables anteriores

modelo <- lm(price ~ carlength + carwidth + carheight + horsepower, wheelbase + curbweight + 
     enginesize + boreratio + stroke + compressionratio + peakrpm + citympg + 
     highwaympg + fueltype, data = datos)


