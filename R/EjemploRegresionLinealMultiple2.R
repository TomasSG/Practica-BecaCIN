# --------------------------------------Bibliotecas------------------------------------

library(readxl)
library(dplyr)
library(ggcorrplot) #ggcorrplot
library(ggplot2)
library(car)

source("./R/Utils.R")

# --------------------------------------Datos------------------------------------

datos_crudos <- read.csv("./data/datasets_383055_741735_CarPrice_Assignment.csv")

# Convertimos en factor las variables categoricas

datos <- datos_crudos %>% 
  mutate(fueltype = as.factor(fueltype))


# --------------------------------------RLM-----------------------------------

# La idea es practicar el uso de variables categoricas asi que en principio no
# vamos a usar todas las variables

# +-------------------------------------------------+
# | PASO 1: Análisis de la relación entre variables |
# +-------------------------------------------------+

datos %>% 
  select(carlength, carwidth, carheight, horsepower, wheelbase, curbweight, enginesize, 
         boreratio, stroke, compressionratio, peakrpm, citympg, highwaympg, price) %>% 
  cor(method = "pearson") %>% 
  ggcorrplot(lab = TRUE)

ggplot(datos, aes(fueltype, price, fill = fueltype)) +
  geom_boxplot() +
  geom_jitter(width = .1)

# +---------------------------+
# | PASO 2: Generar un modelo |
# +---------------------------+

modelo_sin_seleccionar <- lm(price ~ carlength + carwidth + carheight + horsepower + wheelbase + curbweight + 
     enginesize + boreratio + stroke + compressionratio + peakrpm + citympg + 
     highwaympg + fueltype, data = datos)

summary(modelo_sin_seleccionar)

# La interpretación que se le da al B de fueltypegas es en promedio el precio de los
# autos con gas son 0.006334$ superiores a los autos con diesel. 
# Sin embargo, el pbalor obtenido es de 0.265594 por lo que no se puede afirmar
# que este coeficiente es estadísticamente distinto de cero.

# Para el resto de B, el precio medio de los autos aumenta en 1.575 pesos cuando hay un
# incremento de una unidad en el curbweight, manteniendo el resto de variables constantes.

# +----------------------------------------------+
# | PASO 3: Selección de los mejores predictores |
# +----------------------------------------------+

step(modelo_sin_seleccionar, direction = "both")
modelo <- lm(formula = price ~ carwidth + carheight + horsepower + enginesize + 
              stroke + peakrpm + fueltype, data = datos)
summary(modelo)


# +---------------------------------+
# | PASO 4: Validación de supuestos |
# +---------------------------------+

datos %>% 
  select(price, carwidth, carheight, horsepower, enginesize, stroke, peakrpm) %>% 
  cor(method = "pearson") %>% 
  ggcorrplot(lab = TRUE)

df_aux <- data.frame(valores_ajustados = modelo$fitted.values,
                     residuos = modelo$residuals,
                     residuos_student = rstudent(modelo))

ggplot(df_aux, aes(valores_ajustados, residuos_student)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-3, 3), color = "firebrick")# +
  geom_segment(aes(xend = valores_ajustados, y = 0, yend = residuos_student, 
                   color = residuos_student))
  
ggplot(df_aux, aes(sample = residuos)) +
  geom_qq() +
  geom_qq_line()

shapiro.test(df_aux$residuos)

vif(modelo)

# - Relación lineal Y con predictoras: No con todas
# - Residuos se distribuyen alrededor del cero: Si
# - Varianza constante: No, parece que tiene forma cónica
# - Correlación de errores: No
# - Normalidad de residuos: No
# - Multicolinealidad regresores: No (criterio VIF > 10)

# Identificamos los datos atípicos

which(abs(df_aux$residuos_student) > 3)

# Vemos si son influyentes 

summary(influence.measures(modelo))
