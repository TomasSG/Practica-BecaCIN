# --------------------------------------Bibliotecas------------------------------------

library(readxl)
library(dplyr)
library(ggcorrplot) #ggcorrplot
library(ggplot2)
library(car)
library(ggpubr) #ggarrange
library(sjPlot) #tab_model

source("./R/Utils.R")

# --------------------------------------Datos------------------------------------

datos_crudos <- read.csv("./data/datasets_383055_741735_CarPrice_Assignment.csv")

# Nos quedamos con las variables de interes, cambiamos los nombres y 
# transformamos a factor

# NOTA: Reduje la cantidad de variables para que sea más manejable 
# la info, me quede con los que sabia su significado

datos <- datos_crudos %>% 
  select(CarName, fueltype, carlength, carwidth, carheight, enginesize, horsepower,
         peakrpm, citympg, highwaympg, price) %>% 
  rename(nombre = CarName, tipo_combustible = fueltype, largo = carlength,
         ancho = carwidth, alto = carheight, tamanio_motor = enginesize,
         poder = horsepower, maxima_rpm = peakrpm, vel_ciudad = citympg,
         vel_ruta = highwaympg, precio = price) %>% 
  mutate(tipo_combustible = as.factor(tipo_combustible))


# --------------------------------------RLM-----------------------------------

# +-------------------------------------------------+
# | PASO 1: Análisis de la relación entre variables |
# +-------------------------------------------------+

# Obtenemos la matriz de correlación

datos %>% 
  select(largo, ancho, alto, tamanio_motor, poder, maxima_rpm, vel_ciudad,
         vel_ruta, precio) %>% 
  cor(method = "pearson") %>% 
  ggcorrplot(lab = TRUE)

# Graficamos VD contra VI para las que el r es alto

g <- ggplot(datos, aes(x = precio))

g1 <- g + geom_point(aes(y = largo)) 
g2 <- g + geom_point(aes(y = ancho)) 
g3 <- g + geom_point(aes(y = tamanio_motor)) 
g4 <- g + geom_point(aes(y = vel_ciudad)) 
g5 <- g + geom_point(aes(y = vel_ruta))

arrange_r_alto <- ggarrange(g1, g2, g3, g4, g5, nrow = 2, ncol = 3)

annotate_figure(arrange_r_alto, top = text_grob("Diagramas dispersión de VI con r grande"))

# Graficamos VD contra VI para las que el r es bajo


g1 <- g + geom_point(aes(y = alto)) 
g2 <- g + geom_point(aes(y = maxima_rpm)) 

arrange_r_bajo <- ggarrange(g1, g2, nrow = 2, ncol = 1)

annotate_figure(arrange_r_bajo, top = text_grob("Diagramas dispersión de VI con r chico"))

# +---------------------------+
# | PASO 2: Generar un modelo |
# +---------------------------+


modelo_it0 <- lm(precio ~ largo + ancho + alto +
                               poder + maxima_rpm + vel_ciudad + vel_ruta +
                               tipo_combustible, data = datos)

summary(modelo_it0)

# Para obtener un cuadro más lindo

tab_model(modelo_it0)

# +----------------------------------------------+
# | PASO 3: Selección de los mejores predictores |
# +----------------------------------------------+

# Usamos un stepwise en ambas direcciones

step(modelo_it0, direction = "both")

# Calulamos el nuevo modelo

modelo_it1 <- lm(formula = precio ~ ancho + alto + tamanio_motor + poder + 
           maxima_rpm + tipo_combustible, data = datos)

tab_model(modelo_it1)

data.frame(VIF = vif(modelo_it1))

# segunda iteracion

modelo_it2 <- lm(formula = precio ~ ancho + alto + poder + 
                   maxima_rpm + tipo_combustible, data = datos)

tab_model(modelo_it2)

# Tercera iteración

modelo_it3 <- lm(formula = precio ~ ancho + poder + tipo_combustible, data = datos)

tab_model(modelo_it3)

data.frame(VIF = vif(modelo_it3))

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
  geom_hline(yintercept = c(-3, 3), color = "firebrick")
  
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

# Pareciera que la muestra nro 50 es influyente. Vemos como cambia el modelo si la sacamos

modelo_ajustado <- lm(formula = price ~ carwidth + carheight + horsepower + enginesize + 
               stroke + peakrpm + fueltype, data = datos[-50,])
summary(modelo_ajustado)

# Validamos los supuestos

df_aux_ajustado <- data.frame(valores_ajustados = modelo_ajustado$fitted.values,
                     residuos = modelo_ajustado$residuals,
                     residuos_student = rstudent(modelo_ajustado))

ggplot(df_aux, aes(valores_ajustados, residuos_student)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-3, 3), color = "firebrick")

ggplot(df_aux, aes(sample = residuos)) +
  geom_qq() +
  geom_qq_line()

shapiro.test(df_aux$residuos)

vif(modelo)

# - Relación lineal Y con predictoras: En general Si, algunas no
# - Residuos se distribuyen alrededor del cero: Si
# - Varianza constante: No, parece tener forma cónica
# - Correlación de errores: No
# - Normalidad de residuos: No
# - Multicolinealidad regresores: No  (criterio VIF > 10)
