# --------------------------------------Bibliotecas------------------------------------

library(readxl)
library(ggplot2)
library(ggthemes)
library(stringr)
library(dplyr)
library(lubridate)

# --------------------------------------Carga de datos------------------------------------

datos <- read.csv("./data/datasets_376751_731448_london_merged.csv")

# Sacamos los espacios 


# Cambiamos algunas variables a factores 
datos$is_weekend <- as.factor(datos$is_weekend)

datos$is_holiday <- as.factor(datos$is_holiday)

datos$season <- as.factor(datos$season)

# Indicador de que el día esta lindo (los códigos menores a 5 representan eso)
datos <- datos %>% mutate(esta_lindo = weather_code < 5)

# Obtenemos el día de semana a partir de la fecha
datos$dia <- wday(as.Date(datos$timestamp), label = TRUE)
datos$dia <- factor(datos$dia,
                    levels = c("lun\\.", "mar\\.", "mié\\.",
                    "jue\\.", "vie\\.", "sáb\\.", "dom\\."),
                    labels = c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado", "Domingo"))


# Obtenemos la hora 
datos <- datos %>% mutate(hora = hour(as_datetime(datos$timestamp)))

# Tomamos las horas pico son 6 a 10 y 17 a 21
datos <- datos %>% mutate(es_pico = ((hora >= 6 & hora <= 10) | (hora >= 17 & hora <= 21)))

# --------------------------------------Exploración datos------------------------------------

ggplot(datos, aes(is_weekend, y = ..count.. / sum(..count..), fill = is_weekend)) +
  geom_bar() +
  theme_gdocs()

ggplot(datos, aes(is_weekend, cnt, color = is_weekend)) +
  geom_boxplot() +
  theme_gdocs()

ggplot(datos, aes(cnt, is_weekend,color = is_weekend)) +
  geom_point() +
  theme_gdocs()

ggplot(datos, aes(cnt, esta_lindo ,color = esta_lindo)) +
  geom_point() +
  theme_gdocs()

ggplot(datos, aes(esta_lindo, cnt, color = esta_lindo)) +
  geom_boxplot() +
  theme_gdocs() +
  coord_cartesian(ylim = c(0, 2000))

ggplot(datos, aes(cnt, is_holiday,color = is_holiday)) +
  geom_point() +
  theme_gdocs()

ggplot(datos, aes(is_holiday, cnt, color = is_holiday)) +
  geom_boxplot() +
  geom_jitter(width = .1) +
  theme_gdocs()

ggplot(datos, aes(dia, y = ..count.. / sum(..count..))) +
  geom_bar() +
  theme_gdocs() +
  facet_grid(. ~ is_holiday)


ggplot(datos, aes(season, y = ..count.. / sum(..count..))) +
  geom_bar() +
  theme_gdocs()

ggplot(datos, aes(season, cnt, color = season)) +
  geom_boxplot() +
  theme_gdocs()

ggplot(datos, aes(es_pico, cnt, color = es_pico)) +
  geom_boxplot()


ggplot(datos, aes(cnt, es_pico, color = es_pico)) +
  geom_point()



# --------------------------------------Regresión logística simple------------------------------------


# +---------------------------------+
# | PASO 1: Representación de datos |
# +---------------------------------+

g1 <- ggplot(datos, aes(esta_lindo, cnt, color = esta_lindo)) +
  geom_boxplot() +
  theme_gdocs() 

# Hacemos un zoom para pareciar las diferencias
g1 + coord_cartesian(ylim = c(0, 2000))

# Parece que existe una diferencia en la cantidad de bicis prestadas según si el día esta lindo


# +----------------------------------+
# | PASO 2: Generar el modelo de RLS |
# +----------------------------------+

modelo <- glm(esta_lindo ~ cnt, data =  datos, family = "binomial")
summary(modelo)

# INTERPRETACIÓN: Por cada unidad que se incementa la cantidad de bicis alquiladas, los odds de que sea
# un día lindo aumentan en promedio 1.00061.

# +----------------------------+
# | PASO 3: Gráfico del modelo |
# +----------------------------+

# Primero generamos nuevos puntos
nuevos_puntos <- seq(min(datos$cnt), max(datos$cnt), 1)

# Ahora, predecimos las probabilidades
predicciones <- predict(modelo, data.frame("cnt" = nuevos_puntos), type = "response")

# Graficamos las probabilidades
df_aux <- data.frame(predicciones = predicciones, cnt = nuevos_puntos)

ggplot(df_aux, aes(cnt, predicciones)) +
  geom_line() +
  theme_gdocs()

# Por qué empieza en 0.8?

# +-------------------------------+
# | PASO 3: Validación del modelo |
# +-------------------------------+

anova(modelo, test = "Chisq")


