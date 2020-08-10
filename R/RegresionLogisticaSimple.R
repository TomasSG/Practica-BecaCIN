# --------------------------------------Bibliotecas------------------------------------

library(readxl)
library(ggplot2)
library(ggthemes)
library(stringr)
library(dplyr)
library(lubridate)
library(scales)
library(extrafont)

# --------------------------------------Temas------------------------------------

tema_viejo <- theme_get()
theme_set(theme_gdocs() + theme(text = element_text(family = "Rockwell"),
                                plot.title = element_text(hjust = .5,size = 25, color = "black"),
                                axis.title = element_text(size = 15, color = "black")))

# --------------------------------------Carga de datos------------------------------------

# Link a la info de este dataset: https://www.kaggle.com/c/titanic/data?select=train.csv

datos <- read.csv("./data/train.csv")

# Cambiamos los nombres de las variables a minúsculas
names(datos) <- c("passenger_id", "survived", "p_class", "name",
                  "sex", "age", "sib_sp", "parch", "ticket",
                  "fare", "cabin", "embarked")

# Cambiamos algunas variables a factores 
datos$p_class <- as.factor(datos$p_class)

# Creamos un indicador de primera clase
datos <- datos %>% mutate(es_primera = ifelse(p_class == 1, 1, 0))
datos$es_primera <- as.factor(datos$es_primera)

# --------------------------------------Exploración de datos------------------------------------

g1 <- ggplot(datos, aes(p_class, fare, color = p_class)) +
  geom_boxplot() +
  geom_jitter(width = .1) 

g1 + coord_cartesian(ylim = c(0, 100))

# Es razonable que las tarifas sean distintas según la clase en que se viaje

ggplot(datos, aes(es_primera, fare, color = es_primera)) +
  geom_boxplot() +
  geom_jitter(width = .1)

ggplot(datos, aes(es_primera, fare, color = es_primera)) +
  geom_point()

# Hacemos una regresión logística para tratar de predecir si el ticket es de priemra clase o no según
# su precio

# --------------------------------------Regresión logística simple------------------------------------


# +---------------------------------+
# | PASO 1: Representación de datos |
# +---------------------------------+

g1 <- ggplot(datos, aes(es_primera, fare, color = es_primera)) +
  geom_boxplot(show.legend = FALSE) +
  ylab("Tarifa") +
  xlab("") +
  ggtitle("") +
  scale_y_continuous(labels = label_number(prefix = "$"))

g2 <- ggplot(datos, aes(fare, es_primera, color = es_primera)) +
  geom_point(alpha = .4, show.legend = FALSE) +
  xlab("Tarifa") +
  ylab("") +
  ggtitle("") +
  scale_x_continuous(labels = label_number(prefix = "$"))

annotate_figure(ggarrange(g1, g2, nrow = 1, ncol = 2),
                top = text_grob("Tarifa contra indicador de primera clase (sin zoom)",
                                size = TAMANIO_LETRA_TITULO_PRINCIPAL, color = "black", family = FAMILIA_LETRA))

g1 <- g1 + coord_cartesian(ylim = c(0, 200))
g2 <- g2 + coord_cartesian(xlim = c(0, 200))

annotate_figure(ggarrange(g1, g2, nrow = 1, ncol = 2),
                top = text_grob("Tarifa contra indicador de primera clase (con zoom)",
                                size = TAMANIO_LETRA_TITULO_PRINCIPAL, 
                                color = "black", family = FAMILIA_LETRA))


# +----------------------------------+
# | PASO 2: Generar el modelo de RLS |
# +----------------------------------+

modelo <- glm(es_primera ~ fare, data =  datos, family = "binomial")
summary(modelo)

# INTERPRETACIÓN: Por cada unidad que se incementa la tarifa, los odds de que sea
# un ticket de primera clase aumentan en promedio 1.0089991.

# +----------------------------+
# | PASO 3: Gráfico del modelo |
# +----------------------------+

# Primero generamos nuevos puntos
nuevos_puntos <- seq(min(datos$fare), max(datos$fare), .5)

# Ahora, predecimos las probabilidades
predicciones <- predict(modelo, data.frame(fare = nuevos_puntos), type = "response")

# Graficamos las probabilidades
df_aux <- data.frame(predicciones = predicciones, fare = nuevos_puntos)

ggplot(df_aux, aes(fare, predicciones)) +
  geom_line(color = "darkblue") +
  xlab("Tarifa") +
  ylab("P(Y = 1|X)") +
  ggtitle("Predicciones realizadas") +
  scale_x_continuous(label = label_number(prefix = "$")) +
  coord_cartesian(xlim = c(0, 150))

# +-------------------------------+
# | PASO 4: Validación del modelo |
# +-------------------------------+

anova(modelo, test = "Chisq")

# Por el resultado del test el modelo se considera significativo muestra una mejora
# en las explicaciones que el modelo nullo

# +---------------------------------------------------------------+
# | PASO 5: Comparación de clasificación predicha y observaciones |
# +---------------------------------------------------------------+

# Primero realizamos las predicciones con los datos
predicciones <- ifelse(modelo$fitted.values > 0.5, 1, 0)

# Armamos un df para ver si es capaz de clasificar correctamente
df <- data.frame(fare = modelo$model$fare, es_primera = modelo$model$es_primera,
                 predicciones = predicciones)
df <- df %>% mutate(prediccion_correcta = ifelse(predicciones == es_primera,
                                                 "Correcto",
                                                 "No correcto"))

df %>% 
  mutate(es_primera = factor(es_primera, 
                             levels = c(0, 1), 
                             labels = c("No es primera clase", "Si es primera clase"))) %>% 
  group_by(es_primera, prediccion_correcta) %>% 
  summarise(freq_abs = n()) %>% 
  mutate(freq_rel = freq_abs / sum(freq_abs)) %>% 
  ggplot(aes(prediccion_correcta, freq_rel, fill = prediccion_correcta)) +
  geom_bar(stat = "identity", alpha = .6, show.legend = FALSE) +
  facet_grid(. ~ es_primera) +
  xlab("") +
  ylab("Proporciones") +
  ggtitle("Proporciones de predicciones correctas") +
  scale_y_continuous(labels = label_percent()) +
  scale_fill_manual(values = c("#33FF52","#FF3333"))

df %>% 
  mutate(es_primera = factor(es_primera, 
                             levels = c(0, 1), 
                             labels = c("No es primera clase", "Si es primera clase"))) %>% 
  group_by(prediccion_correcta) %>% 
  summarise(freq_abs = n()) %>% 
  mutate(freq_rel = freq_abs / sum(freq_abs)) %>% 
  ggplot(aes(prediccion_correcta, freq_rel, fill = prediccion_correcta)) +
  geom_bar(stat = "identity", alpha = .6, show.legend = FALSE) +
  xlab("") +
  ylab("Proporciones") +
  ggtitle("Proporciones de predicciones correctas") +
  scale_y_continuous(labels = label_percent()) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_manual(values = c("#33FF52","#FF3333"))

