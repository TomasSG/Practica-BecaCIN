# --------------------------------------Bibliotecas------------------------------------

library(ggplot2, warn.conflicts = FALSE)
library(ggthemes, warn.conflicts = FALSE)
library(GGally, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(gridExtra, warn.conflicts = FALSE)
library(extrafont, warn.conflicts = FALSE)

# font_import()


# --------------------------------------Regresión lineal múltiple------------------------------------

datos <- as.data.frame(state.x77)
datos <- rename(habitantes = Population, analfabetismo = Illiteracy,
                ingresos = Income, esp_vida = `Life Exp`, asesinatos = Murder,
                universitarios = `HS Grad`, heladas = Frost, area = Area,
                .data = datos)

# El objetivo del modelo a hacer es predecir valores para la esperanza de vida media en función
# de distintas variables.

# Paso 1: Análisis de la relación entre variables

ggpairs(datos, low = list(continuous = wrap("smooth", alpha = .6)), axisLabels = "none") +
  theme_few() +
  theme(plot.title = element_text(hjust = .5), 
        plot.subtitle = element_text(hjust = .5),
        text = element_text(family = "Rekha")) +
  ggtitle("Prueba GGaly", "Esto es un subtitulo") +
  scale_color_brewer(palette = "Dark2")





