# --------------------------------------Bibliotecas------------------------------------

library(ggplot2, warn.conflicts = FALSE)
library(ggthemes, warn.conflicts = FALSE)
library(GGally, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(gridExtra, warn.conflicts = FALSE)

# --------------------------------------Regresión lineal múltiple------------------------------------

datos <- as.data.frame(state.x77)
datos <- rename(habitantes = Population, analfabetismo = Illiteracy,
                ingresos = Income, esp_vida = `Life Exp`, asesinatos = Murder,
                universitarios = `HS Grad`, heladas = Frost, area = Area,
                .data = datos)


