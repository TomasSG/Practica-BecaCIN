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

ggpairs(datos, 
        low = list(continuous = wrap("smooth", alpha = .6)), axisLabels = "none",
        diag = list(continuous = "densityDiag")) +
  theme_few() +
  theme(plot.title = element_text(hjust = .5), 
        plot.subtitle = element_text(hjust = .5),
        text = element_text(family = "Rekha")) +
  ggtitle("Prueba GGaly", "Esto es un subtitulo") +
  scale_color_brewer(palette = "Dark2")

# Las variables que tienen un R mayor a 0.5 con esperanza de vida son: 
#   Asesinato 
#   Universitarios
#   Analfabetismo
# Las variables que tienen un R mayor a 0.5 con asesinato son:
#   Analfabetismo
#   Heladas
# Las variables que tienen un R mayor a 0.5 con universitarios son :
#   Ingresos
#   Analfabetismo
# Las variables que tienen un R mayor a 0.5 con heladas son :
#   Analfabetismo
#   Asesinatos
# En cuanto a las distribuciones pareciera que habitatnes y area tienen una distribución exponencial 

# Paso 2: generar el modelo. Iniciamos con un modelo con todas las variables y 
# lo vamos a ir reduciendo.

modelo <- lm(esp_vida ~ habitantes + ingresos + analfabetismo + asesinatos + universitarios + 
               heladas + area, datos)
summary(modelo)

# El R2 ajustado esd e .692, lo que es alto. El p-valor del modelo es sifnificativo por lo que 
# se puede aceptar. Además, tenemos 3 B1 que son significativos pero hay que decidir que
# hacer con el resto

# Paso 3: selección de los mejores predictores
# Realizamos un stepwise en mixto para la elección

step(modelo, trace = 1)

modelo <- lm(formula = esp_vida ~ habitantes + asesinatos + universitarios + 
    heladas, data = datos)
summary(modelo)

# De este modelo vemos que el R2 ajustado es mejor aunque el R2 sea menor al inicialmente 
# planteado. Con este modelo se logra explicar el 73.6% de la variabilidad de la 
# media de esperanza de vida.
# Interpretamos un B1: el incremento estimado de la esperanza de vida promedio es de 
# -0.3001 por cada incremento de una unidad de los asesinatos, manteniendo el resto de 
# variables constantes.



