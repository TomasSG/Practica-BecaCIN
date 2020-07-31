# --------------------------------------Bibliotecas------------------------------------

library(ggplot2, warn.conflicts = FALSE)
library(ggthemes, warn.conflicts = FALSE)
library(GGally, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(gridExtra, warn.conflicts = FALSE)
library(extrafont, warn.conflicts = FALSE)
library(corrplot, warn.conflicts = FALSE)
library(ggcorrplot, warn.conflicts = FALSE)
library(car, warn.conflicts = FALSE)

# font_import()


# --------------------------------------Regresión lineal múltiple------------------------------------

datos <- as.data.frame(state.x77)
datos <- rename(habitantes = Population, analfabetismo = Illiteracy,
                ingresos = Income, esp_vida = `Life Exp`, asesinatos = Murder,
                universitarios = `HS Grad`, heladas = Frost, area = Area,
                .data = datos)

# El objetivo del modelo a hacer es predecir valores para la esperanza de vida media en función
# de distintas variables.

# +-------------------------------------------------+
# | PASO 1: Análisis de la relación entre variables |
# +-------------------------------------------------+


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

# +---------------------------+
# | PASO 2: Generar un modelo |
# +---------------------------+
# Iniciamos con un modelo con todas las variables y lo vamos a ir reduciendo.

modelo <- lm(esp_vida ~ habitantes + ingresos + analfabetismo + asesinatos + universitarios + 
               heladas + area, datos)
summary(modelo)

# El R2 ajustado es de .692, lo que es alto. El p-valor del modelo es sifnificativo por lo que 
# se puede aceptar. Además, tenemos 3 B1 que son significativos pero hay que decidir que
# hacer con el resto

# +----------------------------------------------+
# | PASO 3: Selección de los mejores predictores |
# +----------------------------------------------+
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

# +---------------------------------+
# | PASO 4: Validación de supuestos |
# +---------------------------------+

# A) Relación lineal entre predictores y respuesta
# Graficamos los residuos contra los predictores

datos <- data.frame(datos, "residuos" = modelo$residuals)


formato_titulo <- function(string) {
  string <- tolower(string)
  substr(string, 1, 1) <- toupper(substr(string, 1, 1))
  return(string)
}

scatterplot_residuos <- function(nombre_var, is_ic = TRUE){
  
  ggplot(datos, aes_string(tolower(nombre_var), "residuos")) +
    geom_point() +
    geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
    geom_smooth(color = "firebrick", se = is_ic) +
    theme_gdocs() +
    ylab("Residuos") +
    xlab(formato_titulo(nombre_var)) +
    ggtitle(paste("Relación lineal con", formato_titulo(nombre_var))) +
    theme(plot.title = element_text(hjust = 0.5, size = 20, color = "black"),
          axis.title = element_text(size = 15, color = "black"),
          text = element_text(family = "Bookman Old Style"))
}

g1 <- scatterplot_residuos("habitantes")
g2 <- scatterplot_residuos("asesinatos")
g3 <- scatterplot_residuos("universitarios")
g4 <- scatterplot_residuos("heladas")

grid.arrange(g1, g2, g3, g4, nrow = 2)

# Por los gráficos pareciera que se distribuyen alrededor del cero los residuos.

# B) Distribución normal de residuos

ggplot(datos, aes(sample = residuos)) +
  geom_qq(color = "darkblue") +
  geom_qq_line() +
  theme_minimal() +
  xlab("Theoretical") +
  ylab("Sample") +
  ggtitle("QQplot residuos") +
  theme(plot.title = element_text(hjust = .5, size = 20),
        axis.title = element_text(size = 15), 
        text = element_text(family = "Rockwell"))
  
calcular_bins <- function(n) 1 + 3.33 * log10(n)

ggplot(datos, aes(residuos)) +
  geom_histogram(bins = calcular_bins(nrow(datos)), fill = "white", color = "firebrick") +
  theme_gdocs() +
  xlab("Residuos") +
  ylab("") +
  ggtitle("Histograma de residuos") +
  theme(plot.title = element_text(hjust = .5, color = "black", size = 20),
        axis.title = element_text(size = 15),
        text = element_text(family = "Poor Richard"))

shapiro.test(datos$residuos)

# En base al análisis gráfico y el test podemos se cree que los residuos tienen una distribución normal

# C) Homocedasticidad

datos <- data.frame(datos, "valores_ajustados" = modelo$fitted.values)

scatterplot_residuos("valores_ajustados", is_ic = FALSE)

# No pareciera que exista algún patrón en la distribución de los residuos

# D) No colinialidad o multicolinialidad

ggcorrplot(cor(datos[, c("habitantes", "asesinatos", "universitarios", "heladas")], method = "pearson"),
           ggtheme = theme_bw,
           hc.order = TRUE,
           lab = TRUE, 
           lab_size = 6,
           type = "lower",
           colors = c("#ff0084", "#ededed", "#e73827"))

# No pareciera exisitr colinealidad, aunque el R entre asesinatos y heladas es de 0.54.

vif(modelo)

# Por los resultados del VIF no pareciera existir multicolinialidad

# E) Autocorrelación

dwt(modelo, alternative = "two.sided")

# Por el test se cree que las muestras son independientes

# F) Identificación de los posibles valores atípicos o influyentes

# Primero empeazamos por los atípicos

datos <- data.frame(datos, "s_residuos" = rstudent(modelo))

ggplot(datos, aes(valores_ajustados, abs(s_residuos))) +
  geom_point(size = 1.5) +
  geom_point(data = datos[abs(datos$s_residuos) > 3,], color = "firebrick") +
  geom_hline(yintercept = 3, linetype = "dashed") +
  theme_gdocs() +
  ylab("Residuos studintized") +
  xlab("Valores ajustados") +
  ggtitle("Análisis de outliers") +
  theme(plot.title = element_text(color = "black", hjust = .5),
        text = element_text(family = "Tw Cen MT"))

# No se encontrno outliers
# Ahora buscamos valores influyentes

aux <- data.frame(summary(influence.measures(modelo)))

# Se considera un valor influyente a aquellos hat que superen
#   umbral = 2.5 * (p + 1) / n
# con  p = número de predictores y n = cantidad de muestras

umbral_hat <- 2.5 * length(modelo$coefficients)  / nrow(datos)

aux %>% 
    filter(hat > umbral_hat)
  
# Con este análisis tenemos que las muestras de California y Nevada parecen ser influyentes.
# Calculamos el modelo excluynedolas

indices <- which.names(c("California", "Nevada"), datos)
modelo_aux <- lm(esp_vida ~ habitantes + asesinatos + universitarios + 
     heladas , datos[-indices,])

summary(modelo_aux)
summary(modelo)

# Como vemos no se mejora la modelo con la exlcusión de estos datos.