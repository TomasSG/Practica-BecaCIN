# --------------------------------------Bibliotecas------------------------------------

library(ggplot2, warn.conflicts = FALSE)
library(ggthemes, warn.conflicts = FALSE)
library(GGally, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(gridExtra, warn.conflicts = FALSE)
library(MASS, warn.conflicts = FALSE)
data("Cars93")

# --------------------------------------Análisis de correlación lineal------------------------------------ 

# Para este ejemplos vamos a aplicar una regresión lineal simple para encontrar una expresión que explique
# la relación que hay entre entre el peso de un vehículo (Weight) y su potencia (Horsepower).

# El primer paso es analizar si existe algún tipo de relación lineal entre ambas nombre_vars. Para ello
# exploramos el gráfico de dispersión.

ggplot(Cars93, aes(Weight, Horsepower)) +
  geom_point(color = "darkgoldenrod") +
  theme_pander() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Diagrama de dispersión") +
  xlab("Potencia") +
  ylab("Peso (lb)")

# Por el gráfico se peude afirmar que existe algún tipo de relación lineal.
# Para poder calcular el coeficiente de correlación de Pearson, lo que tenemos que hacer es validar una serie 
# de condiciones. Lo primer es verificar la normalidad de ambas nombre_vars

hacer_histograma <- function(nombre_var, color, nbins = 30) {
  ggplot(Cars93, aes_string(nombre_var)) +
    geom_histogram(fill = "white", color = color, bins = nbins) +
    theme_gdocs() +
    theme(plot.title = element_text(hjust = 0.5)) +
    ylab("") +
    ggtitle(paste("Histograma (bins = ",nbins, ")")) +
    xlab(toupper(nombre_var))
}

g1 <- hacer_histograma("Weight", "darkred", 10)
g2 <- hacer_histograma("Horsepower", "blue", 10)
grid.arrange(g1, g2, nrow = 1)

# La variable Horsepower parece tener cierta normalidad, sin embargo de Weight no se podría decir lo
# mismo. Vamos a hacer un qqplot para seguir analizando la normalidad

hacer_qqplot <- function(nombre_var, color){
  ggplot(Cars93, aes_string(sample = nombre_var)) +
    geom_qq(color = color) +
    geom_qq_line() +
    theme_gdocs() +
    ggtitle(paste("QQplot", toupper(nombre_var))) +
    theme() +
    theme(plot.title = element_text(hjust = 0.5))
}

g1 <- hacer_qqplot("Weight", "darkred")
g2 <- hacer_qqplot("Horsepower", "blue")
grid.arrange(g1, g2, nrow = 1)

# Con los qqplots nos damos cuenta que la que presenta una mayor normalidad es Weight, en comparación con
# Horsepower. 
# Por último, con el shapiro test vemos analíticamente los resultados

shapiro.test(Cars93$Weight)
shapiro.test(Cars93$Horsepower)

# Por los resultados, el Weight se puede considerar normal pero Horsepower no  es así. Transformamos estas 
# variables para lograr aplicar el coeficiente de correlación.

Cars93 <- data.frame(Cars93, "Log_horsepower" = log10(Cars93$Horsepower))

g1 <- hacer_histograma("Log_horsepower", "blue", 10)
g2 <- hacer_qqplot("Log_horsepower", "blue")
grid.arrange(g1, g2, nrow = 1)

shapiro.test(Cars93$Log_horsepower)

# Con esta transformación se consigue una distribución normal, por lo que a partir de ahora seguimos con Weight 
# y log10(Horsepower).

# Ahora verificamos la homocedasticidad
  # TODO : No se hacerlo
# Por último calculamos el coeficiente de correlación con el método de Pearson

cor(Cars93$Weight, Cars93$Log_horsepower, method = "pearson")

# Obtenemos un coeficiente de correlación alto (0.809)
# Debemos calcular la significancia de la correlación para aceptar como válido este resultado

cor.test(Cars93$Weight, Cars93$Log_horsepower, method = "pearson", alternative = "two.sided", conf.level =  .95)

# Como obtenemos un pvalor < 2.2e-16 significa que es significativo el coeficiente de correlación calculado.

# --------------------------------------Regresión lineal simple------------------------------------ 

# El modelo generado va a buscar cuantificar la relación entre Weight y log10(Horsepower).

# 1) El primer paso es representar los datos para intuir si existe una relación y cuantificar la relación 
# me diante un coeficiente de relación. Resumiendo los resultados de la sección anterior

ggplot(Cars93, aes(Log_horsepower, Weight)) +
  geom_point(color = "darkblue") +
  theme_gdocs() +
  ggtitle("Diagrama de dispersión") +
  xlab( "Log10(Horsepower)") +
  theme(plot.title = element_text(hjust = .5, color = "black"))

cor.test(Cars93$Log_horsepower, Cars93$Weight)

# 2) Calculo del modelo de regresión lineal simple

modelo <- lm(Weight ~ Log_horsepower, Cars93)

summary(modelo)  

# Del resumen interpretamos:
# 1. R2 = 0.6556, quiere decir que el modelo explica el 65.56% de la variación total en los pesos observados
# 2. B1 = 3075.7, quiere decir que el incremento estimado para el peso promedio por cada aumento de una unidad
# del logaritmo de la potencia es de 3075.7 (NOTA: no pongo unidades porque las desconosco).
# 3. los pvalor de B1 y B0, quiere decir que los parámetros son significativamente distintos de cero. Para el B1 es
# más interesante porque se interpreta como existe una relación lineal entre la variable predictora y respuesta.

# 3) IC para B0 y B1

confint(modelo)

# 4) Represetnación gráfica del modelo

ggplot(Cars93, aes(Log_horsepower, Weight)) +
  geom_point(color = "darkblue") +
  geom_smooth(method = "lm", color = "firebrick") +
  theme_gdocs() +
  ggtitle("Weigth ~ log10(Horsepower)") +
  xlab("log10(Horsepower)") +
  theme(plot.title = element_text(hjust = .5, color = "black"),
        axis.title.x = element_text(color = "black"),
        axis.title.y = element_text(color = "black"))



# 5) Verificar las condiciones para aceptar un modelo lineal
  
  # Primero se evalua la relación lineal entre variable predictora y respuesta. Se representa con un
  # scatterplot los residuos, que se deben distribuir en torno al valor 0

resultados <- data.frame("predicciones" =  modelo$fitted.values, "residuos" = modelo$residuals)

ggplot(resultados, aes(predicciones, residuos, color = residuos)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_segment(aes(x = predicciones, y = 0, xend = predicciones, yend = residuos), color = "black", alpha = 0.3) +
  theme_clean() +
  ylab("Residuos") +
  xlab("Predicciones") +
  ggtitle("Análisis de la media de los residuos") +
  theme(plot.title = element_text(hjust = .5, size = 20),
        axis.title = element_text(size = 15),
        legend.position = "none") +
  scale_color_gradient2(low = "darkblue", mid = "gray", high = "firebrick")

  # Parecieria que los residuos se distribuyen entorno al cero por lo que se acepta la linealidad

  # Luego, distribución normal de reisduos

cant_bins <- function(x){
  return(1 + 3.322 * log10(x))
}

ggplot(resultados, aes(residuos, ..density..)) +
  geom_histogram(bins = cant_bins(nrow(resultados)), fill = "white", color = "firebrick", ) +
  theme_few() +
  ylab("") +
  xlab("Residuos") +
  ggtitle("Histograma (análisis normalidad residuos)") +
  theme(plot.title = element_text(hjust = .5, color = "black", size = 20))

ggplot(resultados, aes(sample = residuos)) +
  geom_qq_line() +
  geom_qq(color = "firebrick") +
  theme_few() +
  ggtitle("QQplot (análisis normalidad residuos)") +
  theme(plot.title = element_text(hjust = .5, color = "black", size = 20))

shapiro.test(resultados$residuos)
  
  # Si bien por el histograma parece ser que hay cierta normalidad, por el qqplot y el shapiro test obtenemos que
  # los residuos no son normales (pvalor = .00834 < .05) 

  # Varianza constante de los residuos (homocedasticidad)
