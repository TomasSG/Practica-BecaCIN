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


