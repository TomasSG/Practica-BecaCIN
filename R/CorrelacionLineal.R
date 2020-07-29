#-----------------------------Bibliotecas---------------------------------

library(MASS, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(ggthemes, warn.conflicts = FALSE)
library(corrplot, warn.conflicts = FALSE)
library(gridExtra)
data("Cars93")
data("iris")

#-----------------------------Visualización de datos---------------------------------

# Gráfico de dispersión para evaluar posible relación lineal o monotónica

ggplot(Cars93, aes(Weight, Horsepower)) +
  geom_point() +
  theme_few() +
  xlab("Peso") +
  ylab("Potencia") +
  ggtitle("Diagrama de dispersión Peso contra Potencia")


# Histograma para evaluar normalidad de ambas variables

g <- ggplot(Cars93) +
  theme_few() +
  ylab("")

g1 <- g + geom_histogram(aes(Weight), color = "darkred", fill = "white", bins = 15) + xlab("Peso")
g2 <- g + geom_histogram(aes(Horsepower), color = "blue", fill = "white", bins = 15) + xlab("Potencia")

grid.arrange(g1, g2, nrow = 1)

qqnorm(Cars93$Weight)
qqline(Cars93$Weight)

# Gráficos de dispersión de a pares

pairs(iris[, -5], lower.panel = NULL)

# Representación gráfica de la matriz de correlación

corrplot(cor(iris[,-5], method = "pearson"), method = "number")


#-----------------------------Cálculo del coeficiente de correlación---------------------------------

cor.test(x = Cars93$Weight,
         y = Cars93$Horsepower,
         alternative = "two.sided",
         conf.level = 0.95,
         metho = "pearson")

# Calculo de R2

R2 <- cor(x = Cars93$Weight, y = Cars93$Horsepower)

# Ahora, si quiero calcular el coeficiente con todas las viariables. Usamos el dataset "iris"

cor(iris[,-5], method = "pearson")
