# Agregar la biblioteca----
install.packages("ggplot2") # Instala la biblioteca en la PC
library("ggplot2") # Carga el paquete para que se pueda utilizar

# Ejemplos de uso de ggplot2----
ggplot(data = mtcars, aes(x = mtcars$wt, y = mtcars$qsec)) +
  geom_point(color = "red", size  = 2) +
  scale_x_continuous(name = "wt") +
  scale_y_continuous(name = "qsec") +
  theme_classic()

ini <- c(1, 8, 2)
fin <- c(3, 10, 6)
data_linea <- data.frame(ini,fin) 
names(data_linea) <- c("x","y")
ggplot(data = data_linea, aes(x = data_linea$X, y = data_linea$y)) +
  geom_line(aes(x = x, y = y))

# Limpieza de environment----
rm(list = ls()) #ls() me da una lista con todos los objetos en memoria
