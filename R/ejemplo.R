# --------------------------------------Bibliotecas------------------------------------

library(readxl, warn.conflicts = FALSE)

# --------------------------------------Cargar datos------------------------------------

datos <- read_excel("../../Descargas/Gaby-presion-JULIO 2020.xlsx")
View(datos)

# --------------------------------------Visualización------------------------------------

ggplot(datos, aes(Fecha, Pulsasiones, color = Momento)) +
  geom_point()
