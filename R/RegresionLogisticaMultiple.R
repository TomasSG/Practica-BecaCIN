# --------------------------------------Bibliotecas------------------------------------

library(dplyr)
library(ggplot2)
library(ggpubr)

source("./R/Utils.R")

# --------------------------------------Datos------------------------------------

datos_crudos <- read.csv(file ="./data/train.csv")

# Renombramos variables y dejamos las que no son de interes

datos_crudos$PassengerId <- NULL
datos_crudos$Name <- NULL
datos_it0 <- datos_crudos %>% 
  rename(sobrevivio = Survived, clase = Pclass, sexo = Sex, edad = Age, her_esp = SibSp,
         padre_hijo = Parch, ticket = Ticket, precio_ticket = Fare, nro_cabina = Cabin,
         puerto_embarcacion = Embarked)

# --------------------------------------Exploración datos------------------------------------

write.csv(summary(datos_it0), file = "./resultados/summary_datos_it0.csv")

# Sacamos los NAs en edad aproximando con su media y convertimos a factor las variables
# necesarias

media_edad <- mean(datos_it0$edad, na.rm = TRUE)
lista_indices <- which(is.na(datos_it0$edad))
datos_it0$edad <- replace(datos_it0$edad, lista_indices, media_edad)


datos_it1 <- datos_it0 %>% 
  mutate(sobrevivio = as.factor(sobrevivio), clase = as.factor(clase), sexo = as.factor(sexo),
         nro_cabina = as.factor(nro_cabina), ticket = as.factor(ticket),
         puerto_embarcacion = as.factor(puerto_embarcacion)) 
  
write.csv(summary(datos_it1), file = "./resultados/summary_datos_it1.csv")

# Análisis variables categóricas de interés

g0 <- ggplot(datos_it1, aes(y = ..count.. / sum(..count..))) 
g1 <- g0 + geom_bar(aes(x = clase)) + ylab("")
g2 <- g0 + geom_bar(aes(x = sexo)) + ylab("")
g3 <- g0 + geom_bar(aes(x = sobrevivio)) + ylab("")

arrange <- ggarrange(g1, g2, g3, nrow = 3, ncol = 1)

annotate_figure(arrange, 
                top = text_grob("Análisis variables categóricas"))

# Análisis variables cuantitativas de interés

n <- nrow(datos_it1)
g0 <- ggplot(datos_it1)
g1 <- g0 + geom_histogram(aes(precio_ticket, ..density..), bins = calcular_cant_bins(n),
                          fill = "white", color = "blue", alpha = .4)
g2 <- g0 + geom_boxplot(aes(precio_ticket))
g3 <- g0 + geom_histogram(aes(edad, ..density..), bins = calcular_cant_bins(n),
                          fill = "white", color = "blue", alpha = .4)
g4 <- g0 + geom_boxplot(aes(edad))

arrange <- ggarrange(g1, g2, g3, g4, nrow = 2, ncol = 2)
annotate_figure(arrange,
                top = text_grob("Análisis variables cuantitativas"))

# Cuantitativas contra sobrevivio

g0 <- ggplot(datos_it1, aes(x = sobrevivio, color = sobrevivio))
g1 <- g0 + geom_boxplot(aes(y = precio_ticket))
g2 <- g0 + geom_boxplot(aes(y = edad))

arrange <- ggarrange(g1, g2, nrow = 1, ncol = 2, common.legend = TRUE, legend = "bottom")

annotate_figure(arrange,
                top = text_grob("Variables cuantitativas contra sobrevivio"))
# zoom

g1 + coord_cartesian(ylim = c(0, 100)) + ggtitle("Precio del ticket contra sobrevivio (zoom)")

# cualitativas contra sobrevivio

g1 <- hacer_barplot_con_dos_cuantitativas(datos_it1, "clase", "sobrevivio")
g2 <- hacer_barplot_con_dos_cuantitativas(datos_it1, "sexo", "sobrevivio")

arrange <- ggarrange(g1, g2, nrow = 1, ncol = 2, common.legend = TRUE, legend = "bottom")
annotate_figure(arrange,
                top = text_grob("Análisis sobrevivio vs resto de cualitativas"))

# --------------------------------------Regresión Logística Múltiple------------------------------------


