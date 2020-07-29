#-----------------------------Bibliotecas---------------------------------

library(ggplot2, warn.conflicts = FALSE)
library(ggthemes, warn.conflicts = FALSE)
library(corrplot, warn.conflicts = FALSE)
library(gridExtra, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)

 data(state.x77)
#-----------------------------Carga datos---------------------------------

datos <- read.table("https://www.diegocalvo.es/wp-content/uploads/2016/09/datos-regresion-lineal-multiple.txt", header = TRUE)

#-----------------------------Regresión lienal múltiple---------------------------------

# Primero exploramos la linealidad entre las tres variables

g <- ggplot(datos) + theme_gdocs() 

g1 <- g + geom_point(aes(Tiempo, N_cajas))
g2 <- g + geom_point(aes(Distancia, N_cajas))
g3 <- g + geom_point(aes(Tiempo, Distancia))

grid.arrange(g1, g2, g3, nrow = 3)

# Miramos con un mapa de calor las correlaciones

corrplot(cor(datos, method = "pearson"), method = "number") # TODO: Ver para hacerlo más lindo

# No presenta una buen índice de correlación entre Distancia y N_cajas y Tiempo y Distancia

modelo1 <- lm(datos$Tiempo ~ datos$N_cajas + datos$Distancia, data = datos)
summary(modelo1)

# Pregunta: por qué es malo este R2?
# Hacemos otro modelo

modelo2 <- lm(datos$Tiempo ~ datos$N_cajas, data = datos)
summary(modelo2)

# Comparamos ambos modelos
anova(modelo1, modelo2)

# Resulta que es mejor el primer modelo (¿por qué?)

# Verificamos la normalidad de los residuos

residuos <- data.frame(r = rstandard(modelo1))

ggplot(residuos, aes(r)) +
  geom_histogram(color = "blue", fill = "white", bins = 15) +
  theme_economist_white()

qqnorm(residuos$r)
qqline(residuos$r)

# Verificamos que la varianza de los errores es constante

residuos <- data.frame(residuos, val_ajustados = fitted.values(modelo1))

ggplot(residuos, aes(val_ajustados, r)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") + 
  theme_gdocs()

# Ahora vemos la independencia de errores

g <- residuos %>% 
  mutate(N_cajas = datos$N_cajas, Distancia = datos$Distancia, r = rstandard(modelo1)) %>% 
  ggplot() +
  theme_gdocs() +
  ylab("Residuos") +
  geom_hline(yintercept = 0, color = "blue")

g1 <- g + geom_point(aes(N_cajas, r))
g2 <- g + geom_point(aes(Distancia, r))

grid.arrange(g1, g2)
