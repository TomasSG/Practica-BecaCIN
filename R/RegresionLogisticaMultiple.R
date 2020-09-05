# --------------------------------------Bibliotecas------------------------------------

library(dplyr)
library(ggplot2)
library(ggpubr)
library(caret) #confusionMatrix
library(car) #vif
library(ggthemes)
library(scales)
library(extrafont)


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
  mutate(sobrevivio = factor(sobrevivio, levels = c(1, 0), labels = c("Sí", "No")), 
         clase = factor(clase, levels = c(1, 2, 3), labels = c("1ra", "2da", "3ra")), 
         sexo = factor(sexo, levels = c("male", "female"), labels = c("Hombre", "Mujer")),
         nro_cabina = as.factor(nro_cabina), ticket = as.factor(ticket),
         puerto_embarcacion = as.factor(puerto_embarcacion)) 
  
write.csv(summary(datos_it1), file = "./resultados/summary_datos_it1.csv")

# Análisis variables categóricas de interés

{
  g0 <- ggplot(datos_it1, aes(y = ..count.. / sum(..count..))) + scale_y_continuous(labels = label_percent())
  g1 <- g0 + geom_bar(aes(x = clase), fill = "firebrick", alpha = .6) + ylab("") + xlab("Clase")
  g2 <- g0 + geom_bar(aes(x = sexo), fill = "darkblue", alpha = .6) + ylab("") + xlab("Sexo")
  g3 <- g0 + geom_bar(aes(x = sobrevivio), fill = "darkorange", alpha = .6) + ylab("") + xlab("Sobrevivió")
  
  arrange <- ggarrange(g1, g2, g3, nrow = 3, ncol = 1)
  
  annotate_figure(arrange, 
                  top = text_grob("Análisis Variables Categóricas", face = "bold", size = 20,
                                  family = "Dubai Medium", hjust = .5))
}

# Análisis variables cuantitativas de interés

{
  n <- nrow(datos_it1)
  g0 <- ggplot(datos_it1)
  
  g1 <- g0 + geom_histogram(aes(precio_ticket, ..density..), bins = calcular_cant_bins(n),
                            fill = "white", color = "darkblue",  alpha = .4) + 
    ylab("") +
    xlab("Precio Ticket") +
    scale_x_continuous(labels = label_number(prefix = "$"))
  
  g2 <- g0 + geom_boxplot(aes(precio_ticket), color = "darkblue") +
    ylab("") +
    xlab("Precio Ticket") +
    scale_x_continuous(labels = label_number(prefix = "$")) +
    theme(axis.text.y = element_blank())
  
  g3 <- g0 + geom_histogram(aes(edad, ..density..), bins = calcular_cant_bins(n),
                            color = "firebrick", fill = "white", alpha = .4) +
    ylab("") +
    xlab("Edad") 

  g4 <- g0 + geom_boxplot(aes(edad), color = "firebrick") +
    ylab("") +
    xlab("Edad") +
    theme(axis.text.y = element_blank())
  
  arrange <- ggarrange(g1, g2, g3, g4, nrow = 2, ncol = 2)
  annotate_figure(arrange, 
                  top = text_grob("Análisis Variables Cuantitativas", face = "bold", size = 20,
                                  family = "Dubai Medium", hjust = .5))
}

# Cuantitativas contra sobrevivio

{
  g0 <- ggplot(datos_it1, aes(x = sobrevivio, color = sobrevivio)) + xlab("")
  
  g1 <- g0 + geom_boxplot(aes(y = precio_ticket)) +
    ylab("Precio Ticket") +
    scale_y_continuous(labels = label_number(prefix = "$"))
  
  g2 <- g0 + geom_boxplot(aes(y = edad)) +
    ylab("Edad")
  
  arrange <- ggarrange(g1, g2, nrow = 1, ncol = 2, common.legend = TRUE, legend = "bottom")
  annotate_figure(arrange, 
                  top = text_grob("Variables Cuantitativas vs Sobrevivio", face = "bold", size = 20,
                                  family = "Dubai Medium", hjust = .5))
}

# zoom

g1 + coord_cartesian(ylim = c(0, 100)) + 
  ggtitle("Precio Ticket vs Sobrevivió (con zoom)") +
  theme(legend.position =  "none") +
  xlab("Sobrevivió")
  

# cualitativas contra sobrevivio

{
  g1 <- hacer_barplot_con_dos_cuantitativas(datos_it1, "clase", "sobrevivio") +
    xlab("Clase")
  
  g2 <- hacer_barplot_con_dos_cuantitativas(datos_it1, "sexo", "sobrevivio") +
    xlab("Sexo")
  
  arrange <- ggarrange(g1, g2, nrow = 1, ncol = 2, common.legend = TRUE, legend = "bottom")
  annotate_figure(arrange, 
                  top = text_grob("Análisis Sobrevivió vs Cualitativas", face = "bold", size = 20,
                                  family = "Dubai Medium", hjust = .5))
}

{
  t1 <- table(datos_it1$sobrevivio, datos_it1$clase)
  t2 <- table(datos_it1$sobrevivio, datos_it1$sexo)
  write.csv(t1, "./resultados/t1.csv")
  write.csv(t2, "./resultados/t2.csv")
}
# --------------------------------------Regresión Logística Múltiple------------------------------------

# Generamos le modelo

modelo_it0 <- glm(sobrevivio ~ clase + sexo + edad + precio_ticket, data = datos_it1,
                  family = "binomial")

write.csv(summary(modelo_it0)$coefficients, "./resultados/summary_log_modeloit0.csv")

calcular_pseudo_R2(modelo_it0)

# Sacamos las variables que no son significativas

modelo_it1 <- glm(sobrevivio ~ clase + sexo + edad, data = datos_it1, family = "binomial")

write.csv(summary(modelo_it1)$coefficients, "./resultados/summary_log_modeloit1.csv")

calcular_pseudo_R2(modelo_it1)

# Aplicamos la técnica de stepwise para seleccionar variables

step(modelo_it0, direction = "both")

# Buscamos puntos de mal ajuste

df_residuos <- data.frame(d_residuos = residuals(modelo_it1, type = "deviance"),
                         residuos = modelo_it1$residuals,
                         valores_ajustado = modelo_it1$fitted.values)



ggplot(df_residuos, aes(valores_ajustado, d_residuos)) +
  geom_point() +
  geom_hline(yintercept = c(2, -2), color = "firebrick", size = 1.3, linetype = 2) +
  ylab("Residuos de Deviancia") +
  xlab("Valores Ajustados") +
  ggtitle("Análisis de Residuos") 

indices <- which(abs(df_residuos$d_residuos) >= 2)

df_obs_mal_ajuste <- datos_it1[indices,]

{
  g0 <- ggplot(df_obs_mal_ajuste) + ylab("")
  
  g1 <- g0 + geom_boxplot(aes(edad), color = "firebrick") +
    xlab("Edad") +
    theme(axis.text.y = element_blank())
  
  g2 <- g0 + geom_boxplot(aes(precio_ticket), color = "darkblue") +
    xlab("Precio Ticket") +
    theme(axis.text.y = element_blank()) +
    scale_x_continuous(labels = label_number(prefix = "$"))
  
  g3 <- g0 + geom_bar(aes(sexo, ..count.. / sum(..count..)), fill = "white", color = "darkorange", alpha = .4) +
    scale_y_continuous(labels = label_percent()) +
    xlab("Sexo")
  
  
  g4 <- g0 + geom_bar(aes(clase, ..count.. / sum(..count..)), fill = "white", color = "tomato", alpha = .4) +
    scale_y_continuous(labels = label_percent()) +
    xlab("Clase")
  
  arrange <- ggarrange(g1, g2, g3, g4, nrow = 2, ncol = 2)
  
  annotate_figure(arrange, 
                  top = text_grob("Análisis Observaciones con Mal Ajuste", face = "bold", size = 20,
                                  family = "Dubai Medium", hjust = .5))
}

# Busqueda outliers y puntos influyentes

puntos_influyentes_outliers <- data.frame(leverage = hatvalues(modelo_it1),
                                          distancia_cook = cooks.distance(modelo_it1))

puntos_influyentes_outliers <- puntos_influyentes_outliers %>% arrange(-distancia_cook)

# Nos quedamos con los primeros 3 valores

head(puntos_influyentes_outliers, n = 3)
write.csv(head(puntos_influyentes_outliers, n = 3), "./resultados/head_puntos_influyentes.csv")
indices_influyentes <- c(298, 631, 571)

# Vemos si alguno de estos valores influyentes son los que tienen mal ajuste

indices_coincidentes <- intersect(indices, indices_influyentes)

write.csv(datos_it1[indices_coincidentes,], "./resultados/observaciones_influyentes_outliers.csv")

# Nuevo modelo sin estas observaciones

modelo_it2 <- glm(sobrevivio ~ clase + sexo + edad + precio_ticket, 
                  data = datos_it1[-indices_coincidentes,],
                  family = "binomial")
summary(modelo_it2)

calcular_pseudo_R2(modelo_it2)

#Seleccionamos variables

step(modelo_it2, direction = "both")

modelo_it3 <- glm(formula = sobrevivio ~ clase + sexo + edad, family = "binomial", 
                  data = datos_it1[-indices_coincidentes, ])
write.csv(summary(modelo_it3)$coefficients, "./resultados/summary_log_modeloit3.csv")

calcular_pseudo_R2(modelo_it3)

# Buscamos el valor de corte óptimo

df_valores_corte <- obtener_resultados_todos_posibles_valores_criticos(
  valores_reales = datos_it1[-indices_coincidentes,]$sobrevivio,
  probabilidades_estimadas = modelo_it3$fitted.values)

# Buscamos el punto de intersección entre ambas curvas

aux <- df_valores_corte %>% dplyr::filter(abs(sensitividad - especificidad) < 0.01)
write.csv(aux, "./resultados/valores_corte.csv")


# Código para hacerlo más lindo

df_grafico <- data.frame(valor_corte = df_valores_corte$valor_corte,
                         valor =df_valores_corte$sensitividad,
                         curva = "Sensitividad")

df_grafico <- rbind(df_grafico, data.frame(valor_corte = df_valores_corte$valor_corte,
                                           valor =df_valores_corte$especificidad,
                                           curva = "Especificidad"))

ggplot(mapping = aes(x = valor_corte)) +
  geom_line(df_grafico %>% filter(curva == "Sensitividad"), mapping =  aes(y = valor, color = curva, 
                                                      linetype = curva), size = 1.3) +
  geom_line(df_grafico %>% filter(curva == "Especificidad"), mapping =  aes(y = valor, color = curva, 
                                                      linetype = curva), size = 1.3) +
  xlab("Valor de Corte") +
  ylab("") + 
  ggtitle("Análisis de Posibles Valores de Corte") +
  scale_y_continuous(labels = label_percent()) +
  scale_x_continuous(breaks = seq(0, 1, .1)) 

rm(df_grafico)

ggplot(df_valores_corte, aes(valor_corte, accuracy)) + geom_line()

# Hacemos la matriz de confusión para el valor de corte

valor_corte <- .43

predicciones <- ifelse(modelo_it1$fitted.values > valor_corte, 1, 0)
predicciones_factor <- factor(predicciones,
                              levels = c(1, 0),
                              labels = c("Sí", "No"))

matriz_confusion <- confusionMatrix(predicciones_factor, datos_it1$sobrevivio)
write.csv(matriz_confusion$table, "./resultados/matriz_confusion.csv")

# --------------------------------------Tema ggplot------------------------------------

tema_viejo <- theme_get()
theme_set(theme_gdocs() + theme(text = element_text(family = "Dubai Medium"),
                                axis.title = element_text(face = "italic", size = 15),
                                plot.title = element_text(hjust = .5, face = "bold", size = 20)))





