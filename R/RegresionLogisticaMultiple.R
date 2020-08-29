# --------------------------------------Bibliotecas------------------------------------

library(dplyr)
library(ggplot2)

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
