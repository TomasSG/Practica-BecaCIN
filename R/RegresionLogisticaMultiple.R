# --------------------------------------Bibliotecas------------------------------------

library(dplyr)
library(ggplot2)

# --------------------------------------Datos------------------------------------

datos_crudos <- read.csv(file ="./data/train.csv")

# Renombramos variables y dejamos las que no son de interes

datos_crudos$PassengerId <- NULL
datos_crudos$Name <- NULL
datos <- datos_crudos %>% 
  rename(sobrevivio = Survived, clase = Pclass, sexo = Sex, edad = Age, her_esp = SibSp,
         padre_hijo = Parch, ticket = Ticket, precio_ticket = Fare, nro_cabina = Cabin,
         puerto_embarcacion = Embarked)


# --------------------------------------Exploración datos------------------------------------

write.csv(summary(datos), file = "./example.csv")

# Sacamos los NAs en edad aproximando con su media

media_edad <- mean(datos$edad, na.rm = TRUE)
lista_indices <- which(is.na(datos$edad))
datos$edad <- replace(datos$edad, lista_indices, media_edad)

# Convertimos a factor las variables necesarias

datos <- datos %>% 
  mutate(sobrevivio = as.factor(sobrevivio), clase = as.factor(clase))

write.csv(summary(datos), file = "./example.csv")
