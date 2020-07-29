# Variables ----
x <- 3
x

# Vecotres----
vector <- c(3, 5, 2, 5, 10, 20)
vector[1] # Accedo a un elemnto
vector[2:5] # Accedo a un rango
vector[-3] # Accedo a todos los elementos menos el indicado
vector[-1:-3] # Combinación de las dos anteriores
vector > 6 # Genera un vector de booleans, donde la posición i es TRUE si vector[i] cumple la condición
           # (en este caso ser mayor a 6) y si no es FALSE
vector[c(T,F,T,F,T,F)] # Solo muestra las posiciones donde el vector de boolean es TRUE
vector[vector > 6] # Combinación de los dos casos anteriores

# Dataframes----
vec1 <- c("Gato", "Perro", "Pato", "Jirafa")
vec2 <- seq(from = 1, to = 4,by = 1) # Igual a 1:4
vec3 <- c(1.2, 1.6, 9.2, 88) 
data_frame <- data.frame(vec1, vec2, vec3) # Todas las columnas deben tener el mismo tamaño pero pueden
                                           # ser de distinto tipo
str(data_frame)  # Permite ver como esta conformado un data frame
names(data_frame) <- c("Animales", "Numeros_enteros", "Numeros_reales") # Cambiamos el nomre
names(data_frame)
data_frame$Numeros_enteros # Una forma de acceder a los datos de una columna
data_frame[, 1] # Otra distinta
data_frame[, "Numeros_reales"] # Otra mas

# Solo pára ver la diferencia con data frames
vec4 <- c(1:30)
lista <- list(vec1, vec2, vec3, vec4) # Concatena objetos con diferentes estructura 
                                      # (distinto tipo y tamaño) 
str(lista)

# Las matrices, y los array en general, deben ser todos los datos del mismo tipo y
# tener la misma longitud
