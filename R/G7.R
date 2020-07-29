# Bibliotecas ----
library(ggplot2)
library(dplyr)

# Declaracion de algunas variables importantes ----
arch <- "./data/Ajoblanco.csv"

# Obtencion datos ----
datos <- read.csv(file = arch, sep = ";")
names(datos) <- c("anio", "perim", "peso")
datos$anio <- factor(x = datos$anio) # Esto es para que lo trate como un factor

# Ejercicio 1 ----

# 1) Identificar variables cualitativas y cuantitativas. 
# Para cada una indicar los valores posibles (rango o valores específicos segun corresponda).

# Vemos como trata a cada una de las variables
str(datos)

# Si bien anio la representa como un entero, es una variable cualitativa que se usa para 
# clasificar la muestra. Entonces:
# anio -> cualitativa
# perim -> cuantitativa
# peso -> cuantitativa

# Identificamos los rangos de las variables cuantitativa
min_perim <- min(datos$perim)
max_perim <- max(datos$perim)
paste("La variable perimetro se encuentra en el intervalo [", min_perim, ", ", max_perim, "] cm", sep = "")

min_peso <- min(datos$peso)
max_peso <- max(datos$peso)
paste("La variable pesp se encuentra en el intervalo [", min_peso, ", ", max_peso, "] cm", sep = "")

# Para anio vamos a ver todos los posibles valores.
niveles_anio <- levels(factor(datos$anio))
paste("Los posibles valores de anio son:")
paste(niveles_anio)

#----------------------------------------------------------------------------------------------------#

# Ejercicio 2 ----

# 2) Realizar graficos para cada variable segun corresponda histograma, boxplot 
# y/ o grafico de tortas.

# Generamos un data frame auxiliar donde tengamos los años

# Forma 1
data_aux <- datos %>% 
  select(anio) %>%
  group_by(anio) %>% 
  summarise(n = n()) %>% 
  mutate(porcentaje = n / sum(n))

# Forma 2
data_aux <- datos %>% 
  count(anio) %>%  
  mutate(porcentaje = n / sum(n))
  

# Grafico de torta para anio
ggplot(data_aux, aes(x = "", y = porcentaje, fill = anio)) + 
  geom_bar(stat = "identity") +
  coord_polar("y") +
  theme_void() +
  theme(legend.position = "F") + 
  labs(title = "Cantidad de datos por año")
  
# Grafico de barras para anio, con esta opción no se tiene calculado explícitamente calculado
# el % dentro del propio data frame
ggplot(datos, aes(x = anio, y = ..count.. / sum(..count..))) +
  geom_bar() +
  labs(x = "Año", 
       y = "Porcentaje de  muestras",
       title = "Porcentaje de muestras por año") +
  scale_y_continuous(labels = scales::percent)

# Grafico de barras para anio, con esta opción existe una columna que contiene el %
ggplot(data_aux, aes(x = reorder(anio, porcentaje), y = porcentaje)) +
  geom_bar(stat = "identity") + # Identity indica que se proporciona la cantidad directamente
  geom_text(aes(label = paste(round(porcentaje * 100, 3), "%")), vjust = -0.5) +
  labs(x = "Año", 
       y = "Porcentaje de  muestras",
       title = "Porcentaje de muestras por año") +
  scale_y_continuous(limits = c(0, .6), 
                     breaks = seq(from = 0, to = .6, by = .1), 
                     labels = scales::percent) +
  theme_grey() +
  theme(plot.title = element_text(hjust = 0.5))

# Histograma para el peso
ggplot(datos, aes(x = peso)) +
  geom_histogram(fill = "cornflowerblue", color = "white", bins = 15, ) +
  labs(x = "", y = "", title = "Histograma de peso") +
  scale_x_continuous(breaks = seq(0, 150, 10)) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme_grey()
  
# Boxplot para el perimetro
  ggplot(datos, aes(y = perim, x =)) +
    geom_boxplot() +
    labs( y = "Permítero", x = "", title = "Boxplot de perímetro") + 
    scale_x_continuous(limits = c(-.5, .5), breaks = seq(-.5, .5, .1)) +
    theme_grey() +
    theme(plot.title = element_text(hjust = 0.5)) 
  
rm(data_aux)
  
#----------------------------------------------------------------------------------------------------#

# Ejercicio 3 ----
  
# 3) Identificar datos atipicos segun año  

# Para esto hay que realizar un boxplot filtrando según el año
# Empezamos con un boxplot del perimetro
ggplot(datos, aes(x = anio, y = perim)) +
  geom_boxplot() +
  labs(title = "Boxplot de perímetro según año",
       x = "Año",
       y = "Perimetro") +
  theme_grey() +
  theme(plot.title = element_text(hjust = 0.5))

# Con los gráficos podemos observar que en el 2016 se tuvo un dato atípico y en 2017 otro. 
# Para identificarlos podemos hacer uso de la bibliotecad dyplr

data_aux <- datos %>%
  select(anio, perim) %>% 
  group_by(anio) %>% 
  summarise(min = min(perim), max = max(perim))
# Para el 2016 el dato atípico es 8 y para el 2017 es 23.2

# Seguimos con el boxplot del peso
ggplot(datos, aes(x = anio, y = peso)) +
  geom_boxplot(outlier.size = 3) + 
  labs(title = "Boxplot del peso segun año",
       x = "Año",
       y = "Peso") +
  scale_y_continuous(lim = c (0, 155), breaks = seq(0, 155, 5)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Como se observa en el 2016 no hubo datos atípicos pero en el 2017 hay varios.
# Para identificarlos simplemente calculamos el límite del bigote y superior y buscamos todos 
# aquellos datos que superen dicho valor.

data_aux <- datos %>% 
  select(anio, peso) %>% 
  filter(anio == 2017)

lim_sup <- 1.5 * IQR(data_aux$peso) + quantile(data_aux$peso, probs = 0.75)

datos_atipicos <- data_aux %>% 
  filter(peso > lim_sup) %>% 
  select(peso)

# Entonces los datos atípicos son los que se encuentrarn en el dataframe datos_atipicos.

rm(data_aux, lim_sup)

#----------------------------------------------------------------------------------------------------#

# Ejercicio 4 ----

# 4) Hallar las medidas resumen según el tpio de variable

summary(datos)

#----------------------------------------------------------------------------------------------------#

# Ejercicio 5 ----

# 5) 
#   a) Analizar el peso con tablas de frecuencia, separando por año.
#   b) Comparar con los histogramas.
#   c) Analice la adecuacion o ajuste de una variable normal en cada caso.

# a)

cant_int <- 15

# Primero hacemos la tabla de frecuencias con el anio 2016.
tabla_freq_peso_2016 <-datos %>% 
      filter(anio == 2016) %>%
      select(peso) %>% 
      mutate(peso_grupos = cut(peso, breaks = cant_int)) %>% 
      group_by(peso_grupos) %>% 
      summarise(freq_abs = n()) %>% 
      mutate(freq_abs_acum = cumsum(freq_abs)) %>% 
      mutate(freq_rel = freq_abs / sum(freq_abs), freq_rel_acum = cumsum(freq_rel))

# Primero hacemos la tabla de frecuencias con el anio 2017.
tabla_freq_peso_2017 <- datos %>% 
      filter(anio == 2017) %>%
      select(peso) %>% 
      mutate(peso_grupos = cut(peso, breaks = cant_int)) %>% 
      group_by(peso_grupos) %>% 
      summarise(freq_abs = n()) %>% 
      mutate(freq_abs_acum = cumsum(freq_abs)) %>% 
      mutate(freq_rel = freq_abs / sum(freq_abs), freq_rel_acum = cumsum(freq_rel))

# b)

# Histograma de freq. absolutas del peso para 2016
datos %>% 
  filter( anio == 2016) %>% 
  select(peso) %>% 
  ggplot(aes(x = peso)) +
  geom_histogram(color = "white", bins = cant_int) +
  scale_x_continuous(breaks = seq(0, 160, 10)) +
  scale_y_continuous(breaks = seq(0, 130, 10)) +
  labs(title = "Histograma para el peso en el 2016", x = "Peso", y = "Freq. abs.") +
  theme_bw()


# Histograma de freq. absolutas del peso para 2017
datos %>% 
  filter( anio == 2017) %>% 
  select(peso) %>% 
  ggplot(aes(x = peso)) +
  geom_histogram(color = "white", bins = cant_int) +
  scale_x_continuous(breaks = seq(0, 160, 10)) +
  scale_y_continuous(breaks = seq(0, 130, 10)) +
  labs(title = "Histograma para el peso en el 2017", x = "Peso", y = "Freq. abs.") +
  theme_bw()

# c) TODO: ni idea

rm(cant_int)

#----------------------------------------------------------------------------------------------------#

# Ejercicio 6 ----

# 6)
# a) Analice las medidas de resumen de las variables cuantitativas 
# b) Son comparables los valores de las medidas de perimetro y peso? 
# c) Que variable esta mejor medida?

# Medidas resumen del peso
resumen_peso <- datos %>% 
                select(peso) %>% 
                summarise(media = mean(peso), des_est = sd(peso), min = min(peso), max = max(peso))

# Medidas resumen del perimetro
resumen_perim <- datos %>% 
                  select(perim) %>% 
                  summarise(media = mean(perim), des_est = sd(perim), min = min(perim), max = max(perim))

# b) No son comparables los valores de las medidas porque están expresadas en unidades distintas y 
# representan cosas totalmente diferentes.

# c) Para conocer esto hay que usar el coeficiente de variación (CV)

# Medidas resumen del peso
resumen_peso <- datos %>% 
  select(peso) %>% 
  summarise(media = mean(peso), des_est = sd(peso), min = min(peso), max = max(peso), cv = des_est/media)

# Medidas resumen del perímetro
resumen_perim <- datos %>% 
  select(perim) %>% 
  summarise(media = mean(perim), des_est = sd(perim), min = min(perim), max = max(perim), cv = des_est/media)

# Como el perim tiene un cv del 19% aprox. se puede decir que esta mejor medida que el peso (que tiene un 50%)

#----------------------------------------------------------------------------------------------------#

# Ejercicio 7 ----

# 7) 
# a) Analice las medidas de resumen de las variables cuantitativas segun una variable cualitativa 
# b) Explique que significa cada una 
# c) Que variable está mejor medida?

# Analizamos el peso segun el año

resumen_peso_por_anio <- datos %>% 
  group_by(anio) %>% 
  select(peso) %>% 
  summarise(media = mean(peso), des_est = sd(peso), min = min(peso), max = max(peso), cv = des_est/media)

# Analizamos el perimetro segun año

resume_perim_por_anio <- datos %>% 
  group_by(anio) %>% 
  select(perim) %>% 
  summarise(media = mean(perim), des_est = sd(perim), min = min(perim), max = max(perim), cv = des_est/media)

# Como el perim tiene un cv del 19% aprox. se puede decir que esta mejor medida que el peso (que tiene un 50%)

#----------------------------------------------------------------------------------------------------#

# Ejercicio 8 ----

# 8) Calcule para la variable Perímetro el porcentaje teórico con perímetro mayor a 17.8 cm y 
# compare con la proporción de la muestra

# Para el valor teorico vamos a usar una normal usando las estimaciones puntuales como parametros.
p_teorico <- pnorm(17.8, mean = 17.19315, sd = 3.277521, lower.tail = FALSE)

# Ahora calculamos la proporcion de la muestra
cant_casos_favorables <- sum(datos$perim > 17.8)
p_obs <- cant_casos_favorables / nrow(datos)

paste("La proporcion observada es de: ", round(p_obs, 4) * 100, "%")
paste("La proporcion teorica es de: ", round(p_teorico, 4) * 100, "%")

#----------------------------------------------------------------------------------------------------#

# Ejercicio 9 ----

# 9) Si se toma 20 cabezas de ajo cuál es la probabilidad de encontrar 
# menos de 5 con perímetro menor a 17.8 cm

# Ya que no conocemos le verdadero valor de la probabilidad, vamos a aproximarlo con la
# proporción observada. De esta forma, obtendremos una estimacion de la probabilidad.

p_binomial <- pbinom(4, size = 20, prob = p_obs)
paste("La probabilidad  de encontrar menos de 5 cabezas de ajo con perimetro
      menor a 17.8 en 20 revisadas es de", round(p_binomial, 4))

