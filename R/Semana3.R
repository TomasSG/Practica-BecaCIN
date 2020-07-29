library(ggplot2)

sample(1 : 100, 1000, replace = T)
# sample me toma 1000 muestras del vector 1:100 con reemplazo. Notar que si se desean tomar más
# muestras que elementos en el vector, entonces el replace debe ser TRUE porque en otro caso no se 
# tendrían suficientes elementos.

rep(x = 1, 20)
# rep repite el valor o vector que se especifica en x la cantidad de veces que se inidica en el 
# segundo parámetro

#data <- data.frame(x = sample(seq(1, 300, 0.2), 1000, replace = T))
#data <- data.frame(x = rnorm(n = 60, mean = 50, sd = 0.5))
ggplot(data, aes(x = x)) +
  geom_histogram(bins = 10)
