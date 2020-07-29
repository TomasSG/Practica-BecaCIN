# Se sabe que el número de flexiones se distribuye según una Normal de varianza poblacional 7.5. 
# ¿Puede asumirse, considerando un nivel de significación del 5%, 
# que el número medio de flexiones que realizan los alumnos es de 55?

# H0: mu_0 = 55
# H1: mu_1 != 55

datos <- read.table("data/Flexiones.txt", header = TRUE)

alpha <- .05
x_obs <- mean(datos$Flexiones)
mu_0 <- 55
var <- 7.5
n <- nrow(datos)

z_obs <- (x_obs - mu_0) / sqrt(var / n)

# Calculamos el p-valor

p_valor <- 2 * pnorm(abs(z_obs), lower.tail = FALSE)
print(paste("El p-valor obtenido para", round(z_obs, 4), "es de", round(p_valor, 4)))

if( p_valor > alpha) {
  print("No hay evidencia para rechazar la hipotesis nula")
} else {
  print("Hay evidencia para rechazar la hipotesis nula, en favor de la hipotesis alternativa")
}

# Contrastar a un nivel de significación del 2% la hipótesis de que el número medio de flexiones realizada por los alumnos 
# es de 50. Suponer en este caso que el número de flexiones se distribuye según una normal de varianza desconocida.

# H0: mu_0 = 55
# H1: mu_1 != 55

t.test(x = datos$Flexiones, mu = 50, alternative = "two.sided", conf.level = .98 )

# Lo calculamos a mano

alpha <- .02
x_obs <- mean(datos$Flexiones)
mu_0 <- 50
s_obs <- sd(datos$Flexiones)
n <- nrow(datos)

t_obs <- (x_obs - mu_0) / (s_obs / sqrt(n))

p_valor <- 2 * pt(q = abs(t_obs),df = n -1, lower.tail = FALSE)

print(paste("El p-valor obtenido para", round(z_obs, 4), "es de", round(p_valor, 4)))

if( p_valor > alpha) {
  print("No hay evidencia para rechazar la hipotesis nula")
} else {
  print("Hay evidencia para rechazar la hipotesis nula, en favor de la hipotesis alternativa")
}

#  Contrastar a un nivel de confianza del 95%, si la proporción de alumnos varones es mayor o igual que 0.5 
# frente a que dicha proporción es menor

# H0: p >= .5
# H1: p < .5

n <- nrow(datos)
x <- length(datos$Flexiones[datos$Sexo == "H"])
prop.test(x = x, n = n, p = .5, alternative = "less", conf.level = .95)


# Lo calculamos a mano


alpha <- .05
x <- length(datos$Flexiones[datos$Sexo == "H"])
n <- nrow(datos)
p_obs <-x / n
p_0 <- .5


z_obs <- (p_obs - p_0) / sqrt(p_obs * (1 - p_obs) / n)

p_valor <- pnorm(q = z_obs,lower.tail = TRUE)

print(paste("El p-valor obtenido para", round(z_obs, 4), "es de", round(p_valor, 4)))
