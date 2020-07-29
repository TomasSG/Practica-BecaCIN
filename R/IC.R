
datos <- read.csv(file = "./data/Ajoblanco.csv", sep = ";")
names(datos) <- c("anio", "perim", "peso")

# IC para la media con desvio conocido con NC = 98% para el peso
t.test(x = datos$peso, conf.level = .98)

# Lo calculamos a mano para asegurarnos de que sea correcto el resultado

n <- length(datos$peso)
nc <- .98
x_obs <- mean(datos$peso)
s_obs <- sd(datos$peso)

t_critico <- qt(p = 0.99, df = n - 1,lower.tail = TRUE)

li <- x_obs - t_critico * (s_obs/sqrt(n))
ls <- x_obs + t_critico * (s_obs/sqrt(n))

paste("IC del 98%: [", round(li, 4), "; ", round(ls, 4), "]", sep = "")

# IC para la media con desvio conocido con NC = 98% para la proporción de muestras con peso mayor a 53
casos_fav <- length(datos$peso[datos$peso > 53])

prop.test(x = casos_fav, n = n, conf.level = .98)

# Calculamos el IC a mano
p_obs <- casos_fav / n
z_critico <- qnorm(p = .99)

li <- p_obs - z_critico * (sqrt(p_obs * (1 - p_obs) / n))
ls <- p_obs + z_critico * (sqrt(p_obs * (1 - p_obs) / n))

paste("IC del 98%: [", round(li, 4), "; ", round(ls, 4), "]", sep = "")


