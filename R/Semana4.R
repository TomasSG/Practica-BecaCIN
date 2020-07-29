library(UsingR)
library(ggplot2)

# Miramos el data frame
father.son

# Como es muy largo, miramos las primeras filas
head(father.son)

# Graficamos en un diagrama de dispersión
graf <- ggplot(data = father.son, aes(x = father.son$fheight, y = father.son$sheight)) +
          geom_point() +
          scale_x_continuous(name = "Father height") +
          scale_y_continuous(name = "Son height") +
          theme_classic()
graf

# TODO: ¿Hacer algo con regresión?