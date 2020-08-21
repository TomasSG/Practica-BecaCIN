# --------------------------------------Bibliotecas------------------------------------

library(readxl, warn.conflicts = FALSE)
library(ggplot2)
library(ggthemes)
library(extrafont)
library(scales)
library(lubridate)

font_import()

# --------------------------------------Cargar datos------------------------------------

datos <- read_excel("./data/Gaby-presion-JULIO 2020.xlsx")
View(datos)
names(datos) <- gsub("á", "a", tolower(names(datos)))
names(datos) <- gsub("í", "i", tolower(names(datos)))
names(datos)[6] <- "pulsaciones"
datos$momento <- factor(datos$momento, levels = c("M", "T", "N"), labels = c("Mañana", "Tarde", "Noche"))
is.POSIXt(datos$fecha)
                
# --------------------------------------Visualización------------------------------------

colores <- c("Pulsaciones" = "firebrick", 
             "Presión arterial máxima" = "darkblue", 
             "Presión arterial mínima" = "darkorange")

ggplot(datos, aes(x = fecha)) +
  geom_line(aes(y = pulsaciones, color = "Pulsaciones"), size = 1.5) +
  geom_line (aes(y = max, color = "Presión arterial máxima"), size = 1.5) +
  geom_line(aes(y = min, color = "Presión arterial mínima"), size = 1.5) +
  geom_vline(xintercept = as.POSIXct("2020-07-21"), linetype = "dashed", size = 1.5, color = "gray") +
  geom_vline(xintercept = as.POSIXct("2020-08-04"), linetype = "dashed", size = 1.5, color = "gray") +
  geom_vline(xintercept = as.POSIXct("2020-08-06"), linetype = "dashed", size = 1.5, color = "gray") +
  theme_classic() +
  xlab("Fecha") +
  ylab("") +
  labs(color = "Curvas") +
  ggtitle("Seguimiento de presión y pulsaciones", "¿Tendrá algún problema mamá?") +
  theme(plot.title = element_text(color = "black", size = 25), 
        plot.subtitle = element_text(size = 20),
        axis.title = element_text(color = "black", size = 15),
        text = element_text(family = "Century"),
        panel.grid.major = element_line(color = "gray"), 
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        legend.position = "bottom") +
  scale_y_continuous(breaks = seq(0, 200, 50), limits = c(0, 200)) +
  scale_x_datetime(labels = date_format("%d/%m"), breaks = date_breaks("4 day")) +
  facet_wrap(~ momento)
