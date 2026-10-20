#Lectura de mi base de precipitaciones desde un documento csv.
install.packages('tsibble')
library(readr)
data <- read.csv2("data-row.csv")
head(data)

#Se procede al cambio de nombre de la columna 'anio'de la base de datos por 'Año'por estetica
colnames(data)
data2 <- data;names(data2)[names(data2) == "anio"] <- "Año"
head(data2)

# Transformación a tsibble, tomamos a la variable Año como índice y tomamos una clave: Estación
library(tsibble)
dt <- data2 %>% as_tsibble(index = Año, key = Estacion)
head(dt)

library(tidyverse)
#Guardamos la base transformada a tsibble en un documento csv
dt %>% write_csv("data.csv")
