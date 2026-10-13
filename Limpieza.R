#Lectura de mi base de precipitaciones desde un documento csv.
install.packages('tsiblle')
library(readr)
data <- read.csv2("C:/Users/math.epn.edu.ec/Documents/SERIES TEMPORALES/inamhi-precipitacion-2019diciembre.csv")
head(data)

#Se procede al cambio de nombre de la columna 'anio'de la base de datos por 'Año'por estetica
colnames(data)
data2 <- data;names(data2)[names(data2) == "anio"] <- "Año"
head(data2)

# Transformación a tsibble, tomamos a la variable Año como índice y tomamos una clave: Estación
library(tsibble)
data3 <- data2 %>% as_tsibble(key = Estacion,index = Año)
head(data3) 
