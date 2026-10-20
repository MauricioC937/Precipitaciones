#Lectura de mi base de precipitaciones desde un documento csv.
install.packages('tsibble')
library(readr)
data <- read.csv2("data-row.csv")
head(data)

#Se procede al cambio de nombre de la columna 'anio'de la base de datos por 'Año'por estetica
colnames(data)
data2 <- data;names(data2)[names(data2) == "anio"] <- "Año"
head(data2)

library(tidyverse)
library(tsibble)

#Nuestro archivo data2 tiene un formato ancho, por lo que se procede a transformarlo a un formato largo, 
#creando un índice mensual.
meses <- c("ene", "feb", "mar", "abr", "may", "jun",
           "jul", "ago", "sep", "oct", "nov", "dic")

# Transformación a tsibble, tomamos a la variable Fecha como índice y tomamos una clave: Estación
dt <- data2 %>% pivot_longer(        #Se transforma las columnas mensuales en dos columnas: Mes y Precipitacion. 
    cols = all_of(meses),            #Así, cada fila pasa a representar un mes de una estación.
    names_to = "Mes",
    values_to = "Precipitación"
  ) %>% mutate(
    NumeroMes = match(Mes, meses),
    Fecha = as.Date(
      yearmonth(sprintf("%d-%02d", Año, NumeroMes)) #Se crea un índice mensual a partir del año y el número de mes.
    )
  ) %>% select(-NumeroMes) %>% as_tsibble(index = Fecha, key = Estacion)
head(dt)
#Guardamos la base de datos en un archivo csv.
write_csv(dt, "data.csv")
