#Librerias
library(readr)
library(tidyverse)
library(ggplot2)
library(fpp3)

#Abrimos la base de datos 
precipitaciones <- read_csv("data.csv",col_types = cols(Fecha = col_date())
                            ) %>% mutate(Fecha = yearmonth(Fecha)) %>% 
                            as_tsibble(key = Estacion, index = Fecha)
head(precipitaciones)

#Gráfica de evolución temporal

#Evolución temporal por estaciones donde se tomaron mediciones de precipitación
unique(precipitaciones$NombreEstacion) #Estaciones existentes
#Se toma la estación QUEROCHACA(UTA)
Prec_Estacion<- precipitaciones %>% fill_gaps() %>% filter(
  NombreEstacion == 'QUEROCHACA(UTA)') %>% autoplot(Precipitación) +
  labs(x = "Fecha", y = "Precipitación", title = 'Precipitaciones en la Estación Querochaca')
Prec_Estacion


#Comparación del patrón de precipitaciones de cada año

Patron_Estacion <- precipitaciones %>% filter(NombreEstacion == 'QUEROCHACA(UTA)') %>% 
  fill_gaps() %>% gg_season(Precipitación, period = "year") +
  labs(x = "Mes", y = "Precipitación", title = 'Patron de Precipitaciones-Estación Querochaca')
Patron_Estacion

#Variación de las precipitaciones por mes y promedio mensual 
Var_Estacion <- precipitaciones %>% filter(NombreEstacion == 'QUEROCHACA(UTA)') %>% 
  fill_gaps() %>% gg_subseries(Precipitación) +
  labs(x = "Año", y = "Precipitación", title = 'Variación de Precipitaciones-Estación Querochaca')
Var_Estacion


# Comentario JV
# Tendencia
# No hay una tendencia creciente (no está lloviendo cada vez mas) ni decreciente a largo plazo
# Es una serie estacionaria en la media
# 2. Estacionalidad
# Estacionalidad moderada a fuerte con patron anual repetitivo.
# - Picos altos (lluvia): Abril, Mayo y Junio, con un extremo en Mayo del 2000 
# -Picos bajos (seco): Enero, Agosto y Septiembre
# 3. Ciclos
# No hay ciclos evidentes, no existen ondas de subida o bajada que duren mas de 2 años,
# solo fluctuaciones anuales y picos aislados 
# 4. Estacionariedad
# -Fuerte: No se cumple.
# - Debil: Se cumple a nivel anual por tener media constante (50 mm aprox), pero a nivel mensual 
#   varia por la estacionalidad (Junio 75 mm vs Agosto 35 mm (aprox)). 

