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




