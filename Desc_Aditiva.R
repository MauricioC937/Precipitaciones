#Librerias
library(readr)
library(tidyverse)
library(ggplot2)
library(readr)
library(fpp3)

#Abrimos la base de datos 
precipitaciones <- read_csv("data.csv",col_types = cols(Fecha = col_date())) %>% 
  mutate(Fecha = yearmonth(Fecha)) %>% as_tsibble(key = Estacion, index = Fecha)


#Evolución temporal por estaciones donde se tomaron mediciones de precipitación
#Se toma la estación QUEROCHACA(UTA)
Prec_Estacion<- precipitaciones %>% fill_gaps() %>% filter(
  NombreEstacion == 'QUEROCHACA(UTA)',Año>1990) 

#Gráfica temporal de la precipitación en la estación QUEROCHACA(UTA)
Prec_Estacion%>% autoplot(Precipitación) +
  labs(x = "Fecha", y = "Precipitación", title = 'Precipitaciones en la Estación Querochaca')


#------------------Descomposición aditiva-----------------------
#Paso 1: Estimar la tendencia con una media móvil centrada 2 × 12-MA.
#El Periodo de la serie es 12, ya que es una serie mensual
Prec_Estacion_ma <- Prec_Estacion |>
  mutate(
    `12-MA` = slider::slide_dbl(
      Precipitación,
      mean,
      .before = 5,
      .after = 6,
      .complete = TRUE
    ),
    `2x12-MA` = slider::slide_dbl(
      `12-MA`,
      mean,
      .before = 1,
      .after = 0,
      .complete = TRUE
    )
  )
#Gráfica de la serie original y la tendencia T^_t
Prec_Estacion_ma |>
  autoplot(Precipitación, colour = "gray") +
  geom_line(aes(y = `2x12-MA`), colour = "#D55E00") +
  labs(
    y = "Precipitación",
    title = "Precipitación en la estación Querochaca"
  )
yt <- Prec_Estacion  # Serie original
Tt <- Prec_Estacion_ma # Serie de tendencia

#Paso 2: Serie sin tendencia: y_t - T^_t
Zt <- Prec_Estacion %>% mutate(yt_Tt=yt$Precipitación-Tt$`2x12-MA`) #Serie sin tendencia
# Gráfico de la serie sin tendencia
Zt |> autoplot(yt_Tt, colour = "#176B87", na.rm = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55") +
  labs(
    title = "Serie sin tendencia",
  )

#Paso 3: Componente estacional
#Calcular la media de los valores sin tendencia, se calcula la media 
#de los valores sin tendencia para cada mes
St <- Zt|>group_by(month(Fecha))|> mutate(media= mean(yt_Tt,na.rm=TRUE))
#Gráfica de la componente estacional
St |> autoplot(media, colour = "#0072B2", na.rm = TRUE)+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55") +
  labs(
    title = "Componente estacional",
  )

#Paso 4: Componente residual
#Calcular el residuo, que es la diferencia entre la serie sin tendencia y 
#la componente estacional Rt=yt_Tt-St
Rt <- St %>% mutate(Residuo=yt_Tt-media)
#Gráfica del residuo
Rt |> autoplot(Residuo, colour = "#009E73", na.rm = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55") +
  labs(
    title = "Componente residual",
  )

#-----------Descomposición aditiva con classical_decomposition ---------

Prec_Estacion |>
  model(
    classical_decomposition(Precipitación, type = "additive")
  ) |>
  components() |>
  autoplot() +
  labs(
    title = "Descomposición aditiva clásica"
  )





