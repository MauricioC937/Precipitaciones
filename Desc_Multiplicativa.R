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

#-----------Descomposición multiplicativa ---------
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

# Paso 2. Quitar la tendencia dividiendo por la media móvil 
Zt_mult <- Prec_Estacion_ma |>
  mutate(
    yt_div_Tt = Precipitación / `2x12-MA`,
    mes = month(Fecha)
  )

# Paso 3. Cálculo de la media de los valores sin tendencia
indices_mult <- Zt_mult |>
  as_tibble() |>
  group_by(mes) |>
  summarise(media_mes = mean(yt_div_Tt, na.rm = TRUE), .groups = "drop") |>
  transmute(mes, Estacional = media_mes / mean(media_mes))

St_mult <- Zt_mult |>
  left_join(indices_mult, by = "mes")

#Gráfica estacional
St_mult |>
  autoplot(Estacional, colour = "#0072B2") +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey55") +
  labs(title = "Componente estacional multiplicativa", y = "Índice estacional")

# Paso 4. Dividir la serie sin tendencia por el índice estacional
Rt_mult <- St_mult |>
  mutate(Residuo = yt_div_Tt / Estacional)
#Gráfica residual
Rt_mult |>
  autoplot(Residuo, colour = "#009E73", na.rm = TRUE) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey55") +
  labs(title = "Componente residual multiplicativa", y = "Factor residual")

#-----------Descomposición multiplicativa con classical_decomposition ---------

Prec_Estacion |>
  model(
    classical_decomposition(Precipitación, type = "multiplicative")
  ) |>
  components() |>
  autoplot() +
  labs(
    title = "Descomposición aditiva multiplicativa"
  )

