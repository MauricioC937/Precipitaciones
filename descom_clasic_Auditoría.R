# Libreriaas
library(readr)
library(tidyverse)
library(fpp3)
library(slider)

# Datos
precipitaciones <- read_csv("data.csv", col_types = cols(Fecha = col_date())) %>%
  mutate(Fecha = yearmonth(Fecha)) %>%
  as_tsibble(key = Estacion, index = Fecha)

# Estacion
est <- "QUEROCHACA(UTA)"
d <- precipitaciones %>%
  filter(NombreEstacion == est,Año > 1990) %>%
  fill_gaps() %>%
  as_tibble()

# Descomposicion manual clasica aditiva, periodo 12 (2x12-MA)
w <- c(1/24, rep(1/12, 11), 1/24)    # pesos de la media móvil centrada

d <- d %>%
  mutate(
    Tt    = slide_dbl(Precipitación, ~ sum(.x * w),
                      .before = 6, .after = 6, .complete = TRUE),
    yt_Tt = Precipitación - Tt,
    mes   = month(as.Date(Fecha))
  )

St_tab <- d %>%
  group_by(mes) %>%
  summarise(S = mean(yt_Tt, na.rm = TRUE), .groups = "drop") %>%
  mutate(St = S - mean(S)) %>%       # centrar para que sume 0
  select(mes, St)

d <- d %>%
  left_join(St_tab, by = "mes") %>%
  mutate(Rt = yt_Tt - St)

# Grafica 1: serie original y tendencia
ggplot(d, aes(as.Date(Fecha))) +
  geom_line(aes(y = Precipitación), colour = "gray70") +
  geom_line(aes(y = Tt), colour = "#D55E00", linewidth = 1) +
  labs(x = NULL, y = "Precipitación (mm)",
       title = paste("Serie original y tendencia (2x12-MA) -", est))

# Grafica 2: componente estacional
ggplot(d, aes(as.Date(Fecha), St)) +
  geom_line(colour = "#0072B2") +
  labs(x = NULL, y = "St",
       title = paste("Componente estacional anual -", est))

# Grafica 3: residuo
ggplot(d, aes(as.Date(Fecha), Rt)) +
  geom_line(colour = "#009E73") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = NULL, y = "Rt",
       title = paste("Componente aleatorio (residuo) -", est))

#--------------------------AUDITORÍA----------------------------
#Contraste con classical_decomposition(). 
#Al aplicar a ambos cálculos se tiene que coinciden la tendencia, la estacionalidad 
#l residuo salvo errores de redondeo. Se comprueba que la diferencia es cero o muy pequeña (máx abs < 1e-15)
Prec_Estacion<- precipitaciones %>% fill_gaps() %>% filter(
  NombreEstacion == 'QUEROCHACA(UTA)',Año>1990) 

referencia <- Prec_Estacion |>
  model(clasica = classical_decomposition(Precipitación, type = "additive")) |>
  components() |>
  as_tibble() |>
  select(Fecha, trend, seasonal, random)

comparacion <- d |>
  select(Fecha, Tt, St, Rt) |>
  inner_join(referencia, by = "Fecha", relationship = "one-to-one")

stopifnot(
  nrow(comparacion) == 336L,
  all(is.na(comparacion$Tt) == is.na(comparacion$trend)),
  all(is.na(comparacion$St) == is.na(comparacion$seasonal)),
  all(is.na(comparacion$Rt) == is.na(comparacion$random))
)

comparacion |>
  transmute(
    Tendencia = Tt - trend,
    Estacionalidad = St - seasonal,
    Residuo = Rt - random
  ) |>
  pivot_longer(everything(), names_to = "Componente", values_to = "Diferencia") |>
  group_by(Componente) |>
  summarise(
    Meses = sum(!is.na(Diferencia)),
    Max_abs_mm = max(abs(Diferencia), na.rm = TRUE),
    .groups = "drop"
  )

#Qué muestran las gráficas
#1. Serie original y tendencia.
#La línea gris muestra la variación mensual y la naranja la suaviza con una media móvil centrada. 
#La tendencia coincide con la calculada por classical_decomposition() 

#2. Componente estacional. 
#La curva repite los mismos 12 valores cada año,
#la línea representa el patrón mensual determinado con classical_decomposition()

#3. Residuo. El caso más destacado se muestra en el año 2000, mismo que se obtiene al aplicar la función
#classical_decomposition(). La línea de cero permite ver rápidamente qué meses quedaron por encima 
#o por debajo de lo esperado según ambos componentes. 

