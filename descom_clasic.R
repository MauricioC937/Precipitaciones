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
  filter(NombreEstacion == est) %>%
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