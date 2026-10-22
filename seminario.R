install.packages("scales")
install.packages("vctrs")
install.packages("lifecycle")
install.packages("climaemet")



aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJzYW50aWFnb2hlcnJlcm9ydWlwZXJlekBnbWFpbC5jb20iLCJqdGkiOiJmMTk1MGIxNS02ZTRhLTRlNDItOTFmOC00N2Y2NWFlNTFiM2IiLCJleHAiOjE3OTkyMTgzNDUsImlzcyI6IkFFTUVUIiwiaWF0IjoxNzkwNTc4MzQ1LCJ1c2VySWQiOiJmMTk1MGIxNS02ZTRhLTRlNDItOTFmOC00N2Y2NWFlNTFiM2IiLCJyb2xlIjoiIn0.LtEUbPj5lkXjO1rb2dew51L0hIMRshfRe1K-g4_8HL4", install = TRUE)

library(lubridate)
library(dplyr)
library(climaemet)


clima_diario <- climaemet::aemet_daily_clim(
  station = "all", 
  start = "2000-01-01",
  end = "2001-01-01"
)
head(clima_diario)

#En España, la AEMET considera que hay una ola de calor cuando se registran
#temperaturas máximas diarias superiores al percentil 95 de su serie hitórica
#de referencia (meses de julio y agosto) en al menos eñ 10% de las estaciones
#metereológicas, durante un periodo mínimo de tres días consecutivos,
#AEMET considera que la referencia de datos son los registrados entre los años
#1971 y 2000

#no volver a ejecutar
datos_referencia <- climaemet::aemet_daily_clim(
  station = "all",
  start = "1971-01-01",
  end = "2000-12-31"
) %>% 
  filter(month(fecha) %in% c(7,8))
head(datos_referencia)
#no volver a ejecutar
lista <- list()
for (y in 2001:2025) {
  lista[[as.character(y)]] <- climaemet::aemet_daily_clim(
    station = "all",
    start   = paste0(y, "-05-01"),
    end     = paste0(y, "-09-30")
  )
}
datos_trabajar <- dplyr::bind_rows(lista)




#EJECUTAR A PARTOR DE AQUI
library(lubridate)
library(dplyr)
library(climaemet)

#calculo percentil 95:
p95 <- datos_referencia %>%
  filter(!is.na(tmax)) %>%
  group_by(indicativo) %>%
  summarise(p95 = quantile(tmax, 0.95), n_obs = n(), .groups = "drop") %>%
  filter(n_obs >= 600)

#tabla de % de estaciones supera el umbral de calor requerido
diario <- datos_trabajar %>%
  inner_join(p95, by = "indicativo") %>%
  filter(!is.na(tmax), !is.na(p95)) %>%
  group_by(fecha) %>%
  summarise(n_est = n(),
            pct_sup = mean(tmax > p95) * 100,
            .groups = "drop") %>%
  arrange(fecha) %>%
  mutate(dia_calido = pct_sup >= 10)

#olas:
olas <- diario %>%
  mutate(nueva = is.na(lag(dia_calido)) |
           dia_calido != lag(dia_calido) |
           as.numeric(fecha - lag(fecha)) != 1,
         racha = cumsum(nueva)) %>%
  filter(dia_calido) %>%
  group_by(racha) %>%
  summarise(inicio = min(fecha),
            fin = max(fecha),
            dias = n(),
            max_pct = max(pct_sup),
            .groups = "drop") %>%
  filter(dias >= 3)


olas %>% count(año = year(inicio))

