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




#EJECUTAR A PARTIR DE AQUI
library(lubridate)
library(dplyr)
library(climaemet)
library(ggplot2)

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


#Representación de los datos de holas de calor en ggplot2

#Representación para visualizar cuando ocurrieron las rachas de calor, la intendidad y cuanto duró cada uan
ggplot(olas, aes(x = inicio, xend = fin, y = reorder(factor(racha), inicio), color = max_pct)) +
  geom_segment(linewidth = 2, lineend = "round") +
  scale_color_viridis_c(option = "inferno") + #escala de calor
  labs(
    title = "Cronología e Intensidad de Olas de Calor",
    subtitle = "Cada barra representa una racha desde su inicio hasta su fin",
    x = "Fecha",
    y = "Racha",
    color = "Max Pct (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

#Representacion para ver si las olas de calor más duraderas son también las más intensas
ggplot(olas, aes(x = dias, y = max_pct, color = year(inicio))) +
  geom_point(size = 4, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "gray50") +
  labs(
    title = "Relación entre Duración e Intensidad",
    subtitle = "Comparativa de eventos de olas de calor",
    x = "Duración (Días)",
    y = "Intensidad Máxima (Max Pct %)",
    color = "Año"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold")
  )
#Representacion para ver si la intensidad de las olas de calor aumenta según los años
ggplot(olas, aes(x = inicio, xend = fin, y=max_pct, color=max_pct))+
  geom_point(size=4, alpha=0.8) +
  geom_smooth(method = "lm", se=FALSE, linetype="dashed", color="gray50") +
  labs(
    title = "Relación entre Año e Intensidad",
    x="Año",
    y="Intensidad Máxima(Max Pct%",
    color="Año"
  )+
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face="bold")
  )
  
