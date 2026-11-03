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
library(jsonlite)
library(tidyverse)
library(tidyjson)

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

# importación datos de mortalidad
peninsula <- tidyjson::read_json("INPUT/DATA/peninsula.json")

x <- fromJSON("INPUT/DATA/peninsula.json")

datos <- peninsula %>% 
  gather_array("serie") %>%
  spread_all() %>%
  select(document.id, Nombre) %>%
  filter(str_detect(Nombre, "Menos de 1 año|Menores de 1 año")) %>% # se filtran las edades para que sólo sean bebés menores de 1 año
  enter_object(Data) %>%
  gather_array() %>%
  spread_all() %>%
  filter(Anyo >= 2000, Anyo <= 2025) %>% # se filtran los años de 2000 a 2025 (no hay 2026) 
  select(Nombre, Anyo, Valor, T3_TipoDato) %>%
  as_tibble()

View(datos)


#Representación de los datos de olas de calor en ggplot2

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
  
# Representación cuántas muertes y cuánto varian entre años según la comunidad y la causa

# se separa el nombre de la serie en columnas
datos <- datos %>%
  separate_wider_delim(Nombre, delim = ". ",
                       names = c("causa", "sexo", "edad", "comunidad", "tipo"),
                       too_many = "drop")

# se define la gráfica de cajas
grafica_caja <- function(df, titulo, etiqueta_x){
  ggplot(df, aes(x = fct_reorder(grupo, Valor), y = Valor, fill = grupo)) +
    geom_boxplot(alpha = 0.4, outlier.shape = NA, show.legend = FALSE) +
    geom_jitter(aes(colour = grupo), height = 0.15, size = 2,
                show.legend = FALSE) +
    labs(x = etiqueta_x, y = "Número de muertes en menores de 1 año", title = titulo) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}
  
# se toman todas las causas y ambos sexos, por comunidad
por_comunidad <- datos %>%
  filter(str_detect(causa, "Todas las causas"), sexo == "Total",
         comunidad != "Total Nacional", comunidad != "Extranjero") %>%
  rename(grupo = comunidad)
  
# se toma el total nacional y ambos sexos, por causa
por_causa <- datos %>%
  filter(!str_detect(causa, "Todas las causas"), sexo == "Total",
         comunidad != "Total Nacional", comunidad != "Extranjero") %>%
  group_by(causa, Anyo) %>%
  summarise(Valor = sum(Valor), .groups = "drop") %>%
  rename(grupo = causa) %>%
  mutate(grupo = str_wrap(grupo, width = 25)) # para los nombres demasiado largos
  
# se dibujan las dos gráficas
grafica_caja(por_comunidad, "Por comunidad (2000-2025)", "Comunidad autónoma")
grafica_caja(por_causa, "Por causa (2000-2025)", "Causa de muerte")

# Importación de datos de PIB 
pib_raw <- fromJSON("INPUT/DATA/pib.json")

pib_tabla <- pib_raw %>%
  as_tibble() %>%
  select(COD, Nombre, T3_Unidad, T3_Escala, Data) %>%
  unnest(cols = c(Data)) %>%
  as_tibble()

# Se toma los datos del PIB por CCAA
pib_ccaa <- pib_tabla %>%
  filter(!str_detect(Nombre, "Total Nacional|España|Extranjero")) %>%
  filter(T3_TipoDato %in% c("Definitivo", "Provisional", "Avance")) %>%
  mutate(
    grupo = str_remove(Nombre, ".*\\.\\s*"),
    Valor = Valor / 1e6  # Pasamos el valor a Miles de Millones de € para el eje Y
  )

# Representación de los datos de PIB por CCAA
ggplot(pib_ccaa, aes(x = grupo, y = Valor, color = Valor)) +
  geom_point(size = 4, alpha = 0.8) +
  scale_color_viridis_c(option = "inferno") +
  labs(
    title = "PIB por Comunidad Autónoma",
    x = "Comunidad Autónoma",
    y = "PIB (MM €)",
    color = "PIB (MM €)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

# Se definde la gráfica de cajas 
grafica_caja_pib <- function(df, titulo, etiqueta_x){
  ggplot(df, aes(x = fct_reorder(grupo, Valor), y = Valor, fill = grupo)) +
    geom_boxplot(alpha = 0.4, outlier.shape = NA, show.legend = FALSE) +
    geom_jitter(aes(colour = grupo), height = 0, width = 0.15, size = 2, show.legend = FALSE) +
    labs(x = etiqueta_x, y = "PIB (Miles de millones de €)", title = titulo) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1),
      plot.title = element_text(face = "bold")
    )
}

# Dibujamos la gráfica
grafica_caja_pib(pib_ccaa, "Distribución del PIB por Comunidad Autónoma (2000-2025)", "Comunidad autónoma")
