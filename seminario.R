#----------------------------------------------------------------------------------
#INSTALACIÓN DE PAQUETES Y API
#----------------------------------------------------------------------------------
install.packages("scales")
install.packages("vctrs")
install.packages("lifecycle")
install.packages("climaemet")



aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJzYW50aWFnb2hlcnJlcm9ydWlwZXJlekBnbWFpbC5jb20iLCJqdGkiOiJmMTk1MGIxNS02ZTRhLTRlNDItOTFmOC00N2Y2NWFlNTFiM2IiLCJleHAiOjE3OTkyMTgzNDUsImlzcyI6IkFFTUVUIiwiaWF0IjoxNzkwNTc4MzQ1LCJ1c2VySWQiOiJmMTk1MGIxNS02ZTRhLTRlNDItOTFmOC00N2Y2NWFlNTFiM2IiLCJyb2xlIjoiIn0.LtEUbPj5lkXjO1rb2dew51L0hIMRshfRe1K-g4_8HL4", install = TRUE)

#----------------------------------------------------------------------------------
#CARGA DE LIBRERIAS
#----------------------------------------------------------------------------------

library(lubridate)
library(dplyr)
library(climaemet)
library(ggplot2)
library(jsonlite)
library(tidyverse)
library(tidyjson)



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





#EJECUTAR A PARTIR DE AQUI


#----------------------------------------------------------------------------------
#CARGA DE DATOS
#----------------------------------------------------------------------------------

#CARGAR EL ENVIROMENT GUARDADO COMO datosolascalor.RData

peninsula <- tidyjson::read_json("INPUT/DATA/peninsula.json")

pib_raw <- fromJSON("INPUT/DATA/pib.json")


#----------------------------------------------------------------------------------
#LIMPIEZA DE DATOS
#----------------------------------------------------------------------------------
datos_trabajar <- dplyr::bind_rows(lista)

#limpio el formato de precipitación y aseguro tipo date
datos_trabajo2 <- datos_trabajar %>%
  mutate(
    fecha = as.Date(fecha),
    prec = as.numeric(stringr::str_replace(prec, ",", "."))
  )

#calculo del percentil 95 por estacion
p95 <- datos_referencia %>%
  filter(!is.na(tmax)) %>%
  group_by(indicativo) %>%
  summarise(p95 = quantile(tmax, 0.95), n_obs = n(), .groups = "drop") %>%
  filter(n_obs >= 600)

#tabla diaria del % de estaciones que superan el umbral de calor
diario <- datos_trabajo2 %>%
  inner_join(p95, by = "indicativo") %>%
  filter(!is.na(tmax), !is.na(p95)) %>%
  group_by(fecha) %>%
  summarise(
    n_est = n(),
    pct_sup = mean(tmax > p95) * 100,
    .groups = "drop"
  ) %>%
  arrange(fecha) %>%
  mutate(dia_calido = pct_sup >= 10)

#identificacion de rachas (olas de calor oficiales de >= 3 días)
olas <- diario %>%
  mutate(
    nueva = is.na(lag(dia_calido)) |
      dia_calido != lag(dia_calido) |
      as.numeric(fecha - lag(fecha)) != 1,
    racha_id = cumsum(nueva)
  ) %>%
  filter(dia_calido) %>%
  group_by(racha_id) %>%
  summarise(
    inicio = as.Date(min(fecha)),
    fin = as.Date(max(fecha)),
    dias = n(),
    max_pct = max(pct_sup),
    .groups = "drop"
  ) %>%
  filter(dias >= 3) %>%
  mutate(racha = row_number()) #id del 1 al total de olas

olas %>% count(año = lubridate::year(inicio))

# ¿QUÉ DIMENSIÓN DE LAS OLAS DE CALOR EJERCE UN MAYOR IMPACTO DIFERENCIAL SOBRE LA MORTALIDAD INFANTIL?

#expando cada ola oficial a sus días concretos usando id_ola para que no coincida con racha de aemet (viento)
dias_ola_oficial <- do.call(rbind, lapply(seq_len(nrow(olas)), function(i) {
  data.frame(
    fecha = seq(as.Date(olas$inicio[i]), as.Date(olas$fin[i]), by = "day"),
    id_ola = olas$racha[i]
  )
}))

#cruzo con los datos diarios de las estaciones para calcular metricas por provincia y año
olas_por_provincia <- datos_trabajo2 %>%
  mutate(fecha = as.Date(fecha)) %>%
  inner_join(dias_ola_oficial, by = "fecha") %>%
  inner_join(p95, by = "indicativo") %>%
  filter(tmax > p95) %>%
  mutate(
    anio = lubridate::year(fecha),
    exceso_termico = tmax - p95
  ) %>%
  ungroup() %>%
  group_by(provincia, anio, id_ola) %>%
  summarise(
    dias_ola = dplyr::n_distinct(fecha),
    intensidad_media = mean(exceso_termico, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(provincia, anio) %>%
  summarise(
    frecuencia_anual = dplyr::n_distinct(id_ola),
    duracion_media_ola = mean(dias_ola, na.rm = TRUE),
    intensidad_acumulada = sum(intensidad_media, na.rm = TRUE),
    .groups = "drop"
  )

head(olas_por_provincia)




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




# se separa el nombre de la serie en columnas
datos <- datos %>%
  separate_wider_delim(Nombre, delim = ". ",
                       names = c("causa", "sexo", "edad", "comunidad", "tipo"),
                       too_many = "drop")

# se define la gráfica de cajas

# Representación cuántas muertes y cuánto varian entre años según la comunidad y la causa
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




pib_tabla <- pib_raw %>%
  as_tibble() %>%
  mutate(
    nivel      = map_chr(MetaData, ~ .x$T3_Variable[1]),  # CCAA / Provincias / NUTS1 / Total
    territorio = map_chr(MetaData, ~ .x$Nombre[1]),       # nombre original del INE
    tipo       = map_chr(MetaData, ~ .x$Nombre[2])        # Dato base / Tasa de Variación Interanual / Estructura Porcentual
  ) %>%
  select(COD, nivel, territorio, tipo, T3_Unidad, T3_Escala, Data) %>%
  unnest(Data) %>%
  mutate(Valor = as.numeric(Valor))


# Filtramos solo las CCAA, sin provincias ni ciudades
pib_ccaa <- pib_tabla %>%
  filter(nivel == "Comunidades y Ciudades Autónomas") %>%
  mutate(grupo = str_remove(territorio,
                            ", Comunidad de|, Región de|, Principado de|, Comunidad Foral de"))
# Filtramos por tasa de variación y por euros de manera independeinte
pib_euros <- pib_ccaa %>%
  filter(tipo == "Dato base") %>%
  mutate(Valor = Valor / 1e6)          

pib_tasa <- pib_ccaa %>%
  filter(tipo == "Tasa de Variación Interanual")



#----------------------------------------------------------------------------------
#REPRESENTACION
#----------------------------------------------------------------------------------



#Representación de los datos de olas de calor en ggplot2

#Representación para visualizar cuando ocurrieron las rachas de calor, la intendidad y cuanto duró cada una
#ggplot(olas, aes(x = inicio, xend = fin, y = reorder(factor(racha), inicio), color = max_pct)) +
#  geom_segment(linewidth = 2, lineend = "round") +
#  scale_color_viridis_c(option = "inferno") + #escala de calor
#  labs(
#    title = "Cronología e Intensidad de Olas de Calor",
#    subtitle = "Cada barra representa una racha desde su inicio hasta su fin",
#   x = "Fecha",
#  y = "Racha",
# color = "Max Pct (%)"
#  ) +
# theme_minimal(base_size = 12) +
#theme(
# panel.grid.minor = element_blank(),
#plot.title = element_text(face = "bold")
#)

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
ggplot(olas, aes(x = inicio, xend = fin, y=max_pct, color=-max_pct))+
  geom_point(size=4, alpha=0.8) +
  geom_smooth(method = "lm", se=FALSE, linetype="dashed", color="red") +
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





# se dibujan las dos gráficas
grafica_caja(por_comunidad, "Por comunidad (2000-2025)", "Comunidad autónoma")
grafica_caja(por_causa, "Por causa (2000-2025)", "Causa de muerte")



# Representación gráficas de puntos y de cajas
grafica_puntos_pib <- function(df, titulo, etiqueta_y) {
  ggplot(df, aes(x = fct_reorder(grupo, Valor), y = Valor, color = Valor)) +
    geom_point(size = 3.5, alpha = 0.7) +
    scale_color_viridis_c(option = "inferno") +
    labs(title = titulo, x = "Comunidad Autónoma", y = etiqueta_y, color = etiqueta_y) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      plot.title  = element_text(face = "bold")
    )
}

grafica_caja_pib <- function(df, titulo, etiqueta_y) {
  ggplot(df, aes(x = fct_reorder(grupo, Valor), y = Valor, fill = grupo)) +
    geom_boxplot(alpha = 0.4, outlier.shape = NA, show.legend = FALSE) +
    geom_jitter(aes(colour = grupo), height = 0, width = 0.15, size = 1.8,
                show.legend = FALSE) +
    labs(x = "Comunidad autónoma", y = etiqueta_y, title = titulo) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      plot.title  = element_text(face = "bold")
    )
}

# PIB en euros
grafica_puntos_pib(pib_euros, "PIB por Comunidad Autónoma (2000-2025)",
                   "PIB (miles de millones de €)")
grafica_caja_pib(pib_euros, "Distribución del PIB por Comunidad Autónoma (2000-2025)",
                 "PIB (miles de millones de €)")

# Tasa de variación interanual
grafica_puntos_pib(pib_tasa, "Variación interanual del PIB por Comunidad Autónoma (2001-2025)",
                   "Tasa de variación (%)")
grafica_caja_pib(pib_tasa, "Distribución de la variación interanual del PIB por CCAA (2001-2025)",
                 "Tasa de variación (%)")

