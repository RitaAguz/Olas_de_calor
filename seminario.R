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
  filter(Anyo >= 2001, Anyo <= 2025) %>% # se filtran los años de 2000 a 2025 (no hay 2026) 
  select(Nombre, Anyo, Valor, T3_TipoDato) %>%
  as_tibble()

View(datos)

# se guarda la tabla de mortalidad
save(datos, file = "OUTPUT/DATA/tabla_mortalidad.RData")


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

# ¿CÓMO VARÍA LA MORTALIDAD INFANTIL DURANTE LOS AÑOS CON OLAS DE CALOR ENTRE LAS COMUNIDADES AUTÓNOMAS CON DISTINTO PIB?

# Homogenización de los datos para poder trabajar con ellos
ola_ccaa <- olas_por_provincia %>%
  mutate(provincia = toupper(provincia)) %>%
  mutate(grupo = case_when(
    provincia %in% c("ALMERIA", "ALMERÍA", "CADIZ", "CÁDIZ", "CORDOBA", "CÓRDOBA", "GRANADA",
                     "HUELVA", "JAEN", "JAÉN", "MALAGA", "MÁLAGA", "SEVILLA")        ~ "Andalucía",
    provincia %in% c("HUESCA", "TERUEL", "ZARAGOZA")                                ~ "Aragón",
    provincia %in% c("ASTURIAS")                                                    ~ "Asturias",
    provincia %in% c("BALEARES", "ILLES BALEARS", "ISLAS BALEARES")                 ~ "Balears, Illes",
    provincia %in% c("LAS PALMAS", "PALMAS, LAS", "SANTA CRUZ DE TENERIFE",
                     "STA. CRUZ DE TENERIFE")                                       ~ "Canarias",
    provincia %in% c("CANTABRIA")                                                   ~ "Cantabria",
    provincia %in% c("AVILA", "ÁVILA", "BURGOS", "LEON", "LEÓN", "PALENCIA", "SALAMANCA",
                     "SEGOVIA", "SORIA", "VALLADOLID", "ZAMORA")                    ~ "Castilla y León",
    provincia %in% c("ALBACETE", "CIUDAD REAL", "CUENCA", "GUADALAJARA", "TOLEDO")  ~ "Castilla - La Mancha",
    provincia %in% c("BARCELONA", "GIRONA", "GERONA", "LLEIDA", "LERIDA", "TARRAGONA") ~ "Cataluña",
    provincia %in% c("ALICANTE", "CASTELLON", "CASTELLÓN", "VALENCIA")              ~ "Comunitat Valenciana",
    provincia %in% c("BADAJOZ", "CACERES", "CÁCERES")                               ~ "Extremadura",
    provincia %in% c("CORUÑA, A", "A CORUÑA", "LA CORUÑA", "LUGO", "OURENSE",
                     "PONTEVEDRA")                                                  ~ "Galicia",
    provincia %in% c("MADRID")                                                      ~ "Madrid",
    provincia %in% c("MURCIA")                                                      ~ "Murcia",
    provincia %in% c("NAVARRA")                                                     ~ "Navarra",
    provincia %in% c("ALAVA", "ÁLAVA", "ARABA/ALAVA", "GIPUZKOA", "GUIPUZCOA",
                     "BIZKAIA", "VIZCAYA")                                          ~ "País Vasco",
    provincia %in% c("RIOJA, LA", "LA RIOJA")                                       ~ "Rioja, La",
    provincia %in% c("CEUTA")                                                       ~ "Ceuta",
    provincia %in% c("MELILLA")                                                     ~ "Melilla",
    .default = NA
  ))

# Nos quedamos solo con la ccaa y el año que hubo ola
años_con_ola <- ola_ccaa %>%
  drop_na(grupo) %>%
  distinct(grupo, anio) %>%
  mutate(con_ola = "Con ola")

# Cuento cuántos bebés mueren cada año en cada comunidad
mortalidad <- por_comunidad %>%
  mutate(grupo = str_remove(grupo, ", Comunidad de|, Región de|, Principado de|, Comunidad Foral de"),
         anio  = as.integer(Anyo)) %>%
  group_by(grupo, anio) %>%
  summarise(muertes = sum(Valor, na.rm = TRUE), .groups = "drop") %>%
  filter(anio >= 2001)

# Aquellas comunidades mas grandes (ej: Madrid) tienen una mayor natalidad que otras mas pequeñas, y esta diferencia hace que se distorsionen los datos.
mortalidad <- mortalidad %>%
  group_by(grupo) %>%
  mutate(indice = muertes / mean(muertes) * 100)

# Calculo el PIB de cada ccaa, y lo clasifico segun sea PIB alto o uno bajo 
pib_nivel <- pib_euros %>%
  group_by(grupo) %>%
  summarise(pib_medio = mean(Valor, na.rm = TRUE), .groups = "drop") %>%
  mutate(nivel_pib = ifelse(pib_medio > mean(pib_medio), "PIB alto", "PIB bajo"),
         nivel_pib = factor(nivel_pib, levels = c("PIB bajo", "PIB alto"))) %>%
  arrange(pib_medio)
# Juntamos todos los datos obtenidos hasta el momento en una sola tabla
total <- mortalidad %>%
  left_join(pib_nivel,     by = "grupo") %>%
  left_join(años_con_ola, by = c("grupo", "anio")) %>%
  mutate(con_ola = ifelse(is.na(con_ola), "Sin ola", con_ola))
# Tabla en la que podemos ver el indice medio segun las ccaa con olas y sin olas y según tenga PIB alto o bajo
resumen <- total %>%
  group_by(nivel_pib, con_ola) %>%
  summarise(indice_medio = mean(indice, na.rm = TRUE),
            n_anios_ccaa = n(),
            .groups = "drop")
print(resumen)

# Gráfico de lo obtenido en la tabla de "resumen"
ggplot(data = resumen, aes(x = nivel_pib, y = indice_medio, fill = con_ola)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8) +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Mortalidad infantil según el PIB de la CCAA y la presencia de olas, o no de calor",
       x = "", y = "Índice de mortalidad infantil (100 = año normal)",
       fill = "Año") +
  theme_classic()

# Gráfico en el que se ve la mortalidad por ccaa segun su PIB
por_ccaa <- panel %>%
  filter(con_ola == "Con ola") %>%
  group_by(grupo) %>%
  summarise(indice_medio = mean(indice, na.rm = TRUE), .groups = "drop") %>%
  left_join(pib_nivel, by = "grupo")

ggplot(data = por_ccaa, aes(x = reorder(grupo, pib_medio), y = indice_medio, fill = nivel_pib)) +
  geom_col() +
  labs(title = "Mortalidad infantil en años con ola de calor por CCAA (de menor a mayor PIB)",
       x = "", y = "Índice de mortalidad infantil (100 = año normal)",
       fill = "Grupo de PIB") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))   


#----------------------------------------------------------------------------------
#REPRESENTACION
#----------------------------------------------------------------------------------





# se dibujan las dos gráficas
g_comunidad <- grafica_caja(por_comunidad, "Por comunidad (2001-2025)", "Comunidad autónoma")
g_causa <- grafica_caja(por_causa, "Por causa (2001-2025)", "Causa de muerte")
g_comunidad
g_causa

# se guardan las gráficas de representación
save(g_comunidad, g_causa, file = "OUTPUT/GRAPHICS/graficas_mortalidad.RData")


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


#----------------------------------------------------------------------------------
#PREGUNTAS
#----------------------------------------------------------------------------------

# 1. ¿Cómo varía la mortalidad infantil según el número de olas de calor en cada comunidad autónoma?

# se cuentan las olas por comunidad y año
olas_anuales <- ola_ccaa %>%
  drop_na(grupo) %>%
  group_by(grupo, anio) %>%
  summarise(n_olas = max(frecuencia_anual), .groups = "drop")

# se unen las olas con la mortalidad
pregunta1 <- pregunta1 %>%
  group_by(grupo) %>%
  mutate(esperado = fitted(lm(muertes ~anio)),
         indice_aj = muertes / esperado * 100) %>%
  ungroup()

# se dibuja la dispersión interactiva
p <- ggplot(pregunta1, aes(x = factor(n_olas), y = indice_aj,
                           text = paste(grupo, "-", anio))) +
  geom_boxplot(alpha = 0.3, outlier.shape = NA, fill = "palevioletred") +
  geom_jitter(width = 0.1, size = 2, colour = "palevioletred") +
  geom_hline(yintercept = 100, linetype = "dashed") +
  labs(x = "Número de olas de calor en el año",
       y = "Mortalidad infantil respecto a lo esperado (100 = lo esperado)",
       colour = "Comunidad autónoma") +
  theme_minimal()
plotly::ggplotly(p, tooltip = "text")

# se calcula el índice medio según el número de olas
barras <- pregunta1 %>%
  group_by(n_olas) %>%
  summarise(indice_medio = mean(indice_aj), .groups = "drop") %>%
  mutate(supera = indice_medio > 100)

# se dibuja la gráfica de barras
p1 <- ggplot(barras, aes(x = factor(n_olas), y = indice_medio, fill = supera)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = "palevioletred", "FALSE" = "lightpink")) +
  geom_hline(yintercept = 100, linetype = "dashed") +
  coord_cartesian(ylim = c(80,115)) + # se fija el inicio del eje en 80 para ver mejor las diferencias
  labs(x = "Número de olas de calor en el año",
       y = "Mortalidad respecto a lo esperado") +
  theme_minimal()
p1

# se guardan las gráficas de la pregunta 1
save(p, p1, file ="OUTPUT/GRAPHICS/graficas_pregunta1.RData")

# 2 # ¿Qué dimensión de las olas de calor (intensidad o duración) 
# ejerce un mayor impacto sobre la mortalidad infantil?

#resumo las metricas de ola de calor por CCAA y año 
clima_ccaa_resumen <- ola_ccaa %>%
  drop_na(grupo) %>%
  group_by(grupo, anio) %>%
  summarise(
    frecuencia_anual = sum(frecuencia_anual, na.rm = TRUE),
    duracion_media_ola = mean(duracion_media_ola, na.rm = TRUE),
    intensidad_acumulada = sum(intensidad_acumulada, na.rm = TRUE),
    .groups = "drop"
  )

# creo mortalidad_ccaa desde por_comunidad homogeneizando grupo
mortalidad_ccaa <- por_comunidad %>%
  mutate(
    grupo = str_remove(grupo, ", Comunidad de|, Región de|, Principado de|, Comunidad Foral de"),
    anio = as.numeric(Anyo),
    muertes = as.numeric(Valor)
  ) %>%
  group_by(grupo, anio) %>%
  summarise(muertes = sum(muertes, na.rm = TRUE), .groups = "drop")

#uno y paso a formato largo para comparar duración e intensidad
datos_grafica <- mortalidad_ccaa %>%
  inner_join(clima_ccaa_resumen, by = c("grupo", "anio")) %>%
  pivot_longer(
    cols = c(duracion_media_ola, intensidad_acumulada),
    names_to = "dimension",
    values_to = "valor_dimension"
  ) %>%
  mutate(
    dimension = recode(
      dimension,
      duracion_media_ola = "Duración media de ola (días)",
      intensidad_acumulada = "Intensidad acumulada (°C)"
    )
  )

#grafica para comparar
duracionVSintensidad<-ggplot(datos_grafica, aes(x = valor_dimension, y = muertes)) +
  geom_point(alpha = 0.45, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "orange", linewidth = 0.9) +
  facet_wrap(~ dimension, scales = "free_x") +
  labs(
    title = "Mortalidad infantil según duración e intensidad de las olas de calor",
    subtitle = "Comparativa por Comunidad Autónoma y año",
    x = "Magnitud de la dimensión térmica",
    y = "Muertes en menores de 1 año"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))
duracionVSintensidad
#guardo la gráfica
save(duracionVSintensidad, file ="OUTPUT/GRAPHICS/graficas_pregunta2.RData")
