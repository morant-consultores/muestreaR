
library(tidyverse)


wd <- "~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INE"
# Insumos -----------------------------------------------------------------

#LISTA NOMINAL INE

ln_re_sexo <- readxl::read_excel(glue::glue("{wd}/Lista Nominal Rangos de edad y Sexo/ln_re_sexo_20220218.xlsx"))
ln_re_sexo <- ln_re_sexo %>% set_names(gsub(pattern = "\r\n",replacement = " ",x = names(ln_re_sexo)))
ln_edomex <- ln_re_sexo %>% filter(`CLAVE ENTIDAD` == 15, SECCION != 0)

# SHAPE FILES INE


mun_shp <- rgdal::readOGR(dsn= glue::glue("{wd}/SHP/2021/15-MEXICO/MUNICIPIO.shp"),encoding = "CP1252") %>%
  sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf()

murb_shp <- rgdal::readOGR(dsn= glue::glue("{wd}/SHP/2021/15-MEXICO/MANCHA_URBANA.shp"),encoding = "CP1252") %>%
  sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf()

loc_shp <- rgdal::readOGR(dsn= glue::glue("{wd}/SHP/2021/15-MEXICO/LOCALIDAD.shp"),encoding = "CP1252") %>%
  sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf()

sec_shp <- rgdal::readOGR(dsn= glue::glue("{wd}/SHP/2021/15-MEXICO/SECCION.shp"),encoding = "CP1252") %>%
  sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf()

mza_shp <- rgdal::readOGR(dsn= glue::glue("{wd}/SHP/2021/15-MEXICO/MANZANA.shp"),encoding = "CP1252") %>%
  sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf()

dl_shp <- rgdal::readOGR(dsn=glue::glue("{wd}/SHP/2021/15-MEXICO/DISTRITO_LOCAL.shp"),encoding = "CP1252") %>%
  sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf()

df_shp <- rgdal::readOGR(dsn=glue::glue("{wd}/SHP/2021/15-MEXICO/DISTRITO_FEDERAL.shp"),encoding = "CP1252") %>%
  sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf()

# Población ---------------------------------------------------------------
devtools::load_all(path = "~/Documents/Git/muestreaR")
edomex <- PoblacionINE$new(nombre = "Estado de México",
                           ln = ln_edomex,
                           shp_mza = mza_shp,
                           shp_loc = loc_shp,
                           shp_mun = mun_shp)

diseño_edomex <- DiseñoINE$new(poblacion=edomex,
                               n=1620,
                               n_0=8,
                               variable_poblacional="lista_nominal",
                               unidad_muestreo="Manzanas",
                               id_unidad_muestreo="id",
                               llave_muestreo="Man")

shp_edomex <- CartografiaINE$new(df_shp, dl_shp, mun_shp, loc_shp, sec_shp, mza_shp)


# Regiones ----------------------------------------------------------------

region <- mun_shp %>% as_tibble %>% mutate(region = sample(glue::glue("Region {1:5}"),size = nrow(.), replace = T)) %>%
  split(.$region) %>% map(~.x %>% pull(NOMBRE))

edomex$regiones(id = "NOMBRE_MUN",
                regiones = region)

diseño_edomex$agregar_nivel("region",
                            tipo="strata",
                            descripcion= "5 regiones aleatorias",
                            llave="region")
(uno <- shp_edomex$graficar_mapa(bd = diseño_edomex$poblacion$marco_muestral, nivel = "MUNICIPIO"))

# Municipios --------------------------------------------------------------

diseño_edomex$agregar_nivel("MUNICIPIO",
                            tipo="cluster",
                            descripcion= "Municipios",
                            llave="Mun")


# Secciones ---------------------------------------------------------------

diseño_edomex$agregar_nivel("SECCION",
                            tipo="cluster",
                            descripcion= "Secciones electorales",
                            llave="SECCION")

# Plan de muestra ---------------------------------------------------------
diseño_edomex$plan_muestra(nivel=1,criterio = "peso", unidades_nivel = 10)
diseño_edomex$plan_muestra(nivel=2,criterio = "uniforme", unidades_nivel = 42)
diseño_edomex$plan_muestra(nivel=3)


# FPC ---------------------------------------------------------------------

diseño_edomex$fpc(nivel = 2)
diseño_edomex$fpc(nivel = 3)
diseño_edomex$fpc(nivel = 0)

# Muestra -----------------------------------------------------------------
diseño_edomex$extraer_muestra(nivel = 1)
diseño_edomex$extraer_muestra(nivel = 2)
uno %>% shp_edomex$graficar_mapa(bd = diseño_edomex$muestra, nivel = "MUNICIPIO")
diseño_edomex$extraer_muestra(nivel = 3)
# diseño_edomex$poblacion$marco_muestral %>%
#   semi_join(diseño_edomex$muestra$SECCION) %>%
#   anti_join(mza_shp %>% as_tibble %>% select(-ENTIDAD,-DISTRITO_F,-DISTRITO_L,-LOCALIDAD) %>%
#               mutate(MUNICIPIO = as.character(MUNICIPIO),SECCION = as.character(SECCION),
#                      MANZANA = as.character(MANZANA)))
uno %>% shp_edomex$graficar_mapa(bd = diseño_edomex$muestra, nivel = "SECCION")

# Cuotas ------------------------------------------------------------------

diseño_edomex$calcular_cuotas()
diseño_edomex$cuotas

# Pruebas de la muestra ---------------------------------------------------
diseño_edomex$revisar_muestra(prop_vars = c("lista_nominal"),var_extra = NULL)

# Google maps -------------------------------------------------------------
diseño_edomex$exportar(shp_edomex, carpeta = "Insumos", zoom = 16)

# descomentar la siguiente línea si existen mapas que haya que cambiar el zoom
# shp_edomex$crear_mapas(diseño = diseño_edomex, zoom = 15, dir = glue::glue("Insumos/Mapas"))

# Sustitiuir --------------------------------------------------------------
#esta sección es recomendable que vaya en otro script
library(ggmap)

devtools::load_all(path = "~/Documents/Git/muestreaR")
diseño_edomex <- readr::read_rds("Insumos/diseño.rda")
shp_edomex <- readr::read_rds("Insumos/shp.rda")
diseño_edomex$sustituir_muestra(shp_edomex, id = 6152)


