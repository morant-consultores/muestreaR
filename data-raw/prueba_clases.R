library(tidyverse)
devtools::load_all()
wd <- "~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INEGI/Censo 2020"

# Insumos -----------------------------------------------------------------


# Idealmente esto se descarga del inego
wd_murb <- list.files(glue::glue("{wd}/Población"), full.names = T)
wd_loc <- list.files(paste(list.files(glue::glue("{wd}/Localidad"),full.names = T), "conjunto_de_datos",sep = "/"), full.names = T)
wd_shp <- list.files(paste(list.files(glue::glue("{wd}/AGEB"), full.names = T)[1:32],"conjunto_de_datos", sep = "/"), full.names = T,pattern = "ar.shp")
wd_shp_loc <- list.files(paste(list.files(glue::glue("{wd}/AGEB"), full.names = T)[1:32],"conjunto_de_datos", sep = "/"), full.names = T,pattern = "[[:digit:]]l.shp")
wd_shp_lpr <- list.files(paste(list.files(glue::glue("{wd}/AGEB"), full.names = T)[1:32],"conjunto_de_datos", sep = "/"), full.names = T,pattern = "[[:digit:]]lpr.shp")

orden <- substr(wd_loc,nchar(wd_loc)-13,nchar(wd_loc)-12) %>% order

mza <- readr::read_csv(wd_murb[[22]], na = "*")
loc <- readr::read_csv(wd_loc[orden][22], na = "*")
ageb_shp <- sf::st_read(wd_shp[22]) %>% sf::st_transform(4326)
loc_shp <- rgdal::readOGR(dsn=wd_shp_loc[22],encoding = "CP1252") %>% sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf()
lpr_shp <- rgdal::readOGR(dsn=wd_shp_lpr[22],encoding = "CP1252") %>% sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf()


# Declarar clases ---------------------------------------------------------
queretaro <- Poblacion$new(nombre="Querétaro",
                           base_manzana =mza,
                           base_localidad = loc,
                           shp_localidad_no_amanzanada =    lpr_shp,
                           shp_localidad_amanzanada =  loc_shp)

diseño_qro <- Diseño$new(poblacion=queretaro,
                         n=1620,
                         n_0=8,
                         variable_poblacional="POBTOT",
                         unidad_muestreo="Localidades rurales no amanzanadas y manzanas",
                         id_unidad_muestreo="id",
                         llave_muestreo="Man")

# Regiones ----------------------------------------------------------------
region_anterior <- list(
  `Sierra Gorda` = c("Arroyo Seco",
                     "Jalpan de Serra",
                     "Landa de Matamoros",
                     "Pinal de Amoles",
                     "San Joaquín",
                     "Cadereyta de Montes",
                     "Colón",
                     "Peñamiller",
                     "Tolimán"),
  `Los Valles Centrales` = c(
    "Ezequiel Montes",
    "El Marqués",
    "Tequisquiapan",
    "Pedro Escobedo",
    "San Juan del Río",
    "Amealco de Bonfil",
    "Huimilpan"
  ),
  `El Bajío Queretano` = c(
    "Querétaro",
    "Corregidora"
  )
)

# Este paso no debería ir
queretaro$marco_muestral<- regiones(queretaro$marco_muestral,
                                    id = "NOM_MUN",
                                    regiones = region_anterior)

# Se propone un nivel
diseño_qro$agregar_nivel("region",
                         tipo="strata",
                         descripcion= "Sierra Gorda, Los Valles Centrales, El Bajío Queretano",
                         llave="region")
#diseño_qro$eliminar_nivel(nivel=1)

# Se evalúa el nivel

# Municipios --------------------------------------------------------------
# Se propone un nivel
# diseño_qro$agregar_nivel("NOM_MUN",
#                          tipo="cluster",
#                          descripcion= "Municipios",
#                          llave="Mun")
#
# diseño_qro$plan_muestreo(nivel=2,
#                          m_i=10,
#                          num=10,
#                          criterio="unidades",
#                          variable_estudio=POBTOT)
# diseño_qro$n_i
# diseño_qro$niveles


# Localidades -----------------------------------------------------------
# Se propone un nivel
diseño_qro$agregar_nivel("ARLU",
                         tipo="cluster",
                         descripcion= "AGEB Rural y localidad urbana",
                         llave="Loc")

diseño_qro$n_i
diseño_qro$niveles
diseño_qro$plan_muestra(nivel=1,criterio = "peso", unidades_nivel = 10)
diseño_qro$plan_muestra(nivel=2,criterio = "uniforme", unidades_nivel = 5)

asignar_m(diseño = diseño_qro, criterio = "peso", unidades_nivel = 10)
asignar_n(diseño = diseño_qro)

debug(asignar_n)

