library(tidyverse)
devtools::load_all()
wd <- "~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INEGI/Censo 2020"

# Insumos -----------------------------------------------------------------


# Idealmente esto se descarga del inego
wd_murb <- list.files(glue::glue("{wd}/Población"), full.names = T)
wd_loc <- list.files(paste(list.files(glue::glue("{wd}/Localidad"),full.names = T), "conjunto_de_datos",sep = "/"), full.names = T)
wd_shp <- list.files(paste(list.files(glue::glue("{wd}/AGEB"), full.names = T)[1:32],"conjunto_de_datos", sep = "/"), full.names = T,pattern = "ar.shp")
wd_shp_loc <- list.files(paste(list.files(glue::glue("{wd}/AGEB"), full.names = T)[1:32],"conjunto_de_datos", sep = "/"), full.names = T,pattern = "[[:digit:]]l.shp")
orden <- substr(wd_loc,nchar(wd_loc)-13,nchar(wd_loc)-12) %>% order

mza <- readr::read_csv(wd_murb[[22]], na = "*")
loc <- readr::read_csv(wd_loc[orden][22], na = "*")
ageb_shp <- sf::st_read(wd_shp[22]) %>% sf::st_transform(4326)
loc_shp <- rgdal::readOGR(dsn=wd_shp_loc[22],encoding = "CP1252") %>% sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf()


# Declarar clases ---------------------------------------------------------
queretaro <- Poblacion$new(nombre="Querétaro",
                           base_manzana =mza,
                           base_localidad = loc,
                           shp_ageb_rural =  ageb_shp,
                           shp_localidad_amanzanada =  loc_shp)

diseño_qro <- Diseño$new(poblacion=queretaro,
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

# Se evalúa el nivel

# Propone un plan de muestreo
diseño_qro$plan_muestreo(nivel=1,
                         num=8,
                         criterio="unidades",
                         variable_estudio=POBTOT)


# Municipios --------------------------------------------------------------
# Se propone un nivel
diseño_qro$agregar_nivel("NOM_MUN",
                         tipo="cluster",
                         descripcion= "Municipios",
                         llave="Mun")

diseño_qro$plan_muestreo(nivel=2, num=5,criterio="uniforme")


# Localidades -----------------------------------------------------------
# Se propone un nivel
diseño_qro$agregar_nivel("LOC",
                         tipo="cluster",
                         descripcion= "Localidades",
                         llave="Loc")

