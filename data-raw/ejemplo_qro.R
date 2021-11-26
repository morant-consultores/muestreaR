# library(tidyverse)
# library(glue)
# library(sf)
# library(rgdal)
# library(sp)
# library(rgdal)
devtools::load_all()

# Crear marco muestral ----------------------------------------------------

wd <- "~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INEGI/Censo 2020"

wd_murb <- list.files(glue::glue("{wd}/Población"), full.names = T)
wd_loc <- list.files(paste(list.files(glue::glue("{wd}/Localidad"),full.names = T), "conjunto_de_datos",sep = "/"), full.names = T)
wd_shp <- list.files(paste(list.files(glue::glue("{wd}/AGEB"), full.names = T)[1:32],"conjunto_de_datos", sep = "/"), full.names = T,pattern = "ar.shp")
wd_shp_loc <- list.files(paste(list.files(glue::glue("{wd}/AGEB"), full.names = T)[1:32],"conjunto_de_datos", sep = "/"), full.names = T,pattern = "[[:digit:]]l.shp")
orden <- substr(wd_loc,nchar(wd_loc)-13,nchar(wd_loc)-12) %>% order

mza <- readr::read_csv(wd_murb[[22]], na = "*")
loc <- readr::read_csv(wd_loc[orden][22], na = "*")
ageb_shp <- sf::st_read(wd_shp[22]) %>% sf::st_transform(4326)
loc_shp <- rgdal::readOGR(dsn=wd_shp_loc[22],encoding = "CP1252") %>% sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf()

qro <- crear_mm(mza = mza, loc = loc, ageb_shp = ageb_shp, loc_shp = loc_shp)


# Diseño de muestra -------------------------------------------------------


# Primer nivel ------------------------------------------------------------


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
marco <- regiones(qro, id = "NOM_MUN", regiones = region_anterior)
n1 <- marco %>% nivel(1, grupo = "region", tipo = "strata")


# Segundo nivel -----------------------------------------------------------


n2 <- n1 %>% nivel(2, grupo = "NOM_MUN", tipo = "id", n = 5, peso_tamaño = POBTOT, criterio_n = "peso")
n2 %>%
# Tercer nivel ------------------------------------------------------------


n3 <- n2 %>% nivel(3, grupo = "NOM_LOC", tipo = "id", n = 50, peso_tamaño = POBTOT, criterio_n = "peso")