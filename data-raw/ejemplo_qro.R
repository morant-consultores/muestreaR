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


wd_shp_mun <- list.files(paste(list.files(glue::glue("{wd}/AGEB"), full.names = T)[1:32],"conjunto_de_datos", sep = "/"), full.names = T,pattern = "[[:digit:]]mun.shp")
wd_shp_loc <- list.files(paste(list.files(glue::glue("{wd}/AGEB"), full.names = T)[1:32],"conjunto_de_datos", sep = "/"), full.names = T,pattern = "[[:digit:]]l.shp")
wd_shp_ageb_r <- list.files(paste(list.files(glue::glue("{wd}/AGEB"), full.names = T)[1:32],"conjunto_de_datos", sep = "/"), full.names = T,pattern = "ar.shp")
wd_shp_ageb <- list.files(paste(list.files(glue::glue("{wd}/AGEB"), full.names = T)[1:32],"conjunto_de_datos", sep = "/"), full.names = T,pattern = "[[:digit:]]a.shp")
wd_shp_lpr <- list.files(paste(list.files(glue::glue("{wd}/AGEB"), full.names = T)[1:32],"conjunto_de_datos", sep = "/"), full.names = T,pattern = "[[:digit:]]lpr.shp")
wd_shp_mza <- list.files(paste(list.files(glue::glue("{wd}/AGEB"), full.names = T)[1:32],"conjunto_de_datos", sep = "/"), full.names = T,pattern = "[[:digit:]]m.shp")
orden <- substr(wd_loc,nchar(wd_loc)-13,nchar(wd_loc)-12) %>% order

mza <- readr::read_csv(wd_murb[[22]], na = "*")
loc <- readr::read_csv(wd_loc[orden][22], na = "*")

mun_shp <- rgdal::readOGR(dsn=wd_shp_mun[22],encoding = "CP1252") %>% sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf()
loc_shp <- rgdal::readOGR(dsn=wd_shp_loc[22],encoding = "CP1252") %>% sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf()
agebR_shp <- sf::st_read(wd_shp_ageb_r[22]) %>% sf::st_transform(4326)
agebU_shp <- rgdal::readOGR(dsn=wd_shp_ageb[22],encoding = "CP1252") %>% sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf()
lpr_shp <- rgdal::readOGR(dsn=wd_shp_lpr[22],encoding = "CP1252") %>% sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf()
mza_shp <- rgdal::readOGR(dsn=wd_shp_mza[22],encoding = "CP1252") %>% sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf()

qro <- crear_mm(mza = mza, loc = loc, loc_shp = loc_shp, lpr_shp = lpr_shp)
qro_shp<- crear_shp(mun_shp, loc_shp, agebR_shp, agebU_shp, lpr_shp, mza_shp)

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
marco %>% analisis_global_nivel()




n1 <- marco %>% agregar_nivel(1, grupo = region, tipo = "strata")
pal <- colorFactor(topo.colors(n_distinct(n1$strata_1)),domain = unique(n1$strata_1))

uno <- qro_shp %>% pluck("Municipios") %>%
  left_join(n1 %>% distinct(MUN,strata_1)) %>%
  group_by(strata_1) %>% summarise(n()) %>% leaflet() %>%
  addPolygons(color = ~pal(strata_1), weight = 1)


n1 %>% analisis_global_nivel()


# Segundo nivel -----------------------------------------------------------


n2 <- n1 %>% agregar_nivel(2, grupo = "NOM_MUN", tipo = "id", n = 5, peso_tamaño = POBTOT, criterio_n = "peso")

# Tercer nivel ------------------------------------------------------------


n3 <- n2 %>% nivel(3, grupo = "NOM_LOC", tipo = "id", n = 50, peso_tamaño = POBTOT, criterio_n = "peso")
