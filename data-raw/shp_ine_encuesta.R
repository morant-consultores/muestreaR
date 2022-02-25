

library(ggmap)
library(tidyverse)
devtools::load_all(path = "~/Documents/Git/muestreaR")

wd <- "C://Users/Pablo/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INE"
# Insumos -----------------------------------------------------------------

#LISTA NOMINAL INE

ln_re_sexo <- readxl::read_excel(glue::glue("{wd}/Lista Nominal Rangos de edad y Sexo/ln_re_sexo_20220218.xlsx"))


# SHAPE FILES INE


mun_shp <- rgdal::readOGR(dsn= glue::glue("{wd}/SHP/2021/15-MEXICO/MUNICIPIO.shp"),encoding = "CP1252") %>% sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf()
loc_shp <- rgdal::readOGR(dsn= glue::glue("{wd}/SHP/2021/15-MEXICO/LOCALIDAD.shp"),encoding = "CP1252") %>% sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf()
sec_shp <- rgdal::readOGR(dsn= glue::glue("{wd}/SHP/2021/15-MEXICO/SECCION.shp"),encoding = "CP1252") %>% sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf()
mza_shp <- rgdal::readOGR(dsn= glue::glue("{wd}/SHP/2021/15-MEXICO/MANZANA.shp"),encoding = "CP1252") %>% sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf()
