library(tidyverse)
devtools::load_all()
wd <- "~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INEGI/Censo 2020"

# Insumos -----------------------------------------------------------------


# Idealmente esto se descarga del inego


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

shp_qro <- Cartografia$new(mun_shp, loc_shp, agebR_shp, agebU_shp, lpr_shp, mza_shp)

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
queretaro$regiones(id = "NOM_MUN",
                   regiones = region_anterior)

# Se propone un nivel
diseño_qro$agregar_nivel("region",
                         tipo="strata",
                         descripcion= "Sierra Gorda, Los Valles Centrales, El Bajío Queretano",
                         llave="region")

uno <- shp_qro$graficar_mapa(bd = diseño_qro$poblacion$marco_muestral, nivel = "MUN")
# Localidades -----------------------------------------------------------
# Se propone un nivel
diseño_qro$agregar_nivel("MUN",
                         tipo="cluster",
                         descripcion= "Municipios",
                         llave="Mun")

diseño_qro$agregar_nivel("AULR",
                         tipo="cluster",
                         descripcion= "AGEB urbana y localidad rural",
                         llave="AULR")


# Plan de muestra ---------------------------------------------------------
diseño_qro$plan_muestra(nivel=1,criterio = "peso", unidades_nivel = 10)
diseño_qro$plan_muestra(nivel=2,criterio = "uniforme", unidades_nivel = 42)
diseño_qro$plan_muestra(nivel=3)
# debug(asignar_m)
# asignar_m(diseño = diseño_qro)

# Fpc ---------------------------------------------------------------------
# debug(calcular_fpc)
# calcular_fpc(diseño_qro, nivel = 0) %>% count(cluster_3,fpc_3)
diseño_qro$fpc(nivel = 2)
diseño_qro$fpc(nivel = 3)
diseño_qro$fpc(nivel = 0)


# Muestra -----------------------------------------------------------------
diseño_qro$extraer_muestra(nivel = 1)
diseño_qro$extraer_muestra(nivel = 2)
uno %>% shp_qro$graficar_mapa(bd = diseño_qro$muestra, nivel = "MUN")
diseño_qro$extraer_muestra(nivel = 3)
uno %>% shp_qro$graficar_mapa(bd = diseño_qro$muestra, nivel = "AULR")

diseño_qro$n_i$cluster_0 %>% semi_join(diseño_qro$muestra[[3]]) %>% summarise(sum(n_0))
diseño_qro$niveles
# Cuotas ------------------------------------------------------------------
diseño_qro$calcular_cuotas()

# Pruebas de la muestra ---------------------------------------------------
diseño_qro$revisar_muestra(prop_vars = c("POCUPADA","TVIVPARHAB"), var_extra = c("TVIVPARHAB"))


# Google maps -------------------------------------------------------------
library(ggmap)
# ggmap(get_map())
diseño_qro$exportar(shp_qro)
# diseño_qro$sustituir_muestra(shp_qro, id = 4)
