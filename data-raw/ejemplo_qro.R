# library(tidyverse)
# library(glue)
# library(sf)
# library(rgdal)
# library(sp)
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
qro_shp <- crear_shp(mun_shp, loc_shp, agebR_shp, agebU_shp, lpr_shp, mza_shp)

# Primer nivel - region ------------------------------------------------------------


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

marco %>%  analisis_global_nivel(region, POCUPADA)

graficar_mapa_poblacion(qro, qro_shp, nivel = "MUN", variable = "POBTOT")

n1 <- marco %>% agregar_nivel(1, grupo = region, tipo = "strata")

uno <- graficar_mapa_muestra(muestra = n1, shp = qro_shp, nivel = "MUN")


# Segundo nivel - MUN -----------------------------------------------------------


n2 <- n1 %>% agregar_nivel(i = 2, grupo = NOM_MUN, tipo = "cluster")

bd_n <- criterio_N(n2 , nivel = 1, variable_estudio = "POBTOT", num = 10, criterio = "peso", ultimo_nivel=F)
n2.fpc <- n2 %>% calcular_fpc(nivel = 1, n_grupo = bd_n)
muestra2 <- muestrear(n2.fpc,1,POBTOT,bd_n)


uno %>% graficar_mapa_muestra(muestra = muestra2,
                              shp = qro_shp, nivel = "MUN")

# Tercer nivel - ARLU ------------------------------------------------------------


n3 <- muestra2 %>% agregar_nivel(i = 3, grupo = ARLU, tipo = "cluster")

bd_n <- criterio_N(n3 , nivel = 2, variable_estudio = "POBTOT", num = 10,
                   criterio = "peso", ultimo_nivel=F)
n3.fpc <- n3 %>% calcular_fpc(nivel = 2, n_grupo = bd_n)
muestra3 <- muestrear(n3.fpc,2,POBTOT,bd_n)

uno %>%
  graficar_mapa_muestra(muestra = muestra3,
                        shp = qro_shp, nivel = "ARLU")

# Cuarto nivel - AULR ----------------------------------------------------

n4 <- muestra3 %>% agregar_nivel(i = 4, grupo = AULR, tipo = "cluster")

bd_n <- criterio_N(n4 , nivel = 3, variable_estudio = "POBTOT", num = 40,
                   criterio = "peso", ultimo_nivel=F)
n4.fpc <- n4 %>% calcular_fpc(nivel = 3, n_grupo = bd_n)
muestra4 <- muestrear(n4.fpc,3,POBTOT,bd_n)

muestra4 %>% distinct(AULR) %>% tidyr::separate(AULR,c("CVEGEO","nivel","tipo")) %>% count(nivel,tipo)


uno %>%
  graficar_mapa_muestra(muestra = muestra4,
                        shp = qro_shp, nivel = "AULR")
