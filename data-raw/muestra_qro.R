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
                         n_0=12,
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


# Localidades -----------------------------------------------------------
# Se propone un nivel
diseño_qro$agregar_nivel("MUN",
                         tipo="cluster",
                         descripcion= "AGEB Rural y localidad urbana",
                         llave="Loc")

diseño_qro$agregar_nivel("AULR",
                         tipo="cluster",
                         descripcion= "AGEB Rural y localidad urbana",
                         llave="Loc")


# Plan de muestra ---------------------------------------------------------
diseño_qro$plan_muestra(nivel=1,criterio = "peso", unidades_nivel = 10)
diseño_qro$plan_muestra(nivel=2,criterio = "peso", unidades_nivel = 20)
diseño_qro$plan_muestra(nivel=3)


# Fpc ---------------------------------------------------------------------
# debug(calcular_fpc)
# calcular_fpc(diseño_qro, nivel = 0) %>% count(cluster_3,fpc_3)
diseño_qro$fpc(nivel = 2)
diseño_qro$fpc(nivel = 3)
diseño_qro$fpc(nivel = 0)


# Muestra -----------------------------------------------------------------
diseño_qro$extraer_muestra(nivel = 1)
diseño_qro$extraer_muestra(nivel = 2)
diseño_qro$extraer_muestra(nivel = 3)


# Cuotas ------------------------------------------------------------------
diseño_qro$calcular_cuotas()
diseño_qro$cuotas %>% summarise(sum(n))
nrow((diseño_qro$muestra %>% pluck(3))) * diseño_qro$n_0

# Pruebas de la muestra ---------------------------------------------------

bd <- diseño_qro$muestra %>% pluck(3) %>% unnest(data)
bd <- bd %>% mutate(prop = POCUPADA/POBTOT)
bd <- bd %>% sample_n(203) %>% mutate(fpc_0 = fpc_0*203/nrow(.))
bd %>% distinct(cluster_3,fpc_0)
library(survey)
diseño <- svydesign(data = bd, ids = ~cluster_2 + cluster_3 + cluster_0, strata = ~strata_1, fpc = ~fpc_2 + fpc_3+fpc_0, pps = "brewer")
options(survey.lonely.psu="remove")
confint(svytotal(~POBTOT, design = diseño, na.rm = T))
confint(svytotal(~POCUPADA, design = diseño, na.rm = T))
diseño_qro$poblacion$marco_muestral %>% summarise(sum(POCUPADA,na.rm = T))
confint(svymean(~prop, design = diseño, na.rm = T))

diseño_qro$n_i$cluster_3 %>% semi_join(
  diseño_qro$muestra %>% pluck(3)
) %>% ungroup %>% summarise(sum(m_3))

# Mapas de la muestra -----------------------------------------------------

graficar_mapa_muestra(muestra = diseño_qro$poblacion$marco_muestral,
                      shp = qro_shp,
                      nivel = "MUN") %>%
  graficar_mapa_muestra(muestra = diseño_qro$muestra %>% pluck(3)  %>% unnest(data),
                        shp = qro_shp, nivel = "AULR")

# bd %>% count(MUN,AGEB, sort = T) %>%

# Google maps -------------------------------------------------------------

google_maps(diseño_qro, shp = qro_shp, zoom = 15)
cuotas(diseño_qro)


