library(tidyverse)
library(glue)
library(sf)
library(sp)
library(rgdal)
devtools::load_all()

# Crear marco muestral ----------------------------------------------------

wd <- "~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INEGI/Censo 2020"

wd_murb <- list.files(glue("{wd}/Población"), full.names = T)
wd_loc <- list.files(paste(list.files(glue("{wd}/Localidad"),full.names = T), "conjunto_de_datos",sep = "/"), full.names = T)
wd_shp <- list.files(paste(list.files(glue("{wd}/AGEB"), full.names = T)[1:32],"conjunto_de_datos", sep = "/"), full.names = T,pattern = "ar.shp")
wd_shp_loc <- list.files(paste(list.files(glue("{wd}/AGEB"), full.names = T)[1:32],"conjunto_de_datos", sep = "/"), full.names = T,pattern = "[[:digit:]]l.shp")
orden <- substr(wd_loc,nchar(wd_loc)-13,nchar(wd_loc)-12) %>% order


qro <- crear_mm(mza = wd_murb[22],loc = wd_loc[orden][22], ageb_shp = wd_shp[22], loc_shp = wd_shp_loc[22])

# usethis::use_data(qro)
qro %>% count(AMBITO)
# Región ------------------------------------------------------------------
#cambiar a qro
prueba <- list(
  reg1 = formatC(1:6,digits = 2,flag = "0"),
  reg2 = formatC(7:12,digits =2,flag = "0"),
  reg3 = formatC(13:18,digits = 2,flag = "0")
)

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
    "Amealco de Bonfil"
  ),
  `El Bajío Queretano` = c(
    "Querétaro",
    "Corregidora",
    "Huimilpan"
  )
)


marco <- regiones(qro, id = "NOM_MUN", regiones = region_anterior)

# Con sampling ------------------------------------------------------------

muestra <- mstage(marco %>% distinct(MUN,LOC, .keep_all = T),
       stage = c("stratified", "cluster"), varnames = c("region", "NOM_MUN"),
       size = list(marco %>% distinct(LOC, .keep_all = T) %>% count(region) %>% pull(n),
                   c(3,3,3)), method = list("","srswor","srswor"))

marco %>% distinct(MUN,LOC, .keep_all = T) %>% count(region,NOM_MUN) %>% count(region)
getdata(marco %>% distinct(LOC, .keep_all = T), muestra) %>% pluck(2) %>% as_tibble %>%
  count(region, MUN)
# Análisis ----------------------------------------------------------------

# Cuántas personas mayores de 18 años hay en Querétaro?

qro %>%
  summarise(resultado=sum(P_18YMAS, na.rm=T)) %>%
  pull()

# Porcentaje de población total:


qro %>%
  summarise(porcentaje=sum(P_18YMAS, na.rm=T)/sum(POBTOT, na.rm=T)) %>%
  pull()



# Unidad de muestreo
nrow(qro)


# Información muestral ----------------------------------------------------

ja <- empaquetar(marco,
                 c("region","NOM_MUN","AMBITO","ARLU","AULR"),
                 c("strata","id","strata","id","id"),
                 peso_tamaño = POBTOT,
                 metodo_prob = "poblacion")

ja %>% distinct(region,fpc_2,fpc_3,fpc_4) %>%
  summarise(across(fpc_2:fpc_4, list(minimo = min, maximo = max),
                   .names = "{.col}.{.fn}")) %>%
  pivot_longer(everything()) %>% separate(name,into=c("col","stat"),sep = "\\.") %>%
  pivot_wider(names_from = stat, values_from = value)


niveles <- c("region","NOM_MUN","NOM_LOC")
t_niveles <- c("strata","id","id")

agrupar <- c()
n_sortear <- c(3,3,6)

aux <- ja
for(i in seq_along(t_niveles)){
  agrupar <- c(agrupar, niveles[i])

  if(t_niveles[i] != "strata"){
    aux <- aux %>% group_by(across(all_of(agrupar))) %>% nest %>% ungroup(niveles[i]) %>%
      sample_n(n_sortear[i]) %>% unnest(data) %>% ungroup
  }
}



