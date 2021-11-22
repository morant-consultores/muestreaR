library(tidyverse)
library(glue)
library(sf)
library(sp)
devtools::load_all()

# Crear marco muestral ----------------------------------------------------

wd <- "~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INEGI/Censo 2020"

wd_murb <- list.files(glue("{wd}/Población"), full.names = T)
wd_loc <- list.files(paste(list.files(glue("{wd}/Localidad"),full.names = T), "conjunto_de_datos",sep = "/"), full.names = T)
wd_shp <- list.files(paste(list.files(glue("{wd}/AGEB"), full.names = T)[1:32],"conjunto_de_datos", sep = "/"), full.names = T,pattern = "ar.shp")
orden <- substr(wd_loc,nchar(wd_loc)-13,nchar(wd_loc)-12) %>% order


ags <- crear_mm(wd_murb[1],wd_loc[orden][1],wd_shp[1])

# Información muestral ----------------------------------------------------

ja <- empaquetar(ags,
                 c("NOM_MUN","NOM_LOC","AGEB"),
                 c("strata","id","id"),
                 peso_tamaño = POBTOT,
                 metodo_prob = "poblacion")


