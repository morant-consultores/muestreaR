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
wd_shp_loc <- list.files(paste(list.files(glue("{wd}/AGEB"), full.names = T)[1:32],"conjunto_de_datos", sep = "/"), full.names = T,pattern = "[[:digit:]]l.shp")
orden <- substr(wd_loc,nchar(wd_loc)-13,nchar(wd_loc)-12) %>% order


qro <- crear_mm(mza = wd_murb[22],loc = wd_loc[orden][22], ageb_shp = wd_shp[22], loc_shp = wd_shp_loc[22])

qro %>% count(AMBITO)
# Región ------------------------------------------------------------------
#cambiar a qro
prueba <- list(reg1 = c("Aguascalientes","Asientos","Calvillo","Cosío"),
     reg2 = c("Jesús María", "Pabellón de Arteaga","Rincón de Romos", "San José de Gracia"),
     reg3 = c("Tepezalá", "El Llano", "San Francisco de los Romo"))



ags <- regiones(ags, id = "NOM_MUN", regiones = prueba)
# Información muestral ----------------------------------------------------

ja <- empaquetar(ags,
                 c("region","NOM_MUN","NOM_LOC","AGEB"),
                 c("strata","id","id","id"),
                 peso_tamaño = POBTOT,
                 metodo_prob = "poblacion")
ja

