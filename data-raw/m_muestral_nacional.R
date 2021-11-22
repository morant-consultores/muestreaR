library(glue)
library(sf)
library(readr)
library(dplyr)
library(ggplot2)
library(leaflet)
library(purrr)
library(sp)
library(beepr)
wd <- "~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INEGI/Censo 2020"

wd_murb <- list.files(glue("{wd}/Población"), full.names = T)
wd_loc <- list.files(paste(list.files(glue("{wd}/Localidad"),full.names = T), "conjunto_de_datos",sep = "/"), full.names = T)
wd_shp <- list.files(paste(list.files(glue("{wd}/AGEB"), full.names = T)[1:32],"conjunto_de_datos", sep = "/"), full.names = T,pattern = "ar.shp")
orden <- substr(wd_loc,nchar(wd_loc)-13,nchar(wd_loc)-12) %>% order


mm_nacional <- pmap(tibble(a=wd_murb, b=wd_loc[orden], cd=wd_shp), function(a,b,cd) crear_mm(a, b, cd)) %>% bind_rows
beep()
# usethis::use_data(mm_nacional)
