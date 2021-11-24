library(tidyverse)
library(glue)
library(sf)
library(rgdal)
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

usethis::use_data(qro)
qro %>% count(AMBITO)
# Región ------------------------------------------------------------------
#cambiar a qro
prueba <- list(
  reg1 = formatC(1:6,digits = 2,flag = "0"),
  reg2 = formatC(7:12,digits =2,flag = "0"),
  reg3 = formatC(13:18,digits = 2,flag = "0")
)



marco <- regiones(qro, id = "MUN", regiones = prueba)

# Análisis ----------------------------------------------------------------

# Cuántas personas mayores de 18 años hay en Querétaro?

marco %>%
  summarise(resultado=sum(P_18YMAS, na.rm=T)) %>%
  pull()

# Porcentaje de población total:


marco %>%
  summarise(porcentaje=sum(P_18YMAS, na.rm=T)/sum(POBTOT, na.rm=T)) %>%
  pull()



# Unidad de muestreo
nrow(qro)


# Población rural y urbana - total y mayores de 18

marco %>%
  group_by(AMBITO) %>%
  summarise(pobtot=sum(POBTOT, na.rm=T))

marco %>%
  group_by(AMBITO) %>%
  summarise(pobtot=sum(P_18YMAS, na.rm=T))

# Población total de estratos (pob tot por municipio, pob tot de manzanas, pob tot de amanzanadas)

# Total pobalción por municipio
marco %>%
  group_by(MUN,NOM_MUN, AMBITO) %>%
  summarise(pobtot=sum(POBTOT, na.rm=T)) %>%
  spread(AMBITO, pobtot)

# total población manzanas

marco %>%
  group_by(AMBITO) %>%
  summarise(pobtot=sum(POBTOT, na.rm=T)) %>%
  spread(AMBITO, pobtot)

# concentración de municipios por ambito

# total localidades no amanzanadas

marco %>%
  filter(MZA==" NA") %>%
  group_by(LOC) %>%
  summarise(pobtot=sum(POBTOT, na.rm=T)) %>%
  ungroup()


# resumen de variable del censo

marco %>%
  select(c(VPH_AGUADV,VPH_SNBIEN, POCUPADA, VPH_C_ELEC)) %>%
  summary()




# graficar porcentajes de variable del censo

## density

marco %>%
  mutate(
         porc= VPH_AGUADV/TVIVPARHAB) %>%
  arrange(desc(porc)) %>%
  ggplot() +
  geom_density(aes(x=porc, color=AMBITO)) +
  facet_wrap(~AMBITO)

## boxplot

marco %>%
  mutate(
    porc= VPH_AGUADV/TVIVPARHAB) %>%
  arrange(desc(porc)) %>%
  ggplot() +
  geom_boxplot(aes(y=porc, color=AMBITO))


# calculo nivel municipal
marco %>%
  select(MUN:MZA, VPH_AGUADV, TVIVPARHAB) %>%
  group_by(MUN) %>%
  summarise(VPH_AGUADV=sum(VPH_AGUADV, na.rm=T),
            TVIVPARHAB=sum(TVIVPARHAB, na.rm=T),
            porc=VPH_AGUADV/TVIVPARHAB) %>%
  ungroup() %>%
  arrange(desc(porc))

# Información muestral ----------------------------------------------------

ja <- empaquetar(marco,
                 c("region","NOM_MUN","NOM_LOC","AGEB"),
                 c("strata","id","id","id"),
                 peso_tamaño = POBTOT,
                 metodo_prob = "poblacion")



