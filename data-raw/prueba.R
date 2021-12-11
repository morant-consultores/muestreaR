library(dplyr)
library(rgdal)
library(sf)
library(leaflet)
library(purrr)
library(readr)
library(here)
library(readxl)
setwd("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INEGI/Censo 2020/AGEB/22_queretaro/conjunto_de_datos")
mun <- st_read("22mun.shp", options = "ENCODING=CP1252") %>% st_transform(4326)
mun <- mun %>% mutate(REGION = case_when(
  NOMGEO %in% c("Arroyo Seco",
                "Jalpan de Serra",
                "Landa de Matamoros",
                "Pinal de Amoles",
                "San Joaquín",
                "Cadereyta de Montes",
                "Colón",
                "Peñamiller",
                "Tolimán") ~ "Sierra Gorda",
  NOMGEO %in% c(
    "Ezequiel Montes",
    "El Marqués",
    "Tequisquiapan",
    "Pedro Escobedo",
    "San Juan del Río",
    "Amealco de Bonfil"
  ) ~ "Los Valles Centrales",
  NOMGEO %in% c(
    "Querétaro",
    "Corregidora",
    "Huimilpan"
  ) ~ "El Bajío Queretano",
  T~NA_character_
))

manzana <- readOGR(dsn="22m.shp",encoding = "CP1252") %>%
  spTransform(CRS("+init=epsg:4326"))

manzana <- st_as_sf(manzana)

# manzana <- manzana %>%
#   filter(TIPOMZA == "Típica")

manzana <- manzana %>%
  left_join(mun %>% as_tibble() %>% select(CVE_MUN, REGION, MUNICIPIO = NOMGEO))

ageb <- readOGR(dsn="22a.shp",encoding = "CP1252")
ageb <- spTransform(ageb, CRS("+init=epsg:4326"))
ageb <- st_as_sf(ageb)
ageb <- ageb %>% left_join(mun %>% as_tibble() %>% select(CVE_MUN, REGION,MUNICIPIO = NOMGEO))


ageb_r <- readOGR(dsn="22ar.shp",encoding = "CP1252")
ageb_r <- spTransform(ageb_r, CRS("+init=epsg:4326"))
ageb_r <- st_as_sf(ageb_r)

localidad <- readOGR(dsn = "22l.shp", encoding = "CP1252") %>%
  spTransform(CRS("+init=epsg:4326")) %>%
  st_as_sf()

localidad_punt <- readOGR(dsn = "22lpr.shp", encoding = "CP1252") %>%
  spTransform(CRS("+init=epsg:4326")) %>%
  st_as_sf()
# Población ---------------------------------------------------------------

setwd("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INEGI/Censo 2020/AGEB/22_queretaro/población")
poblacion <- read_csv("RESAGEBURB_22CSV20.csv", na = "*")%>%
  mutate(CVEGEO = glue::glue("{ENTIDAD}{MUN}{LOC}{AGEB}{MZA}"))
poblacion <- poblacion %>% mutate(CVE_AGEB_R = glue::glue("{ENTIDAD}{MUN}{AGEB}"),
                                  CVE_AGEB_U = glue::glue("{ENTIDAD}{MUN}{LOC}{AGEB}"))
# poblacion <- poblacion %>% filter(NOM_LOC == "Total AGEB urbana")
manzana <- manzana %>% left_join(poblacion %>% select(CVEGEO,POBTOT,TVIVPARHAB))
m <- manzana %>% as_tibble
m %>% count(is.na(POBTOT), AMBITO)

pob_loc <- read_excel("~/Documents/Git/muestreaR/data-raw/ITER_22XLSX20.xlsx")

localidad <- localidad %>% left_join(pob_loc %>% select(CVE_ENT = ENTIDAD,CVE_MUN = MUN,CVE_LOC =LOC,POBTOT))
localidad %>% as_tibble() %>% count(AMBITO,is.na(POBTOT))
localidad_punt %>% left_join(pob_loc %>% select(CVE_ENT = ENTIDAD,CVE_MUN = MUN,CVE_LOC =LOC,POBTOT)) %>%
  as_tibble %>% count(is.na(POBTOT))

pal <- colorFactor(c("blue","red"), domain = unique(manzana$AMBITO))
pal2 <- colorNumeric("Reds", manzana$POBTOT)
localidad %>% leaflet() %>%
  addPolygons(data = manzana, color = ~pal2(POBTOT), weight = 1, fill = F) %>%
  addPolygons(color = ~pal(AMBITO), weight = 1, label = ~NOMGEO, fill = F) %>%
  addCircleMarkers(data = localidad_punt, color = "green", radius = .0001, label = ~paste("punt ",NOMGEO)) %>%
  addLegend(pal = pal, values = ~AMBITO)
# pob ---------------------------------------------------------------------
manzana %>% as_tibble() %>% select(CVEGEO,POBTOT,TVIVPARHAB) %>% na.omit


# Ver ---------------------------------------------------------------------
library(leaflegend)
pal <- colorFactor(c("blue","red"), domain = unique(manzana$AMBITO))

leaflet(manzana) %>% #addPolygons(data = ageb_r) %>%
  addPolygons(color = ~pal(AMBITO), label =~CVEGEO)

ageb_r %>% leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
  addPolygons(color = "green", group = "Ageb rural", weight = 1) %>%
  addPolygons(data = ageb, color = "orange", group = "Ageb urbano", weight = 1) %>%
  addPolygons(data = localidad, fill = F, group = "Localidades", color = ~pal(AMBITO), weight = T) %>%
  addLayersControl(overlayGroups = c("Ageb urbano","Ageb rural", "Localidades")) %>%
  addLegend(colors = c("green","orange","blue","red"),position = "bottomright",
            labels = c("Ageb rural", "Ageb urbano","Localidades rurales","Localidades urbanas"))

localidad %>% filter(AMBITO == "Rural") %>% leaflet() %>% addPolygons() %>%
  addPolygons(data = manzana %>% filter(AMBITO == "Rural"), weight = 1, color = "red")

manzana %>% as_tibble() %>% mutate(id.1 = ) %>% select(contains("id"))

poblacion %>% filter(NOM_LOC == "Total AGEB urbana") %>% ggplot(aes(x = POBTOT)) + geom_density()

ageb_r %>% left_join(poblacion %>% select(CVE_AGEB_R,POBTOT, TVIVPARHAB), by = c("CVEGEO" = "CVE_AGEB_R")) %>%
  na.omit()

ageb %>% left_join(poblacion %>% select(CVE_AGEB_U,POBTOT, TVIVPARHAB), by = c("CVEGEO" = "CVE_AGEB_U")) %>%
  na.omit() %>% nrow
