library(tidyverse)
library(glue)
devtools::load_all()

ja <- empaquetar(ags,
                 c("NOM_MUN","NOM_LOC","AGEB"),
                 c("strata","id","id"),
                 peso_tamaño = POBTOT,
                 metodo_prob = "poblacion")


