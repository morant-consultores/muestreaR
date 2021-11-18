library(tidyverse)
library(glue)
devtools::load_all()

ags %>% select(NOM_ENT,NOM_MUN,NOM_LOC,AGEB,MZA) %>%
  etiquetar(grupo = c("NOM_MUN","NOM_LOC","AGEB"), tipo = c("strata","id","id"))
