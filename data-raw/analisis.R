
total_poblacion <- function(base, variable){
  bd <- base %>%
    summarise(resultado=sum({{variable}}, na.rm=T)) %>%
    pull()
  return(bd)

}

total_poblacion(marco, P_18YMAS)

porcentaje_poblacion <- function(base, variable){

  bd <- base %>%
    summarise(porcentaje=sum(P_18YMAS, na.rm=T)/sum(POBTOT, na.rm=T)) %>%
    pull()
  return(bd)

}





# pruebas  ----------------------------------------------------------------

marco <- marco %>% mutate(region22=if_else(AMBITO=="Rural","Rural", region2),
                          region22=if_else(region22=="Sierra Gorda", "Los Valles Centrales", region22))
base_n <- criterio_N(marco, num = 130, region2, POCUPADA, tipo = "peso", POBTOT)
calcular_varianza_estratificada(marco, region2, POCUPADA, estimador="t", base_n = base_n)/calcular_varianza_mas(marco, 130, POCUPADA)



pacman::p_load(survey)
marco <- marco %>% filter(!is.na(POCUPADA))

v_real <- sum(marco$POCUPADA, na.rm = T)
# 1201532


# Simple Random Sample ----------------------------------------------------
muestra_srs <- marco %>% mutate(fpc=n()) %>%
  slice_sample(n=130)
diseño_srs <- svydesign(data=muestra_srs %>% rownames_to_column("id"),
                        ids = ~id,
                        fpc = ~ fpc)

(estimador_srs <- svytotal(~POCUPADA,diseño_srs, se=T, na.rm = T) %>% as_tibble() %>%  mutate(estimador="srs"))


# Stratified Random Sample - Equal probabilities ------------------------------------------------

muestra_str <- marco %>%
  split(.$region2) %>%
  map2_df(.x=.,.y=base_n$n,
          .f = ~.x %>%  mutate(fpc=n()) %>%
            slice_sample(n = .y))
diseño_str <- svydesign(data=muestra_str %>% rownames_to_column("id"),
                        strata=~region2,
                        ids = ~id,
                        fpc=~fpc)
options(survey.lonely.psu = "remove")
(estimador_str <- svytotal(~POCUPADA, diseño_str, se=T, na.rm = T)%>% as_tibble() %>%  mutate(estimador="str"))

# Stratified Random Sample - Non equal probabilities ------------------------------------------------

muestra_str_pps <- marco %>%
  split(.$region2) %>%
  map2_df(.x=.,.y=base_n$n,
          .f = ~{
            .x %>% # Asumes que está muestreado con método de Tillé
            mutate(fpc=sampling::inclusionprobabilities(n=.y,a = POBTOT),
                   p=POBTOT/sum(POBTOT)) %>%
            slice_sample(n = .y, weight_by = POBTOT)
            })
diseño_str_pps_wr <- svydesign(data=muestra_str_pps %>% rownames_to_column("id"),
                        strata=~region2,
                        ids = ~1,
                        probs=~fpc)
diseño_str_pps_wor <- svydesign(data=muestra_str_pps %>% rownames_to_column("id"),
                               strata=~region2,
                               ids = ~1,
                               fpc=~fpc,
                               pps = "brewer")

(estimador_str_pps <- svytotal(~POCUPADA,diseño_str_pps, se=T, na.rm = T)%>% as_tibble() %>%  mutate(estimador="str_pps"))
(estimador_str_pps_wor <- svytotal(~POCUPADA,diseño_str_pps_wor, se=T, na.rm = T)%>% as_tibble() %>%  mutate(estimador="str_pps_wor"))


bind_rows(estimador_srs, estimador_str, estimador_str_pps, estimador_str_pps_wor) %>%
  mutate(contiene=(v_real>=total-1.96*POCUPADA)&(v_real<=total+1.96*POCUPADA))
v_real
sqrt(calcular_varianza_mas(marco, 130, POCUPADA))
