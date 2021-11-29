analisis_global_nivel <- function(marco){
  p_18ymas <- marco %>%
    summarise(resultado=sum(P_18YMAS, na.rm=T)) %>% arrange(desc(resultado))



  gg.p_18ymas <- p_18ymas %>% ggplot(aes(
    x = ifelse(
      length(group_vars(marco)) > 0,
      !!sym(group_vars(marco)),
      "1"
    ),
    y = resultado)) + geom_col()

  # Porcentaje de población total:


  pct_18ymas <- marco %>%
    summarise(porcentaje=sum(P_18YMAS, na.rm=T)/sum(POBTOT, na.rm=T)) %>% arrange(desc(porcentaje))



  # Unidad de muestreo
  unidades <- marco %>% tally() %>% arrange(desc(n))


  # Población rural y urbana - total y mayores de 18

  p_rural_urbana <- marco %>%
    group_by(AMBITO, .add = T) %>%
    summarise(pobtot=sum(POBTOT), na.rm=T) %>%
    mutate(pct = scales::percent(pobtot/sum(pobtot)))

  gg.p_rural_urbana <- p_rural_urbana %>% ggplot(aes(x = ))

  return(
    list(`Población de 18 años y más` = p_18ymas,
         gg.p_18ymas,
         `Proporción de 18 años y más` = pct_18ymas,
         `Unidades de muestreo` = unidades,
         `Población rural y urbana` = p_rural_urbana)
  )

}
