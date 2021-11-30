analisis_global_nivel <- function(marco, variable=NULL, variable_estudio=NULL, siguiente_nivel=NULL){
  p_18ymas <- marco %>%
    group_by({{variable}}) %>%
    summarise(resultado=sum(P_18YMAS, na.rm=T)) %>%
    mutate(pct=resultado/sum(resultado, na.rm=T)) %>%
    arrange(desc(resultado))

  g1 <- p_18ymas %>%
    head(20) %>%
    ggplot(aes(x = reorder({{variable}}, resultado),
               y= resultado)) +
    geom_bar(stat="identity", position="dodge", fill="#2ca25f") +
    geom_vline(xintercept = 0)+
    ggfittext::geom_bar_text(outside = T,contrast = T,
                             aes(label=paste(scales::comma(resultado), scales::percent(pct), sep=" - "))) +
    labs(x="")+
    scale_y_continuous(labels = scales::label_comma(accuracy = 1))+
    theme_bw()+
    coord_flip()

  # Porcentaje de población total:


  pct_18ymas <- marco %>%
    group_by({{variable}}) %>%
    summarise(porcentaje=sum(P_18YMAS, na.rm=T)/sum(POBTOT, na.rm=T)) %>%
    arrange(desc(porcentaje))

  g2 <- pct_18ymas %>%
    head(20) %>%
    ggplot(aes(x = reorder({{variable}}, porcentaje),
               y= porcentaje)) +
    geom_bar(stat="identity", position="dodge", fill="#2ca25f") +
    geom_vline(xintercept = 0)+
    ggfittext::geom_bar_text(outside = T,contrast = T,
                             aes(label=scales::percent(porcentaje))) +
    labs(x="")+
    scale_y_continuous(labels = scales::label_comma(accuracy = 1))+
    theme_bw()+
    coord_flip()

  # Unidad de muestreo
  unidades <- if(deparse(enquo(siguiente_nivel)) == "~NULL"){
    marco %>%
      group_by({{variable}}) %>%
      count() %>%
      ungroup() %>%
      mutate(porc=n/sum(n,na.rm = T)) %>%
      arrange(desc(n))
  } else{

    marco %>%
      count({{variable}}, {{siguiente_nivel}}) %>%
      count({{variable}}) %>%
      mutate(porc=n/sum(n, na.rm=T))

    }


  g3 <-unidades %>%
    head(20)%>%
    ggplot(aes(x = reorder({{variable}}, n),
               y= n)) +
    geom_bar(stat="identity", position="dodge", fill="#2ca25f") +
    geom_vline(xintercept = 0)+
    ggfittext::geom_bar_text(outside = T,contrast = T,
                             aes(label=paste(scales::comma(n), scales::percent(porc), sep=" - "))) +
    labs(x="")+
    theme_bw()+
    coord_flip()


  # Población rural y urbana - total y mayores de 18

  p_rural_urbana <- marco %>%
    group_by(AMBITO,{{variable}}, .add = T) %>%
    summarise(pobtot=sum(POBTOT, na.rm=T)) %>%
    group_by({{variable}}) %>%
    mutate(pct = pobtot/sum(pobtot))

  g4 <- p_rural_urbana %>%
    ggplot(aes(x = reorder({{variable}},pct),
               y= pct)) +
    geom_bar(stat="identity", position="dodge", fill="#2ca25f") +
    geom_vline(xintercept = 0)+
    ggfittext::geom_bar_text(outside = T,contrast = T,
                             aes(label=scales::percent(pct,accuracy = 1))) +
    labs(y="", x="")+
    facet_wrap(~AMBITO)+
    theme_bw()+
    coord_flip()



  g5 <- marco %>%
    ggplot() +
    geom_density(aes(x={{variable_estudio}}, color={{variable}})) +
    theme_bw()


  poblacion_unidad <- marco %>%
    group_by({{variable}}, id) %>%
    summarise(pobtot=sum(POBTOT, na.rm=T)) %>%
    arrange(desc(pobtot))

  desigualdad <- poblacion_unidad %>%
    summarise(gini=ineq::Gini(pobtot)) %>%
    arrange(desc(gini))
  g6 <- poblacion_unidad %>%
    slice(1:20) %>%
    ggplot(aes(x = reorder(id,pobtot),
               y= pobtot)) +
    geom_bar(stat="identity", position="dodge", fill="#2ca25f") +
    geom_vline(xintercept = 0)+
    ggfittext::geom_bar_text(outside = T,contrast = T,
                             aes(label=scales::comma(pobtot,accuracy = 1))) +
    labs(y="", x="")+
    facet_wrap(vars({{variable}}), scales = "free_y") +
    theme_bw()+
    coord_flip()

# concentración de población
  return(
    list(`Población de 18 años y más` = p_18ymas,
         `Gráfica - Población de 18 años y más`= g1,
         `Proporción de 18 años y más` = pct_18ymas,
         `Gráfica - Proporción de 18 años y más`=g2,
         `Unidades de muestreo` = unidades,
         `Gráfica - Unidades de muestreo`= g3,
         `Población rural y urbana` = p_rural_urbana,
         `Gráfica - Población rural y urbana` = g4,
         `Gráfica - Distribución variable estudio` = g5,
         `Gráfica - Distribución población` = g6,
         `Desigualdad (Gini)` = desigualdad)
  )

}
