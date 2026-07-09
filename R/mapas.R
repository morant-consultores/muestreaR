#' Mapa de la población por nivel
#'
#' Dibuja un mapa interactivo (`leaflet`) coloreando las unidades del nivel
#' indicado según una variable poblacional.
#'
#' @param bd Marco muestral o `tibble` con la geometría a graficar.
#' @param shp Lista de cartografías del diseño.
#' @param nivel Nivel geográfico a graficar (p. ej. `"MUNICIPIO"`, `"SECCION"`).
#' @param variable Variable poblacional usada para colorear el mapa.
#'
#' @return Un objeto `leaflet`.
#' @export
#' @import leaflet
graficar_mapa_poblacion <- function(bd, shp, nivel, variable){
  aux <- shp %>% purrr::pluck(nivel) %>%
    inner_join(
      bd %>% count(across(all_of(nivel)), wt = across(all_of(variable)))
    )

  pal <- leaflet::colorNumeric(palette = "Reds", domain = aux$n)

  aux <- aux %>% mutate(color = pal(n))

  leaflet::leaflet() %>%
    leaflet::addProviderTiles("CartoDB.DarkMatter") %>%
    leaflet::addPolygons(data = aux %>% filter(sf::st_geometry_type(.) != "POINT"),
                         fillColor = ~color, fillOpacity = .7, weight = 1,stroke = T, color = "black") %>%
    leaflet::addCircleMarkers(data = aux %>% filter(sf::st_geometry_type(.) == "POINT"),
                              radius = 1, color = ~color, fillOpacity = 1,stroke = T,opacity = 1, , weight = 1) %>%
    leaflet::addLegend(data = aux, pal = pal, values = ~n, title = variable)


}

#' Mapa de la muestra por nivel (marco censal INEGI)
#'
#' Dibuja un mapa interactivo (`leaflet`) resaltando las unidades seleccionadas
#' en la muestra para el nivel indicado.
#'
#' @param lflt Objeto `leaflet` base sobre el que dibujar (opcional).
#' @param muestra Muestra extraída del diseño (lista por nivel o `data.frame`).
#' @param shp Lista de cartografías del diseño.
#' @param nivel Nivel geográfico a graficar.
#'
#' @return Un objeto `leaflet`.
#' @export
graficar_mapa_muestra <- function(lflt = NULL, muestra, shp, nivel){
  pal <- if(nivel == "MUN"){
    colorFactor(topo.colors(n_distinct(muestra$strata_1)), domain = unique(muestra$strata_1))
  } else{
    colorFactor(c("orange","red"),c("LOC","AGEB"))
  }

  mapa <- if(is.null(lflt)){
    shp %>% purrr::pluck(nivel) %>%
      left_join(muestra %>% distinct(MUN,strata_1)) %>%
      group_by(strata_1) %>% summarise(n()) %>%
      sf::st_buffer(dist = 0) %>%
      leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
      addPolygons(color = ~pal(strata_1), opacity = 1, fill = F) %>%
      addLegend(pal = pal, values = ~strata_1, position = "bottomleft")
  } else{
    if(nivel == "MUN"){
      lflt %>% addPolygons(data = shp %>% purrr::pluck(nivel) %>% inner_join(muestra %>% distinct(across(all_of(nivel)), .keep_all = T)),
                           color = ~pal(strata_1), fillOpacity = 1, label = ~glue::glue("Municipio: {NOM_MUN}"))
    } else{
      mapear <- shp %>% purrr::pluck(nivel) %>% inner_join(muestra %>% distinct(across(all_of(nivel)), .keep_all = T)) %>%
        tidyr::separate(!!sym(nivel),c("CVEGEO","nivel","tipo"))

      if(!"Nombre" %in% names(mapear)){
        mapear <- mapear %>% mutate(Nombre = "")
      }
      lflt %>% addPolygons(data = mapear %>%
                             filter(sf::st_geometry_type(.) != "POINT"), stroke = T, color = "black",
                           fillColor = ~pal(nivel), fillOpacity = 1,weight = 1, opacity = 1,
                           popup = ~glue::glue("Tipo: {nivel} <br>
                                                Ámbito: {tipo} <br>
                                                CVEGEO: {CVEGEO}
                                               "))  %>%
        addCircleMarkers(data = mapear %>% filter(sf::st_geometry_type(.) == "POINT"),
                         radius = 3, color = "red", weight = 1,
                         clusterOptions = markerClusterOptions(),
                         popup = ~glue::glue("Nombre: {Nombre} <br>
                                              Tipo: {nivel} <br>
                                              Ámbito: {tipo} <br>
                                              CVEGEO: {CVEGEO}
                                               ")
        ) %>% addLegend(data = mapear, pal = pal, values = ~nivel)
    }

  }

  return(mapa)
}

#' Mapa de la muestra por nivel (marco electoral INE)
#'
#' Versión para el marco del INE: dibuja un mapa interactivo (`leaflet`)
#' resaltando las unidades seleccionadas en la muestra para el nivel indicado.
#'
#' @param lflt Objeto `leaflet` base sobre el que dibujar (opcional).
#' @param muestra Muestra extraída del diseño (lista por nivel o `data.frame`).
#' @param shp Lista de cartografías electorales del diseño.
#' @param nivel Nivel geográfico a graficar.
#'
#' @return Un objeto `leaflet`.
#' @export
graficar_mapa_muestra_ine <- function(lflt = NULL, muestra, shp, nivel){
  pal <- if(nivel == "MUNICIPIO"){
    colorFactor(topo.colors(n_distinct(muestra$strata_1)), domain = unique(muestra$strata_1))
  } else{
    colorFactor(c("orange","red"),c("LOCALIDAD","SECCION"))
  }

  mapa <- if(is.null(lflt)){
    shp %>% purrr::pluck(nivel) %>%
      left_join(muestra %>% distinct(MUNICIPIO,strata_1)) %>%
      group_by(strata_1) %>% summarise(n()) %>%
      sf::st_buffer(dist = 0) %>%
      leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
      addPolygons(color = ~pal(strata_1), opacity = 1, fill = F) %>%
      addLegend(pal = pal, values = ~strata_1, position = "bottomleft")
  } else{
    if(nivel == "MUNICIPIO"){
      lflt %>% addPolygons(data = shp %>% purrr::pluck(nivel) %>% inner_join(muestra %>% distinct(across(all_of(nivel)), .keep_all = T)),
                           fillColor = ~pal(strata_1), color = "black", opacity = 1, weight = 1, fillOpacity = 1, label = ~glue::glue("Municipio: {NOMBRE_MUN}"))
    } else{
      if(nivel == "MANZANA"){
        mapear <- shp %>% purrr::pluck(nivel) %>% inner_join(muestra %>% distinct(across(all_of(nivel)), .keep_all = T))

        lflt %>%
          addCircleMarkers(data = mapear %>% filter(sf::st_geometry_type(.) == "POINT"),
                              label = ~glue::glue("Localidad: {MANZANA}"), opacity = 1, fillOpacity = 1,
                              fillColor = "#f72585", color = "black", weight = 1) %>%
          addLegend(position = "bottomright", colors = "#f72585", labels = "Localidades rurales")

      } else{
        mapear <- shp %>% purrr::pluck(nivel) %>% inner_join(muestra %>% distinct(across(all_of(nivel)), .keep_all = T))

        nivel <- mapear %>% as_tibble %>% select(contains("cluster")) %>% names |> readr::parse_number() %>% max

        popup_cluster <- paste0("cluster_",nivel,": ", as_tibble(mapear)[[paste("cluster",nivel,sep = "_")]])
        popup_mun <- paste("Municipio: ", mapear$NOMBRE_MUN)
        lflt %>% addPolygons(data = mapear,
                             stroke = T, color = "black",
                             fillColor = ~pal(nivel), fillOpacity = .2,weight = 1, opacity = 1,
                             # popup = ~glue::glue("Sección: {SECCION}")
                             popup = paste(popup_mun, popup_cluster, sep = "<br>")
        ) %>%
          addLegend(data = mapear, pal = pal, values = ~nivel, position = "bottomright")
      }
    }
  }

  return(mapa)
}

# Anotación operativa de los mapas de campo -------------------------------
#
# El equipo ya no levanta por cuotas (la composición la corrige el rake), así
# que el subtítulo de los mapas dejó de mostrar el desglose por rango/sexo y
# ahora indica lo que el encuestador necesita saber del conglomerado: el zoom
# del mapa, cuántas manzanas visitar, cuántos contactos (viviendas a
# levantar) y cuántas entrevistas efectivas se planean. Es común a los dos
# flujos (google_maps y google_maps_ine).

# Tasa de rechazo declarada en la asignación del diseño (si existe): sirve
# para pasar de contactos (a levantar) a entrevistas efectivas planeadas. Sin
# asignación (diseño hecho a mano) se asume 0: no hay sobremuestra declarada.
resolver_tasa_rechazo <- function(diseño){
  asig <- attr(diseño, "asignacion")
  if(is.null(asig) || is.null(asig[["tasa_rechazo"]])) return(0)
  mean(asig[["tasa_rechazo"]], na.rm = TRUE)
}

# Resumen operativo por conglomerado (último nivel): manzanas de la muestra,
# contactos (suma de n_0 = viviendas a levantar), entrevistas efectivas
# planeadas (contactos ajustados por la tasa de rechazo) y la numeración
# estable del mapa (`mapa` de `total_mapas`, para el contador n/N en la
# esquina del PNG). Una fila por conglomerado, ordenada por el cluster de
# último nivel para que el número no dependa del orden de dibujo.
resumen_operativo <- function(diseño){
  u_cluster <- diseño$niveles %>%
    filter(nivel == diseño$ultimo_nivel) %>%
    transmute(paste(tipo, nivel, sep = "_")) %>%
    pull(1)
  muestra <- diseño$muestra %>% purrr::pluck(length(diseño$muestra))
  tasa <- resolver_tasa_rechazo(diseño)

  muestra %>%
    left_join(diseño$n_i$cluster_0, by = "cluster_0") %>%
    group_by(!!rlang::sym(u_cluster)) %>%
    summarise(manzanas = n(), contactos = sum(n_0), .groups = "drop") %>%
    arrange(!!rlang::sym(u_cluster)) %>%
    mutate(entrevistas = round(contactos * (1 - tasa)),
           total_mapas = n(),
           mapa = row_number())
}

# Subtítulo del mapa a partir de una fila del resumen operativo y el zoom.
etiqueta_mapa <- function(resumen_i, zoom){
  paste(
    glue::glue("Zoom: {zoom}"),
    glue::glue("Manzanas: {resumen_i$manzanas}"),
    glue::glue("Contactos planeados: {resumen_i$contactos}"),
    glue::glue("Entrevistas planeadas: {resumen_i$entrevistas}"),
    sep = "\n"
  )
}

#' Exportar mapas de campo con Google Maps (marco censal INEGI)
#'
#' Genera y guarda en disco un mapa por unidad mínima de la muestra usando
#' imágenes de Google Maps, para el trabajo de campo. El subtítulo de cada
#' mapa indica lo operativo del conglomerado (no cuotas): el zoom, las
#' manzanas a visitar, los contactos (viviendas a levantar) y las entrevistas
#' efectivas planeadas.
#'
#' @param diseño Objeto de la clase [Diseño] con la muestra extraída.
#' @param shp Lista de cartografías del diseño.
#' @param zoom Nivel de zoom de Google Maps.
#' @param dir Carpeta de destino de los mapas (por defecto `"Mapas"`).
#'
#' @return Invisible. Se ejecuta por su efecto secundario (escribe los mapas).
#' @export
google_maps <- function(diseño, shp, zoom, dir = "Mapas"){

  u_nivel <- diseño$niveles %>% filter(nivel == diseño$ultimo_nivel)
  u_cluster <- u_nivel %>% transmute(paste(tipo,nivel,sep = "_")) %>% pull(1)
  bd <- diseño$muestra %>% purrr::pluck(length(diseño$muestra)) %>% tidyr::unnest(data)

  
  cluster <- bd %>% distinct(!!rlang::sym(u_cluster)) %>% pull(1)
  ya <- list.files(path=dir) %>% gsub('^.*_\\s*|\\s*.png.*$', '', .)
  cluster <- cluster[!cluster %in% ya]
  
  
  # agebs <- agebs %>% mutate(CVE_AGEB = paste0(22,CVE_MUN,CVE_LOC,CVE_AGEB))
  shp_mapa <- shp %>% purrr::pluck(u_nivel %>% pull(variable)) %>% inner_join(bd)
  man_shp <- shp %>% purrr::pluck("MZA") %>% inner_join(bd)

  # subtítulo operativo por conglomerado (ya no cuotas): zoom, manzanas,
  # contactos y entrevistas efectivas planeadas
  resumen <- resumen_operativo(diseño)

  for(i in cluster){
    resumen_i <- resumen %>% filter(!!rlang::sym(u_cluster) == i)
    man <- man_shp %>% filter(!!rlang::sym(u_cluster) == i)
    aux_mapeo <- shp_mapa %>% filter(!!rlang::sym(u_cluster) == i)
    caja <- aux_mapeo %>% sf::st_union() %>% sf::st_centroid() %>% sf::st_coordinates() %>% as.numeric()
    nc_map <- ggmap::get_map(location = caja, maptype = "roadmap",
                             source = "google",force = T, zoom = zoom)
    Google <- ggmap::ggmap(nc_map)
    # Google
    g <- Google +
      # contorno del AGEB (azul, grueso) y manzanas a levantar (rojo, con
      # relleno tenue para que resalten sobre el mapa)
      geom_sf(data = aux_mapeo,
              inherit.aes = F, fill = NA, color = "blue", linewidth = 1.4) +
      geom_sf(data = man,
              inherit.aes = F, fill = "red", alpha = 0.3, color = "red",
              linewidth = 1.1) +
      # scale_x_continuous(limits = c(caja[1], caja[3])) + scale_y_continuous(limits = c(caja[2],caja[4])) +
      guides(fill = "none") +
      theme_minimal() +
      ggtitle(glue::glue("Municipio: {unique(aux_mapeo$NOM_MUN)} \n Localidad: {unique(aux_mapeo$NOM_LOC)}  \n {u_cluster}: {i}")) +
      labs(subtitle =  etiqueta_mapa(resumen_i, zoom),
           caption = glue::glue("{resumen_i$mapa}/{resumen_i$total_mapas}")) +
      theme(plot.title = element_text(hjust = 1),
            plot.subtitle = element_text(size = 10, hjust = 0),
            plot.caption = element_text(size = 16, hjust = 1, face = "bold"))

    ggsave(g, filename= sprintf("%s.png", i),
           path=dir,width = 11,height = 8.5,units = "in",dpi = "print", bg = "white")
    
  }
  beepr::beep()

}

#' Exportar mapas de campo con Google Maps (marco electoral INE)
#'
#' Versión para el marco del INE: genera y guarda en disco un mapa por unidad
#' mínima de la muestra usando imágenes de Google Maps. El subtítulo de cada
#' mapa indica lo operativo del conglomerado (no cuotas): el zoom, las
#' manzanas a visitar, los contactos (viviendas a levantar) y las entrevistas
#' efectivas planeadas.
#'
#' @param diseño Objeto de la clase [DiseñoINE] con la muestra extraída.
#' @param shp Lista de cartografías electorales del diseño.
#' @param zoom Nivel de zoom de Google Maps.
#' @param dir Carpeta de destino de los mapas (por defecto `"Mapas"`).
#' @param exportar `logical`. Si es `TRUE`, escribe los mapas en disco.
#' @param cluster Identificador opcional de un conglomerado para limitar la
#'   generación de mapas a ese conglomerado.
#'
#' @return Invisible. Se ejecuta por su efecto secundario (escribe los mapas).
#' @export
google_maps_ine <- function(diseño, shp, zoom, dir = "Mapas", exportar = T, cluster = NULL){

  u_nivel <- diseño$niveles %>% filter(nivel == diseño$ultimo_nivel)
  u_cluster <- u_nivel %>% transmute(paste(tipo,nivel,sep = "_")) %>% pull(1)
  bd <- diseño$muestra %>% purrr::pluck(length(diseño$muestra)) %>% tidyr::unnest(data)

  if(is.null(cluster)){
    cluster <- bd %>% distinct(!!rlang::sym(u_cluster)) %>% pull(1)
    ya <- list.files(path=dir) %>% gsub('^.*_\\s*|\\s*.png.*$', '', .)
    cluster <- cluster[!cluster %in% ya]
  }

  # agebs <- agebs %>% mutate(CVE_AGEB = paste0(22,CVE_MUN,CVE_LOC,CVE_AGEB))
  shp_mapa <- shp %>% purrr::pluck(u_nivel %>% pull(variable)) %>% inner_join(bd)
  man_shp <- shp %>% purrr::pluck("MANZANA") %>% inner_join(bd)

  # subtítulo operativo por conglomerado (ya no cuotas): zoom, manzanas,
  # contactos y entrevistas efectivas planeadas
  resumen <- resumen_operativo(diseño)

  for(i in cluster){
    resumen_i <- resumen %>% filter(!!rlang::sym(u_cluster) == i)
    man <- man_shp %>% filter(!!rlang::sym(u_cluster) == i)
    aux_mapeo <- shp_mapa %>% filter(!!rlang::sym(u_cluster) == i)
    caja <- aux_mapeo %>% sf::st_make_valid() %>% sf::st_union() %>% sf::st_centroid() %>% sf::st_coordinates() %>% as.numeric()
    nc_map <- ggmap::get_map(location = caja, maptype = "roadmap",
                             source = "google",force = T, zoom = zoom)
    Google <- ggmap::ggmap(nc_map)
    # Google
    puntos <- man %>% filter(sf::st_geometry_type(.) == "POINT")
    man <- man %>% filter(sf::st_geometry_type(.) != "POINT")
    g <- Google +
      # contorno de la sección (azul, grueso) y manzanas a levantar (rojo,
      # con relleno tenue para que resalten sobre el mapa)
      geom_sf(data = aux_mapeo,
              inherit.aes = F, fill = NA, color = "blue", linewidth = 1.4) +
      geom_sf(data = man,
              inherit.aes = F, fill = "red", alpha = 0.3, color = "red",
              linewidth = 1.1) +
      geom_sf(data = puntos,
              inherit.aes = F, alpha = 1, color = "red", size = 3) +
      geom_sf_label(data = puntos, color = "red",
                    inherit.aes = F, aes(label = MANZANA), hjust = "inward",
                   vjust = "inward", size = 2) +
      # scale_x_continuous(limits = c(caja[1], caja[3])) + scale_y_continuous(limits = c(caja[2],caja[4])) +
      guides(fill = "none") +
      theme_minimal() +
      ggtitle(glue::glue("Municipio: {unique(aux_mapeo$NOMBRE_MUN)}  \n {u_cluster}: {i}")) +
      labs(subtitle =  etiqueta_mapa(resumen_i, zoom),
           caption = glue::glue("{resumen_i$mapa}/{resumen_i$total_mapas}")) +
      theme(plot.title = element_text(hjust = 1),
            plot.subtitle = element_text(size = 10, hjust = 0),
            plot.caption = element_text(size = 16, hjust = 1, face = "bold"))

    
    
      
    if(exportar){
      ggsave(g, filename= sprintf("%s.png", i),
      path=dir,width = 11,height = 8.5,units = "in",dpi = "print")
    } else{
      return(g)
    }
  }
  beepr::beep()

}
