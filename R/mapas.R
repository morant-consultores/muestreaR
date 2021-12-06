#' Title
#'
#' @param bd
#' @param shp
#' @param nivel
#' @param variable
#'
#' @return
#' @export
#' @import leaflet
#' @examples
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
                             filter(sf::st_geometry_type(.) != "POINT"),
                           color = ~pal(nivel), fillOpacity = 1,weight = 1,
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
