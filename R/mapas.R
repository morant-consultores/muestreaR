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
