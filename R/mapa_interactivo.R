# Mapa interactivo de la muestra para planear rutas de campo ---------------
#
# A diferencia de los PNG de google_maps() (uno por AGEB, para levantar en
# sitio), este mapa es UNO SOLO, interactivo y exportable a HTML: campo lo
# abre en el navegador y planea las rutas viendo todas las manzanas a la vez,
# agrupadas por AGEB, con lo que hay que hacer en cada una.

# Capas sf listas para leaflet: los AGEBs sorteados y las manzanas sorteadas,
# cada una con su columna `popup` (HTML) y las variables operativas. Es la
# parte con lógica de datos (testeable) del mapa interactivo del flujo censal.
capas_leaflet_ageb <- function(diseño, cartografia) {
  shp <- if (inherits(cartografia, "Cartografia")) cartografia$shp else cartografia
  u_cluster <- paste0("cluster_", diseño$ultimo_nivel)

  bd <- diseño$muestra %>%
    purrr::pluck(length(diseño$muestra)) %>%
    tidyr::unnest(data)
  resumen <- resumen_operativo(diseño)

  # viviendas a levantar por manzana (n_0, ya con el ajuste de extraer_muestra)
  viviendas <- diseño$n_i$cluster_0 %>% dplyr::select(cluster_0, viviendas = n_0)

  manzanas <- shp %>%
    purrr::pluck("MZA") %>%
    dplyr::inner_join(
      bd %>% dplyr::select(dplyr::all_of(u_cluster), cluster_0, MZA, AGEB,
                           MUN, NOM_MUN, NOM_LOC),
      by = "MZA"
    ) %>%
    dplyr::left_join(viviendas, by = "cluster_0") %>%
    dplyr::mutate(popup = paste0(
      "<b>", NOM_MUN, "</b><br>", NOM_LOC,
      "<br>AGEB: ", AGEB,
      "<br>Manzana: ", MZA,
      "<br><b>Viviendas a levantar: ", viviendas, "</b>"
    ))

  agebs <- shp %>%
    purrr::pluck("AGEB") %>%
    dplyr::inner_join(
      bd %>% dplyr::distinct(dplyr::across(dplyr::all_of(u_cluster)),
                             AULR, AGEB, MUN, NOM_MUN),
      by = "AULR"
    ) %>%
    dplyr::left_join(resumen, by = u_cluster) %>%
    dplyr::mutate(popup = paste0(
      "<b>AGEB ", AGEB, "</b> — mapa ", mapa, "/", total_mapas,
      "<br>", NOM_MUN,
      "<br>Manzanas: ", manzanas,
      "<br>Contactos (viviendas): ", contactos,
      "<br>Entrevistas efectivas: ", entrevistas
    ))

  # manzanas sorteadas cuyo AGEB no tiene polígono (se dibujarían sin su
  # contorno): sólo ocurre si el marco no se reconcilió con la cartografía
  huerfanas <- setdiff(unique(manzanas$AGEB), unique(agebs$AGEB))
  if (length(huerfanas) > 0) {
    warning(sum(manzanas$AGEB %in% huerfanas), " manzana(s) de ",
            length(huerfanas), " AGEB(s) sin polígono de AGEB en la ",
            "cartografía (saldrían sin su contorno). El marco no está ",
            "reconciliado: usa reconciliar_marco_cartografia() antes del ",
            "sorteo para que sólo sea muestreable lo mapeable.",
            call. = FALSE)
  }

  list(agebs = agebs, manzanas = manzanas)
}

#' Mapa interactivo de la muestra para planear rutas de campo (leaflet)
#'
#' Construye un mapa interactivo con **todas** las manzanas sorteadas y los
#' AGEBs de la muestra (flujo censal por AGEB, [disenar_muestra_ageb()]),
#' pensado para que campo **planee las rutas** desde el navegador: las
#' manzanas se colorean por AGEB (las de un mismo AGEB comparten color, se
#' visitan juntas) y cada una trae en su popup lo que hay que hacer
#' (municipio, localidad, AGEB, manzana y viviendas a levantar); cada AGEB
#' trae su resumen operativo y su número de mapa (para cruzarlo con los PNG
#' impresos). Incluye capas base (calles y satélite), control de capas y una
#' herramienta de medición de distancias para trazar recorridos.
#'
#' Complementa a [google_maps()] (un PNG por AGEB, para levantar en sitio):
#' este es un solo HTML de planeación.
#'
#' @param diseño Objeto [Diseño] con la muestra extraída (censal por AGEB).
#' @param cartografia Objeto [Cartografia] (o su lista `$shp`) del diseño.
#' @param archivo Ruta opcional del HTML a exportar. Si se da, escribe el
#'   widget (autocontenido si hay `pandoc`; si no, HTML + carpeta de
#'   dependencias) y devuelve la ruta de forma invisible. Si es `NULL`
#'   (default) devuelve el objeto `leaflet`.
#' @param titulo Título del mapa (y del HTML exportado).
#'
#' @return Un objeto `leaflet`, o la ruta del HTML si se dio `archivo`.
#' @seealso [google_maps()]
#' @export
mapa_interactivo_ageb <- function(diseño, cartografia,
                                  archivo = NULL,
                                  titulo = "Muestra — planeación de rutas") {
  capas <- capas_leaflet_ageb(diseño, cartografia)
  u_cluster <- paste0("cluster_", diseño$ultimo_nivel)

  # un color por AGEB: las manzanas de un mismo AGEB comparten color (se
  # precomputa la columna de grupo para no depender de get() en la fórmula)
  capas$manzanas$.grupo <- as.character(capas$manzanas[[u_cluster]])
  pal <- leaflet::colorFactor("Set1", domain = unique(capas$manzanas$.grupo))

  mapa <- leaflet::leaflet(
    options = leaflet::leafletOptions(preferCanvas = TRUE)
  ) %>%
    leaflet::addProviderTiles("CartoDB.Positron", group = "Calles") %>%
    leaflet::addProviderTiles("Esri.WorldImagery", group = "Satélite") %>%
    # contorno de los AGEBs (contexto y resumen operativo)
    leaflet::addPolygons(
      data = capas$agebs,
      group = "AGEBs",
      fill = FALSE, color = "#1d3557", weight = 2, opacity = 0.9,
      popup = ~popup,
      label = ~lapply(paste0("AGEB ", AGEB, " (mapa ", mapa, "/", total_mapas, ")"),
                      htmltools::HTML)
    ) %>%
    # manzanas sorteadas (lo que se levanta), coloreadas por AGEB
    leaflet::addPolygons(
      data = capas$manzanas,
      group = "Manzanas a levantar",
      fillColor = ~pal(.grupo), fillOpacity = 0.5,
      color = "#e63946", weight = 1.5, opacity = 1,
      popup = ~popup,
      label = ~lapply(paste0("Manzana ", MZA, " · ", viviendas, " viviendas"),
                      htmltools::HTML),
      highlightOptions = leaflet::highlightOptions(
        weight = 3, color = "#000000", fillOpacity = 0.8, bringToFront = TRUE
      )
    ) %>%
    leaflet::addLayersControl(
      baseGroups = c("Calles", "Satélite"),
      overlayGroups = c("AGEBs", "Manzanas a levantar"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) %>%
    # herramienta de medición para trazar recorridos
    leaflet::addMeasure(
      primaryLengthUnit = "meters", secondaryLengthUnit = "kilometers",
      primaryAreaUnit = "sqmeters", position = "topleft"
    ) %>%
    leaflet::addControl(htmltools::HTML(paste0("<b>", titulo, "</b>")),
                        position = "topright")

  if (is.null(archivo)) {
    return(mapa)
  }

  # HTML autocontenido si hay pandoc; si no, HTML + carpeta de dependencias
  # (igual abre en el navegador). Ambos son "exportables" para campo.
  ok <- tryCatch({
    htmlwidgets::saveWidget(mapa, archivo, selfcontained = TRUE, title = titulo)
    TRUE
  }, error = function(e) FALSE)
  if (!ok) {
    htmlwidgets::saveWidget(mapa, archivo, selfcontained = FALSE, title = titulo)
  }
  invisible(archivo)
}
