# Mapa interactivo de la muestra para planear rutas de campo ---------------
#
# A diferencia de los PNG de google_maps() (uno por AGEB, para levantar en
# sitio), este mapa es UNO SOLO, interactivo y exportable a HTML: campo lo
# abre en el navegador y planea las rutas viendo todas las manzanas a la vez,
# agrupadas por AGEB, con lo que hay que hacer en cada una.

# Paleta de colores para agrupar las manzanas por AGEB: un color por AGEB,
# recicla una paleta cualitativa (los AGEBs lejanos pueden repetir color, no
# importa; lo que cuenta es que las manzanas de un mismo AGEB compartan uno).
# Recicla a mano (no interpola) y funciona con cualquier número de AGEBs sin
# el warning de RColorBrewer (Set1 topa en 9).
paleta_agebs <- function(grupos) {
  base <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00",
            "#a65628", "#f781bf", "#1b9e77", "#666666", "#e6ab02")
  leaflet::colorFactor(rep(base, length.out = length(grupos)), domain = grupos)
}

# Capas sf listas para leaflet: municipios (cobertura), los AGEBs sorteados y
# las manzanas sorteadas, cada una con su columna `popup` (HTML) y las
# variables operativas. Es la parte con lógica de datos (testeable) del mapa
# interactivo del flujo censal.
capas_leaflet_ageb <- function(diseño, cartografia) {
  shp <- if (inherits(cartografia, "Cartografia")) cartografia$shp else cartografia
  u_cluster <- paste0("cluster_", diseño$ultimo_nivel)

  bd <- diseño$muestra %>%
    purrr::pluck(length(diseño$muestra)) %>%
    tidyr::unnest(data)
  resumen <- resumen_operativo(diseño)

  # viviendas a levantar por manzana (n_0, ya con el ajuste de extraer_muestra)
  viviendas <- diseño$n_i$cluster_0 %>% dplyr::select(cluster_0, viviendas = n_0)
  # el id corto de manzana del cuestionario (1..k por conglomerado): la MISMA
  # numeración que imprimen los PNG de google_maps y los CSV de campo
  numeracion <- numerar_manzanas(diseño) %>%
    dplyr::select(cluster_0, manzana_num)

  manzanas <- shp %>%
    purrr::pluck("MZA") %>%
    dplyr::inner_join(
      bd %>% dplyr::select(dplyr::all_of(u_cluster), cluster_0, MZA, AGEB,
                           MUN, NOM_MUN, NOM_LOC),
      by = "MZA"
    ) %>%
    dplyr::left_join(viviendas, by = "cluster_0") %>%
    dplyr::left_join(numeracion, by = "cluster_0") %>%
    dplyr::mutate(popup = paste0(
      "<b>cluster_2 ", .data[[u_cluster]],
      "</b> · Manzana <b>", manzana_num, "</b>",
      "<br>", NOM_MUN, " — ", NOM_LOC,
      "<br>AGEB: ", AGEB,
      "<br>Clave de manzana: ", MZA,
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
    # `.cluster` = el id cluster_2 que nombra y titula el PNG de google_maps;
    # se muestra igual aquí para que campo cruce el mapa impreso con este
    dplyr::mutate(
      .cluster = .data[[u_cluster]],
      popup = paste0(
        "<b>cluster_2 ", .cluster, "</b> — mapa ", mapa, "/", total_mapas,
        "<br>AGEB ", AGEB,
        "<br>", NOM_MUN,
        "<br>Manzanas: ", manzanas,
        "<br>Contactos (viviendas): ", contactos,
        "<br>Entrevistas efectivas: ", entrevistas
      )
    )

  # capa municipal: TODOS los municipios de la cartografía, marcados según
  # tengan o no AGEBs en la muestra, con el total de contactos y entrevistas
  # planeadas (para que campo vea la cobertura y reparta la carga por municipio)
  mun_agg <- bd %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(u_cluster)), MUN) %>%
    dplyr::left_join(resumen, by = u_cluster) %>%
    dplyr::group_by(MUN) %>%
    dplyr::summarise(agebs = dplyr::n(),
                     contactos = sum(contactos),
                     entrevistas = sum(entrevistas),
                     .groups = "drop")

  municipios <- shp %>%
    purrr::pluck("MUN") %>%
    dplyr::left_join(mun_agg, by = "MUN") %>%
    dplyr::mutate(
      # los nombres vienen del shapefile (CP1252): a UTF-8 para que el widget
      # serialice bien aunque la cartografía se haya leído con otro encoding
      NOM_MUN     = enc2utf8(as.character(NOM_MUN)),
      en_muestra  = !is.na(agebs),
      agebs       = dplyr::coalesce(agebs, 0L),
      contactos   = dplyr::coalesce(contactos, 0),
      entrevistas = dplyr::coalesce(entrevistas, 0),
      popup = paste0(
        "<b>", NOM_MUN, "</b><br>",
        ifelse(en_muestra, "EN MUESTRA", "Sin muestra"),
        "<br>AGEBs en muestra: ", agebs,
        "<br>Contactos planeados: ", contactos,
        "<br>Entrevistas planeadas: ", entrevistas
      )
    )

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

  list(municipios = municipios, agebs = agebs, manzanas = manzanas)
}

#' Mapa interactivo de la muestra para planear rutas de campo (leaflet)
#'
#' Construye un mapa interactivo con **todas** las manzanas sorteadas y los
#' AGEBs de la muestra (flujo censal por AGEB, [disenar_muestra_ageb()]),
#' pensado para que campo **planee las rutas** desde el navegador: las
#' manzanas se colorean por AGEB (las de un mismo AGEB comparten color, se
#' visitan juntas) y cada una trae en su popup lo que hay que hacer
#' (municipio, localidad, AGEB, manzana y viviendas a levantar); cada AGEB
#' se identifica con su **cluster_2** — el MISMO id que nombra y titula los
#' PNG de [google_maps()] — para que campo cruce sin ambigüedad el mapa
#' impreso con este, más su resumen operativo y su número de mapa. Sobre esas dos capas va la **capa municipal**: todos los
#' municipios del estado, en verde si salieron en la muestra y gris si no,
#' con el nombre y el total de contactos y entrevistas planeadas por
#' municipio (para ver la cobertura y repartir la carga de campo). Incluye
#' capas base (calles y satélite), control de capas (municipios / AGEBs /
#' manzanas), leyenda de cobertura y una herramienta de medición de
#' distancias para trazar recorridos.
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
  pal <- paleta_agebs(unique(capas$manzanas$.grupo))

  mapa <- leaflet::leaflet(
    options = leaflet::leafletOptions(preferCanvas = TRUE)
  ) %>%
    leaflet::addProviderTiles("CartoDB.Positron", group = "Calles") %>%
    leaflet::addProviderTiles("Esri.WorldImagery", group = "Satélite") %>%
    # municipios: verde si salieron en la muestra, gris si no; el label trae
    # el nombre y el total de contactos y entrevistas planeadas del municipio
    leaflet::addPolygons(
      data = capas$municipios,
      group = "Municipios",
      fillColor = ~ifelse(en_muestra, "#2a9d8f", "#adb5bd"),
      fillOpacity = 0.2, color = "#495057", weight = 1, opacity = 0.6,
      popup = ~popup,
      label = ~lapply(paste0(
        "<b>", NOM_MUN, "</b><br>",
        ifelse(en_muestra,
               paste0("Contactos: ", contactos,
                      " · Entrevistas: ", entrevistas),
               "Sin muestra")),
        htmltools::HTML),
      highlightOptions = leaflet::highlightOptions(
        weight = 2, color = "#000000", fillOpacity = 0.35, bringToFront = FALSE
      )
    ) %>%
    # contorno de los AGEBs (contexto y resumen operativo)
    leaflet::addPolygons(
      data = capas$agebs,
      group = "AGEBs",
      fill = FALSE, color = "#1d3557", weight = 2, opacity = 0.9,
      popup = ~popup,
      label = ~lapply(paste0("cluster_2 ", .cluster, " (mapa ", mapa, "/",
                             total_mapas, ")"),
                      htmltools::HTML)
    ) %>%
    # manzanas sorteadas (lo que se levanta), coloreadas por AGEB
    leaflet::addPolygons(
      data = capas$manzanas,
      group = "Manzanas a levantar",
      fillColor = ~pal(.grupo), fillOpacity = 0.5,
      color = "#e63946", weight = 1.5, opacity = 1,
      popup = ~popup,
      label = ~lapply(paste0("cluster_2 ", .grupo, " · Manzana <b>",
                             manzana_num, "</b> · ", viviendas, " viviendas"),
                      htmltools::HTML),
      highlightOptions = leaflet::highlightOptions(
        weight = 3, color = "#000000", fillOpacity = 0.8, bringToFront = TRUE
      )
    ) %>%
    # el número corto de cada manzana, siempre visible sobre su polígono
    # (el mismo que capturan en el cuestionario y que imprime el PNG)
    leaflet::addLabelOnlyMarkers(
      data = sf::st_point_on_surface(sf::st_geometry(capas$manzanas)) %>%
        sf::st_sf(manzana_num = capas$manzanas$manzana_num),
      group = "Manzanas a levantar",
      label = ~as.character(manzana_num),
      labelOptions = leaflet::labelOptions(
        noHide = TRUE, textOnly = TRUE, direction = "center",
        style = list("font-weight" = "bold", "font-size" = "13px",
                     "color" = "#1d1d1d",
                     "text-shadow" = "0 0 3px #ffffff, 0 0 3px #ffffff")
      )
    ) %>%
    leaflet::addLayersControl(
      baseGroups = c("Calles", "Satélite"),
      overlayGroups = c("Municipios", "AGEBs", "Manzanas a levantar"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) %>%
    # leyenda de la cobertura municipal
    leaflet::addLegend(
      position = "bottomright",
      colors = c("#2a9d8f", "#adb5bd"),
      labels = c("Municipio en muestra", "Municipio sin muestra"),
      title = "Cobertura", opacity = 0.7
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
