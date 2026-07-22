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
    dplyr::select(MZA) %>%
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
    # solo la llave de unión (+geometría): la capa puede traer también la
    # llave simple AGEB (contrato Preproceso) y duplicaría columnas del bd
    dplyr::select(AULR) %>%
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

# ---- Flujo SECCIONAL INE (UPM = sección electoral) ------------------------

# primera capa existente entre varios nombres posibles (las cartografías INE
# nombran MANZANA/SECCION/MUNICIPIO; fixtures y variantes viejas usan MZA/MUN)
.capa_shp <- function(shp, candidatos) {
  nombre <- intersect(candidatos, names(shp))
  if (length(nombre) == 0) {
    stop("La cartografía no trae ninguna capa ", paste(candidatos, collapse = "/"),
         ".", call. = FALSE)
  }
  shp[[nombre[1]]]
}

# llaves de unión entre una capa sf y la muestra: la intersección de las
# columnas de IDENTIDAD INE presentes en ambas (nunca columnas de datos)
.llaves_ine <- function(capa, bd, candidatos) {
  llaves <- intersect(candidatos, intersect(names(capa), names(bd)))
  if (length(llaves) == 0) {
    stop("Sin llaves comunes entre la capa y la muestra (busqué: ",
         paste(candidatos, collapse = ", "), ").", call. = FALSE)
  }
  llaves
}

# Capas sf listas para leaflet del flujo seccional INE: municipios
# (cobertura), secciones sorteadas y manzanas sorteadas con su popup.
# `ruta` (opcional): el presupuesto de campo por manzana, keyed por
# cluster_0 — columnas orden_ruta, toques_esperados, dentro_presupuesto y
# puertas_presupuesto_seccion. Sin `ruta`, la dosis mostrada es n_0 y el
# orden es el determinista (manzanas de cada sección por su clave).
capas_leaflet_seccional <- function(diseño, cartografia, ruta = NULL) {
  shp <- if (inherits(cartografia, "Cartografia")) cartografia$shp else cartografia
  if ("shp" %in% names(shp) && !inherits(shp, "sf")) shp <- shp$shp

  bd <- diseño$muestra %>%
    purrr::pluck(length(diseño$muestra)) %>%
    tidyr::unnest(data)
  viviendas <- diseño$n_i$cluster_0 %>%
    dplyr::select(cluster_0, viviendas = n_0)

  candidatos_mza <- c("ENTIDAD", "DISTRITO_F", "DISTRITO_L", "MUNICIPIO",
                      "SECCION", "LOCALIDAD", "MANZANA", "id", "cluster_0")

  shp_mza <- .capa_shp(shp, c("MANZANA", "MZA"))
  llaves_mza <- .llaves_ine(shp_mza, bd, candidatos_mza)
  cols_bd <- unique(c(llaves_mza, "cluster_0", "cluster_2", "SECCION",
                      "NOMBRE_MUN"))
  cols_bd <- intersect(cols_bd, names(bd))

  manzanas <- shp_mza %>%
    dplyr::select(dplyr::all_of(llaves_mza)) %>%
    dplyr::inner_join(bd %>% dplyr::select(dplyr::all_of(cols_bd)),
                      by = llaves_mza) %>%
    dplyr::left_join(viviendas, by = "cluster_0")

  if (!is.null(ruta)) {
    stopifnot("cluster_0" %in% names(ruta))
    manzanas <- manzanas %>%
      dplyr::left_join(
        ruta %>% dplyr::select(dplyr::any_of(c(
          "cluster_0", "orden_ruta", "toques_esperados",
          "dentro_presupuesto", "puertas_presupuesto_seccion"))),
        by = "cluster_0")
  }
  if (!"orden_ruta" %in% names(manzanas)) {
    clave_orden <- intersect(c("MANZANA", "id", "cluster_0"), names(manzanas))[1]
    manzanas <- manzanas %>%
      dplyr::group_by(SECCION) %>%
      dplyr::arrange(.data[[clave_orden]], .by_group = TRUE) %>%
      dplyr::mutate(orden_ruta = dplyr::row_number()) %>%
      dplyr::ungroup()
  }

  manzanas <- manzanas %>%
    dplyr::mutate(popup = paste0(
      "<b>Sección ", SECCION, "</b> · Manzana <b>", orden_ruta, "</b>",
      if ("NOMBRE_MUN" %in% names(manzanas)) paste0("<br>", NOMBRE_MUN) else "",
      if ("MANZANA" %in% names(manzanas))
        paste0("<br>Clave de manzana: ", manzanas$MANZANA) else "",
      if ("toques_esperados" %in% names(manzanas)) paste0(
        "<br><b>Toques esperados (sistemático de 2): ",
        manzanas$toques_esperados, "</b>",
        "<br>", ifelse(manzanas$dentro_presupuesto,
                       "Dentro del presupuesto",
                       "RESERVA (no caminar salvo instrucción)")
      ) else paste0("<br><b>Viviendas a levantar: ", manzanas$viviendas, "</b>")
    ))

  # resumen por sección (de la ruta si existe; si no, del modelo n_0)
  resumen_secc <- if (!is.null(ruta) &&
                      "puertas_presupuesto_seccion" %in% names(manzanas)) {
    manzanas %>% sf::st_drop_geometry() %>%
      dplyr::group_by(SECCION) %>%
      dplyr::summarise(
        mzas = dplyr::n(),
        contactos = dplyr::first(puertas_presupuesto_seccion),
        detalle = paste0("Presupuesto de puertas: ",
                         dplyr::first(puertas_presupuesto_seccion),
                         " · manzanas en ruta: ",
                         sum(dplyr::coalesce(dentro_presupuesto, TRUE)),
                         " (+", sum(!dplyr::coalesce(dentro_presupuesto, TRUE)),
                         " reserva)"),
        .groups = "drop")
  } else {
    manzanas %>% sf::st_drop_geometry() %>%
      dplyr::group_by(SECCION) %>%
      dplyr::summarise(
        mzas = dplyr::n(),
        contactos = sum(viviendas, na.rm = TRUE),
        detalle = paste0("Contactos (viviendas): ",
                         sum(viviendas, na.rm = TRUE)),
        .groups = "drop")
  }

  shp_secc <- .capa_shp(shp, c("SECCION", "SECC"))
  llaves_secc <- .llaves_ine(shp_secc, bd, c("ENTIDAD", "MUNICIPIO", "SECCION"))
  secciones <- shp_secc %>%
    dplyr::select(dplyr::all_of(llaves_secc)) %>%
    dplyr::inner_join(
      bd %>% dplyr::distinct(dplyr::across(dplyr::all_of(
        intersect(c(llaves_secc, "NOMBRE_MUN"), names(bd))))),
      by = llaves_secc) %>%
    dplyr::left_join(resumen_secc, by = "SECCION") %>%
    dplyr::mutate(popup = paste0(
      "<b>Sección ", SECCION, "</b>",
      if ("NOMBRE_MUN" %in% names(.)) paste0("<br>", NOMBRE_MUN) else "",
      "<br>Manzanas sorteadas: ", mzas,
      "<br>", detalle
    ))

  shp_mun <- .capa_shp(shp, c("MUNICIPIO", "MUN"))
  llaves_mun <- .llaves_ine(shp_mun, bd, c("ENTIDAD", "MUNICIPIO"))
  col_nombre <- intersect(c("NOMBRE_MUN", "NOM_MUN", "NOMBRE"), names(shp_mun))
  mun_agg <- secciones %>% sf::st_drop_geometry() %>%
    dplyr::left_join(
      bd %>% dplyr::distinct(SECCION,
                             dplyr::across(dplyr::all_of(llaves_mun))),
      by = intersect(c("SECCION", llaves_mun), names(secciones))) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(llaves_mun))) %>%
    dplyr::summarise(secciones = dplyr::n(),
                     contactos = sum(contactos, na.rm = TRUE),
                     .groups = "drop")

  municipios <- shp_mun %>%
    dplyr::select(dplyr::all_of(c(llaves_mun, col_nombre[1]))) %>%
    dplyr::left_join(mun_agg, by = llaves_mun) %>%
    dplyr::mutate(
      nombre = enc2utf8(as.character(.data[[col_nombre[1]]])),
      en_muestra = !is.na(secciones),
      secciones = dplyr::coalesce(secciones, 0L),
      contactos = dplyr::coalesce(contactos, 0),
      popup = paste0(
        "<b>", nombre, "</b><br>",
        ifelse(en_muestra, "EN MUESTRA", "Sin muestra"),
        "<br>Secciones en muestra: ", secciones,
        "<br>Puertas presupuestadas: ", contactos
      )
    )

  list(municipios = municipios, secciones = secciones, manzanas = manzanas)
}

#' Mapa interactivo de la muestra seccional para planear rutas (leaflet)
#'
#' El equivalente de [mapa_interactivo_ageb()] para el flujo INE por
#' SECCIÓN electoral ([disenar_muestra_ine()]): un solo HTML con todas
#' las manzanas sorteadas coloreadas por sección y numeradas con su ORDEN
#' DE RUTA (el mismo del listado/Excel de campo), el contorno de las
#' secciones con su presupuesto, y la capa municipal de cobertura. Capas
#' base calles/satélite, control de capas, leyenda y medidor de
#' distancias.
#'
#' @param diseño Objeto [DiseñoINE] con la muestra extraída.
#' @param cartografia Objeto [CartografiaINE]/[Cartografia] (o su lista
#'   `$shp`) con capas MANZANA/SECCION/MUNICIPIO (o MZA/SECC/MUN).
#' @param ruta `data.frame` opcional del presupuesto de campo por manzana,
#'   keyed por `cluster_0`: `orden_ruta`, `toques_esperados`,
#'   `dentro_presupuesto`, `puertas_presupuesto_seccion` (el listado del
#'   presupuesto). Sin él, se muestran las viviendas n_0 del modelo.
#' @param archivo Ruta opcional del HTML a exportar (autocontenido si hay
#'   pandoc). `NULL` devuelve el objeto leaflet.
#' @param titulo Título del mapa.
#'
#' @return Un objeto `leaflet`, o la ruta del HTML si se dio `archivo`.
#' @seealso [mapa_interactivo_ageb()], [disenar_muestra_ine()]
#' @export
mapa_interactivo_seccional <- function(diseño, cartografia, ruta = NULL,
                                       archivo = NULL,
                                       titulo = "Muestra seccional — planeación de rutas") {
  capas <- capas_leaflet_seccional(diseño, cartografia, ruta = ruta)

  capas$manzanas$.grupo <- as.character(capas$manzanas$SECCION)
  pal <- paleta_agebs(unique(capas$manzanas$.grupo))
  etiqueta_dosis <- if ("toques_esperados" %in% names(capas$manzanas)) {
    paste0(capas$manzanas$toques_esperados, " toques")
  } else {
    paste0(capas$manzanas$viviendas, " viviendas")
  }

  mapa <- leaflet::leaflet(
    options = leaflet::leafletOptions(preferCanvas = TRUE)
  ) %>%
    leaflet::addProviderTiles("CartoDB.Positron", group = "Calles") %>%
    leaflet::addProviderTiles("Esri.WorldImagery", group = "Satélite") %>%
    leaflet::addPolygons(
      data = capas$municipios,
      group = "Municipios",
      fillColor = ~ifelse(en_muestra, "#2a9d8f", "#adb5bd"),
      fillOpacity = 0.2, color = "#495057", weight = 1, opacity = 0.6,
      popup = ~popup,
      label = ~lapply(paste0(
        "<b>", nombre, "</b><br>",
        ifelse(en_muestra,
               paste0("Secciones: ", secciones, " · Puertas: ", contactos),
               "Sin muestra")),
        htmltools::HTML),
      highlightOptions = leaflet::highlightOptions(
        weight = 2, color = "#000000", fillOpacity = 0.35, bringToFront = FALSE
      )
    ) %>%
    leaflet::addPolygons(
      data = capas$secciones,
      group = "Secciones",
      fill = FALSE, color = "#1d3557", weight = 2, opacity = 0.9,
      popup = ~popup,
      label = ~lapply(paste0("Sección <b>", SECCION, "</b> · ", detalle),
                      htmltools::HTML)
    ) %>%
    {
      # el shapefile de MANZANA del INE mezcla polígonos con PUNTOS (manzanas
      # dispersas): los polígonos van como polígonos y los puntos como
      # marcadores circulares — addPolygons truena con geometría mixta
      m <- .
      tipo <- as.character(sf::st_geometry_type(capas$manzanas))
      es_pol <- tipo %in% c("POLYGON", "MULTIPOLYGON")
      mz_pol <- capas$manzanas[es_pol, ]
      mz_pto <- capas$manzanas[!es_pol, ]
      etiquetas <- lapply(paste0("Sección ", capas$manzanas$.grupo,
                                 " · Manzana <b>", capas$manzanas$orden_ruta,
                                 "</b> · ", etiqueta_dosis),
                          htmltools::HTML)
      if (nrow(mz_pol) > 0) {
        m <- m %>% leaflet::addPolygons(
          data = mz_pol,
          group = "Manzanas a levantar",
          fillColor = ~pal(.grupo), fillOpacity = 0.5,
          color = "#e63946", weight = 1.5, opacity = 1,
          popup = ~popup,
          label = etiquetas[es_pol],
          highlightOptions = leaflet::highlightOptions(
            weight = 3, color = "#000000", fillOpacity = 0.8,
            bringToFront = TRUE
          )
        )
      }
      if (nrow(mz_pto) > 0) {
        m <- m %>% leaflet::addCircleMarkers(
          data = sf::st_centroid(mz_pto),
          group = "Manzanas a levantar",
          radius = 7, fillColor = ~pal(.grupo), fillOpacity = 0.7,
          color = "#e63946", weight = 1.5, opacity = 1,
          popup = ~popup,
          label = etiquetas[!es_pol]
        )
      }
      m
    } %>%
    leaflet::addLabelOnlyMarkers(
      data = suppressWarnings(
        sf::st_point_on_surface(sf::st_geometry(capas$manzanas))) %>%
        sf::st_sf(orden_ruta = capas$manzanas$orden_ruta),
      group = "Manzanas a levantar",
      label = ~as.character(orden_ruta),
      labelOptions = leaflet::labelOptions(
        noHide = TRUE, textOnly = TRUE, direction = "center",
        style = list("font-weight" = "bold", "font-size" = "13px",
                     "color" = "#1d1d1d",
                     "text-shadow" = "0 0 3px #ffffff, 0 0 3px #ffffff")
      )
    ) %>%
    leaflet::addLayersControl(
      baseGroups = c("Calles", "Satélite"),
      overlayGroups = c("Municipios", "Secciones", "Manzanas a levantar"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) %>%
    leaflet::addLegend(
      position = "bottomright",
      colors = c("#2a9d8f", "#adb5bd"),
      labels = c("Municipio en muestra", "Municipio sin muestra"),
      title = "Cobertura", opacity = 0.7
    ) %>%
    leaflet::addMeasure(
      primaryLengthUnit = "meters", secondaryLengthUnit = "kilometers",
      primaryAreaUnit = "sqmeters", position = "topleft"
    ) %>%
    leaflet::addControl(htmltools::HTML(paste0("<b>", titulo, "</b>")),
                        position = "topright")

  if (is.null(archivo)) {
    return(mapa)
  }
  ok <- tryCatch({
    htmlwidgets::saveWidget(mapa, archivo, selfcontained = TRUE, title = titulo)
    TRUE
  }, error = function(e) FALSE)
  if (!ok) {
    htmlwidgets::saveWidget(mapa, archivo, selfcontained = FALSE, title = titulo)
  }
  invisible(archivo)
}
