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
# NOTA: si el rechazo varía por estrato se usa el promedio (exacto en el caso
# uniforme, el habitual); es una guía de planeación para el mapa — el número
# exacto por conglomerado vive en el plan versionado (`plan_ageb$n_plan`).
# La tasa de rechazo vive en el ATRIBUTO `asignacion` que pone
# `disenar_muestra_ine()`. Devolver 0 cuando falta parecía un default inocente y
# no lo es: `resumen_operativo()` calcula entrevistas = contactos x (1 - tasa),
# así que con tasa 0 el mapa impreso promete UNA ENTREVISTA POR PUERTA. Con una
# tasa realista de 0.23 eso es cuatro veces la realidad, y sale sin un solo aviso.
#
# El caso no es hipotético: `$clone()` de R6 NO copia los atributos del objeto, así
# que cualquier diseño clonado —recortar la muestra a la ruta, por ejemplo— pierde
# la asignación y cae aquí. Ver `clonar_diseno()`.
#
# Devuelve NA_real_ (no 0) cuando no hay de dónde saberla: quien la use decide qué
# hacer con un dato que falta, en vez de recibir un número inventado.
resolver_tasa_rechazo <- function(diseño){
  asig <- attr(diseño, "asignacion")
  if(is.null(asig) || is.null(asig[["tasa_rechazo"]])){
    warning("El diseño no trae la asignación (atributo `asignacion`), así que no ",
            "se puede saber la tasa de rechazo. Si el diseño viene de `$clone()`, ",
            "usa `clonar_diseno()`: R6 no copia los atributos.",
            call. = FALSE)
    return(NA_real_)
  }
  mean(asig[["tasa_rechazo"]], na.rm = TRUE)
}

#' Clonar un diseño conservando sus atributos
#'
#' `$clone()` de R6 copia los campos del objeto pero **no sus atributos**, y
#' muestreaR guarda en atributos cosas que el diseño necesita para describirse:
#' `asignacion` (de donde sale la tasa de rechazo y la dosis), `numeracion_base`,
#' `tasas_cluster` y `plan_ageb`. Un clon los pierde en silencio.
#'
#' El síntoma típico es un mapa impreso que promete una entrevista por puerta:
#' sin `asignacion`, [resumen_operativo()] no puede aplicar la tasa de rechazo.
#'
#' @param diseño Objeto de diseño (R6) a clonar.
#' @param deep ¿Clonado profundo? (default `TRUE`, que es lo que se quiere cuando
#'   se va a modificar la muestra del clon sin tocar el original).
#' @return El clon, con los atributos del original.
#' @examples
#' \dontrun{
#' d2 <- clonar_diseno(diseno)          # conserva la asignación
#' identical(attr(d2, "asignacion"), attr(diseno, "asignacion"))  # TRUE
#' }
#' @export
clonar_diseno <- function(diseño, deep = TRUE) {
  if (!inherits(diseño, "R6")) {
    stop("`diseño` debe ser un objeto R6 de diseño.", call. = FALSE)
  }
  clon <- diseño$clone(deep = deep)
  # se copian TODOS los atributos del original salvo los que R gestiona solo
  attrs <- attributes(diseño)
  attrs <- attrs[setdiff(names(attrs), c("class", "names", "row.names"))]
  for (nm in names(attrs)) attr(clon, nm) <- attrs[[nm]]
  clon
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

  res <- muestra %>%
    left_join(diseño$n_i$cluster_0, by = "cluster_0") %>%
    group_by(!!rlang::sym(u_cluster)) %>%
    summarise(manzanas = n(), contactos = sum(n_0), .groups = "drop") %>%
    arrange(!!rlang::sym(u_cluster))

  # tasa POR CONGLOMERADO (attr "tasas_cluster": u_cluster + tasa) — la fija
  # la calibración por AGEB (presupuesto de puertas): así el mapa imprime las
  # entrevistas esperadas de ESE AGEB (contactos x su tasa de logro), no una
  # media global que confunde a campo. Sin el attr, tasa global como siempre.
  tasas_cluster <- attr(diseño, "tasas_cluster")
  if (!is.null(tasas_cluster) &&
      all(c(u_cluster, "tasa") %in% names(tasas_cluster))) {
    res <- res %>%
      left_join(tasas_cluster %>%
                  select(dplyr::all_of(c(u_cluster, "tasa"))),
                by = u_cluster) %>%
      mutate(tasa = dplyr::coalesce(tasa, !!tasa))
  } else {
    res <- res %>% mutate(tasa = !!tasa)
  }

  res %>%
    mutate(entrevistas = round(contactos * (1 - tasa)),
           total_mapas = n(),
           mapa = row_number()) %>%
    select(-tasa)
}

# Conglomerados con polígono en la cartografía (los que sobreviven el join):
# los sorteados sin cartografía —p. ej. AGEBs sin marco geoestadístico 2025—
# no se pueden mapear (su centroide sería una geometría vacía y get_map
# fallaría), así que se excluyen del loop conservando el orden original.
clusters_dibujables <- function(cluster, shp_mapa, u_cluster){
  cluster[cluster %in% unique(shp_mapa[[u_cluster]])]
}

#' Numerar las manzanas de la muestra dentro de cada conglomerado (1, 2, 3, ...)
#'
#' Asigna a cada manzana sorteada un **identificador corto** dentro de su
#' conglomerado (1..k), que es el que usa campo en el **cuestionario** (la
#' clave CVEGEO de 16 caracteres es impráctica para capturar). Es la fuente
#' única de esa numeración: la consumen [google_maps()] (número impreso en
#' cada manzana del PNG), [mapa_interactivo_ageb()] (labels/popups) y los
#' listados CSV que se entregan a campo — así el número es EL MISMO en los
#' tres materiales.
#'
#' La numeración es **estable y reproducible**: dentro de cada conglomerado
#' se ordena por la clave de manzana (`MZA` en el flujo censal, `MANZANA`
#' en el electoral; `cluster_0` como último recurso), así que regenerar
#' cualquier material produce los mismos números. Si el diseño trae el attr
#' `"numeracion_base"` (lo fija [ampliar_manzanas_ageb()]), esos números son
#' inmutables y las manzanas nuevas continúan la secuencia (max+1, ...).
#'
#' @param diseño Objeto [Diseño] (o [DiseñoINE]) con la muestra extraída.
#'
#' @return `tibble` con una fila por manzana sorteada: la columna del
#'   conglomerado de último nivel (p. ej. `cluster_2`), `cluster_0` (la
#'   llave de unión con el marco/muestra) y `manzana_num` (1..k dentro del
#'   conglomerado).
#' @seealso [google_maps()], [mapa_interactivo_ageb()]
#' @export
numerar_manzanas <- function(diseño){
  u_cluster <- diseño$niveles %>%
    filter(nivel == diseño$ultimo_nivel) %>%
    transmute(paste(tipo, nivel, sep = "_")) %>%
    pull(1)
  bd <- diseño$muestra %>%
    purrr::pluck(length(diseño$muestra)) %>%
    tidyr::unnest(data)
  # la clave que ordena: MZA (censal) o MANZANA (INE); cluster_0 de respaldo
  llave <- intersect(c("MZA", "MANZANA"), names(bd))[1]
  if (is.na(llave)) llave <- "cluster_0"

  numer <- bd %>%
    select(dplyr::all_of(unique(c(u_cluster, "cluster_0", llave))))

  # Numeración BASE (attr "numeracion_base", la fija ampliar_manzanas_ageb):
  # los números que campo YA tiene impresos en mapas/cuestionario son
  # inmutables; las manzanas nuevas continúan (max+1, max+2, ...) ordenadas
  # por la misma clave. Sin base, numeración estable 1..k por clave.
  base <- attr(diseño, "numeracion_base")
  if (is.null(base)) {
    return(numer %>%
      group_by(!!rlang::sym(u_cluster)) %>%
      arrange(!!rlang::sym(llave), .by_group = TRUE) %>%
      mutate(manzana_num = dplyr::row_number()) %>%
      ungroup() %>%
      select(dplyr::all_of(c(u_cluster, "cluster_0", "manzana_num"))))
  }

  conocidas <- base %>%
    dplyr::semi_join(numer, by = "cluster_0") %>%
    select(dplyr::all_of(c(u_cluster, "cluster_0", "manzana_num")))
  topes <- conocidas %>%
    group_by(!!rlang::sym(u_cluster)) %>%
    summarise(.tope = max(manzana_num), .groups = "drop")
  nuevas <- numer %>%
    dplyr::anti_join(base, by = "cluster_0") %>%
    dplyr::left_join(topes, by = u_cluster) %>%
    mutate(.tope = dplyr::coalesce(.tope, 0L)) %>%
    group_by(!!rlang::sym(u_cluster)) %>%
    arrange(!!rlang::sym(llave), .by_group = TRUE) %>%
    mutate(manzana_num = .tope + dplyr::row_number()) %>%
    ungroup() %>%
    select(dplyr::all_of(c(u_cluster, "cluster_0", "manzana_num")))
  dplyr::bind_rows(conocidas, nuevas) %>%
    arrange(!!rlang::sym(u_cluster), manzana_num)
}

# Zoom de Google Static Maps que ENCUADRA un bbox (Web Mercator): el mayor
# nivel al que el bbox del conglomerado (con margen) cabe en un tile de
# `size` px lógicos. Los mapas de campo lo calculan POR conglomerado, con el
# `zoom` del usuario como techo de detalle: un zoom fijo corta manzanas
# cuando la muestra crece (p. ej. ampliación de 4 a 9 manzanas por AGEB).
zoom_para_bbox <- function(bbox, size = 640, margen = 1.15, zoom_max = 16){
  merc <- function(lat) log(tan(pi / 4 + pmax(pmin(lat, 85), -85) * pi / 360))
  lon_span <- as.numeric(bbox[["xmax"]] - bbox[["xmin"]])
  lat_span <- merc(as.numeric(bbox[["ymax"]])) - merc(as.numeric(bbox[["ymin"]]))
  z_lon <- if (isTRUE(lon_span > 0)) log2(size * 360 / (256 * lon_span * margen)) else Inf
  z_lat <- if (isTRUE(lat_span > 0)) log2(size * 2 * pi / (256 * lat_span * margen)) else Inf
  z <- suppressWarnings(floor(min(z_lon, z_lat)))
  if (!is.finite(z)) z <- zoom_max
  as.integer(max(1, min(zoom_max, z)))
}

# bbox conjunto del conglomerado (polígono del cluster + sus manzanas)
# Encuadre del mapa de un conglomerado.
#
# Por default toma la UNIÓN del contorno del conglomerado y sus manzanas, que es
# lo correcto cuando las manzanas cubren buena parte del conglomerado. Deja de
# serlo cuando el conglomerado es mucho más grande que lo sorteado: medido en
# Huehuetoca (ago-2026), una sección de 7 km con 4 manzanas juntas encuadraba a
# zoom 13 y las manzanas salían como puntos ilegibles — el mapa dejaba de servir
# para caminar.
#
# Con `contexto_m` el encuadre se calcula sobre las MANZANAS más ese margen en
# metros, y el contorno del conglomerado sigue dibujándose (recortado por el
# encuadre). En el mismo caso, el zoom pasó de 13 a 16.
bbox_cluster <- function(aux_mapeo, man, contexto_m = NULL){
  if (!is.null(contexto_m) && nrow(man) > 0) {
    bm <- sf::st_bbox(man)
    # grados por metro a esta latitud: la longitud se encoge con el coseno
    lat_med <- mean(c(bm[["ymin"]], bm[["ymax"]]))
    d_lat <- contexto_m / 110540
    d_lon <- contexto_m / (111320 * max(cos(lat_med * pi / 180), 1e-6))
    bm[["xmin"]] <- bm[["xmin"]] - d_lon; bm[["xmax"]] <- bm[["xmax"]] + d_lon
    bm[["ymin"]] <- bm[["ymin"]] - d_lat; bm[["ymax"]] <- bm[["ymax"]] + d_lat
    return(bm)
  }
  bb <- sf::st_bbox(aux_mapeo)
  if (nrow(man) > 0) {
    bm <- sf::st_bbox(man)
    bb[["xmin"]] <- min(bb[["xmin"]], bm[["xmin"]])
    bb[["ymin"]] <- min(bb[["ymin"]], bm[["ymin"]])
    bb[["xmax"]] <- max(bb[["xmax"]], bm[["xmax"]])
    bb[["ymax"]] <- max(bb[["ymax"]], bm[["ymax"]])
  }
  bb
}

# Subtítulo del mapa a partir de una fila del resumen operativo y el zoom.
etiqueta_mapa <- function(resumen_i, zoom){
  # si no se pudo resolver la tasa, se dice que no se sabe. Antes se imprimía un
  # número igual a los contactos —una entrevista por puerta— que campo lee como
  # una meta alcanzable.
  entrevistas <- if (is.na(resumen_i$entrevistas)) "no disponible (falta la asignación)"
                 else resumen_i$entrevistas
  paste(
    glue::glue("Zoom: {zoom}"),
    glue::glue("Manzanas: {resumen_i$manzanas}"),
    glue::glue("Contactos planeados: {resumen_i$contactos}"),
    glue::glue("Entrevistas planeadas: {entrevistas}"),
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
  # el id corto de manzana del cuestionario (1..k por conglomerado), impreso
  # sobre cada manzana; la MISMA numeración del mapa interactivo y los CSV
  man_shp <- man_shp %>%
    left_join(numerar_manzanas(diseño) %>% select(cluster_0, manzana_num),
              by = "cluster_0")

  # conglomerados sin polígono en la cartografía: se reportan y se saltan
  sin_carto <- setdiff(cluster, clusters_dibujables(cluster, shp_mapa, u_cluster))
  if(length(sin_carto) > 0){
    warning(length(sin_carto), " conglomerado(s) sin cartografía se omiten ",
            "del mapeo (georreferenciar aparte): ",
            paste(utils::head(sin_carto, 10), collapse = ", "), call. = FALSE)
  }
  cluster <- clusters_dibujables(cluster, shp_mapa, u_cluster)

  for(i in cluster){
    resumen_i <- resumen %>% filter(!!rlang::sym(u_cluster) == i)
    man <- man_shp %>% filter(!!rlang::sym(u_cluster) == i)
    aux_mapeo <- shp_mapa %>% filter(!!rlang::sym(u_cluster) == i)
    # encuadre por conglomerado: centro del bbox AGEB+manzanas y el mayor
    # zoom al que TODO cabe (con `zoom` como techo de detalle) — con zoom
    # fijo, las muestras ampliadas dejan manzanas fuera del cuadro
    bb <- bbox_cluster(aux_mapeo, man)
    zoom_i <- zoom_para_bbox(bb, zoom_max = zoom)
    caja <- c(mean(c(bb[["xmin"]], bb[["xmax"]])),
              mean(c(bb[["ymin"]], bb[["ymax"]])))
    nc_map <- ggmap::get_map(location = caja, maptype = "roadmap",
                             source = "google",force = T, zoom = zoom_i)
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
      # el número corto de cada manzana (el que va en el cuestionario)
      geom_sf_label(data = man, inherit.aes = F, aes(label = manzana_num),
                    color = "red", fontface = "bold", size = 4.5,
                    alpha = 0.85, label.size = 0) +
      # scale_x_continuous(limits = c(caja[1], caja[3])) + scale_y_continuous(limits = c(caja[2],caja[4])) +
      guides(fill = "none") +
      # Los límites se fijan AL ENCUADRE. Sin esto `geom_sf` del contorno del
      # conglomerado expande las escalas hasta cubrirlo y el zoom calculado no
      # sirve de nada: el mosaico de Google llega al zoom pedido pero el panel se
      # abre para meter el polígono completo. `expand = FALSE` evita el margen
      # extra que ggplot agrega por default.
      ggplot2::coord_sf(xlim = c(bb[["xmin"]], bb[["xmax"]]),
                        ylim = c(bb[["ymin"]], bb[["ymax"]]),
                        expand = FALSE, default_crs = sf::st_crs(4326)) +
      theme_minimal() +
      ggtitle(glue::glue("Municipio: {unique(aux_mapeo$NOM_MUN)} \n Localidad: {unique(aux_mapeo$NOM_LOC)}  \n {u_cluster}: {i}")) +
      labs(subtitle =  etiqueta_mapa(resumen_i, zoom_i),
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
#' @param contexto_m Margen en metros alrededor de las manzanas para encuadrar el
#'   mapa. `NULL` (default) encuadra el conglomerado completo, que es lo correcto
#'   cuando las manzanas cubren buena parte de él. Ponerlo es lo que mantiene
#'   legible un conglomerado mucho más grande que lo sorteado (ver
#'   `bbox_cluster()`).
google_maps_ine <- function(diseño, shp, zoom, dir = "Mapas", exportar = T,
                            cluster = NULL, contexto_m = NULL){

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
  # el id corto de manzana del cuestionario (1..k por conglomerado)
  man_shp <- man_shp %>%
    left_join(numerar_manzanas(diseño) %>% select(cluster_0, manzana_num),
              by = "cluster_0")

  # conglomerados sin polígono en la cartografía: se reportan y se saltan
  sin_carto <- setdiff(cluster, clusters_dibujables(cluster, shp_mapa, u_cluster))
  if(length(sin_carto) > 0){
    warning(length(sin_carto), " conglomerado(s) sin cartografía se omiten ",
            "del mapeo (georreferenciar aparte): ",
            paste(utils::head(sin_carto, 10), collapse = ", "), call. = FALSE)
  }
  cluster <- clusters_dibujables(cluster, shp_mapa, u_cluster)

  for(i in cluster){
    resumen_i <- resumen %>% filter(!!rlang::sym(u_cluster) == i)
    man <- man_shp %>% filter(!!rlang::sym(u_cluster) == i)
    aux_mapeo <- shp_mapa %>% filter(!!rlang::sym(u_cluster) == i)
    # encuadre por conglomerado (ver google_maps): nada se queda fuera
    bb <- bbox_cluster(sf::st_make_valid(aux_mapeo), man, contexto_m = contexto_m)
    zoom_i <- zoom_para_bbox(bb, zoom_max = zoom)
    caja <- c(mean(c(bb[["xmin"]], bb[["xmax"]])),
              mean(c(bb[["ymin"]], bb[["ymax"]])))
    nc_map <- ggmap::get_map(location = caja, maptype = "roadmap",
                             source = "google",force = T, zoom = zoom_i)
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
      # el número corto de cada manzana (el que va en el cuestionario)
      geom_sf_label(data = man, inherit.aes = F, aes(label = manzana_num),
                    color = "red", fontface = "bold", size = 4.5,
                    alpha = 0.85, label.size = 0) +
      geom_sf(data = puntos,
              inherit.aes = F, alpha = 1, color = "red", size = 3) +
      geom_sf_label(data = puntos, color = "red",
                    inherit.aes = F, aes(label = MANZANA), hjust = "inward",
                   vjust = "inward", size = 2) +
      # scale_x_continuous(limits = c(caja[1], caja[3])) + scale_y_continuous(limits = c(caja[2],caja[4])) +
      guides(fill = "none") +
      # Los límites se fijan AL ENCUADRE. Sin esto `geom_sf` del contorno del
      # conglomerado expande las escalas hasta cubrirlo y el zoom calculado no
      # sirve de nada: el mosaico de Google llega al zoom pedido pero el panel se
      # abre para meter el polígono completo. `expand = FALSE` evita el margen
      # extra que ggplot agrega por default.
      ggplot2::coord_sf(xlim = c(bb[["xmin"]], bb[["xmax"]]),
                        ylim = c(bb[["ymin"]], bb[["ymax"]]),
                        expand = FALSE, default_crs = sf::st_crs(4326)) +
      theme_minimal() +
      ggtitle(glue::glue("Municipio: {unique(aux_mapeo$NOMBRE_MUN)}  \n {u_cluster}: {i}")) +
      labs(subtitle =  etiqueta_mapa(resumen_i, zoom_i),
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
