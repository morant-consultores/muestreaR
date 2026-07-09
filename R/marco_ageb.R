# Marcos censales INEGI por AGEB y manzana ---------------------------------
#
# Fuente: dataset "Principales resultados por AGEB y manzana urbana" del
# Censo de Población y Vivienda 2020 (un CSV por entidad, p. ej.
# conjunto_de_datos_ageb_urbana_15_cpv2020.csv). El CSV mezcla filas de
# totales (entidad, municipio, localidad urbana, AGEB) con filas de
# manzana; se distinguen por NOM_LOC. Leerlo SIN adivinar tipos
# (readr::read_csv(..., col_types = readr::cols(.default = "c"))): aquí se
# parsea lo numérico con el enmascaramiento INEGI ("*" = celda protegida
# por confidencialidad) convertido a NA.

# claves numéricas a ancho fijo aceptando character ("58" -> "058"): formatC
# con flag "0" solo rellena números, así que se rellena el espacio a mano
formato_clave <- function(x, ancho) {
  gsub(" ", "0", formatC(trimws(as.character(x)), width = ancho))
}

# el código AGEB es alfanumérico de 4 ("0010", "025A"); se normaliza igual
formato_ageb <- function(x) {
  gsub(" ", "0", formatC(toupper(trimws(as.character(x))), width = 4))
}

# numérico censal: enmascarados y vacíos a NA
parsear_censo <- function(x) {
  readr::parse_double(as.character(x), na = c("", "NA", "*", "N/D"))
}

columnas_censo <- c("ENTIDAD", "MUN", "NOM_MUN", "LOC", "NOM_LOC", "AGEB",
                    "MZA", "POBTOT", "P_18YMAS", "VIVPAR_HAB")

validar_censo <- function(censo) {
  faltan <- setdiff(columnas_censo, names(censo))
  if (length(faltan) > 0) {
    stop("Al censo le faltan columnas del dataset ageb_mza_urbana: ",
         paste(faltan, collapse = ", "), call. = FALSE)
  }
  invisible(censo)
}

# una clave más ancha que su campo (LOC de 5, MUN de 4, ...) produciría
# llaves que no empatan con los CVEGEO de los shapefiles: se detiene
validar_ancho_llave <- function(llave, ancho, nombre) {
  mal <- nchar(llave) != ancho
  if (any(mal)) {
    stop(sum(mal), " llave(s) de ", nombre, " no miden ", ancho,
         " caracteres (p. ej. ", llave[which(mal)[1]],
         "): revisa los anchos de ENTIDAD/MUN/LOC/AGEB/MZA en el insumo.",
         call. = FALSE)
  }
  invisible(llave)
}

#' Construir el marco muestral por AGEB (censo INEGI)
#'
#' Deriva el marco de AGEBs urbanas del dataset "ageb_mza_urbana" del
#' Censo 2020: una fila por AGEB (las filas `NOM_LOC == "Total AGEB
#' urbana"`) con su llave CVEGEO de 13 caracteres
#' (entidad 2 + municipio 3 + localidad 4 + AGEB 4, la misma del shapefile
#' de AGEB urbana del marco geoestadístico) y sus medidas de tamaño. Es el
#' insumo de [planear_muestra_ageb()].
#'
#' El marco cubre el universo urbano del censo (localidades con AGEB y
#' manzana); esa cobertura debe declararse en la nota metodológica.
#'
#' @param censo `tibble` del CSV del censo leído como texto
#'   (`readr::read_csv(..., col_types = readr::cols(.default = "c"))`).
#'
#' @return `tibble` con `ageb` (llave de 13), `entidad`, `municipio_cod`
#'   (5), `nombre_municipio`, `localidad_cod` (9), `pob18` (población de
#'   18 años y más, la medida de tamaño default del sorteo), `pobtot` y
#'   `viviendas` (particulares habitadas). Enmascarados INEGI (`*`) quedan
#'   como `NA` (tamaño 0 en el sorteo: nunca sorteables).
#' @export
construir_marco_ageb <- function(censo) {
  validar_censo(censo)
  agebs <- censo[censo$NOM_LOC == "Total AGEB urbana", , drop = FALSE]
  if (nrow(agebs) == 0) {
    stop("El censo no trae filas 'Total AGEB urbana': ",
         "¿es el dataset ageb_mza_urbana del Censo 2020?", call. = FALSE)
  }

  marco <- agebs |>
    dplyr::transmute(
      entidad = formato_clave(.data$ENTIDAD, 2),
      municipio_cod = paste0(entidad, formato_clave(.data$MUN, 3)),
      nombre_municipio = .data$NOM_MUN,
      localidad_cod = paste0(municipio_cod, formato_clave(.data$LOC, 4)),
      ageb = paste0(localidad_cod, formato_ageb(.data$AGEB)),
      pob18 = parsear_censo(.data$P_18YMAS),
      pobtot = parsear_censo(.data$POBTOT),
      viviendas = parsear_censo(.data$VIVPAR_HAB)
    ) |>
    dplyr::relocate(ageb)

  validar_ancho_llave(marco$ageb, 13, "AGEB")
  if (anyDuplicated(marco$ageb) > 0) {
    stop("Hay llaves de AGEB duplicadas en el censo (",
         sum(duplicated(marco$ageb)), "): revisa el insumo.", call. = FALSE)
  }

  sin_pob <- is.na(marco$pob18) | marco$pob18 <= 0
  if (any(sin_pob)) {
    message(sum(sin_pob), " AGEB(s) sin población sorteable (pob18 ",
            "enmascarada o en cero): tamaño 0, nunca sorteables. ",
            "Declararlo como cobertura en la nota metodológica.")
  }
  marco
}

#' Construir el marco de manzanas (censo INEGI)
#'
#' Deriva el marco de manzanas urbanas del mismo dataset "ageb_mza_urbana":
#' una fila por manzana (las filas que no son totales) con su llave CVEGEO
#' de 16 caracteres (la llave del AGEB + manzana 3, la misma del shapefile
#' de manzanas) anidada en la llave de [construir_marco_ageb()]. Es el
#' insumo de la Etapa II ([seleccionar_manzanas()]).
#'
#' @inheritParams construir_marco_ageb
#'
#' @return `tibble` con `manzana` (llave de 16), `ageb` (llave de 13),
#'   `municipio_cod`, `nombre_municipio`, `nombre_localidad`, `pob18`,
#'   `pobtot` y `viviendas`. El enmascaramiento INEGI (`*`, común en
#'   manzanas chicas) queda como `NA`.
#' @export
construir_marco_manzanas <- function(censo) {
  validar_censo(censo)
  mzas <- censo[!grepl("^Total", censo$NOM_LOC), , drop = FALSE]
  if (nrow(mzas) == 0) {
    stop("El censo no trae filas de manzana: ",
         "¿es el dataset ageb_mza_urbana del Censo 2020?", call. = FALSE)
  }

  marco <- mzas |>
    dplyr::transmute(
      entidad = formato_clave(.data$ENTIDAD, 2),
      municipio_cod = paste0(entidad, formato_clave(.data$MUN, 3)),
      nombre_municipio = .data$NOM_MUN,
      nombre_localidad = .data$NOM_LOC,
      ageb = paste0(municipio_cod, formato_clave(.data$LOC, 4),
                    formato_ageb(.data$AGEB)),
      manzana = paste0(ageb, formato_clave(.data$MZA, 3)),
      pob18 = parsear_censo(.data$P_18YMAS),
      pobtot = parsear_censo(.data$POBTOT),
      viviendas = parsear_censo(.data$VIVPAR_HAB)
    ) |>
    dplyr::relocate(manzana, ageb) |>
    dplyr::select(-entidad)

  validar_ancho_llave(marco$manzana, 16, "manzana")
  if (anyDuplicated(marco$manzana) > 0) {
    stop("Hay llaves de manzana duplicadas en el censo (",
         sum(duplicated(marco$manzana)), "): revisa el insumo.",
         call. = FALSE)
  }
  marco
}

#' Construir el marco muestral censal AGEB para la clase [Diseño]
#'
#' Versión del marco por manzana COMPATIBLE CON LA CLASE del equipo
#' (la que se exporta como `diseño.rda` y consumen AppAuditoria y
#' encuestar): una fila por manzana urbana del dataset "ageb_mza_urbana"
#' con las columnas y llaves de [crear_mm()] — `ENTIDAD` (2), `MUN` (5),
#' `LOC` (9), `AGEB` (13), `MZA` (16), las llaves de cruce con la
#' cartografía (`ARLU`, `AULR`), `id` — y TODO el bloque censal numérico
#' parseado (de `POBTOT` en adelante; el enmascaramiento `*` queda como
#' `NA`). [cuotas()] y el rake `tipo_encuesta = "inegi"` de encuestar usan
#' de ahí `P_18A24_*`, `P_18YMAS_*` y `P_60YMAS_*`.
#'
#' A diferencia de [crear_mm()] no requiere la base de localidades (ITER)
#' ni shapefiles: el dataset es el universo urbano (localidades con AGEB y
#' manzana), que es el marco del diseño por AGEBs.
#'
#' @inheritParams construir_marco_ageb
#'
#' @return `tibble` con una fila por manzana, listo para
#'   `PoblacionAGEB$new()` / [Diseño].
#' @export
crear_mm_ageb <- function(censo) {
  validar_censo(censo)
  mzas <- censo[!grepl("^Total", censo$NOM_LOC), , drop = FALSE]
  if (nrow(mzas) == 0) {
    stop("El censo no trae filas de manzana: ",
         "¿es el dataset ageb_mza_urbana del Censo 2020?", call. = FALSE)
  }

  # bloque censal numérico completo (POBTOT en adelante), como crear_mm
  cols_num <- names(mzas)[match("POBTOT", names(mzas)):ncol(mzas)]

  marco <- mzas |>
    dplyr::mutate(dplyr::across(dplyr::all_of(cols_num), parsear_censo)) |>
    dplyr::mutate(
      ENTIDAD = formato_clave(.data$ENTIDAD, 2),
      .cve_mun = formato_clave(.data$MUN, 3),
      .cve_loc = formato_clave(.data$LOC, 4),
      .cve_ageb = formato_ageb(.data$AGEB),
      .cve_mza = formato_clave(.data$MZA, 3),
      AMBITO = "Urbana",
      tipo_localidad = "Localidad amanzanada",
      tipo = "Localidad amanzanada",
      MUN = paste0(.data$ENTIDAD, .data$.cve_mun),
      LOC = paste0(.data$MUN, .data$.cve_loc),
      AGEB = paste0(.data$LOC, .data$.cve_ageb),
      MZA = paste0(.data$AGEB, .data$.cve_mza),
      ARLU = paste0(.data$LOC, "-LOC-Urbana"),
      AULR = paste0(.data$AGEB, "-AGEB-Urbana")
    ) |>
    dplyr::select(-dplyr::starts_with(".cve_")) |>
    tibble::rownames_to_column("id")

  validar_ancho_llave(marco$AGEB, 13, "AGEB")
  validar_ancho_llave(marco$MZA, 16, "manzana")
  if (anyDuplicated(marco$MZA) > 0) {
    stop("Hay llaves de manzana duplicadas en el censo (",
         sum(duplicated(marco$MZA)), "): revisa el insumo.", call. = FALSE)
  }
  marco
}
