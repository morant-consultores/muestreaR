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

  if (anyDuplicated(marco$manzana) > 0) {
    stop("Hay llaves de manzana duplicadas en el censo (",
         sum(duplicated(marco$manzana)), "): revisa el insumo.",
         call. = FALSE)
  }
  marco
}
