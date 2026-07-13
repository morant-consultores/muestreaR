# Puente del plan por UPM al contrato seccional de encuestar ---------------

#' Adaptar un plan por UPM al contrato de encuestar::construir_diseno_capas
#'
#' `encuestar::construir_diseno_capas()` (capa 1 = selección planeada)
#' espera las columnas del plan seccional: `seccion`, `pi_seccion`,
#' `n_plan` y opcionalmente `ln_seccion`. Mientras encuestar hable
#' "seccional", esta función renombra un plan de cualquier unidad
#' ([planear_muestra_upm()], [planear_muestra_ageb()]) a ese contrato:
#' `seccion` pasa a ser la llave de la UPM que sea (p. ej. el CVEGEO del
#' AGEB). El snapshot debe traer esa misma llave en la columna que se
#' declara en el argumento `seccion` de `construir_diseno_capas()`.
#'
#' Sobre un plan ya seccional (o sin atributo `"unidad"`, como los planes
#' versionados antes de este cambio) es la identidad.
#'
#' @param plan Plan muestral versionado (de [planear_muestra_upm()] o sus
#'   envolturas).
#' @param unidad Unidad del plan; por default se lee del atributo
#'   `"unidad"` y, si no existe, se asume `"seccion"`.
#'
#' @return El plan con las columnas del contrato seccional (`seccion`,
#'   `pi_seccion` y, si existe la medida de tamaño, `ln_seccion`),
#'   conservando los demás atributos y agregando `"unidad_original"`.
#' @export
plan_para_capas <- function(plan, unidad = NULL) {
  if (is.null(unidad)) unidad <- attr(plan, "unidad")
  if (is.null(unidad)) unidad <- "seccion"
  if (identical(unidad, "seccion")) {
    return(plan)
  }

  requeridas <- c(unidad, paste0("pi_", unidad))
  faltan <- setdiff(requeridas, names(plan))
  if (length(faltan) > 0) {
    stop("Al plan le faltan columnas de la unidad `", unidad, "`: ",
         paste(faltan, collapse = ", "), call. = FALSE)
  }

  atributos <- attributes(plan)
  res <- plan |>
    dplyr::rename(seccion = dplyr::all_of(unidad),
                  pi_seccion = dplyr::all_of(paste0("pi_", unidad)))
  if (paste0("ln_", unidad) %in% names(res)) {
    res <- res |>
      dplyr::rename(ln_seccion = dplyr::all_of(paste0("ln_", unidad)))
  }

  # conservar los atributos del plan (asignacion, dominios, lista_negra,
  # parametros); names/row.names ya vienen del rename
  for (a in setdiff(names(atributos), c("names", "row.names", "class"))) {
    attr(res, a) <- atributos[[a]]
  }
  attr(res, "unidad") <- "seccion"
  attr(res, "unidad_original") <- unidad
  res
}
