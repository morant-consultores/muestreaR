#' Derivar la asignación de la muestra desde el modelo operativo
#'
#' Traduce los objetivos de entrevistas por estrato a la cantidad de secciones,
#' manzanas por sección y entrevistas a levantar, aplicando el modelo operativo
#' (entrevistas por manzana, manzanas por sección) y el ajuste por rechazo.
#'
#' El número de secciones se determina por las entrevistas **efectivas** objetivo
#' (`entrevistas / (n_0 * manzanas_por_seccion)`). El ajuste por rechazo
#' (`factor = 1 / (1 - tasa_rechazo)`) se absorbe según `modo_rechazo`:
#' inflando las manzanas por sección (sin dispersar la muestra) o inflando las
#' secciones (útil en estratos pequeños).
#'
#' @param estratos `data.frame` con una fila por estrato. Columnas: `estrato`
#'   (valor del estrato), `entrevistas` (entrevistas efectivas objetivo) y,
#'   opcionalmente, `tasa_rechazo` (rechazo por estrato; si falta se usa el
#'   argumento `tasa_rechazo`).
#' @param n_0 Entrevistas por manzana (por defecto 5).
#' @param manzanas_por_seccion Manzanas por sección base (por defecto 2).
#' @param tasa_rechazo Tasa de rechazo global en \[0, 1) (por defecto 0). Se usa
#'   cuando `estratos` no trae una columna `tasa_rechazo`.
#' @param modo_rechazo `"manzanas"` (default: infla las manzanas por sección,
#'   secciones fijas) o `"secciones"` (infla las secciones, manzanas fijas).
#'
#' @return `tibble` con una fila por estrato y columnas: `estrato`, `entrevistas`
#'   (objetivo efectivo), `tasa_rechazo`, `factor`, `secciones`,
#'   `manzanas_por_seccion`, `entrevistas_a_levantar`.
#' @export
#' @examples
#' estratos <- data.frame(estrato = c("Toluca", "Resto"),
#'                        entrevistas = c(300, 900))
#' calcular_asignacion(estratos, tasa_rechazo = 0.5)
calcular_asignacion <- function(estratos,
                                n_0 = 5,
                                manzanas_por_seccion = 2,
                                tasa_rechazo = 0,
                                modo_rechazo = c("manzanas", "secciones")) {
  modo_rechazo <- match.arg(modo_rechazo)
  estratos <- tibble::as_tibble(estratos)

  # tasa por estrato si existe la columna; si no, el escalar global
  tasa <- if ("tasa_rechazo" %in% names(estratos)) estratos$tasa_rechazo else tasa_rechazo
  tasa <- ifelse(is.na(tasa), tasa_rechazo, tasa)
  if (any(tasa < 0 | tasa >= 1)) {
    stop("`tasa_rechazo` debe estar en el intervalo [0, 1).")
  }

  factor <- 1 / (1 - tasa)
  # secciones base por el objetivo efectivo
  secciones_base <- estratos$entrevistas / (n_0 * manzanas_por_seccion)

  if (modo_rechazo == "manzanas") {
    secciones <- round(secciones_base)
    mps       <- round(manzanas_por_seccion * factor)
  } else {
    secciones <- round(secciones_base * factor)
    mps       <- rep(manzanas_por_seccion, length(secciones))
  }

  tibble::tibble(
    estrato                = estratos$estrato,
    entrevistas            = estratos$entrevistas,
    tasa_rechazo           = tasa,
    factor                 = factor,
    secciones              = secciones,
    manzanas_por_seccion   = mps,
    entrevistas_a_levantar = secciones * n_0 * mps
  )
}

#' Validar la tabla de estratos de un diseño
#'
#' Comprueba que la tabla de estratos y los parámetros del modelo operativo sean
#' coherentes con el marco muestral, **acumulando** todos los problemas (no sólo
#' el primero) para que puedan mostrarse juntos (p. ej. en una interfaz o skill).
#'
#' @param poblacion Objeto `PoblacionINE` (o equivalente con `$marco_muestral`).
#' @param estratos `data.frame` de estratos (ver [calcular_asignacion()]).
#' @param variable_estrato Columna del marco que define el estrato.
#' @param variable_cluster Columna del conglomerado (último nivel); si se da, se
#'   verifica que las secciones requeridas no excedan las disponibles por estrato.
#' @param n_0,manzanas_por_seccion,tasa_rechazo,modo_rechazo Parámetros del modelo
#'   operativo (ver [calcular_asignacion()]).
#'
#' @return Vector de caracteres con los problemas encontrados (longitud 0 si la
#'   especificación es válida). No lanza error.
#' @export
validar_estratos <- function(poblacion, estratos,
                             variable_estrato = "region",
                             variable_cluster = "SECCION",
                             n_0 = 5,
                             manzanas_por_seccion = 2,
                             tasa_rechazo = 0,
                             modo_rechazo = c("manzanas", "secciones")) {
  problemas <- character(0)
  marco <- poblacion$marco_muestral

  # estructura de la tabla
  if (!all(c("estrato", "entrevistas") %in% names(estratos))) {
    problemas <- c(problemas,
                   "`estratos` debe tener al menos las columnas `estrato` y `entrevistas`.")
    return(problemas)   # sin estas columnas no se puede seguir validando
  }
  if (!is.numeric(estratos$entrevistas) || any(estratos$entrevistas <= 0, na.rm = TRUE)) {
    problemas <- c(problemas, "`entrevistas` debe ser numérica y positiva en todos los estratos.")
  }
  if (anyDuplicated(estratos$estrato)) {
    problemas <- c(problemas, "Hay estratos duplicados en la tabla.")
  }

  # tasa de rechazo
  tasa <- if ("tasa_rechazo" %in% names(estratos)) estratos$tasa_rechazo else tasa_rechazo
  if (any(tasa < 0 | tasa >= 1, na.rm = TRUE)) {
    problemas <- c(problemas, "`tasa_rechazo` debe estar en el intervalo [0, 1).")
  }

  # variable de estrato en el marco
  if (!variable_estrato %in% names(marco)) {
    problemas <- c(problemas,
                   sprintf("La variable de estrato '%s' no existe en el marco muestral.",
                           variable_estrato))
  } else {
    faltan <- setdiff(as.character(estratos$estrato), as.character(unique(marco[[variable_estrato]])))
    if (length(faltan)) {
      problemas <- c(problemas,
                     sprintf("Estratos que no existen en el marco ('%s'): %s.",
                             variable_estrato, paste(faltan, collapse = ", ")))
    }
  }

  # secciones requeridas vs disponibles (si se conoce el cluster y todo lo previo va bien)
  if (length(problemas) == 0 && variable_cluster %in% names(marco) &&
      variable_estrato %in% names(marco)) {
    asig <- calcular_asignacion(estratos, n_0, manzanas_por_seccion, tasa_rechazo, modo_rechazo)
    disp <- marco |>
      dplyr::distinct(.data[[variable_estrato]], .data[[variable_cluster]]) |>
      dplyr::count(.data[[variable_estrato]], name = "disponibles")
    names(disp)[1] <- "estrato"
    chequeo <- dplyr::left_join(asig, disp, by = "estrato")
    excede <- chequeo[which(chequeo$secciones > chequeo$disponibles), ]
    if (nrow(excede)) {
      problemas <- c(problemas, sprintf(
        "Las secciones requeridas exceden las disponibles en: %s.",
        paste(sprintf("%s (%d > %d)", excede$estrato, excede$secciones, excede$disponibles),
              collapse = ", ")))
    }
  }

  problemas
}
