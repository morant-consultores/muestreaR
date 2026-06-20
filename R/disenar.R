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
