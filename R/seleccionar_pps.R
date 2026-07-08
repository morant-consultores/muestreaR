#' Selección proporcional al tamaño consistente con las pi declaradas
#'
#' Selecciona `n` unidades del marco con probabilidades de inclusión de
#' primer orden EXACTAMENTE iguales a las que declara
#' [sampling::inclusionprobabilities()] — las mismas que `calcular_fpc()`
#' guarda como `fpc_*` y que alimentan los pesos `1/pi` del estimador
#' Horvitz-Thompson en encuestar.
#'
#' Usa muestreo sistemático aleatorizado ([sampling::UPrandomsystematic()]),
#' cuyo diseño cumple pi_i = pik por construcción; las unidades topadas en
#' pi = 1 se incluyen con certeza y las de tamaño 0 nunca se seleccionan.
#'
#' Reemplaza al mecanismo anterior (`dplyr::slice_sample(weight_by = )`,
#' muestreo sucesivo proporcional al tamaño), cuyas probabilidades de
#' inclusión NO son n*p_i (Rosén, 1997): con tamaños desiguales las
#' unidades grandes quedaban sub-representadas respecto a las pi
#' declaradas (una unidad topada en pi = 1 podía quedar fuera ~10% de las
#' veces) y los pesos 1/pi resultaban sesgados.
#'
#' @param bd Marco (tibble) con una fila por unidad a sortear.
#' @param n Número de unidades a seleccionar.
#' @param variable_tamano Columna con la medida de tamaño (default
#'   `"total"`, la suma poblacional que construye `muestrear()`).
#' @return Las filas de `bd` seleccionadas (todas si `n >= nrow(bd)`).
#' @export
seleccionar_pps <- function(bd, n, variable_tamano = "total") {
  # con n no entero, sum(pik) no es entero y el sistemático devolvería
  # floor(n) o ceiling(n) al azar: la garantía "exactamente n" se
  # rompería en silencio (alcanzable vía criterio = "manual" o tablas de
  # sobremuestra sin redondear)
  if (length(n) != 1 || is.na(n) || n != round(n)) {
    stop("`n` debe ser un entero (recibido: ", paste(n, collapse = ", "), ").")
  }
  if (n <= 0) {
    return(bd[0, , drop = FALSE])
  }
  if (n >= nrow(bd)) {
    return(bd)
  }
  tamano <- bd[[variable_tamano]]
  if (is.null(tamano)) {
    stop("No existe la columna de tamaño `", variable_tamano, "` en el marco.")
  }
  if (all(tamano <= 0, na.rm = TRUE)) {
    stop(
      "Todas las unidades tienen tamaño 0 o NA en `", variable_tamano,
      "`: no hay nada que sortear con PPS."
    )
  }
  tamano[is.na(tamano)] <- 0

  pik <- sampling::inclusionprobabilities(tamano, n)
  seleccion <- sampling::UPrandomsystematic(pik)
  bd[seleccion == 1, , drop = FALSE]
}
