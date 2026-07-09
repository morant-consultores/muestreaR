# Etapa II: manzanas con PPT dentro de cada UPM del plan -------------------

#' Seleccionar manzanas dentro de cada UPM del plan (Etapa II)
#'
#' Para cada UPM sorteada del plan ([planear_muestra_upm()] o sus
#' envolturas) selecciona `manzanas_por_upm` manzanas con probabilidad
#' proporcional al tamaño ([seleccionar_pps()]) y reparte el `n_plan` y los
#' `contactos` de la UPM entre sus manzanas ([repartir_cociente()]). Las
#' `pi_manzana` se calculan sobre TODAS las manzanas de la UPM (las
#' probabilidades del diseño, no una renormalización). Es la Etapa II del
#' diseño por conglomerados censales (p. ej. 2 manzanas por AGEB, espejo
#' de la nota de Enkoll); la Etapa III (viviendas, sistemático con
#' arranque aleatorio) es operación de campo dimensionada por el
#' `n_plan`/`contactos` por manzana.
#'
#' Casos límite, siempre declarados:
#' - UPM con menos manzanas que las pedidas: entran todas con `pi = 1`
#'   (warning agregado; el plan completo de la UPM se reparte entre ellas).
#' - UPM con todas sus manzanas sin tamaño (enmascaramiento INEGI `*`):
#'   sorteo equiprobable dentro de la UPM (message agregado).
#' - UPM del plan sin manzanas en el marco: error con las llaves (un
#'   listado de campo hueco no debe salir en silencio).
#'
#' @param plan Plan muestral versionado (una fila por UPM sorteada), con
#'   atributo `"unidad"` o `unidad` explícita.
#' @param marco_manzanas Marco de manzanas (de [construir_marco_manzanas()]
#'   o equivalente), con la columna de la unidad y la llave de manzana.
#' @param manzanas_por_upm Manzanas a seleccionar en cada UPM (default 2,
#'   el modelo operativo).
#' @param variable_tamano Medida de tamaño para el PPT (default `"pobtot"`;
#'   la población total de manzana está menos enmascarada que la adulta).
#' @param llave_manzana Columna que identifica la manzana.
#' @param unidad Unidad del plan; por default se lee del atributo.
#' @param semilla Semilla para reproducibilidad del sorteo.
#'
#' @return `tibble` con una fila por manzana seleccionada: las columnas del
#'   marco de manzanas más `pi_manzana`, `n_plan` y `contactos` (repartidos
#'   por manzana; suman lo del plan en cada UPM). Atributos: `"unidad"` y
#'   `"parametros"`.
#' @export
seleccionar_manzanas <- function(plan, marco_manzanas,
                                 manzanas_por_upm = 2,
                                 variable_tamano = "pobtot",
                                 llave_manzana = "manzana",
                                 unidad = NULL,
                                 semilla = NULL) {
  if (is.null(unidad)) unidad <- attr(plan, "unidad")
  if (is.null(unidad)) unidad <- "seccion"
  if (!unidad %in% names(plan)) {
    stop("El plan no trae la columna de su unidad (`", unidad, "`).",
         call. = FALSE)
  }
  for (col in c(unidad, llave_manzana, variable_tamano)) {
    if (!col %in% names(marco_manzanas)) {
      stop("No existe la columna `", col, "` en el marco de manzanas.",
           call. = FALSE)
    }
  }
  requeridas <- c("n_plan", "contactos")
  faltan <- setdiff(requeridas, names(plan))
  if (length(faltan) > 0) {
    stop("Al plan le faltan columnas: ", paste(faltan, collapse = ", "),
         call. = FALSE)
  }

  sin_manzanas <- setdiff(plan[[unidad]], marco_manzanas[[unidad]])
  if (length(sin_manzanas) > 0) {
    stop(length(sin_manzanas), " UPM(s) del plan sin manzanas en el marco (",
         paste(utils::head(sin_manzanas, 5), collapse = ", "),
         if (length(sin_manzanas) > 5) ", ..." else "",
         "): empata las llaves antes de la Etapa II.", call. = FALSE)
  }

  if (!is.null(semilla)) set.seed(semilla)
  cortas <- character(0)
  equiprobables <- character(0)

  res <- lapply(seq_len(nrow(plan)), function(i) {
    fila <- plan[i, , drop = FALSE]
    llave_i <- fila[[unidad]]
    mzas <- marco_manzanas[marco_manzanas[[unidad]] == llave_i, , drop = FALSE]

    k <- min(manzanas_por_upm, nrow(mzas))
    if (nrow(mzas) < manzanas_por_upm) {
      cortas <<- c(cortas, llave_i)
    }

    tam <- mzas[[variable_tamano]]
    tam[is.na(tam)] <- 0
    if (all(tam <= 0)) {
      equiprobables <<- c(equiprobables, llave_i)
      tam <- rep(1, nrow(mzas))
    }
    mzas$.tam_pps <- tam
    # pi del diseño: sobre TODAS las manzanas de la UPM, antes de sortear
    mzas$pi_manzana <- sampling::inclusionprobabilities(tam, k)

    sel <- seleccionar_pps(mzas, n = k, variable_tamano = ".tam_pps")
    sel$n_plan <- repartir_cociente(fila$n_plan, rep(fila$n_plan / k, k))
    sel$contactos <- repartir_cociente(fila$contactos,
                                       rep(fila$contactos / k, k))
    sel$.tam_pps <- NULL
    sel
  }) |>
    dplyr::bind_rows() |>
    dplyr::relocate(dplyr::all_of(c(unidad, llave_manzana)))

  if (length(cortas) > 0) {
    warning(length(cortas), " UPM(s) con menos manzanas que las pedidas (",
            "entran todas con pi = 1): ",
            paste(utils::head(cortas, 5), collapse = ", "),
            if (length(cortas) > 5) ", ..." else "", call. = FALSE)
  }
  if (length(equiprobables) > 0) {
    message(length(equiprobables), " UPM(s) con todas sus manzanas sin ",
            "tamaño (enmascaramiento INEGI): sorteo equiprobable ahí. ",
            "Declararlo en la nota metodológica.")
  }

  attr(res, "unidad") <- unidad
  attr(res, "parametros") <- list(
    manzanas_por_upm = manzanas_por_upm,
    variable_tamano = variable_tamano,
    semilla = semilla
  )
  res
}
