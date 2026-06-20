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

#' Diseñar una muestra polietápica de forma declarativa (marco electoral INE)
#'
#' Receta de alto nivel que construye una muestra polietápica a partir de una
#' tabla de estratos y los parámetros del modelo operativo, ejecutando todo el
#' pipeline en el orden correcto (niveles, plan, fpc, extracción, cuotas) sin que
#' el usuario tenga que llamar a los métodos de bajo nivel ni recordar el orden.
#'
#' Soporta **asignación desproporcionada** (distinto número de entrevistas por
#' estrato) y **ajuste por rechazo** (ver [calcular_asignacion()]). Es aditiva:
#' usa internamente los métodos de [DiseñoINE] sin modificarlos.
#'
#' @param poblacion Objeto `PoblacionINE` (o equivalente con `$marco_muestral`).
#' @param estratos `data.frame` de estratos: columnas `estrato`, `entrevistas` y,
#'   opcionalmente, `tasa_rechazo` (ver [calcular_asignacion()]).
#' @param variable_estrato Columna del marco que define el estrato (nivel 1).
#' @param variable_cluster Columna del conglomerado / último nivel.
#' @param variable_poblacional Columna de tamaño para el muestreo proporcional.
#' @param id_unidad Columna que identifica la unidad mínima (manzana).
#' @param n_0,manzanas_por_seccion,tasa_rechazo,modo_rechazo Parámetros del modelo
#'   operativo (ver [calcular_asignacion()]).
#' @param semilla Semilla para reproducibilidad (ver [DiseñoINE]).
#' @param calcular_cuotas `logical`. Si es `TRUE`, calcula las cuotas.
#' @param ajustar_cuotas `logical`. Se pasa a `calcular_cuotas()`.
#' @param validar `logical`. Si es `TRUE`, valida con [validar_estratos()] y
#'   aborta con un mensaje si hay problemas.
#'
#' @return El objeto [DiseñoINE] con la muestra extraída (y cuotas), con la
#'   asignación derivada adjunta como atributo `"asignacion"`.
#' @export
disenar_muestra_ine <- function(poblacion, estratos,
                                variable_estrato = "region",
                                variable_cluster = "SECCION",
                                variable_poblacional = "lista_nominal",
                                id_unidad = "id",
                                n_0 = 5,
                                manzanas_por_seccion = 2,
                                tasa_rechazo = 0,
                                modo_rechazo = c("manzanas", "secciones"),
                                semilla = NULL,
                                calcular_cuotas = TRUE,
                                ajustar_cuotas = TRUE,
                                validar = TRUE) {
  modo_rechazo <- match.arg(modo_rechazo)

  if (validar) {
    problemas <- validar_estratos(poblacion, estratos, variable_estrato, variable_cluster,
                                  n_0, manzanas_por_seccion, tasa_rechazo, modo_rechazo)
    if (length(problemas)) {
      stop("Especificación de muestra inválida:\n- ", paste(problemas, collapse = "\n- "),
           call. = FALSE)
    }
  }

  asig <- calcular_asignacion(estratos, n_0, manzanas_por_seccion, tasa_rechazo, modo_rechazo)

  diseno <- DiseñoINE$new(
    poblacion            = poblacion,
    n                    = sum(asig$entrevistas_a_levantar),
    n_0                  = n_0,
    variable_poblacional = variable_poblacional,
    unidad_muestreo      = "Manzanas",
    id_unidad_muestreo   = id_unidad,
    llave_muestreo       = "Man",
    semilla              = semilla
  )
  diseno$agregar_nivel(variable_estrato, tipo = "strata",
                       descripcion = variable_estrato, llave = variable_estrato)
  diseno$agregar_nivel(variable_cluster, tipo = "cluster",
                       descripcion = variable_cluster, llave = variable_cluster)

  # Mapear la asignación (por nombre de estrato) al orden interno de strata_1.
  orden <- diseno$poblacion$marco_muestral |>
    dplyr::distinct(.data[[variable_estrato]], strata_1) |>
    dplyr::arrange(strata_1)
  idx <- match(as.character(orden[[variable_estrato]]), as.character(asig$estrato))

  # Nivel 1 (estrato): se fijan las secciones manualmente y se sobrescribe el
  # número de entrevistas por estrato con el objetivo (en vez de proporcional a
  # población). Con m_1 y n_1 fijados, el último nivel deriva las manzanas/sección.
  diseno$plan_muestra(nivel = 1, criterio = "manual", manual = asig$secciones[idx])
  diseno$n_i$strata_1$n_1 <- asig$entrevistas_a_levantar[idx][
    match(diseno$n_i$strata_1$strata_1, orden$strata_1)]

  diseno$plan_muestra(nivel = 2)   # último nivel
  diseno$fpc(nivel = 2)
  diseno$fpc(nivel = 0)
  diseno$extraer_muestra(nivel = 1)
  diseno$extraer_muestra(nivel = 2)
  if (calcular_cuotas) diseno$calcular_cuotas(ajustar = ajustar_cuotas)

  attr(diseno, "asignacion") <- asig
  diseno
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
