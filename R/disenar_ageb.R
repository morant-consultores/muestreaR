# Declarativa del flujo de la CLASE para el diseño por AGEB ----------------

#' Diseñar una muestra polietápica por AGEB de forma declarativa (clase)
#'
#' Receta de alto nivel del **flujo de la clase** para el marco censal por
#' AGEB: construye el [Diseño] completo (niveles estrato + AGEB, plan,
#' fpc, extracción y cuotas censales) a partir de una tabla de estratos y
#' el modelo operativo, igual que [disenar_muestra_ine()] para el marco
#' electoral. El objeto resultante es el que el equipo ya opera: la
#' población vive adentro, `diseno$exportar(cartografia)` escribe
#' `diseño.rda` + `shp.rda` + `cuotas.csv` + los mapas de Google por AGEB
#' (instrucciones del encuestador), y es el insumo de AppAuditoria y de
#' `encuestar` (`tipo_encuesta = "inegi"`).
#'
#' El ajuste por rechazo es el del modelo operativo del equipo:
#' `modo_rechazo = "manzanas"` infla las manzanas por AGEB (p. ej. 2 -> 4
#' con tasa 0.5) manteniendo `n_0` entrevistas por manzana y los AGEBs
#' fijos; `"agebs"` infla los AGEBs con las manzanas fijas.
#'
#' Además adjunta el **plan muestral versionado** ([derivar_plan_ageb()],
#' atributo `"plan_ageb"`): el contrato de la capa 1 de
#' `encuestar::construir_diseno_capas()`, derivado del MISMO sorteo de la
#' clase (cero inconsistencia entre el diseño operativo y el de pesos).
#'
#' @param poblacion Objeto [PoblacionAGEB] (o equivalente con
#'   `$marco_muestral` de [crear_mm_ageb()]).
#' @param estratos `data.frame` de estratos: columnas `estrato`,
#'   `entrevistas` (efectivas objetivo) y opcionalmente `tasa_rechazo`
#'   (ver [calcular_asignacion()]).
#' @param variable_estrato Columna del marco que define el estrato
#'   (nivel 1). Para el espejo sin estratificar, una columna constante.
#' @param variable_cluster Columna del conglomerado (default `"AGEB"`).
#' @param variable_poblacional Columna de tamaño para el PPS (default
#'   `"P_18YMAS"`).
#' @param id_unidad Columna que identifica la unidad mínima (manzana).
#' @param n_0 Entrevistas por manzana (default 5).
#' @param manzanas_por_ageb Manzanas por AGEB base (default 2).
#' @param tasa_rechazo Tasa de rechazo global en `[0, 1)`.
#' @param modo_rechazo `"manzanas"` (default) o `"agebs"`.
#' @param semilla Semilla para reproducibilidad.
#' @param calcular_cuotas `logical`. Cuotas censales de edad/sexo por AGEB
#'   (default `TRUE`: son las instrucciones que [google_maps()] imprime en
#'   los mapas de campo; el espejo tipo Enkoll también las usa).
#' @param validar `logical`. Validar con [validar_estratos()].
#'
#' @return El objeto [Diseño] con la muestra extraída, la asignación como
#'   atributo `"asignacion"` y el plan versionado como `"plan_ageb"`.
#' @export
disenar_muestra_ageb <- function(poblacion, estratos,
                                 variable_estrato = "region",
                                 variable_cluster = "AGEB",
                                 variable_poblacional = "P_18YMAS",
                                 id_unidad = "id",
                                 n_0 = 5,
                                 manzanas_por_ageb = 2,
                                 tasa_rechazo = 0,
                                 modo_rechazo = c("manzanas", "agebs"),
                                 semilla = NULL,
                                 calcular_cuotas = TRUE,
                                 validar = TRUE) {
  modo_rechazo <- match.arg(modo_rechazo)
  # calcular_asignacion habla "secciones": su columna `secciones` son los
  # AGEBs a sortear y `manzanas_por_seccion` las manzanas por AGEB
  modo_interno <- if (identical(modo_rechazo, "agebs")) "secciones" else "manzanas"

  if (calcular_cuotas) {
    # cuotas() truena críptico ("object 'P_18A24_F' not found") si el marco
    # no trae el desglose censal; se valida aquí con un error accionable
    demograficas <- c("P_18A24_F", "P_18A24_M", "P_18YMAS_F", "P_18YMAS_M",
                      "P_60YMAS_F", "P_60YMAS_M")
    faltan_demo <- setdiff(demograficas, names(poblacion$marco_muestral))
    if (length(faltan_demo) > 0) {
      stop("El marco no trae el bloque demográfico del censo para las ",
           "cuotas (faltan: ", paste(faltan_demo, collapse = ", "), "). ",
           "Construye la población con crear_mm_ageb() desde el dataset ",
           "ageb_mza_urbana completo o usa calcular_cuotas = FALSE.",
           call. = FALSE)
    }
  }

  if (validar) {
    problemas <- validar_estratos(poblacion, estratos, variable_estrato,
                                  variable_cluster, n_0, manzanas_por_ageb,
                                  tasa_rechazo, modo_interno)
    if (length(problemas)) {
      stop("Especificación de muestra inválida:\n- ",
           paste(problemas, collapse = "\n- "), call. = FALSE)
    }
  }

  asig <- calcular_asignacion(estratos, n_0, manzanas_por_ageb,
                              tasa_rechazo, modo_interno)

  diseno <- Diseño$new(
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
  idx <- match(as.character(orden[[variable_estrato]]),
               as.character(asig$estrato))

  # Nivel 1 (estrato): AGEBs fijados manualmente y n por estrato = objetivo
  # a levantar (no proporcional a población). El último nivel deriva las
  # manzanas por AGEB.
  diseno$plan_muestra(nivel = 1, criterio = "manual",
                      manual = asig$secciones[idx])
  diseno$n_i$strata_1$n_1 <- asig$entrevistas_a_levantar[idx][
    match(diseno$n_i$strata_1$strata_1, orden$strata_1)]

  diseno$plan_muestra(nivel = 2)   # último nivel
  diseno$fpc(nivel = 2)
  diseno$fpc(nivel = 0)
  diseno$extraer_muestra(nivel = 1)
  diseno$extraer_muestra(nivel = 2)
  if (calcular_cuotas) diseno$calcular_cuotas()

  attr(diseno, "asignacion") <- asig
  attr(diseno, "plan_ageb") <- derivar_plan_ageb(diseno, asignacion = asig)
  diseno
}

#' Derivar el plan muestral versionado desde el diseño de la clase
#'
#' Reconstruye el **plan versionado por AGEB** (el contrato de la capa 1 de
#' `encuestar::construir_diseno_capas()`, ver [planear_muestra_ageb()])
#' desde un [Diseño] de la clase con la muestra ya extraída: las `pi_ageb`
#' son exactamente las del sorteo (el `fpc` del nivel AGEB, calculado
#' sobre el marco completo del estrato) — un solo sorteo, cero
#' inconsistencia entre el diseño operativo y el de pesos. El plan
#' resultante alimenta [plan_para_capas()] y [planear_siguiente_ola()].
#'
#' @param diseno Objeto [Diseño] con niveles estrato + AGEB, `fpc`
#'   calculado y muestra extraída (típicamente de
#'   [disenar_muestra_ageb()]).
#' @param asignacion Tabla de [calcular_asignacion()]; por default se lee
#'   del atributo `"asignacion"` del diseño.
#'
#' @return Plan versionado: `tibble` con `ageb`, `estrato`, `ln_ageb`
#'   (tamaño del AGEB en la variable poblacional), `pi_ageb`, `n_plan`
#'   (efectivas objetivo por AGEB) y `contactos` (entrevistas a levantar
#'   por AGEB). Atributos `"unidad"` y `"parametros"`.
#' @export
derivar_plan_ageb <- function(diseno, asignacion = attr(diseno, "asignacion")) {
  if (is.null(asignacion)) {
    stop("El diseño no trae la asignación del modelo operativo: pásala en ",
         "`asignacion` o usa disenar_muestra_ageb().", call. = FALSE)
  }
  niveles <- diseno$niveles
  var_estrato <- niveles$variable[niveles$nivel == 1]
  var_ageb <- niveles$variable[niveles$nivel == diseno$ultimo_nivel]
  col_fpc <- paste0("fpc_", diseno$ultimo_nivel)
  marco <- diseno$poblacion$marco_muestral
  if (!col_fpc %in% names(marco)) {
    stop("El marco no trae `", col_fpc, "`: corre diseno$fpc(nivel = ",
         diseno$ultimo_nivel, ") antes de derivar el plan.", call. = FALSE)
  }
  if (is.null(diseno$muestra)) {
    stop("El diseño no tiene muestra extraída.", call. = FALSE)
  }

  ult <- diseno$muestra |>
    purrr::pluck(length(diseno$muestra)) |>
    tidyr::unnest(data)
  agebs_sel <- unique(ult[[var_ageb]])

  info <- marco |>
    dplyr::filter(.data[[var_ageb]] %in% agebs_sel) |>
    dplyr::group_by(.data[[var_estrato]], .data[[var_ageb]]) |>
    dplyr::summarise(
      ln_ageb = sum(.data[[diseno$variable_poblacional]], na.rm = TRUE),
      pi_ageb = unique(.data[[col_fpc]]),
      .groups = "drop"
    )
  names(info)[1:2] <- c("estrato", "ageb")

  dosis <- asignacion |>
    dplyr::transmute(
      estrato = as.character(estrato),
      n_plan = round(entrevistas / secciones),
      contactos = round(entrevistas_a_levantar / secciones)
    )

  plan <- info |>
    dplyr::mutate(estrato = as.character(estrato)) |>
    dplyr::left_join(dosis, by = "estrato") |>
    dplyr::relocate(ageb, estrato, ln_ageb, pi_ageb, n_plan, contactos)

  attr(plan, "unidad") <- "ageb"
  attr(plan, "parametros") <- list(
    n_total = sum(asignacion$entrevistas),
    n_0 = diseno$n_0,
    tasa_rechazo = unique(asignacion$tasa_rechazo),
    semilla = diseno$semilla,
    unidad = "ageb",
    origen = "clase"
  )
  plan
}
