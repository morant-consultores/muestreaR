# Muestra seccional por estratos electorales -----------------------------
#
# Flujo de la arquitectura de pesos por capas (ver encuestar): el sorteo
# entrega un PLAN VERSIONADO por sección (seccion, pi_seccion, ln_seccion,
# n_plan, contactos) que es el contrato con encuestar::construir_diseno_capas
# (capa 1 = selección planeada). La no respuesta NO se resuelve sustituyendo:
# se dimensiona con sobremuestra (contactos) y se corrige en las capas.
#
# La sección es UNA unidad primaria de muestreo (UPM) posible; el mismo flujo
# opera con AGEBs del marco censal INEGI (ver planear_muestra_upm() y
# planear_muestra_ageb() en R/plan_upm.R). El parámetro `unidad` de las
# funciones de este archivo solo controla cómo se NOMBRAN columnas y
# mensajes; la matemática es idéntica.

# plural de la unidad para nombrar columnas de conteo ("seccion" es el único
# plural irregular que manejamos; el resto agrega "s": ageb -> agebs)
plural_unidad <- function(unidad) {
  if (identical(unidad, "seccion")) "secciones" else paste0(unidad, "s")
}

#' Estratificar secciones por tipo electoral y región
#'
#' Clasifica cada sección por su comportamiento electoral histórico usando
#' cortes del margen de victoria neto (MVN) y forma el estrato como la
#' interacción **región de interés × tipo electoral**. Este criterio
#' minimiza la varianza dentro del estrato y la maximiza entre estratos
#' (secciones de voto parecido se parecen entre sí más que las de un mismo
#' ámbito urbano/rural).
#'
#' @param marco `tibble` del marco seccional; una fila por sección.
#' @param mvn Nombre de la columna con el margen de victoria neto
#'   (oficialista − opositor, normalizado).
#' @param region Nombre de la columna con la región de interés del cliente.
#' @param cortes Vector numérico decreciente de 3 cortes del MVN:
#'   `> cortes[1]` = duro oficialista, `> cortes[2]` = blando,
#'   `>= cortes[3]` = competitiva, `< cortes[3]` = opositor.
#' @param etiquetas Etiquetas de los 4 tipos, en el mismo orden.
#'
#' @return El marco con dos columnas nuevas: `tipo_electoral` y `estrato`
#'   (`region / tipo_electoral`). Las secciones sin MVN quedan con estrato
#'   `NA` (se excluyen del sorteo con aviso en [planear_muestra_seccional()]).
#' @export
#' @examples
#' marco <- tibble::tibble(region = "Capital", lista_nominal = 1000,
#'                         margen_victoria_neto = c(0.2, -0.1))
#' estratificar_electoral(marco)
estratificar_electoral <- function(marco,
                                   mvn = "margen_victoria_neto",
                                   region = "region",
                                   cortes = c(0.15, 0.05, -0.05),
                                   etiquetas = c("Duro oficialista", "Blando",
                                                 "Competitiva", "Opositor")) {
  for (col in c(mvn, region)) {
    if (!col %in% names(marco)) {
      stop("No existe la columna `", col, "` en el marco.", call. = FALSE)
    }
  }
  if (length(cortes) != 3 || is.unsorted(rev(cortes))) {
    stop("`cortes` debe ser un vector decreciente de 3 valores del MVN.",
         call. = FALSE)
  }

  v <- marco[[mvn]]
  reg <- marco[[region]]
  tipo <- dplyr::case_when(
    is.na(v)        ~ NA_character_,
    v > cortes[1]   ~ etiquetas[1],
    v > cortes[2]   ~ etiquetas[2],
    v >= cortes[3]  ~ etiquetas[3],
    TRUE            ~ etiquetas[4]
  )
  if (anyNA(tipo)) {
    warning(sum(is.na(tipo)), " sección(es) sin MVN quedan sin estrato ",
            "(no entran al sorteo).", call. = FALSE)
  }
  # una región NA también deja sin estrato (si no, paste() serializa el NA
  # como texto y nace un estrato fantasma "NA / ...")
  if (anyNA(reg)) {
    warning(sum(is.na(reg)), " sección(es) sin región quedan sin estrato ",
            "(no entran al sorteo).", call. = FALSE)
  }

  marco |>
    dplyr::mutate(
      tipo_electoral = tipo,
      estrato = ifelse(is.na(tipo) | is.na(reg), NA_character_,
                       paste(reg, tipo, sep = " / "))
    )
}

#' Asignar entrevistas a estratos por el método de la potencia
#'
#' Reparte `n_total` entrevistas entre dominios de interés con la asignación
#' de potencia sobre el tamaño poblacional (`potencia = 1` proporcional,
#' `0.5` raíz-cuadrada, `0` igualitaria) y, dentro de cada dominio,
#' proporcional a la lista nominal de cada estrato. La raíz-cuadrada es el
#' punto medio recomendado cuando el cliente pide leer los dominios chicos
#' sin sacrificar demasiado la precisión estatal.
#'
#' @param marco Marco seccional estratificado (ver [estratificar_electoral()]).
#' @param n_total Entrevistas efectivas objetivo en total.
#' @param m_por_seccion Entrevistas planeadas por sección (6–8 recomendado:
#'   controla el deff de conglomerado `1 + (m-1)·rho`).
#' @param potencia Exponente de la asignación por dominio, en `[0, 1]`.
#' @param dominio Columna del dominio de interés (default `"region"`). `NULL`
#'   aplica la potencia directamente sobre los estratos.
#' @param variable_estrato,variable_tamano Columnas del estrato y del tamaño
#'   (lista nominal en marcos electorales; población adulta en censales).
#' @param min_secciones Mínimo de UPMs por estrato (default 2: con una
#'   sola la varianza del estrato no es estimable).
#' @param unidad Nombre de la UPM, solo para nombrar las columnas de conteo
#'   y los mensajes: `"seccion"` (default) produce `secciones` y
#'   `secciones_disponibles`; `"ageb"` produce `agebs` y `agebs_disponibles`.
#'
#' @return `tibble` con una fila por estrato: dominio, `estrato`,
#'   `ln_estrato`, `<unidad en plural>_disponibles`, `entrevistas_obj`,
#'   `<unidad en plural>` (UPMs a sortear), `entrevistas_plan`
#'   (= `UPMs * m_por_seccion`). La asignación por dominio va en el
#'   atributo `"dominios"`.
#' @export
asignar_potencia <- function(marco, n_total, m_por_seccion,
                             potencia = 0.5,
                             dominio = "region",
                             variable_estrato = "estrato",
                             variable_tamano = "lista_nominal",
                             min_secciones = 2,
                             unidad = "seccion") {
  if (potencia < 0 || potencia > 1) {
    stop("`potencia` debe estar en [0, 1].", call. = FALSE)
  }
  marco <- marco[!is.na(marco[[variable_estrato]]), ]
  if (is.null(dominio)) {
    dominio <- variable_estrato
  }
  grupos <- unique(c(dominio, variable_estrato))

  # nivel dominio: potencia sobre el tamaño
  doms <- marco |>
    dplyr::group_by(.data[[dominio]]) |>
    dplyr::summarise(ln_dominio = sum(.data[[variable_tamano]], na.rm = TRUE),
                     .groups = "drop")
  if (any(is.na(doms$ln_dominio) | doms$ln_dominio <= 0)) {
    stop("Dominio(s) sin tamaño poblacional (lista nominal 0 o NA): ",
         paste(doms[[1]][is.na(doms$ln_dominio) | doms$ln_dominio <= 0],
               collapse = ", "), call. = FALSE)
  }
  peso <- doms$ln_dominio^potencia
  doms$entrevistas_dominio <- repartir_cociente(n_total,
                                                n_total * peso / sum(peso))

  # nivel estrato: proporcional a la lista nominal dentro del dominio
  asig <- marco |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grupos))) |>
    dplyr::summarise(
      ln_estrato = sum(.data[[variable_tamano]], na.rm = TRUE),
      secciones_disponibles = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::left_join(doms, by = dominio) |>
    dplyr::group_by(.data[[dominio]]) |>
    dplyr::mutate(
      entrevistas_obj = entrevistas_dominio * ln_estrato / sum(ln_estrato)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      # las disponibles mandan: pedir min_secciones a un estrato de 1 sección
      # rompería el invariante secciones <= disponibles (y el sorteo lo
      # entregaría corto en silencio)
      secciones = pmin(secciones_disponibles,
                       pmax(min_secciones,
                            ceiling(entrevistas_obj / m_por_seccion))),
      entrevistas_plan = secciones * m_por_seccion
    ) |>
    dplyr::select(dplyr::all_of(grupos), ln_estrato,
                  secciones_disponibles, entrevistas_obj, secciones,
                  entrevistas_plan) |>
    dplyr::rename(estrato = !!rlang::sym(variable_estrato))

  pl <- plural_unidad(unidad)
  if (!identical(pl, "secciones")) {
    asig <- asig |>
      dplyr::rename("{pl}" := secciones,
                    "{pl}_disponibles" := secciones_disponibles)
  }

  cortos <- asig[[paste0(pl, "_disponibles")]] < min_secciones
  if (any(cortos)) {
    warning("Estrato(s) con menos de ", min_secciones, " ", pl,
            " disponibles (la varianza no será estimable ahí): ",
            paste(asig$estrato[cortos], collapse = ", "), call. = FALSE)
  }

  attr(asig, "dominios") <- doms
  asig
}

#' Aplicar la lista negra al marco (exclusión documentada, sin sustitución)
#'
#' Excluye del marco las secciones o municipios estructuralmente inoperables
#' ANTES del sorteo, dejando la exclusión documentada para la nota
#' metodológica. Es el reemplazo de la sustitución en campo: lo que no se
#' puede levantar se declara, no se cambia en silencio por otra sección.
#'
#' @param marco Marco de UPMs (seccional o censal).
#' @param secciones Vector de llaves de UPM a excluir (opcional). El nombre
#'   del parámetro es histórico: acepta llaves de la unidad que sea
#'   (secciones, AGEBs, ...).
#' @param municipios Vector de claves de municipio a excluir (opcional).
#' @param variable_municipio,llave_seccion,variable_estrato Nombres de columna.
#' @param min_secciones Umbral para avisar si un estrato queda corto (en UPMs).
#' @param variable_tamano Columna del tamaño con que se documenta y reporta
#'   el porcentaje excluido (default `"lista_nominal"`; en marcos censales,
#'   p. ej. `"pob18"`).
#' @param unidad Nombre de la UPM para la columna llave del documento y los
#'   mensajes (default `"seccion"`).
#'
#' @return El marco filtrado, con el atributo `"lista_negra"`: tibble de las
#'   UPMs excluidas (`<unidad>`, `estrato`, `<variable_tamano>`, `motivo`).
#' @export
aplicar_lista_negra <- function(marco, secciones = NULL, municipios = NULL,
                                variable_municipio = "municipio_cod",
                                llave_seccion = "seccion",
                                variable_estrato = "estrato",
                                min_secciones = 2,
                                variable_tamano = "lista_nominal",
                                unidad = "seccion") {
  fuera_mun <- if (!is.null(municipios)) {
    marco[[variable_municipio]] %in% municipios
  } else {
    rep(FALSE, nrow(marco))
  }
  fuera_sec <- if (!is.null(secciones)) {
    marco[[llave_seccion]] %in% secciones
  } else {
    rep(FALSE, nrow(marco))
  }

  tam <- if (variable_tamano %in% names(marco)) {
    marco[[variable_tamano]]
  } else {
    NA_real_
  }
  # el marco puede venir sin estratificar (excluir antes de estratificar es
  # un orden válido); la documentación registra NA en ese caso
  est <- if (variable_estrato %in% names(marco)) {
    marco[[variable_estrato]]
  } else {
    rep(NA_character_, nrow(marco))
  }
  etiqueta <- if (identical(unidad, "seccion")) "sección" else unidad
  doc <- tibble::tibble(
    "{unidad}" := marco[[llave_seccion]][fuera_mun | fuera_sec],
    estrato = est[fuera_mun | fuera_sec],
    "{variable_tamano}" := tam[fuera_mun | fuera_sec],
    motivo = dplyr::case_when(
      fuera_mun[fuera_mun | fuera_sec] ~ "municipio en lista negra",
      TRUE ~ paste(etiqueta, "en lista negra")
    )
  )
  res <- marco[!(fuera_mun | fuera_sec), , drop = FALSE]

  if (nrow(doc) > 0) {
    pct <- sum(doc[[variable_tamano]], na.rm = TRUE) /
      sum(marco[[variable_tamano]], na.rm = TRUE)
    message("Lista negra: ", nrow(doc), " ", etiqueta, "(es) excluidas (",
            round(100 * pct, 1), "% de `", variable_tamano, "`). ",
            "Declararlo en la nota metodológica.")
  }

  if (variable_estrato %in% names(res)) {
    restantes <- res |>
      dplyr::filter(!is.na(.data[[variable_estrato]])) |>
      dplyr::count(.data[[variable_estrato]])
    cortos <- restantes[[2]] < min_secciones
    if (any(cortos)) {
      warning("La lista negra dejó estrato(s) con menos de ", min_secciones,
              " ", plural_unidad(unidad), ": ",
              paste(restantes[[1]][cortos], collapse = ", "),
              call. = FALSE)
    }
  }

  attr(res, "lista_negra") <- doc
  res
}

#' Planear una muestra seccional: PPS por estrato con plan versionado
#'
#' Sortea las secciones de cada estrato con probabilidad proporcional a la
#' lista nominal ([seleccionar_pps()], probabilidades de inclusión exactas)
#' y entrega el **plan muestral versionable**: el contrato de la capa 1 de
#' pesos de `encuestar::construir_diseno_capas()`. Las `pi_seccion` se
#' calculan sobre el marco COMPLETO del estrato (no sobre las sorteadas):
#' son las probabilidades del diseño, no una renormalización a posteriori.
#'
#' Es la cara seccional del motor genérico [planear_muestra_upm()] (la
#' matemática vive ahí); para marcos censales por AGEB, ver
#' [planear_muestra_ageb()].
#'
#' El plan debe guardarse en el repo de la ola ANTES de campo
#' (`saveRDS(plan, "salidas/plan_ola1.rds")`): sin plan versionado la capa
#' de ajuste por sección no puede reconstruir los pesos.
#'
#' @inheritParams planear_muestra_upm
#' @param m_por_seccion Entrevistas planeadas por sección (6–8 recomendado:
#'   controla el deff de conglomerado `1 + (m-1)·rho`).
#' @param llave_seccion Columna que identifica la sección.
#' @param lista_negra Lista opcional `list(secciones =, municipios =)` que se
#'   aplica con [aplicar_lista_negra()] antes del sorteo.
#'
#' @return `tibble` (una fila por sección sorteada) con `seccion`, el
#'   dominio, `estrato`, `ln_seccion`, `pi_seccion`, `n_plan` (efectivas
#'   planeadas) y `contactos` (viviendas a tocar). Atributos: `"asignacion"`
#'   (tabla de [asignar_potencia()]), `"dominios"`, `"lista_negra"`,
#'   `"unidad"` y `"parametros"`.
#' @export
planear_muestra_seccional <- function(marco, n_total, m_por_seccion = 8,
                                      potencia = 0.5,
                                      dominio = "region",
                                      variable_estrato = "estrato",
                                      variable_tamano = "lista_nominal",
                                      llave_seccion = "seccion",
                                      min_secciones = 2,
                                      tasa_rechazo = 0,
                                      lista_negra = NULL,
                                      semilla = NULL) {
  planear_muestra_upm(
    marco, n_total,
    m_por_upm = m_por_seccion,
    potencia = potencia,
    dominio = dominio,
    variable_estrato = variable_estrato,
    variable_tamano = variable_tamano,
    llave_upm = llave_seccion,
    unidad = "seccion",
    min_secciones = min_secciones,
    tasa_rechazo = tasa_rechazo,
    lista_negra = lista_negra,
    semilla = semilla
  )
}

#' Derivar el plan muestral versionado desde el diseño INE de la clase
#'
#' Reconstruye el **plan versionado por sección** (el contrato de la capa 1
#' de `encuestar::construir_diseno_capas()`) desde un [DiseñoINE] con la
#' muestra ya extraída: las `pi_seccion` son exactamente las del sorteo
#' (el `fpc` del nivel de sección, calculado sobre el marco completo del
#' estrato) — un solo sorteo, cero inconsistencia entre el diseño operativo
#' y el de pesos. Espejo seccional de [derivar_plan_ageb()].
#'
#' @param diseno Objeto [DiseñoINE] con niveles estrato + sección, `fpc`
#'   calculado y muestra extraída (típicamente de [disenar_muestra_ine()]).
#' @param asignacion Tabla de [calcular_asignacion()]; por default se lee
#'   del atributo `"asignacion"` del diseño.
#'
#' @return Plan versionado: `tibble` con `seccion`, `estrato`, `ln_seccion`
#'   (tamaño de la sección en la variable poblacional), `pi_seccion`,
#'   `n_plan` (efectivas objetivo por sección) y `contactos` (entrevistas a
#'   levantar por sección). Atributos `"unidad"` y `"parametros"`.
#' @export
derivar_plan_seccional <- function(diseno,
                                   asignacion = attr(diseno, "asignacion")) {
  if (is.null(asignacion)) {
    stop("El diseño no trae la asignación del modelo operativo: pásala en ",
         "`asignacion` o usa disenar_muestra_ine().", call. = FALSE)
  }
  niveles <- diseno$niveles
  var_estrato <- niveles$variable[niveles$nivel == 1]
  var_seccion <- niveles$variable[niveles$nivel == diseno$ultimo_nivel]
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
  secciones_sel <- unique(ult[[var_seccion]])

  info <- marco |>
    dplyr::filter(.data[[var_seccion]] %in% secciones_sel) |>
    dplyr::group_by(.data[[var_estrato]], .data[[var_seccion]]) |>
    dplyr::summarise(
      ln_seccion = sum(.data[[diseno$variable_poblacional]], na.rm = TRUE),
      pi_seccion = unique(.data[[col_fpc]]),
      .groups = "drop"
    )
  names(info)[1:2] <- c("estrato", "seccion")

  dosis <- asignacion |>
    dplyr::transmute(
      estrato = as.character(estrato),
      n_plan = round(entrevistas / secciones),
      contactos = round(entrevistas_a_levantar / secciones)
    )

  plan <- info |>
    dplyr::mutate(estrato = as.character(estrato)) |>
    dplyr::left_join(dosis, by = "estrato") |>
    dplyr::relocate(seccion, estrato, ln_seccion, pi_seccion, n_plan,
                    contactos)

  attr(plan, "unidad") <- "seccion"
  attr(plan, "parametros") <- list(
    n_total = sum(asignacion$entrevistas),
    n_0 = diseno$n_0,
    tasa_rechazo = unique(asignacion$tasa_rechazo),
    semilla = diseno$semilla,
    unidad = "seccion",
    origen = "clase"
  )
  plan
}
