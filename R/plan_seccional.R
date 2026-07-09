# Muestra seccional por estratos electorales -----------------------------
#
# Flujo de la arquitectura de pesos por capas (ver encuestar): el sorteo
# entrega un PLAN VERSIONADO por sección (seccion, pi_seccion, ln_seccion,
# n_plan, contactos) que es el contrato con encuestar::construir_diseno_capas
# (capa 1 = selección planeada). La no respuesta NO se resuelve sustituyendo:
# se dimensiona con sobremuestra (contactos) y se corrige en las capas.

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
#'   (lista nominal).
#' @param min_secciones Mínimo de secciones por estrato (default 2: con una
#'   sola sección la varianza del estrato no es estimable).
#'
#' @return `tibble` con una fila por estrato: dominio, `estrato`,
#'   `ln_estrato`, `secciones_disponibles`, `entrevistas_obj`, `secciones`,
#'   `entrevistas_plan` (= `secciones * m_por_seccion`). La asignación por
#'   dominio va en el atributo `"dominios"`.
#' @export
asignar_potencia <- function(marco, n_total, m_por_seccion,
                             potencia = 0.5,
                             dominio = "region",
                             variable_estrato = "estrato",
                             variable_tamano = "lista_nominal",
                             min_secciones = 2) {
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

  cortos <- asig$secciones_disponibles < min_secciones
  if (any(cortos)) {
    warning("Estrato(s) con menos de ", min_secciones, " secciones ",
            "disponibles (la varianza no será estimable ahí): ",
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
#' @param marco Marco seccional.
#' @param secciones Vector de llaves de sección a excluir (opcional).
#' @param municipios Vector de claves de municipio a excluir (opcional).
#' @param variable_municipio,llave_seccion,variable_estrato Nombres de columna.
#' @param min_secciones Umbral para avisar si un estrato queda corto.
#'
#' @return El marco filtrado, con el atributo `"lista_negra"`: tibble de las
#'   secciones excluidas (`seccion`, `estrato`, `lista_nominal`, `motivo`).
#' @export
aplicar_lista_negra <- function(marco, secciones = NULL, municipios = NULL,
                                variable_municipio = "municipio_cod",
                                llave_seccion = "seccion",
                                variable_estrato = "estrato",
                                min_secciones = 2) {
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

  ln <- if ("lista_nominal" %in% names(marco)) marco$lista_nominal else NA_real_
  # el marco puede venir sin estratificar (excluir antes de estratificar es
  # un orden válido); la documentación registra NA en ese caso
  est <- if (variable_estrato %in% names(marco)) {
    marco[[variable_estrato]]
  } else {
    rep(NA_character_, nrow(marco))
  }
  doc <- tibble::tibble(
    seccion = marco[[llave_seccion]][fuera_mun | fuera_sec],
    estrato = est[fuera_mun | fuera_sec],
    lista_nominal = ln[fuera_mun | fuera_sec],
    motivo = dplyr::case_when(
      fuera_mun[fuera_mun | fuera_sec] ~ "municipio en lista negra",
      TRUE ~ "sección en lista negra"
    )
  )
  res <- marco[!(fuera_mun | fuera_sec), , drop = FALSE]

  if (nrow(doc) > 0) {
    pct <- sum(doc$lista_nominal, na.rm = TRUE) /
      sum(marco$lista_nominal, na.rm = TRUE)
    message("Lista negra: ", nrow(doc), " sección(es) excluidas (",
            round(100 * pct, 1), "% de la lista nominal). ",
            "Declararlo en la nota metodológica.")
  }

  if (variable_estrato %in% names(res)) {
    restantes <- res |>
      dplyr::filter(!is.na(.data[[variable_estrato]])) |>
      dplyr::count(.data[[variable_estrato]])
    cortos <- restantes[[2]] < min_secciones
    if (any(cortos)) {
      warning("La lista negra dejó estrato(s) con menos de ", min_secciones,
              " secciones: ", paste(restantes[[1]][cortos], collapse = ", "),
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
#' El plan debe guardarse en el repo de la ola ANTES de campo
#' (`saveRDS(plan, "salidas/plan_ola1.rds")`): sin plan versionado la capa
#' de ajuste por sección no puede reconstruir los pesos.
#'
#' @inheritParams asignar_potencia
#' @param llave_seccion Columna que identifica la sección.
#' @param tasa_rechazo Tasa esperada de no respuesta en `[0, 1)`: infla los
#'   `contactos` por sección (`n_plan / (1 - tasa)`), nunca el `n_plan`.
#' @param lista_negra Lista opcional `list(secciones =, municipios =)` que se
#'   aplica con [aplicar_lista_negra()] antes del sorteo.
#' @param semilla Semilla para reproducibilidad del sorteo.
#'
#' @return `tibble` (una fila por sección sorteada) con `seccion`, el
#'   dominio, `estrato`, `ln_seccion`, `pi_seccion`, `n_plan` (efectivas
#'   planeadas) y `contactos` (viviendas a tocar). Atributos: `"asignacion"`
#'   (tabla de [asignar_potencia()]), `"dominios"`, `"lista_negra"`,
#'   `"parametros"`.
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
  if (tasa_rechazo < 0 || tasa_rechazo >= 1) {
    stop("`tasa_rechazo` debe estar en el intervalo [0, 1).", call. = FALSE)
  }

  doc_lista <- NULL
  if (!is.null(lista_negra)) {
    marco <- aplicar_lista_negra(
      marco,
      secciones = lista_negra$secciones,
      municipios = lista_negra$municipios,
      llave_seccion = llave_seccion,
      variable_estrato = variable_estrato,
      min_secciones = min_secciones
    )
    doc_lista <- attr(marco, "lista_negra")
  }

  sin_estrato <- is.na(marco[[variable_estrato]])
  if (any(sin_estrato)) {
    message(sum(sin_estrato), " sección(es) sin estrato fuera del sorteo.")
    marco <- marco[!sin_estrato, , drop = FALSE]
  }

  # lista nominal NA = tamaño 0 (nunca sorteable), consistente con
  # seleccionar_pps; sin sanear, inclusionprobabilities() muere críptico
  tam_na <- is.na(marco[[variable_tamano]])
  if (any(tam_na)) {
    message(sum(tam_na), " sección(es) con lista nominal NA: se tratan ",
            "como tamaño 0 (nunca sorteables).")
    marco[[variable_tamano]][tam_na] <- 0
  }

  asig <- asignar_potencia(marco, n_total, m_por_seccion, potencia, dominio,
                           variable_estrato, variable_tamano, min_secciones)

  if (!is.null(semilla)) set.seed(semilla)
  plan <- asig$estrato |>
    lapply(function(h) {
      secs_h <- marco[marco[[variable_estrato]] == h, , drop = FALSE]
      n_h <- asig$secciones[asig$estrato == h]
      # pi del diseño: sobre TODO el estrato, antes de sortear
      secs_h$pi_seccion <- sampling::inclusionprobabilities(
        secs_h[[variable_tamano]], n_h
      )
      seleccionar_pps(secs_h, n = n_h, variable_tamano = variable_tamano)
    }) |>
    dplyr::bind_rows()

  cols_dom <- setdiff(if (is.null(dominio)) character(0) else dominio,
                      variable_estrato)
  plan <- plan |>
    dplyr::transmute(
      seccion = .data[[llave_seccion]],
      dplyr::across(dplyr::all_of(cols_dom)),
      estrato = .data[[variable_estrato]],
      ln_seccion = .data[[variable_tamano]],
      pi_seccion,
      n_plan = m_por_seccion,
      contactos = ceiling(m_por_seccion / (1 - tasa_rechazo))
    )

  attr(plan, "asignacion") <- asig
  attr(plan, "dominios") <- attr(asig, "dominios")
  attr(plan, "lista_negra") <- doc_lista
  attr(plan, "parametros") <- list(
    n_total = n_total, m_por_seccion = m_por_seccion, potencia = potencia,
    tasa_rechazo = tasa_rechazo, semilla = semilla
  )
  plan
}
