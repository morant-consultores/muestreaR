# Motor genérico del plan muestral por UPM --------------------------------
#
# La unidad primaria de muestreo (UPM) del plan versionado puede ser la
# sección electoral (marco INE, ver R/plan_seccional.R) o el AGEB urbano
# (marco censal INEGI, ver R/marco_ageb.R). Este motor contiene TODA la
# matemática del sorteo; las envolturas planear_muestra_seccional() y
# planear_muestra_ageb() solo fijan defaults y nombres de columna.
#
# El esquema del plan es el mismo en cualquier unidad:
#   <unidad>, [dominio], estrato, ln_<unidad>, pi_<unidad>, n_plan, contactos
# donde `ln_` es la MEDIDA DE TAMAÑO de la UPM (lista nominal en marcos
# electorales, población adulta en censales; el prefijo es herencia del
# flujo seccional y se conserva por regularidad del esquema).

#' Planear una muestra por UPM: PPS por estrato con plan versionado
#'
#' Motor genérico del plan muestral versionable: sortea las UPMs de cada
#' estrato con probabilidad proporcional al tamaño ([seleccionar_pps()],
#' probabilidades de inclusión exactas) y entrega el plan que es el
#' contrato de la capa 1 de pesos de `encuestar::construir_diseno_capas()`
#' (para unidades distintas de la sección, ver [plan_para_capas()]).
#' Las `pi` se calculan sobre el marco COMPLETO del estrato (no sobre las
#' sorteadas): son las probabilidades del diseño, no una renormalización a
#' posteriori.
#'
#' El plan debe guardarse en el repo de la ola ANTES de campo
#' (`saveRDS(plan, "salidas/plan_ola1.rds")`): sin plan versionado la capa
#' de ajuste por UPM no puede reconstruir los pesos.
#'
#' @inheritParams asignar_potencia
#' @param m_por_upm Entrevistas planeadas por UPM.
#' @param llave_upm Columna del marco que identifica la UPM.
#' @param unidad Nombre de la UPM; nombra las columnas del plan
#'   (`<unidad>`, `ln_<unidad>`, `pi_<unidad>`) y los mensajes.
#' @param tasa_rechazo Tasa esperada de no respuesta en `[0, 1)`: infla los
#'   `contactos` por UPM (`n_plan / (1 - tasa)`), nunca el `n_plan`.
#' @param lista_negra Lista opcional que se aplica con
#'   [aplicar_lista_negra()] antes del sorteo. Las UPMs a excluir se pasan
#'   en `upms` o en el plural de la unidad (`secciones`, `agebs`, ...);
#'   los municipios, en `municipios`.
#' @param semilla Semilla para reproducibilidad del sorteo.
#'
#' @return `tibble` (una fila por UPM sorteada) con `<unidad>`, el dominio,
#'   `estrato`, `ln_<unidad>` (medida de tamaño), `pi_<unidad>`, `n_plan`
#'   (efectivas planeadas) y `contactos` (viviendas a tocar). Atributos:
#'   `"asignacion"` (tabla de [asignar_potencia()]), `"dominios"`,
#'   `"lista_negra"`, `"unidad"` y `"parametros"`.
#' @seealso [planear_muestra_seccional()], [planear_muestra_ageb()],
#'   [planear_siguiente_ola()], [plan_para_capas()]
#' @export
planear_muestra_upm <- function(marco, n_total, m_por_upm,
                                potencia = 0.5,
                                dominio = "region",
                                variable_estrato = "estrato",
                                variable_tamano = "lista_nominal",
                                llave_upm = "upm",
                                unidad = "upm",
                                min_secciones = 2,
                                tasa_rechazo = 0,
                                lista_negra = NULL,
                                semilla = NULL) {
  if (tasa_rechazo < 0 || tasa_rechazo >= 1) {
    stop("`tasa_rechazo` debe estar en el intervalo [0, 1).", call. = FALSE)
  }
  etiqueta <- if (identical(unidad, "seccion")) "sección" else unidad

  doc_lista <- NULL
  if (!is.null(lista_negra)) {
    fuera_upms <- lista_negra$upms
    if (is.null(fuera_upms)) {
      fuera_upms <- lista_negra[[plural_unidad(unidad)]]
    }
    marco <- aplicar_lista_negra(
      marco,
      secciones = fuera_upms,
      municipios = lista_negra$municipios,
      llave_seccion = llave_upm,
      variable_estrato = variable_estrato,
      min_secciones = min_secciones,
      variable_tamano = variable_tamano,
      unidad = unidad
    )
    doc_lista <- attr(marco, "lista_negra")
  }

  sin_estrato <- is.na(marco[[variable_estrato]])
  if (any(sin_estrato)) {
    message(sum(sin_estrato), " ", etiqueta,
            "(es) sin estrato fuera del sorteo.")
    marco <- marco[!sin_estrato, , drop = FALSE]
  }

  # medida de tamaño NA = tamaño 0 (nunca sorteable), consistente con
  # seleccionar_pps; sin sanear, inclusionprobabilities() muere críptico
  tam_na <- is.na(marco[[variable_tamano]])
  if (any(tam_na)) {
    message(sum(tam_na), " ", etiqueta, "(es) con ",
            gsub("_", " ", variable_tamano), " NA: se tratan ",
            "como tamaño 0 (nunca sorteables).")
    marco[[variable_tamano]][tam_na] <- 0
  }

  asig <- asignar_potencia(marco, n_total, m_por_upm, potencia, dominio,
                           variable_estrato, variable_tamano, min_secciones,
                           unidad = unidad)
  n_por_estrato <- asig[[plural_unidad(unidad)]]

  if (!is.null(semilla)) set.seed(semilla)
  plan <- asig$estrato |>
    lapply(function(h) {
      upms_h <- marco[marco[[variable_estrato]] == h, , drop = FALSE]
      n_h <- n_por_estrato[asig$estrato == h]
      # pi del diseño: sobre TODO el estrato, antes de sortear
      upms_h$.pi_upm <- sampling::inclusionprobabilities(
        upms_h[[variable_tamano]], n_h
      )
      seleccionar_pps(upms_h, n = n_h, variable_tamano = variable_tamano)
    }) |>
    dplyr::bind_rows()

  cols_dom <- setdiff(if (is.null(dominio)) character(0) else dominio,
                      variable_estrato)
  plan <- plan |>
    dplyr::transmute(
      "{unidad}" := .data[[llave_upm]],
      dplyr::across(dplyr::all_of(cols_dom)),
      estrato = .data[[variable_estrato]],
      "ln_{unidad}" := .data[[variable_tamano]],
      "pi_{unidad}" := .data$.pi_upm,
      n_plan = m_por_upm,
      contactos = ceiling(m_por_upm / (1 - tasa_rechazo))
    )

  attr(plan, "asignacion") <- asig
  attr(plan, "dominios") <- attr(asig, "dominios")
  attr(plan, "lista_negra") <- doc_lista
  attr(plan, "unidad") <- unidad
  attr(plan, "parametros") <- list(
    n_total = n_total, m_por_upm = m_por_upm, potencia = potencia,
    tasa_rechazo = tasa_rechazo, semilla = semilla, unidad = unidad
  )
  plan
}
