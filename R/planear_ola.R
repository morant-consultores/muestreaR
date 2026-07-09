# Olas consecuentes -------------------------------------------------------
#
# La ola anterior deja dos activos: el registro de contactos (tasa de
# respuesta real por sección) y las secciones inoperables reportadas por
# campo. Esta función los convierte en el plan de la siguiente ola con
# sobremuestra DIFERENCIADA: más contactos donde costó más, sin sustituir.

#' Planear la siguiente ola con las tasas de respuesta aprendidas
#'
#' Construye el plan muestral de una ola consecuente usando el **registro de
#' contactos** de la ola anterior (protocolo de campo: todo intento se
#' registra, conteste o no). La tasa de respuesta esperada de cada sección
#' se estima con encogimiento hacia la tasa agregada de su estrato
#' (`tasa_hat = encogimiento * tasa_propia + (1 - encogimiento) * tasa_estrato`)
#' y dimensiona los contactos: `contactos = min(ceiling(n_plan / tasa_hat),
#' tope_contactos * n_plan)`.
#'
#' Dos métodos:
#' \describe{
#'   \item{`"resortear"`}{sorteo PPS nuevo e independiente (vía
#'     [planear_muestra_seccional()]). Las secciones que repiten usan su tasa
#'     propia encogida; las nuevas, la tasa agregada de su estrato (o la
#'     general si el estrato no se observó).}
#'   \item{`"panel"`}{conserva las secciones y `pi_seccion` de la ola
#'     anterior (comparabilidad máxima, misma logística) y solo re-dosifica
#'     los contactos por sección.}
#' }
#'
#' Las secciones con **cero efectivas** en el registro se devuelven en el
#' atributo `"inoperables"`: gabinete decide si son caso puntual (reintentar
#' con más sobremuestra) o estructural (pasarlas a la lista negra de la
#' siguiente ola y declararlas). La exclusión nunca es automática.
#'
#' @param marco Marco seccional estratificado (el vigente para la ola nueva).
#' @param plan_anterior Plan de la ola previa ([planear_muestra_seccional()]).
#' @param registro_contactos `tibble` con una fila por sección levantada:
#'   `seccion`, `contactos` (intentos totales registrados por la app) y
#'   `efectivas` (entrevistas válidas tras auditoría).
#' @param metodo `"resortear"` (default) o `"panel"`.
#' @param n_total,m_por_seccion Objetivos de la ola nueva; por defecto los de
#'   `plan_anterior`.
#' @param encogimiento Peso de la tasa propia de la sección en `[0, 1]`
#'   (default 0.6). Con pocos contactos por sección conviene bajarlo.
#' @param tasa_minima Piso de la tasa estimada (default 0.2): evita que una
#'   sección con muy mala suerte pida contactos infinitos.
#' @param tope_contactos Tope de contactos como múltiplo de `n_plan`
#'   (default 3).
#' @param lista_negra Lista `list(secciones =, municipios =)` actualizada con
#'   lo reportado en campo; solo aplica con `metodo = "resortear"`.
#' @param semilla Semilla del re-sorteo.
#' @param ... Argumentos extra para [planear_muestra_seccional()] (`potencia`,
#'   `dominio`, `min_secciones`, ...).
#'
#' @return Plan de la ola nueva (mismo esquema que
#'   [planear_muestra_seccional()]) con atributos: `"tasas"` (tabla por
#'   estrato: contactos, efectivas, `tasa_estrato`), `"tasa_general"`,
#'   `"inoperables"` (secciones con 0 efectivas) y `"metodo"`.
#' @export
planear_siguiente_ola <- function(marco, plan_anterior, registro_contactos,
                                  metodo = c("resortear", "panel"),
                                  n_total = NULL, m_por_seccion = NULL,
                                  encogimiento = 0.6,
                                  tasa_minima = 0.2,
                                  tope_contactos = 3,
                                  lista_negra = NULL,
                                  semilla = NULL, ...) {
  metodo <- match.arg(metodo)
  requeridas <- c("seccion", "contactos", "efectivas")
  faltan <- setdiff(requeridas, names(registro_contactos))
  if (length(faltan) > 0) {
    stop("Al registro de contactos le faltan columnas: ",
         paste(faltan, collapse = ", "), call. = FALSE)
  }
  if (encogimiento < 0 || encogimiento > 1) {
    stop("`encogimiento` debe estar en [0, 1].", call. = FALSE)
  }

  if (is.null(n_total)) n_total <- sum(plan_anterior$n_plan)
  if (is.null(m_por_seccion)) {
    m_por_seccion <- as.integer(stats::median(plan_anterior$n_plan))
  }

  # tasas observadas: por sección (acotada) y agregadas por estrato
  registro <- registro_contactos |>
    dplyr::left_join(plan_anterior |> dplyr::select(seccion, estrato),
                     by = "seccion") |>
    dplyr::mutate(
      tasa_propia = pmin(pmax(efectivas / contactos, tasa_minima), 1)
    )
  tasas <- registro |>
    dplyr::filter(!is.na(estrato)) |>
    dplyr::group_by(estrato) |>
    dplyr::summarise(
      contactos = sum(contactos), efectivas = sum(efectivas),
      tasa_estrato = pmin(pmax(efectivas / contactos, tasa_minima), 1),
      .groups = "drop"
    )
  tasa_general <- pmin(pmax(
    sum(registro$efectivas) / sum(registro$contactos), tasa_minima), 1)
  inoperables <- registro$seccion[registro$efectivas == 0]

  if (metodo == "panel") {
    plan <- plan_anterior
    if (!is.null(lista_negra)) {
      warning("`lista_negra` se ignora con metodo = 'panel' (las secciones ",
              "se conservan). Para excluir, usa metodo = 'resortear'.",
              call. = FALSE)
    }
  } else {
    plan <- planear_muestra_seccional(
      marco, n_total = n_total, m_por_seccion = m_por_seccion,
      lista_negra = lista_negra, semilla = semilla, ...
    )
  }

  # tasa esperada por sección del plan nuevo: propia encogida si se observó,
  # tasa del estrato si es nueva, general si el estrato no se observó
  plan <- plan |>
    dplyr::left_join(registro |> dplyr::select(seccion, tasa_propia),
                     by = "seccion") |>
    dplyr::left_join(tasas |> dplyr::select(estrato, tasa_estrato),
                     by = "estrato") |>
    dplyr::mutate(
      tasa_estrato = dplyr::coalesce(tasa_estrato, tasa_general),
      tasa_hat = dplyr::if_else(
        is.na(tasa_propia),
        tasa_estrato,
        encogimiento * tasa_propia + (1 - encogimiento) * tasa_estrato
      ),
      tasa_hat = pmin(pmax(tasa_hat, tasa_minima), 1),
      contactos = pmin(ceiling(n_plan / tasa_hat),
                       tope_contactos * n_plan)
    ) |>
    dplyr::select(-tasa_propia, -tasa_estrato, -tasa_hat)

  attr(plan, "tasas") <- tasas
  attr(plan, "tasa_general") <- tasa_general
  attr(plan, "inoperables") <- inoperables
  attr(plan, "metodo") <- metodo
  plan
}
