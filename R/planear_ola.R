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
#' registra, conteste o no). Opera sobre planes de cualquier unidad
#' (sección electoral o AGEB censal): la unidad se lee del atributo
#' `"unidad"` del plan. La tasa de respuesta esperada de cada UPM
#' se estima con encogimiento hacia la tasa agregada de su estrato
#' (`tasa_hat = encogimiento * tasa_propia + (1 - encogimiento) * tasa_estrato`)
#' y dimensiona los contactos: `contactos = min(ceiling(n_plan / tasa_hat),
#' tope_contactos * n_plan)`.
#'
#' Dos métodos:
#' \describe{
#'   \item{`"resortear"`}{sorteo PPS nuevo e independiente (vía
#'     [planear_muestra_upm()]). Las UPMs que repiten usan su tasa
#'     propia encogida; las nuevas, la tasa agregada de su estrato (o la
#'     general si el estrato no se observó).}
#'   \item{`"panel"`}{conserva las UPMs y sus `pi` de la ola
#'     anterior (comparabilidad máxima, misma logística) y solo re-dosifica
#'     los contactos por UPM.}
#' }
#'
#' Las UPMs con **cero efectivas** en el registro se devuelven en el
#' atributo `"inoperables"`: gabinete decide si son caso puntual (reintentar
#' con más sobremuestra) o estructural (pasarlas a la lista negra de la
#' siguiente ola y declararlas). La exclusión nunca es automática.
#'
#' @param marco Marco de UPMs estratificado (el vigente para la ola nueva).
#' @param plan_anterior Plan de la ola previa ([planear_muestra_upm()] o sus
#'   envolturas seccional/AGEB).
#' @param registro_contactos `tibble` con una fila por UPM levantada: la
#'   llave de la unidad (`seccion`, `ageb`, ...), `contactos` (intentos
#'   totales registrados por la app) y `efectivas` (entrevistas válidas
#'   tras auditoría).
#' @param metodo `"resortear"` (default) o `"panel"`.
#' @param n_total,m_por_seccion Objetivos de la ola nueva; por defecto los de
#'   `plan_anterior` (`m_por_seccion` es el `m` por UPM; conserva su nombre
#'   histórico).
#' @param encogimiento Peso de la tasa propia de la UPM en `[0, 1]`
#'   (default 0.6). Con pocos contactos por UPM conviene bajarlo.
#' @param tasa_minima Piso de la tasa estimada (default 0.2): evita que una
#'   UPM con muy mala suerte pida contactos infinitos.
#' @param tope_contactos Tope de contactos como múltiplo de `n_plan`
#'   (default 3).
#' @param lista_negra Lista (`list(upms =, municipios =)` o el plural de la
#'   unidad) actualizada con lo reportado en campo; solo aplica con
#'   `metodo = "resortear"`.
#' @param semilla Semilla del re-sorteo.
#' @param unidad Unidad del plan; por default se lee del atributo
#'   `"unidad"` de `plan_anterior` (planes viejos sin atributo: `"seccion"`).
#' @param llave_upm Columna del marco que identifica la UPM en el re-sorteo;
#'   por default el nombre de la unidad.
#' @param ... Argumentos extra para [planear_muestra_upm()] (`potencia`,
#'   `dominio`, `variable_tamano`, `min_secciones`, ...).
#'
#' @return Plan de la ola nueva (mismo esquema que `plan_anterior`) con
#'   atributos: `"tasas"` (tabla por estrato: contactos, efectivas,
#'   `tasa_estrato`), `"tasa_general"`, `"inoperables"` (UPMs con 0
#'   efectivas) y `"metodo"`.
#' @export
planear_siguiente_ola <- function(marco, plan_anterior, registro_contactos,
                                  metodo = c("resortear", "panel"),
                                  n_total = NULL, m_por_seccion = NULL,
                                  encogimiento = 0.6,
                                  tasa_minima = 0.2,
                                  tope_contactos = 3,
                                  lista_negra = NULL,
                                  semilla = NULL,
                                  unidad = NULL,
                                  llave_upm = NULL, ...) {
  metodo <- match.arg(metodo)
  if (is.null(unidad)) unidad <- attr(plan_anterior, "unidad")
  if (is.null(unidad)) unidad <- "seccion"
  if (is.null(llave_upm)) llave_upm <- unidad
  col_pi <- paste0("pi_", unidad)
  etiqueta <- if (identical(unidad, "seccion")) "secciones" else
    plural_unidad(unidad)

  requeridas <- c(unidad, "contactos", "efectivas")
  faltan <- setdiff(requeridas, names(registro_contactos))
  if (length(faltan) > 0) {
    stop("Al registro de contactos le faltan columnas: ",
         paste(faltan, collapse = ", "), call. = FALSE)
  }
  # el registro alimenta joins y tasas: datos corruptos aquí corrompen el
  # plan entero en silencio, así que se validan de entrada
  if (anyDuplicated(registro_contactos[[unidad]])) {
    stop("Hay ", etiqueta, " duplicadas en el registro de contactos (doble ",
         "carga): agrégalo a una fila por ", unidad, " antes de planear.",
         call. = FALSE)
  }
  if (anyNA(registro_contactos$contactos) ||
      anyNA(registro_contactos$efectivas)) {
    stop("El registro de contactos trae NA en `contactos`/`efectivas`.",
         call. = FALSE)
  }
  if (any(registro_contactos$contactos < 0 |
            registro_contactos$efectivas < 0)) {
    stop("El registro de contactos trae valores negativos.", call. = FALSE)
  }
  if (any(registro_contactos$efectivas > registro_contactos$contactos)) {
    stop("Hay ", etiqueta, " con más efectivas que contactos en el ",
         "registro: revisa la carga.", call. = FALSE)
  }
  req_plan <- c(unidad, "estrato", col_pi, "n_plan")
  faltan_plan <- setdiff(req_plan, names(plan_anterior))
  if (length(faltan_plan) > 0) {
    stop("A `plan_anterior` le faltan columnas del plan versionado: ",
         paste(faltan_plan, collapse = ", "), call. = FALSE)
  }
  if (encogimiento < 0 || encogimiento > 1) {
    stop("`encogimiento` debe estar en [0, 1].", call. = FALSE)
  }

  if (is.null(n_total)) n_total <- sum(plan_anterior$n_plan)
  if (is.null(m_por_seccion)) {
    m_por_seccion <- as.integer(stats::median(plan_anterior$n_plan))
  }

  # tasas observadas: por UPM (acotada) y agregadas por estrato
  registro <- registro_contactos |>
    dplyr::left_join(
      plan_anterior |> dplyr::select(dplyr::all_of(c(unidad, "estrato"))),
      by = unidad
    ) |>
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
  inoperables <- registro[[unidad]][registro$efectivas == 0]

  if (metodo == "panel") {
    plan <- plan_anterior
    if (!is.null(lista_negra)) {
      warning("`lista_negra` se ignora con metodo = 'panel' (las ",
              etiqueta, " se conservan). Para excluir, usa ",
              "metodo = 'resortear'.", call. = FALSE)
    }
  } else {
    plan <- planear_muestra_upm(
      marco, n_total = n_total, m_por_upm = m_por_seccion,
      llave_upm = llave_upm, unidad = unidad,
      lista_negra = lista_negra, semilla = semilla, ...
    )
  }

  # tasa esperada por UPM del plan nuevo: propia encogida si se observó,
  # tasa del estrato si es nueva, general si el estrato no se observó
  plan <- plan |>
    dplyr::left_join(
      registro |> dplyr::select(dplyr::all_of(unidad), tasa_propia),
      by = unidad
    ) |>
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

  attr(plan, "unidad") <- unidad
  attr(plan, "tasas") <- tasas
  attr(plan, "tasa_general") <- tasa_general
  attr(plan, "inoperables") <- inoperables
  attr(plan, "metodo") <- metodo
  plan
}
