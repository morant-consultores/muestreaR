# Estado de campo contra la ruta y la reserva impresas -------------------
#
# Cruza los toques de puerta ya normalizados (una fila por intento, con el
# desenlace canónico y la clase de la razón de no registro) contra el
# material que campo trae impreso, y contesta las dos preguntas del día:
# ¿cuánto presupuesto de puertas queda en cada cluster, y qué manzana
# tiene que caminar campo mañana?
#
# NO muta el diseño. La reserva ya está sorteada y su número impreso es su
# propio `orden_ruta`, que continúa la secuencia de la ruta sin huecos, así
# que recorrerla no cambia ninguna probabilidad de selección. Lo único que
# esta función hace es DECIR cuál sigue.

#' Estado de campo por manzana y por cluster
#'
#' @param toques Data frame con una fila por intento de puerta:
#'   `cluster_2`, `manzana_num`, `resultado` (`"efectiva"`, `"rechazo"`,
#'   `"no_abrio"`, `"sin_registro"`) y `clase_razon` (`"no_vivienda"`,
#'   `"sin_acceso"`, `"recorrida_sin_entrevistas"` o `NA`).
#' @param ruta Ruta impresa: `cluster`, `orden_ruta`, `manzana`,
#'   `seccion`, `hoja`, `puertas_a_tocar`,
#'   `presupuesto_puertas_cluster`, `entrevistas_meta_cluster`.
#' @param reserva Reserva sorteada y NO impresa: `cluster`,
#'   `orden_ruta`, `manzana`, `seccion`, `puertas_esperadas_manzana`.
#' @param cuotas Cuotas por cluster: `cluster_2`, `SECCION`,
#'   `entrevistas`, `puertas`. (Reservado para validaciones futuras contra
#'   el plan de cuotas; hoy `estado_de_campo()` no lo usa todavía).
#' @param n_0 Entrevistas objetivo por manzana (default 6).
#' @param tope_cierre Múltiplo de `puertas_a_tocar` a partir del cual la
#'   manzana se considera cerrada por presupuesto (default 1.25). Sin
#'   tope, un cluster se lleva varias veces su dosis.
#'
#' @return `list` con dos tibbles:
#'   * `manzanas`: una fila por manzana del sorteo (ruta + reserva) más las
#'     manzanas tocadas fuera del sorteo, con su `estado` y la `accion` de
#'     campo (`"CONTINUAR"`, `"SUSTITUIR con la manzana <n> (reserva)"`,
#'     `"RESERVA AGOTADA..."`, `"CERRADA"` o `"FUERA DEL SORTEO..."`).
#'   * `clusters`: una fila por cluster de la ruta (ninguno desaparece,
#'     tenga o no toques) con el presupuesto de puertas consumido y
#'     restante, la tasa medida y el avance de la ruta impresa
#'     (`mzas_sin_iniciar` + `mzas_en_proceso` + `mzas_cerradas` +
#'     `mzas_a_sustituir` == `manzanas_ruta`).
#' @export
estado_de_campo <- function(toques, ruta, reserva, cuotas,
                            n_0 = 6, tope_cierre = 1.25) {
  req <- c("cluster_2", "manzana_num", "resultado", "clase_razon")
  falta <- setdiff(req, names(toques))
  if (length(falta)) {
    stop("`toques` no trae: ", paste(falta, collapse = ", "), call. = FALSE)
  }

  # ---- universo de manzanas del sorteo, ruta y reserva en la MISMA llave
  sorteo <- dplyr::bind_rows(
    dplyr::transmute(ruta, cluster_2 = as.integer(cluster),
                     manzana_num = as.integer(orden_ruta),
                     manzana_ine = manzana, seccion = as.character(seccion),
                     hoja = as.character(hoja),
                     puertas_plan = as.integer(puertas_a_tocar),
                     origen = "ruta"),
    dplyr::transmute(reserva, cluster_2 = as.integer(cluster),
                     manzana_num = as.integer(orden_ruta),
                     manzana_ine = manzana, seccion = as.character(seccion),
                     hoja = NA_character_,
                     puertas_plan = as.integer(puertas_esperadas_manzana),
                     origen = "reserva"))

  t <- dplyr::mutate(toques,
                     cluster_2 = as.integer(cluster_2),
                     manzana_num = as.integer(manzana_num))

  # ---- conteo por manzana TOCADA
  por_mza <- t |>
    dplyr::group_by(cluster_2, manzana_num) |>
    dplyr::summarise(
      # una puerta de VIVIENDA es la que se pudo evaluar: abrió (efectiva o
      # rechazo) o no abrió. La que no era vivienda no gastó presupuesto.
      puertas_vivienda    = sum(resultado %in% c("efectiva", "rechazo", "no_abrio")),
      puertas_no_vivienda = sum(!is.na(clase_razon) & clase_razon == "no_vivienda"),
      efectivas = sum(resultado == "efectiva"),
      rechazos  = sum(resultado == "rechazo"),
      no_abrio  = sum(resultado == "no_abrio"),
      hay_sin_acceso = any(!is.na(clase_razon) & clase_razon == "sin_acceso"),
      hay_recorrida  = any(!is.na(clase_razon) &
                             clase_razon == "recorrida_sin_entrevistas"),
      .groups = "drop")

  mz <- dplyr::full_join(sorteo, por_mza, by = c("cluster_2", "manzana_num")) |>
    dplyr::mutate(
      origen = dplyr::coalesce(origen, "fuera_del_sorteo"),
      dplyr::across(c(puertas_vivienda, puertas_no_vivienda, efectivas,
                      rechazos, no_abrio), ~dplyr::coalesce(.x, 0L)),
      hay_sin_acceso = dplyr::coalesce(hay_sin_acceso, FALSE),
      hay_recorrida  = dplyr::coalesce(hay_recorrida, FALSE),
      tocada = puertas_vivienda + puertas_no_vivienda > 0,
      # El orden de las ramas es semántico, no cosmético: fuera_del_sorteo
      # va primero porque una manzana que no existe en el material no puede
      # tener ningún otro estado; cerrada_por_recorrido antes que sin_acceso
      # porque si campo declaró las dos cosas, la manzana SÍ se caminó.
      estado = dplyr::case_when(
        origen == "fuera_del_sorteo"                        ~ "fuera_del_sorteo",
        hay_recorrida                                      ~ "cerrada_por_recorrido",
        hay_sin_acceso & efectivas == 0                     ~ "sin_acceso",
        # una manzana donde TODO lo tocado resultó no ser vivienda no tiene
        # puertas que ofrecer: hay que sustituirla, no insistir
        tocada & puertas_vivienda == 0 & puertas_no_vivienda > 0 ~ "no_es_vivienda",
        efectivas >= n_0                                   ~ "cerrada_por_dosis",
        !is.na(puertas_plan) &
          puertas_vivienda >= ceiling(puertas_plan * tope_cierre) ~ "cerrada_por_presupuesto",
        tocada                                             ~ "en_proceso",
        TRUE                                               ~ "sin_iniciar"))

  # ---- asignación de reserva: determinista y sin repetir
  # Candidatas: reserva del cluster NO tocada, en orden de `manzana_num`.
  # Se reparten una por hueco (por turno, vía dplyr::row_number(), no con
  # un acumulador mutable); cuando se agotan, la acción lo dice.
  tocadas <- unique(t[, c("cluster_2", "manzana_num")])
  libres <- mz |>
    dplyr::filter(origen == "reserva", !tocada) |>
    dplyr::anti_join(tocadas, by = c("cluster_2", "manzana_num")) |>
    dplyr::arrange(cluster_2, manzana_num) |>
    dplyr::group_by(cluster_2) |>
    dplyr::mutate(turno = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::select(cluster_2, sustituta = manzana_num, turno)

  huecos <- mz |>
    dplyr::filter(estado %in% c("sin_acceso", "no_es_vivienda")) |>
    dplyr::arrange(cluster_2, manzana_num) |>
    dplyr::group_by(cluster_2) |>
    dplyr::mutate(turno = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::select(cluster_2, manzana_num, turno) |>
    dplyr::left_join(libres, by = c("cluster_2", "turno")) |>
    dplyr::select(cluster_2, manzana_num, sustituta)

  manzanas <- mz |>
    dplyr::left_join(huecos, by = c("cluster_2", "manzana_num")) |>
    dplyr::mutate(accion = dplyr::case_when(
      estado == "fuera_del_sorteo" ~ paste0(
        "FUERA DEL SORTEO - no cuenta: la manzana ", manzana_num,
        " no existe en el material del cluster ", cluster_2),
      estado %in% c("sin_acceso", "no_es_vivienda") & !is.na(sustituta) ~ paste0(
        "SUSTITUIR con la manzana ", sustituta, " (reserva)"),
      estado %in% c("sin_acceso", "no_es_vivienda") ~
        "RESERVA AGOTADA - escalar con el lider de campo",
      startsWith(estado, "cerrada") ~ "CERRADA",
      TRUE ~ "CONTINUAR")) |>
    dplyr::select(cluster_2, manzana_num, origen, seccion, hoja, puertas_plan,
                  puertas_vivienda, puertas_no_vivienda, efectivas, rechazos,
                  no_abrio, estado, accion) |>
    dplyr::arrange(cluster_2, manzana_num)

  # ---- resumen por cluster. Se parte del PLAN para que ningún cluster
  # desaparezca por no tener toques todavía.
  plan_cl <- ruta |>
    dplyr::group_by(cluster_2 = as.integer(cluster)) |>
    dplyr::summarise(seccion = dplyr::first(as.character(seccion)),
                     presupuesto_puertas = dplyr::first(as.integer(presupuesto_puertas_cluster)),
                     entrevistas_meta = dplyr::first(as.integer(entrevistas_meta_cluster)),
                     manzanas_ruta = dplyr::n(), .groups = "drop")

  # el presupuesto y las efectivas los consume la RUTA y la RESERVA ya
  # activada por igual: ambas tienen probabilidad de selección conocida.
  # Lo de fuera del sorteo no entra: su probabilidad de selección es
  # desconocida.
  consumo <- manzanas |>
    dplyr::filter(origen != "fuera_del_sorteo") |>
    dplyr::group_by(cluster_2) |>
    dplyr::summarise(puertas_vivienda = sum(puertas_vivienda),
                     efectivas = sum(efectivas),
                     .groups = "drop")

  # estos 4 conteos SÍ deben ser una partición de la ruta impresa (por eso
  # suman manzanas_ruta): una reserva todavía sin convocar no es una
  # manzana "sin iniciar" del plan, ni siquiera es parte del plan hasta que
  # se activa como sustituta. Si se contara aquí, un cluster con reserva
  # de sobra se vería con manzanas "sin iniciar" que campo nunca tiene que
  # caminar.
  avance_ruta <- manzanas |>
    dplyr::filter(origen == "ruta") |>
    dplyr::group_by(cluster_2) |>
    dplyr::summarise(
      mzas_sin_iniciar = sum(estado == "sin_iniciar"),
      mzas_en_proceso  = sum(estado == "en_proceso"),
      mzas_cerradas    = sum(startsWith(estado, "cerrada")),
      mzas_a_sustituir = sum(estado %in% c("sin_acceso", "no_es_vivienda")),
      .groups = "drop")

  fuera_por_cluster <- t |>
    dplyr::semi_join(dplyr::filter(manzanas, origen == "fuera_del_sorteo"),
                     by = c("cluster_2", "manzana_num")) |>
    dplyr::count(cluster_2, name = "toques_fuera_del_sorteo")

  clusters <- plan_cl |>
    dplyr::left_join(consumo, by = "cluster_2") |>
    dplyr::left_join(avance_ruta, by = "cluster_2") |>
    dplyr::left_join(fuera_por_cluster, by = "cluster_2") |>
    dplyr::mutate(
      dplyr::across(c(puertas_vivienda, efectivas, mzas_sin_iniciar,
                      mzas_en_proceso, mzas_cerradas, mzas_a_sustituir,
                      toques_fuera_del_sorteo), ~dplyr::coalesce(.x, 0L)),
      presupuesto_restante = presupuesto_puertas - puertas_vivienda,
      # NA y no 0 cuando no hay puertas: una tasa de 0 sobre 0 puertas
      # diría "este cluster no funciona" cuando nadie fue todavía
      tasa_medida = dplyr::if_else(puertas_vivienda > 0,
                                   efectivas / puertas_vivienda, NA_real_)) |>
    dplyr::select(cluster_2, seccion, entrevistas_meta, efectivas,
                  presupuesto_puertas, puertas_vivienda, presupuesto_restante,
                  tasa_medida, manzanas_ruta, mzas_sin_iniciar, mzas_en_proceso,
                  mzas_cerradas, mzas_a_sustituir, toques_fuera_del_sorteo) |>
    dplyr::arrange(cluster_2)

  list(manzanas = manzanas, clusters = clusters)
}
