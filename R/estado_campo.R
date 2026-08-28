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
#' Antes de calcular nada, exige que `ruta` y `cuotas` describan el mismo
#' plan por cluster (misma `entrevistas_meta_cluster`/`entrevistas` y mismo
#' `presupuesto_puertas_cluster`/`puertas`): si divergen, el material que
#' campo trae impreso y el plan con el que se sortearon las cuotas ya no
#' son el mismo, y la función se detiene en vez de reportar avance contra
#' una meta ajena.
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
#'   `entrevistas`, `puertas`. Se coteja contra `ruta` (ver la descripción
#'   de arriba); si algún cluster difiere o falta de un lado,
#'   `estado_de_campo()` se detiene con un error que nombra el o los
#'   clusters y ambos valores.
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

  # ---- guarda: esquema y coerción de `ruta`/`cuotas` ANTES de comparar
  # Un valor no coercible (o ya ausente) en estas columnas se vuelve NA vía
  # as.integer() sin avisar con claridad, y una comparación `!=` contra un
  # NA hace que `any()` truene con el error críptico de R base ("valor
  # ausente donde TRUE/FALSE es necesario") justo en el caso de dato sucio
  # para el que existe esta guarda. Se revisa ANTES de comparar para dar
  # el diagnóstico en español que la guarda promete, no el de R.
  req_ruta_chk <- c("cluster", "entrevistas_meta_cluster", "presupuesto_puertas_cluster")
  falta_ruta_chk <- setdiff(req_ruta_chk, names(ruta))
  if (length(falta_ruta_chk)) {
    stop("`ruta` no trae: ", paste(falta_ruta_chk, collapse = ", "), call. = FALSE)
  }
  req_cuotas_chk <- c("cluster_2", "entrevistas", "puertas")
  falta_cuotas_chk <- setdiff(req_cuotas_chk, names(cuotas))
  if (length(falta_cuotas_chk)) {
    stop("`cuotas` no trae: ", paste(falta_cuotas_chk, collapse = ", "), call. = FALSE)
  }

  cols_enteras_chk <- list(
    list(tabla = "ruta", columna = "cluster", valores = ruta$cluster),
    list(tabla = "ruta", columna = "entrevistas_meta_cluster",
         valores = ruta$entrevistas_meta_cluster),
    list(tabla = "ruta", columna = "presupuesto_puertas_cluster",
         valores = ruta$presupuesto_puertas_cluster),
    list(tabla = "cuotas", columna = "cluster_2", valores = cuotas$cluster_2),
    list(tabla = "cuotas", columna = "entrevistas", valores = cuotas$entrevistas),
    list(tabla = "cuotas", columna = "puertas", valores = cuotas$puertas))
  for (col_chk in cols_enteras_chk) {
    malos_chk <- is.na(col_chk$valores) |
      is.na(suppressWarnings(as.integer(col_chk$valores)))
    if (any(malos_chk)) {
      ofensores_chk <- unique(vapply(col_chk$valores[malos_chk], function(v) {
        if (is.na(v)) "NA" else as.character(v)
      }, character(1)))
      stop("`", col_chk$tabla, "$", col_chk$columna, "` trae valor(es) no ",
           "coercibles a entero: ", paste(ofensores_chk, collapse = ", "),
           call. = FALSE)
    }
  }

  # ---- guarda: `ruta` y `cuotas` deben describir el MISMO plan por cluster
  # `cuotas` es redundante con los totales de `ruta` A PROPÓSITO: esa
  # redundancia es lo único que permite detectar que el material impreso y
  # el plan con el que se sortearon las cuotas ya divergieron. Si divergen,
  # el Excel reportaría el avance contra una meta que no es la del cluster
  # y campo perseguiría un número ajeno. Este proyecto ya se quemó con
  # divergencias silenciosas entre plan y material: la guarda falla
  # ruidoso en vez de reportar metas falsas.
  ruta_cl_chk <- ruta |>
    dplyr::group_by(cluster_2 = as.integer(cluster)) |>
    dplyr::summarise(
      entrevistas_meta_cluster = dplyr::first(as.integer(entrevistas_meta_cluster)),
      presupuesto_puertas_cluster = dplyr::first(as.integer(presupuesto_puertas_cluster)),
      .groups = "drop")
  cuotas_cl_chk <- dplyr::transmute(cuotas, cluster_2 = as.integer(cluster_2),
                                    entrevistas = as.integer(entrevistas),
                                    puertas = as.integer(puertas))
  cotejo_chk <- dplyr::full_join(ruta_cl_chk, cuotas_cl_chk, by = "cluster_2")

  # "ausente" (NA por no tener fila del otro lado) y "presente pero NA" son
  # cosas distintas: is.na() sobre la columna de VALOR después del join las
  # confunde y manda a buscar en la tabla equivocada. Comparar LLAVES con
  # %in% es correcto por construcción (la guarda de coerción de arriba ya
  # descarta el caso "presente pero NA", pero esto no depende de ese orden).
  solo_ruta_chk   <- !cotejo_chk$cluster_2 %in% cuotas_cl_chk$cluster_2
  solo_cuotas_chk <- !cotejo_chk$cluster_2 %in% ruta_cl_chk$cluster_2
  ambos_chk <- cotejo_chk[!solo_ruta_chk & !solo_cuotas_chk, ]
  mal_entrevistas_chk <- ambos_chk$entrevistas_meta_cluster != ambos_chk$entrevistas
  mal_puertas_chk <- ambos_chk$presupuesto_puertas_cluster != ambos_chk$puertas

  # sprintf(), nunca paste0(), para armar cada línea: con un vector vacío
  # paste0() recicla a "" (por diseño de R) y deja un mensaje fantasma
  # ("cluster  esta en...") en vez de no aportar nada; sprintf() sí
  # colapsa a character(0) cuando no hay ningún cluster que nombrar.
  problemas_chk <- c(
    if (any(solo_ruta_chk))
      sprintf("cluster %d está en `ruta` pero no en `cuotas`",
              cotejo_chk$cluster_2[solo_ruta_chk]),
    if (any(solo_cuotas_chk))
      sprintf("cluster %d está en `cuotas` pero no en `ruta`",
              cotejo_chk$cluster_2[solo_cuotas_chk]),
    if (any(mal_entrevistas_chk))
      sprintf("cluster %d: entrevistas_meta_cluster (ruta) = %d vs entrevistas (cuotas) = %d",
              ambos_chk$cluster_2[mal_entrevistas_chk],
              ambos_chk$entrevistas_meta_cluster[mal_entrevistas_chk],
              ambos_chk$entrevistas[mal_entrevistas_chk]),
    if (any(mal_puertas_chk))
      sprintf("cluster %d: presupuesto_puertas_cluster (ruta) = %d vs puertas (cuotas) = %d",
              ambos_chk$cluster_2[mal_puertas_chk],
              ambos_chk$presupuesto_puertas_cluster[mal_puertas_chk],
              ambos_chk$puertas[mal_puertas_chk]))

  if (length(problemas_chk)) {
    stop("`ruta` y `cuotas` no describen el mismo plan:\n",
         paste("-", problemas_chk, collapse = "\n"), call. = FALSE)
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

  # ---- guarda: ninguna manzana puede estar a la vez en ruta y en reserva
  # Si `ruta` y `reserva` comparten un (cluster, orden_ruta), el full_join
  # de más abajo duplica esa fila y una sola puerta efectiva se cuenta DOS
  # veces en puertas_vivienda/efectivas, corrompiendo presupuesto_restante
  # y tasa_medida EN SILENCIO (sin error ni warning). Hoy Huehuetoca no
  # tiene traslape (la reserva continúa la ruta sin huecos en los 64
  # clusters), pero esta función vive en un paquete para reusarse en cada
  # encuesta futura: aquí truena en vez de corromper.
  dup_chk <- sorteo |>
    dplyr::count(cluster_2, manzana_num, name = "n_chk") |>
    dplyr::filter(n_chk > 1L)
  if (nrow(dup_chk)) {
    stop("`ruta` y `reserva` comparten manzana(s) (una manzana no puede ",
         "estar a la vez en ruta y en reserva): ",
         paste(sprintf("cluster %d, manzana %d", dup_chk$cluster_2,
                       dup_chk$manzana_num), collapse = "; "),
         call. = FALSE)
  }

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
