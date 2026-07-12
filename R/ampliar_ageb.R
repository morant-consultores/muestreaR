# Ampliación ADITIVA de la muestra de manzanas (diseño sin revisita) --------

#' Ampliar aditivamente las manzanas por AGEB de un diseño en campo
#'
#' Sorteo **complementario** de manzanas dentro de cada AGEB ya sorteado,
#' para cuando el rendimiento por puerta observado exige más sobremuestra de
#' la planeada (diseño sin revisita ni sustitución: el colchón son las
#' manzanas). La restricción que gobierna todo: **la muestra que ya está en
#' campo no se toca** — las manzanas originales quedan idénticas (misma fila,
#' mismo `cluster_0`) y conservan su número corto impreso en mapas y
#' cuestionario ([numerar_manzanas()] respeta el attr `"numeracion_base"`
#' que esta función fija); las nuevas continúan la numeración (5, 6, ...).
#'
#' El sorteo complementario es equiprobable dentro del AGEB — el mismo
#' esquema de la Etapa II de la clase ([muestrear()]) — excluyendo las
#' manzanas ya sorteadas: la probabilidad final de cada manzana del AGEB es
#' k'/N (simétrica), como si se hubieran sorteado k' desde el inicio. Los
#' AGEBs con menos manzanas disponibles que el objetivo entran completos
#' (se declara en el attr `"ampliacion"`); el faltante se resuelve en el
#' siguiente top-up o con [planear_siguiente_ola()].
#'
#' Se devuelve un **clon**: el diseño original no se modifica. En el clon se
#' actualizan `n` (total a levantar), `n_i` (m/n del último nivel), la
#' asignación (`entrevistas_a_levantar` recalculado; `tasa_rechazo` pasa a
#' ser la tasa de no-logro implícita `1 - efectivas/levantar`, con la que
#' [resumen_operativo()] imprime las entrevistas planeadas por mapa) y el
#' plan versionado (`pi_ageb` intactas — mismo sorteo de Etapa I —,
#' `contactos` por AGEB = suma real de `n_0` de sus manzanas). Si el diseño
#' traía cuotas, se recalculan sobre la muestra ampliada.
#'
#' @param diseño Objeto [Diseño] con la muestra extraída (típicamente el
#'   `diseño.rda` exportado que está operando en campo).
#' @param manzanas_por_ageb Objetivo TOTAL de manzanas por AGEB (incluidas
#'   las que ya están en campo).
#' @param semilla Semilla del sorteo complementario (documentarla: es parte
#'   del plan versionado de la ampliación).
#'
#' @return Un [Diseño] nuevo con la muestra ampliada y los atributos
#'   `"asignacion"`, `"plan_ageb"`, `"numeracion_base"` y `"ampliacion"`.
#' @seealso [numerar_manzanas()], [derivar_plan_ageb()],
#'   [planear_siguiente_ola()]
#' @export
ampliar_manzanas_ageb <- function(diseño, manzanas_por_ageb, semilla = NULL) {
  if (is.null(diseño$muestra)) {
    stop("El diseño no tiene muestra extraída: no hay nada que ampliar.",
         call. = FALSE)
  }
  u_nivel <- diseño$niveles %>% filter(nivel == diseño$ultimo_nivel)
  u_cluster <- paste(u_nivel$tipo, u_nivel$nivel, sep = "_")
  var_ageb <- u_nivel$variable

  ultimo <- length(diseño$muestra)
  actual <- diseño$muestra %>% purrr::pluck(ultimo)
  # la numeración que campo YA tiene, ANTES de tocar nada (si el diseño ya
  # venía de una ampliación previa, esto la respeta y permite encadenar)
  base_num <- numerar_manzanas(diseño)

  marco <- diseño$poblacion$marco_muestral %>% ungroup()

  faltantes <- actual %>%
    count(.data[[u_cluster]], name = "en_campo") %>%
    mutate(faltan = pmax(0L, as.integer(manzanas_por_ageb) - en_campo))

  candidatas <- marco %>%
    dplyr::semi_join(actual, by = u_cluster) %>%
    dplyr::anti_join(actual, by = "cluster_0") %>%
    dplyr::inner_join(
      faltantes %>% filter(faltan > 0) %>%
        select(dplyr::all_of(u_cluster), faltan),
      by = u_cluster
    )

  if (!is.null(semilla)) set.seed(semilla)
  nuevas <- candidatas %>%
    group_by(dplyr::across(dplyr::all_of(u_cluster))) %>%
    dplyr::group_modify(~ dplyr::slice_sample(.x, n = min(.x$faltan[1],
                                                          nrow(.x)))) %>%
    ungroup() %>%
    select(-faltan)

  # anidar las nuevas con la MISMA estructura que muestrear() (una fila por
  # manzana: llaves de nivel + total + data), alineando el esquema interno
  # con el de la muestra en campo (el marco pudo ganar columnas después del
  # sorteo original, p. ej. parches de insumos)
  nuevas_nested <- marco %>%
    filter(cluster_0 %in% nuevas$cluster_0) %>%
    agrupar_nivel("cluster_0") %>%
    mutate(total = sum(.data[[diseño$variable_poblacional]], na.rm = TRUE)) %>%
    group_by(total, .add = TRUE) %>%
    tidyr::nest() %>%
    ungroup()
  cols_data <- names(actual$data[[1]])
  nuevas_nested$data <- lapply(
    nuevas_nested$data,
    function(x) x[, intersect(cols_data, names(x)), drop = FALSE]
  )

  d2 <- diseño$clone(deep = TRUE)
  d2$muestra[[ultimo]] <- dplyr::bind_rows(actual, nuevas_nested) %>%
    arrange(.data[[u_cluster]], cluster_0)

  # ---- totales y plan interno (n, n_i, niveles) ----------------------------
  por_manzana <- d2$muestra[[ultimo]] %>%
    select(-data) %>%
    left_join(d2$n_i$cluster_0, by = "cluster_0")
  d2$n <- sum(por_manzana$n_0)
  d2$niveles <- d2$niveles %>%
    mutate(unidades = if_else(nivel == 0, as.numeric(nrow(por_manzana)),
                              as.numeric(unidades)))

  tab <- d2$n_i[[u_cluster]] %>% ungroup()
  m_col <- paste0("m_", u_nivel$nivel)
  n_col <- paste0("n_", u_nivel$nivel)
  llaves_tab <- setdiff(names(tab), c(m_col, n_col))
  reales_nivel <- por_manzana %>%
    group_by(dplyr::across(dplyr::all_of(llaves_tab))) %>%
    summarise("{m_col}.real" := dplyr::n(),
              "{n_col}.real" := sum(n_0), .groups = "drop")
  d2$n_i[[u_cluster]] <- tab %>%
    left_join(reales_nivel, by = llaves_tab) %>%
    mutate(
      !!m_col := dplyr::coalesce(.data[[paste0(m_col, ".real")]],
                                 .data[[m_col]]),
      !!n_col := dplyr::coalesce(.data[[paste0(n_col, ".real")]],
                                 .data[[n_col]])
    ) %>%
    select(-dplyr::ends_with(".real"))

  # ---- asignación del modelo operativo (attr "asignacion") -----------------
  asig <- attr(diseño, "asignacion")
  asig2 <- NULL
  if (!is.null(asig)) {
    var_estrato <- diseño$niveles$variable[diseño$niveles$nivel == 1]
    lev <- por_manzana %>%
      left_join(marco %>% distinct(strata_1, .data[[var_estrato]]),
                by = "strata_1") %>%
      group_by(.data[[var_estrato]]) %>%
      summarise(.levantar = sum(n_0), .manzanas = dplyr::n(),
                .groups = "drop")
    asig2 <- asig %>%
      mutate(estrato = as.character(estrato)) %>%
      left_join(lev %>% mutate("{var_estrato}" := as.character(
        .data[[var_estrato]])),
        by = c("estrato" = var_estrato)) %>%
      mutate(
        entrevistas_a_levantar = .levantar,
        manzanas_por_seccion = .manzanas / secciones,
        factor = entrevistas_a_levantar / entrevistas,
        tasa_rechazo = 1 - entrevistas / entrevistas_a_levantar
      ) %>%
      select(dplyr::all_of(names(asig)))
    attr(d2, "asignacion") <- asig2
  }

  # ---- plan versionado (attr "plan_ageb") ----------------------------------
  if (!is.null(asig2)) {
    plan2 <- derivar_plan_ageb(d2, asignacion = asig2)
    contactos_reales <- por_manzana %>%
      left_join(marco %>% distinct(cluster_0, .data[[var_ageb]]),
                by = "cluster_0") %>%
      group_by(.data[[var_ageb]]) %>%
      summarise(.contactos = sum(n_0), .groups = "drop")
    plan2 <- plan2 %>%
      left_join(contactos_reales, by = c("ageb" = var_ageb)) %>%
      mutate(contactos = .contactos) %>%
      select(-.contactos)
    attr(d2, "plan_ageb") <- plan2
  }

  attr(d2, "numeracion_base") <- base_num
  attr(d2, "ampliacion") <- list(
    objetivo_por_ageb = manzanas_por_ageb,
    semilla = semilla,
    manzanas_previas = nrow(actual),
    manzanas_nuevas = nrow(nuevas_nested),
    agebs_cortos = faltantes %>%
      dplyr::anti_join(
        por_manzana %>% count(.data[[u_cluster]], name = "logradas") %>%
          filter(logradas >= manzanas_por_ageb),
        by = u_cluster
      ) %>%
      dplyr::pull(dplyr::all_of(u_cluster))
  )

  if (!is.null(diseño$cuotas)) d2$calcular_cuotas()
  d2
}
