# derivar_plan_seccional: el plan versionado sale del MISMO sorteo de la clase
# (espejo seccional del test de derivar_plan_ageb)

test_that("derivar_plan_seccional reconstruye el contrato seccional desde la clase", {
  pob <- generar_poblacion_ine()
  est <- data.frame(estrato = c("Region 1", "Region 2"),
                    entrevistas = c(50, 50),
                    tasa_rechazo = c(0.5, 0.2))

  d <- disenar_muestra_ine(pob, est, semilla = 123)
  plan <- derivar_plan_seccional(d)

  # contrato de la capa 1 de encuestar::construir_diseno_capas
  expect_named(plan, c("seccion", "estrato", "ln_seccion", "pi_seccion",
                       "n_plan", "contactos"))
  expect_identical(attr(plan, "unidad"), "seccion")
  expect_identical(attr(plan, "parametros")$origen, "clase")

  # una fila por sección sorteada, exactamente las de la muestra
  ult <- d$muestra |> purrr::pluck(length(d$muestra)) |> tidyr::unnest(data)
  expect_setequal(plan$seccion, unique(ult$SECCION))

  # pi = el fpc del nivel de sección del marco (mismo sorteo, cero recalculo)
  marco <- d$poblacion$marco_muestral
  col_fpc <- paste0("fpc_", d$ultimo_nivel)
  fpc_marco <- marco |>
    dplyr::filter(SECCION %in% plan$seccion) |>
    dplyr::distinct(SECCION, .data[[col_fpc]])
  cotejo <- dplyr::left_join(plan, fpc_marco, by = c("seccion" = "SECCION"))
  expect_equal(cotejo$pi_seccion, cotejo[[col_fpc]])
  expect_true(all(plan$pi_seccion > 0 & plan$pi_seccion <= 1))

  # dosis por estrato: n_plan = efectivas/secciones, contactos las infla por
  # la tasa de rechazo del estrato
  asig <- attr(d, "asignacion")
  dosis <- asig |>
    dplyr::transmute(estrato = as.character(estrato),
                     n_plan = round(entrevistas / secciones),
                     contactos = round(entrevistas_a_levantar / secciones))
  cotejo2 <- plan |> dplyr::distinct(estrato, n_plan, contactos) |>
    dplyr::arrange(estrato)
  expect_equal(cotejo2, dosis |> dplyr::arrange(estrato) |> tibble::as_tibble(),
               ignore_attr = TRUE)
  expect_true(all(plan$contactos >= plan$n_plan))

  # ln_seccion = lista nominal de la sección en el marco completo
  ln_marco <- marco |>
    dplyr::filter(SECCION %in% plan$seccion) |>
    dplyr::group_by(SECCION) |>
    dplyr::summarise(ln = sum(lista_nominal, na.rm = TRUE), .groups = "drop")
  cotejo3 <- dplyr::left_join(plan, ln_marco, by = c("seccion" = "SECCION"))
  expect_equal(cotejo3$ln_seccion, cotejo3$ln)

  # plan_para_capas es la identidad sobre un plan ya seccional
  capas <- plan_para_capas(plan)
  expect_named(capas, names(plan))
})

test_that("derivar_plan_seccional exige asignación y fpc", {
  pob <- generar_poblacion_ine()
  est <- data.frame(estrato = c("Region 1", "Region 2"), entrevistas = c(50, 50))
  d <- disenar_muestra_ine(pob, est, semilla = 123)

  attr(d, "asignacion") <- NULL
  expect_error(derivar_plan_seccional(d), "asignaci")
})
