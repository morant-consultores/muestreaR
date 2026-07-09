# Muestra por AGEB: motor UPM genérico detrás del plan seccional, cara
# planear_muestra_ageb, puente al contrato de encuestar y olas por unidad.
# El fixture marco_ageb_prueba() vive en helper-fixture.R (lo comparten los
# tests de manzanas).

# ---- asignar_potencia por unidad ----

test_that("asignar_potencia nombra las columnas de conteo por unidad", {
  asig <- asignar_potencia(marco_ageb_prueba(), n_total = 200,
                           m_por_seccion = 10,
                           variable_tamano = "pob18", unidad = "ageb")
  expect_true(all(c("agebs", "agebs_disponibles") %in% names(asig)))
  expect_false(any(c("secciones", "secciones_disponibles") %in% names(asig)))
  expect_equal(asig$entrevistas_plan, asig$agebs * 10)
})

# ---- aplicar_lista_negra por unidad ----

test_that("aplicar_lista_negra documenta con la llave y el tamaño de la unidad", {
  marco <- marco_ageb_prueba()
  res <- aplicar_lista_negra(marco, secciones = marco$ageb[1:2],
                             municipios = "15058",
                             llave_seccion = "ageb",
                             variable_tamano = "pob18",
                             unidad = "ageb")
  doc <- attr(res, "lista_negra")
  expect_true(all(c("ageb", "estrato", "pob18", "motivo") %in% names(doc)))
  expect_false("seccion" %in% names(doc))
  expect_setequal(unique(doc$motivo),
                  c("municipio en lista negra", "ageb en lista negra"))
  expect_equal(nrow(res), 18)
  expect_equal(nrow(doc), 22)
})

# ---- planear_muestra_upm (el motor) ----

test_that("planear_muestra_upm emite el plan con columnas por unidad", {
  plan <- planear_muestra_upm(
    marco_ageb_prueba(), n_total = 200, m_por_upm = 10,
    llave_upm = "ageb", unidad = "ageb",
    variable_tamano = "pob18", dominio = "region",
    tasa_rechazo = 0.5, semilla = 15
  )
  expect_true(all(c("ageb", "region", "estrato", "ln_ageb", "pi_ageb",
                    "n_plan", "contactos") %in% names(plan)))
  expect_true(all(plan$n_plan == 10))
  expect_true(all(plan$contactos == 20))          # 0.5 de rechazo -> 2x
  expect_equal(attr(plan, "unidad"), "ageb")
  expect_equal(attr(plan, "parametros")$unidad, "ageb")

  # pi del diseño: sobre TODO el estrato, no sobre las sorteadas
  asig <- attr(plan, "asignacion")
  expect_true("agebs" %in% names(asig))
  marco <- marco_ageb_prueba()
  for (h in unique(plan$estrato)) {
    dispo <- marco[marco$estrato == h, ]
    n_h <- asig$agebs[asig$estrato == h]
    esperadas <- sampling::inclusionprobabilities(dispo$pob18, n_h)
    obs <- plan[plan$estrato == h, ]
    expect_equal(nrow(obs), n_h)
    expect_equal(obs$pi_ageb, esperadas[match(obs$ageb, dispo$ageb)])
  }
})

test_that("el motor UPM acepta lista negra con llave genérica 'upms' o por unidad", {
  marco <- marco_ageb_prueba()
  plan <- planear_muestra_upm(
    marco, 200, 10, llave_upm = "ageb", unidad = "ageb",
    variable_tamano = "pob18", dominio = "region",
    lista_negra = list(upms = marco$ageb[1:3]),
    semilla = 1
  )
  expect_false(any(plan$ageb %in% marco$ageb[1:3]))
  expect_equal(names(attr(plan, "lista_negra"))[1], "ageb")

  plan2 <- planear_muestra_upm(
    marco, 200, 10, llave_upm = "ageb", unidad = "ageb",
    variable_tamano = "pob18", dominio = "region",
    lista_negra = list(agebs = marco$ageb[1:3]),
    semilla = 1
  )
  expect_equal(plan2$ageb, plan$ageb)
})

# ---- plan_para_capas ----

test_that("plan_para_capas renombra al contrato seccional de encuestar", {
  plan <- planear_muestra_ageb(marco_ageb_prueba(), 200, 10,
                               dominio = "region", semilla = 2)
  capas <- plan_para_capas(plan)
  expect_true(all(c("seccion", "pi_seccion", "ln_seccion", "n_plan",
                    "contactos") %in% names(capas)))
  expect_equal(capas$seccion, plan$ageb)
  expect_equal(capas$pi_seccion, plan$pi_ageb)
  expect_equal(attr(capas, "unidad_original"), "ageb")
  # los atributos del plan sobreviven al renombre
  expect_equal(attr(capas, "asignacion"), attr(plan, "asignacion"))
})

test_that("plan_para_capas es identidad sobre planes seccionales", {
  plan_sec <- tibble::tibble(seccion = "x", pi_seccion = 0.1, n_plan = 8)
  attr(plan_sec, "unidad") <- "seccion"
  expect_identical(plan_para_capas(plan_sec), plan_sec)
  # sin attr de unidad se asume seccional (planes viejos versionados)
  plan_viejo <- tibble::tibble(seccion = "x", pi_seccion = 0.1, n_plan = 8)
  expect_identical(plan_para_capas(plan_viejo), plan_viejo)
})

# ---- planear_muestra_ageb ----

test_that("planear_muestra_ageb es el espejo del flujo seccional con defaults censales", {
  plan <- planear_muestra_ageb(marco_ageb_prueba(), n_total = 200,
                               m_por_ageb = 10, dominio = "region",
                               tasa_rechazo = 0.5, semilla = 8)
  expect_true(all(c("ageb", "ln_ageb", "pi_ageb", "n_plan", "contactos")
                  %in% names(plan)))
  expect_equal(attr(plan, "unidad"), "ageb")
  expect_true(all(plan$contactos == 20))
  expect_equal(sum(plan$n_plan), sum(attr(plan, "asignacion")$entrevistas_plan))
})

test_that("planear_muestra_ageb acepta lista negra de agebs y municipios", {
  marco <- marco_ageb_prueba()
  plan <- planear_muestra_ageb(
    marco, 100, 10, dominio = "region", semilla = 2,
    lista_negra = list(agebs = marco$ageb[1:2], municipios = "15058")
  )
  expect_false(any(plan$ageb %in% marco$ageb[1:2]))
  expect_false(any(plan$ageb %in% marco$ageb[marco$municipio_cod == "15058"]))
})

test_that("planear_muestra_seccional conserva su contrato exacto (delegación)", {
  marco <- tibble::tibble(
    seccion = sprintf("15_%04d", 1:20),
    region = rep(c("A", "B"), each = 10),
    estrato = rep(c("A", "B"), each = 10),
    lista_nominal = 500 + 1:20 * 10
  )
  plan <- planear_muestra_seccional(marco, 80, 8, semilla = 3)
  expect_true(all(c("seccion", "ln_seccion", "pi_seccion", "n_plan",
                    "contactos") %in% names(plan)))
  expect_equal(attr(plan, "unidad"), "seccion")
  expect_true("secciones" %in% names(attr(plan, "asignacion")))
})

# ---- planear_siguiente_ola por unidad ----

test_that("planear_siguiente_ola opera sobre planes AGEB (registro por ageb)", {
  marco <- marco_ageb_prueba()
  plan1 <- planear_muestra_ageb(marco, 200, 10, dominio = "region",
                                tasa_rechazo = 0.5, semilla = 4)
  registro <- plan1 |>
    dplyr::transmute(ageb, contactos = 20,
                     efectivas = rep(c(10, 5), length.out = dplyr::n()))

  plan2 <- planear_siguiente_ola(marco, plan1, registro,
                                 metodo = "resortear", dominio = "region",
                                 variable_tamano = "pob18", semilla = 9)
  expect_true(all(c("ageb", "pi_ageb", "n_plan", "contactos")
                  %in% names(plan2)))
  expect_equal(attr(plan2, "unidad"), "ageb")
  expect_s3_class(attr(plan2, "tasas"), "tbl_df")
  expect_equal(attr(plan2, "metodo"), "resortear")

  # panel conserva agebs, pi y re-dosifica contactos
  plan2p <- planear_siguiente_ola(marco, plan1, registro, metodo = "panel")
  expect_equal(plan2p$ageb, plan1$ageb)
  expect_equal(plan2p$pi_ageb, plan1$pi_ageb)
  # tasa 50% (10/20) y 25%->tope 3x: contactos re-dosificados por ageb
  expect_true(all(plan2p$contactos >= plan1$n_plan))
})

test_that("planear_siguiente_ola valida el registro con la llave de la unidad", {
  marco <- marco_ageb_prueba()
  plan1 <- planear_muestra_ageb(marco, 200, 10, dominio = "region",
                                semilla = 4)
  registro_mal <- tibble::tibble(seccion = plan1$ageb, contactos = 20,
                                 efectivas = 10)
  expect_error(
    planear_siguiente_ola(marco, plan1, registro_mal, metodo = "panel"),
    "ageb"
  )
})
