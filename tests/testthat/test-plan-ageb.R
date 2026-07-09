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
