# Muestra seccional: estratificación electoral, asignación por potencia,
# lista negra y plan versionado (contrato con encuestar::construir_diseno_capas)

marco_prueba <- function() {
  tibble::tibble(
    seccion = sprintf("08_%04d", 1:40),
    municipio_cod = rep(c("08_019", "08_037", "08_001", "08_002"), each = 10),
    region = rep(c("Capital", "Juárez", "Resto", "Resto"), each = 10),
    lista_nominal = rep(c(1000, 2000, 500, 800), each = 10) +
      rep(seq(0, 900, by = 100), 4),
    margen_victoria_neto = rep(c(-0.2, 0.2, 0.02, 0.08), each = 10)
  )
}

# ---- estratificar_electoral ----

test_that("estratificar_electoral clasifica por los cortes del MVN", {
  marco <- marco_prueba() |> dplyr::select(-margen_victoria_neto)
  marco$margen_victoria_neto <- c(0.16, 0.15, 0.06, 0.05, 0.0, -0.05, -0.06, -0.2,
                                  rep(0.2, 32))
  res <- estratificar_electoral(marco)
  expect_equal(
    res$tipo_electoral[1:8],
    c("Duro oficialista", "Blando", "Blando", "Competitiva", "Competitiva",
      "Competitiva", "Opositor", "Opositor")
  )
  expect_equal(res$estrato[1], "Capital / Duro oficialista")
})

test_that("estratificar_electoral avisa por secciones sin MVN y deja NA", {
  marco <- marco_prueba()
  marco$margen_victoria_neto[c(3, 7)] <- NA
  expect_warning(res <- estratificar_electoral(marco), "sin MVN")
  expect_true(all(is.na(res$estrato[c(3, 7)])))
  expect_false(anyNA(res$estrato[-c(3, 7)]))
})

test_that("estratificar_electoral valida columnas", {
  expect_error(estratificar_electoral(marco_prueba(), mvn = "no_existe"),
               "no_existe")
})

# ---- asignar_potencia ----

test_that("asignar_potencia con potencia 1 es proporcional y 0 igualitaria por dominio", {
  marco <- estratificar_electoral(marco_prueba())
  prop <- asignar_potencia(marco, n_total = 400, m_por_seccion = 8, potencia = 1)
  dom_prop <- attr(prop, "dominios")
  expect_equal(
    dom_prop$entrevistas_dominio,
    repartir_cociente(400, 400 * dom_prop$ln_dominio / sum(dom_prop$ln_dominio))
  )

  igual <- asignar_potencia(marco, n_total = 400, m_por_seccion = 8, potencia = 0)
  dom_igual <- attr(igual, "dominios")
  expect_true(max(dom_igual$entrevistas_dominio) -
                min(dom_igual$entrevistas_dominio) <= 1)
})

test_that("asignar_potencia raíz-cuadrada queda entre proporcional e igualitaria", {
  marco <- estratificar_electoral(marco_prueba())
  raiz <- attr(asignar_potencia(marco, 400, 8, potencia = 0.5), "dominios")
  prop <- attr(asignar_potencia(marco, 400, 8, potencia = 1), "dominios")
  # Juárez es el dominio más grande: con raíz recibe MENOS que proporcional
  ent_raiz <- raiz$entrevistas_dominio[raiz$region == "Juárez"]
  ent_prop <- prop$entrevistas_dominio[prop$region == "Juárez"]
  expect_lt(ent_raiz, ent_prop)
})

test_that("asignar_potencia respeta min_secciones y las disponibles", {
  marco <- estratificar_electoral(marco_prueba())
  asig <- asignar_potencia(marco, n_total = 64, m_por_seccion = 8,
                           potencia = 0.5, min_secciones = 2)
  expect_true(all(asig$secciones >= 2))
  expect_true(all(asig$secciones <= asig$secciones_disponibles))
  expect_equal(asig$entrevistas_plan, asig$secciones * 8)
})

test_that("asignar_potencia ignora secciones sin estrato", {
  marco <- estratificar_electoral(marco_prueba())
  marco$estrato[1:2] <- NA
  asig <- asignar_potencia(marco, 400, 8)
  expect_equal(sum(asig$secciones_disponibles), 38)
})

# ---- aplicar_lista_negra ----

test_that("aplicar_lista_negra excluye y documenta", {
  marco <- estratificar_electoral(marco_prueba())
  res <- aplicar_lista_negra(marco, municipios = "08_019",
                             secciones = "08_0011")
  expect_equal(nrow(res), 29)
  doc <- attr(res, "lista_negra")
  expect_equal(nrow(doc), 11)
  expect_true(all(c("seccion", "estrato", "lista_nominal", "motivo") %in% names(doc)))
})

test_that("aplicar_lista_negra avisa si un estrato queda corto", {
  marco <- estratificar_electoral(marco_prueba())
  expect_warning(
    aplicar_lista_negra(marco, secciones = sprintf("08_%04d", 1:9)),
    "Capital"
  )
})

# ---- planear_muestra_seccional ----

test_that("el plan cumple el contrato de encuestar y las pi son las del marco completo", {
  marco <- estratificar_electoral(marco_prueba())
  plan <- planear_muestra_seccional(marco, n_total = 240, m_por_seccion = 8,
                                    semilla = 42)
  expect_true(all(c("seccion", "region", "estrato", "ln_seccion",
                    "pi_seccion", "n_plan", "contactos") %in% names(plan)))
  expect_true(all(plan$n_plan == 8))

  # pi exactas: recalculadas sobre TODO el estrato (no sobre las sorteadas)
  asig <- attr(plan, "asignacion")
  for (h in unique(plan$estrato)) {
    secs_h <- marco[marco$estrato == h, ]
    n_h <- asig$secciones[asig$estrato == h]
    esperadas <- tibble::tibble(
      seccion = secs_h$seccion,
      pi = sampling::inclusionprobabilities(secs_h$lista_nominal, n_h)
    )
    obs <- plan[plan$estrato == h, ]
    expect_equal(nrow(obs), n_h)
    expect_equal(obs$pi_seccion,
                 esperadas$pi[match(obs$seccion, esperadas$seccion)])
  }
})

test_that("planear_muestra_seccional es reproducible con semilla", {
  marco <- estratificar_electoral(marco_prueba())
  p1 <- planear_muestra_seccional(marco, 240, 8, semilla = 7)
  p2 <- planear_muestra_seccional(marco, 240, 8, semilla = 7)
  expect_equal(p1$seccion, p2$seccion)
})

test_that("la tasa de rechazo infla los contactos, no el plan", {
  marco <- estratificar_electoral(marco_prueba())
  plan <- planear_muestra_seccional(marco, 240, 8, tasa_rechazo = 0.5,
                                    semilla = 1)
  expect_true(all(plan$n_plan == 8))
  expect_true(all(plan$contactos == 16))
})

test_that("planear_muestra_seccional aplica la lista negra antes del sorteo", {
  marco <- estratificar_electoral(marco_prueba())
  plan <- planear_muestra_seccional(
    marco, 240, 8, semilla = 1,
    lista_negra = list(municipios = "08_001")
  )
  # el municipio 08_001 son las secciones 21 a 30: ninguna puede salir sorteada
  expect_false(any(plan$seccion %in% sprintf("08_%04d", 21:30)))
  expect_s3_class(attr(plan, "lista_negra"), "tbl_df")
})

test_that("planear_muestra_seccional excluye secciones sin estrato con aviso", {
  marco <- estratificar_electoral(marco_prueba())
  marco$margen_victoria_neto[5] <- NA
  marco$estrato[5] <- NA
  expect_message(
    plan <- planear_muestra_seccional(marco, 240, 8, semilla = 1),
    "sin estrato"
  )
  expect_false("08_0005" %in% plan$seccion)
})
