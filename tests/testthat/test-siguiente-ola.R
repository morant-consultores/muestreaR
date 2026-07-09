# Olas consecuentes: sobremuestra diferenciada con las tasas de respuesta
# aprendidas del registro de contactos de la ola anterior

marco_ola <- function() {
  tibble::tibble(
    seccion = sprintf("08_%04d", 1:40),
    municipio_cod = rep(c("08_019", "08_037", "08_001", "08_002"), each = 10),
    region = rep(c("Capital", "Juárez", "Resto", "Resto"), each = 10),
    lista_nominal = rep(c(1000, 2000, 500, 800), each = 10) +
      rep(seq(0, 900, by = 100), 4),
    margen_victoria_neto = rep(c(-0.2, 0.2, 0.02, 0.08), each = 10)
  ) |> estratificar_electoral()
}

registro_de <- function(plan, tasa) {
  tibble::tibble(
    seccion = plan$seccion,
    contactos = plan$contactos,
    efectivas = round(plan$contactos * tasa)
  )
}

test_that("panel: conserva secciones y pi, y dosifica contactos por la tasa propia", {
  marco <- marco_ola()
  plan1 <- planear_muestra_seccional(marco, 240, 8, tasa_rechazo = 0.5,
                                     semilla = 3)
  registro <- registro_de(plan1, tasa = 0.5)  # todas con tasa observada 0.5
  plan2 <- planear_siguiente_ola(marco, plan1, registro, metodo = "panel")

  expect_equal(sort(plan2$seccion), sort(plan1$seccion))
  expect_equal(plan2$pi_seccion[order(plan2$seccion)],
               plan1$pi_seccion[order(plan1$seccion)])
  # tasa_hat = tasa propia = tasa estrato = 0.5 -> contactos = 8 / 0.5 = 16
  expect_true(all(plan2$contactos == 16))
})

test_that("panel: el encogimiento mezcla tasa propia y de estrato, con tope", {
  marco <- marco_ola()
  plan1 <- planear_muestra_seccional(marco, 240, 8, tasa_rechazo = 0.5,
                                     semilla = 3)
  registro <- registro_de(plan1, tasa = 0.8)
  # una sección de Juárez costó mucho más que sus pares
  dificil <- plan1$seccion[plan1$region == "Juárez"][1]
  registro$efectivas[registro$seccion == dificil] <- 2   # tasa propia = 2/16

  plan2 <- planear_siguiente_ola(marco, plan1, registro,
                                 metodo = "panel", encogimiento = 0.6,
                                 tasa_minima = 0.1, tope_contactos = 3)
  tasas <- attr(plan2, "tasas")
  t_estrato <- tasas$tasa_estrato[
    tasas$estrato == plan1$estrato[plan1$seccion == dificil]]
  esperada <- 0.6 * (2 / 16) + 0.4 * t_estrato
  obs <- plan2$contactos[plan2$seccion == dificil]
  expect_equal(obs, min(ceiling(8 / esperada), 3 * 8))
  # las fáciles piden menos contactos que la difícil
  expect_true(all(plan2$contactos[plan2$seccion != dificil] < obs))
})

test_that("resortear: secciones nuevas usan la tasa del estrato", {
  marco <- marco_ola()
  plan1 <- planear_muestra_seccional(marco, 240, 8, tasa_rechazo = 0.5,
                                     semilla = 3)
  registro <- registro_de(plan1, tasa = 0.5)
  plan2 <- planear_siguiente_ola(marco, plan1, registro,
                                 metodo = "resortear", semilla = 99)

  expect_true(all(c("seccion", "pi_seccion", "n_plan", "contactos") %in%
                    names(plan2)))
  nuevas <- setdiff(plan2$seccion, plan1$seccion)
  expect_gt(length(nuevas), 0)
  # tasa estrato 0.5 en todos lados -> contactos 16 también en las nuevas
  expect_true(all(plan2$contactos[plan2$seccion %in% nuevas] == 16))
})

test_that("las secciones con cero efectivas se reportan como inoperables", {
  marco <- marco_ola()
  plan1 <- planear_muestra_seccional(marco, 240, 8, tasa_rechazo = 0.5,
                                     semilla = 3)
  registro <- registro_de(plan1, tasa = 0.6)
  registro$efectivas[1:2] <- 0
  plan2 <- planear_siguiente_ola(marco, plan1, registro, metodo = "panel")
  expect_equal(sort(attr(plan2, "inoperables")), sort(registro$seccion[1:2]))
})

test_that("resortear respeta la lista negra actualizada", {
  marco <- marco_ola()
  plan1 <- planear_muestra_seccional(marco, 240, 8, semilla = 3)
  registro <- registro_de(plan1, tasa = 0.7)
  plan2 <- planear_siguiente_ola(
    marco, plan1, registro, metodo = "resortear", semilla = 5,
    lista_negra = list(municipios = "08_001")
  )
  expect_false(any(plan2$seccion %in% sprintf("08_%04d", 21:30)))
})

test_that("valida el esquema del registro de contactos", {
  marco <- marco_ola()
  plan1 <- planear_muestra_seccional(marco, 240, 8, semilla = 3)
  expect_error(
    planear_siguiente_ola(marco, plan1, tibble::tibble(seccion = "x"),
                          metodo = "panel"),
    "contactos"
  )
})

# ---- hallazgos del code review del PR #23 ----

test_that("secciones duplicadas en el registro abortan (no duplican el plan)", {
  marco <- marco_ola()
  plan1 <- planear_muestra_seccional(marco, 240, 8, semilla = 3)
  registro <- registro_de(plan1, tasa = 0.5)
  registro <- rbind(registro, registro[1, ])   # doble carga de la app
  expect_error(
    planear_siguiente_ola(marco, plan1, registro, metodo = "panel"),
    "duplicad"
  )
})

test_that("NA o efectivas > contactos en el registro abortan con mensaje claro", {
  marco <- marco_ola()
  plan1 <- planear_muestra_seccional(marco, 240, 8, semilla = 3)

  con_na <- registro_de(plan1, tasa = 0.5)
  con_na$efectivas[2] <- NA
  expect_error(
    planear_siguiente_ola(marco, plan1, con_na, metodo = "panel"),
    "NA"
  )

  invertido <- registro_de(plan1, tasa = 0.5)
  invertido$efectivas[1] <- invertido$contactos[1] + 5
  expect_error(
    planear_siguiente_ola(marco, plan1, invertido, metodo = "panel"),
    "efectivas"
  )
})

test_that("plan_anterior sin el esquema del plan aborta al inicio", {
  marco <- marco_ola()
  plan1 <- planear_muestra_seccional(marco, 240, 8, semilla = 3)
  registro <- registro_de(plan1, tasa = 0.5)
  expect_error(
    planear_siguiente_ola(marco, plan1 |> dplyr::select(-n_plan), registro,
                          metodo = "panel"),
    "n_plan"
  )
})
