# Etapa II del diseño por conglomerados: manzanas con PPT dentro de cada
# UPM del plan (espejo de la Etapa II de Enkoll: 2 manzanas por AGEB).
# Usa marco_ageb_prueba() de helper-fixture.R.

plan_manzanas_fixture <- function() {
  marco <- marco_ageb_prueba()
  plan <- suppressWarnings(
    planear_muestra_ageb(marco, 200, 10, dominio = "region",
                         tasa_rechazo = 0.5, semilla = 5)
  )
  mzas <- tidyr::expand_grid(ageb = marco$ageb, mza = sprintf("%03d", 1:5)) |>
    dplyr::mutate(manzana = paste0(ageb, mza),
                  pobtot = 50 + (dplyr::row_number() %% 7) * 40,
                  pob18 = round(pobtot * 0.7))
  list(plan = plan, mzas = mzas)
}

test_that("seleccionar_manzanas: k por UPM, pi sobre todas las manzanas y reparto exacto", {
  fx <- plan_manzanas_fixture()
  sel <- seleccionar_manzanas(fx$plan, fx$mzas, manzanas_por_upm = 2,
                              semilla = 11)
  expect_equal(nrow(sel), 2 * nrow(fx$plan))
  expect_true(all(c("ageb", "manzana", "pi_manzana", "n_plan", "contactos")
                  %in% names(sel)))
  expect_equal(attr(sel, "unidad"), "ageb")

  # el reparto conserva los totales del plan en cada AGEB
  tot <- sel |>
    dplyr::group_by(ageb) |>
    dplyr::summarise(n = sum(n_plan), contactos = sum(contactos))
  expect_true(all(tot$n == 10))
  expect_true(all(tot$contactos == 20))

  # pi calculadas sobre TODAS las manzanas del AGEB (no las sorteadas)
  a1 <- fx$plan$ageb[1]
  mzas_a1 <- fx$mzas[fx$mzas$ageb == a1, ]
  esperadas <- sampling::inclusionprobabilities(mzas_a1$pobtot, 2)
  obs <- sel[sel$ageb == a1, ]
  expect_equal(obs$pi_manzana,
               esperadas[match(obs$manzana, mzas_a1$manzana)])

  # reproducible con la misma semilla
  sel2 <- seleccionar_manzanas(fx$plan, fx$mzas, manzanas_por_upm = 2,
                               semilla = 11)
  expect_equal(sel$manzana, sel2$manzana)
})

test_that("UPM con menos manzanas que las pedidas: entran todas con pi 1 y aviso", {
  fx <- plan_manzanas_fixture()
  pocas <- fx$mzas |>
    dplyr::group_by(ageb) |>
    dplyr::slice(1) |>
    dplyr::ungroup()
  expect_warning(
    sel <- seleccionar_manzanas(fx$plan, pocas, manzanas_por_upm = 2,
                                semilla = 1),
    "menos manzanas"
  )
  expect_true(all(sel$pi_manzana == 1))
  expect_true(all(sel$n_plan == 10))   # todo el plan cae en la única manzana
  expect_true(all(sel$contactos == 20))
})

test_that("UPM con todas las manzanas enmascaradas: sorteo equiprobable con aviso", {
  fx <- plan_manzanas_fixture()
  fx$mzas$pobtot[fx$mzas$ageb == fx$plan$ageb[1]] <- NA
  expect_message(
    sel <- seleccionar_manzanas(fx$plan, fx$mzas, manzanas_por_upm = 2,
                                semilla = 3),
    "equiprobable"
  )
  obs <- sel[sel$ageb == fx$plan$ageb[1], ]
  expect_equal(nrow(obs), 2)
  expect_equal(obs$pi_manzana, rep(2 / 5, 2))
})

test_that("UPM del plan sin manzanas en el marco detiene con las llaves", {
  fx <- plan_manzanas_fixture()
  sin <- fx$mzas |> dplyr::filter(ageb != fx$plan$ageb[1])
  expect_error(
    seleccionar_manzanas(fx$plan, sin, manzanas_por_upm = 2),
    fx$plan$ageb[1]
  )
})

test_that("seleccionar_manzanas opera también sobre planes seccionales", {
  marco <- tibble::tibble(
    seccion = sprintf("15_%04d", 1:10),
    region = "A", estrato = "A",
    lista_nominal = 500 + 1:10 * 10
  )
  plan <- planear_muestra_seccional(marco, 40, 8, semilla = 3)
  mzas <- tidyr::expand_grid(seccion = marco$seccion,
                             mza = sprintf("%03d", 1:4)) |>
    dplyr::mutate(manzana = paste0(seccion, "-", mza), pobtot = 100)
  sel <- seleccionar_manzanas(plan, mzas, manzanas_por_upm = 2, semilla = 2)
  expect_true("seccion" %in% names(sel))
  expect_equal(nrow(sel), 2 * nrow(plan))
  expect_true(all(sel$n_plan == 4))    # 8 efectivas repartidas en 2 manzanas
})
