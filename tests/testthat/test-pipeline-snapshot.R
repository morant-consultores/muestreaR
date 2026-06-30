# Snapshot de regresión del pipeline completo DiseñoINE sobre el fixture sintético.
# La PRIMERA vez que se corre, testthat graba el baseline en _snaps/. A partir de
# ahí, cualquier cambio que altere la muestra o las cuotas hará fallar el test.
# Para (re)grabar a propósito: testthat::snapshot_accept("pipeline-snapshot").

test_that("el pipeline DiseñoINE produce una muestra estable (semilla = 123)", {
  diseno <- generar_diseno_ine(semilla = 123)
  diseno$extraer_muestra(1); diseno$extraer_muestra(2)

  resumen_muestra <- diseno$muestra |>
    purrr::pluck(length(diseno$muestra)) |>
    tidyr::unnest(data) |>
    dplyr::count(region, MUNICIPIO, SECCION, name = "manzanas") |>
    dplyr::arrange(region, MUNICIPIO, SECCION)

  expect_snapshot(as.data.frame(resumen_muestra))
})

test_that("las cuotas del pipeline son estables (semilla = 123)", {
  diseno <- generar_diseno_ine(semilla = 123)
  diseno$extraer_muestra(1); diseno$extraer_muestra(2)
  diseno$calcular_cuotas()

  cuotas <- diseno$cuotas |>
    dplyr::arrange(dplyr::across(dplyr::everything()))

  expect_snapshot(as.data.frame(cuotas))
})
