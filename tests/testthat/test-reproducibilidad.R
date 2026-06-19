# Verifica que el parámetro `semilla` hace el diseño reproducible y que, sin él,
# el comportamiento histórico se conserva (no se llama a set.seed).

muestra_final <- function(diseno) {
  diseno$muestra |>
    purrr::pluck(length(diseno$muestra)) |>
    tidyr::unnest(data) |>
    dplyr::arrange(id)
}

test_that("con la misma semilla, dos diseños producen la misma muestra y cuotas", {
  d1 <- generar_diseno_ine(semilla = 123)
  d1$extraer_muestra(1); d1$extraer_muestra(2)
  d1$calcular_cuotas()

  d2 <- generar_diseno_ine(semilla = 123)
  d2$extraer_muestra(1); d2$extraer_muestra(2)
  d2$calcular_cuotas()

  expect_equal(muestra_final(d1), muestra_final(d2))
  expect_equal(d1$cuotas, d2$cuotas)
})

test_that("semillas distintas pueden producir muestras distintas", {
  da <- generar_diseno_ine(semilla = 1)
  da$extraer_muestra(1); da$extraer_muestra(2)

  db <- generar_diseno_ine(semilla = 999)
  db$extraer_muestra(1); db$extraer_muestra(2)

  # No es estrictamente garantizado, pero con marcos de este tamaño debe diferir.
  secc_a <- muestra_final(da)$SECCION |> unique() |> sort()
  secc_b <- muestra_final(db)$SECCION |> unique() |> sort()
  expect_false(identical(secc_a, secc_b))
})

test_that("sin semilla (NULL) el diseño sigue funcionando (retrocompatibilidad)", {
  d <- generar_diseno_ine(semilla = NULL)
  expect_null(d$semilla)
  expect_error(
    { d$extraer_muestra(1); d$extraer_muestra(2) },
    NA   # no debe lanzar error
  )
})
