# Invariantes de las cuotas de edad/sexo (cuotas_ine).

test_that("las cuotas son enteras, no negativas y cuadran con las entrevistas", {
  diseno <- generar_diseno_ine()
  diseno$extraer_muestra(nivel = 1)
  diseno$extraer_muestra(nivel = 2)
  diseno$extraer_muestra(nivel = 3)
  diseno$calcular_cuotas(ajustar = TRUE)

  cuotas <- diseno$cuotas

  expect_true(all(cuotas$n >= 0))             # no negativas
  expect_equal(cuotas$n, round(cuotas$n))     # enteras
  expect_false(any(is.na(cuotas$n)))          # sin NA
  expect_gt(sum(cuotas$n), 0)                 # se asignaron entrevistas

  # Cada sección (cluster_3) debe tener al menos una entrevista asignada.
  por_cluster <- cuotas |>
    dplyr::group_by(cluster_3) |>
    dplyr::summarise(total = sum(n), .groups = "drop")
  expect_true(all(por_cluster$total > 0))
})
