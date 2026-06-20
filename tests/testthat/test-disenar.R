# Tests de la API declarativa (Fase 4).

test_that("calcular_asignacion reproduce el modelo operativo de referencia", {
  est <- data.frame(estrato = c("A", "B"), entrevistas = c(300, 900))

  a <- calcular_asignacion(est)                          # sin rechazo
  expect_equal(a$secciones, c(30, 90))
  expect_equal(a$manzanas_por_seccion, c(2, 2))
  expect_equal(a$entrevistas_a_levantar, c(300, 900))

  m <- calcular_asignacion(est, tasa_rechazo = 0.5)      # 50% rechazo, modo manzanas
  expect_equal(m$secciones, c(30, 90))                   # secciones fijas
  expect_equal(m$manzanas_por_seccion, c(4, 4))          # manzanas duplicadas
  expect_equal(m$entrevistas_a_levantar, c(600, 1800))

  s <- calcular_asignacion(est, tasa_rechazo = 0.5, modo_rechazo = "secciones")
  expect_equal(s$secciones, c(60, 180))                  # secciones duplicadas
  expect_equal(s$manzanas_por_seccion, c(2, 2))          # manzanas fijas
})

test_that("calcular_asignacion soporta tasa de rechazo por estrato", {
  est <- data.frame(estrato = c("A", "B"), entrevistas = c(300, 300),
                    tasa_rechazo = c(0.5, 0))
  a <- calcular_asignacion(est)
  expect_equal(a$manzanas_por_seccion, c(4, 2))
})

test_that("calcular_asignacion rechaza tasas fuera de [0,1)", {
  expect_error(calcular_asignacion(data.frame(estrato = "A", entrevistas = 100),
                                   tasa_rechazo = 1))
})

test_that("validar_estratos acumula los problemas y aprueba lo válido", {
  pob <- generar_poblacion_ine()
  ok <- validar_estratos(pob, data.frame(estrato = c("Region 1", "Region 2"),
                                         entrevistas = c(50, 50)))
  expect_length(ok, 0)

  probs <- validar_estratos(pob, data.frame(estrato = c("Region 9", "Region 2"),
                                            entrevistas = c(-5, 50)))
  expect_gte(length(probs), 2)   # entrevistas negativas + estrato inexistente
})

test_that("disenar_muestra_ine es reproducible con la misma semilla", {
  pob <- generar_poblacion_ine()
  est <- data.frame(estrato = c("Region 1", "Region 2"), entrevistas = c(50, 50))

  muestra_final <- function(d) {
    d$muestra |> purrr::pluck(length(d$muestra)) |>
      tidyr::unnest(data) |> dplyr::arrange(id)
  }
  d1 <- disenar_muestra_ine(pob, est, semilla = 123)
  d2 <- disenar_muestra_ine(pob, est, semilla = 123)

  expect_equal(muestra_final(d1), muestra_final(d2))
  expect_equal(d1$cuotas, d2$cuotas)
})

test_that("disenar_muestra_ine respeta el total a levantar", {
  pob <- generar_poblacion_ine()
  est <- data.frame(estrato = c("Region 1", "Region 2"), entrevistas = c(50, 40))
  d <- disenar_muestra_ine(pob, est, semilla = 123)

  r <- resumen_diseno(d)
  expect_equal(sum(r$entrevistas_real),
               sum(attr(d, "asignacion")$entrevistas_a_levantar))
  expect_true(all(c("objetivo", "secciones_real", "entrevistas_real") %in% names(r)))
})

test_that("disenar_muestra_ine valida y aborta ante una especificación inválida", {
  pob <- generar_poblacion_ine()
  expect_error(
    disenar_muestra_ine(pob, data.frame(estrato = "Region 9", entrevistas = 50)),
    "inv"
  )
})
