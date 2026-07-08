# seleccionar_pps(): el mecanismo de selección debe producir EXACTAMENTE
# las probabilidades de inclusión que calcular_fpc() declara vía
# sampling::inclusionprobabilities().
#
# Antecedente teórico: el mecanismo anterior (slice_sample(weight_by=),
# muestreo sucesivo proporcional al tamaño) NO tiene probabilidades de
# inclusión n*p_i (Rosén): las unidades grandes quedan sub-representadas
# respecto a las pi declaradas y los pesos 1/pi resultan sesgados. El
# sistemático aleatorizado (UPrandomsystematic) sí cumple pi_i = pik.

test_that("devuelve exactamente n unidades del marco", {
  set.seed(1)
  bd <- tibble::tibble(id = 1:20, total = rpois(20, 50) + 1)
  sel <- seleccionar_pps(bd, n = 6)
  expect_equal(nrow(sel), 6)
  expect_true(all(sel$id %in% bd$id))
  expect_equal(anyDuplicated(sel$id), 0)
})

test_that("una unidad con probabilidad declarada 1 SIEMPRE se selecciona", {
  # tamaños muy desiguales: inclusionprobabilities topa la unidad grande
  # en pi = 1. El muestreo sucesivo la omitía ~10% de las veces; el
  # mecanismo correcto debe incluirla siempre.
  bd <- tibble::tibble(id = 1:6, total = c(100, 10, 10, 10, 10, 10))
  pik <- sampling::inclusionprobabilities(bd$total, 2)
  expect_equal(pik[1], 1) # precondición del caso

  set.seed(42)
  for (i in 1:200) {
    sel <- seleccionar_pps(bd, n = 2)
    expect_true(1 %in% sel$id)
    expect_equal(nrow(sel), 2)
  }
})

test_that("las frecuencias de inclusión empíricas coinciden con las pi declaradas", {
  set.seed(7)
  bd <- tibble::tibble(id = 1:12, total = c(5, 8, 12, 20, 20, 30, 45, 60, 80, 120, 200, 400))
  n <- 4
  pik <- sampling::inclusionprobabilities(bd$total, n)

  reps <- 4000
  conteo <- numeric(nrow(bd))
  for (i in seq_len(reps)) {
    sel <- seleccionar_pps(bd, n = n)
    conteo[sel$id] <- conteo[sel$id] + 1
  }
  frec <- conteo / reps
  # tolerancia: 4 errores estándar binomiales de cada pi (holgado pero
  # suficiente para detectar el sesgo del muestreo sucesivo, que en las
  # unidades grandes excede por mucho este margen)
  tol <- 4 * sqrt(pik * (1 - pik) / reps) + 1e-9
  expect_true(
    all(abs(frec - pik) < pmax(tol, 0.02)),
    label = paste0(
      "max desviación: ", round(max(abs(frec - pik)), 4)
    )
  )
})

test_that("casos borde: n >= N devuelve todo; n = 0 devuelve vacío", {
  bd <- tibble::tibble(id = 1:3, total = c(10, 20, 30))
  expect_equal(nrow(seleccionar_pps(bd, n = 3)), 3)
  expect_equal(nrow(seleccionar_pps(bd, n = 5)), 3)
  expect_equal(nrow(seleccionar_pps(bd, n = 0)), 0)
})

test_that("las unidades con tamaño 0 nunca se seleccionan", {
  bd <- tibble::tibble(id = 1:5, total = c(0, 10, 20, 0, 30))
  set.seed(3)
  for (i in 1:50) {
    sel <- seleccionar_pps(bd, n = 2)
    expect_false(any(sel$id %in% c(1, 4)))
  }
})

test_that("es reproducible con semilla", {
  bd <- tibble::tibble(id = 1:15, total = 1:15 * 10)
  set.seed(99)
  a <- seleccionar_pps(bd, n = 5)
  set.seed(99)
  b <- seleccionar_pps(bd, n = 5)
  expect_equal(a, b)
})

test_that("acepta otra columna de tamaño", {
  bd <- tibble::tibble(id = 1:6, LN = c(100, 10, 10, 10, 10, 10))
  set.seed(5)
  sel <- seleccionar_pps(bd, n = 2, variable_tamano = "LN")
  expect_true(1 %in% sel$id)
})

test_that("n no entero es error explícito (nunca n±1 silencioso)", {
  bd <- tibble::tibble(id = 1:10, total = 1:10 * 5)
  expect_error(seleccionar_pps(bd, n = 2.5), regexp = "entero")
  expect_error(seleccionar_pps(bd, n = NA_real_), regexp = "entero")
  expect_error(seleccionar_pps(bd, n = c(2, 3)), regexp = "entero")
})

test_that("todo tamaño NA es error con mensaje que menciona NA", {
  bd <- tibble::tibble(id = 1:3, total = c(NA_real_, NA_real_, NA_real_))
  expect_error(seleccionar_pps(bd, n = 2), regexp = "NA")
})
