# Invariantes del reparto por mayor residuo (Hamilton).

test_that("repartir_cociente suma exactamente n y devuelve enteros", {
  x <- c(2.4, 3.3, 4.3)   # suma 10
  res <- repartir_cociente(n = 10, x = x)

  expect_equal(sum(res), 10)              # cuadra al total
  expect_equal(res, round(res))           # enteros
  expect_length(res, length(x))           # conserva longitud
  expect_true(all(res >= 0))              # no negativos
})

test_that("repartir_cociente reparte el residuo a los mayores fraccionarios", {
  # x = c(1.9, 1.05, 1.05), suma 4 -> piso c(1,1,1), sobra 1 al mayor residuo (1.9)
  res <- repartir_cociente(n = 4, x = c(1.9, 1.05, 1.05))
  expect_equal(res, c(2, 1, 1))
})

test_that("repartir_cociente rechaza vectores inválidos", {
  # sum(x)+length(x)-1 <= n  => se considera inválido y debe abortar
  expect_error(repartir_cociente(n = 10, x = c(1, 1)))
})
