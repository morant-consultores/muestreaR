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

test_that("el sobrante va al mayor residuo aunque el vector no venga ordenado", {
  # contraejemplo del bug order-vs-rank: residuos .5, .1, .9 y sobran 2
  expect_equal(repartir_cociente(n = 13, x = c(4.5, 4.1, 3.9)), c(5, 4, 4))
  # residuos desordenados con 1 sobrante: va al .9 (posición 3)
  expect_equal(repartir_cociente(n = 13, x = c(4.5, 4.1, 4.9)), c(4, 4, 5))
})

test_that("funciona con un solo elemento (dominio único)", {
  expect_equal(repartir_cociente(n = 100, x = 100), 100)
})

test_that("empates en el residuo se resuelven de forma determinista", {
  res <- repartir_cociente(n = 7, x = c(2.5, 2.5, 1.5))
  expect_equal(sum(res), 7)
  expect_equal(res, c(3, 3, 1))  # ties.method = "first"
})
