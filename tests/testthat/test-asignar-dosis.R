# Invariantes del reparto proporcional con piso.
#
# El contrato es corto y todo lo demás se deriva de él: la suma es EXACTA, el
# piso se respeta siempre, y por encima del piso la dosis sigue al tamaño.

test_that("suma exactamente el total y respeta el piso", {
  d <- asignar_dosis_proporcional(c(287, 1090, 10685), total = 768, piso = 6)

  expect_equal(sum(d), 768)
  expect_true(all(d >= 6))
  expect_equal(d, round(d))
  expect_length(d, 3)
  expect_type(d, "integer")
})

test_that("la dosis es monótona en el tamaño", {
  tam <- c(300, 800, 1500, 4000, 10000)
  d <- asignar_dosis_proporcional(tam, total = 500, piso = 5)

  # ordenar por tamaño debe ordenar por dosis (empates permitidos)
  expect_true(all(diff(d[order(tam)]) >= 0))
})

test_that("con unidades iguales el reparto es uniforme", {
  d <- asignar_dosis_proporcional(rep(100, 8), total = 80)
  expect_equal(d, rep(10L, 8))
})

test_that("por encima del piso la dosis es proporcional al tamaño", {
  # tamaños que no tocan el piso: la razón dosis/tamano debe ser ~constante
  tam <- c(1000, 2000, 3000, 4000)
  d <- asignar_dosis_proporcional(tam, total = 200, piso = 1)

  razon <- d / tam
  expect_lt(stats::sd(razon) / mean(razon), 0.05)
})

test_that("el piso se respeta aunque la proporcionalidad pida menos", {
  # la unidad de 50 pediría ~1 de 100 con un tamaño total de 5100
  d <- asignar_dosis_proporcional(c(50, 50, 5000), total = 100, piso = 10)

  expect_equal(sum(d), 100)
  expect_true(all(d >= 10))
  expect_equal(d[1:2], c(10L, 10L))   # ambas quedan EN el piso
})

test_that("el caso borde total == n * piso devuelve el piso exacto", {
  expect_equal(asignar_dosis_proporcional(c(10, 500, 9000), total = 9, piso = 3),
               rep(3L, 3))
})

test_that("una sola unidad se lleva todo", {
  expect_equal(asignar_dosis_proporcional(1234, total = 50, piso = 5), 50L)
})

test_that("el reparto reduce la desigualdad de pesos frente a la dosis fija", {
  # el caso que motivó la función: censo de unidades muy desiguales, donde el PPS
  # ya no puede dar la autoponderación (pi = 1 en todas)
  set.seed(1)
  tam <- c(287, 291, 368, 476, 1090, 1158, 2384, 3954, 7905, 10685)
  total <- 240

  cv <- function(w, d) {
    m <- sum(d * w) / sum(d)
    sqrt(sum(d * (w - m)^2) / sum(d)) / m
  }
  d_prop <- asignar_dosis_proporcional(tam, total = total, piso = 6)
  d_fija <- rep(total / length(tam), length(tam))

  cv_prop <- cv(tam / d_prop, d_prop)
  cv_fija <- cv(tam / d_fija, d_fija)

  expect_lt(cv_prop, cv_fija)     # los pesos quedan más planos
  expect_lt(1 + cv_prop^2, 1.5)   # y el deff por pesos, cerca de 1
})

test_that("es determinista: mismo insumo, mismo reparto", {
  tam <- c(500, 1500, 2500, 9000)
  expect_identical(asignar_dosis_proporcional(tam, 300, 6),
                   asignar_dosis_proporcional(tam, 300, 6))
})

test_that("rechaza insumos inválidos con mensajes accionables", {
  expect_error(asignar_dosis_proporcional(c(1, 1, 1), total = 2, piso = 1),
               "no alcanza para dar el piso")
  expect_error(asignar_dosis_proporcional(c(1, -1), total = 10),
               "debe ser positivo")
  expect_error(asignar_dosis_proporcional(c(1, NA), total = 10),
               "sin NA")
  expect_error(asignar_dosis_proporcional(numeric(0), total = 10),
               "no vacío")
  expect_error(asignar_dosis_proporcional(c(1, 2), total = 10.5),
               "entero positivo")
  expect_error(asignar_dosis_proporcional(c(1, 2), total = 10, piso = -1),
               "no negativo")
})
