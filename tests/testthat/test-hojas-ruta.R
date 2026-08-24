# Invariantes de la hoja de ruta. El contrato es corto: la partición es completa,
# CONTIGUA en el orden de recorrido, respeta la frontera, y la carga queda pareja.

ruta_demo <- function(cargas, localidades = NULL, cl = 1) {
  n <- length(cargas)
  data.frame(cluster_2 = rep(cl, n), orden_ruta = seq_len(n),
             puertas_a_tocar = cargas,
             LOCALIDAD = if (is.null(localidades)) rep(1, n) else localidades)
}

test_that("un conglomerado que cabe en la carga no se parte", {
  h <- partir_en_hojas(ruta_demo(c(9, 9, 8)), carga_max = 120)
  expect_equal(unique(h$n_hojas), 1L)
  expect_equal(unique(h$id_hoja), "1")   # sin letra: no se partió
})

test_that("se parte por carga y las hojas quedan parejas", {
  h <- partir_en_hojas(ruta_demo(rep(34, 8)), carga_max = 120)   # 272 puertas
  expect_equal(unique(h$n_hojas), 3L)

  carga <- tapply(h$puertas_a_tocar, h$hoja, sum)
  expect_true(all(carga <= 120))
  # "parejas": ninguna hoja al doble de otra
  expect_lt(max(carga) / min(carga), 2)
})

test_that("las hojas son CONTIGUAS en el orden de recorrido", {
  h <- partir_en_hojas(ruta_demo(rep(34, 8)), carga_max = 120)
  rangos <- tapply(h$orden_ruta, h$hoja, function(o) c(min(o), max(o)))
  # cada hoja es un rango cerrado y los rangos no se traslapan
  for (r in rangos) {
    expect_equal(sum(h$orden_ruta >= r[1] & h$orden_ruta <= r[2]),
                 r[2] - r[1] + 1)
  }
  expect_identical(h$hoja, sort(h$hoja))   # el papel se lee en orden
})

test_that("la frontera no se cruza", {
  # 7 manzanas en la localidad 1 y una en la 21: la aislada va en su propia hoja
  h <- partir_en_hojas(ruta_demo(c(rep(34, 7), 4), c(rep(1, 7), 21)),
                       carga_max = 120, frontera = "LOCALIDAD")
  locs <- tapply(h$LOCALIDAD, h$hoja, function(l) length(unique(l)))
  expect_true(all(locs == 1))

  sola <- h[h$LOCALIDAD == 21, ]
  expect_equal(nrow(sola), 1)
  expect_equal(sum(h$hoja == sola$hoja), 1)   # está sola en su hoja
})

test_that("sin frontera declarada, la carga puede juntar localidades", {
  # el contraste con la prueba anterior: es lo que `frontera` viene a evitar
  h <- partir_en_hojas(ruta_demo(c(rep(34, 7), 4), c(rep(1, 7), 21)),
                       carga_max = 120)
  locs <- tapply(h$LOCALIDAD, h$hoja, function(l) length(unique(l)))
  expect_true(any(locs > 1))
})

test_that("la partición es completa y no duplica", {
  r <- rbind(ruta_demo(rep(34, 8), cl = 1),
             ruta_demo(c(9, 9, 8), cl = 2))
  h <- partir_en_hojas(r, carga_max = 120)

  expect_equal(nrow(h), nrow(r))
  expect_equal(sum(h$puertas_a_tocar), sum(r$puertas_a_tocar))
  # un id_hoja pertenece a un solo conglomerado
  expect_equal(nrow(unique(h[, c("id_hoja", "cluster_2")])),
               length(unique(h$id_hoja)))
})

test_that("las hojas se numeran por el orden de recorrido: la A trae las primeras", {
  h <- partir_en_hojas(ruta_demo(rep(34, 8)), carga_max = 120)
  expect_equal(h$id_hoja[1], "1-A")
  expect_equal(h$id_hoja[nrow(h)], "1-C")
})

test_that("una unidad más grande que la carga máxima se queda sola", {
  h <- partir_en_hojas(ruta_demo(c(500, 10)), carga_max = 120)
  expect_equal(nrow(h), 2)
  expect_equal(sum(h$hoja == h$hoja[1]), 1)
})

test_that("es determinista", {
  r <- ruta_demo(c(30, 41, 22, 55, 18, 47, 33))
  expect_identical(partir_en_hojas(r, 100)$id_hoja,
                   partir_en_hojas(r, 100)$id_hoja)
})

test_that("no se rompe con el orden de entrada desordenado", {
  r <- ruta_demo(rep(34, 8))
  h1 <- partir_en_hojas(r, 120)
  h2 <- partir_en_hojas(r[c(5, 2, 8, 1, 7, 3, 6, 4), ], 120)
  expect_identical(h1$id_hoja, h2$id_hoja)   # se ordena por (cluster, orden)
})

test_that("pasa de 26 hojas sin repetir etiqueta", {
  h <- partir_en_hojas(ruta_demo(rep(100, 30)), carga_max = 100)
  expect_equal(unique(h$n_hojas), 30L)
  expect_equal(length(unique(h$id_hoja)), 30L)
  expect_true("1-AA" %in% h$id_hoja)   # la 27a
})

test_that("una ruta vacía devuelve una ruta vacía", {
  vacia <- ruta_demo(numeric(0))
  h <- partir_en_hojas(vacia, 100)
  expect_equal(nrow(h), 0)
  expect_true(all(c("hoja", "n_hojas", "id_hoja") %in% names(h)))
})

test_that("rechaza insumos inválidos con mensajes accionables", {
  expect_error(partir_en_hojas(ruta_demo(c(1, 2)), 100, carga = "no_existe"),
               "faltan columnas")
  expect_error(partir_en_hojas(ruta_demo(c(1, 2)), 0), "positivo")
  expect_error(partir_en_hojas(ruta_demo(c(1, NA)), 100), "sin NA")
  expect_error(partir_en_hojas(ruta_demo(c(1, 2)), 100, frontera = "nada"),
               "faltan columnas")
})
