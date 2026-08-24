# Dos defectos que producían material de campo silenciosamente equivocado:
#   1. el clon de un diseño pierde sus atributos -> la tasa de rechazo se resolvía
#      en 0 y el mapa prometía una entrevista por puerta;
#   2. el encuadre del mapa lo mandaba el contorno del conglomerado -> una sección
#      mucho mayor que sus manzanas salía a zoom bajo, ilegible.

cuadro <- function(cx, cy, s) {
  sf::st_polygon(list(matrix(
    c(cx - s, cy - s, cx + s, cy - s, cx + s, cy + s, cx - s, cy + s, cx - s, cy - s),
    ncol = 2, byrow = TRUE)))
}

# ---- 1. atributos al clonar -------------------------------------------------

test_that("clonar_diseno conserva los atributos y $clone no", {
  d <- generar_diseno_ine(n = 120, n_0 = 5, semilla = 42, unidades_nivel = 4)
  attr(d, "asignacion") <- tibble::tibble(estrato = "A", tasa_rechazo = 0.8)

  # el comportamiento de R6 que causa el problema, documentado como prueba
  expect_null(attr(d$clone(deep = TRUE), "asignacion"))

  clon <- clonar_diseno(d)
  expect_identical(attr(clon, "asignacion"), attr(d, "asignacion"))
  expect_s3_class(clon, "R6")
})

test_that("clonar_diseno copia todos los atributos de muestreaR, no solo la asignación", {
  d <- generar_diseno_ine(n = 120, n_0 = 5, semilla = 42, unidades_nivel = 4)
  attr(d, "asignacion") <- tibble::tibble(estrato = "A", tasa_rechazo = 0.5)
  attr(d, "numeracion_base") <- tibble::tibble(cluster_0 = 1L, manzana_num = 7L)
  attr(d, "tasas_cluster") <- tibble::tibble(cluster_2 = 1L, tasa = 0.3)

  clon <- clonar_diseno(d)
  for (a in c("asignacion", "numeracion_base", "tasas_cluster")) {
    expect_identical(attr(clon, a), attr(d, a), info = a)
  }
})

test_that("clonar_diseno es un clon de verdad: tocar el clon no toca el original", {
  d <- generar_diseno_ine(n = 120, n_0 = 5, semilla = 42, unidades_nivel = 4)
  attr(d, "asignacion") <- tibble::tibble(estrato = "A", tasa_rechazo = 0.5)
  n_antes <- d$n_0

  clon <- clonar_diseno(d)
  clon$n_0 <- 99
  expect_equal(d$n_0, n_antes)
})

test_that("clonar_diseno rechaza lo que no es un diseño", {
  expect_error(clonar_diseno(list(a = 1)), "objeto R6")
})

# ---- 2. la tasa que falta no se inventa -------------------------------------

test_that("resolver_tasa_rechazo avisa y devuelve NA cuando falta la asignación", {
  d <- generar_diseno_ine(n = 120, n_0 = 5, semilla = 42, unidades_nivel = 4)
  expect_warning(tasa <- muestreaR:::resolver_tasa_rechazo(d), "asignaci")
  expect_true(is.na(tasa))   # antes devolvía 0 sin avisar
})

test_that("resolver_tasa_rechazo devuelve la tasa cuando sí está", {
  d <- generar_diseno_ine(n = 120, n_0 = 5, semilla = 42, unidades_nivel = 4)
  attr(d, "asignacion") <- tibble::tibble(estrato = c("A", "B"),
                                          tasa_rechazo = c(0.7, 0.9))
  expect_equal(muestreaR:::resolver_tasa_rechazo(d), 0.8)
})

test_that("la etiqueta del mapa no promete una entrevista por puerta", {
  sin <- muestreaR:::etiqueta_mapa(
    data.frame(manzanas = 4, contactos = 108, entrevistas = NA_real_), zoom = 16)
  expect_match(sin, "no disponible")
  expect_no_match(sin, "Entrevistas planeadas: 108")

  con <- muestreaR:::etiqueta_mapa(
    data.frame(manzanas = 4, contactos = 108, entrevistas = 25), zoom = 16)
  expect_match(con, "Entrevistas planeadas: 25")
})

# ---- 3. el encuadre ---------------------------------------------------------

test_that("sin contexto_m el encuadre incluye todo el conglomerado", {
  # conglomerado grande (0.06 grados ~ 6.6 km) con las manzanas en una esquina
  secc <- sf::st_sf(geometry = sf::st_sfc(cuadro(-99.20, 19.86, 0.03), crs = 4326))
  man <- sf::st_sf(geometry = sf::st_sfc(cuadro(-99.185, 19.835, 0.0015), crs = 4326))

  bb <- muestreaR:::bbox_cluster(secc, man)
  expect_equal(as.numeric(bb[["xmin"]]), -99.23, tolerance = 1e-6)
  expect_equal(as.numeric(bb[["xmax"]]), -99.17, tolerance = 1e-6)
})

test_that("con contexto_m el encuadre sigue a las manzanas y el zoom sube", {
  secc <- sf::st_sf(geometry = sf::st_sfc(cuadro(-99.20, 19.86, 0.03), crs = 4326))
  man <- sf::st_sf(geometry = sf::st_sfc(cuadro(-99.185, 19.835, 0.0015), crs = 4326))

  bb_todo <- muestreaR:::bbox_cluster(secc, man)
  bb_ctx  <- muestreaR:::bbox_cluster(secc, man, contexto_m = 400)

  # el encuadre con contexto cabe DENTRO del completo y es mucho más chico
  expect_gt(as.numeric(bb_ctx[["xmin"]]), as.numeric(bb_todo[["xmin"]]))
  expect_lt(as.numeric(bb_ctx[["xmax"]]), as.numeric(bb_todo[["xmax"]]))
  ancho <- function(b) as.numeric(b[["xmax"]] - b[["xmin"]])
  expect_lt(ancho(bb_ctx), ancho(bb_todo) / 3)

  # y eso es lo que importa: el mapa sale a un zoom mucho mayor
  z_todo <- muestreaR:::zoom_para_bbox(bb_todo, zoom_max = 16)
  z_ctx  <- muestreaR:::zoom_para_bbox(bb_ctx,  zoom_max = 16)
  expect_gt(z_ctx, z_todo)
  expect_gte(z_ctx, 15)
})

test_that("el contexto es simétrico y del tamaño pedido", {
  man <- sf::st_sf(geometry = sf::st_sfc(cuadro(-99.185, 19.835, 0.001), crs = 4326))
  bb0 <- sf::st_bbox(man)
  bb <- muestreaR:::bbox_cluster(man, man, contexto_m = 500)

  # 500 m en latitud son ~0.00452 grados; en longitud, más por el coseno
  expect_equal(as.numeric(bb[["ymin"]]), as.numeric(bb0[["ymin"]]) - 500 / 110540,
               tolerance = 1e-9)
  expect_gt(as.numeric(bb0[["xmin"]]) - as.numeric(bb[["xmin"]]), 500 / 111320)
})

test_that("contexto_m se ignora si no hay manzanas que encuadrar", {
  secc <- sf::st_sf(geometry = sf::st_sfc(cuadro(-99.20, 19.86, 0.03), crs = 4326))
  vacio <- secc[0, ]
  expect_equal(muestreaR:::bbox_cluster(secc, vacio, contexto_m = 400),
               muestreaR:::bbox_cluster(secc, vacio))
})
