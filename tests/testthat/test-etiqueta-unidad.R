# El título del mapa y el nombre de su archivo salían del mismo valor. Cuando el
# id del archivo no es la llave que el encuestador captura (hojas de ruta: el
# archivo es `1-A.png` pero el cluster es `1`), el título mentía y campo copiaba
# un cluster inexistente.

test_that("sin etiquetas se conserva el título de siempre", {
  expect_equal(muestreaR:::.etiqueta_unidad(7, "cluster_2"), "cluster_2: 7")
  expect_equal(muestreaR:::.etiqueta_unidad("1-A", "cluster_2"), "cluster_2: 1-A")
})

test_that("la etiqueta reemplaza el título sin tocar el id", {
  et <- c("1-A" = "Cluster 1 · hoja A de 4")
  expect_equal(muestreaR:::.etiqueta_unidad("1-A", "cluster_2", et),
               "Cluster 1 · hoja A de 4")
})

test_that("una unidad sin entrada cae al default (se pueden etiquetar solo algunas)", {
  et <- c("1-A" = "Cluster 1 · hoja A")
  expect_equal(muestreaR:::.etiqueta_unidad("23", "cluster_2", et), "cluster_2: 23")
})

test_that("entradas vacías o NA caen al default en vez de imprimir un título vacío", {
  expect_equal(muestreaR:::.etiqueta_unidad("1-A", "cluster_2", c("1-A" = "")),
               "cluster_2: 1-A")
  expect_equal(muestreaR:::.etiqueta_unidad("1-A", "cluster_2",
                                            c("1-A" = NA_character_)),
               "cluster_2: 1-A")
})

test_that("el id se busca como texto: da igual que la unidad venga numérica", {
  et <- c("7" = "Cluster 7")
  expect_equal(muestreaR:::.etiqueta_unidad(7, "cluster_2", et), "Cluster 7")
  expect_equal(muestreaR:::.etiqueta_unidad("7", "cluster_2", et), "Cluster 7")
})

test_that("acepta lista además de vector", {
  expect_equal(muestreaR:::.etiqueta_unidad("1-A", "cluster_2",
                                            list("1-A" = "Cluster 1")),
               "Cluster 1")
})

test_that("devuelve siempre un character de largo 1", {
  x <- muestreaR:::.etiqueta_unidad(7, "cluster_2")
  expect_type(x, "character")
  expect_length(x, 1)
})

test_that("un vector sin nombres se rechaza con un mensaje accionable", {
  expect_error(muestreaR:::.validar_etiquetas_unidad(c("Cluster 1", "Cluster 2")),
               "CON NOMBRES")
  # un nombre vacío tampoco resuelve a ninguna unidad
  sin_nombre <- c("x", "y"); names(sin_nombre) <- c("1-A", "")
  expect_error(muestreaR:::.validar_etiquetas_unidad(sin_nombre), "CON NOMBRES")
})

test_that("nombres repetidos se rechazan: no se sabría cuál gana", {
  repetido <- c("x", "y"); names(repetido) <- c("1-A", "1-A")
  expect_error(muestreaR:::.validar_etiquetas_unidad(repetido), "repetidos")
})

test_that("NULL es válido: es el default", {
  expect_silent(muestreaR:::.validar_etiquetas_unidad(NULL))
})

test_that("google_maps_ine y google_maps exponen el parámetro", {
  for (f in list(muestreaR::google_maps_ine, muestreaR::google_maps)) {
    expect_true("etiquetas_unidad" %in% names(formals(f)))
    expect_null(eval(formals(f)$etiquetas_unidad))
  }
  # y la clase lo pasa hacia abajo
  expect_true("etiquetas_unidad" %in%
                names(formals(muestreaR::CartografiaINE$public_methods$crear_mapas)))
})

test_that("el validador corre ANTES de tocar Google", {
  # con etiquetas inválidas debe abortar por el validador, no por la cartografía
  expect_error(
    muestreaR::google_maps_ine(diseño = NULL, shp = NULL, zoom = 16,
                               etiquetas_unidad = c("sin", "nombres")),
    "CON NOMBRES")
})
