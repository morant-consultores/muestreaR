# Mapa interactivo del flujo SECCIONAL INE (capas_leaflet_seccional +
# mapa_interactivo_seccional). Espejo de test-leaflet.R con el fixture INE.

# cartografía sintética para el diseño INE: cuadrados por manzana (keyed
# MUNICIPIO/SECCION/id), sección y municipio (con NOMBRE_MUN)
cartografia_ine_prueba <- function(marco) {
  sq <- function(x, y, s = 0.01) sf::st_polygon(list(matrix(
    c(x, y, x + s, y, x + s, y + s, x, y + s, x, y), ncol = 2, byrow = TRUE)))

  mzas <- marco |> dplyr::distinct(MUNICIPIO, SECCION, id)
  shp_mza <- sf::st_sf(
    mzas,
    geometry = sf::st_sfc(lapply(seq_len(nrow(mzas)),
                                 function(i) sq(i * 0.02, 0)), crs = 4326))

  secc <- marco |> dplyr::distinct(MUNICIPIO, SECCION)
  shp_secc <- sf::st_sf(
    secc,
    geometry = sf::st_sfc(lapply(seq_len(nrow(secc)),
                                 function(i) sq(i * 0.1, 1, s = 0.05)),
                          crs = 4326))

  mun <- marco |> dplyr::distinct(MUNICIPIO, NOMBRE_MUN)
  shp_mun <- sf::st_sf(
    mun,
    geometry = sf::st_sfc(lapply(seq_len(nrow(mun)),
                                 function(i) sq(i * 0.5, 2, s = 0.2)),
                          crs = 4326))

  list(MANZANA = shp_mza, SECCION = shp_secc, MUNICIPIO = shp_mun)
}

diseno_ine_con_muestra <- function() {
  pob <- generar_poblacion_ine()
  est <- data.frame(estrato = c("Region 1", "Region 2"),
                    entrevistas = c(50, 50), tasa_rechazo = c(0.5, 0.2))
  disenar_muestra_ine(pob, est, semilla = 123)
}

test_that("capas_leaflet_seccional arma manzanas/secciones/municipios", {
  d <- diseno_ine_con_muestra()
  cart <- cartografia_ine_prueba(d$poblacion$marco_muestral)
  capas <- capas_leaflet_seccional(d, cart)

  expect_named(capas, c("municipios", "secciones", "manzanas"))
  expect_s3_class(capas$manzanas, "sf")
  expect_s3_class(capas$secciones, "sf")

  bd <- d$muestra |> purrr::pluck(length(d$muestra)) |> tidyr::unnest(data)
  expect_equal(nrow(capas$manzanas), nrow(bd))
  expect_equal(nrow(capas$secciones), dplyr::n_distinct(bd$SECCION))

  # orden de ruta determinista: 1..k dentro de cada sección
  orden <- capas$manzanas |> sf::st_drop_geometry() |>
    dplyr::group_by(SECCION) |>
    dplyr::summarise(ok = identical(sort(orden_ruta),
                                    seq_len(max(orden_ruta))),
                     .groups = "drop")
  expect_true(all(orden$ok))

  # sin ruta: la dosis mostrada es n_0 (viviendas)
  expect_true(all(capas$manzanas$viviendas == d$n_0))
  expect_true(all(grepl("Viviendas a levantar", capas$manzanas$popup)))

  # capa municipal: flag y totales
  mun <- capas$municipios
  expect_true(all(c("nombre", "en_muestra", "secciones", "contactos", "popup")
                  %in% names(mun)))
  expect_setequal(mun$MUNICIPIO[mun$en_muestra], unique(bd$MUNICIPIO))
})

test_that("capas_leaflet_seccional integra la ruta del presupuesto", {
  d <- diseno_ine_con_muestra()
  cart <- cartografia_ine_prueba(d$poblacion$marco_muestral)

  bd <- d$muestra |> purrr::pluck(length(d$muestra)) |> tidyr::unnest(data)
  ruta <- bd |>
    dplyr::group_by(SECCION) |>
    dplyr::mutate(orden_ruta = dplyr::row_number(),
                  toques_esperados = 12L,
                  puertas_presupuesto_seccion = 60L,
                  dentro_presupuesto = orden_ruta <= 2) |>
    dplyr::ungroup() |>
    dplyr::select(cluster_0, orden_ruta, toques_esperados,
                  dentro_presupuesto, puertas_presupuesto_seccion)

  capas <- capas_leaflet_seccional(d, cart, ruta = ruta)

  expect_true(all(grepl("Toques esperados", capas$manzanas$popup)))
  expect_true(any(grepl("RESERVA", capas$manzanas$popup)))
  # el resumen de la sección usa el presupuesto, no n_0
  expect_true(all(capas$secciones$contactos == 60L))
  expect_true(all(grepl("Presupuesto de puertas", capas$secciones$detalle)))
})

test_that("mapa_interactivo_seccional devuelve un widget leaflet", {
  d <- diseno_ine_con_muestra()
  cart <- cartografia_ine_prueba(d$poblacion$marco_muestral)
  m <- mapa_interactivo_seccional(d, cart)
  expect_s3_class(m, "leaflet")
})
