# Mapa interactivo (leaflet) de la muestra para planear rutas de campo.
# El helper capas_leaflet_ageb() arma las dos capas sf (AGEBs y manzanas
# sorteadas) con las columnas de popup; mapa_interactivo_ageb() las envuelve
# en un widget leaflet exportable a HTML. Fixtures en helper-fixture.R.

test_that("capas_leaflet_ageb arma manzanas y AGEBs con la info operativa", {
  diseno <- diseno_ageb_con_muestra()
  cart <- cartografia_ageb_prueba(diseno$poblacion$marco_muestral)
  capas <- capas_leaflet_ageb(diseno, cart)

  expect_named(capas, c("agebs", "manzanas"))
  expect_s3_class(capas$agebs, "sf")
  expect_s3_class(capas$manzanas, "sf")

  # una fila por AGEB sorteado y una por manzana sorteada
  bd <- diseno$muestra |> purrr::pluck(length(diseno$muestra)) |>
    tidyr::unnest(data)
  expect_equal(nrow(capas$agebs), dplyr::n_distinct(bd$AGEB))
  expect_equal(nrow(capas$manzanas), dplyr::n_distinct(bd$MZA))

  # columnas para el popup / color de ruta
  expect_true(all(c("AGEB", "MZA", "NOM_MUN", "viviendas", "popup")
                  %in% names(capas$manzanas)))
  expect_true(all(c("AGEB", "manzanas", "contactos", "entrevistas", "mapa",
                    "popup") %in% names(capas$agebs)))

  # las viviendas por manzana salen de n_0 (5 en el fixture)
  expect_true(all(capas$manzanas$viviendas == 5))
  # el resumen del AGEB coincide con resumen_operativo
  res <- resumen_operativo(diseno)
  expect_equal(sort(capas$agebs$contactos), sort(res$contactos))
})

test_that("mapa_interactivo_ageb devuelve un widget leaflet y exporta HTML", {
  skip_if_not_installed("leaflet")
  skip_if_not_installed("htmlwidgets")
  diseno <- diseno_ageb_con_muestra()
  cart <- cartografia_ageb_prueba(diseno$poblacion$marco_muestral)

  m <- mapa_interactivo_ageb(diseno, cart)
  expect_s3_class(m, "leaflet")

  archivo <- withr::local_tempfile(fileext = ".html")
  ruta <- mapa_interactivo_ageb(diseno, cart, archivo = archivo)
  expect_equal(ruta, archivo)
  expect_true(file.exists(archivo))
  expect_gt(file.info(archivo)$size, 0)
})
